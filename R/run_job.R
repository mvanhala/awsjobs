
#' Get EC2 instance state
#'
#' Gets the current state of an EC2 instance
#' @param ec2 An EC2 service objected created by [paws::ec2]
#' @param instance_id The id of the instance of which to get the status
#' @return The instance state name
#' @export
get_instance_state <- function(ec2, instance_id) {
  instance_status <- ec2$describe_instance_status(
    InstanceIds = list(instance_id),
    IncludeAllInstances = TRUE
  )
  instance_status$InstanceStatuses[[1]]$InstanceState$Name
}

#' Run R worker job
#'
#' Spin up an AWS EC2 instance and run an R script or R Markdown document in that instance.
#' It launched an EC2 instance, attaches EFS storage, then send a command to execute the
#' R script or RMD file as an SSM command.
#' This function will block the R session until execution is complete.
#'
#' @param script Path to the R script or R Markdown document to execute (on EFS storage)
#' @param job_id Job id
#' @param image_id id of the AMI to launch
#' @param instance_type Instance type
#' @param spot Boolean value for whether to launch a spot instance
#' @param spot_max_price If launching a spot instance, the maximum bid price
#' @param efs_name EFS id
#' @param security_group_id Security group id
#' @param ssm_iam_profile Name of IAM profile with SSM permissions. This profile will be
#' attached to the launched EC2 instance.
#' @param efs_mnt_pt Mount point of EFS storage on worker instance
#' @param region AWS EC2 region
#' @param instance_max_hours Maximum lifetime of the instance in hours before it will
#' automatically shut down
#' @param exec_timeout_hours Number of hours for timeout of the SSM command for executing
#' the script
#' @param r_version Version of R to use
#' @param ssm_work_dir Working directory of SSM command for executing R script/document
#' @param log_file Path of log file to which stdout/stderr of R script will be written
#' @param rmd_opts If running an R Markdown document, optional parameters for the output file path
#' and document parameters
#' @param status_sleep_seconds How many seconds to wait between checking the status of
#' the execution of the R script
#' @export
run_job <- function(script,
                    job_id,
                    image_id,
                    instance_type,
                    spot,
                    spot_max_price,
                    efs_name,
                    security_group_id,
                    ssm_iam_profile,
                    efs_mnt_pt,
                    region,
                    instance_max_hours,
                    exec_timeout_hours,
                    r_version,
                    ssm_work_dir,
                    log_file,
                    rmd_opts = list(),
                    status_sleep_seconds = 30) {
  checkmate::assert_number(instance_max_hours, lower = 0, upper = 48)
  checkmate::assert_number(exec_timeout_hours, lower = 0.001, upper = 48)
  extension <- stringr::str_to_upper(fs::path_ext(script))
  checkmate::assert_subset(extension, c("R", "RMD"))

  username <- Sys.getenv("USER")

  config <- list(region = region)
  svc_ec2 <- paws::ec2(config = config)
  svc_ssm <- paws::ssm(config = config)

  user_data <- glue::glue(
    "#!/bin/bash\n",
    "mkdir -p {efs_mnt_pt}\n",
    "mount -t efs {efs_name}:/ {efs_mnt_pt}\n",
    "snap stop amazon-ssm-agent\n",
    "snap start amazon-ssm-agent\n",
    "shutdown -h +{minutes}",
    minutes = round(60 * instance_max_hours + 10)
  )
  user_data_enc <- base64enc::base64encode(charToRaw(user_data))

  market_opts <- if (spot) {
    list(
      MarketType = "spot",
      SpotOptions = list(
        MaxPrice = spot_max_price,
        SpotInstanceType = "one-time"
      )
    )
  } else NULL

  instance <- svc_ec2$run_instances(
    MinCount = 1,
    MaxCount = 1,
    ImageId = image_id,
    InstanceType = instance_type,
    SecurityGroupIds = security_group_id,
    IamInstanceProfile = list(Name = ssm_iam_profile),
    TagSpecifications = list(
      list(
        ResourceType = "instance",
        Tags = list(
          list(
            Key = "Name",
            Value = glue::glue(
              "R-job-{username}-{time}",
              time = strftime(Sys.time(), "%Y%m%d-%H%M%S")
            )
          ),
          list(Key = "JOB_TYPE", Value = "R worker job"),
          list(Key = "R_JOB_ID" , Value = job_id)
        )
      )
    ),
    InstanceMarketOptions = market_opts,
    UserData = user_data_enc
  )

  instance_id <- instance$Instances[[1]]$InstanceId

  message("Instance type: ", instance_type)
  if (spot) message("Spot instance") else message("On-demand instance")
  message("Instance id: ", instance_id)

  on.exit(svc_ec2$terminate_instances(InstanceIds = instance_id), add = TRUE)

  tries <- 0
  while (tries < 100) {
    tries <- tries + 1
    state <- get_instance_state(svc_ec2, instance_id)
    if (state == "running") break
    message("Waiting for instance to launch")
    Sys.sleep(5)
  }
  if (state != "running") {
    stop("Waiting too long for instance to launch, terminating")
  }

  script_path <- fs::path_rel(script, ssm_work_dir)
  output_dir <- if (!is.null(rmd_opts$output_dir)) {
    fs::path_rel(rmd_opts$output_dir, ssm_work_dir)
  } else {
    NULL
  }

  if (extension == "R") {
    expr <- glue::glue('"{script_path}"')
  } else if (extension == "RMD") {
    args <- list(
      input = script_path,
      output_dir = output_dir,
      output_file = rmd_opts$output_file,
      params = rmd_opts$params
    )
    args_file <- fs::path(
      ssm_work_dir,
      paste0(".args-", digest::digest(sodium::random(16)), ".rds")
    )
    on.exit(try(fs::file_delete(args_file)), add = TRUE)
    saveRDS(args, args_file)
    expr <- glue::glue(
      '-e "args <- readRDS(\\"{args_file}\\"); ',
      'purrr::lift_dl(rmarkdown::render)(c(args, envir = globalenv()))"'
    )
  }

  if (!is.null(log_file)) {
    log_path <- fs::path_rel(log_file, ssm_work_dir)
    fs::dir_create(fs::path_dir(log_path))
    expr <- paste0(expr, ' > "', log_path, '" 2>&1')
  }

  tries <- 0
  succeeded <- FALSE
  while(tries < 10) {
    tries <- tries + 1
    test_cmd <- try(
      svc_ssm$send_command(
        InstanceIds = list(instance_id),
        DocumentName = "AWS-RunShellScript",
        Parameters = list(
          commands = list(glue::glue('ls "{ssm_work_dir}"')),
          workingDirectory = efs_mnt_pt,
          executionTimeout = 60
        )
      )
    )

    Sys.sleep(1)

    if (!inherits(test_cmd, "try-error")) {
      while (TRUE) {
        test_result <- svc_ssm$list_command_invocations(
          CommandId = test_cmd$Command$CommandId,
          InstanceId = instance_id,
          Details = TRUE
        )

        test_status <- try(test_result$CommandInvocations[[1]]$StatusDetails)

        if (!inherits(test_status, "try-error")) {
          if (test_status %in% c("Success")) {
            succeeded <- TRUE
            break
          }
          if (test_status %in% c("DeliveryTimedOut",
                                 "ExecutionTimedOut",
                                 "Failed",
                                 "Canceled",
                                 "Undeliverable",
                                 "Terminated")) {
            break
          }
        }

        Sys.sleep(5)
      }
      if (succeeded) break
    }
    message("Waiting to initialize and mount EFS")
    Sys.sleep(10)
  }

  tries <- 0
  while (tries < 10) {
    tries <- tries + 1
    send_cmd <- try(
      svc_ssm$send_command(
        InstanceIds = list(instance_id),
        DocumentName = "AWS-RunShellScript",
        Parameters = list(
          commands = list(
            glue::glue(
              "su - ubuntu -c 'cd \"{ssm_work_dir}\"; /opt/R/{r_version}/bin/Rscript {expr}'"
            )
          ),
          workingDirectory = efs_mnt_pt,
          executionTimeout = round(3600 * exec_timeout_hours)
        ),
        Comment = glue::glue("Run {script} on {instance_id}")
      )
    )
    if (!inherits(send_cmd, "try-error")) break
    message("Waiting to send command to job")
    Sys.sleep(10)
  }
  if (inherits(send_cmd, "try-error")) {
    stop("Failed to send command for running job")
  }

  message("Sent command to run script, currently executing")
  cmd_id <- send_cmd$Command$CommandId
  Sys.sleep(1)

  while (TRUE) {
    result <- svc_ssm$list_command_invocations(
      CommandId = cmd_id,
      InstanceId = instance_id,
      Details = TRUE
    )

    cmd_status <- try(result$CommandInvocations[[1]]$StatusDetails)

    if (!inherits(cmd_status, "try-error")) {
      if (cmd_status %in% c("Success")) break
      if (cmd_status %in% c("DeliveryTimedOut",
                            "ExecutionTimedOut",
                            "Failed",
                            "Canceled",
                            "Undeliverable",
                            "Terminated")) {
        stop("Command status - ", cmd_status, call. = FALSE)
      }
    }

    state <- get_instance_state(svc_ec2, instance_id)
    if (state != "running") {
      svc_ssm$cancel_command(CommandId = cmd_id, InstanceIds = list(instance_id))
      stop("Instance is no longer running, canceled command")
    }

    Sys.sleep(status_sleep_seconds)
  }

  message("Script executed successfully")

  output <- result$CommandInvocations[[1]]$CommandPlugins[[1]]$Output
  invisible(output)
}


