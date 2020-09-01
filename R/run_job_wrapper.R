#' Run R worker job
#'
#' This is a convenience function for running an R script or R Markdown document on
#' an EC2 worker instance. The user just has to provide the script/document to execute,
#' the desired memory and cpu, and whether to execute as a spot instance.
#' Other parameters are filled with defaults passed to [run_job()].
#'
#' Some parameters, such as the image id, EFS id, security group id, and SSM IAM profile
#' to attach to the launched instance, are assumed to be stored in environment variables.
#' The user could put these in a configuration file or otherwise set these environment
#' variables, or simply override them in invoking the function.
#'
#' The user specifies an amount of memory and CPU and whether to use a spot instance.
#' The function then determines the optimal spot (or on-demand) instance to use.
#'
#' This function does not block the running R session. What it does is launch an
#' RStudio Job in a child process. Information about the running job is displayed in the
#' RStudio Jobs pane. However, note that the user should *not* user the stop button
#' to stop a running job. This will leave the running EC2 instance severed/orphaned and
#' it will keep running without any connection to the R process. Because the instance
#' has a maximum lifetime set when launched, the instance won't keep running forever, and
#' will shutdown when the maximum is reached (or the ordinary execution on the instances finishes).
#' Nonetheless, the user will instead want to call [stop_job()], providing the job id
#' printed in the RStudio Jobs pane to send an interrupt to the child R process which will
#' trigger the termination of the running EC2 worker instance.
#'
#' @param script Path to the R script or R Markdown document to execute (on EFS storage)
#' @param memory Minimum memory (in GB) desired
#' @param cpu Minimum number of CPU cores desired
#' @param spot Logical, whether to run a spot instance
#' @param image_id id of the AMI to launch
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
#' @param job_name Name of the job
#' @param job_run_dir Working directory for the RStudio Job that is launched (should not need to change)
#' @export
run_job_wrapper <- function(script,
                            memory,
                            cpu,
                            spot,
                            image_id = Sys.getenv("AWS_R_IMAGE"),
                            efs_name = Sys.getenv("AWS_R_EFS_ID"),
                            security_group_id = Sys.getenv("AWS_R_SECURITY_GROUP"),
                            ssm_iam_profile = Sys.getenv("AWS_R_SSM_PROFILE"),
                            efs_mnt_pt = "/efs",
                            region = "us-west-2",
                            instance_max_hours = 24,
                            exec_timeout_hours = 24,
                            r_version = as.character(getRversion()),
                            ssm_work_dir = efs_mnt_pt,
                            log_file = NULL,
                            rmd_opts = list(),
                            job_name = script,
                            job_run_dir = getwd()) {
  checkmate::assert_choice(region, c("us-east-1", "us-east-2", "us-west-1", "us-west-2"))

  if (spot) {
    top <- get_optimal_spot(memory, cpu, region)
  } else {
    top <- get_optimal_ondemand(memory, cpu, region)
  }

  job_id <- digest::digest(
    paste(job_name, formatC(as.numeric(Sys.time()), digits = 6, format = "f")),
    serialize = FALSE
  )

  pars <- list(
    script = script,
    job_id = job_id,
    image_id = image_id,
    instance_type = top$InstanceType,
    spot = spot,
    spot_max_price = top$OnDemandPrice,
    efs_name = efs_name,
    security_group_id = security_group_id,
    ssm_iam_profile = ssm_iam_profile,
    efs_mnt_pt = efs_mnt_pt,
    region = region,
    instance_max_hours = instance_max_hours,
    exec_timeout_hours = exec_timeout_hours,
    r_version = r_version,
    ssm_work_dir = ssm_work_dir,
    log_file = log_file,
    rmd_opts = rmd_opts
  )

  pars_file <- fs::file_temp(ext = "rds")
  job_file <- fs::file_temp(ext = "R")

  job_script <- glue::glue(
    "pars <- readRDS('{pars_file}')\n",
    "try(fs::file_delete('{pars_file}'))\n",
    "try(fs::file_delete('{job_file}'))\n",
    "message('Job id: ', Sys.getenv('R_JOB_ID'))\n",
    "purrr::lift_dl(awsjobs::run_job)(pars)\n"
  )

  tryCatch({
    saveRDS(pars, pars_file)
    write(job_script, job_file)
    withr::with_envvar(
      list("R_JOB_ID" = job_id),
      rstudioapi::jobRunScript(job_file, name = job_name, workingDir = job_run_dir)
    )
    invisible(job_id)
  }, error = function(e) {
    try(fs::file_delete(pars_file))
    try(fs::file_delete(job_file))
    rlang::abort("Unable to run script")
  })
}

