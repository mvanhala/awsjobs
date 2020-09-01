
to_na <- function(x) if (length(x) == 0) NA_character_ else x

tags_to_list <- function(tags) {
  rlang::set_names(purrr::map(tags, "Value"), purrr::map_chr(tags, "Key"))
}

#' List running R jobs
#'
#' List R jobs running on EC2 worker instances in a given region. It gets a list of
#' all running EC2 instances, applying a filter on the `JOB_TYPE` tag associated
#' with the EC2 instances. The `JOB_TYPE` tag is set to `R worker job` when a job is
#' launched with [run_job()].
#'
#' @param region AWS region
#' @param states Instance states to include
#' @return A data frame with information about running R jobs
#' @export
list_r_job_instances <- function(region,
                                 states = c("running", "pending")) {
  svc_ec2 <- paws::ec2(config = list(region = region))
  result <- svc_ec2$describe_instances(
    Filters = list(
      list(
        Name = "tag:JOB_TYPE",
        Values = list("R worker job")
      )
    )
  )
  instances <- result$Reservations %>%
    purrr::map("Instances") %>%
    purrr::map(1) %>%
    purrr::keep(~ .$State$Name %in% states)

  if (length(instances) == 0) {
    instance_df <- tibble::tibble(
      InstanceId = character(0),
      ImageId = character(0),
      InstanceType = character(0),
      LaunchTime = as.POSIXct(character(0)),
      PrivateIpAddress = character(0),
      State = character(0),
      SubnetId = character(0),
      IamInstanceProfile = character(0),
      InstanceLifecycle = character(0),
      SecurityGroup = character(0),
      SpotInstanceRequestId = character(0),
      tags = list()
    )
  } else {
    instance_df <- instances %>%
      purrr::map_dfr(
        ~list(
          InstanceId = .$InstanceId,
          ImageId = .$ImageId,
          InstanceType = .$InstanceType,
          LaunchTime = lubridate::with_tz(.$LaunchTime, Sys.timezone()),
          PrivateIpAddress = .$PrivateIpAddress,
          State = .$State$Name,
          SubnetId = .$SubnetId,
          IamInstanceProfile = to_na(.$IamInstanceProfile$Id),
          InstanceLifecycle = .$InstanceLifecycle,
          SecurityGroup = .$SecurityGroups[[1]]$GroupId,
          SpotInstanceRequestId = to_na(.$SpotInstanceRequestId),
          tags = list(.$Tags)
        )
      )
  }

  spot_req <- svc_ec2$describe_spot_instance_requests(
    SpotInstanceRequestIds = instance_df$SpotInstanceRequestId
  )

  spot_req_df <- spot_req$SpotInstanceRequests %>%
    purrr::map_dfr(
      ~list(
        SpotInstanceRequestId = .$SpotInstanceRequestId,
        SpotPrice = as.numeric(.$SpotPrice)
      )
    )

  if (nrow(spot_req_df) == 0) {
    spot_req_df <-  tibble::tibble(
      SpotInstanceRequestId = character(0),
      SpotPrice = numeric(0)
    )
  }

  instance_df %>%
    dplyr::left_join(spot_req_df, by = "SpotInstanceRequestId") %>%
    dplyr::mutate(tags = purrr::map(tags, tags_to_list)) %>%
    tidyr::hoist(
      tags,
      "Name" = "Name",
      "R_JOB_ID" = "R_JOB_ID",
      "JOB_TYPE" = "JOB_TYPE",
      .ptype = list(Name = "", R_JOB_ID = "", JOB_TYPE = "")
    )
}

