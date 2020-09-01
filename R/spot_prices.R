#' Get current spot prices
#'
#' Get the current spot prices for a collection of EC2 instance types in a given region.
#'
#' Note that this will not get the current spot price for the instance type in
#' every availability zone in a region. Once it gets the spot price for at least
#' one availability zone, it will move on. A further enhancement would be to get spot
#' prices at all availability zones (or a specified subset of availability zones).
#'
#' @param types Vector of instance types
#' @param region AWS EC2 region
#' @export
get_current_spot <- function(types, region) {
  ec2 <- paws::ec2(config = list(region = region))

  types_remain <- types

  price_df <- tibble::tibble(
    AvailabilityZone = character(0),
    InstanceType = character(0),
    ProductDescription = character(0),
    SpotPrice = numeric(0),
    Timestamp = as.POSIXct(character(0))
  )

  while (length(types_remain) > 0) {
    res <- ec2$describe_spot_price_history(
      InstanceTypes = as.list(types_remain),
      ProductDescriptions = list("Linux/UNIX")
    )

    if (length(res[[2]]) == 0) break

    prices_next <- res[[2]] %>%
      purrr::map_dfr(tibble::as_tibble) %>%
      dplyr::group_by(InstanceType, AvailabilityZone) %>%
      dplyr::filter(dplyr::row_number(dplyr::desc(Timestamp)) == 1) %>%
      dplyr::ungroup() %>%
      dplyr::mutate_at(dplyr::vars(SpotPrice), as.numeric)

    price_df <- dplyr::bind_rows(price_df, prices_next)

    types_remain <- setdiff(types_remain, price_df$InstanceType)
  }
  price_df
}

#' Get instance prices
#'
#' Get the on-demand and current spot prices for a set of EC2 instances.
#' If the user provides memory (in GB) and CPU cores requested, it will get the
#' on-demand and spot prices for instances with at least that much memory and CPU.
#' If the user provides a universe of instances, it will get prices at that set of instances.
#'
#' @param memory Minimum memory (in GB) requested
#' @param cpu Minimum number of CPU cores requested
#' @param region AWS EC2 region
#' @param universe Universe of instances to consider. If null, will use memory and cpu to get the instance universe.
#' @export
get_instance_resource_prices <- function(memory,
                                         cpu,
                                         region,
                                         universe = NULL) {
  checkmate::assert_number(memory, lower = 1, upper = 128)
  checkmate::assert_number(cpu, lower = 1, upper = 48)
  checkmate::assert_choice(region, aws_regions$region)

  if (is.null(universe)) {
    universe <- get_instance_universe(memory, cpu, region)
  }

  if (nrow(universe) == 0) stop("No instances satisfying conditions")

  spot_prices <- get_current_spot(universe$instanceType, region = region)

  spot_prices %>%
    dplyr::left_join(
      universe %>%
        dplyr::select(InstanceType = instanceType, memory, vcpu, OnDemandPrice = price),
      by = "InstanceType"
    ) %>%
    dplyr::arrange(SpotPrice)
}

#' Get EC2 instance universe
#'
#' Get a data frame of EC2 instances with greater than or equal to a given
#' amount of memory and CPU in a given EC2 region.
#'
#' Only instances types in the c5, m5, m5a, r5, r5a, t3, and t3a classes with less than
#' 128GB of memory are included. For t3/t3a, only instances with less than or equal
#' to 4GB of memory are included. The 128GB memory cap is included to prevent inadvertently
#' choosing a massive instance.
#' @param memory Minimum memory (in GB) requested
#' @param cpu Minimum number of CPU cores requested
#' @param region AWS EC2 region
#' @export
get_instance_universe <- function(memory,
                                  cpu,
                                  region) {
  instance_list %>%
    dplyr::mutate(prefix = stringr::str_extract(instanceType, "^[:alnum:]+")) %>%
    dplyr::filter(
      currentGeneration == "Yes",
      prefix %in% c("c5", "m5", "m5a", "r5", "r5a", "t3", "t3a"),
      memory <= fs::as_fs_bytes("128G"),
      prefix %in% c("c5", "m5", "m5a", "r5", "r5a") | memory <= fs::as_fs_bytes("4G"),
      region == !!region
    ) %>%
    dplyr::filter(
      memory >= fs::as_fs_bytes(paste(!!memory, "G")),
      vcpu >= !!cpu
    )
}

#' Get optimal spot instance
#'
#' The user provides a desired minimum amount of memory and CPU, along with a region.
#' The function will find the instance with the lowest spot price that has at least
#' that much memory and CPU.
#'
#' @param memory Minimum memory (in GB) requested
#' @param cpu Minimum number of CPU cores requested
#' @param region AWS EC2 region
#' @param universe Universe of instances to consider. If null, will use memory and cpu to get the instance universe.
#' @return A list with information about the lowest-priced instance satisfying the request
#' @export
get_optimal_spot <- function(memory,
                             cpu,
                             region,
                             universe = NULL) {
  prices <- get_instance_resource_prices(memory, cpu, region, universe)

  top <- prices %>%
    dplyr::slice(1) %>%
    as.list()

  if (top$SpotPrice > top$OnDemandPrice) {
    stop(
      glue::glue(
        "Spot price ({spot}) exceeds on-demand price ({price}) for {instance} instance",
        spot = top$SpotPrice,
        price = top$OnDemandPrice,
        instance = top$InstanceType
      )
    )
  }

  top
}

#' Get optimal on-demand instance
#'
#' The user provides a desired minimum amount of memory and CPU, along with a region.
#' The function will find the lowest price on-demand instance having at least
#' that much memory and CPU.
#'
#' @param memory Minimum memory (in GB) requested
#' @param cpu Minimum number of CPU cores requested
#' @param region AWS EC2 region
#' @param universe Universe of instances to consider. If null, will use memory and cpu to get the instance universe.
#' @return A list with information about the lowest-priced instance satisfying the request
#' @export
get_optimal_ondemand <- function(memory,
                                 cpu,
                                 region,
                                 universe = NULL) {
  get_instance_universe(memory, cpu, region) %>%
    dplyr::arrange(price) %>%
    dplyr::slice(1) %>%
    dplyr::transmute(
      InstanceType = instanceType,
      OnDemandPrice = price
    ) %>%
    as.list()
}

