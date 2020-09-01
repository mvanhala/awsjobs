
## get a list of EC2 instances in the us-east-1, us-east-2, us-west-1, and us-west-2 region
## to be used as internal data


# AWS regions: https://docs.aws.amazon.com/general/latest/gr/rande.html#ec2_region

library(dplyr)

aws_regions <- tribble(
  ~region, ~region_name,
  "us-east-2", "US East (Ohio)",
  "us-east-1", "US East (N. Virginia)",
  "us-west-1", "US West (N. California)",
  "us-west-2", "US West (Oregon)",
  "ap-east-1", "Asia Pacific (Hong Kong)",
  "ap-south-1", "Asia Pacific (Mumbai)",
  "ap-northeast-3", "Asia Pacific (Osaka-Local)",
  "ap-northeast-2", "Asia Pacific (Seoul)",
  "ap-southeast-1", "Asia Pacific (Singapore)",
  "ap-southeast-2", "Asia Pacific (Sydney)",
  "ap-northeast-1", "Asia Pacific (Tokyo)",
  "ca-central-1", "Canada (Central)",
  "eu-central-1", "Europe (Frankfurt)",
  "eu-west-1", "Europe (Ireland)",
  "eu-west-2", "Europe (London)",
  "eu-west-3", "Europe (Paris)",
  "eu-north-1", "Europe (Stockholm)",
  "me-south-1", "Middle East (Bahrain)",
  "sa-east-1", "South America (São Paulo)"
)

extract_price <- function(list) {
  if (any(names(list) == "pricePerUnit")) return(as.numeric(list$pricePerUnit$USD))
  if (!is.list(list) || length(list) == 0) return(character(0))
  purrr::flatten_dbl(purrr::map(list, extract_price))
}

get_instance_types <- function(region) {
  checkmate::assert_choice(region, aws_regions$region)

  region_name <- filter(aws_regions, region == !!region)$region_name

  pricing <- paws::pricing(config = list(region = "us-east-1"))

  inst_list <- list()
  i <- 1
  token <- NULL

  while (TRUE) {
    inst_list[[i]] <- pricing$get_products(
      ServiceCode = "AmazonEC2",
      Filters = list(
        list(
          Field = "location",
          Type = "TERM_MATCH",
          Value = region_name
        ),
        list(Field = "operatingSystem", Type = "TERM_MATCH", Value = "Linux"),
        list(Field = "tenancy", Type = "TERM_MATCH", Value = "Shared"),
        list(Field = "operation", Type = "TERM_MATCH", Value = "RunInstances"),
        list(Field = "capacitystatus", Type = "TERM_MATCH", Value = "Used")
      ),
      NextToken = token
    )
    token <- inst_list[[i]]$NextToken
    if (length(token) == 0) break
    i <- i + 1
  }

  inst_parsed <- inst_list %>%
    purrr::map("PriceList") %>%
    purrr::flatten() %>%
    purrr::map(jsonlite::fromJSON)

  inst_parsed %>%
    purrr::map_dfr(
      function(record) {
        tibble::as_tibble(record$product$attributes) %>%
          dplyr::mutate(
            sku = record$product$sku,
            price = extract_price(record$terms$OnDemand),
            region = !!region
          ) %>%
          dplyr::select(
            instanceType, instanceFamily, currentGeneration,
            memory, vcpu, price,
            region,
            region_name = location,
            everything()
          )
      }
    ) %>%
    dplyr::mutate(memory = fs::as_fs_bytes(memory), vcpu = as.numeric(vcpu))
}

instance_list <- purrr::map_dfr(
  c("us-east-1", "us-east-2", "us-west-1", "us-west-2"),
  get_instance_types
)

usethis::use_data(
  aws_regions,
  instance_list,
  internal = TRUE,
  overwrite = TRUE
)

