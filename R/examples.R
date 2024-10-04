#' Example ID
#'
#' @description
#' This function returns examples of working IDs that can be used with the API.
#'
#' @param level Level of ID example to return: "publication" or "data set"
#' @param environment Environment to return a working example for
#' @param group Choose the publication group of examples to use. Can be "attendance".
#'
#' @return String containing an example ID present in the API
#' @export
#'
#' @examples
#' example_id()
example_id <- function(
    level = "dataset",
    environment = "dev",
    group = "public-api-testing") {
  example_ids <- data.frame(
    levels = c(
      "publication",
      "dataset",
      "location_id",
      "location_code",
      "filter",
      "filter_item",
      "indicator",
      "publication",
      "dataset",
      "location_id",
      "location_code",
      "filter",
      "filter_item",
      "indicator"
    ),
    environments = c(
      "dev",
      "dev",
      "dev",
      "dev",
      "dev",
      "dev",
      "dev",
      "dev",
      "dev",
      "dev",
      "dev",
      "dev",
      "dev",
      "dev"
    ),
    example_group = c(
      "attendance",
      "attendance",
      "attendance",
      "attendance",
      "attendance",
      "attendance",
      "attendance",
      "public-api-testing",
      "public-api-testing",
      "public-api-testing",
      "public-api-testing",
      "public-api-testing",
      "public-api-testing",
      "public-api-testing"
    ),
    examples = c(
      "b6d9ed96-be68-4791-abc3-08dcaba68c04",
      "7c0e9201-c7c0-ff73-bee4-304e731ec0e6",
      "NAT|id|dP0Zw",
      "NAT|code|E92000001",
      "4kdUZ",
      "5UNdi",
      "bqZtT",
      "d823e4df-626f-4450-9b21-08dc8b95fc02",
      "830f9201-9e11-ad75-8dcd-d2efe2834457",
      "LA|id|ml79K",
      "NAT|code|E92000001",
      "5mvdi",
      "HsQzL",
      "h8fyW"
    )
  )
  if (level == "all") {
    return(example_ids)
  } else {
    if (!(level %in% example_ids$levels)) {
      stop(
        paste0(
          "Non-valid element level received by validate_id.\n",
          "Should be one of:\n",
          paste(example_ids$levels, collapse = "\", \"")
        )
      )
    }
    return(
      example_ids |>
        dplyr::filter(
          example_ids$levels == level,
          example_ids$environments == environment,
          example_ids$example_group == group
        ) |>
        dplyr::pull("examples")
    )
  }
}

#' Create an example json query string
#' @description
#' Create an example json query string for use in examples and tests
#'
#' @return String containing an example json query
#' @export
#'
#' @examples
#' example_json_query() |> cat()
example_json_query <- function() {
  parse_tojson_params(
    indicators = example_id("indicator", group = "attendance"),
    time_periods = "2024|W23",
    geographies = c("NAT|id|dP0Zw", "REG|id|rg3Nj"),
    filter_items = list(
      attendance_status = c("pmRSo"),
      attendance_type = c("CvuId", "6AXrf"),
      education_phase = c("ThDPJ", "crH31"),
      day_number = c("uLQo4"),
      reason = c("bBrtT")
    )
  )
}

#' Create an example geography-query data frame
#'
#' @param level Query level within available options, can be one of \"nat_yorks\" or
#' \"nat_yorks_yorkslas\"
#'
#' @return Data frame containing an example geography query
#' @export
#'
#' @examples
#' example_geography_query()
example_geography_query <- function(level = "nat_yorks") {
  example_geography_queries <- list(
    nat_yorks =
      data.frame(
        return_level = c("NAT", "REG"),
        search_level = c("NAT", "REG"),
        identifier_type = c("code", "code"),
        identifier = c("E92000001", "E12000002")
      ),
    nat_yorks_yorkslas = data.frame(
      return_level = c("NAT", "REG", "LA"),
      search_level = c("NAT", "REG", "REG"),
      identifier_type = c("code", "code", "code"),
      identifier = c("E92000001", "E12000004", "E12000004")
    )
  )
  example_geography_queries |>
    magrittr::extract2(level)
}
