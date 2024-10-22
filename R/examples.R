#' Example ID
#' @description
#' This function returns examples of working IDs that can be used with the eesyapi functions, such
#' as query_dataset().
#'
#' @param level Level of ID example to return. A range are available, although not every example
#' group necessarily contains all possible examples. The following are generally available.
#'   - "all": Return full list of example options for given group.
#'   - "publication": Return example publication ID
#'   - "dataset": Return example data set ID
#'   - "time_period" / "time_periods": Return example time_period(s)
#'   - "location_id" / "location_ids": Return example location ID(s)
#'   - "location_code / location_codes": Return example location code(s)
#'   - "filter": Return example filter column ID
#'   - "filter_item" / "filter_items_short" / "filter_items_long": Return example filter ID or
#'     example short / long filter query list.
#'   - "indicator": Return example indicator ID
#' @param ees_environment Environment to return a working example for: "dev" or "test"
#' @param group Choose the publication group of examples to use. Options are:
#'   - "attendance": Large example data set, careful what you ask for
#'   - "public-api-testing": Smaller example data set
#'
#' @return String, vector or list containing example ID(s) present in the API
#' @export
#'
#' @examples
#' example_id("all")
#' example_id()
#' example_id("publication")
#' example_id("publication", group = "attendance")
#' example_id("time_period", group = "attendance")
#' example_id("location_ids", group = "attendance")
#' example_id("filter_items_short", group = "attendance")
#' example_id("indicator", group = "attendance")
example_id <- function(
    level = "dataset",
    ees_environment = "dev",
    group = "public-api-testing") {
  example_id_list <- list(
    attendance = list(
      dev = list(
        publication = "b6d9ed96-be68-4791-abc3-08dcaba68c04",
        dataset = "7c0e9201-c7c0-ff73-bee4-304e731ec0e6",
        time_period = "2024|W23",
        time_periods = c("2024|W21", "2024|W23"),
        location_id = "NAT|id|dP0Zw",
        location_ids = c("NAT|id|dP0Zw", "REG|id|rg3Nj"),
        location_code = "NAT|code|E92000001",
        location_codes = c("REG|code|E12000001", "REG|code|E12000002"),
        filter = "4kdUZ",
        filter_item = "5UNdi",
        filter_items_long = list(
          attendance_status = c("pmRSo", "7SdXo"),
          attendance_type = c("CvuId", "6AXrf", "0k3T5", "YdkHK"),
          education_phase = c("ThDPJ", "crH31"),
          day_number = c("uLQo4"),
          reason = c("bBrtT")
        ),
        filter_items_short = list(
          attendance_status = c("pmRSo"),
          attendance_type = c("CvuId", "6AXrf"),
          education_phase = c("ThDPJ", "crH31"),
          day_number = c("uLQo4"),
          reason = c("bBrtT")
        ),
        indicator = "bqZtT"
      )
      # TODO: add in test pupil attendance details here
    ),
    `public-api-testing` = list(
      dev = list(
        publication = "d823e4df-626f-4450-9b21-08dc8b95fc02",
        dataset = "830f9201-9e11-ad75-8dcd-d2efe2834457",
        location_id = "LA|id|ml79K",
        location_code = "NAT|code|E92000001",
        location_codes = c("REG|code|E12000001", "REG|code|E12000002"),
        filter = "01tT5",
        filter_item = "wEZcb",
        indicator = "PbNeb"
      ),
      test = list(
        publication = "25d0e40b-643a-4f73-3ae5-08dcf1c4d57f",
        dataset = "e1ae9201-2fff-d376-8fa3-bd3c3660d4c8",
        location_id = "NAT|id|mRj9K",
        location_code = "NAT|code|E92000001",
        filter = "arLPb",
        filter_item = "VN5XE",
        filter_items = c("VN5XE", "PEebW"),
        indicator = "dPe0Z",
        indicators = c("OBXCL", "7YFXo")
      )
    )
  )
  if (!(group %in% names(example_id_list))) {
    stop(paste0("Chosen group (", group, ") not found in examples list."))
  }
  if (!(ees_environment %in% c("dev", "test"))) {
    stop(paste0("Chosen ees_environment (", ees_environment, ") should be one of: dev or test."))
  }

  group_examples <- example_id_list |>
    magrittr::extract2(group) |>
    magrittr::extract2(ees_environment)

  if (any(level == "all")) {
    return(group_examples)
  } else {
    if (any(!(level %in% names(group_examples)))) {
      stop(
        paste0(
          "Non-valid element level received by validate_id.\n",
          "Should be one of:\n\"",
          paste(names(group_examples), collapse = "\", \""),
          "\"."
        )
      )
    }
    return(
      if (length(level) > 1) {
        group_examples |>
          magrittr::extract(level) |>
          unlist()
      } else {
        group_examples |>
          magrittr::extract2(level)
      }
    )
  }
}

#' Example raw data
#'
#' @description
#' Download some example raw data. Mainly intended for use in developing / testing the sqid parsing
#' or as an example of getting raw data if any end users would prefer to do the sqid parsing
#' themselves.
#'
#' @inheritParams example_id
#' @param size Number of rows to return (max = 1000)
#'
#' @return Nested list form of example data from the API
#' @export
#'
#' @examples
#' example_data_raw()
example_data_raw <- function(
    group = "attendance",
    size = 32) {
  eesyapi::api_url(
    "get-data",
    dataset_id = example_id(group = group),
    indicators = example_id("indicator", group = group),
    page = 1, page_size = size
  ) |>
    httr::GET() |>
    httr::content("text") |>
    jsonlite::fromJSON() |>
    magrittr::use_series("results")
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
  eesyapi::parse_tojson_params(
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
        identifier = c("E92000001", "E12000003")
      ),
    nat_yorks_yorkslas = data.frame(
      return_level = c("NAT", "REG", "LA"),
      search_level = c("NAT", "REG", "REG"),
      identifier_type = c("code", "code", "code"),
      identifier = c("E92000001", "E12000003", "E12000003")
    )
  )
  example_geography_queries |>
    magrittr::extract2(level)
}
