#' Create a json query string
#'
#' @description
#' A short description...
#'
#' @inheritParams api_url
#'
#' @return String containing json query body for use with http POST request
#' @export
#'
#' @examples
#' parse_params_to_json(example_id("indicator"))
parse_params_to_json <- function(
    indicators,
    time_periods = NULL,
    geographic_levels = NULL,
    locations = NULL,
    filter_items = NULL,
    page = 1,
    page_size = 1000) {
  filter_items_str <- "  {\n      \"filters\": {\n        \"eq\": \"jYyAM\"\n      }\n    }\n"
  bridge <- "]\n},"
  indicators_str <- "\n\"indicators\": [\n  \"bqZtT\"\n]"
  debug_str <- ",\n\"debug\": true"
  pages_str <- ",\n\"page\": 1,\n\"pageSize\": 1000\n}"

  json_query <- paste0(
    "{\n",
    ifelse(
      any(!is.null(c(time_periods, geographic_levels, locations, filter_items))),
      "\"criteria\": {\n  \"and\": [\n    ",
      ""
    ),
    ifelse(
      !is.null(geographic_levels),
      eesyapi::parse_geographic_levels_to_json(geographic_levels),
      ""
    ),
    ifelse(
      !is.null(geographic_levels),
      ",\n",
      ""
    ),
    ifelse(
      !is.null(time_periods),
      eesyapi::parse_time_periods_to_json(time_periods),
      ""
    ),
    ",",
    filter_items_str,
    bridge,
    indicators_str,
    debug_str,
    pages_str
  )
  return(json_query)
}

#' Parse time_periods to json
#'
#' @description
#' Create a json query sub-string based on geographic levels constraints
#'
#' @inheritParams api_url
#'
#' @return String containing json form query for geographic levels
#' @export
#'
#' @examples
#' parse_time_periods_to_json(c("2023|W25", "2024|W12"))
parse_time_periods_to_json <- function(time_periods) {
  df_time_periods <- time_periods |>
    stringr::str_split("\\|", simplify = TRUE) |>
    as.data.frame() |>
    dplyr::rename(period = "V1", code = "V2")
  paste0(
    "    {\n      \"timePeriods\": {\n        \"in\": [\n",
    paste0(
      "          {\n            \"period\": \"",
      df_time_periods$period,
      "\",\n            \"code\": \"",
      df_time_periods$code,
      "\"\n          }",
      collapse = ",\n"
    ),
    "\n        ]\n      }\n    }"
  )
}

#' Parse geographic levels to json
#'
#' @description
#' Create a json query sub-string based on geographic levels constraints
#'
#' @inheritParams api_url
#'
#' @return String containing json form query for geographic levels
#' @export
#'
#' @examples
#' parse_geographic_levels_to_json(c("NAT", "REG"))
parse_geographic_levels_to_json <- function(geographic_levels) {
  paste0(
    "{\n      \"geographicLevels\": {\n        \"in\": [\n          \"",
    paste0(geographic_levels, collapse = "\",\n          \""),
    "\"\n        ]\n      }\n    }"
  )
}
