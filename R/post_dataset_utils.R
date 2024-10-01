#' Create a json query string
#'
#' @description
#' A short description...
#'
#' @inheritParams api_url
#' @inheritParams parse_tojson_geographies
#'
#' @return String containing json query body for use with http POST request
#' @export
#'
#' @examples
#' parse_tojson_params(example_id("indicator")) |>
#'   cat()
#'
#' parse_tojson_params(
#'   example_id("indicator"),
#'   time_periods = "2024|W23",
#'   geographies = c("NAT|id|dP0Zw", "REG|id|rg3Nj"),
#'   filter_items = c("pmRSo", "7SdXo")
#' ) |>
#'   cat()
#'
#' # Create a geographies data frame to find both of:
#' #   - England national level data
#' #   - all LAs in a specified region ("E12000004")
#' dfgeographies <- data.frame(
#'   return_level = c("NAT", "LA"),
#'   search_level = c("NAT", "REG"),
#'   identifier_type = c("code", "code"),
#'   identifier = c("E92000001", "E12000004")
#' )
#'
#' parse_tojson_params(
#'   example_id("indicator"),
#'   time_periods = "2024|W23",
#'   geographies = dfgeographies,
#'   filter_items = c("pmRSo")
#' ) |>
#'   cat()
parse_tojson_params <- function(
    indicators,
    time_periods = NULL,
    geographies = NULL,
    filter_items = NULL,
    page = 1,
    page_size = 1000) {
  # Set some default strings
  bridge <- "\n  ]\n},"
  indicators_str <- "\n\"indicators\": [\n  \"bqZtT\"\n]"
  debug_str <- ",\n\"debug\": true"
  pages_str <- paste0(
    ",\n\"page\": ",
    page,
    ",\n\"pageSize\": ",
    page_size,
    "1000\n}"
  )

  json_query <- paste0(
    "{\n",
    ifelse(
      any(!is.null(c(time_periods, geographies, filter_items))),
      paste0(
        "\"criteria\": {\n  \"and\": [\n",
        paste(
          eesyapi::parse_tojson_time_periods(time_periods),
          eesyapi::parse_tojson_geographies(geographies),
          eesyapi::parse_tojson_filter_in(filter_items, filter_type = "filter_items"),
          sep = ",\n"
        ) |>
          stringr::str_replace_all(",\\n,\\n,\\n|,\\n,\\n", ",\\\n") |>
          stringr::str_remove_all("^,\\n|,\\n$"),
        bridge
      ),
      ""
    ),
    indicators_str,
    debug_str,
    pages_str
  )
  return(json_query)
}

#' Parse time_periods to json
#'
#' @description
#' Create a json query sub-string based on time periods constraints
#'
#' @inheritParams api_url
#'
#' @return String containing json form query for time periods
#' @export
#'
#' @examples
#' parse_tojson_time_periods(c("2023|W25", "2024|W12"))
parse_tojson_time_periods <- function(time_periods) {
  if (!is.null(time_periods)) {
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
  } else {
    NULL
  }
}

#' Parse geographic levels to json
#'
#' @description
#' Create a json query sub-string based on geographic levels constraints
#'
#' @inheritParams api_url
#' @inheritParams parse_tourl_filter_in
#'
#' @return String containing json form query for geographic levels
#' @export
#'
#' @examples
#' parse_tojson_filter_in(c("NAT", "REG"), filter_type = "geographic_levels")
parse_tojson_filter_in <- function(filter_items, filter_type = "filter_items") {
  eesyapi::validate_ees_filter_type(filter_type)
  api_filter_type <- eesyapi::to_api_filter_type(filter_type)
  if (!is.null(filter_items)) {
    paste0(
      "    {\n      \"",
      api_filter_type,
      "\": {\n        \"in\": [\n          \"",
      paste0(filter_items, collapse = "\",\n          \""),
      "\"\n        ]\n      }\n    }"
    )
  } else {
    NULL
  }
}

#' Parse geographic levels to json
#'
#' @description
#' Create a json query sub-string based on geographic levels constraints
#'
#' @inheritParams api_url
#' @inheritParams parse_tourl_filter_in
#'
#' @return String containing json form query for geographic levels
#' @export
#'
#' @examples
#' parse_tojson_filter_eq("NAT", filter_type = "geographic_levels") |> cat()
parse_tojson_filter_eq <- function(filter_items, filter_type = "filter_items") {
  eesyapi::validate_ees_filter_type(filter_type)
  api_filter_type <- eesyapi::to_api_filter_type(filter_type)
  if (!is.null(filter_items)) {
    paste0(
      "        {\n          \"",
      api_filter_type,
      "\": {\n            \"eq\": \"",
      filter_items,
      "\"\n          }\n        }"
    )
  } else {
    NULL
  }
}

#' Parse geographies to json
#'
#' @description
#' Create a json query sub-string based on location constraints
#'
#' @param geographies String, vector or data frame containing the geographic levels and
#' locations to be queried.
#'
#' @return String containing json form query for geographies
#' @export
#'
#' @examples
#' parse_tojson_geographies(c("NAT|id|dP0Zw", "REG|id|rg3Nj")) |>
#'   cat()
parse_tojson_geographies <- function(geographies) {
  if (is.null(geographies)) {
    return(NULL)
  } else if (is.vector(geographies) || is.character(geographies)) {
    geographies <- geographies |>
      stringr::str_split("\\|", simplify = TRUE) |>
      as.data.frame() |>
      dplyr::rename(search_level = "V1", identifier_type = "V2", identifier = "V3")
    geographies <- geographies |>
      dplyr::mutate(
        return_level = geographies |>
          dplyr::pull("search_level")
      )
  } else if (is.data.frame(geographies)) {
    if (
      !all(
        c(
          "return_level",
          "search_level",
          "identifier_type",
          "identifier"
        ) %in%
          colnames(geographies)
      )
    ) {
      stop("The column \"search_level\" is required in the geographies data frame.")
    }
  } else {
    stop("The geographies parameter should be given as either a data frame, vector or string.")
  }
  paste0(
    "    {\n      \"or\": [\n",
    paste0(
      "        {\n          \"and\": [\n",
      parse_tojson_filter_eq(
        geographies |>
          dplyr::pull("return_level"),
        filter_type = "geographic_levels"
      ),
      ",\n    {\n      \"locations\": {\n        \"in\": [\n",
      "          {\n            \"level\": \"",
      geographies |> dplyr::pull("search_level"),
      "\",\n            \"",
      geographies |> dplyr::pull("identifier_type"),
      "\": \"",
      geographies |> dplyr::pull("identifier"),
      "\"\n          }\n        ]\n      }\n    }\n  ]\n  }",
      collapse = ",\n"
    ),
    "\n    ]\n  }"
  )
}
