#' Create a json query
#'
#' @description
#' Creates a json query for use when POST-ing a query to the API. This takes time period,
#' geography, filter item and indicator criteria and produces a working json query as a
#' single string. The result can be used directly by post_dataset() or the output of
#' `parse_tojson_params(...) |> cat()` can be copied and pasted as the "body" content in
#' other API connection software (such as Postman) to POST a query to the EES API.
#'
#' @inheritParams api_url
#' @inheritParams parse_tojson_geographies
#' @param debug Run POST query in debug mode. Logical, default = FALSE
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
#'
#' # Create a filter list to find the combination of:
#' #   - day_number is in c("uLQo4", "qf0jG", "aMjLP") *and*
#' #   - reason is in c("bBrtT", "ThjPJ", "hsHyW", "m2m9K") *and*
#' #   - education_phase is in c("5UNdi", "crH31")
#' filter_list <- list(
#'   day_number = c("uLQo4", "qf0jG", "aMjLP"),
#'   reason = c("bBrtT", "ThjPJ", "hsHyW", "m2m9K"),
#'   education_phase = c("5UNdi", "crH31")
#' )
#'
#' parse_tojson_params(
#'   example_id("indicator"),
#'   time_periods = "2024|W23",
#'   geographies = "NAT|code|E92000001",
#'   filter_items = filter_list
#' ) |>
#'   cat()
#'
parse_tojson_params <- function(
    indicators,
    time_periods = NULL,
    geographies = NULL,
    filter_items = NULL,
    page = 1,
    page_size = 1000,
    debug = FALSE,
    verbose = FALSE) {
  # Set some default strings
  bridge <- "\n  ]\n},"
  debug_str <- paste(",\n\"debug\":", debug) |> tolower()
  pages_str <- paste0(
    ",\n\"page\": ",
    ifelse(is.null(page), 1, page),
    ",\n\"pageSize\": ",
    page_size,
    "\n}"
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
          eesyapi::parse_tojson_filter(filter_items, filter_type = "filter_items"),
          sep = ",\n"
        ) |>
          stringr::str_replace_all(",\\n,\\n,\\n|,\\n,\\n", ",\\\n") |>
          stringr::str_remove_all("^,\\n|,\\n$"),
        bridge
      ),
      ""
    ),
    parse_tojson_indicators(indicators),
    debug_str,
    pages_str
  )
  if (verbose) {
    json_query |> cat()
  }
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
  eesyapi::validate_time_periods(time_periods)
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

#' Parse a combination-filter query to json
#'
#' @description
#' Create a json query sub-string based on a combination \"in\" and \"and\" constraints
#'
#' @inheritParams parse_tourl_filter_in
#' @return String containing json form query with \"and\"-combination of different filter
#' selections
#' @export
#'
#' @examples
#' parse_tojson_filter(
#'   list(
#'     day_number = c("uLQo4", "qf0jG", "aMjLP"),
#'     reason = c("bBrtT", "ThjPJ", "hsHyW", "m2m9K"),
#'     education_phase = c("5UNdi", "crH31")
#'   )
#' ) |>
#'   cat()
parse_tojson_filter <- function(items, filter_type = "filter_items") {
  eesyapi::validate_ees_filter_type(filter_type)
  if (is.list(items)) {
    # If items is a list, then process it as a combination separate "in" queries
    paste0(
      "{\n\"and\": [\n",
      sapply(items, parse_tojson_filter_in, filter_type) |>
        paste(collapse = ",\n"), "\n]\n}"
    )
  } else if (is.vector(items)) {
    # If items is a vector, then revert to just a single "in" query
    parse_tojson_filter_in(items)
  } else {
    NULL
  }
}

#' Parse a filter-in type query to json
#'
#' @description
#' Create a json query sub-string based on filter-in constraints
#'
#' @inheritParams parse_tourl_filter_in
#'
#' @return String containing json form query based on filter-in constraints
#' @export
#'
#' @examples
#' parse_tojson_filter_in(c("NAT", "REG"), filter_type = "geographic_levels")
parse_tojson_filter_in <- function(items, filter_type = "filter_items") {
  eesyapi::validate_ees_filter_type(filter_type)
  if (!is.null(items)) {
    api_filter_type <- eesyapi::convert_api_filter_type(filter_type)
    paste0(
      "    {\n      \"",
      api_filter_type,
      "\": {\n        \"in\": [\n          \"",
      paste0(items, collapse = "\",\n          \""),
      "\"\n        ]\n      }\n    }"
    )
  } else {
    NULL
  }
}

#' Parse a filter-equal type query to json
#'
#' @description
#' Create a json query sub-string based on filter-equal constraints
#'
#' @inheritParams parse_tourl_filter_in
#'
#' @return String containing json form query based on filter-equal-to constraints
#' @export
#'
#' @examples
#' parse_tojson_filter_eq("NAT", filter_type = "geographic_levels") |> cat()
parse_tojson_filter_eq <- function(items, filter_type = "filter_items") {
  eesyapi::validate_ees_filter_type(filter_type)
  if (!is.null(items)) {
    api_filter_type <- eesyapi::convert_api_filter_type(filter_type)
    paste0(
      "        {\n          \"",
      api_filter_type,
      "\": {\n            \"eq\": \"",
      items,
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
#' parse_tojson_geographies(c("NAT", "REG")) |>
#'   cat()
#' parse_tojson_geographies(c("NAT|id|dP0Zw", "REG|id|rg3Nj")) |>
#'   cat()
#' parse_tojson_geographies(c("NAT|id|dP0Zw", "REG")) |>
#'   cat()
#' parse_tojson_geographies(c("NAT|id|dP0Zw", "REG")) |>
#'   cat()
parse_tojson_geographies <- function(geographies) {
  if (is.null(geographies)) {
    return(NULL)
  } else if (is.vector(geographies) || is.character(geographies)) {
    geographies <- geographies |>
      stringr::str_split("\\|", simplify = TRUE) |>
      as.data.frame()
    if (ncol(geographies) == 1) {
      geographies <- geographies |>
        dplyr::mutate(
          V2 = "",
          V3 = ""
        )
    }
    geographies <- geographies |>
      dplyr::rename(search_level = "V1", identifier_type = "V2", identifier = "V3") |>
      dplyr::mutate(return_level = !!rlang::sym("search_level"))
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
      parse_tojson_location(geographies, include_comma = TRUE),
      "\n  ]\n  }",
      collapse = ",\n"
    ),
    "\n    ]\n  }"
  )
}

#' Create json location search string from geographies
#'
#' @param geographies Vector or data frame of search geographies
#' @param include_comma Include a comma before return strings (logical)
#'
#' @return Vector of strings containing json location search string
#' @export
#'
#' @examples
#' parse_tojson_location(example_geography_query()) |> cat()
parse_tojson_location <- function(geographies, include_comma = FALSE) {
  comma_string <- ifelse(include_comma, ",", "")
  location_json <- geographies |>
    dplyr::mutate(
      location_json = dplyr::if_else(
        !!rlang::sym("identifier_type") != "",
        paste0(
          comma_string,
          "\n    {\n      \"locations\": {\n        \"in\": [\n",
          "          {\n            \"level\": \"",
          !!rlang::sym("search_level"),
          "\",\n            \"",
          !!rlang::sym("identifier_type"),
          "\": \"",
          !!rlang::sym("identifier"),
          "\"\n          }\n        ]\n      }\n    }"
        ),
        ""
      )
    ) |>
    dplyr::pull("location_json")
  return(location_json)
}


#' Parse an indicator-in type query to json
#'
#' @description
#' Create a json query sub-string based on indicator-in constraints
#'
#' @param indicators String or vector of strings containing indicator ids
#'
#' @return A json query string to select a set of indicators
#' @export
#'
#' @examples
#' parse_tojson_indicators(example_id("indicator")) |>
#'   cat()
parse_tojson_indicators <- function(indicators) {
  eesyapi::validate_ees_id(indicators, level = "indicator")
  paste0(
    "\n\"indicators\": [\n  \"",
    paste0(
      indicators,
      collapse = "\",\n  \""
    ),
    "\"\n]"
  )
}
