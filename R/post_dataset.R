#' Query a data set using POST and a query json
#'
#' @description
#' This function provides a method for generating and sending a json based data query to the
#' EES API. As a minimum, it requires the dataset_id flag and either the indicators flag or
#' a json file containing a query to be provided.
#'
#' @inheritParams api_url
#' @inheritParams parse_tojson_params
#' @param json_query Optional path to a json file containing the query parameters
#' @param parse Logical flag to activate parsing of the results. Default: TRUE
#'
#' @return Data frame containing query results of an API data set
#' @export
#'
#' @examples
#' post_dataset(
#'   example_id(group = "attendance"),
#'   json_query = example_json_query()
#' )
#'
#' # Run post_dataset() to select rows containing either of two geographic locations and either of
#' # two filter items.
#' post_dataset(
#'   example_id(group = "attendance"),
#'   indicators = example_id("indicator", group = "attendance"),
#'   time_periods = "2024|W23",
#'   geographies = c("NAT|id|dP0Zw", "REG|id|rg3Nj"),
#'   filter_items = c("CvuId", "6AXrf"),
#'   page = 1,
#'   page_size = 32
#' )
#'
#' # Run post_dataset() using set parameters giving a combination of filter options
#' post_dataset(
#'   example_id(group = "attendance"),
#'   indicators = example_id("indicator", group = "attendance"),
#'   time_periods = "2024|W23",
#'   geographies = c("NAT|id|dP0Zw", "REG|id|rg3Nj"),
#'   filter_items = list(
#'     attendance_status = c("pmRSo", "7SdXo"),
#'     attendance_type = c("CvuId", "6AXrf", "0k3T5", "YdkHK"),
#'     education_phase = c("ThDPJ", "crH31"),
#'     day_number = c("uLQo4"),
#'     reason = c("bBrtT")
#'   )
#' )
post_dataset <- function(
    dataset_id,
    indicators = NULL,
    time_periods = NULL,
    geographies = NULL,
    filter_items = NULL,
    json_query = NULL,
    dataset_version = NULL,
    ees_environment = NULL,
    api_version = NULL,
    page = NULL,
    page_size = 1000,
    parse = TRUE,
    debug = FALSE,
    verbose = FALSE) {
  if (is.null(indicators) && is.null(json_query)) {
    stop("At least one of either indicators or json_query must not be NULL.")
  }
  if (!is.null(json_query)) {
    if (any(!is.null(c(indicators, time_periods, geographies, filter_items)))) {
      warning(
        paste(
          "json_query is set - ignoring indicators, time_periods, geographies",
          " and filter_items params."
        )
      )
    }
    if (json_query |> stringr::str_sub(-5) == ".json") {
      json_body <- readLines(json_query) |>
        paste0(collapse = "\n")
    } else {
      if (verbose) {
        message("Parsing query options")
      }
      json_body <- json_query
    }
  } else {
    json_body <- eesyapi::parse_tojson_params(
      indicators = indicators,
      time_periods = time_periods,
      geographies = geographies,
      filter_items = filter_items,
      page = page,
      page_size = page_size,
      debug = debug,
      verbose = verbose
    )
  }
  if (verbose) {
    json_body |> cat(fill = TRUE)
  }
  response <- eesyapi::api_url(
    "post-data",
    dataset_id = dataset_id,
    dataset_version = dataset_version,
    ees_environment = ees_environment,
    api_version = api_version
  ) |> httr::POST(
    body = json_body,
    encode = "json",
    httr::content_type("application/json")
  )
  if (verbose) {
    print(response)
    print(
      response |>
        httr::content("text") |>
        jsonlite::fromJSON()
    )
  }
  eesyapi::http_request_error(response)
  # Unless the user specifies a specific page of results to get, loop through all available pages.
  response_json <- response |>
    httr::content("text") |>
    jsonlite::fromJSON()
  if (verbose) {
    message(paste("Total number of pages: ", response_json$paging$totalPages))
  }
  dfresults <- response_json$results |>
    eesyapi::parse_api_dataset(dataset_id, verbose = verbose)
  # Unless the user has requested a specific page, then assume they'd like all pages collated and
  # recursively run the query.
  if (is.null(page) && is.null(json_query)) {
    if (response_json$paging$totalPages > 1) {
      for (page in c(2:response_json$paging$totalPages)) {
        json_body <- eesyapi::parse_tojson_params(
          indicators = indicators,
          time_periods = time_periods,
          geographies = geographies,
          filter_items = filter_items,
          page = page,
          page_size = page_size,
          verbose = verbose
        )
        response_page <- eesyapi::api_url(
          "post-data",
          dataset_id = dataset_id,
          dataset_version = dataset_version,
          ees_environment = ees_environment,
          api_version = api_version
        ) |>
          httr::POST(
            body = json_body,
            encode = "json",
            httr::content_type("application/json")
          ) |>
          httr::content("text") |>
          jsonlite::fromJSON()
        response_page |> eesyapi::warning_max_pages()
        dfresults <- dfresults |>
          dplyr::bind_rows(
            response_page$results |>
              eesyapi::parse_api_dataset(dataset_id, verbose = verbose)
          )
      }
    }
  }
  return(dfresults)
}
