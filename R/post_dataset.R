#' Query a data set using POST and a query json
#'
#' @description
#' This function provides a method for generating and sending a json based data query to the
#' EES API. As a minimum, it requires the dataset_id flag and either the indicators flag or
#' a json file containing a query to be provided.
#'
#' @inheritParams api_url
#' @param json_query Optional path to a json file containing the query parameters
#' @param parse Logical flag to activate parsing of the results. Default: TRUE
#'
#' @return Data frame containing query results of an API data set
#' @export
#'
#' @examples
#' post_dataset(
#'   example_id(),
#'   example_id("indicator"),
#'   geographic_levels = "NAT",
#'   filter_items = example_id("filter_item")
#' )
post_dataset <- function(
    dataset_id,
    indicators = NULL,
    time_periods = NULL,
    geographic_levels = NULL,
    locations = NULL,
    json_query = NULL,
    filter_items = NULL,
    dataset_version = NULL,
    api_version = NULL,
    page = NULL,
    page_size = 1000,
    parse = TRUE,
    verbose = FALSE) {
  if (is.null(indicators) && is.null(json_query)) {
    stop("At least one of either indicators or json_query must not be NULL.")
  } else if (!is.null(json_query)) {
    if (any(!is.null(c(indicators, time_periods, geographic_levels, locations, filter_items)))) {
      warning(
        paste(
          "json_query is set - ignoring indicators, time_periods, geographic_levels,",
          "locations and filter_items params."
        )
      )
    }
    if (json_query |> stringr::str_sub(-5) == ".json") {
      json_body <- readLines(json_query) |>
        paste0(collapse = "\n")
    }else {
      message("Parsing query options")
      json_body <- json_query
    }
  } else {
    message("Parsing filters not implemented yet")
    json_body <- "Some json created from the user inputs..."
  }
  api_url(
    "query-data",
    dataset_id = dataset_id,
    dataset_version = dataset_version
  ) |> httr::POST(
    body = json_body,
    encode = "json",
    httr::content_type("application/json")
  )
}
