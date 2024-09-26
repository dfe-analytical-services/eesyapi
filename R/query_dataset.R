#' Query a data set
#'
#' @description
#' Create and send a query to the EES API. Queries can be constructed by including the
#' codes to the relevant flags to filter on time period, geographic level, location,
#' and data set specific filters. If none of the above are set in the function call,
#' then the entire data set will be retrieved. The data set id and specific indicators
#' of interest must be supplied explictly using the dataset_id and indicators params.
#'
#' @inheritParams api_url
#' @param method An API query method. Needs to be "GET"
#'
#' @return Data frame containing query results
#' @export
#'
#' @examples
#' query_dataset(
#'   example_id(),
#'   geographic_levels = "NAT",
#'   filter_items = example_id("filter_item"),
#'   indicators = example_id("indicator")
#' )
#' query_dataset(
#'   example_id(),
#'   indicators = example_id("indicator"),
#'   page = 1,
#'   page_size = 10
#' )
query_dataset <- function(
    dataset_id,
    indicators,
    time_periods = NULL,
    geographic_levels = NULL,
    locations = NULL,
    filter_items = NULL,
    method = "GET",
    dataset_version = NULL,
    api_version = NULL,
    page_size = 1000,
    page = NULL,
    verbose = FALSE) {
  if (method != "GET") {
    stop(
      paste(
        "Invalid method selected. The keyword method should be set to GET",
        "(an option to use POST is being developed)."
      )
    )
  }
  if (method == "GET") {
    warning(
      paste(
        "Using GET to query a data set offers limited functionality, we recommend",
        "using POST alongside a JSON structured query instead:\n",
        "  - query_dataset(..., method = 'POST')"
      )
    )
    eesyapi::get_dataset(
      dataset_id = dataset_id,
      indicators = indicators,
      time_periods = time_periods,
      geographic_levels = geographic_levels,
      locations = locations,
      filter_items = filter_items,
      dataset_version = dataset_version,
      api_version = api_version,
      page_size = page_size,
      page = page,
      verbose = verbose
    )
  }
}
