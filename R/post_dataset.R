#' Query a data set using POST and a query json
#'
#' @description
#' This function provides a method for generating and sending a json based data query to the
#' EES API. As a minimum, it requires the dataset_id flag and either the indicators flag or
#' a json file containing a query to be provided.
#'
#' @inheritParams api_url
#' @param json_file Optional path to a json file containing the query parameters
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
    json_file = NULL,
    filter_items = NULL,
    dataset_version = NULL,
    api_version = NULL,
    page = NULL,
    page_size = 1000,
    parse = TRUE,
    verbose = FALSE) {
  if (is.null(indicators) && is.null(json_file)) {
    stop("At least one of either indicators or json_file must not be NULL.")
  }
}
