#' Query a data set using GET and a query URL
#'
#' @description
#' This function provides a method for generating and sending a URL based data query to the
#' EES API. As a minimum, it requires the dataset_id and indicators flags to be provided.
#'
#' Note that the GET command is very limited on the level of logic it can process. For example
#' there is no way of using GET to make a query that combines different filters with AND logic.
#' So if you give GET a set of filter items to search on, it can only return all rows containing
#' any of those items.
#'
#' @inheritParams api_url
#'
#' @return Data frame containing query results of an API data set
#' @export
#'
#' @examples
#' post_dataset(example_id(), indicators = "session_count")
post_dataset <- function(
    dataset_id,
    indicators,
    time_periods = NULL,
    geographic_levels = NULL,
    locations = NULL,
    filter_items = NULL,
    dataset_version = NULL,
    api_version = NULL,
    verbose = FALSE) {
  response <- api_url(
    "post-data",
    dataset_id = dataset_id,
    indicators = indicators,
    time_periods = time_periods,
    geographic_levels = geographic_levels,
    locations = locations,
    filter_items = filter_items,
    verbose = verbose
  ) |>
    httr::GET()
  if (response$status != 200) {
    print(response$errors)
  }
}
