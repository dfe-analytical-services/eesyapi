#' Query a data set using GET and a query URL
#'
#' @description
#' This function provides a method for generating and sending a URL based data query to the
#' EES API. As a minimum, it requires the dataset_id and indicators flags to be provided.
#'
#' @param dataset_id ID of data set to be connected to
#' @param indicators Indicators required as a string or vector of strings
#' @param time_periods Time periods required as a string ("period|code") or vector of strings
#' @param geographic_levels Geographic levels required as a string or vector of strings
#' @param dataset_version Version of data set to be connected to
#' @param api_version EES API version
#'
#' @return Data frame containing query results of an API data set
#' @export
#'
#' @examples
#' query_dataset_get(example_id(), indicators = "session_count")
query_dataset_get <- function(
    dataset_id,
    indicators,
    time_periods = NULL,
    geographic_levels = NULL,
    dataset_version = NULL,
    api_version = NULL) {
  httr::GET(
    api_url(
      "get-data",
      dataset_id = dataset_id,
      indicators = indicators,
      time_periods = time_periods,
      geographic_levels = geographic_levels
      )
  ) |>
    httr::content("text") |>
    jsonlite::fromJSON()
}
