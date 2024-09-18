#' Query a data set using GET and a query URL
#'
#' @description
#' This function provides a method for generating and sending a URL based data query to the
#' EES API. As a minimum, it requires the dataset_id and indicators flags to be provided.
#'
#' @inheritParams api_url
#' @inheritParams api_url_query
#'
#' @return Data frame containing query results of an API data set
#' @export
#'
#' @examples
#' query_get_dataset(example_id(), indicators = "session_count")
query_get_dataset <- function(
    dataset_id,
    indicators,
    time_periods = NULL,
    geographic_levels = NULL,
    dataset_version = NULL,
    api_version = NULL) {
  api_url(
    "get-data",
    dataset_id = dataset_id,
    indicators = indicators,
    time_periods = time_periods,
    geographic_levels = geographic_levels
  ) |>
    httr::GET() |>
    httr::content("text") |>
    jsonlite::fromJSON()
}
