#' Query a data set using GET and a query URL
#'
#' @description
#' This function provides a method for generating and sending a URL based data query to the
#' EES API. As a minimum, it requires the dataset_id and indicators flags to be provided.
#'
#' @inheritParams api_url
#'
#' @return Data frame containing query results of an API data set
#' @export
#'
#' @examples
#' get_dataset(example_id(), indicators = "session_count")
get_dataset <- function(
    dataset_id,
    indicators,
    time_periods = NULL,
    geographic_levels = NULL,
    locations = NULL,
    filter_items = NULL,
    dataset_version = NULL,
    api_version = NULL) {
  response <- api_url(
    "get-data",
    dataset_id = dataset_id,
    indicators = indicators,
    time_periods = time_periods,
    geographic_levels = geographic_levels,
    locations = locations,
    filter_items = filter_items
  ) |>
    httr::GET()
  if (response$status != 200) {
    print(response$errors)
  }
  response_json <- response |>
    httr::content("text") |>
    jsonlite::fromJSON()
  result <- response_json$results
  dplyr::bind_cols(
    result$timePeriod,
    data.frame(geographic_level = result$geographicLevel),
    data.frame(location_code = result$locations),
    result$filters,
    result$values
  ) |>
    dplyr::rename(
      time_identifier = "code",
      time_period = "period"
    )
}
