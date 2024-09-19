#' Generate URL code for API data query
#'
#' @description
#' Generates the query code to append to the base API query URL for use by query_dataset_get().
#'
#' @param indicators Indicators required as a string or vector of strings (required)
#' @param time_periods Time periods required as a string ("period|code") or vector of strings
#' @param geographic_levels Geographic levels required as a string or vector of strings
#' @param locations Location code required as a string or vector of strings
#' @param filter_items Filter items required as a string or vector of strings
#'
#' @return String containing data query string to append to GET data query URL
#' @export
#'
#' @examples
#' api_url_query("session_count")
#' api_url_query(c("session_count", "session_percent"))
api_url_query <- function(
    indicators,
    time_periods = NULL,
    geographic_levels = NULL,
    locations = NULL,
    filter_items = NULL) {
  if (is.null(indicators)) {
    stop("The keyword indicators must be supplied")
  }
  if (!is.null(time_periods)) {
    query_time_periods <- parse_filter_in(time_periods, "time_periods")
  }
  if (!is.null(geographic_levels)) {
    query_geographic_levels <- query_geographic_levels <- parse_filter_in(geographic_levels, type = "geographic_levels")
  }
  if (!is.null(locations)) {
    query_locations <- parse_filter_in(locations, type = "locations")
  }
  if (!is.null(filter_items)) {
    query_filter_items <- parse_filter_in(filter_items)
  }
  query_indicators <- paste0(
    "indicators=",
    paste0(indicators, collapse = "%2C%20")
  )
  query <- paste0(
    "?",
    ifelse(
      !is.null(time_periods),
      paste0(query_time_periods, "&"),
      ""
    ),
    ifelse(
      !is.null(geographic_levels),
      paste0(query_geographic_levels, "&"),
      ""
    ),
    ifelse(
      !is.null(locations),
      paste0(query_locations, "&"),
      ""
    ),
    ifelse(
      !is.null(filter_items),
      paste0(query_filter_items, "&"),
      ""
    ),
    query_indicators
  )
  return(query)
}
