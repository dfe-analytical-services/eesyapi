#' Generate URL code for API data query
#'
#' @description
#' Generates the query code to append to the base API query URL for use by query_dataset_get().
#'
#' @param indicators Indicators required as a string or vector of strings (required)
#' @param time_periods Time periods required as a string ("period|code") or vector of strings
#' @param geographic_levels Geographic levels required as a string or vector of strings
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
    geographic_levels = NULL) {
  if (is.null(indicators)) {
    stop("The keyword inidcators must be supplied")
  }
  query_geographic_levels <- "geographicLevels.in=NAT%2CREG"
  query_time_periods <- "timePeriods.in=2024%7CW23%2C2024%7CW24"
  query_indicators <- paste0(
    "indicators=",
    paste0(indicators, collapse = "%2C%20")
  )
  query <- paste0(
    "?",
    ifelse(
      !is.null(geographic_levels),
      paste(query_geographic_levels, "&"),
      ""
    ),
    query_time_periods,
    "&",
    query_indicators
  )
  return(query)
}
