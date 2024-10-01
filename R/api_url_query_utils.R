#' Create <filter>.in query string for URL queries
#'
#' @description
#' Outputs a URL query string containing timePeriods.in=..., geographicLevels.in=..., etc for
#' use with querying a data set via GET.
#'
#' @param items items to be included in the "in" statement
#' @param filter_type type of filter being queried: "time_periods", "geographic_levels",
#' "locations" or "filter_items"
#'
#' @return Query string for use in URL based API queries
#' @export
#'
#' @examples
#' parse_tourl_filter_in(c("2024|W11", "2024|W12"), filter_type = "time_periods")
parse_tourl_filter_in <- function(
    items,
    filter_type) {
  validate_ees_filter_type(filter_type)
  type_string <- to_api_filter_type(filter_type)
  if (!is.null(items)) {
    if (filter_type %in% c("time_period", "locations")) {
      items <- gsub("\\|", "%7C", items)
    }
    paste0(
      type_string,
      ".in=",
      items |>
        paste0(collapse = "%2C"),
      "&"
    )
  }
}
