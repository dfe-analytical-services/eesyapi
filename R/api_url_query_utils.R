#' Create <filter>.in query string for URL queries
#'
#' @description
#' Outputs a URL query string containing timePeriods.in=..., geographicLevels.in=..., etc for
#' use with querying a data set via GET.
#'
#' @param items items to be included in the "in" statement
#' @param type type of filter items being queried: "time_periods", "geographic_levels",
#' "locations" or "filter_items"
#'
#' @return Query string for use in URL based API queries
#' @export
#'
#' @examples
#' parse_filter_in(c("2024|W11", "2024|W12"), type = "time_periods")
parse_filter_in <- function(
    items = NULL,
    type = NULL) {
  if (!(type %in% c("time_periods", "geographic_levels", "locations", "filter_items"))) {
    stop("type keyword should be one of time_periods, geographic_levels, locations or filter_items")
  }
  type_string <- type |>
    stringr::str_replace("_item", "")
  type_string <- gsub("_(\\w?)", "\\U\\1", type_string, perl = TRUE)
  if (!is.null(items)) {
    if (type %in% c("time_period", "locations")) {
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
