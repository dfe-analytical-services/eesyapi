#' Create "filter in" query string for url queries
#'
#' @description
#' A short description...
#'
#' @param items items to be included in the "in" statement
#' @param type type of filter items being queried: "time_periods", "geographic_levels", "locations" or
#' "filters"
#'
#' @return Query string for use in URL based API queries
#' @export
#'
#' @examples
#' parse_standard_filter_in(c("2024|W11", "2024|W12"), type = "time_period")
parse_filter_in <- function(
    items = NULL,
    type = "filters") {
  type_string <- gsub("_(\\w?)", "\\U\\1", type, perl = T)
  print(items)
  print(items |>
    paste0(collapse = "%2C"))
  if (!is.null(items)) {
    if (type == "time_period") {
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
