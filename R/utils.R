#' Convert filter type to API filter type
#'
#' @description
#' The API uses a slightly different naming convention for the different types of
#' filters to what is used by analysts within data files. The function just converts
#' from the file versions to the API versions.
#'
#' @inheritParams parse_filter_in
#'
#' @return String containing API friendly filter type descriptor
#' @export
#'
#' @examples
#' to_api_filter_type("filter_items")
#' to_api_filter_type("geographic_levels")
#' to_api_filter_type("locations")
#' to_api_filter_type("filter_items")
to_api_filter_type <- function(filter_type) {
  if (!(filter_type %in% c("time_periods", "geographic_levels", "locations", "filter_items"))) {
    stop(
      paste0(
        "Invalid filter type: ",
        filter_type,
        "\nShould be one of \"time_periods\", \"geographic_levels\", ",
        "\"locations\" or \"filter_items\"."
      )
    )
  }
  filter_type <- filter_type |>
    stringr::str_replace("_item", "")
  gsub("_(\\w?)", "\\U\\1", filter_type, perl = TRUE)
}
