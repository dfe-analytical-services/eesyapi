#' Convert filter type to API filter type
#'
#' @description
#' The API uses a slightly different naming convention for the different types of
#' filters to what is used by analysts within data files. The function just converts
#' from the file versions to the API versions.
#'
#' @inheritParams parse_tourl_filter_in
#'
#' @return String containing API friendly filter type descriptor
#' @export
#'
#' @examples
#' convert_api_filter_type("filter_items")
#' convert_api_filter_type("geographic_levels")
#' convert_api_filter_type("locations")
#' convert_api_filter_type("filter_items")
convert_api_filter_type <- function(filter_type) {
  validate_ees_filter_type(filter_type)
  filter_type <- filter_type |>
    stringr::str_replace("_item", "")
  gsub("_(\\w?)", "\\U\\1", filter_type, perl = TRUE)
}
