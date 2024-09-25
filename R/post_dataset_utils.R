#' Create a json query string
#'
#' @description
#' A short description...
#'
#' @inheritParams api_url
#'
#' @return String containing json query body for use with http POST request
#' @export
#'
#' @examples
#' parse_params_to_json(example_id("indicator"))
parse_params_to_json <- function(
    indicators,
    time_periods = NULL,
    geographic_levels = NULL,
    locations = NULL,
    filter_items = NULL) {
  "A json query string"
}
