#' http request error
#'
#' @description
#' Translate a http error code into an error message.
#'
#' @param response_status Response code from an API request
#'
#' @return Translation of the response code
#' @export
#'
#' @examples
#' http_request_error(200)
http_request_error <- function(response_status) {
  status_lookup <- list(
    "200" = "Successful API request.",
    "400" = "Invalid query, data set ID, data set version or API version submitted to API.",
    "500" = "Internal server error encountered."
  )
  status_lookup[format(round(response_status, -1))][[1]]
}
