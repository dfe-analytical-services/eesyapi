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
  status_lookup <- data.frame(
    response_group = c(
      2,
      4,
      5
    ),
    response_text = c(
      "Successful API request.",
      "Invalid query, data set ID, data set version or API version submitted to API.",
      "Internal server error encountered."
    )
  )
  status_group <- trunc(response_status / 100.)
  if (status_group %in% status_lookup$response_group) {
    return(
      status_lookup |>
        dplyr::filter(status_lookup$response_group == status_group) |>
        dplyr::pull("response_text")
    )
  } else {
    return("API http response code not recognised.")
  }
}
