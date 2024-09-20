#' Contextualise http request errors
#'
#' @description
#' Translate a http error code into an error message.
#'
#' @param response HTTP response from the API
#' @param verbose Run in verbose mode, logical, default = FALSE
#'
#'
#' @return Translation of the response code
#' @export
#'
#' @examples
#' http_request_error(list(status = 200))
http_request_error <- function(
    response,
    verbose = FALSE) {
  status_lookup <- data.frame(
    response_group = c(
      2,
      4,
      5
    ),
    response_text = c(
      "Successful API request.",
      "Invalid query, data set ID, data set version or API version submitted to API.",
      paste(
        "Internal server error encountered - please contact the EES API team at",
        "explore.statistics@education.gov.uk",
        "providing the query you were attempting to submit."
      )
    )
  )
  status_group <- trunc(response$status / 100.)
  if (status_group %in% status_lookup$response_group) {
    status_response_text <- status_lookup |>
      dplyr::filter(status_lookup$response_group == status_group) |>
      dplyr::pull("response_text")
    if (!(status_group %in% c(2, 5)) && !is.null(response$errors)) {
      status_response_text <- status_response_text |>
        paste0(
          "\n",
          paste(response$errors, collapse = ". ")
        )
    }
  } else {
    status_response_text <- "API http response code not recognised."
  }
  if (status_group != 2) {
    stop(
      paste0(
        "HTTP connection error: ",
        response$status,
        "\n",
        status_response_text
      )
    )
  }
  return(status_response_text)
}
