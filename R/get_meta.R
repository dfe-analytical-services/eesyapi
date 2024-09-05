#' Title
#'
#' @param dataset_id ID of data set to be connected to
#' @param dataset_version Version of data set to be connected to
#' @param api_version EES API version
#' @param parse Parse result into structured list
#'
#' @return Results of query to meta data
#' @export
#'
#' @examples
#' get_meta("d7329101-f275-d277-bbfe-d8cfaa709833")
get_meta <- function(dataset_id, dataset_version = NULL, api_version = NULL, parse = TRUE) {
  meta_url <- eesyapi::eesapi_url(
    endpoint = "get-meta",
    dataset_id = dataset_id,
    dataset_version = dataset_version
  )
  response <- httr::GET(meta_url)
  if (response$status_code < 300) {
    if (parse) {
      result <- response |>
        httr::content("text") |>
        jsonlite::fromJSON()
    } else {
      result <- response
    }
    return(result)
  } else {
    stop(paste0(
      "Query returned error, status: ",
      response$status,
      "\n      ",
      eesyapi::http_request_error(response$status)
    ))
  }
}
