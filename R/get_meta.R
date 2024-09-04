#' Title
#'
#' @param dataset_id ID of dataset to be connected to
#' @param dataset_version Version of dataset to be connected to
#' @param api_version EES API version
#' @param parsed
#'
#' @return Results of query to meta data
#' @export
#'
#' @examples
#' get_meta("d7329101-f275-d277-bbfe-d8cfaa709833")
get_meta <- function(dataset_id, dataset_version = NULL, api_version = NULL, parsed = TRUE) {
  meta_url <- eesapi_url(
    endpoint = "get-meta",
    dataset_id = dataset_id,
    dataset_version = dataset_version
  )
  response <- httr::GET(meta_url)
  if (response$status_code == 200) {
    if (parsed) {
      result <- response %>%
        httr::content("text") %>%
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
      http_request_error(response$status)
    ))
  }
}
