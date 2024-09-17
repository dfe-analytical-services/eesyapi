#' Query a data set using GET and a query URL
#'
#' @param dataset_id ID of data set to be connected to
#' @param dataset_version Version of data set to be connected to
#' @param api_version EES API version
#'
#' @return
#' @export
#'
#' @examples
query_dataset_get <- function(
    dataset_id,
    dataset_version = NULL,
    api_version = NULL) {
  print(api_url("get-data", dataset_id = dataset_id))
  httr::GET(
    api_url("get-data", dataset_id = dataset_id)
  ) |>
    httr::content("text") |>
    jsonlite::fromJSON()
}
