#' Title
#'
#' @param dataset_id
#' @param dataset_version
#' @param api_version
#'
#' @return
#' @export
#'
#' @examples
get_data_set <- function(
    dataset_id,
    dataset_version = NULL,
    api_version = NULL) {
  print(    api_url("get-data", dataset_id = dataset_id)
)
  httr::GET(
    api_url("get-data", dataset_id = dataset_id)
  ) |>
    httr::content("text") |>
    jsonlite::fromJSON()
}
