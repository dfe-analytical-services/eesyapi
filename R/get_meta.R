#' Title
#'
#' @param dataset_id ID of dataset to be connected to
#' @param dataset_version Version of dataset to be connected to
#' @param api_version EES API version
#'
#' @return Results of query to meta data
#' @export
#'
#' @examples
#' get_meta("d7329101-f275-d277-bbfe-d8cfaa709833")
get_meta <- function(dataset_id, dataset_version=NULL, api_version=NULL){
  meta_url <- eesapi_url(
    endpoint = "get-meta",
    dataset_id = dataset_id,
    dataset_version=dataset_version
  )
  response <- httr::GET(meta_url)
  if(response$status_code == 200){
    return(response %>% httr::content())
  } else {
    stop(paste0("Query returned error, status: ", response$status))
  }
}
