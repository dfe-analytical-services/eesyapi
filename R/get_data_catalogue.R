#' Get data set catalogue
#'
#' @param publication_id The publication ID, leave as NULL to query across all publications
#'
#' @return Data frame listing data sets
#' @export
#'
#' @examples
#' get_data_catalogue()
get_data_catalogue <- function(publication_id = NULL) {
  if (is.null(publication_id)) {
    "I'm going to get a listing of all data sets from all publications..."
  } else {
    "I'm going to get a listing of all data sets from a single publication..."
  }
}

#' Get publications
#'
#' @param page_size Number of results to return in a single query
#'
#' @return Data frame listing all available publications
#' @export
#'
#' @examples
#' get_publications()
get_publications <- function(page_size = 40) {
  httr::GET(
    eesapi_url(page_size = page_size)
  ) |>
    httr::content("text") |>
    jsonlite::fromJSON()
}
