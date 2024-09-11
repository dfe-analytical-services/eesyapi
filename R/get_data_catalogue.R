#' Get publication specific data set catalogue
#'
#' @param publication_id The publication ID as used by the API
#'
#' @return Data frame listing the data sets contained within a single publication
#' @export
#'
#' @examples
#' get_publication_dataset_list(example_id("publication"))
get_publication_dataset_list <- function(publication_id) {

}

#' Get publications
#'
#' @param page_size Number of results to return in a single query (max 40)
#' @param page Page number to return
#' @param verbose Add extra contextual information whilst running
#'
#' @return Data frame listing all available publications
#' @export
#'
#' @examples
#' get_publications()
get_publications <- function(page_size = 40, page = NULL, verbose = FALSE) {
  validate_page_size(page_size)
  result <- httr::GET(
    eesapi_url(page_size = page_size, page = page, verbose = verbose)
  ) |>
    httr::content("text") |>
    jsonlite::fromJSON()
  result |> warning_max_pages()
  return(result)
}
