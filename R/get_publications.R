#' Get publications
#'
#' @param page_size Number of results to collect in a single query to the API (max 40)
#' @param page Page number to return (Default is NULL which will loop through until all
#' pages of the query are collated).
#' @param verbose Add extra contextual information whilst running
#'
#' @return Data frame listing all available publications
#' @export
#'
#' @examples
#' get_publications()
get_publications <- function(
    page_size = 40,
    page = NULL,
    verbose = FALSE) {
  eesyapi::validate_page_size(page_size)
  response <- httr::GET(
    eesyapi::api_url(page_size = page_size, page = page, verbose = verbose)
  ) |>
    httr::content("text") |>
    jsonlite::fromJSON()
  # Unless the user specifies a specific page of results to get, loop through all available pages.
  if (is.null(page)) {
    if (response$paging$totalPages > 1) {
      for (page in c(2:response$paging$totalPages)) {
        response_page <- httr::GET(
          eesyapi::api_url(page_size = page_size, page = page, verbose = verbose)
        ) |>
          httr::content("text") |>
          jsonlite::fromJSON()
        response$results <- response$results |>
          rbind(response_page$results)
      }
    }
  }
  response |> eesyapi::warning_max_pages()
  return(response$results)
}
