#' Get publications
#'
#' @param page_size Number of results to return in a single query to the API (max 40)
#' @param page Page number to return (Default is NULL which will loop through until all
#' pages of the query are collated).
#' @param verbose Add extra contextual information whilst running
#'
#' @return Data frame listing all available publications
#' @export
#'
#' @examples
#' get_publication_catalogue()
get_publication_catalogue <- function(
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
  if(is.null(page)){
    if(response$paging$totalPages > 1){
      for(page in c(2:response$paging$totalPages)){
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
  return(response)
}

#' Get publication specific data set catalogue
#'
#' @param publication_id The publication ID as used by the API
#' @param page_size Number of results to return in a single query (max 40)
#' @param page Page number to return
#' @param verbose Add extra contextual information whilst running
#'
#' @return Data frame listing the data sets contained within a single publication
#' @export
#'
#' @examples
#' get_publication_datasets(example_id("publication"))
get_publication_datasets <- function(
    publication_id,
    page_size = NULL,
    page = NULL,
    verbose = FALSE) {
  # Validate input parameters
  eesyapi::validate_ees_id(publication_id)
  eesyapi::validate_page_size(page_size)
  # Send the GET call to the API
  response <- httr::GET(
    eesyapi::api_url(
      endpoint = "get-data-catalogue",
      publication_id = publication_id,
      page_size = page_size,
      page = page,
      verbose = verbose
    )
  ) |>
    httr::content("text") |>
    jsonlite::fromJSON()
  # Check that the query hasn't tried to retrieve results beyond the final page of results
  response |> eesyapi::warning_max_pages()
  return(response)
}
