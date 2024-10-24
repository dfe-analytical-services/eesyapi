#' Get publication specific data set catalogue
#'
#' @inheritParams api_url
#'
#' @return Data frame listing the data sets contained within a single publication
#' @export
#'
#' @examples
#' get_data_catalogue(example_id("publication"))
get_data_catalogue <- function(
    publication_id,
    ees_environment = NULL,
    api_version = NULL,
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
      ees_environment = ees_environment,
      api_version = api_version,
      page_size = page_size,
      page = page,
      verbose = verbose
    )
  ) |>
    httr::content("text") |>
    jsonlite::fromJSON()
  # Unless the user specifies a specific page of results to get, loop through all available pages.
  if (is.null(page)) {
    if (response$paging$totalPages > 1) {
      for (page in c(2:response$paging$totalPages)) {
        response_page <- httr::GET(
          eesyapi::api_url(
            endpoint = "get-data-catalogue",
            publication_id = publication_id,
            ees_environment = ees_environment,
            api_version = api_version,
            page_size = page_size,
            page = page,
            verbose = verbose
          )
        ) |>
          httr::content("text") |>
          jsonlite::fromJSON()
        response$results <- response$results |>
          dplyr::bind_rows(response_page$results)
      }
    }
  }
  # Check that the query hasn't tried to retrieve results beyond the final page of results
  response |> eesyapi::warning_max_pages()
  return(response$results)
}
