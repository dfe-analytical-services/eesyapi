#' Get publications
#'
#' @inheritParams api_url
#'
#' @return Data frame listing all available publications
#' @export
#'
#' @examples
#' get_publications()
get_publications <- function(
    ees_environment = NULL,
    api_version = NULL,
    page_size = 40,
    page = NULL,
    verbose = FALSE) {
  eesyapi::validate_page_size(page_size)
  response <- httr::GET(
    eesyapi::api_url(
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
          rbind(response_page$results)
      }
    }
  }
  response |> eesyapi::warning_max_pages()
  return(response$results)
}
