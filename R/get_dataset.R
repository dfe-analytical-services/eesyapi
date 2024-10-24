#' Query a data set using GET and a query URL
#'
#' @description
#' This function provides a method for generating and sending a URL based data query to the
#' EES API. As a minimum, it requires the dataset_id and indicators flags to be provided.
#'
#' Note that the GET command is very limited on the level of logic it can process. For example
#' there is no way of using GET to make a query that combines different filters with AND logic.
#' So if you give GET a set of filter items to search on, it can only return all rows containing
#' any of those items.
#'
#' @inheritParams api_url
#' @param parse Logical flag to activate parsing of the results. Default: TRUE
#'
#' @return Data frame containing query results of an API data set
#' @export
#'
#' @examples
#' get_dataset(
#'   example_id(),
#'   geographic_levels = c("NAT"),
#'   filter_items = example_id("filter_item"),
#'   indicators = example_id("indicator")
#' )
get_dataset <- function(
    dataset_id,
    indicators,
    time_periods = NULL,
    geographic_levels = NULL,
    locations = NULL,
    filter_items = NULL,
    dataset_version = NULL,
    ees_environment = NULL,
    api_version = NULL,
    page = NULL,
    page_size = 10000,
    parse = TRUE,
    verbose = FALSE) {
  api_call <- eesyapi::api_url(
    "get-data",
    dataset_id = dataset_id,
    indicators = indicators,
    time_periods = time_periods,
    geographic_levels = geographic_levels,
    locations = locations,
    filter_items = filter_items,
    dataset_version = dataset_version,
    ees_environment = ees_environment,
    api_version = api_version,
    page_size = page_size,
    page = page,
    verbose = verbose
  )
  response <- api_call |>
    httr::GET()
  eesyapi::http_request_error(response)
  toggle_message("Retrieved data", verbose = verbose)
  # Unless the user specifies a specific page of results to get, loop through all available pages.
  response_json <- response |>
    httr::content("text") |>
    jsonlite::fromJSON()
  if (verbose) {
    message(paste("Total number of pages: ", response_json$paging$totalPages))
  }
  dfresults <- response_json |>
    magrittr::extract2("results")
  # Unless the user has requested a specific page, then assume they'd like all pages collated and
  # recursively run the query.
  if (is.null(page)) {
    if (response_json$paging$totalPages > 1) {
      if (response_json$paging$totalPages * page_size > 100000) {
        message(
          paste(
            "Downloading up to", response_json$paging$totalPages * page_size, "rows.",
            "This may take a while.",
            "We recommend downloading the full data set using download_dataset()",
            "for large volumes of data"
          )
        )
      }
      for (page in c(2:response_json$paging$totalPages)) {
        response_page <- eesyapi::api_url(
          "get-data",
          dataset_id = dataset_id,
          indicators = indicators,
          time_periods = time_periods,
          geographic_levels = geographic_levels,
          locations = locations,
          filter_items = filter_items,
          dataset_version = dataset_version,
          ees_environment = ees_environment,
          api_version = api_version,
          page_size = page_size,
          page = page,
          verbose = verbose
        ) |>
          httr::GET() |>
          httr::content("text") |>
          jsonlite::fromJSON()
        response_page |> eesyapi::warning_max_pages()
        toggle_message(
          paste0("Retrieved page ", page, " of ", response_json$paging$totalPages),
          verbose = verbose
        )
        dfresults <- dfresults |>
          dplyr::bind_rows(
            response_page |>
              magrittr::extract2("results")
          )
      }
    }
  }
  if (parse) {
    dfresults <- dfresults |>
      eesyapi::parse_api_dataset(
        dataset_id,
        verbose = verbose,
        ees_environment = ees_environment
      )
  }
  return(dfresults)
}
