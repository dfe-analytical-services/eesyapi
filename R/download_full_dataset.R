#' Download the raw CSV for an API data set
#'
#' This gives a super quick way to just fetch the whole file in a human
#' readable format.
#'
#' @description
#' This function is mostly designed for exploring the API, and is unlikely to
#' be suitable for long term production use.
#'
#' There is no filtering down of the file so you will always get the whole file
#' and in some instances this may be very large.
#'
#' As there are no IDs involved, this is brittle and code relying on this
#' function will likely break whenever there is renaming of variables or items
#' in the data.
#'
#' It is recommended to subscribe to the data set you're querying and then keep
#' track of any updates, including minor updates to the data.
#'
#' @param dataset_id
#' @param dataset_version
#' @param api_version
#' @param verbose
#'
#' @return
#' @export
#'
#' @examples
download_full_dataset <- function(
    dataset_id,
    dataset_version = NULL,
    api_version = NULL , # TODO: check how this is handled
    verbose = FALSE
) {

  # Some validation

  # Add in messaging of some kind to update on progress and size?

  # Actual query
  url <- "https://dev.statistics.api.education.gov.uk/api/v1/data-sets/7c0e9201-c7c0-ff73-bee4-304e731ec0e6/csv"
  response <- httr::GET(url)
  output <- httr::content(response)

  return(output)
}
