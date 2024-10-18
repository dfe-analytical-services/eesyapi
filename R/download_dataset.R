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
#' @param dataset_id ID of data set
#' @param dataset_version Version number of data set
#' @param api_version EES API version
#' @param verbose Run with additional contextual messaging, logical, default = FALSE
#'
#' @return data.frame
#' @export
#'
#' @examples
#' download_dataset(example_id("dataset", group = "public-api-testing"))
download_dataset <- function(
    dataset_id,
    dataset_version = NULL,
    api_version = NULL, # TODO: check how this is handled
    verbose = FALSE) {
  # Validation ----------------------------------------------------------------
  if (!is.null(dataset_version)) {
    warning(
      paste(
        "Support for dataset_version is not yet available for downloading",
        "full data sets. Returning latest available version of data set."
      )
    )
  }

  eesyapi::validate_ees_id(dataset_id, level = "dataset")

  # Generate query ------------------------------------------------------------
  query_url <- eesyapi::api_url(
    endpoint = "get-data",
    response_format = "CSV",
    dataset_id = dataset_id,
    verbose = verbose
  )

  toggle_message("Requesting data...", verbose = verbose)

  response <- httr::GET(query_url)

  eesyapi::http_request_error(response, verbose = verbose)

  toggle_message("Parsing response...", verbose = verbose)

  # Parse into data.frame -----------------------------------------------------
  output <- httr::content(
    response,

    # All EES CSVs should be UTF-8 and are validated on import
    encoding = "UTF-8",

    # httr uses read_csv() underneath, controlling read_csv() verbosity
    show_col_types = verbose,
    progress = verbose
  ) |>
    as.data.frame()

  return(output)
}
