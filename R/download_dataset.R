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
#' It is recommended to take the time to set up custom queries using the
#' `query_dataset()` function instead. If you are using this function for more
#' than exploratory purposes, make sure you subscribe to the data set you're
#' downloading and then keep track of any updates to the data.
#'
#' @inheritParams api_url
#'
#' @return data.frame
#' @export
#'
#' @examples
#' download_dataset(example_id("dataset"))
download_dataset <- function(
    dataset_id,
    dataset_version = NULL,
    ees_environment = NULL,
    api_version = NULL,
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

  if (!is.logical(verbose)) {
    stop("verbose must be a logical value, either TRUE or FALSE")
  }

  eesyapi::validate_ees_id(dataset_id, level = "dataset")

  # Generate query ------------------------------------------------------------
  query_url <- eesyapi::api_url(
    endpoint = "get-csv",
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
