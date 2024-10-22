#' Preview the raw CSV for an API data set
#'
#' This gives a super quick way to just fetch the file in a human readable
#' format.
#'
#' @description
#' This function is mostly designed for exploring the API, and is unlikely to
#' be suitable for long term production use.
#'
#' You can set the number of rows to preview using the n_max parameter. This
#' uses the n_max from `readr::read_csv()` under the hood.
#'
#' As there are no IDs involved, this is brittle and code relying on this
#' function will likely break whenever there is renaming of variables or items
#' in the data.
#'
#' It is recommended to take the time to set up custom queries using the
#' `query_dataset()` function instead.
#'
#' If you are using this function for more than exploratory purposes, make
#' sure you subscribe to the data set you're downloading and then keep track
#' of any updates to the data.
#'
#' @param dataset_id ID of data set
#' @param dataset_version Version number of data set
#' @param api_version EES API version
#' @param n_max maximum number of rows to preview, 10 by default, Inf will get
#'  all available rows
#' @param verbose Run with additional contextual messaging, logical,
#' default = FALSE
#'
#' @return data.frame
#' @export
#'
#' @examples
#' # Preview first 10 rows
#' preview_dataset(example_id("dataset"))
#'
#' # Get 2 rows
#' preview_dataset(example_id("dataset"), n_max = 2)
#'
#' # Get all rows
#' preview_dataset(example_id("dataset"), n_max = Inf)
preview_dataset <- function(
    dataset_id,
    dataset_version = NULL,
    api_version = NULL,
    n_max = 10,
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

  if (n_max != Inf) {
    if (!check_integer(n_max)) {
      stop("n_max must be a positive integer value, e.g. 15, or Inf")
    }
  }

  eesyapi::validate_ees_id(dataset_id, level = "dataset")

  # Generate query ------------------------------------------------------------
  query_url <- eesyapi::api_url(
    endpoint = "get-csv",
    dataset_id = dataset_id,
    verbose = verbose
  )

  # Check we can request successfully -----------------------------------------
  toggle_message("Requesting data...", verbose = verbose)

  response <- query_url |>
    httr2::request() |>
    httr2::req_perform()

  eesyapi::http_request_error(response, verbose = verbose)

  # Read in the CSV -----------------------------------------------------------
  toggle_message("Reading response...", verbose = verbose)

  output <- query_url |>
    readr::read_csv(
      show_col_types = verbose,
      progress = verbose,
      n_max = n_max
    ) |>
    as.data.frame()

  return(output)
}
