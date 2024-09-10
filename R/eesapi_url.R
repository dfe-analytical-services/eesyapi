#' Generate an EES API URL
#'
#' @param api_version EES API version
#' @param endpoint Name of endpoint, can be "get-summary", "get-meta", "get-data", "query-data"
#' @param dataset_id ID of data set to be connected to
#' @param dataset_version Version of data set to be connected to
#'
#' @return A string containing the URL for connecting to the EES API
#' @export
#'
#' @examples
#' eesapi_url()
eesapi_url <- function(
    api_version = "1.0",
    endpoint = NULL,
    dataset_id = NULL,
    dataset_version = NULL) {
  # Check that the API version is valid
  is_valid_api_version <- function(vapi) {
    !grepl(
      "[a-z_%+-]",
      as.character(vapi),
      ignore.case = TRUE
    )
  }

  if (is_valid_api_version(api_version) == FALSE) {
    stop(
      "You have entered an invalid API version in the api_version argument.
      This should be numerical values only."
    )
  }

  # Check that the endpoint is either NULL or valid
  is_valid_endpoint <- function(endpoint) {
    endpoint %in% c("get-summary", "get-meta", "get-data", "query-data")
  }

  if (!is.null(endpoint)) {
    if (is_valid_endpoint(endpoint) == FALSE) {
      stop(
        paste(
          "You have entered an invalid endpoint, this should one of:",
          "get-summary, get-meta, get-data or query-data"
        )
      )
    }
  }

  is_valid_dataset_info <- function(dataset_id, dataset_version) {
    !is.null(dataset_id) & (is.numeric(dataset_version) | is.null(dataset_version))
  }

  # Check that if endpoint is not NULL then neither is dataset_id
  if (!is.null(endpoint)) {
    if (is_valid_dataset_info(dataset_id, dataset_version) == FALSE) {
      stop(
        paste(
          "You have entered invalid data set info. The following rules must be",
          "met:\n",
          " - dataset_id must not be NULL\n",
          " - dataset_version should either be:\n",
          "   - NULL (gives latest version) or\n",
          "   - a numeric"
        )
      )
    }
  }


  paste0(
    "https://dev.statistics.api.education.gov.uk/api/v",
    api_version,
    "/data-sets/",
    ifelse(
      endpoint %in% c("get-summary", "get-meta", "get-data", "query-data"),
      dataset_id,
      ""
    ),
    ifelse(
      endpoint %in% c("get-summary", "get-meta", "query-data"),
      paste0("/", gsub("get-|-data", "", endpoint)),
      ""
    ),
    ifelse(
      !is.null(dataset_version),
      paste0("?dataSetVersion=", dataset_version),
      ""
    )
  )
}
