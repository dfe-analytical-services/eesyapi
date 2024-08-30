#' eesapi_url
#'
#' @param api_version EES API version
#' @param endpoint Name of endpoint, can be get-summary, get-meta, get-data, query-data
#' @param dataset_id ID of dataset to be connected to
#' @param dataset_version Verson of dataset to be connected to
#'
#' @return The EES URL for connecting to the API
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

  # Check that if endpoint is not NULL then neither is dataset_id
  if (!is.null(endpoint)) {
    if (is.null(dataset_id)) {
      stop("If an endpoint is set, then daatset_id should not be NULL")
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
      !is.null(data_version),
      paste0("?data-version=",dataset_version)
    )
  )
}
