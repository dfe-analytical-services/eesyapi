#' Generate an EES API URL
#'
#' @description
#' This function returns a single URL to connect to a chosen EES API endpoint. The resulting URL can
#' be used with GET or POST (as appropriate) in order to connect. Whether the publication_id or
#' dataset_id (or neither) parameter are required depends on the endpoint chose.
#'
#' \tabular{lr}{
#' \strong{Endpoints} \tab \strong{id required} \cr
#' get-publications \tab Neither  \cr
#' get-data-catalogue \tab publication_id  \cr
#' get-summary, get-meta, get-data, post-data \tab dataset_id  \cr
#' }
#'
#' @param endpoint Name of endpoint, can be "get-publications", "get-data-catalogue",
#' "get-summary", "get-meta", "get-data" or "post-data"
#' @param publication_id ID of the publication to be connected to. This is required if the
#' endpoint is "get-data-catalogue"
#' @param dataset_id ID of data set to be connected to. This is required if the endpoint is one
#' of "get-summary", "get-meta", "get-data" or "post-data"
#' @inheritParams api_url_query
#' @param dataset_version Version of data set to be connected to
#' @param page_size Number of results to return in a single query
#' @param page Page number of query results to return
#' @param api_version EES API version
#' @param environment EES environment to connect to: "dev", "test", "preprod" or "prod"
#' @param verbose Add extra contextual information whilst running
#'
#' @return A string containing the URL for connecting to the EES API
#' @export
#'
#' @examples
#' api_url()
#' api_url("get-publications")
#' api_url("get-data-catalogue", publication_id = eesyapi::example_id("publication"))
#' api_url("get-summary", dataset_id = eesyapi::example_id("dataset"))
#' api_url("get-meta", dataset_id = eesyapi::example_id("dataset"))
#' api_url(
#'   "get-data",
#'   dataset_id = eesyapi::example_id("dataset"),
#'   indicators = example_id("indicator"),
#'   time_periods = c("2024|W12", "2024|W13"),
#'   geographic_levels = c("NAT"),
#'   filter_items = example_id("filter_item")
#' )
#' api_url(
#'   "post-data",
#'   dataset_id = eesyapi::example_id("dataset"),
#'   indicators = example_id("indicator")
#' )
api_url <- function(
    endpoint = "get-publications",
    publication_id = NULL,
    dataset_id = NULL,
    indicators = NULL,
    time_periods = NULL,
    geographic_levels = NULL,
    locations = NULL,
    filter_items = NULL,
    dataset_version = NULL,
    page_size = NULL,
    page = NULL,
    api_version = "1.0",
    environment = "dev",
    verbose = FALSE) {
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
    endpoint %in% c(
      "get-publications", "get-data-catalogue",
      "get-summary", "get-meta",
      "get-data", "post-data"
    )
  }

  if (!is.null(endpoint)) {
    if (is_valid_endpoint(endpoint) == FALSE) {
      stop(
        paste(
          "You have entered an invalid endpoint, this should one of:",
          "get-summary, get-meta, get-data or post-data"
        )
      )
    }
  }

  is_valid_dataset_info <- function(dataset_id, dataset_version) {
    !is.null(dataset_id) & (is.numeric(dataset_version) | is.null(dataset_version))
  }

  # Check that if endpoint requires a data set then dataset_id is not null
  if (endpoint %in% c("get-summary", "get-meta", "get-data", "post-data")) {
    eesyapi::validate_ees_id(dataset_id, level = "dataset")
    if (is_valid_dataset_info(dataset_id, dataset_version) == FALSE) {
      stop(
        paste(
          "You have entered invalid data set info. The following rules must be",
          "met:\n",
          "   - dataset_id must not be NULL\n",
          "   - dataset_version should either be:\n",
          "     - NULL (gives latest version) or\n",
          "     - a numeric"
        )
      )
    }
  }

  # Check the environment param is valid
  if (!(environment %in% c("dev", "test", "preprod", "prod"))) {
    stop(
      paste(
        "You have entered invalid EES environment. The environment should be one of:\n",
        "   - dev, test, preprod or prod"
      )
    )
  }
  # End of validation

  endpoint_base <- list(
    dev = "https://dev.statistics.api.education.gov.uk/api/",
    test = "https://test.statistics.api.education.gov.uk/api/",
    preprod = "https://pre-production.statistics.api.education.gov.uk/api/",
    prod = "https://statistics.api.education.gov.uk/api/"
  )

  endpoint_base_version <- paste0(
    endpoint_base[[environment]],
    "v", api_version, "/"
  )

  if (endpoint == "get-publications") {
    url <- paste0(
      endpoint_base_version,
      "publications?",
      eesyapi::api_url_pages(page_size = page_size, page = page)
    )
  } else if (endpoint == "get-data-catalogue") {
    url <- paste0(
      endpoint_base_version,
      "publications/",
      publication_id,
      "/data-sets?",
      eesyapi::api_url_pages(page_size = page_size, page = page)
    )
  } else {
    url <- paste0(
      endpoint_base_version,
      "data-sets/",
      ifelse(
        endpoint %in% c("get-summary", "get-meta", "get-data", "post-data"),
        dataset_id,
        ""
      )
    )
    if (endpoint != "get-summary") {
      url <- paste0(
        url,
        ifelse(
          endpoint == "get-meta",
          paste0("/", "meta"),
          paste0("/", "query")
        ),
        ifelse(
          !is.null(dataset_version),
          paste0("?dataSetVersion=", dataset_version),
          ""
        )
      )
    }
    if (endpoint == "get-data") {
      # Force default page size if page is given by user and page_size isn't
      if (!is.null(page) && is.null(page_size)) {
        page_size <- 1000
      }
      # Force first page if page size is given by user and page isn't
      if (!is.null(page_size) && is.null(page)) {
        page <- 1
      }
      if (verbose) {
        message(paste("paging:", page, page_size))
      }
      url <- url |>
        paste0(
          eesyapi::api_url_query(
            indicators = indicators,
            time_periods = time_periods,
            geographic_levels = geographic_levels,
            locations = locations,
            filter_items = filter_items
          ),
          ifelse(
            !is.null(page) & !is.null(page_size),
            paste0("&", eesyapi::api_url_pages(page_size = page_size, page = page)),
            ""
          )
        )
    }
  }
  if (endpoint %in% c("get-publications", "get-data-catalogue", "get-summary", "get-meta")) {
    if (
      any(!is.null(c(time_periods, geographic_levels, locations, filter_items, indicators)))
    ) {
      warning(
        paste0(
          "None of the params ",
          "time_periods, geographic_levels, locations, filter_items or indicators",
          " are used by api_url when the endpoint param is set to ",
          endpoint
        )
      )
    }
  }
  if (verbose) {
    cat("Generated the following query url:", fill = TRUE)
    cat(url, fill = TRUE)
  }
  return(url)
}
