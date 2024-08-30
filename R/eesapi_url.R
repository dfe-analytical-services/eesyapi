#' eesapi_url
#'
#' @param api_version EES API version
#'
#' @return The EES URL for connecting to the API
#' @export
#'
#' @examples
#' eesapi_url()
eesapi_url <- function(api_version="1.0"){
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
  paste0("https://dev.statistics.api.education.gov.uk/api/v", api_version,"/data-sets/")
}
