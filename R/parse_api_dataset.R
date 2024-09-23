#' Parse contents of API data set json output
#'
#' @description
#' This takes the json results output from the API and converts it into a usable data frame.
#' It's primary use is as a helper to get_dataset and post_dataset, but can be used in
#' isolation from those by running an API query and passing the following to
#' parse_api_dataset():
#'    response |>
#'       httr::content("text") |>
#'       jsonlite::fromJSON() |>
#'       parse_api_dataset()
#'
#' @param api_data_result A json data result list as returned from the API
#' @param dataset_id ID of data set to be connected to.
#'
#' @return Data frame containing API data results
#' @export
#'
#' @examples
#' api_url("get-data", dataset_id = example_id(), indicators = example_id("indicator"), page_size=10) |>
#'   httr::GET() |>
#'   httr::content("text") |>
#'   jsonlite::fromJSON() |>
#'   parse_api_dataset()
parse_api_dataset <- function(
    api_data_result,
    dataset_id = NULL,
    verbose = FALSE) {
  if (!is.null(dataset_id)) {
    eesyapi::validate_ees_id(dataset_id, level = "dataset")
  }
  if ("results" %in% names(api_data_result)) {
    api_data_result <- api_data_result$results
  }
  if(verbose){
    print(names(api_data_result))
    print(names(api_data_result$locations))
    print(names(api_data_result$filters))
  }
  dplyr::bind_cols(
    api_data_result$timePeriod,
    data.frame(geographic_level = api_data_result$geographicLevel),
    api_data_result$locations,
    api_data_result$filters,
    api_data_result$values
  )
  # Next aim here is to pull in the meta data automatically at this point to translate
  # all the API codes...
}
