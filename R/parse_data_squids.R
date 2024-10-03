#' Parse IDs in returned API data
#'
#' @description
#' The API uses unique IDs (squids) to identify each filter column, filter_item and indicator column.
#' This facilitates continuity, i.e. data creators may need to change the col_name and labeling in
#' their data files, whilst maintaining the same fundamental content. Results from the API are given
#' using these IDs, but it's expected that analysts will want to translate these back into the
#' col_names assigned by analysts. `parse_data_squids()` takes a data frame extracted from the API
#' and translates all the squids in that data frame to their assigned col_names based on the meta
#' data available from the API.
#'
#' @param data Data frame containing data as returned from the API by `get_dataset()` or
#' `post_dataset()`
#' @param datatset_id String containing the data set ID
#'
#' @return Data frame
#' @export
#'
#' @examples
#' get_dataset(example_id(), indicators = example_id("indicator"), page = 1) |>
#'   parse_data_squids(example_id())
parse_data_squids <- function(
    data,
    datatset_id) {
  meta <- get_meta(dataset_id)
}
