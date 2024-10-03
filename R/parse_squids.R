#' Parse IDs in returned API data
#'
#' @description
#' The API uses unique IDs (squids) to identify each filter column, filter_item and indicator
#' column. This facilitates continuity, i.e. data creators may need to change the col_name and
#' labeling in their data files, whilst maintaining the same fundamental content. Results from the
#' API are given using these IDs, but it's expected that analysts will want to translate these back
#' into the col_names assigned by analysts. `parse_data_squids()` takes a data frame extracted from
#' the API and translates all the squids in that data frame to their assigned col_names based on
#' the meta data available from the API.
#'
#' @param data Data frame containing data as returned from the API by `get_dataset()` or
#' `post_dataset()`
#' @param dataset_id String containing the data set ID
#'
#' @return Data frame
#' @export
#'
#' @examples
#' get_dataset(example_id(), indicators = example_id("indicator"), page = 1) |>
#'   parse_squids_dataset(example_id())
parse_squids_dataset <- function(
    data,
    dataset_id) {
  meta <- get_meta(dataset_id)
  filters <- meta |>
    magrittr::use_series("filter_columns") |>
    dplyr::pull(col_name)
  for (column in filters) {
    data <- data |>
      parse_squids_filter(meta, column)
  }
  return(data)
}

#' Parse IDs in a filter column
#'
#' @description
#' The API uses unique IDs (squids) to identify each filter column and its contents (filter items).
#' This function parses those into the data creators id and item labels based on the meta data
#' stored on the API for the data set.
#'
#' @inheritParams parse_squids_dataset
#' @param meta Meta data for the data set as provided by `get_meta()`
#' @param column_squid The
#'
#' @return Data frame
#' @export
#'
#' @examples
parse_squids_filter <- function(data, meta, column_squid) {
  col_name <- meta |>
    magrittr::use_series("filter_columns") |>
    dplyr::filter(col_name == column_squid) |>
    dplyr::pull("label")
  lookup <- meta |>
    magrittr::use_series("filter_items") |>
    dplyr::filter(col_name == column_squid) |>
    dplyr::select("item_label", "item_id") |>
    dplyr::rename(
      !!rlang::sym(col_name) := "item_label",
      !!rlang::sym(column_squid) := "item_id"
    )
  data <- data |>
    dplyr::left_join(lookup) |>
    dplyr::select(-column_squid)
  return(data)
}
