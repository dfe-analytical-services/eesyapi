#' Parse IDs in returned API data
#'
#' @description
#' The API uses unique IDs (sqids) to identify each filter column, filter_item and indicator
#' column. This facilitates continuity, i.e. data creators may need to change the col_name and
#' labeling in their data files, whilst maintaining the same fundamental content. Results from the
#' API are given using these IDs, but it's expected that analysts will want to translate these back
#' into the col_names assigned by analysts. `parse_data_sqids()` takes a data frame extracted from
#' the API and translates all the sqids in that data frame to their assigned col_names based on
#' the meta data available from the API.
#'
#' @param data Data frame containing data as returned from the API by `get_dataset()` or
#' `post_dataset()`
#' @param dataset_id String containing the data set ID
#' @param verbose Output status messaging for user
#'
#' @return Data frame
#' @export
#'
#' @examples
#' get_dataset(example_id(), indicators = example_id("indicator"), page = 1) |>
#'   parse_sqids_dataset(example_id())
parse_sqids_dataset <- function(
    data,
    dataset_id,
    verbose = FALSE) {
  meta <- get_meta(dataset_id)
  indicators <- meta |>
    magrittr::use_series("indicators") |>
    dplyr::mutate(col_id = paste0("indicator-", !!rlang::sym("col_id"))) |>
    dplyr::filter(col_id %in% colnames(data))
  indicator_lookup <- indicators |>
    dplyr::pull("col_id")
  names(indicator_lookup) <- indicators |> dplyr::pull("col_name")
  data <- data |>
    dplyr::rename(dplyr::all_of(indicator_lookup))
  filters <- meta |>
    magrittr::use_series("filter_columns") |>
    dplyr::mutate(col_id = paste0("filter-", !!rlang::sym("col_id"))) |>
    dplyr::filter(col_id %in% colnames(data)) |>
    dplyr::pull("col_id")
  for (column in filters) {
    data <- data |>
      parse_sqids_filter(meta, column, verbose = TRUE)
  }
  return(data)
}

#' Parse IDs in a filter column
#'
#' @description
#' The API uses unique IDs (sqids) to identify each filter column and its contents (filter items).
#' This function parses those into the data creators id and item labels based on the meta data
#' stored on the API for the data set.
#'
#' @inheritParams parse_sqids_dataset
#' @param meta Meta data for the data set as provided by `get_meta()`
#' @param column_sqid The filter col_id
#'
#' @return Data frame
#' @export
#'
#' @examples
#' parse_sqids_filter(
#'   get_dataset(example_id(), indicators = example_id("indicator"), page = 1),
#'   get_meta(example_id()),
#'   example_id("filter")
#' )
parse_sqids_filter <- function(data, meta, column_sqid, verbose = FALSE) {
  if (!grepl("filter-", column_sqid)) {
    column_sqid <- paste0("filter-", column_sqid)
  }
  print(!grepl("filter-", column_sqid))
  print(column_sqid)
  col_name <- meta |>
    magrittr::use_series("filter_columns") |>
    dplyr::mutate(col_id = paste0("filter-", !!rlang::sym("col_id"))) |>
    dplyr::filter(!!rlang::sym("col_id") == column_sqid) |>
    dplyr::pull("col_name")
  if (verbose) {
    message("Matched ", column_sqid, " to ", col_name)
  }
  lookup <- meta |>
    magrittr::use_series("filter_items") |>
    dplyr::mutate(col_id = paste0("filter-", !!rlang::sym("col_id"))) |>
    dplyr::filter(!!rlang::sym("col_id") == column_sqid) |>
    dplyr::select("item_label", "item_id") |>
    dplyr::rename(
      !!rlang::sym(col_name) := "item_label",
      !!rlang::sym(column_sqid) := "item_id"
    )
  data <- data |>
    dplyr::left_join(lookup, by = column_sqid) |>
    dplyr::select(-column_sqid)
  return(data)
}

#' Parse IDs in a filter column
#'
#' @description
#' The API uses unique IDs (sqids) to identify each filter column and its contents (filter items).
#' This function parses those into the data creators id and item labels based on the meta data
#' stored on the API for the data set.
#'
#' @inheritParams parse_sqids_dataset
#' @inheritParams parse_sqids_filter
#' @param column_sqid The indicator col_id
#'
#' @return Data frame
#' @export
#'
#' @examples
#' parse_sqids_indicator(
#'   example_id("indicator"),
#'   get_meta(example_id()),
#' )
parse_sqids_indicator <- function(column_sqid, meta, verbose = FALSE) {
  col_name <- meta |>
    magrittr::use_series("indicators") |>
    dplyr::filter(!!rlang::sym("col_id") == column_sqid) |>
    dplyr::pull("col_name")
  if (verbose) {
    message("Matched ", column_sqid, " to ", col_name)
  }
  return(col_name)
}
