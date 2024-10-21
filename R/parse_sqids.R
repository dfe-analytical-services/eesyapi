#' Parse location sqids
#'
#' @description
#' The API uses unique IDs (sqids) to identify each location in a data set. This function parses
#' those into the corresponding location codes and names based on the meta data stored on the API
#' for the data set.
#'
#' @inheritParams parse_sqids_filters
#' @param locations A set of location columns as taken from a data set downloaded from the API
#'
#' @return Data frame of parsed geography information
#' @export
#'
#' @examples
#' example_data_raw() |>
#'   magrittr::use_series("locations") |>
#'   parse_sqids_locations(get_meta(example_id(group = "attendance")))
parse_sqids_locations <- function(locations, meta, verbose = FALSE) {
  lookup <- meta |>
    magrittr::use_series("locations") |>
    dplyr::filter(!!rlang::sym("geographic_level_code") %in% names(locations)) |>
    dplyr::rename(name = "label")
  for (level in names(locations)) {
    locations <- locations |>
      dplyr::rename("item_id" = !!rlang::sym(level)) |>
      dplyr::left_join(
        lookup |>
          dplyr::filter(!!rlang::sym("geographic_level_code") == level) |>
          dplyr::select(-dplyr::all_of(c("geographic_level_code", "geographic_level"))) |>
          dplyr::rename_with(~ paste0(tolower(level), "_", .x), !dplyr::matches("item_id")),
        by = dplyr::join_by("item_id")
      ) |>
      dplyr::select(-"item_id")
  }
  return(
    locations |>
      dplyr::select(
        dplyr::where(
          ~ !all(is.na(.x))
        )
      )
  )
}

#' Parse IDs in a set of filters
#'
#' @description
#' The API uses unique IDs (sqids) to identify each filter column and its contents (filter items).
#' This function parses those into the data creators' id and item labels based on the meta data
#' stored on the API for the data set.
#'
#' @param filters A set of filter item columns as taken from a data set downloaded from the API
#' @param meta Meta data for the data set as provided by `get_meta()`
#' @param verbose Run in verbose mode with debugging messages
#'
#' @return Data frame
#' @export
#'
#' @examples
#' example_data_raw() |>
#'   magrittr::use_series("filters") |>
#'   parse_sqids_filters(get_meta(example_id(group = "attendance")))
parse_sqids_filters <- function(filters, meta, verbose = FALSE) {
  filter_ids <- meta |>
    magrittr::use_series("filter_columns") |>
    dplyr::filter(!!rlang::sym("col_id") %in% colnames(filters)) |>
    dplyr::pull("col_id")
  if (verbose) {
    print(filter_ids)
  }
  for (column_sqid in filter_ids) {
    col_name <- meta |>
      magrittr::use_series("filter_columns") |>
      dplyr::filter(!!rlang::sym("col_id") == column_sqid) |>
      dplyr::pull("col_name")
    if (verbose) {
      message("Matched ", column_sqid, " to ", col_name)
    }
    lookup <- meta |>
      magrittr::use_series("filter_items") |>
      dplyr::filter(!!rlang::sym("col_id") == column_sqid) |>
      dplyr::select("item_label", "item_id") |>
      dplyr::rename(
        !!rlang::sym(col_name) := "item_label",
        !!rlang::sym(column_sqid) := "item_id"
      )
    filters <- filters |>
      dplyr::left_join(lookup, by = column_sqid) |>
      dplyr::select(-dplyr::all_of(column_sqid))
  }
  return(filters)
}

#' Parse IDs in a set of indicators
#'
#' @description
#' The API uses unique IDs (sqids) to identify each indicator column. This function parses those
#' into the data creators' column names based on the meta data stored on the API for the data set.
#'
#' @inheritParams parse_sqids_filters
#' @param indicators A set of indicator columns as taken from a data set downloaded from the API
#'
#' @return Data frame
#' @export
#'
#' @examples
#' example_data_raw(group = "attendance") |>
#'   magrittr::use_series("values") |>
#'   parse_sqids_indicators(get_meta(example_id(group = "attendance")))
parse_sqids_indicators <- function(indicators, meta, verbose = FALSE) {
  indicator_ids <- meta |>
    magrittr::use_series("indicators") |>
    dplyr::filter(!!rlang::sym("col_id") %in% colnames(indicators))
  indicator_lookup <- indicator_ids |>
    dplyr::pull("col_id")
  names(indicator_lookup) <- indicator_ids |> dplyr::pull("col_name")
  indicators <- indicators |>
    dplyr::rename(dplyr::all_of(indicator_lookup))
  return(indicators)
}
