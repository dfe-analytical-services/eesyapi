#' Get a data set's metadata
#'
#' @description
#' Get a list of metadata information for a data set available from the EES API. Provides either
#' look-up tables from human readable labels to ids used in the API, or the raw response from the
#' meta endpoint.
#'
#' @param dataset_id ID of data set to be connected to
#' @param dataset_version Version of data set to be connected to
#' @param api_version EES API version
#' @param parse Parse result into structured list
#'
#' @return Results of query to meta data
#' @export
#'
#' @examples
#' get_meta("d7329101-f275-d277-bbfe-d8cfaa709833")
get_meta <- function(dataset_id, dataset_version = NULL, api_version = NULL, parse = TRUE) {
  # Check that the parse flag is valid
  if (is.logical(parse) == FALSE) {
    stop(
      "You have entered an invalid parse argument, this should be a logical TRUE or FALSE only."
    )
  }

  # Use eesyapi_url to retrieve the relevant api url - note that this will perform
  # validation checks on dataset_id, dataset_version and api_version, so haven't
  # added any explicit validation of those to the current function.
  meta_url <- eesyapi::eesapi_url(
    endpoint = "get-meta",
    dataset_id = dataset_id,
    dataset_version = dataset_version
  )

  response <- httr::GET(meta_url)
  if (response$status_code > 299) {
    stop(paste0(
      "Query returned error, status: ",
      response$status,
      "\n      ",
      eesyapi::http_request_error(response$status)
    ))
  } else {
    if (parse) {
      result <- response |>
        httr::content("text") |>
        jsonlite::fromJSON()
    } else {
      result <- response
    }
    return(result)
  }
}

#' Parse API meta to give the filter columns
#'
#' @param api_meta_filters Filter information provided by the API output
#'
#' @return data frame containing column names and labels
#' @export
#'
#' @examples
parse_meta_filter_columns <- function(api_meta_filters) {
  data.frame(
    col_name = api_meta_filters$id,
    col_label = api_meta_filters$label
  )
}

#' Parse API meta to give the filter item codes
#'
#' @param api_meta_filters Filter information provided by the API output
#'
#' @return Data frame containing filter item codes matched to filter item labels and col_name
#' @export
#'
#' @examples
parse_meta_filter_item_codes <- function(api_meta_filters) {
  nfilters <- length(api_meta_filters$id)
  filter_items <- data.frame(
    col_name = NA,
    item_code = NA,
    item_label = NA,
    isAggregate = NA
  ) |>
    dplyr::filter(!is.na(col_name))
  for (i in 1:nfilters) {
    filter_items_i <- as.data.frame(
      api_meta_filters$options[i]
    ) |>
      dplyr::rename(
        item_code = id,
        item_label = label
      ) %>%
      dplyr::mutate(col_name = api_meta_filters$id[i])
    if (!("isAggregate" %in% names(filter_items_i))) {
      filter_items_i <- filter_items_i |>
        dplyr::mutate(isAggregate = NA)
    }
    filter_items <- filter_items |>
      rbind(
        filter_items_i |>
          dplyr::select(col_name, item_label, item_code, isAggregate)
      )
  }
  return(filter_items)
}
