#' Get the base API response for a data set's meta data
#'
#' @description
#' Get a list of metadata information for a data set available from the EES API. Provides either
#' look-up tables from human readable labels to ids used in the API, or the raw response from the
#' meta endpoint.
#'
#' @param dataset_id ID of data set to be connected to
#' @param dataset_version Version of data set to be connected to
#' @param api_version EES API version
#'
#' @return List of data frames containing a data set's meta data
#' @export
#'
#' @examples
#' get_meta(example_id())
get_meta <- function(dataset_id, dataset_version = NULL, api_version = NULL) {
  meta_data_response <- get_meta_response(
    dataset_id,
    dataset_version = dataset_version,
    api_version = api_version,
    parse = TRUE
  )
  meta_data <- list(
    time_periods = parse_meta_time_periods(meta_data_response$timePeriods),
    locations = parse_meta_location_ids(meta_data_response$locations),
    filter_columns = parse_meta_filter_columns(meta_data_response$filters),
    filter_items = parse_meta_filter_item_ids(meta_data_response$filters),
    indicators = parse_meta_filter_columns(meta_data_response$indicators)
  )
  return(meta_data)
}

#' Get the base API response for a data set's meta data
#'
#' @description
#' Get the metadata information for a data set available from the EES API.
#'
#' @param dataset_id ID of data set to be connected to
#' @param dataset_version Version of data set to be connected to
#' @param api_version EES API version
#' @param parse Parse result into structured list
#'
#' @return Results of query to API meta data endpoint
#' @export
#'
#' @examples
#' get_meta_response(example_id())
get_meta_response <- function(
    dataset_id,
    dataset_version = NULL,
    api_version = NULL,
    parse = TRUE) {
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
      "Query returned error, status ",
      response$status,
      ": ",
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

#' Parse API meta to give the time periods
#'
#' @param api_meta_time_periods Time periods information provided by the API output
#'
#' @return Data frame containing location item codes matched
#' @export
#'
#' @examples
#' get_meta_response(example_id())$timePeriods |>
#'   parse_meta_time_periods()
parse_meta_time_periods <- function(api_meta_time_periods) {
  time_periods <- api_meta_time_periods |>
    dplyr::mutate(code_num = as.numeric(gsub("[a-zA-Z]", "", api_meta_time_periods$code)))
  time_periods <- time_periods |>
    dplyr::arrange(time_periods$code_num) |>
    dplyr::select(-c("code_num"))
  return(time_periods)
}


#' Parse API meta to give the locations
#'
#' @param api_meta_locations Locations information provided by the API output
#'
#' @return Data frame containing location item codes matched
#' @export
#'
#' @examples
#' get_meta_response(example_id())$locations |>
#'   parse_meta_location_ids()
parse_meta_location_ids <- function(api_meta_locations) {
  nlevels <- length(api_meta_locations$level)
  location_items <- data.frame(
    geographic_level = NA,
    code = NA,
    label = NA,
    item_id = NA
  )
  location_items <- location_items |>
    dplyr::filter(!is.na(location_items$geographic_level))
  for (i in 1:nlevels) {
    location_items_i <- as.data.frame(
      api_meta_locations$options[i]
    ) |>
      dplyr::mutate(geographic_level = api_meta_locations$level$label[i])
    location_items <- location_items |>
      rbind(
        location_items_i |>
          dplyr::select("geographic_level", "code", "label", item_id = "id")
      )
  }
  return(location_items)
}

#' Parse API meta to give the filter columns
#'
#' @param api_meta_filters Filter information provided by the API output
#'
#' @return data frame containing column names and labels
#' @export
#'
#' @examples
#' get_meta_response(example_id())$filters |>
#'   parse_meta_filter_columns()
parse_meta_filter_columns <- function(api_meta_filters) {
  data.frame(
    col_name = api_meta_filters$id,
    label = api_meta_filters$label
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
#' get_meta_response(example_id())$filters |>
#'   parse_meta_filter_item_ids()
parse_meta_filter_item_ids <- function(api_meta_filters) {
  nfilters <- length(api_meta_filters$id)
  filter_items <- data.frame(
    col_name = NA,
    item_id = NA,
    item_label = NA,
    isAggregate = NA
  )
  filter_items <- filter_items |>
    dplyr::filter(!is.na(filter_items$col_name))
  for (i in 1:nfilters) {
    filter_items_i <- as.data.frame(
      api_meta_filters$options[i]
    ) |>
      dplyr::rename(
        item_id = "id",
        item_label = "label"
      ) |>
      dplyr::mutate(col_name = api_meta_filters$id[i])
    if (!("isAggregate" %in% names(filter_items_i))) {
      filter_items_i <- filter_items_i |>
        dplyr::mutate(isAggregate = NA)
    }
    filter_items <- filter_items |>
      rbind(
        filter_items_i |>
          dplyr::select("col_name", "item_label", "item_id", default_item = "isAggregate")
      )
  }
  return(filter_items)
}

#' Parse API meta to give the indicator columns
#'
#' @param api_meta_indicators Indicator information provided by the API output
#'
#' @return data frame containing indicator column names and labels
#' @export
#'
#' @examples
#' get_meta_response(example_id())$indicators |>
#'   parse_meta_indicator_columns()
parse_meta_indicator_columns <- function(api_meta_indicators) {
  data.frame(
    col_name = api_meta_indicators$id,
    label = api_meta_indicators$label
  )
}
