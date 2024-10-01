#' Validate page size
#'
#' @param page_size Chosen page size
#' @param min Minimum valid page_size for EES API
#' @param max Maximum valid page_size for EES API
#'
#' @return Logic
#' @export
#'
#' @examples
#' validate_page_size(20)
validate_page_size <- function(page_size, min = 1, max = 40) {
  if (!is.null(page_size)) {
    if (is.numeric(page_size)) {
      valid <- dplyr::between(page_size, 1, 40)
    } else {
      valid <- FALSE
    }
    if (!valid) {
      stop(
        "The page size can only be a numeric value within the range 1 <= page_size <= 40."
      )
    }
  }
}

#' Validate element IDs
#'
#' @param element_id ID for publication or a data set
#' @param level ID level: "publication", "dataset", "location", "filter_item" or "indicator"
#'
#' @return NULL
#' @export
#'
#' @examples
#' validate_ees_id(example_id("publication"), level = "publication")
validate_ees_id <- function(element_id, level = "publication") {
  if (!(level %in% c("publication", "dataset", "location", "filter_item", "indicator"))) {
    stop(
      paste0(
        "Non-valid element level received by validate_id.\n",
        'Should be one of "publication", "dataset", "location", "filter_item" or indicator.'
      )
    )
  }
  if (is.null(element_id)) {
    stop(
      "The variable ", level,
      "_id is NULL, please provide a valid ", level,
      "_id."
    )
  }
  if (level == "location") {
    locations <- element_id |>
      stringr::str_split("\\|", simplify = TRUE)
    if ("" %in% locations) {
      stop('Invalid locations found, these should be of the form "LEVEL|xxxx|1b3d5".')
    } else {
      # Extract the individual 5 digit location IDs
      df_locations <- locations |>
        as.data.frame() |>
        dplyr::rename(level = "V1", identifier_type = "V2", identifier = "V3")
      location_type <- df_locations |>
        dplyr::pull("identifier_type") |>
        unique()
      if (length(location_type) != 1 || !(location_type %in% c("id", "code"))) {
        stop("The middle entry in \"LEVEL|xxxx|1b3d5\" should be one of \"id\" or \"code\"")
      }
      level <- paste(level, location_type, sep = "_")
      element_id <- df_locations |>
        dplyr::pull("identifier")
    }
  }
  example_id_string <- eesyapi::example_id(level)
  if (grepl("location", level)) {
    example_id_string <- example_id_string |>
      stringr::str_split("\\|", simplify = TRUE) |>
      as.data.frame() |>
      dplyr::pull("V3")
  }
  if (any(stringr::str_length(element_id) != stringr::str_length(example_id_string))) {
    err_string <- paste0(
      "The ", level,
      "(s) provided (", paste0(element_id, collapse = ", "),
      ") is expected to be a ",
      stringr::str_length(example_id_string),
      " character string in the format:\n    ",
      example_id_string,
      "\n  Please double check your ", level,
      "_id."
    )
    stop(err_string)
  } else if (
    any(
      gsub("[0-9a-zA-Z]", "", element_id) != gsub("[0-9a-zA-Z]", "", example_id_string)
    )
  ) {
    stop(err_string)
  }
}

#' Validate filter type
#'
#' @param filter_type type of filter being queried: "time_periods", "geographic_levels",
#'
#' @return NULL
#' @export
#'
#' @examples
#' validate_ees_filter_type("time_periods")
validate_ees_filter_type <- function(filter_type) {
  if (!(filter_type %in% c("time_periods", "geographic_levels", "locations", "filter_items"))) {
    stop(
      paste(
        "filter_type keyword should be one of \"time_periods\", \"geographic_levels\",",
        "\"locations\" or \"filter_items\""
      )
    )
  }
}
