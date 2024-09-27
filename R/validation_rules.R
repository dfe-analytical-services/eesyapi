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
  skip_tests <- FALSE
  if (is.null(element_id)) {
    stop(
      "The variable ", level,
      "_id is NULL, please provide a valid ", level,
      "_id."
    )
  } else {
    if (level == "location") {
      locations <- element_id |>
        stringr::str_split("\\|")
      if (any(locations |> sapply(length) < 3)) {
        stop('Invalid location IDs found, these should be of the form "XXX|xxxx|1b3d5".')
      } else {
        # Extract the individual 5 digit location IDs
        locations <- locations |>
          as.data.frame() |>
          t() |>
          as.data.frame() |>
          dplyr::rename(level = "V1", type = "V2", value = "V3")
        element_id_proc <- locations$value
        if (all(locations$type == "id")) {
          example_vector <- eesyapi::example_id("location") |>
            stringr::str_split("\\|")
          example_id_string <- example_vector[[1]][3]
        } else {
          skip_tests <- TRUE
        }
      }
    } else {
      element_id_proc <- element_id
      example_id_string <- eesyapi::example_id(level)
    }
    if (!skip_tests) {
      if (any(stringr::str_length(element_id) != stringr::str_length(eesyapi::example_id(level)))) {
        err_string <- paste0(
          "The ", level,
          "_id(s) provided (", paste0(element_id_proc, collapse = ", "),
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
          gsub("[0-9a-zA-Z]", "", element_id_proc) != gsub("[0-9a-zA-Z]", "", example_id_string)
        )
      ) {
        stop(err_string)
      }
    }
  }
}

#' Validate filter type
#'
#' @param filter_type
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
