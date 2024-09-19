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
#' @param level ID level: "publication" or "dataset"
#'
#' @return NULL
#' @export
#'
#' @examples
#' validate_ees_id(example_id("publication"), level = "publication")
validate_ees_id <- function(element_id, level = "publication") {
  if (!(level %in% c("publication", "dataset", "filter_item"))) {
    stop(
      paste0(
        "Non-valid element level received by validate_id.\n",
        'Should be one of "publication", "dataset" or "filter_item".'
      )
    )
  }
  if (is.null(element_id)) {
    stop(
      "The variable ", level,
      "_id is NULL, please provide a valid ", level,
      "_id."
    )
  } else {
    err_string <- paste0(
      "The ", level,
      "_id(s) provided (", paste0(element_id, collapse = ", "),
      ") is expected to be a ",
      stringr::str_length(example_id(level)),
      " character string in the format:\n    ",
      eesyapi::example_id(level),
      "\n  Please double check your ", level,
      "_id."
    )

    if (any(stringr::str_length(element_id) != stringr::str_length(eesyapi::example_id(level)))) {
      stop(err_string)
    } else if (
      any(
        gsub("[0-9a-zA-Z]", "", element_id) != gsub("[0-9a-zA-Z]", "", eesyapi::example_id(level))
      )
    ) {
      stop(err_string)
    }
  }
}
