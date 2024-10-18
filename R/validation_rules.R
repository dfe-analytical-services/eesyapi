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

#' Validate time periods
#'
#' @inheritParams api_url_query
#'
#' @return NULL
#' @export
#'
#' @examples
#' validate_time_periods(c("2023|AY", "2024|AY"))
validate_time_periods <- function(time_periods) {
  time_pipes <- time_periods |>
    stringr::str_replace_all("[a-zA-Z0-9]", "")
  if (!all(stringr::str_length(time_pipes) == 1)) {
    invalid_pipes <- time_periods[stringr::str_length(time_pipes) != 1]
    stop(
      paste(
        "Invalid time periods provided:",
        paste(invalid_pipes, collapse = ", "),
        "\nThese should be in the format {period}|{code}, e.g. 2024|AY, 2023|W21"
      )
    )
  }
}

#' Validate element IDs
#'
#' @param element_id ID for publication or a data set
#' @param level ID level: "publication", "dataset", "location", "filter_item" or "indicator"
#' @param verbose Run in verbose mode
#'
#' @return NULL
#' @export
#'
#' @examples
#' validate_ees_id(example_id("publication"), level = "publication")
validate_ees_id <- function(element_id, level = "publication", verbose = FALSE) {
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
    if ("" %in% locations || ncol(locations) != 3) {
      stop('Invalid locations found, these should be of the form "LEVEL|xxxx|1b3d5".')
    } else {
      # Extract the individual 5 digit location IDs
      df_locations <- locations |>
        as.data.frame() |>
        dplyr::rename(level = "V1", identifier_type = "V2", identifier = "V3")
      location_type <- df_locations |>
        dplyr::pull("identifier_type") |>
        unique()
      if (any(!(location_type %in% c("id", "code")))) {
        stop("The middle entry in \"LEVEL|xxxx|1b3d5\" should be one of \"id\" or \"code\"")
      }
      level <- paste(level, location_type, sep = "_")
      element_id <- df_locations
    }
  } else {
    element_id <- data.frame(identifier = element_id) |>
      dplyr::mutate(identifier_type = "id")
  }
  example_id_string <- eesyapi::example_id(level, group = "attendance")
  if (any(grepl("location", level))) {
    example_id_string <- example_id_string |>
      stringr::str_split("\\|", simplify = TRUE) |>
      as.data.frame() |>
      dplyr::rename(
        identifier_type = "V2",
        identifier = "V3"
      )
  } else {
    example_id_string <- data.frame(identifier = example_id_string) |>
      dplyr::mutate(identifier_type = "id")
  }
  check_frame <- element_id |>
    dplyr::left_join(example_id_string, by = "identifier_type")
  error_rows <- check_frame |>
    dplyr::filter(
      !!rlang::sym("identifier_type") == "id",
      stringr::str_length(!!rlang::sym("identifier.x")) <
        stringr::str_length(!!rlang::sym("identifier.y"))
    ) |>
    dplyr::bind_rows(
      check_frame |>
        dplyr::filter(
          !!rlang::sym("identifier_type") == "code",
          stringr::str_length(!!rlang::sym("identifier.x")) !=
            stringr::str_length(!!rlang::sym("identifier.y"))
        )
    )
  if (nrow(error_rows) != 0) {
    err_string <- paste0(
      "The ", paste(level, collapse = ","),
      "(s) provided (",
      paste0(error_rows |> dplyr::pull("identifier.x"), collapse = ", "),
      ") is expected to be a ",
      paste0(error_rows |> dplyr::pull("identifier.y") |> stringr::str_length(), collapse = ", "),
      " character string in the format:\n    ",
      paste0(error_rows |> dplyr::pull("identifier.y"), collapse = ", "),
      "\n  Please double check your ", paste(level, collapse = ","),
      "."
    )
    stop(err_string)
  } else if (
    any(
      gsub("[0-9a-zA-Z]", "", element_id |> dplyr::pull("identifier")) !=
        gsub("[0-9a-zA-Z]", "", example_id_string |> dplyr::pull("identifier"))
    )
  ) {
    stop(
      paste(
        "Some elements in",
        paste(element_id |> dplyr::pull("identifier"), collapse = ", "),
        "do not match the expected structure: ",
        example_id_string |> dplyr::pull("identifier")
      )
    )
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
