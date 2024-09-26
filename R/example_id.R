#' Example ID
#' @description
#' This function returns examples of working IDs that can be used with the API.
#'
#' @param level Level of ID example to return: "publication" or "data set"
#' @param environment Environment to return a working example for
#' @param group Choose the publication group of examples to use. Can be "attendance".
#'
#' @return String containing an example ID present in the API
#' @export
#'
#' @examples
#' example_id()
example_id <- function(
    level = "dataset",
    environment = "dev",
    group = "public-api-testing") {
  example_ids <- data.frame(
    levels = c(
      "publication",
      "dataset",
      "location",
      "filter_item",
      "indicator",
      "publication",
      "dataset",
      "location",
      "filter_item",
      "indicator"
    ),
    environments = c(
      "dev",
      "dev",
      "dev",
      "dev",
      "dev",
      "dev",
      "dev",
      "dev",
      "dev",
      "dev"
    ),
    example_group = c(
      "attendance",
      "attendance",
      "attendance",
      "attendance",
      "attendance",
      "public-api-testing",
      "public-api-testing",
      "public-api-testing",
      "public-api-testing",
      "public-api-testing"
    ),
    examples = c(
      "b6d9ed96-be68-4791-abc3-08dcaba68c04",
      "7c0e9201-c7c0-ff73-bee4-304e731ec0e6",
      "NAT|id|dP0Zw",
      "hl2Gy",
      "bqZtT",
      "d823e4df-626f-4450-9b21-08dc8b95fc02",
      "830f9201-9e11-ad75-8dcd-d2efe2834457",
      "LA|id|ml79K",
      "kKOhs",
      "mRj9K"
    )
  )
  if (!(level %in% example_ids$levels)) {
    stop(
      paste0(
        "Non-valid element level received by validate_id.\n",
        'Should be one of "publication", "dataset", "filter_item" or indicator.'
      )
    )
  }
  return(
    example_ids |>
      dplyr::filter(
        example_ids$levels == level,
        example_ids$environments == environment,
        example_ids$example_group == group
      ) |>
      dplyr::pull("examples")
  )
}
