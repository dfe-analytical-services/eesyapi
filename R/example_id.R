#' Example ID
#' @description
#' This function returns examples of working IDs that can be used with the API.
#'
#' @param level Level of ID example to return: "publication" or "data set"
#' @param environment Environment to return a working example for
#'
#' @return String containing an example ID present in the API
#' @export
#'
#' @examples
#' example_id()
example_id <- function(level = "dataset", environment = "dev") {
  examples <- data.frame(
    type = c(
      "publication",
      "dataset"
    ),
    environment = c(
      "dev",
      "dev"
    ),
    example = c(
      "b6d9ed96-be68-4791-abc3-08dcaba68c04",
      "d7329101-f275-d277-bbfe-d8cfaa709833"
    )
  )
  return(
    examples |>
      dplyr::filter(
        examples$type == level,
        examples$environment == environment
      ) |>
      dplyr::pull("example")
  )
}
