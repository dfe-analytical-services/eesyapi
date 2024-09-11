#' Validate page size
#'
#' @param page_size Chosen page size
#' @param min Minimum valid page_size for EES API
#' @param max Maximum valid page_size for EES API
#'
#' @return Logic
#'
#' @examples
#' validate_page_size(20)
validate_page_size <- function(page_size, min = 1, max = 40) {
  if (is.numeric(page_size)) {
    valid <- dplyr::between(page_size, 1, 40)
  } else {
    valid <- FALSE
  }
  if(!valid){
    stop(
      "The page size can only be a numeric value within the range 1 <= page_size <= 40."
    )
  }
}
