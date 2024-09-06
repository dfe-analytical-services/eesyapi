#' Get data set catalogue
#'
#' @param publication_id The publication ID, leave as NULL to query across all publications
#'
#' @return Data frame listing data sets
#' @export
#'
#' @examples
get_data_catalogue <- function(publication = NULL) {
  if (is.null(publication)) {
    message("I'm going to get a listing of all data sets from all publications...")
  } else {
    message("I'm going to get a listing of all data sets from a single publication...")
  }
}

#' Get publications
#'
#' @return Data frame listing all available publications
#' @export
#'
#' @examples
get_publications <- function() {

}
