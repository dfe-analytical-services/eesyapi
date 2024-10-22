#' EES API URL paging
#'
#' @description
#' EES API results are paged, such that only one page is returned by a single request submitted
#' to the API. This function creates the URL query syntax to specify the number of results to be
#' returned per page and the page number to retrieve. This is largely used by api_url() to
#' automatically generate API request URLs, but is exported as part of the package for any users
#' who wish to generate their own URLs.
#'
#' @param page_size Number of rows to return in a single query. The maximum allowable value varies
#' between query type:
#'    - get-publications: 40
#'    - get-data-catalogue: 20
#'    - query_dataset: 10000
#' @param page Page number to return
#'
#' @return String containing pages query
#' @export
#'
#' @examples
#' api_url_pages()
#' api_url_pages(page_size = 20, page = 2)
api_url_pages <- function(page_size = 40, page = NULL) {
  paste0(
    ifelse(
      !is.null(page),
      paste0("page=", page),
      ""
    ),
    ifelse(
      !is.null(page) & !is.null(page_size),
      paste0("&"),
      ""
    ),
    ifelse(
      !is.null(page_size),
      paste0("pageSize=", page_size),
      ""
    )
  )
}
