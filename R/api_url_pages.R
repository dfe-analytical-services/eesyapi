#' EES API URL paging
#'
#' @param page_size Number of results to return in a single query (max 40)
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
