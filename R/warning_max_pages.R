#' Overshot maximum pages in results
#'
#' @param api_result Output from an API get query
#'
#' @return NULL
#' @export
#'
#' @examples
#' get_publications(40, 4) |> warning_max_pages()
warning_max_pages <- function(api_result) {
  if (api_result$paging$page > api_result$paging$totalPages) {
    warning(
      paste0(
        "Your query has requested a page number (",
        api_result$paging$page,
        ") greater than the available number of result pages (",
        api_result$paging$totalPages,
        ")."
      )
    )
  }
}
