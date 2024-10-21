#' Controllable console messages
#'
#' Quick expansion to the `message()` function aimed for use in functions for
#' an easy addition of a global verbose TRUE / FALSE argument to toggle the
#' messages on or off
#'
#' @param ... any message you would normally pass into `message()`. See
#' \code{\link{message}} for more details
#'
#' @param verbose logical, usually a variable passed from the function you are
#' using this within
#'
#' @keywords internal
toggle_message <- function(..., verbose) {
  if (verbose) {
    message(...)
  }
}
