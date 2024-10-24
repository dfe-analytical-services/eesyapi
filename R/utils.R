#' Set current default API version
#'
#' @return String containing default API version
#'
#' @keywords internal
default_api_version <- function(){
  return("1.0")
}

#' Set current default EES environment
#'
#' @return String containing default EES environment
#'
#' @keywords internal
default_ees_environment <- function(){
  return("test")
}

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

#' Check if a value is an integer
#'
#' is.integer checks the object class, not the value, so credit to VitoshKa
#' on stack overflow for the core of this function...
#'
#' https://stackoverflow.com/questions/3476782/check-if-the-number-is-integer
#'
#' looks like it's been adopted in installr too, avoiding needing that as a
#' dependency by putting the code we need here.
#'
#' @param x a value to test
#'
#' @return logical, false if not an integer, true if an integer
#' @keywords internal
check_integer <- function(x) {
  if (!is.double(x)) {
    # Return early if wrapped in quotes
    return(FALSE)
  } else {
    !grepl("[^[:digit:]]", format(x, digits = 20, scientific = FALSE))
  }
}
