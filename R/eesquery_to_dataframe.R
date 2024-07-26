#' EES query output to data.frame
#'
#' @param api_output The results of an EES API data query
#' @param clear_codes Flag as to whether to clear the API code tags from the
#' output
#'
#' @return data.frame containing the API query results
#' @export
#'
#' @examples
eesquery_to_dataframe <- function(api_output, clear_codes = TRUE) {
  results <- api_output$results
  dfnames <- c(
    "time_period", "time_identifier", "geographic_level",
    names(results[[1]]$locations),
    names(results[[1]]$filters),
    names(results[[1]]$values)
  )
  df <- data.frame(
    matrix(vector(), 0, length(dfnames),
           dimnames = list(c(), dfnames)
    ),
    stringsAsFactors = F
  )
  if (clear_codes) {
    code_string <- ".*: "
  } else {
    code_string <- "code clearing off"
  }
  for (i in 1:length(results)) {
    df[i, "time_period"] <- results[[i]]$timePeriod$period
    df[i, "time_identifier"] <- results[[i]]$timePeriod$code
    df[i, "geographic_level"] <- results[[i]]$geographicLevel
    for (j in 1:length(results[[i]]$locations)) {
      loc_name <- names(results[[i]]$locations[j])
      df[i, loc_name] <- gsub(code_string, "", results[[i]]$locations[[loc_name]])
    }
    for (j in 1:length(results[[i]]$filters)) {
      filter_name <- names(results[[i]]$filters[j])
      df[i, filter_name] <- gsub(code_string, "", results[[i]]$filters[[filter_name]])
    }
    for (j in 1:length(results[[i]]$values)) {
      value_name <- names(results[[i]]$values[j])
      df[i, value_name] <- gsub(code_string, "", results[[i]]$values[[value_name]])
    }
  }
  df
}
