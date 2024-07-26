#' EES meta to filter table
#'
#' @param api_meta
#'
#' @return Data frame containing the data set filters
#' @export
#'
#' @examples
eesmeta_to_filtertable <- function(api_meta) {
  output <- api_meta %>%
    content("text") %>%
    jsonlite::fromJSON()
  nfilters <- length(output$filters$id)
  filter_table <- data.frame(
    row = 1:nfilters,
    id = output$filters$id,
    label = output$filters$label
  )
  filter_item_lookup <- data.frame(row = NA, item_id = NA, item_label = NA)
  for (i in 1:nfilters) {
    filter_item_lookup <- filter_item_lookup %>%
      rbind(
        output$filters$options[[i]] %>%
          select(item_id = id, item_label = label) %>%
          mutate(row = i)
      )
  }
  filter_table %>%
    left_join(filter_item_lookup)
}
