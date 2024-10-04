#' Query a data set
#'
#' @description
#' Create and send a query to the EES API. Queries can be constructed by including the codes to the
#' relevant flags to filter on time period, geographies, and data set specific filters. If none of
#' the above are set in the function call, then the all rows will be retrieved. The data set id and
#' specific indicators of interest must be supplied explicitly using the dataset_id and indicators
#' params.
#'
#' You can request a specific set of rows using the page and page_size parameters. Keeping the
#' default of page = NULL will return all rows matching the query. Setting page and page_size to
#' numerical values will attempt to return a subset of rows, with page_size defining the number of
#' rows and page defining which subset of rows to return from the query (i.e. page = 1, page_size =
#'  20 will return the first 20 rows, page = 2 and page_size = 20 will return the second 20 rows
#'  and so on).
#'
#' @inheritParams api_url
#' @inheritParams post_dataset
#' @param method The API query method to be used. Can be  "POST" or "GET". Default: "POST".
#'
#' @return Data frame containing query results
#' @export
#'
#' @examples
#' # Run query_dataset() using a json query string input to json_query (this can also be done by
#' # passing a filename of a file containing your json query string).
#' query_dataset(
#'   example_id(group = "attendance"),
#'   json_query = example_json_query()
#' )
#'
#' # If you don't want to have tio write your own json query, the rest of the examples illustrate
#' # how to use query_dataset() with parameters to construct queries in R.
#'
#' # Run query_dataset() to select rows containing either of two geographic locations and either of
#' # two filter items.
#' query_dataset(
#'   example_id(group = "attendance"),
#'   indicators = example_id("indicator", group = "attendance"),
#'   time_periods = "2024|W23",
#'   geographies = c("NAT|id|dP0Zw", "REG|id|rg3Nj"),
#'   filter_items = c("CvuId", "6AXrf"),
#'   page = 1,
#'   page_size = 32
#' )
#'
#' # Run query_dataset() using set parameters giving a combination of filter options
#' query_dataset(
#'   example_id(group = "attendance"),
#'   indicators = example_id("indicator", group = "attendance"),
#'   time_periods = "2024|W23",
#'   geographies = c("NAT"),
#'   filter_items = list(
#'     attendance_status = c("pmRSo", "7SdXo"),
#'     attendance_type = c("CvuId", "6AXrf", "0k3T5", "YdkHK"),
#'     education_phase = c("ThDPJ", "crH31"),
#'     day_number = c("uLQo4"),
#'     reason = c("bBrtT")
#'   )
#' )
#'
#' # Run a query with a more complex geography selection. Return data for all of:
#' #   - England
#' #   - Yorkshire and the Humber
#' #   - All LAs in Yorkshire and the Humber
#' example_geography_query("nat_yorks_yorkslas")
#' query_dataset(
#'   example_id(group = "attendance"),
#'   indicators = example_id("indicator", group = "attendance"),
#'   time_periods = "2024|W23",
#'   geographies = example_geography_query("nat_yorks_yorkslas"),
#'   filter_items = list(
#'     attendance_status = c("pmRSo"),
#'     attendance_type = c("CvuId"),
#'     education_phase = c("ThDPJ"),
#'     day_number = c("uLQo4"),
#'     reason = c("bBrtT")
#'   )
#' )
#'
#' # Run a basic query using GET instead of POST
#' query_dataset(
#'   example_id(),
#'   method = "GET",
#'   geographic_levels = "NAT",
#'   filter_items = example_id("filter_item"),
#'   indicators = example_id("indicator"),
#'   page = 1,
#'   page_size = 10
#' )
#'
query_dataset <- function(
    dataset_id,
    indicators = NULL,
    time_periods = NULL,
    geographies = NULL,
    geographic_levels = NULL,
    locations = NULL,
    filter_items = NULL,
    json_query = NULL,
    method = "POST",
    dataset_version = NULL,
    api_version = NULL,
    page_size = 1000,
    page = NULL,
    debug = FALSE,
    verbose = FALSE) {
  if (!(method %in% c("POST", "GET"))) {
    stop(
      paste(
        "Invalid method selected. The keyword method should be set to GET",
        "(an option to use POST is being developed)."
      )
    )
  }
  if (method == "POST") {
    eesyapi::post_dataset(
      dataset_id = dataset_id,
      indicators = indicators,
      time_periods = time_periods,
      geographies = geographies,
      filter_items = filter_items,
      json_query = json_query,
      dataset_version = dataset_version,
      api_version = api_version,
      page_size = page_size,
      page = page,
      debug = debug,
      verbose = verbose
    )
  } else {
    warning(
      paste(
        "Using GET to query a data set offers limited functionality, we recommend",
        "using POST alongside a JSON structured query instead:\n",
        "  - query_dataset(..., method = 'POST')"
      )
    )
    eesyapi::get_dataset(
      dataset_id = dataset_id,
      indicators = indicators,
      time_periods = time_periods,
      geographic_levels = geographic_levels,
      locations = locations,
      filter_items = filter_items,
      dataset_version = dataset_version,
      api_version = api_version,
      page_size = page_size,
      page = page,
      verbose = verbose
    )
  }
}
