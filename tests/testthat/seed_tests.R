# This script contains a set of functions to update test data from the current state of the API
# (i.e. when new publications / datasets get added).
#
# Refreshing test data
# ====================
# To refresh all test data, run seed_tests() or to refresh individual test data sets, find the
# relevant see function and run that.
#
# Adding extra seeds
# ==================
# - Add an individual function for an individual test data file.
# - Add that function as a call in seed_tests() to make sure it gets refreshed as part of any
# bulk refresh
# - test data should be saved as rds files in tests/testthat/testdata/


# Refresh all test data
seed_tests <- function() {
  message("Updating publication list")
  seed_get_publications()
  message("Updating data catalogue list")
  seed_get_data_catalogue()
  seed_post_dataset()
  message("Updating example meta data")
  seed_get_meta()
}

# Refresh the publication list
seed_get_publications <- function() {
  saveRDS(
    eesyapi::get_publications(),
    file = "tests/testthat/testdata/example_publication_catalogue.rds"
  )
}

# Refresh the data sets list from the standard example publication
seed_get_data_catalogue <- function() {
  saveRDS(
    eesyapi::get_data_catalogue(eesyapi::example_id("publication")),
    file = "tests/testthat/testdata/example_publication_datasets.rds"
  )
}

seed_post_dataset <- function() {
  query_dataset(
    example_id(group = "attendance"),
    indicators = example_id("indicator", group = "attendance"),
    time_periods = "2024|W23",
    geographies = c("NAT|id|dP0Zw", "REG|id|rg3Nj"),
    filter_items = list(
      attendance_status = c("pmRSo", "7SdXo"),
      attendance_type = c("CvuId", "6AXrf", "0k3T5", "YdkHK"),
      education_phase = c("ThDPJ", "crH31"),
      day_number = c("uLQo4"),
      reason = c("bBrtT")
    )
  ) |>
    saveRDS(
      file = "tests/testthat/testdata/example_post_dataset.rds"
    )
}

# Refresh the data sets list from the standard example publication
seed_get_meta <- function() {
  saveRDS(
    eesyapi::get_meta_response(eesyapi::example_id()),
    file = "tests/testthat/testdata/example_meta_unparsed.rds"
  )
  saveRDS(
    eesyapi::get_meta(eesyapi::example_id()),
    file = "tests/testthat/testdata/example_meta_parsed.rds"
  )
}
