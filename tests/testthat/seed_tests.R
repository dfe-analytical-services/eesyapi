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
  seed_get_publications()
  seed_get_datasets()
}

# Refresh the publication list
seed_get_publications <- function() {
  saveRDS(
    eesyapi::get_publications(),
    file = "tests/testthat/testdata/example_publication_catalogue.rds"
  )
}

# Refresh the data sets list from the standard example publication
seed_get_datasets <- function() {
  saveRDS(
    eesyapi::get_datasets(eesyapi::example_id("publication")),
    file = "tests/testthat/testdata/example_publication_datasets.rds"
  )
}
