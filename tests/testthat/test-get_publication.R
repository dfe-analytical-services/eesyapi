# Check the get_publication_catalogue() function returns the expected data
# WARNING: This depends on live data, so may fail due to real-life changes.
#          If that's the case, take a new snapshot by running seed_tests()
test_that("Filter columns parsing works as expected", {
  expect_equal(
    get_publication_catalogue(),
    readRDS("testdata/example_publication_catalogue.rds")
  )
})

# Check the get_publication_datasets() function returns the expected data
# WARNING: This depends on live data, so may fail due to real-life changes.
#          If that's the case, take a new snapshot by running seed_tests()
test_that("Filter columns parsing works as expected", {
  expect_equal(
    get_publication_datasets(example_id("publication")),
    readRDS("testdata/example_publication_datasets.rds")
  )
})
