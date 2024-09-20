test_that("Dataset ID not found", {
  expect_error(
    get_meta(example_id("publication")), # Using the publication ID as this passes the dataset_id
    # validation, but will definitely not be am actual dataset_id
    paste0(
      "HTTP connection error: ",
      404,
      "\n",
      "Invalid query, data set ID, data set version or API version submitted to API."
    )
  )
})

test_that("Non-logical parse flag given", {
  expect_error(
    get_meta_response("this-is-not-a-dataset", parse = 1),
    "You have entered an invalid parse argument, this should be a logical TRUE or FALSE only."
  )
})


# This next test uses an existing test data set ID from EES admin. So this test could
# fail if either the code is broken, the data set gets taken down or the dev
# site is down. I guess this isn't totally within the usual principles of
# package testing, but putting in for now partially as a useful check on some
# of the above collectively.
test_that("Meta query runs successfully", {
  expect_equal(
    get_meta_response(example_id(), parse = FALSE)$status,
    200
  )
})

test_that("Time period parsing works as expected", {
  expect_equal(
    readRDS("testdata/example_meta_unparsed.rds")$timePeriods |>
      parse_meta_time_periods(),
    readRDS("testdata/example_meta_parsed.rds")$time_periods
  )
})

test_that("Location parsing works as expected", {
  expect_equal(
    readRDS("testdata/example_meta_unparsed.rds")$locations |>
      parse_meta_location_ids(),
    readRDS("testdata/example_meta_parsed.rds")$locations
  )
})

test_that("Filter columns parsing works as expected", {
  expect_equal(
    readRDS("testdata/example_meta_unparsed.rds")$filters |>
      parse_meta_filter_columns(),
    readRDS("testdata/example_meta_parsed.rds")$filter_columns
  )
})

test_that("Filter item ids parsing works as expected", {
  expect_equal(
    readRDS("testdata/example_meta_unparsed.rds")$filters |>
      parse_meta_filter_item_ids(),
    readRDS("testdata/example_meta_parsed.rds")$filter_items
  )
})

test_that("Indicator ids parsing works as expected", {
  expect_equal(
    readRDS("testdata/example_meta_unparsed.rds")$indicators |>
      parse_meta_indicator_columns(),
    readRDS("testdata/example_meta_parsed.rds")$indicators
  )
})
