test_that("Invalid method selected", {
  expect_error(
    query_dataset(example_id(), indicators = example_id("indicator"), method = "QUERY")
  )
})

test_that("Invalid dataset_id", {
  expect_error(
    query_dataset("kjdhf873kdjf", indicators = example_id("indicator"))
  )
})

test_that("No indicator supplied", {
  expect_error(
    query_dataset(example_id())
  )
})
