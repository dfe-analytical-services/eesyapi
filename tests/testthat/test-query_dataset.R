test_that("Invalid method selected", {
  expect_error(
    query_dataset(example_id(), indicators = example_id("indicator"), method = "QUERY"),
    paste0(
      "Invalid method selected. The keyword method should be set to GET (an option ",
      "to use POST is being developed.)"
    )
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
