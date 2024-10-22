test_that("Returns a data frame and has no errors", {
  expect_true(class(preview_dataset(example_id("dataset"))) == "data.frame")
  expect_no_error(preview_dataset(example_id("dataset")))
})

test_that("Incorrect inputs cause errors", {
  expect_error(preview_dataset("ark-of-the-covenent"))

  expect_error(
    preview_dataset(example_id("dataset"), verbose = "chatty"),
    "verbose must be a logical value, either TRUE or FALSE"
  )

  expect_error(
    preview_dataset(example_id("dataset"), n_max = 20.2),
    "n_max must be a positive integer value, e.g. 15, or Inf"
  )

  expect_error(
    preview_dataset(example_id("dataset"), n_max = "20"),
    "n_max must be a positive integer value, e.g. 15, or Inf"
  )

  expect_error(
    preview_dataset(example_id("dataset"), n_max = -2),
    "n_max must be a positive integer value, e.g. 15, or Inf"
  )

  expect_error(
    preview_dataset(example_id("dataset"), n_max = "fifty"),
    "n_max must be a positive integer value, e.g. 15, or Inf"
  )

  expect_error(
    preview_dataset(example_id("dataset"), n_max = -Inf),
    "n_max must be a positive integer value, e.g. 15, or Inf"
  )
})

test_that("only previews a specified number of rows", {
  expect_equal(
    preview_dataset(example_id("dataset")) |> nrow(),
    10
  )

  expect_equal(
    preview_dataset(example_id("dataset"), n_max = 0) |> nrow(),
    0
  )

  expect_equal(
    preview_dataset(example_id("dataset"), n_max = 42) |> nrow(),
    42
  )
})

test_that("returns all rows", {
  # In truth, the test doesn't check all rows as we don't reliably know that
  # number, so just that it returns more than an arbitrary number above the
  # default
  expect_gt(
    preview_dataset(example_id("dataset"), n_max = Inf) |>
      nrow(),
    200
  )
})
