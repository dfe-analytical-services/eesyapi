test_that("Returns a data frame and has no errors", {
  expect_true(class(download_dataset(example_id("dataset"))) == "data.frame")
  expect_no_error(download_dataset(example_id("dataset")))
})

test_that("Unnecessary or incorrect inputs cause errors", {
  expect_error(download_dataset("ark-of-the-covenent"))
  expect_error(
    download_dataset(example_id("dataset"), verbose = "chatty"),
    "verbose must be a logical value, either TRUE or FALSE"
  )
})
