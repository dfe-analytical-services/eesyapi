test_that("Returns a data frame and has no errors", {
  expect_true(
    class(
      download_dataset(
        example_id("dataset", group = "public-api-testing")
      )
    ) == "data.frame"
  )

  expect_no_error(
    download_dataset(
      example_id("dataset", group = "public-api-testing")
    )
  )
})
