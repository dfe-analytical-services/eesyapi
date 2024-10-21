test_that("api_url", {
  expect_equal(
    api_url(),
    "https://test.statistics.api.education.gov.uk/api/v1.0/publications?"
  )
  expect_error(
    api_url(api_version = "1.x")
  )
  expect_error(
    api_url(endpoint = "query", dataset_id = example_id())
  )
  expect_error(
    api_url(endpoint = "post-data")
  )
  expect_equal(
    api_url(endpoint = "post-data", dataset_id = example_id()),
    paste0(
      "https://test.statistics.api.education.gov.uk/api/v1.0/data-sets/",
      example_id(),
      "/query"
    )
  )
  expect_error(
    api_url(
      endpoint = "post-data",
      dataset_id = example_id(),
      dataset_version = "x"
    )
  )
  expect_equal(
    api_url(
      endpoint = "post-data",
      dataset_id = example_id(),
      dataset_version = 2.1
    ),
    paste0(
      "https://test.statistics.api.education.gov.uk/api/v1.0/data-sets/",
      example_id(),
      "/query?dataSetVersion=2.1"
    )
  )
  expect_error(
    api_url(
      environment = "invalid-endpoint"
    ),
    paste(
      "You have entered invalid EES environment. The environment should be one of:\n",
      "   - dev, test, preprod or prod"
    )
  )

  expect_error(
    api_url("get-csv"),
    "The variable dataset_id is NULL, please provide a valid dataset_id."
  )

  expect_warning(
    api_url("get-csv", dataset_id = example_id("dataset"), indicators = "qwerty")
  )

  expect_equal(
    api_url("get-csv", dataset_id = example_id("dataset")),
    paste0(
      "https://test.statistics.api.education.gov.uk/api/v1.0/data-sets/",
      example_id("dataset"),
      "/csv"
    )
  )
})
