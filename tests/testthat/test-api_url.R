test_that("api_url", {
  expect_equal(
    api_url(),
    "https://dev.statistics.api.education.gov.uk/api/v1.0/publications?"
  )
  expect_error(
    api_url(api_version = "1.x")
  )
  expect_error(
    api_url(endpoint = "query", dataset_id = "dummy:dataset:id")
  )
  expect_error(
    api_url(endpoint = "query-data")
  )
  expect_equal(
    api_url(endpoint = "query-data", dataset_id = "dummy:dataset:id"),
    "https://dev.statistics.api.education.gov.uk/api/v1.0/data-sets/dummy:dataset:id/query"
  )
  expect_error(
    api_url(
      endpoint = "query-data",
      dataset_id = "dummy:dataset:id",
      dataset_version = "x"
    )
  )
  expect_equal(
    api_url(
      endpoint = "query-data",
      dataset_id = "dummy:dataset:id",
      dataset_version = 2.1
    ),
    paste0(
      "https://dev.statistics.api.education.gov.uk/api/v1.0/data-sets/",
      "dummy:dataset:id/query?dataSetVersion=2.1"
    )
  )
})
