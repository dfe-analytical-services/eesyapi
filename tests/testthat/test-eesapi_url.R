test_that("eesapi_url", {
  expect_equal(
    eesapi_url(),
    "https://dev.statistics.api.education.gov.uk/api/v1.0/data-sets/"
  )
  expect_error(
    eesapi_url(api_version = "1.x")
  )
  expect_error(
    eesapi_url(endpoint = 'query', dataset_id = "dummy:dataset:id")
  )
  expect_error(
    eesapi_url(endpoint = 'query-data')
  )
  expect_equal(
    eesapi_url(endpoint = 'query-data', dataset_id = "dummy:dataset:id"),
    "https://dev.statistics.api.education.gov.uk/api/v1.0/data-sets/dummy:dataset:id/query"
  )
  expect_error(
    eesapi_url(
      endpoint = 'query-data',
      dataset_id = "dummy:dataset:id",
      dataset_version = "x"
      )
  )
  expect_equal(
    eesapi_url(
      endpoint = 'query-data',
      dataset_id = "dummy:dataset:id",
      dataset_version = 2.1
      ),
    "https://dev.statistics.api.education.gov.uk/api/v1.0/data-sets/dummy:dataset:id/query?data-version=2.1"
  )

})
