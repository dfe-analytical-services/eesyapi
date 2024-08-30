test_that("eesapi_url", {
  expect_equal(
    eesapi_url(),
    "https://dev.statistics.api.education.gov.uk/api/v1.0/data-sets/"
  )
  expect_error(
    eesapi_url("1.x")
  )
})
