test_that("Successful connection message", {
  expect_equal(http_request_error(200), "Successful API request.")
})

test_that("Bad URL / query message", {
  expect_equal(
    http_request_error(404),
    "Invalid query, data set ID, data set version or API version submitted to API."
    )
})

test_that("Server error message", {
  expect_equal(http_request_error(503), "Internal server error encountered.")
})
