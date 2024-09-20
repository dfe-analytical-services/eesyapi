test_that("Successful connection message", {
  expect_equal(
    http_request_error(response <- list(status = 200)),
    "Successful API request."
  )
})

test_that("Bad URL / query message", {
  expect_error(
    http_request_error(response <- list(status = 404, errors = "Demo error")),
    paste0(
      "HTTP connection error: ",
      404,
      "\n",
      "Invalid query, data set ID, data set version or API version submitted to API.",
      "\n",
      "Demo error"
    )
  )
})

test_that("Server error message", {
  expect_error(
    http_request_error(response <- list(status = 503)),
    paste0(
      "HTTP connection error: ",
      503,
      "\n",
      "Internal server error encountered - please contact the EES API team at ",
      "explore.statistics@education.gov.uk ",
      "providing the query you were attempting to submit."
    )
  )
})
