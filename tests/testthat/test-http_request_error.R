test_that("Successful connection message", {
  expect_equal(
    http_request_error(list(status = 200)),
    "Successful API request."
  )
})

test_that("Bad URL / query message", {
  expect_error(
    api_url("get-meta", dataset_id = example_id("publication")) |>
      httr::GET() |>
      http_request_error(),
    paste0(
      "HTTP connection error: ",
      404,
      "\n",
      "Invalid query, data set ID, data set version or API version submitted to API."
    )
  )
})
