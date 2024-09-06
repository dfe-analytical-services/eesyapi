test_that("Dataset ID not found", {
  expect_error(
    get_meta("this-is-not-a-dataset"),
    paste0(
      "Query returned error, status 404: ",
      "Invalid query, data set ID, data set version or API version submitted to API."
    )
  )
})

test_that("Dataset ID not found", {
  expect_error(
    get_meta_response("this-is-not-a-dataset"),
    paste0(
      "Query returned error, status 404: ",
      "Invalid query, data set ID, data set version or API version submitted to API."
    )
  )
})

test_that("Non-logical parse flag given", {
  expect_error(
    get_meta_response("this-is-not-a-dataset", parse = 1),
    "You have entered an invalid parse argument, this should be a logical TRUE or FALSE only."
  )
})


# This test uses an existing test data set ID from EES admin. So this test could
# fail if either the code is broken, the data set gets taken down or the dev
# site is down. I guess this isn't totally within the usual principles of
# package testing, but putting in for now partially as a useful check on some
# of the above collectively.
test_that("Meta query runs successfully", {
  expect_equal(
    get_meta_response(example_id(), parse = FALSE)$status,
    200
  )
})
