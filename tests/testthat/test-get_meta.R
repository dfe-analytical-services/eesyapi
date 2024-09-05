test_that("Dataset ID not found", {
  expect_error(get_meta("this-is-not-a-dataset"))
})

test_that("Non-logical parse flag given", {
  expect_error(get_meta("this-is-not-a-dataset", parse = 1))
})


# This test uses an existing test data set ID from EES admin. So this test could
# fail if either the code is broken, the data set gets taken down or the dev
# site is down. I guess this isn't totally within the usual principles of
# package testing, but putting in for now partially as a useful check on some
# of the above collectively.
test_that("Meta query runs successfully", {
  expect_equal(
    get_meta("d7329101-f275-d277-bbfe-d8cfaa709833", parse = FALSE)$status,
    200
  )
})
