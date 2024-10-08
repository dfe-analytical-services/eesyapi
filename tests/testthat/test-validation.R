test_that("Location validation works", {
  expect_no_error(
    validate_ees_id("NAT|id|23897", level = "location")
  )
  expect_no_error(
    validate_ees_id(c("NAT|id|23897", "REG|code|sd897asdf"), level = "location")
  )
  expect_error(
    validate_ees_id("NATid23897", level = "location")
  )
  expect_error(
    validate_ees_id(c("NATid|23897", "REG|code|sd897"), level = "location")
  )
})
