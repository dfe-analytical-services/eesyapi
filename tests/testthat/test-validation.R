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

test_that("Time period validation", {
  expect_no_error(
    validate_time_periods(c("2023|AY", "2023|FY"))
  )
  expect_no_error(
    validate_time_periods("2023|AY")
  )
  expect_error(
    validate_time_periods(c("2023AY", "2023|FY"))
  )
  expect_error(
    validate_time_periods("2023AY")
  )
}
)

test_that("Filter type validation", {
  expect_no_error(
    validate_ees_filter_type("time_periods")
  )
  expect_no_error(
    validate_ees_filter_type("locations")
  )
  expect_no_error(
    validate_ees_filter_type("geographic_levels")
  )
  expect_no_error(
    validate_ees_filter_type("filter_items")
  )
  expect_error(
    validate_ees_filter_type("time_period")
  )
  expect_error(
    validate_ees_filter_type("location")
  )
  expect_error(
    validate_ees_filter_type("geographic")
  )
  expect_error(
    validate_ees_filter_type("filter_item")
  )
}
)
