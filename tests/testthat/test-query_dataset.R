test_that("Invalid method selected", {
  expect_error(
    query_dataset(example_id(), indicators = example_id("indicator"), method = "QUERY")
  )
})

test_that("Invalid dataset_id", {
  expect_error(
    query_dataset("kjdhf873kdjf", indicators = example_id("indicator"))
  )
})

test_that("No indicator supplied", {
  expect_warning(
    query_dataset(example_id()),
    "No indicators provided, defaulted to using all indicators from meta data"
  )
})

test_that("Run query from file", {
  query_result <- query_dataset(
    example_id(group = "attendance"),
    json_query = "testdata/test_query.json"
  )
  query_result <- query_result |>
    dplyr::arrange(dplyr::across(colnames(query_result)))
  expect_equal(
    query_result,
    readRDS("testdata/example_json-from-file_dataset.rds") |>
      dplyr::select(dplyr::all_of(colnames(query_result))) |>
      dplyr::arrange(dplyr::across(colnames(query_result)))
  )
})

test_that("Run query from string", {
  query_result <- query_dataset(
    example_id(group = "attendance"),
    json_query = example_json_query()
  )
  query_result <- query_result |>
    dplyr::arrange(dplyr::across(colnames(query_result)))
  expect_equal(
    query_result,
    readRDS("testdata/example_json-from-string_dataset.rds") |>
      dplyr::select(dplyr::all_of(colnames(query_result))) |>
      dplyr::arrange(dplyr::across(colnames(query_result)))
  )
})

test_that("Time period query returns expected time periods", {
  expect_equal(
    post_dataset(
      example_id(group = "attendance"),
      indicators = example_id("indicator", group = "attendance"),
      time_periods = eesyapi::example_id("time_periods", group = "attendance"),
      geographies = eesyapi::example_id("location_ids", group = "attendance"),
      filter_items = eesyapi::example_id("filter_item", group = "attendance")
    ) |>
      dplyr::select("code", "period") |>
      dplyr::distinct() |>
      dplyr::arrange(code, period),
    data.frame(
      code = c("W21", "W23"),
      period = c("2024", "2024")
    )
  )
})

test_that("Time period query errors on badly formatted time period", {
  expect_error(
    post_dataset(
      example_id(group = "attendance"),
      indicators = example_id("indicator", group = "attendance"),
      time_periods = c("2024W21", "2024|W23"),
      geographies = eesyapi::example_id("location_ids", group = "attendance"),
      filter_items = eesyapi::example_id("filter_item", group = "attendance")
    )
  )
})


test_that("Geography query returns expected geographies", {
  expect_equal(
    post_dataset(
      example_id(group = "attendance"),
      indicators = example_id("indicator", group = "attendance"),
      time_periods = eesyapi::example_id("time_period", group = "attendance"),
      geographies = eesyapi::example_id("location_ids", group = "attendance"),
      filter_items = eesyapi::example_id("filter_item", group = "attendance")
    ) |>
      dplyr::select("geographic_level", "nat_code", "reg_code") |>
      dplyr::distinct() |>
      dplyr::arrange(geographic_level),
    data.frame(
      geographic_level = c("NAT", "REG"),
      nat_code = rep("E92000001", 2),
      reg_code = c(NA, "E12000004")
    )
  )
})

test_that("Test filter-combinations POST dataset query", {
  query_result <- query_dataset(
    example_id(group = "attendance"),
    indicators = example_id("indicator", group = "attendance"),
    time_periods = eesyapi::example_id("time_period", group = "attendance"),
    geographies = eesyapi::example_id("location_ids", group = "attendance"),
    filter_items = eesyapi::example_id("filter_items_long", group = "attendance")
  )
  query_result <- query_result |>
    dplyr::arrange(dplyr::across(colnames(query_result)))
  expect_equal(
    query_result,
    readRDS("testdata/example_post_dataset.rds") |>
      dplyr::select(dplyr::all_of(colnames(query_result))) |>
      dplyr::arrange(dplyr::across(colnames(query_result)))
  )
  query_result <- query_dataset(
    example_id(group = "attendance"),
    indicators = example_id("indicator", group = "attendance"),
    time_periods = eesyapi::example_id("time_period", group = "attendance"),
    geographies = eesyapi::example_id("location_ids", group = "attendance"),
    filter_items = eesyapi::example_id("filter_items_short", group = "attendance")
  ) |>
    dplyr::select(
      "attendance_status",
      "attendance_type",
      "day_number",
      "establishment_phase",
      "reason"
    ) |>
    dplyr::distinct()
  expect_equal(
    query_result,
    data.frame(
      attendance_status = rep("Attendance", 4),
      attendance_type = rep(c("Present", "Approved educational activity"), 2),
      day_number = rep("Total", 4),
      establishment_phase = c(rep("Secondary", 2), rep("Special", 2)),
      reason = rep("Total", 4)
    )
  )
})

test_that("Indicators not found in data set", {
  expect_error(
    query_dataset(example_id(), indicators = c("uywet", "uywed")),
    "\nHTTP connection error: 400\nOne or more indicators could not be found.\n     uywet, uywed"
  )
})
