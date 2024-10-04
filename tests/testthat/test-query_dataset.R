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
  expect_error(
    query_dataset(example_id())
  )
})

test_that("Run query from file", {
  query_result <- query_dataset(
    example_id(group = "attendance"),
    json_query = "testdata/test_query.json"
  )
  expect_equal(
    query_result,
    readRDS("testdata/example_json-from-file_dataset.rds") |>
      dplyr::select(all_of(colnames(query_result)))
  )
})

test_that("Run query from string", {
  query_result <- query_dataset(
    example_id(group = "attendance"),
    json_query = example_json_query()
  )
  expect_equal(
    query_result,
    readRDS("testdata/example_json-from-string_dataset.rds") |>
      dplyr::select(all_of(colnames(query_result)))
  )
})

test_that("Geography query returns expected geographies", {
  expect_equal(
    post_dataset(
      example_id(group = "attendance"),
      indicators = example_id("indicator", group = "attendance"),
      time_periods = "2024|W23",
      geographies = c("NAT|id|dP0Zw", "REG|id|rg3Nj"),
      filter_items = c("pmRSo")
    ) |>
      dplyr::select(geographic_level, NAT, REG) |>
      dplyr::distinct() |>
      dplyr::arrange(geographic_level),
    data.frame(
      geographic_level = c("NAT", "REG"),
      NAT = c("dP0Zw", "dP0Zw"),
      REG = c(NA, "rg3Nj")
    )
  )
})

test_that("Test filter-combinations POST dataset query", {
  query_result <- query_dataset(
    example_id(group = "attendance"),
    indicators = example_id("indicator", group = "attendance"),
    time_periods = "2024|W23",
    geographies = c("NAT|id|dP0Zw", "REG|id|rg3Nj"),
    filter_items = list(
      attendance_status = c("pmRSo", "7SdXo"),
      attendance_type = c("CvuId", "6AXrf", "0k3T5", "YdkHK"),
      education_phase = c("ThDPJ", "crH31"),
      day_number = c("uLQo4"),
      reason = c("bBrtT")
    )
  ) |>
    dplyr::arrange("emJuS", "bqZtT")
  expect_equal(
    query_result,
    readRDS("testdata/example_post_dataset.rds") |>
      dplyr::select(all_of(colnames(query_result))) |>
      dplyr::arrange("emJuS", "bqZtT")
  )
})
