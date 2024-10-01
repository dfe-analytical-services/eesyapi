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
      dplyr::distinct(),
    data.frame(
      geographic_level = c("NAT", "REG"),
      NAT = c("dP0Zw :: England (code = E92000001)", "dP0Zw :: England (code = E92000001)"),
      REG = c(NA, "rg3Nj :: East Midlands (code = E12000004)")
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
    dplyr::arrange("emJuS :: attendance_type")
  expect_equal(
    query_result,
    readRDS("testdata/example_post_dataset.rds") |>
      dplyr::select(all_of(colnames(query_result)))|>
      dplyr::arrange("emJuS :: attendance_type")
  )
})
