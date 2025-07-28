test_that("decompose_dtc works comprehensively", {
  # Test data with various date-time formats
  test_data <- tibble::tribble(
    ~ID,             ~STDTC,             ~ENDTC,             ~EXDTC,
    1, "2024-12-05T08:12",       "2024-12-05", "2024-12-05T14:30",
    2,       "2024-12-06", "2024-12-06T00:00", "2024-12-06T09:15",
    3, "2024-12-07T00:00",       "2024-12-07", "2024-12-07T16:45",
    4, "2024-12-08T12:00", "2024-12-08T12:00",       "2024-12-08"
  ) %>%
    lubrify_dates()

  # Test single field decomposition
  result_single <- decompose_dtc(test_data, "STDTC")

  # Check that new columns are created
  expect_true("STDTC_date" %in% names(result_single))
  expect_true("STDTC_time" %in% names(result_single))
  expect_false("has_time" %in% names(result_single))  # Should be removed

  # Check date extraction
  expect_equal(result_single$STDTC_date,
               c("2024-12-05", "2024-12-06", "2024-12-07", "2024-12-08"))

  # Check time extraction (midnight times are considered as no time by has_time function)
  expect_equal(result_single$STDTC_time,
               c("08:12", NA, NA, "12:00"))

  # Test multiple field decomposition
  result_multiple <- decompose_dtc(test_data, c("STDTC", "ENDTC"))

  # Check that all expected columns are created
  expect_true(all(c("STDTC_date", "STDTC_time", "ENDTC_date", "ENDTC_time") %in%
                  names(result_multiple)))

  # Check ENDTC decomposition
  expect_equal(result_multiple$ENDTC_date,
               c("2024-12-05", "2024-12-06", "2024-12-07", "2024-12-08"))
  expect_equal(result_multiple$ENDTC_time,
               c(NA, NA, NA, "12:00"))

  # Test with all three fields
  result_three <- decompose_dtc(test_data, c("STDTC", "ENDTC", "EXDTC"))

  # Check EXDTC decomposition
  expect_equal(result_three$EXDTC_date,
               c("2024-12-05", "2024-12-06", "2024-12-07", "2024-12-08"))
  expect_equal(result_three$EXDTC_time,
               c("14:30", "09:15", "16:45", NA))
})


test_that("decompose_dtc handles edge cases", {
  # Test with empty data frame
  empty_df <- data.frame(STDTC = lubridate::as_datetime(character(0)))
  result_empty <- decompose_dtc(empty_df, "STDTC")
  expect_equal(nrow(result_empty), 0)
  expect_true(all(c("STDTC_date", "STDTC_time") %in% names(result_empty)))

  # Test with NA values
  na_df <- tribble(
    ~ID, ~STDTC,
    1, as.POSIXct(NA),
    2, lubridate::as_datetime("2024-12-05T08:12", format = dtc_formats),
    3, as.POSIXct(NA)
  )

  result_na <- decompose_dtc(na_df, "STDTC")
  expect_equal(result_na$STDTC_date, c(NA, "2024-12-05", NA))
  expect_equal(result_na$STDTC_time, c(NA, "08:12", NA))

  # Test with single row
  single_row <- tribble(
    ~ID, ~STDTC,
    1, lubridate::as_datetime("2024-12-05T08:12", format = dtc_formats)
  )

  result_single <- decompose_dtc(single_row, "STDTC")
  expect_equal(nrow(result_single), 1)
  expect_equal(result_single$STDTC_date, "2024-12-05")
  expect_equal(result_single$STDTC_time, "08:12")
})


test_that("decompose_dtc preserves original data", {
  test_data <- tribble(
    ~ID, ~STDTC, ~OTHER_COL,
    1, "2024-12-05T08:12", "value1",
    2, "2024-12-06", "value2",
    3, "2024-12-07T00:00", "value3"
  ) %>%
    mutate(STDTC = lubridate::as_datetime(STDTC, format = dtc_formats))

  result <- decompose_dtc(test_data, "STDTC")

  # Check that original columns are preserved
  expect_true("ID" %in% names(result))
  expect_true("STDTC" %in% names(result))
  expect_true("OTHER_COL" %in% names(result))

  # Check that original data is unchanged
  expect_equal(result$ID, test_data$ID)
  expect_equal(result$STDTC, test_data$STDTC)
  expect_equal(result$OTHER_COL, test_data$OTHER_COL)
})


test_that("decompose_dtc handles different time formats correctly", {
  test_data <- tribble(
    ~ID, ~STDTC,
    1, "2024-12-05T08:12",      # Has time
    2, "2024-12-06",             # Date only
    3, "2024-12-07T00:00",       # Midnight (considered no time)
    4, "2024-12-08T12:30",       # Has time
    5, "2024-12-09T23:59"        # Late time
  ) %>%
    mutate(STDTC = lubridate::as_datetime(STDTC, format = dtc_formats))

  result <- decompose_dtc(test_data, "STDTC")

  # Check time detection (midnight is considered as no time)
  expect_equal(result$STDTC_time,
               c("08:12", NA, NA, "12:30", "23:59"))

  # Check date extraction
  expect_equal(result$STDTC_date,
               c("2024-12-05", "2024-12-06", "2024-12-07", "2024-12-08", "2024-12-09"))
})


test_that("decompose_dtc validates input parameters", {
  test_data <- tribble(
    ~ID, ~STDTC,
      1, "2024-12-05T08:12"
  ) %>%
    mutate(STDTC = lubridate::as_datetime(STDTC, format = dtc_formats))

  # Test invalid obj parameter
  expect_error(decompose_dtc(NULL, "STDTC"), "obj must be a data frame!")
  expect_error(decompose_dtc(list(), "STDTC"), "obj must be a data frame!")
  expect_error(decompose_dtc("not a df", "STDTC"), "obj must be a data frame!")

  # Test invalid DTC_field parameter
  expect_error(decompose_dtc(test_data, NULL), "DTC_field must not be NULL")
  expect_error(decompose_dtc(test_data, 123), "DTC_field must be a string value")
  expect_error(
    decompose_dtc(test_data, c("STDTC", 123)),
    "Column not found in obj: 123")

  # Test missing field
  expect_error(
    decompose_dtc(test_data, "NONEXISTENT"),
    "Column not found in obj: NONEXISTENT")
})


