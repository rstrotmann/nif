test_that("lubrify_dates converts DTC columns to POSIXct when col is NULL", {
  # Test data with various DTC formats
  test_data <- tibble::tribble(
    ~ID, ~STDTC, ~ENDTC, ~OTHER_COL,
    1, "2024-12-05T08:12", "2024-12-05", "not_dtc",
    2, "2024-12-05", "2024-12", "not_dtc",
    3, "2024-12", "2024-12-05T08:12:30", "not_dtc",
    4, "2024-12-05T08:12:30", "2024", "not_dtc",
    5, "2024", "2024-12-05 08:12", "not_dtc",
    6, "2024-12-05 08:12", "2024-12-05 08:12:30", "not_dtc",
    7, "2024-12-05 08:12:30", "2024-12-05T08:12", "not_dtc"
  )

  result <- lubrify_dates(test_data)

  # Check that DTC columns are converted to POSIXct
  expect_s3_class(result$STDTC, "POSIXct")
  expect_s3_class(result$ENDTC, "POSIXct")

  # Check that non-DTC columns remain unchanged
  expect_type(result$OTHER_COL, "character")
  expect_type(result$ID, "double")

  # Check that the conversion worked correctly
  expect_equal(as.character(result$STDTC[1]), "2024-12-05 08:12:00")
  expect_equal(as.character(result$ENDTC[1]), "2024-12-05")
})

test_that("lubrify_dates converts specified columns when col parameter is provided", {
  test_data <- tibble::tribble(
    ~ID, ~CUSTOM_DATE, ~ANOTHER_DATE, ~NOT_DATE,
    1, "2024-12-05T08:12", "2024-12-05", "text",
    2, "2024-12-05", "2024-12", "text"
  )

  result <- lubrify_dates(test_data, col = c("CUSTOM_DATE", "ANOTHER_DATE"))

  # Check that specified columns are converted to POSIXct
  expect_s3_class(result$CUSTOM_DATE, "POSIXct")
  expect_s3_class(result$ANOTHER_DATE, "POSIXct")

  # Check that non-specified columns remain unchanged
  expect_type(result$NOT_DATE, "character")
  expect_type(result$ID, "double")
})

test_that("lubrify_dates handles empty data frame", {
  empty_df <- data.frame(STDTC = character(), ENDTC = character())

  result <- lubrify_dates(empty_df)

  expect_s3_class(result$STDTC, "POSIXct")
  expect_s3_class(result$ENDTC, "POSIXct")
  expect_equal(nrow(result), 0)
})

test_that("lubrify_dates handles data frame with no DTC columns", {
  test_data <- tibble::tribble(
    ~ID, ~NAME, ~VALUE,
    1, "John", 100,
    2, "Jane", 200
  )

  result <- lubrify_dates(test_data)

  # Should return the same data frame unchanged
  expect_equal(result, test_data)
  expect_type(result$ID, "double")
  expect_type(result$NAME, "character")
  expect_type(result$VALUE, "double")
})

test_that("lubrify_dates handles NA values in DTC columns", {
  test_data <- tibble::tribble(
    ~ID, ~STDTC, ~ENDTC,
    1, "2024-12-05T08:12", NA,
    2, NA, "2024-12-05",
    3, "2024-12-05", "2024-12-05T08:12"
  )

  result <- lubrify_dates(test_data)

  expect_s3_class(result$STDTC, "POSIXct")
  expect_s3_class(result$ENDTC, "POSIXct")

  # Check that NA values are preserved
  expect_true(is.na(result$ENDTC[1]))
  expect_true(is.na(result$STDTC[2]))
})

test_that("lubrify_dates handles all supported DTC formats", {
  test_data <- tibble::tribble(
    ~ID, ~STDTC, ~ENDTC, ~TIMEDTC, ~DATEDTC,
    1, "2024-12-05T08:12", "2024-12-05", "2024-12", "2024",
    2, "2024-12-05T08:12:30", "2024-12-05 08:12", "2024-12-05T08:12:30", "2024-12-05 08:12:30"
  )

  result <- lubrify_dates(test_data)

  # All DTC columns should be converted
  expect_s3_class(result$STDTC, "POSIXct")
  expect_s3_class(result$ENDTC, "POSIXct")
  expect_s3_class(result$TIMEDTC, "POSIXct")
  expect_s3_class(result$DATEDTC, "POSIXct")
})

test_that("lubrify_dates throws error for non-data frame input", {
  expect_error(lubrify_dates("not a data frame"), "obj must be a data frame!")
  expect_error(lubrify_dates(123), "obj must be a data frame!")
  expect_error(lubrify_dates(list()), "obj must be a data frame!")
  expect_error(lubrify_dates(NULL), "obj must be a data frame!")
})

test_that("lubrify_dates throws error for missing columns when col is specified", {
  test_data <- tibble::tribble(
    ~ID, ~STDTC, ~ENDTC,
    1, "2024-12-05T08:12", "2024-12-05"
  )

  expect_error(
    lubrify_dates(test_data, col = c("STDTC", "MISSING_COL")),
    "Column not found in data frame: MISSING_COL"
  )

  expect_error(
    lubrify_dates(test_data, col = c("MISSING_COL1", "MISSING_COL2")),
    "Columns not found in data frame: MISSING_COL1 and MISSING_COL2"
  )
})

test_that("lubrify_dates handles single column specification", {
  test_data <- tibble::tribble(
    ~ID, ~STDTC, ~ENDTC,
    1, "2024-12-05T08:12", "2024-12-05"
  )

  result <- lubrify_dates(test_data, col = "STDTC")

  expect_s3_class(result$STDTC, "POSIXct")
  expect_type(result$ENDTC, "character") # Should remain unchanged
})

test_that("lubrify_dates handles mixed column types correctly", {
  test_data <- tibble::tribble(
    ~ID, ~STDTC, ~ENDTC, ~NUMERIC_COL, ~LOGICAL_COL,
    1, "2024-12-05T08:12", "2024-12-05", 42, TRUE,
    2, "2024-12-05", "2024-12", 100, FALSE
  )

  result <- lubrify_dates(test_data)

  # Check that DTC columns are converted
  expect_s3_class(result$STDTC, "POSIXct")
  expect_s3_class(result$ENDTC, "POSIXct")

  # Check that other column types remain unchanged
  expect_type(result$NUMERIC_COL, "double")
  expect_type(result$LOGICAL_COL, "logical")
  expect_type(result$ID, "double")
})

test_that("lubrify_dates preserves data frame structure", {
  test_data <- tibble::tribble(
    ~ID, ~STDTC, ~ENDTC,
    1, "2024-12-05T08:12", "2024-12-05",
    2, "2024-12-05", "2024-12"
  )

  result <- lubrify_dates(test_data)

  # Check that row count is preserved
  expect_equal(nrow(result), nrow(test_data))

  # Check that column count is preserved
  expect_equal(ncol(result), ncol(test_data))

  # Check that column names are preserved
  expect_equal(names(result), names(test_data))
})

test_that("lubrify_dates handles edge case with single row", {
  test_data <- tibble::tribble(
    ~ID, ~STDTC, ~ENDTC,
    1, "2024-12-05T08:12", "2024-12-05"
  )

  result <- lubrify_dates(test_data)

  expect_s3_class(result$STDTC, "POSIXct")
  expect_s3_class(result$ENDTC, "POSIXct")
  expect_equal(nrow(result), 1)
})

test_that("lubrify_dates handles edge case with single column", {
  test_data <- tibble::tribble(
    ~STDTC,
    "2024-12-05T08:12",
    "2024-12-05"
  )

  result <- lubrify_dates(test_data)

  expect_s3_class(result$STDTC, "POSIXct")
  expect_equal(ncol(result), 1)
})

test_that("lubrify_dates works with tibble input", {
  test_data <- tibble::tibble(
    ID = 1:2,
    STDTC = c("2024-12-05T08:12", "2024-12-05"),
    ENDTC = c("2024-12-05", "2024-12")
  )

  result <- lubrify_dates(test_data)

  expect_s3_class(result$STDTC, "POSIXct")
  expect_s3_class(result$ENDTC, "POSIXct")
  expect_s3_class(result, "tbl_df")
})
