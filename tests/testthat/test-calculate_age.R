library(testthat)
library(dplyr)
library(lubridate)

test_that("calculate_age calculates age correctly", {
  # Create test data
  test_df <- data.frame(
    USUBJID = c("001", "002", "003"),
    BRTHDTC = as.POSIXct(c("1980-01-15", "1990-05-20", "2000-12-10")),
    RFICDTC = as.POSIXct(c("2020-01-15", "2020-05-20", "2020-12-10"))
  )
  
  result <- calculate_age(test_df)
  
  # Test that ages are calculated correctly (40, 30, 20 years)
  expect_equal(result$AGE, c(40, 30, 20))
})

test_that("calculate_age preserves existing AGE values when not NA", {
  # Create test data with existing AGE
  test_df <- data.frame(
    USUBJID = c("001", "002", "003"),
    BRTHDTC = as.POSIXct(c("1980-01-15", "1990-05-20", "2000-12-10")),
    RFICDTC = as.POSIXct(c("2020-01-15", "2020-05-20", "2020-12-10")),
    AGE = c(41, NA, 19)  # Intentionally different from calculated ages
  )
  
  result <- calculate_age(test_df, preserve_age = TRUE)
  
  # Test that existing non-NA values are preserved, but NA are filled
  expect_equal(result$AGE, c(41, 30, 19))
})

test_that("calculate_age overwrites existing AGE values when preserve_age = FALSE", {
  # Create test data with existing AGE
  test_df <- data.frame(
    USUBJID = c("001", "002", "003"),
    BRTHDTC = as.POSIXct(c("1980-01-15", "1990-05-20", "2000-12-10")),
    RFICDTC = as.POSIXct(c("2020-01-15", "2020-05-20", "2020-12-10")),
    AGE = c(41, NA, 19)  # Intentionally different from calculated ages
  )
  
  result <- calculate_age(test_df, preserve_age = FALSE)
  
  # Test that all values are calculated, regardless of existing values
  expect_equal(result$AGE, c(40, 30, 20))
})

test_that("calculate_age uses custom reference date column", {
  # Create test data with a different reference date column
  test_df <- data.frame(
    USUBJID = c("001", "002"),
    BRTHDTC = as.POSIXct(c("1980-01-15", "1990-05-20")),
    CUSTOM_DTC = as.POSIXct(c("2030-01-15", "2030-05-20"))
  )
  
  result <- calculate_age(test_df, ref_date_col = "CUSTOM_DTC")
  
  # Test that ages are calculated correctly using the custom column (50, 40 years)
  expect_equal(result$AGE, c(50, 40))
})

test_that("calculate_age returns dataframe unchanged when required columns missing", {
  # Create test data without required columns
  test_df1 <- data.frame(
    USUBJID = c("001", "002"),
    RFICDTC = as.POSIXct(c("2020-01-15", "2020-05-20"))
    # Missing BRTHDTC
  )
  
  test_df2 <- data.frame(
    USUBJID = c("001", "002"),
    BRTHDTC = as.POSIXct(c("1980-01-15", "1990-05-20"))
    # Missing RFICDTC
  )
  
  # Test that dataframe is returned unchanged
  expect_identical(calculate_age(test_df1), test_df1)
  expect_identical(calculate_age(test_df2), test_df2)
})

test_that("calculate_age handles non-dataframe input", {
  # Test that error is thrown for non-dataframe input
  expect_error(calculate_age("not a dataframe"), "Input must be a data frame")
  expect_error(calculate_age(list(a = 1, b = 2)), "Input must be a data frame")
})

test_that("calculate_age handles age_brthdtc temporary column correctly", {
  # Create test data
  test_df <- data.frame(
    USUBJID = c("001"),
    BRTHDTC = as.POSIXct(c("1980-01-15")),
    RFICDTC = as.POSIXct(c("2020-01-15"))
  )
  
  result <- calculate_age(test_df)
  
  # Test that the temporary column is removed
  expect_false("age_brthdtc" %in% names(result))
})

test_that("calculate_age rounds age correctly", {
  # Create test data with exact and partial years
  test_df <- data.frame(
    USUBJID = c("001", "002", "003"),
    BRTHDTC = as.POSIXct(c("1980-01-15", "1990-06-01", "2000-01-01")),
    RFICDTC = as.POSIXct(c("2020-01-14", "2020-05-31", "2020-07-01"))
  )
  
  result <- calculate_age(test_df)
  
  # Test that ages are rounded correctly
  # 40 years minus 1 day = 40 (rounded)
  # 30 years minus 1 day = 30 (rounded)
  # 20 years plus 6 months = 20 (rounded)
  expect_equal(result$AGE, c(40, 30, 20))
}) 