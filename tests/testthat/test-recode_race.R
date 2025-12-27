test_that("recode_race works with default coding table", {
  # Create test data with RACE field
  test_data <- tribble(
    ~ID, ~TIME, ~AMT, ~CMT, ~EVID, ~DOSE, ~DV, ~RACE,
    1, 0, 0, 0, 0, 100, 0, "WHITE",
    2, 0, 0, 0, 0, 100, 0, "ASIAN",
    3, 0, 0, 0, 0, 100, 0, "BLACK OR AFRICAN AMERICAN",
    4, 0, 0, 0, 0, 100, 0, "AMERICAN INDIAN OR ALASKA NATIVE",
    5, 0, 0, 0, 0, 100, 0, "NATIVE HAWAIIAN OR OTHER PACIFIC ISLANDER",
    6, 0, 0, 0, 0, 100, 0, "NOT REPORTED",
    7, 0, 0, 0, 0, 100, 0, "UNKNOWN",
    8, 0, 0, 0, 0, 100, 0, "OTHER"
  )

  # Convert to nif object
  test_nif <- nif(test_data)

  # Test recoding
  result <- recode_race(test_nif, silent = TRUE)

  # Check that RACE is now numeric
  expect_type(result$RACE, "double")

  # Check expected values
  expect_equal(result$RACE, c(0, 1, 2, 3, 4, 5, 6, 7))

  # Check that other columns are preserved
  expect_equal(result$ID, test_data$ID)
  expect_equal(result$TIME, test_data$TIME)
  expect_equal(result$AMT, test_data$AMT)
  expect_equal(result$CMT, test_data$CMT)
  expect_equal(result$EVID, test_data$EVID)
  expect_equal(result$DOSE, test_data$DOSE)
  expect_equal(result$DV, test_data$DV)
})

test_that("recode_race works with custom coding table", {
  # Create test data
  test_data <- tribble(
    ~ID, ~TIME, ~AMT, ~CMT, ~EVID, ~DOSE, ~DV, ~RACE,
    1, 0, 0, 0, 0, 100, 0, "Caucasian",
    2, 0, 0, 0, 0, 100, 0, "Asian",
    3, 0, 0, 0, 0, 100, 0, "African"
  )

  # Create custom coding table
  custom_coding <- tribble(
    ~RACE,        ~RACEN,
    "Caucasian",  1,
    "Asian",      2,
    "African",    3
  )

  # Convert to nif object
  test_nif <- nif(test_data)

  # Test recoding with custom table
  result <- recode_race(test_nif, coding_table = custom_coding, silent = TRUE)

  # Check expected values
  expect_equal(result$RACE, c(1, 2, 3))
})

test_that("recode_race handles unmatched values correctly", {
  # Create test data with some unmatched RACE values
  test_data <- tribble(
    ~ID, ~TIME, ~AMT, ~CMT, ~EVID, ~DOSE, ~DV, ~RACE,
    1, 0, 0, 0, 0, 100, 0, "WHITE",
    2, 0, 0, 0, 0, 100, 0, "UNMATCHED_RACE",
    3, 0, 0, 0, 0, 100, 0, "ASIAN"
  )

  # Convert to nif object
  test_nif <- nif(test_data)

  # Test recoding - should warn about unmatched values
  expect_message(
    result <- recode_race(test_nif, silent = FALSE),
    "The following RACE values could not be matched and will become NA"
  )

  # Check that unmatched values become NA
  expect_equal(result$RACE, c(0, NA, 1))
})

test_that("recode_race handles silent parameter", {
  # Create test data with unmatched RACE values
  test_data <- tribble(
    ~ID, ~TIME, ~AMT, ~CMT, ~EVID, ~DOSE, ~DV, ~RACE,
    1, 0, 0, 0, 0, 100, 0, "WHITE",
    2, 0, 0, 0, 0, 100, 0, "UNMATCHED_RACE"
  )

  # Convert to nif object
  test_nif <- nif(test_data)

  # Test with silent = TRUE - should not show message
  expect_no_message(
    result <- recode_race(test_nif, silent = TRUE)
  )

  # Test with silent = FALSE - should show message
  expect_message(
    result <- recode_race(test_nif, silent = FALSE),
    "The following RACE values could not be matched and will become NA"
  )
})

test_that("recode_race validates input correctly", {
  # Test with non-nif object
  test_data <- data.frame(
    ID = 1,
    RACE = "WHITE"
  )

  expect_error(
    recode_race(test_data),
    "Input must be a nif object"
  )

  # Test with missing RACE field
  test_nif <- nif(data.frame(
    ID = 1,
    TIME = 0,
    AMT = 0,
    CMT = 0,
    EVID = 0,
    DOSE = 100,
    DV = 0
  ))

  expect_error(
    recode_race(test_nif),
    "RACE field not found"
  )

  # Test with invalid coding table (missing RACE column)
  test_data <- tribble(
    ~ID, ~TIME, ~AMT, ~CMT, ~EVID, ~DOSE, ~DV, ~RACE,
    1, 0, 0, 0, 0, 100, 0, "WHITE"
  )
  test_nif <- nif(test_data)

  invalid_coding <- data.frame(
    RACEN = 1
  )

  expect_error(
    recode_race(test_nif, coding_table = invalid_coding),
    "coding_table must contain RACE and RACEN columns"
  )

  # Test with invalid coding table (missing RACEN column)
  invalid_coding2 <- data.frame(
    RACE = "WHITE"
  )

  expect_error(
    recode_race(test_nif, coding_table = invalid_coding2),
    "coding_table must contain RACE and RACEN columns"
  )

  # Test with invalid coding table (non-numeric RACEN)
  invalid_coding3 <- data.frame(
    RACE = "WHITE",
    RACEN = "1"
  )

  expect_error(
    recode_race(test_nif, coding_table = invalid_coding3),
    "RACEN column in coding_table must be numeric"
  )
})

test_that("recode_race handles edge cases", {
  # Test with empty nif object
  empty_nif <- nif()
  expect_error(
    recode_race(empty_nif),
    "RACE field not found"
  )

  # Test with single row
  single_row <- tribble(
    ~ID, ~TIME, ~AMT, ~CMT, ~EVID, ~DOSE, ~DV, ~RACE,
    1, 0, 0, 0, 0, 100, 0, "WHITE"
  )
  single_nif <- nif(single_row)

  result <- recode_race(single_nif)
  expect_equal(result$RACE, 0)
  expect_equal(nrow(result), 1)

  # Test with all unmatched values
  all_unmatched <- tribble(
    ~ID, ~TIME, ~AMT, ~CMT, ~EVID, ~DOSE, ~DV, ~RACE,
    1, 0, 0, 0, 0, 100, 0, "UNMATCHED1",
    2, 0, 0, 0, 0, 100, 0, "UNMATCHED2"
  )
  unmatched_nif <- nif(all_unmatched)

  expect_message(
    result <- recode_race(unmatched_nif, silent = FALSE),
    "The following RACE values could not be matched and will become NA"
  )

  expect_equal(result$RACE, c(NA_integer_, NA_integer_))
})

test_that("recode_race preserves nif object structure", {
  # Create test data with all standard nif fields
  test_data <- tribble(
    ~ID, ~TIME, ~AMT, ~CMT, ~EVID, ~DOSE, ~DV, ~RACE, ~SEX, ~AGE, ~WEIGHT,
    1, 0, 0, 0, 0, 100, 0, "WHITE", "M", 25, 70,
    2, 0, 0, 0, 0, 100, 0, "ASIAN", "F", 30, 60
  )

  # Convert to nif object
  test_nif <- nif(test_data)

  # Test recoding
  result <- recode_race(test_nif)

  # Check that result is still a nif object
  expect_s3_class(result, "nif")
  expect_s3_class(result, "data.frame")

  # Check that all original columns are preserved
  expect_true(all(c("ID", "TIME", "AMT", "CMT", "EVID", "DOSE", "DV", "RACE", "SEX", "AGE", "WEIGHT") %in% names(result)))

  # Check that RACE is now numeric
  expect_type(result$RACE, "double")
  expect_equal(result$RACE, c(0, 1))
})

test_that("recode_race works with mixed case and whitespace", {
  # Create test data with mixed case and whitespace
  test_data <- tribble(
    ~ID, ~TIME, ~AMT, ~CMT, ~EVID, ~DOSE, ~DV, ~RACE,
    1, 0, 0, 0, 0, 100, 0, "white",
    2, 0, 0, 0, 0, 100, 0, "  ASIAN  ",
    3, 0, 0, 0, 0, 100, 0, "Black or African American"
  )

  # Convert to nif object
  test_nif <- nif(test_data)

  # Test recoding - should handle case sensitivity and whitespace
  result <- recode_race(test_nif, silent = TRUE)

  # Check that case-sensitive matching works (exact match required)
  expect_equal(result$RACE, c(NA_integer_, NA_integer_, NA_integer_)) # No matches due to case/whitespace differences
})
