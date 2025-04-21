test_that("make_ntime returns lookup table for valid input", {
  # Create a test data frame with ELTM and DY fields
  test_data <- tibble::tribble(
    ~USUBJID, ~PCELTM, ~PCDY,
           1,  "PT0H",     1,
           1,  "PT1H",     1,
           1,  "PT2H",     1,
           1,  "PT4H",     1,
           2,  "PT0H",     2,
           2,  "PT2H",     2,
           2,  "PT8H",     2
  )

  # Test basic functionality
  result <- make_ntime(test_data, "pc")
  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 5) # Should have distinct values
  expect_equal(names(result), c("PCELTM", "NTIME"))
  expect_equal(result$NTIME, c(0, 1, 2, 4, 8))
})

test_that("make_ntime handles include_day parameter correctly", {
  # Create a test data frame with ELTM and DY fields
  test_data <- tibble::tribble(
    ~USUBJID, ~PCELTM, ~PCDY,
           1,  "PT0H",     1,
           1,  "PT1H",     1,
           1,  "PT2H",     1,
           2,  "PT0H",     2,
           2,  "PT1H",     2,
           2,  "PT2H",     2
  )

  # Test with include_day = FALSE (default)
  result_no_day <- make_ntime(test_data, "pc")
  expect_equal(result_no_day$NTIME, c(0, 1, 2))

  # Test with include_day = TRUE
  result_with_day <- make_ntime(test_data, "pc", include_day = TRUE)
  # Day 1 should add 0 hours (trial day 1 corresponds to 0 elapsed days)
  # Day 2 should add 24 hours (trial day 2 corresponds to 1 elapsed day)
  expect_equal(result_with_day$NTIME, c(0, 1, 2, 24, 25, 26))
})

test_that("make_ntime returns NULL when no ELTM field is present", {
  # Create a test data frame without ELTM field
  test_data <- tibble::tribble(
    ~USUBJID, ~XXTIME, ~XXDY,
           1,  "PT0H",     1,
           2,  "PT1H",     2,
           3,  "PT2H",     3
  )

  # Test with silent = TRUE
  expect_null(make_ntime(test_data, "pc", silent = TRUE))

  # Test with silent = FALSE
  expect_message(
    expect_null(make_ntime(test_data, "pc", silent = FALSE)),
    "ELTM is not defined"
  )
})

test_that("make_ntime handles missing DY column correctly", {
  # Create a test data frame with ELTM but no DY field
  test_data <- tibble::tribble(
    ~USUBJID, ~PCELTM,
           1,  "PT0H",
           2,  "PT1H",
           3,  "PT2H"
  )

  # Should work with include_day = FALSE
  result_no_day <- make_ntime(test_data, "pc", include_day = FALSE)
  expect_equal(result_no_day$NTIME, c(0, 1, 2))

  # Should return NULL (or error) with include_day = TRUE because dy is NULL
  expect_error(make_ntime(test_data, "pc", include_day = TRUE))
})

test_that("make_ntime handles NA values correctly", {
  # Create a test data frame with NA values
  test_data <- tibble::tribble(
    ~USUBJID, ~PCELTM, ~PCDY,
           1,  "PT0H",     1,
           1,  "PT1H",     1,
           1,     NA,      1,
           2,  "PT0H",     2,
           2,  "PT2H",     2
  )

  # Test with include_day = FALSE
  result <- make_ntime(test_data, "pc")
  expect_true(any(is.na(result$NTIME)))
  expect_equal(sum(is.na(result$NTIME)), 1)

  # Test with include_day = TRUE
  result_with_day <- make_ntime(test_data, "pc", include_day = TRUE)
  expect_true(any(is.na(result_with_day$NTIME)))
  expect_equal(sum(is.na(result_with_day$NTIME)), 1)
})

test_that("make_ntime correctly converts ISO 8601 formatted durations", {
  # Create a test data frame with various ISO 8601 duration formats
  test_data <- tibble::tribble(
    ~USUBJID,   ~PCELTM, ~PCDY,
           1,    "PT0H",     1,
           1,  "PT1.5H",     1,
           1, "PT2H30M",     1,
           1,   "PT15M",     1,
           1,   "-PT1H",     1
  )

  # Test conversion
  result <- make_ntime(test_data, "pc")
  expect_equal(result$NTIME, c(0, 1.5, 2.5, 0.25, -1))
})

