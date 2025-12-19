test_that("filter_correct_date_format works", {
  test <- tribble(
    ~DOMAIN, ~TESTDTC,
    "XX", "2024-12-07",
    "XX", "2024-12",
    "XX", "2024",
    "XX", "2024-12-07 10:38",
    "XX", "2024-12-07T10:38",
    "XX", "2024-12-07T10"
  )
  suppressMessages(
    expect_no_error(filter_correct_date_format(test))
  )
})


test_that("check_date_format, check_date_time_format works", {
  expect_no_message(check_date_format(domain(examplinib_sad, "pc")))
  expect_no_message(check_date_time_format(domain(examplinib_sad, "pc")))
  expect_no_message(check_missing_time(domain(examplinib_sad, "pc")))
})


test_that("check_last_exendtc works", {
  expect_no_error(check_last_exendtc(domain(examplinib_poc, "ex")))
})


test_that("check_sdtm works", {
  expect_no_message(check_sdtm(examplinib_poc))
})


test_that("plot.sdtm works", {
  expect_no_error(plot(examplinib_poc, domain = "dm"))
  expect_no_error(plot(examplinib_poc, domain = "ex"))
  expect_no_error(plot(examplinib_poc, domain = "pc"))
  expect_no_error(plot(examplinib_poc, domain = "lb"))
  expect_no_error(plot(examplinib_poc, domain = "vs"))

  temp <- examplinib_poc
  xx <- domain(temp, "pc") %>%
    mutate(DOMAIN = "XX") %>%
    mutate(XXDTC = PCDTC)
  temp$domains$xx <- xx
  expect_no_error(plot(temp, domain = "xx"))
})


test_that("disposition_summary works", {
  expect_equal(
    disposition_summary(examplinib_sad) %>%
      filter(ACTARMCD != "SCRNFAIL"),
    as.data.frame(tribble(
      ~ACTARMCD, ~ONGOING, ~N,
      "C1", FALSE, 3L,
      "C10", FALSE, 12L,
      "C2", FALSE, 3L,
      "C3", FALSE, 3L,
      "C4", FALSE, 3L,
      "C5", FALSE, 6L,
      "C6", FALSE, 3L,
      "C7", FALSE, 6L,
      "C8", FALSE, 6L,
      "C9", FALSE, 3L
    ))
  )
})


# Test file for filter_correct_date_format function

test_that("filter_correct_date_format handles valid ISO 8601 dates correctly", {
  # Create test data with valid ISO 8601 dates
  test_data <- tibble::tribble(
    ~USUBJID, ~DOMAIN, ~STDTC, ~ENDTC,
    "SUBJ-001", "DM", "2023-10-15", "2023-10-16",
    "SUBJ-002", "DM", "2023-10-15", "2023-10-16",
    "SUBJ-003", "DM", "2023-10-15", "2023-10-16"
  )

  # Test that valid dates are preserved
  result <- filter_correct_date_format(test_data, verbose = FALSE)
  expect_equal(nrow(result), nrow(test_data))
  expect_identical(result, test_data)
})


test_that("filter_correct_date_format filters out invalid date formats", {
  # Create test data with mixed valid and invalid dates
  test_data <- tibble::tribble(
    ~USUBJID, ~DOMAIN, ~STDTC, ~ENDTC,
    "SUBJ-001", "DM", "2023-10-15", "2023-10-16", # Valid
    "SUBJ-002", "DM", "2023/10/15", "2023-10-16", # Invalid STDTC
    "SUBJ-003", "DM", "2023-10-15", "2023/10/16", # Invalid ENDTC
    "SUBJ-004", "DM", "2023-10-15", "2023-10-16" # Valid
  )

  # Test that invalid dates are filtered out
  expect_message(
    {
      result <- filter_correct_date_format(
        test_data,
        verbose = FALSE, silent = FALSE
      )
    },
    "DM: 2 rows containing DTC fields with incomplete date format were ignored!"
  )
  expect_equal(nrow(result), 2)
  expect_equal(unique(result$USUBJID), c("SUBJ-001", "SUBJ-004"))
})


test_that("filter_correct_date_format handles empty strings and NA values", {
  # Create test data with empty strings and NA values
  test_data <- tibble::tribble(
    ~USUBJID, ~DOMAIN, ~STDTC, ~ENDTC,
    "SUBJ-001", "DM", "2023-10-15", "", # Valid with empty string
    "SUBJ-002", "DM", "2023-10-15", NA_character_, # Valid with NA
    "SUBJ-003", "DM", "", "2023-10-16", # Valid with empty string
    "SUBJ-004", "DM", NA_character_, "2023-10-16" # Valid with NA
  )

  # Test that empty strings and NA values are preserved
  expect_no_message(
    result <- filter_correct_date_format(test_data, verbose = FALSE, silent = FALSE)
  )
  expect_equal(nrow(result), nrow(test_data))
  expect_identical(result, test_data)
})


test_that("filter_correct_date_format provides correct verbose output", {
  # Create test data with invalid dates
  test_data <- tibble::tribble(
    ~USUBJID, ~DOMAIN, ~STDTC, ~ENDTC,
    "SUBJ-001", "DM", "2023-10-15", "2023/10/16", # Invalid ENDTC
    "SUBJ-002", "DM", "2023/10/15", "2023-10-16" # Invalid STDTC
  )

  # Test verbose output
  expect_message(
    result <- filter_correct_date_format(test_data, verbose = TRUE, silent = FALSE),
    "DM: 2 rows containing DTC fields with incomplete date format"
  )
})


test_that("filter_correct_date_format handles silent parameter correctly", {
  # Create test data with invalid dates
  test_data <- tibble::tribble(
    ~USUBJID, ~DOMAIN, ~STDTC, ~ENDTC,
    "SUBJ-001", "DM", "2023-10-15", "2023/10/16" # Invalid ENDTC
  )

  # Test with silent = TRUE
  expect_no_message(
    result <- filter_correct_date_format(test_data, verbose = TRUE, silent = TRUE)
  )

  # Test with silent = FALSE
  expect_message(
    result <- filter_correct_date_format(test_data, verbose = TRUE, silent = FALSE),
    "DM: 1 rows containing DTC fields with incomplete date format were ignored!"
  )
})


test_that("filter_correct_date_format validates input correctly", {
  # Test with non-dataframe input
  expect_error(
    filter_correct_date_format(list()),
    "Input must be a data frame"
  )

  # Test with missing DOMAIN column
  test_data <- tibble::tribble(
    ~USUBJID, ~STDTC, ~ENDTC,
    "SUBJ-001", "2023-10-15", "2023/10/15"
  )
  expect_message(
    filter_correct_date_format(test_data, silent = FALSE),
    "1 rows containing DTC fields with incomplete date format were ignored!"
  )
})


test_that("filter_correct_date_format handles multiple DTC columns correctly", {
  # Create test data with multiple DTC columns
  test_data <- tibble::tribble(
    ~USUBJID, ~DOMAIN, ~STDTC, ~ENDTC, ~MIDTC,
    "SUBJ-001", "DM", "2023-10-15", "2023-10-16", "2023-10-15", # All valid
    "SUBJ-002", "DM", "2023/10/15", "2023-10-16", "2023-10-15", # Invalid STDTC
    "SUBJ-003", "DM", "2023-10-15", "2023-10-16", "2023/10/15" # Invalid MIDTC
  )

  # Test that all DTC columns are checked
  expect_message(
    result <- filter_correct_date_format(
      test_data,
      verbose = FALSE, silent = FALSE
    ),
    "DM: 2 rows containing DTC fields with incomplete date format were ignored!"
  )
  expect_equal(nrow(result), 1)
  expect_equal(result$USUBJID, "SUBJ-001")
})


test_that("check_missing_time handles valid inputs correctly", {
  # Create test data with various date formats
  test_data <- data.frame(
    USUBJID = c("SUBJ1", "SUBJ2", "SUBJ3"),
    DOMAIN = "TEST",
    DTC = c("2024-01-01", "2024-01-01T10:00:00", ""),
    DTC = c("2024-02-01T15:30:00", "2024-02-01", "2024-02-01T12:00:00")
  )

  # Test with verbose = TRUE
  expect_message(
    result <- check_missing_time(test_data, verbose = TRUE, silent = FALSE),
    "TEST: Missing time in 1 rows!"
  )

  # Test with silent = TRUE
  expect_silent(
    check_missing_time(test_data, verbose = TRUE, silent = TRUE)
  )
})


test_that("check_missing_time handles invalid inputs correctly", {
  # Test with non-dataframe input
  expect_error(
    check_missing_time("not a dataframe"),
    "Input must be a data frame"
  )

  # Test with dataframe but no DTC columns
  test_data_no_dtc <- data.frame(
    USUBJID = c("SUBJ1", "SUBJ2"),
    DOMAIN = "TEST",
    OTHER = c("A", "B")
  )
  expect_warning(
    check_missing_time(test_data_no_dtc, silent = FALSE),
    "No columns ending with 'DTC' found in the input data frame"
  )
})


test_that("check_missing_time handles empty dataframes correctly", {
  # Test with empty dataframe
  empty_df <- data.frame(
    USUBJID = character(0),
    DOMAIN = character(0),
    DTC1 = character(0)
  )

  expect_warning(
    check_missing_time(empty_df, silent = FALSE),
    "No columns ending with 'DTC' found in the input data frame"
  )
})


test_that("check_missing_time handles different date formats correctly", {
  # Create test data with various date formats
  test_data <- data.frame(
    USUBJID = c("SUBJ1", "SUBJ2", "SUBJ3", "SUBJ4"),
    DOMAIN = "TEST",
    DTC = c(
      "2024-01-01", # Date only
      "2024-01-01T10:00:00", # Full datetime
      "", # Empty string
      "2024-01-01T" # Invalid format
    )
  )

  # Should only detect missing time in the date-only format
  expect_message(
    result <- check_missing_time(test_data, verbose = TRUE, silent = FALSE),
    "Missing time in 1 rows!"
  )
})


test_that("check_missing_time preserves input data", {
  test_data <- data.frame(
    USUBJID = c("SUBJ1", "SUBJ2"),
    DOMAIN = "TEST",
    DTC = c("2024-01-01", "2024-01-01T10:00:00")
  )

  # Function should return the same data frame
  result <- check_missing_time(test_data, silent = TRUE)
  expect_identical(result, test_data)
})


test_that("check_missing_time handles multiple DTC columns correctly", {
  test_data <- data.frame(
    USUBJID = c("SUBJ1", "SUBJ2", "SUBJ3"),
    DOMAIN = "TEST",
    A_DTC = c("2024-01-01", "2024-01-01T10:00:00", "2024-01-01"),
    B_DTC = c("2024-02-01", "2024-02-01T15:30:00", "2024-02-01T12:00:00")
  )

  # Should detect missing time in both DTC columns
  expect_message(
    check_missing_time(test_data, verbose = TRUE, silent = FALSE),
    "Missing time in 2 rows!"
  )
})


test_that("check_missing_time handles missing DOMAIN column", {
  test_data <- data.frame(
    USUBJID = c("SUBJ1", "SUBJ2"),
    DTC = c("2024-01-01", "2024-01-01T10:00:00")
  )

  # Should work without DOMAIN column
  expect_message(
    check_missing_time(test_data, verbose = TRUE, silent = FALSE),
    "Missing time in 1 rows!"
  )
})
