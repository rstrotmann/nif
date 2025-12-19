test_that("conditional message works", {
  # expect_message(conditional_message("test"), "test")

  old_silent_value <- nif_option_value("silent")
  nif_option("silent" = TRUE)
  expect_no_message(conditional_message("test"))
  nif_option("silent" = FALSE)
  expect_message(conditional_message("test"), "test")
  nif_option("silent" = old_silent_value)
})


test_that("recode sex works", {
  test <- tribble(
    ~ID, ~SEX, ~EXPECTATION,
    1, "0", 0,
    2, "1", 1,
    3, "M", 0,
    4, "F", 1,
    5, "m", 0,
    6, "f", 1,
    7, "男", 0,
    8, "女", 1,
    9, "\u7537", 0,
    10, "\u5973", 1
  )
  expect_equal(recode_sex(test)$SEX, test$EXPECTATION)
})


test_that("positive_or_zero works", {
  expect_equal(positive_or_zero(2), 2)
  expect_equal(positive_or_zero(-2), 0)
  expect_equal(positive_or_zero(c(2, 1, 0, -1, -2)), c(2, 1, 0, 0, 0))
})


test_that("indent_string works", {
  expect_equal(indent_string(4), "    ")
  expect_equal(indent_string(0), "")
  expect_equal(indent_string(-1), "")
})


test_that("standardize_date_format works", {
  test <- tribble(
    ~ID,              ~STDTC,                ~ENDTC,
    1,    "2024-12-05T08:12",          "2024-12-05",
    2,          "2024-12-05",             "2024-12",
    3,             "2024-12", "2024-12-05T08:12:30",
    4, "2024-12-05T08:12:30",                "2024",
    5,                "2024",    "2024-12-05 08:12",
    6,    "2024-12-05 08:12", "2024-12-05 08:12:30",
    7, "2024-12-05 08:12:30",    "2024-12-05T08:12"
  )
  temp <- standardize_date_format(test, fields = c("STDTC", "ENDTC"))
  expect_s3_class(temp$STDTC, "POSIXct")
  expect_s3_class(temp$ENDTC, "POSIXct")
})


test_that("isofy_date_format works", {
  test <- tribble(
    ~ID,              ~STDTC,                ~ENDTC,
    1, "2024-12-05 08:12:00",          "2024-12-05",
    2,          "2024-12-05",          "2024-12-01",
    3,          "2024-12-01", "2024-12-05 08:12:30",
    4, "2024-12-05 08:12:30",          "2024-01-01",
    5,          "2024-01-01", "2024-12-05 08:12:00",
    6, "2024-12-05 08:12:00", "2024-12-05 08:12:30",
    7, "2024-12-05 08:12:30", "2024-12-05 08:12:00"
  ) %>%
    mutate(STDTC = lubridate::as_datetime(STDTC, format = dtc_formats)) %>%
    mutate(ENDTC = lubridate::as_datetime(ENDTC, format = dtc_formats))

  temp <- isofy_date_format(test, fields = c("STDTC", "ENDTC"))
  expect_equal(
    temp$STDTC,
    c(
      "2024-12-05T08:12",
      "2024-12-05T00:00",
      "2024-12-01T00:00",
      "2024-12-05T08:12",
      "2024-01-01T00:00",
      "2024-12-05T08:12",
      "2024-12-05T08:12"
    )
  )
})


test_that("lubrify_dates works", {
  test <- tribble(
    ~ID,              ~STDTC,                ~ENDTC,
    1,    "2024-12-05T08:12",          "2024-12-05",
    2,          "2024-12-05",             "2024-12",
    3,             "2024-12", "2024-12-05T08:12:30",
    4, "2024-12-05T08:12:30",                "2024",
    5,                "2024",    "2024-12-05 08:12",
    6,    "2024-12-05 08:12", "2024-12-05 08:12:30",
    7, "2024-12-05 08:12:30",    "2024-12-05T08:12"
  )
  temp <- lubrify_dates(test)
  expect_s3_class(temp$STDTC, "POSIXct")
  expect_s3_class(temp$ENDTC, "POSIXct")
})


test_that("isofy_dates works", {
  test <- tribble(
    ~ID,              ~STDTC,                ~ENDTC,
    1, "2024-12-05 08:12:00",          "2024-12-05",
    2,          "2024-12-05",          "2024-12-01",
    3,          "2024-12-01", "2024-12-05 08:12:30",
    4, "2024-12-05 08:12:30",          "2024-01-01",
    5,          "2024-01-01", "2024-12-05 08:12:00",
    6, "2024-12-05 08:12:00", "2024-12-05 08:12:30",
    7, "2024-12-05 08:12:30", "2024-12-05 08:12:00"
  ) %>%
    mutate(STDTC = lubridate::as_datetime(STDTC, format = dtc_formats)) %>%
    mutate(ENDTC = lubridate::as_datetime(ENDTC, format = dtc_formats))

  temp <- isofy_dates(test)
  expect_type(temp$STDTC, "character")
  expect_type(temp$ENDTC, "character")
})


test_that("is_iso_datetime works", {
  test <- c(
    "2024-12-05T08:12",
    "2024-12-05T08:12:30",
    "2024-12-05",
    "2024-12",
    "2024-12-05T08:12:30",
    "2024",
    "2024-12-05 08:12",
    "2024-12-05 08:12:30"
  )
  temp <- as.logical(lapply(test, is_iso_date_time))
  expect_equal(
    temp, c(TRUE, TRUE, FALSE, FALSE, TRUE, FALSE, FALSE, FALSE)
  )
})


test_that("is_iso_date works", {
  test <- c(
    "2024-12-05T08:12",
    "2024-12-05T08:12:30",
    "2024-12-05",
    "2024-12",
    "2024-12-05T08:12:30",
    "2024",
    "2024-12-05 08:12",
    "2024-12-05 08:12:30"
  )
  temp <- as.logical(lapply(test, is_iso_date))
  expect_equal(
    temp, c(TRUE, TRUE, TRUE, FALSE, TRUE, FALSE, TRUE, TRUE)
  )
})


test_that("pt_to_hours works", {
  test <- tribble(
    ~iso,         ~out,
    "",           NA,
    "3H",         NA,
    "PT3H",       3,
    "-PT3H",      -3,
    "PT15M",      0.25,
    "PT3H15M",    3.25,
    "-PT3H15M",   -3.25,
    "PT1H15M30S", 1.25
  )
  expect_equal(pt_to_hours(test$iso), test$out)
})


test_that("compose_dtc works", {
  expect_s3_class(compose_dtc(date = "2022-09-29", time = "09:30"), "POSIXct")
})


test_that("extract_date works", {
  test <- tribble(
    ~ID,              ~STDTC,                ~ENDTC,
    1, "2024-12-05 08:12:00",          "2024-12-05",
    2,          "2024-12-05",          "2024-12-01",
    3,          "2024-12-01", "2024-12-05 08:12:30",
    4, "2024-12-05 08:12:30",          "2024-01-01",
    5,          "2024-01-01", "2024-12-05 08:12:00",
    6, "2024-12-05 08:12:00", "2024-12-05 08:12:30",
    7, "2024-12-05 08:12:30", "2024-12-05 08:12:00"
  ) %>%
    mutate(STDTC = lubridate::as_datetime(STDTC, format = dtc_formats)) %>%
    mutate(ENDTC = lubridate::as_datetime(ENDTC, format = dtc_formats))

  expect_equal(
    extract_date(test$STDTC),
    c(
      "2024-12-05", "2024-12-05", "2024-12-01", "2024-12-05", "2024-01-01",
      "2024-12-05", "2024-12-05"
    )
  )
})


test_that("extract_time works", {
  test <- tribble(
    ~ID,              ~STDTC,                ~ENDTC,
    1, "2024-12-05 08:12:00",          "2024-12-05",
    2,          "2024-12-05",          "2024-12-01",
    3,          "2024-12-01", "2024-12-05 08:12:30",
    4, "2024-12-05 08:12:30",          "2024-01-01",
    5,          "2024-01-01", "2024-12-05 08:12:00",
    6, "2024-12-05 08:12:00", "2024-12-05 08:12:30",
    7, "2024-12-05 08:12:30", "2024-12-05 08:12:00"
  ) %>%
    mutate(STDTC = lubridate::as_datetime(STDTC, format = dtc_formats)) %>%
    mutate(ENDTC = lubridate::as_datetime(ENDTC, format = dtc_formats))

  expect_equal(
    extract_time(test$STDTC),
    c("08:12", "00:00", "00:00", "08:12", "00:00", "08:12", "08:12")
  )
})


test_that("has time works", {
  valid_times <- c("2024-04-30T07:36:00", "2024-04-30T07:36")
  valid_times1 <- lubridate::parse_date_time(valid_times, c("ymd HMS", "ymd HM"))
  invalid_times <- c("2024-04-30", "2024-04-30T")

  expect_true(all(has_time(valid_times)))
  expect_true(all(has_time(valid_times1)))
  expect_false(all(has_time(invalid_times)))

  test <- tribble(
    ~ID,              ~STDTC,                ~ENDTC,
    1, "2024-12-05 08:12:00",          "2024-12-05",
    2,          "2024-12-05",          "2024-12-01",
    3,          "2024-12-01", "2024-12-05 08:12:30",
    4, "2024-12-05 08:12:30",          "2024-01-01",
    5,          "2024-01-01", "2024-12-05 08:12:00",
    6, "2024-12-05 08:12:00", "2024-12-05 08:12:30",
    7, "2024-12-05 08:12:30", "2024-12-05 08:12:00"
  ) %>%
    mutate(STDTC = lubridate::as_datetime(STDTC, format = dtc_formats)) %>%
    mutate(ENDTC = lubridate::as_datetime(ENDTC, format = dtc_formats))

  expect_equal(
    has_time(test$STDTC),
    c(TRUE, FALSE, FALSE, TRUE, FALSE, TRUE, TRUE)
  )
})


test_that("nice enumeration works", {
  expect_equal(nice_enumeration("A"), "A")
  expect_equal(nice_enumeration(c("A", "B", "C")), "A, B and C")
  expect_equal(
    nice_enumeration(c("A", "B", "C"), conjunction = "or"),
    "A, B or C"
  )
})


test_that("plural works", {
  expect_equal(plural("subject", FALSE), "subject")
  expect_equal(plural("subject", TRUE), "subjects")
  expect_equal(plural("study", FALSE), "study")
  expect_equal(plural("study", TRUE), "studies")
})


test_that("safe_mean works", {
  expect_equal(as.numeric(safe_mean(c(2, 2, 2, 2))), 2)
  expect_equal(as.numeric(safe_mean(c(2, 2, 2, NA))), 2)
  expect_equal(as.numeric(safe_mean(c(2, 2, 2, NaN))), 2)
})


test_that("safe_sd works", {
  expect_equal(as.numeric(safe_sd(c(1, 2, 3))), 1)
  expect_equal(as.numeric(safe_sd(c(1, 2, 3, NA))), 1)
  expect_equal(as.numeric(safe_sd(c(1, 2, 3, NaN))), 1)
})


test_that("safe min works", {
  expect_equal(as.numeric(safe_min(c(1, 2, 3))), 1)
  expect_equal(as.numeric(safe_min(c(1, -2, 3))), -2)
  expect_equal(as.numeric(safe_min(c(1, 2, NA))), 1)
  expect_equal(as.numeric(safe_min(c(1, 2, NaN))), 1)
})


test_that("pos_diff works", {
  expect_equal(pos_diff(3, 2), 1)
  expect_equal(
    pos_diff(
      c(1, 2, 3, 4),
      c(2, 2, 2, 2)
    ),
    c(NA, 0, 1, 2)
  )
})


test_that("trialday_to_day works", {
  expect_equal(trialday_to_day(-1), -1)
  expect_equal(trialday_to_day(1), 0)
  expect_equal(trialday_to_day(
    c(-3, -1, 1, 3)
  ), c(-3, -1, 0, 2))
  expect_error(trialday_to_day(0))
  expect_equal(trialday_to_day(c(-1, NA, 1)), c(-1, NA, 0))
})


test_that("is_iso8601_datetime correctly identifies ISO 8601 date-time formats", {
  # Valid formats
  expect_true(is_iso8601_datetime("2023-10-15T14:30:00"))
  expect_true(is_iso8601_datetime("2023-10-15 14:30:00"))
  expect_true(is_iso8601_datetime("2023-10-15T14:30:00Z"))
  expect_true(is_iso8601_datetime("2023-10-15T14:30:00+02:00"))
  expect_true(is_iso8601_datetime("2023-10-15T14:30:00-07:00"))
  expect_true(is_iso8601_datetime("2023-10-15T14:30:00+0200"))
  expect_true(is_iso8601_datetime("2023-10-15T14:30:00.123"))
  expect_true(is_iso8601_datetime("20231015T143000"))
  expect_true(is_iso8601_datetime("20231015T143000Z"))
  expect_true(is_iso8601_datetime("20231015T143000.123"))

  # Mixed formats
  expect_true(is_iso8601_datetime("2023-10-15T143000"))
  expect_true(is_iso8601_datetime("20231015T14:30:00"))

  # Strict mode tests
  expect_true(is_iso8601_datetime("2023-10-15T14:30:00", strict = TRUE))
  expect_false(is_iso8601_datetime("2023-10-15 14:30:00", strict = TRUE))

  # Invalid date-time formats
  expect_false(is_iso8601_datetime("2023-10-15")) # Date only
  expect_false(is_iso8601_datetime("14:30:00")) # Time only
  expect_false(is_iso8601_datetime("2023/10/15T14:30:00")) # Wrong date format
  expect_false(is_iso8601_datetime("2023-10-15T14.30.00")) # Wrong time format
  expect_false(is_iso8601_datetime("2023-10-15 at 14:30:00")) # Wrong separator
  expect_false(is_iso8601_datetime("2023-10-15T14:30")) # Missing seconds
  expect_false(is_iso8601_datetime("Not a datetime")) # Not a datetime
  expect_false(is_iso8601_datetime("")) # Empty string

  # Input validation
  expect_error(is_iso8601_datetime(123), "Input must be a character string")
  expect_error(is_iso8601_datetime(Sys.time()), "Input must be a character string")

  # NA handling
  expect_true(is.na(is_iso8601_datetime(NA_character_)))
})


test_that("is_iso8601_datetime works with vectors", {
  test_datetimes <- c(
    "2023-10-15T14:30:00", # Valid ISO format with T separator
    "2023-10-15 14:30:00", # Valid ISO format with space separator
    "20231015T143000", # Valid ISO basic format
    "2023-10-15", # Date only - invalid for date-time
    "14:30:00", # Time only - invalid for date-time
    "2023/10/15T14:30:00", # Invalid date format
    NA_character_ # NA
  )

  expected_results <- c(TRUE, TRUE, TRUE, FALSE, FALSE, FALSE, NA)

  # Test vector input
  results <- is_iso8601_datetime(test_datetimes)
  expect_equal(results, expected_results)
})


test_that("is_iso8601_date correctly identifies ISO 8601 date formats", {
  # Valid full date formats
  expect_true(is_iso8601_date("2023-10-15"))
  expect_true(is_iso8601_date("20231015"))

  # Valid reduced precision formats (allowed by default)
  expect_true(is_iso8601_date("2023-10"))
  expect_true(is_iso8601_date("202310"))
  expect_true(is_iso8601_date("2023"))

  # Test with allow_reduced_precision = FALSE
  expect_false(is_iso8601_date("2023-10", allow_reduced_precision = FALSE))
  expect_false(is_iso8601_date("202310", allow_reduced_precision = FALSE))
  expect_false(is_iso8601_date("2023", allow_reduced_precision = FALSE))

  # Invalid date formats
  expect_false(is_iso8601_date("2023/10/15"))
  expect_false(is_iso8601_date("10/15/2023"))
  expect_false(is_iso8601_date("15-Oct-2023"))
  expect_false(is_iso8601_date("23-10-15"))
  expect_false(is_iso8601_date("2023.10.15"))
  expect_false(is_iso8601_date("Not a date"))
  expect_false(is_iso8601_date(""))

  # Date with time component (should be false for date-only function)
  expect_false(is_iso8601_date("2023-10-15T14:30:00"))
  expect_false(is_iso8601_date("20231015T143000"))
  expect_false(is_iso8601_date("2023-10-15 14:30:00"))

  # Input validation
  expect_error(is_iso8601_date(123), "Input must be a character string")
  expect_error(is_iso8601_date(Sys.Date()), "Input must be a character string")

  # NA handling
  expect_true(is.na(is_iso8601_date(NA_character_)))
})


test_that("is_iso8601_date works with vectors", {
  test_dates <- c(
    "2023-10-15", # Valid extended format
    "20231015", # Valid basic format
    "2023-10", # Valid reduced precision (year-month)
    "2023", # Valid reduced precision (year)
    "2023/10/15", # Invalid format
    "2023-10-15T14:30", # Has time component (invalid for date-only)
    "", # Empty string
    NA_character_ # NA
  )

  # With default settings (allow_reduced_precision = TRUE)
  expected_results <- c(TRUE, TRUE, TRUE, TRUE, FALSE, FALSE, FALSE, NA)

  # Test vector input
  results <- is_iso8601_date(test_dates)
  expect_equal(results, expected_results)

  # With allow_reduced_precision = FALSE
  expected_results_strict <- c(TRUE, TRUE, FALSE, FALSE, FALSE, FALSE, FALSE, NA)

  # Test vector input with strict settings
  results_strict <- is_iso8601_date(test_dates, allow_reduced_precision = FALSE)
  expect_equal(results_strict, expected_results_strict)
})
