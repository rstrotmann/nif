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
    1,   "0",  0,
    2,   "1",  1,
    3,   "M",  0,
    4,   "F",  1,
    5,   "m",  0,
    6,   "f",  1,
    7,   "男", 0,
    8,   "女", 1,
    9,   "\u7537", 0,
    10,  "\u5973", 1
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


test_that("df_to_string works", {
  test <- tribble(
    ~ID, ~A,  ~B,
    1,   "A", "B",
    2,   "C", "D"
  )
  expect_equal(
    df_to_string(test), "ID   A   B   \n1    A   B   \n2    C   D   ")
  expect_equal(
    df_to_string(test, indent = 2),
    "  ID   A   B   \n  1    A   B   \n  2    C   D   ")
  expect_equal(df_to_string(test, n = 1), "ID   A   B   \n1    A   B   ")
  expect_equal(
    df_to_string(test, header = FALSE),
    "\n1    A   B   \n2    C   D   ")
  expect_equal(df_to_string(data.frame(), show_none = TRUE), "none")
  expect_equal(
    df_to_string(test, header_sep = TRUE),
    "ID   A   B   \n--   -   -\n1    A   B   \n2    C   D   ")
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

  temp <- isofy_date_format(test,  fields = c("STDTC", "ENDTC"))
  expect_equal(
    temp$STDTC,
    c("2024-12-05T08:12",
      "2024-12-05T00:00",
      "2024-12-01T00:00",
      "2024-12-05T08:12",
      "2024-01-01T00:00",
      "2024-12-05T08:12",
      "2024-12-05T08:12"))
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
    "2024-12-05 08:12:30")
  temp <- as.logical(lapply(test, is_iso_date_time))
  expect_equal(
    temp, c(TRUE, TRUE, FALSE, FALSE, TRUE, FALSE, FALSE, FALSE))
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
    "2024-12-05 08:12:30")
  temp <- as.logical(lapply(test, is_iso_date))
  expect_equal(
    temp, c(TRUE, TRUE, TRUE, FALSE, TRUE, FALSE, TRUE, TRUE))
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


test_that("decompose_dtc works", {
  test <- data.frame(
    test1_DTC = c("2022-02-15T09:30",
            "2022-02-16",
            "2022-02-17T00:00"),
    test2_DTC = c("2023-02-15T09:30",
             "2023-02-16",
             "2023-02-17T00:00")) %>%
    lubrify_dates() %>%
    decompose_dtc("test1_DTC")

  expect_equal(!is.na(test$test1_DTC_time), c(TRUE, FALSE, FALSE))
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
    c("2024-12-05", "2024-12-05", "2024-12-01", "2024-12-05", "2024-01-01",
      "2024-12-05", "2024-12-05"))

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
    c("08:12", "00:00", "00:00", "08:12", "00:00", "08:12", "08:12"))

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
    c(TRUE, FALSE, FALSE, TRUE, FALSE, TRUE, TRUE))
})


test_that("nice enumeration works", {
  expect_equal(nice_enumeration("A"), "A")
  expect_equal(nice_enumeration(c("A", "B", "C")), "A, B and C")
  expect_equal(nice_enumeration(c("A", "B", "C"), conjunction = "or"),
               "A, B or C")
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
  expect_equal(pos_diff(
    c(1, 2, 3, 4),
    c(2, 2, 2, 2)),
    c(NA, 0, 1, 2))
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



