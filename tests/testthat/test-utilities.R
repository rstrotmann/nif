
test_that("pt_to_hours works", {
  test <- tibble::tribble(
    ~iso,  ~out,
    "3H",     0,
    "PT3H",     3,
    "-PT3H",    -3,
    "PT15M",  0.25,
    "PT3H15M",  3.25,
    "-PT3H15M", -3.25,
    "PT1H15M30S", 1.25
  )
  expect_equal(pt_to_hours(test$iso), test$out)
})


test_that("split_DTC works", {
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


test_that("has time works", {
  valid_times <- c("2024-04-30T07:36:00", "2024-04-30T07:36")
  valid_times1 <- lubridate::parse_date_time(valid_times, c("ymd HMS", "ymd HM"))
  invalid_times <- c("2024-04-30", "2024-04-30T")

  expect_true(all(has_time(valid_times)))
  expect_true(all(has_time(valid_times1)))
  expect_false(all(has_time(invalid_times)))
})


test_that("decompoase_dtc works", {
  test <- data.frame(
    test1_dtc = c(
      "2024-01-01T8:00",
      "2024-02-01",
      "2024-03-15T01:02:03"
    ),
    test2_dtc = c(
      "2024-04-01T10:11:12",
      "2024-04-15T11:12",
      "2024-05-01"
    )
  ) %>% lubrify_dates()

  expect_no_error(
    temp <- decompose_dtc(test, "test1_dtc")
  )
  expect_equal(temp$test1_dtc_date, c("2024-01-01", "2024-02-01", "2024-03-15"))
  expect_equal(temp$test1_dtc_time, c("08:00", NA, "01:02"))
})


