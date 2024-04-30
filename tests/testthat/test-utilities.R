
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
