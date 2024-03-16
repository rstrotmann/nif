
test_that("split_DTC works", {
  test <- data.frame(
    test1_DTC = c("2022-02-15T09:30",
            "2022-02-16",
            "2022-02-17T00:00"),
    test2_DTC = c("2023-02-15T09:30",
             "2023-02-16",
             "2023-02-17T00:00")) %>%
    lubrify_dates() %>%
    decompose_dtc("test_DTC")

  expect_equal(!is.na(test$test_DTC_time), c(TRUE, FALSE, FALSE))
})



