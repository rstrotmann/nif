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
