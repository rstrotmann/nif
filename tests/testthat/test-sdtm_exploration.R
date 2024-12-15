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
    disposition_summary(examplinib_sad),
    as.data.frame(tribble(
      ~ACTARMCD, ~ONGOING,  ~N,
      "C1",    FALSE,  3L,
      "C10",   FALSE, 12L,
      "C2",    FALSE,  3L,
      "C3",    FALSE,  3L,
      "C4",    FALSE,  3L,
      "C5",    FALSE,  6L,
      "C6",    FALSE,  3L,
      "C7",    FALSE,  6L,
      "C8",    FALSE,  6L,
      "C9",    FALSE,  3L,
      "SCRNFAIL",       NA, 16L
    ))
  )

})
