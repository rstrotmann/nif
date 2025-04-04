
test_that("date_list works", {
  expect_no_error(date_list("2025-1-1", "2025-1-3"))
  expect_no_error(date_list("2025-1-1", "2025-1-3", 1, 3))
  expect_no_error(date_list("2025-1-1", "2025-1-3", 11, 13))
  expect_no_error(date_list("2025-1-1", "2025-1-3", 1, NA))
  expect_no_error(date_list("2025-1-1", "2025-1-3", NA, NA))
  expect_no_error(date_list("2025-1-1", "2025-1-3", NA, 3))

  expect_error(date_list("2025-1-3", "2025-1-1", NA, 3))
  expect_error(date_list("2025-1-1", "2025-1-3", 3, 1))
})





test_that("expand_ex works in general", {
  ex <- tribble(
    ~USUBJID , ~EXTRT , ~EXSTDTC           , ~EXENDTC,
    "A"      , "DRUG" , "2025-01-01T07:00" , "2025-01-03",
    "A"      , "DRUG" , "2025-01-04T08:00" , "2025-01-06",
    "A"      , "DRUG" , "2025-01-09T09:00" , "2025-01-11T10:00"
  ) %>% lubrify_dates()

  expect_no_error(temp <- expand_ex(ex))

  expect_equal(
    temp$IMPUTATION,
    c("",
      "time carried forward",
      "time carried forward",
      "",
      "time carried forward",
      "time carried forward",
      "",
      "time carried forward",
      "")
  )
})


test_that("expand_ex works with TRTDY", {
  ex <- tribble(
    ~USUBJID, ~EXTRT,              ~EXSTDTC,              ~EXENDTC, ~EXSTDY, ~EXENDY,
    "A",      "DRUG", "2025-01-01 07:00:00",          "2025-01-03",       1,       3,
    "A",      "DRUG", "2025-01-04 08:00:00",          "2025-01-06",       4,       6,
    "A",      "DRUG", "2025-01-07 09:00:00", "2025-01-10 10:00:00",       7,      10
  ) %>% lubrify_dates()

  expect_no_error(temp <- expand_ex(ex))

  expect_equal(
    temp$EXDY,
    seq(1, 10)
  )
})


test_that("expand_ex works with missing EXENDTC", {
  ex <- tribble(
    ~USUBJID, ~EXTRT,           ~EXSTDTC,     ~EXENDTC,
    "1",         "A", "2025-01-01T07:00", "2025-01-03",
    "1",         "A", "2025-01-04T08:00",           NA,
    "2",         "A", "2025-01-01",       "2025-01-03",
    "2",         "A", "2025-01-04T10:00",           NA
    ) %>% lubrify_dates()

  expect_no_error(temp <- expand_ex(ex))
  expect_equal(
    temp$IMPUTATION,
    c("",
      "time carried forward",
      "time carried forward",
      "",
      "no time information",
      "no time information",
      "no time information",
      "")
    )
})


test_that("expand_ex errs when end date before start date", {
  ex <- tribble(
    ~USUBJID, ~EXTRT,           ~EXSTDTC,     ~EXENDTC,
    "1",         "A", "2025-01-03T07:00", "2025-01-01"
  ) %>% lubrify_dates()

  expect_error(expand_ex(ex))
})


test_that("expand_ex errs when end day before start day", {
  ex <- tribble(
    ~USUBJID, ~EXTRT,           ~EXSTDTC,     ~EXENDTC, ~EXSTDY, ~EXENDY,
    "1",         "A", "2025-01-01T07:00", "2025-01-03",       3,       1
  ) %>% lubrify_dates()

  expect_error(expand_ex(ex))
})
