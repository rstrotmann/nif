# Comprehensive tests for dose_levels()

test_that("dose_levels returns NULL for empty NIF", {
  empty_nif <- nif()
  result <- dose_levels(empty_nif)
  expect_null(result)
})


test_that("dose_levels returns NULL when no dosing events (all EVID = 0)", {
  dat <- tibble::tribble(
    ~ID,   ~TIME, ~AMT, ~CMT, ~EVID, ~DV,
    1,     0,     0,    1,    0,     0,
    1,     1,     0,    1,    0,     10,
    2,     0,     0,    1,    0,     0
  ) %>% nif()
  result <- dose_levels(dat)
  expect_null(result)
})


test_that("dose_levels returns one row for single subject, single analyte, one dose", {
  dat <- tibble::tribble(
    ~ID,   ~TIME, ~AMT, ~CMT, ~EVID, ~DV,
    1,     0,     100,  1,    1,     NA,
    1,     1,     0,    1,    0,     10,
    1,     2,     0,    1,    0,     20
  ) %>% nif()
  result <- dose_levels(dat)
  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 1)
  expect_equal(result$N, 1)
  expect_equal(result$CMT1, 100)
})


test_that("dose_levels uses first dose only per subject and analyte", {
  dat <- tibble::tribble(
    ~ID,   ~TIME, ~AMT, ~CMT, ~EVID, ~DV,
    1,     0,     100,  1,    1,     NA,
    1,     24,    200,  1,    1,     NA,
    1,     48,    200,  1,    1,     NA,
    1,     1,     0,    1,    0,     10
  ) %>% nif()
  result <- dose_levels(dat)
  expect_equal(nrow(result), 1)
  expect_equal(result$CMT1, 100)
  expect_equal(result$N, 1)
})


test_that("dose_levels aggregates multiple subjects at same dose", {
  dat <- tibble::tribble(
    ~ID,   ~TIME, ~AMT, ~CMT, ~EVID, ~DV,
    1,     0,     50,   1,    1,     NA,
    1,     1,     0,    1,    0,     5,
    2,     0,     50,   1,    1,     NA,
    2,     1,     0,    1,    0,     6,
    3,     0,     50,   1,    1,     NA,
    3,     1,     0,    1,    0,     7
  ) %>% nif()
  result <- dose_levels(dat)
  expect_equal(nrow(result), 1)
  expect_equal(result$N, 3)
  expect_equal(result$CMT1, 50)
})


test_that("dose_levels returns multiple rows for multiple dose levels", {
  dat <- tibble::tribble(
    ~ID,   ~TIME, ~AMT, ~CMT, ~EVID, ~DV,
    1,     0,     25,   1,    1,     NA,
    1,     1,     0,    1,    0,     2,
    2,     0,     50,   1,    1,     NA,
    2,     1,     0,    1,    0,     5,
    3,     0,     100,  1,    1,     NA,
    3,     1,     0,    1,    0,     10
  ) %>% nif()
  result <- dose_levels(dat)
  expect_equal(nrow(result), 3)
  expect_equal(sort(result$CMT1), c(25, 50, 100))
  expect_equal(result$N, c(1, 1, 1))
})


test_that("dose_levels with multiple analytes pivots to one column per analyte", {
  dat <- tibble::tribble(
    ~ID,   ~TIME, ~AMT, ~CMT, ~EVID, ~DV,  ~ANALYTE,
    1,     0,     100,  1,    1,     NA,   "DRUG",
    1,     0,     0,    2,    1,     NA,   "MET",
    1,     1,     0,    1,    0,     10,   "DRUG",
    1,     1,     0,    2,    0,     2,    "MET"
  ) %>% nif()
  result <- dose_levels(dat)
  expect_equal(nrow(result), 1)
  expect_equal(result$N, 1)
  expect_true("DRUG" %in% names(result))
  expect_true("MET" %in% names(result))
  expect_equal(result$DRUG, 100)
  expect_equal(result$MET, 0)
})


test_that("dose_levels with group adds grouping columns", {
  dat <- tibble::tribble(
    ~ID,   ~TIME, ~AMT, ~CMT, ~EVID, ~DV,  ~SEX,
    1,     0,     50,   1,    1,     NA,   "M",
    1,     1,     0,    1,    0,     5,    "M",
    2,     0,     50,   1,    1,     NA,   "F",
    2,     1,     0,    1,    0,     6,    "F",
    3,     0,     50,   1,    1,     NA,   "M",
    3,     1,     0,    1,    0,     7,    "M"
  ) %>% nif()
  result <- dose_levels(dat, group = "SEX")
  expect_equal(nrow(result), 2)
  expect_true("SEX" %in% names(result))
  expect_true("N" %in% names(result))
  expect_equal(sort(result$SEX), c("F", "M"))
  expect_equal(result$N[result$SEX == "M"], 2)
  expect_equal(result$N[result$SEX == "F"], 1)
})


test_that("dose_levels with multiple group columns", {
  dat <- tibble::tribble(
    ~ID,   ~TIME, ~AMT, ~CMT, ~EVID, ~DV,  ~SEX,   ~FASTED,
    1,     0,     50,   1,    1,     NA,   "M",    "Y",
    1,     1,     0,    1,    0,     5,    "M",    "Y",
    2,     0,     50,   1,    1,     NA,   "M",    "N",
    2,     1,     0,    1,    0,     6,    "M",    "N",
    3,     0,     50,   1,    1,     NA,   "F",    "Y",
    3,     1,     0,    1,    0,     7,    "F",    "Y"
  ) %>% nif()
  result <- dose_levels(dat, group = c("SEX", "FASTED"))
  expect_equal(nrow(result), 3)
  expect_true("SEX" %in% names(result))
  expect_true("FASTED" %in% names(result))
  expect_equal(sum(result$N), 3)
})


test_that("dose_levels errors when group column is missing", {
  dat <- tibble::tribble(
    ~ID,   ~TIME, ~AMT, ~CMT, ~EVID, ~DV,
    1,     0,     100,  1,    1,     NA,
    1,     1,     0,    1,    0,     10
  ) %>% nif()
  expect_error(dose_levels(dat, group = "SEX"), "Required.*missing")
})


test_that("dose_levels errors on invalid cmt type", {
  dat <- tibble::tribble(
    ~ID,   ~TIME, ~AMT, ~CMT, ~EVID, ~DV,
    1,     0,     100,  1,    1,     NA,
    1,     1,     0,    1,    0,     10
  ) %>% nif()
  expect_error(dose_levels(dat, cmt = "x"), "numeric")
})


test_that("dose_levels errors when group is not character", {
  dat <- tibble::tribble(
    ~ID,   ~TIME, ~AMT, ~CMT, ~EVID, ~DV,
    1,     0,     100,  1,    1,     NA,
    1,     1,     0,    1,    0,     10
  ) %>% nif()
  expect_error(dose_levels(dat, group = 1), "character")
})


test_that("dose_levels errors on non-nif input", {
  dat <- tibble::tribble(
    ~ID,   ~TIME, ~AMT, ~CMT, ~EVID, ~DV,
    1,     0,     100,  1,    1,     NA,
    1,     1,     0,    1,    0,     10
  )
  expect_error(dose_levels(dat), "nif object")
})


test_that("dose_levels returns data.frame", {
  dat <- tibble::tribble(
    ~ID,   ~TIME, ~AMT, ~CMT, ~EVID, ~DV,
    1,     0,     100,  1,    1,     NA,
    1,     1,     0,    1,    0,     10
  ) %>% nif()
  result <- dose_levels(dat)
  expect_true(is.data.frame(result))
  expect_false(inherits(result, "tbl_df"))
})


test_that("dose_levels with group NULL works as default", {
  dat <- tibble::tribble(
    ~ID,   ~TIME, ~AMT, ~CMT, ~EVID, ~DV,
    1,     0,     100,  1,    1,     NA,
    1,     1,     0,    1,    0,     10
  ) %>% nif()
  result <- dose_levels(dat, group = NULL)
  expect_equal(nrow(result), 1)
  expect_equal(result$N, 1)
  expect_equal(result$CMT1, 100)
})


test_that("dose_levels preserves existing ANALYTE when present", {
  dat <- tibble::tribble(
    ~ID,   ~TIME, ~AMT, ~CMT, ~EVID, ~DV,  ~ANALYTE,
    1,     0,     100,  1,    1,     NA,   "EXAMPLINIB",
    1,     1,     0,    1,    0,     10,   "EXAMPLINIB"
  ) %>% nif()
  result <- dose_levels(dat)
  expect_true("EXAMPLINIB" %in% names(result))
  expect_equal(result$EXAMPLINIB, 100)
  expect_equal(result$N, 1)
})


test_that("dose_levels same dose different groups gives separate rows", {
  dat <- tibble::tribble(
    ~ID,   ~TIME, ~AMT, ~CMT, ~EVID, ~DV,  ~COHORT,
    1,     0,     100,  1,    1,     NA,   "A",
    1,     1,     0,    1,    0,     10,   "A",
    2,     0,     100,  1,    1,     NA,   "B",
    2,     1,     0,    1,    0,     12,   "B"
  ) %>% nif()
  result <- dose_levels(dat, group = "COHORT")
  expect_equal(nrow(result), 2)
  expect_equal(result$N, c(1, 1))
  expect_equal(sort(result$COHORT), c("A", "B"))
  expect_equal(result$CMT1, c(100, 100))
})


test_that("dose_levels first dose per subject uses minimum TIME within group", {
  dat <- tibble::tribble(
    ~ID,   ~TIME, ~AMT, ~CMT, ~EVID, ~DV,
    1,     0,     100,  1,    1,     NA,
    1,     12,    50,   1,    1,     NA,
    1,     24,    50,   1,    1,     NA,
    1,     1,     0,    1,    0,     10
  ) %>% nif()
  result <- dose_levels(dat)
  expect_equal(nrow(result), 1)
  expect_equal(result$CMT1, 100)
})


test_that("dose_levels works with package data examplinib_sad_nif", {
  result <- dose_levels(examplinib_sad_nif)
  expect_gt(length(result), 0)
  expect_true("N" %in% names(result))
})


test_that("dose_levels works with package data examplinib_poc_min_nif", {
  expect_no_error(dose_levels(examplinib_poc_min_nif))
})

