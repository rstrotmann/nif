library(testthat)
library(dplyr)
library(tibble)

test_that("make_subjects validates inputs correctly", {
  # Test with non-data frame input
  expect_error(
    make_subjects(list(a = 1, b = 2)),
    "The 'dm' parameter must be a data frame",
    fixed = TRUE
  )
  
  # Test with missing required columns in dm
  incomplete_dm <- tibble::tribble(
    ~USUBJID, ~ACTARMCD,
    "001", "TRT"
  )
  
  expect_error(
    make_subjects(incomplete_dm),
    "The following required columns are missing from the 'dm' data frame: SEX",
    fixed = TRUE
  )
  
  # Test with incomplete vs data
  valid_dm <- tibble::tribble(
    ~USUBJID, ~SEX, ~ACTARMCD,
    "001", "M", "TRT"
  )
  
  incomplete_vs <- tibble::tribble(
    ~USUBJID, ~VSSTRESN,
    "001", 170
  )
  
  expect_error(
    make_subjects(valid_dm, incomplete_vs),
    "The following required columns are missing from the 'vs' data frame: VSTESTCD",
    fixed = TRUE
  )
  
  # Test with non-data frame vs
  expect_error(
    make_subjects(valid_dm, vs = list(a = 1)),
    "The 'vs' parameter must be a data frame or NULL",
    fixed = TRUE
  )
  
  # Test with missing RFSTDTC when needed
  valid_dm_no_rfstdtc <- tibble::tribble(
    ~USUBJID, ~SEX, ~ACTARMCD,
    "001", "M", "TRT"
  )
  
  vs_no_blfl <- tibble::tribble(
    ~USUBJID, ~VSTESTCD, ~VSSTRESN, ~VSDTC,
    "001", "HEIGHT", 170, "2022-12-20"
  )
  
  expect_error(
    make_subjects(valid_dm_no_rfstdtc, vs_no_blfl),
    "When 'VSBLFL' is not available in vs, 'RFSTDTC' must be present in dm for baseline determination",
    fixed = TRUE
  )
}) 