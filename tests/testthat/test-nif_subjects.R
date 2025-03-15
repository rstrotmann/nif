test_that("calculate_age calculates age correctly", {
  test_df <- tibble::tribble(
    ~USUBJID,     ~BRTHDTC,     ~RFICDTC,
       "001", "1980-01-15", "2020-01-15",
       "002", "1990-05-20", "2020-05-20",
       "003", "2000-12-10", "2020-12-10"
  )

  result <- calculate_age(test_df)

  # Test that ages are calculated correctly (40, 30, 20 years)
  expect_equal(result$AGE, c(40, 30, 20))
})


test_that("calculate_age preserves existing AGE values when not NA", {
  test_df <- tibble::tribble(
    ~USUBJID,     ~BRTHDTC,     ~RFICDTC, ~AGE,
       "001", "1980-01-15", "2020-01-15",   41,
       "002", "1990-05-20", "2020-05-20",   NA,
       "003", "2000-12-10", "2020-12-10",   19
  )

  result <- calculate_age(test_df, preserve_age = TRUE)

  # Test that existing non-NA values are preserved, but NA are filled
  expect_equal(result$AGE, c(41, 30, 19))
})


test_that("calculate_age overwrites existing AGE values when preserve_age = FALSE", {
  test_df <- tibble::tribble(
    ~USUBJID,     ~BRTHDTC,     ~RFICDTC, ~AGE,
       "001", "1980-01-15", "2020-01-15",   41,
       "002", "1990-05-20", "2020-05-20",   NA,
       "003", "2000-12-10", "2020-12-10",   19
  )

  result <- calculate_age(test_df, preserve_age = FALSE)

  # Test that all values are calculated, regardless of existing values
  expect_equal(result$AGE, c(40, 30, 20))
})


test_that("calculate_age uses custom reference date column", {
  test_df <- tibble::tribble(
    ~USUBJID,     ~BRTHDTC,  ~CUSTOM_DTC,
       "001", "1980-01-15", "2030-01-15",
       "002", "1990-05-20", "2030-05-20"
  )

  result <- calculate_age(test_df, ref_date_col = "CUSTOM_DTC")

  # Test that ages are calculated correctly using the custom column (50, 40 years)
  expect_equal(result$AGE, c(50, 40))
})


test_that("calculate_age returns dataframe unchanged when required columns missing", {
  # Missing BRTHDTC
  test_df1 <- tibble::tribble(
    ~USUBJID,     ~RFICDTC,
       "001", "2020-01-15",
       "002", "2020-05-20"
  )

  # Missing RFICDTC
  test_df2 <- tibble::tribble(
    ~USUBJID,     ~BRTHDTC,
       "001", "1980-01-15",
       "002", "1990-05-20"
  )

  # Test that dataframe is returned unchanged
  expect_identical(calculate_age(test_df1), test_df1)
  expect_identical(calculate_age(test_df2), test_df2)
})


test_that("calculate_age handles non-dataframe input", {
  expect_error(calculate_age("not a dataframe"), "Input must be a data frame")
  expect_error(calculate_age(list(a = 1, b = 2)), "Input must be a data frame")
})


test_that("calculate_age rounds age correctly", {
  # Create test data with exact and partial years
  test_df <- tibble::tribble(
    ~USUBJID,     ~BRTHDTC,     ~RFICDTC,
       "001", "1980-01-15", "2020-01-14",
       "002", "1990-06-01", "2020-05-31",
       "003", "2000-01-01", "2020-07-01"
  )

  result <- calculate_age(test_df)

  # Test that ages are rounded correctly
  # 40 years minus 1 day = 40 (rounded)
  # 30 years minus 1 day = 30 (rounded)
  # 20 years plus 6 months = 20 (rounded)
  expect_equal(result$AGE, c(40, 30, 20))
})


make_test_sdtm <- function() {
  dm <- tibble::tribble(
    ~USUBJID, ~DOMAIN, ~SEX,   ~ACTARMCD,          ~RFXSTDTC,
    "1", "DM", "M", "TREATMENT", "2001-01-01T10:29",
    "2", "DM", "M", "TREATMENT", "2001-01-02T09:09",
    "3", "DM", "M", "TREATMENT", "2000-12-29T09:07",
    "4", "DM", "F", "TREATMENT", "2001-01-06T11:18"
  ) %>%
    mutate(RFSTDTC = RFXSTDTC)
  vs <- tibble::tribble(
    ~USUBJID, ~DOMAIN, ~VSTESTCD, ~VSBLFL, ~VSSTRESN,
    "1", "VS", "HEIGHT",     "Y",     190.8,
    "1", "VS", "WEIGHT",     "Y",      79.3,
    "2", "VS", "HEIGHT",     "Y",     199.5,
    "2", "VS", "WEIGHT",     "Y",      81.6,
    "3", "VS", "HEIGHT",     "Y",     185.4,
    "3", "VS", "WEIGHT",     "Y",      92.8,
    "4", "VS", "HEIGHT",     "Y",     177.8,
    "4", "VS", "WEIGHT",     "Y",      83.3
  )
  lb <- tibble::tribble(
    ~USUBJID, ~DOMAIN, ~LBSPEC, ~LBBLFL, ~LBTESTCD,        ~LBSTRESN,
    "1",    "DM", "SERUM",     "Y",   "CREAT", 89.2690855827183,
    "2",    "DM", "SERUM",     "Y",   "CREAT", 73.3255705088018,
    "3",    "DM", "SERUM",     "Y",   "CREAT", 77.8168976104201,
    "4",    "DM", "SERUM",     "Y",   "CREAT", 66.8305453780658
  )
  temp <- list(
    dm = dm,
    vs = vs,
    lb = lb
  )
  return(list(domains = temp))
}


test_that("make subjects", {
  sdtm <- make_test_sdtm()$domains
  expect_no_error(
    test <- make_subjects(sdtm$dm, sdtm$vs)
  )
  expect_equal(dim(test), c(4, 9))
})


test_that("make_subject works with different age definitions", {
  dm <- tibble::tribble(
    ~USUBJID, ~SEX, ~ACTARMCD,
    1, 0, "A",
    2, 1, "A",
    3, 0, "B",
    4, 1, "B"
  )

  make_subjects(dm)

  temp <- make_subjects(
    dm %>%
      mutate(RFICDTC = now()) %>%
      mutate(BRTHDTC = now() - years(50 - as.numeric(USUBJID)))
  )
  expect_equal(temp$AGE, 50 - temp$USUBJID)

  temp <- make_subjects(
    dm %>%
      mutate(RFICDTC = now()) %>%
      mutate(BRTHDTC = now() - years(50 - as.numeric(USUBJID))) %>%
      mutate(AGE = c(30, 20, NA, NA))
  )
  expect_equal(temp$AGE, c(30, 20, 47, 46))
})


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


test_that("BMI calculation handles edge cases correctly", {
  # Create test data with various edge cases
  test_dm <- tibble::tribble(
    ~USUBJID, ~SEX, ~ACTARMCD, ~RFSTDTC,
    "001", "M", "TRT", "2023-01-01",
    "002", "F", "TRT", "2023-01-01",
    "003", "M", "TRT", "2023-01-01",
    "004", "F", "TRT", "2023-01-01",
    "005", "M", "TRT", "2023-01-01",
    "006", "F", "TRT", "2023-01-01"
  )

  test_vs <- tibble::tribble(
    ~USUBJID, ~VSTESTCD, ~VSSTRESN, ~VSBLFL, ~VSDTC,
    # Normal case - should calculate BMI correctly
    "001", "HEIGHT", 170, "Y", "2022-12-20",
    "001", "WEIGHT", 70, "Y", "2022-12-20",

    # Missing HEIGHT - should result in NA BMI
    "002", "WEIGHT", 65, "Y", "2022-12-20",

    # Missing WEIGHT - should result in NA BMI
    "003", "HEIGHT", 180, "Y", "2022-12-20",

    # Zero HEIGHT - should result in NA BMI (would cause division by zero)
    "004", "HEIGHT", 0, "Y", "2022-12-20",
    "004", "WEIGHT", 75, "Y", "2022-12-20",

    # Negative HEIGHT - should result in NA BMI (physically impossible)
    "005", "HEIGHT", -10, "Y", "2022-12-20",
    "005", "WEIGHT", 80, "Y", "2022-12-20",

    # Zero WEIGHT - should result in NA BMI
    "006", "HEIGHT", 165, "Y", "2022-12-20",
    "006", "WEIGHT", 0, "Y", "2022-12-20"
  )

  # Run make_subjects with our test data
  result <- make_subjects(test_dm, test_vs)

  # Check that the results are as expected
  expect_equal(nrow(result), 6) # "Should return all 6 subjects")

  # Test normal case - BMI calculation should work
  normal_bmi <- 70 / (170/100)^2  # Expected BMI for subject 001
  expect_equal(result$BMI[result$USUBJID == "001"], normal_bmi)

  # Test all edge cases - BMI should be NA
  expect_true(is.na(result$BMI[result$USUBJID == "002"]), "Missing HEIGHT should result in NA BMI")
  expect_true(is.na(result$BMI[result$USUBJID == "003"]), "Missing WEIGHT should result in NA BMI")
  expect_true(is.na(result$BMI[result$USUBJID == "004"]), "Zero HEIGHT should result in NA BMI")
  expect_true(is.na(result$BMI[result$USUBJID == "005"]), "Negative HEIGHT should result in NA BMI")
  expect_true(is.na(result$BMI[result$USUBJID == "006"]), "Zero WEIGHT should result in NA BMI")
})

