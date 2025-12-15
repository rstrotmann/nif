test_that("resolve_duplicates works with default fields and mean function", {
  # Create test data with duplicates
  test_df <- tribble(
    ~ID, ~TIME, ~ANALYTE, ~DV,
    1,   0,     "A",      10,
    1,   0,     "A",      20,
    2,   1,     "B",      30,
    2,   1,     "B",      40,
    3,   2,     "C",      50,
    3,   2,     "C",      60,
    3,   2,     "C",      70
  )

  # Test with default fields and mean function
  result <- resolve_duplicates(test_df)

  # Should have one row per unique ID/TIME/ANALYTE combination
  expect_equal(nrow(result), 3)

  # Should apply mean to duplicate values
  expect_equal(result$DV, c(15, 35, 60))

  # Should maintain all columns
  expect_equal(names(result), names(test_df))
})

test_that("resolve_duplicates works with custom fields and sum function", {
  # Create test data with duplicates
  test_df <- tribble(
    ~ID, ~TIME, ~ANALYTE, ~VALUE,
    1,   0,     "A",      10,
    1,   0,     "A",      20,
    2,   1,     "B",      30,
    2,   1,     "B",      40,
    3,   2,     "C",      50,
    3,   2,     "C",      60,
    3,   2,     "C",      70
  )

  # Test with custom fields and sum function
  result <- resolve_duplicates(
    test_df, fields = c("ID", "TIME"), duplicate_function = sum,
    dependent_variable = "VALUE")

  # Should have one row per unique ID/TIME combination
  expect_equal(nrow(result), 3)

  # Should apply sum to duplicate values
  expect_equal(result$VALUE, c(30, 70, 180))
})

test_that("resolve_duplicates works with custom function to keep first value", {
  test_df <- tribble(
    ~ID, ~TIME, ~DV,
    1,   0,     10,
    1,   0,     20,
    2,   1,     30,
    2,   1,     40
  )

  result <- resolve_duplicates(
    test_df, fields = c("ID", "TIME"), duplicate_function = function(x) x[1])
  expect_equal(result$DV, c(10, 30))
})

test_that("resolve_duplicates works with custom function to keep last value", {
  test_df <- tribble(
    ~ID, ~TIME, ~DV,
    1,   0,     10,
    1,   0,     20,
    2,   1,     30,
    2,   1,     40
  )

  result <- resolve_duplicates(
    test_df, fields = c("ID", "TIME"),
    duplicate_function = function(x) x[length(x)])
  expect_equal(result$DV, c(20, 40))
})

test_that("resolve_duplicates works with custom function to keep max value", {
  test_df <- tribble(
    ~ID, ~TIME, ~DV,
    1,   0,     10,
    1,   0,     20,
    2,   1,     30,
    2,   1,     40
  )

  result <- resolve_duplicates(test_df, fields = c("ID", "TIME"),
                              duplicate_function = max)
  expect_equal(result$DV, c(20, 40))
})

test_that("resolve_duplicates handles non-existent fields", {
  test_df <- tribble(
    ~ID, ~TIME,
    1,   0,
    2,   1
  )

  # Should error when field doesn't exist
  expect_error(
    resolve_duplicates(test_df, fields = c("NONEXISTENT")),
    "The following fields do not exist in the data frame: NONEXISTENT"
  )
})

test_that("resolve_duplicates handles empty data frame", {
  empty_df <- tribble(
    ~ID, ~TIME, ~ANALYTE, ~DV
  )

  result <- resolve_duplicates(empty_df)
  expect_equal(nrow(result), 0)
  expect_equal(names(result), names(empty_df))
})

test_that("resolve_duplicates handles data frame with no duplicates", {
  no_dups_df <- tribble(
    ~ID, ~TIME, ~ANALYTE, ~DV,
    1,   1,     "A", 10,
    2,   2,     "B", 20,
    3,   3,     "C", 30
  )

  result <- resolve_duplicates(no_dups_df)
  expect_equal(nrow(result), 3)
  expect_equal(result, as.data.frame(no_dups_df))
})

test_that("resolve_duplicates handles NA values", {
  na_df <- tribble(
    ~ID, ~TIME, ~ANALYTE, ~DV,
    1,   0,     "A",      10,
    1,   0,     "A",      20,
    2,   NA,    "B",      30,
    2,   NA,    "B",      40
  )

  result <- resolve_duplicates(na_df)
  expect_equal(nrow(result), 2)  # NA values should be treated as a unique value
  expect_equal(result$DV, c(15, 35))  # Mean of values for each group
})

test_that("resolve_duplicates works with different data types", {
  test_df <- tribble(
    ~ID, ~TIME, ~DV,
    1,   "A",   TRUE,
    1,   "A",   FALSE,
    2,   "B",   TRUE,
    2,   "B",   FALSE
  )

  result <- resolve_duplicates(test_df, fields = c("ID", "TIME"))
  expect_equal(nrow(result), 2)
  expect_equal(result$DV, c(0.5, 0.5))  # Mean of TRUE/FALSE (1/0)

})

test_that("resolve_duplicates correctly handles NA values with na.rm=TRUE", {
  # Create test data with NA values
  test_df <- tibble::tribble(
    ~ID, ~TIME, ~DV, ~ANALYTE,
      1,     0, 100,      "A",
      1,     0,  NA,      "A",
      2,     1, 200,      "A",
      2,     1,  NA,      "A"
  )

  # With na.rm=TRUE, only non-NA values should be used
  result_na_rm_true <- resolve_duplicates(test_df, na.rm = TRUE)
  expect_equal(nrow(result_na_rm_true), 2)  # One row for each ID/TIME combo
  expect_equal(result_na_rm_true$DV[result_na_rm_true$TIME == 0], 100)
  expect_equal(result_na_rm_true$DV[result_na_rm_true$TIME == 1], 200)

  # With na.rm=FALSE and mean as duplicate_function, NAs should propagate
  result_na_rm_false <- resolve_duplicates(test_df, na.rm = FALSE)
  expect_equal(nrow(result_na_rm_false), 2)
  expect_true(is.na(result_na_rm_false$DV[result_na_rm_false$TIME == 0]))
  expect_true(is.na(result_na_rm_false$DV[result_na_rm_false$TIME == 1]))

  # Test with a custom function that can handle NAs
  na_safe_mean <- function(x, na.rm = TRUE) {
    return(mean(x, na.rm = na.rm))
  }

  result_custom <- resolve_duplicates(test_df, duplicate_function = na_safe_mean, na.rm = TRUE)
  expect_equal(nrow(result_custom), 2)
  expect_equal(result_custom$DV[result_custom$TIME == 0], 100)
  expect_equal(result_custom$DV[result_custom$TIME == 1], 200)
})


test_that("resolve_duplicates works with custom duplicate_identifiers", {
  # Create test data with duplicates
  test_df <- tribble(
    ~ID, ~TIME, ~ANALYTE, ~DV, ~NTIME, ~TRTDY,
    1,   0,     "A",      10, 0, 0,
    1,   0,     "A",      20, 0, 0,
    2,   1,     "B",      30, 1, 0,
    2,   1,     "B",      40, 1, 0,
    3,   2,     "C",      50, 2, 0,
    3,   2,     "C",      60, 2, 0,
    3,   2,     "C",      70, 2, 0,

    1,   0,     "A",      12, 0, 1,
    1,   0,     "A",      22, 0, 1,
    2,   1,     "B",      32, 1, 1,
    2,   1,     "B",      42, 1, 1,
    3,   2,     "C",      52, 2, 1,
    3,   2,     "C",      62, 2, 1,
    3,   2,     "C",      72, 2, 1
  )

  # Test with default fields and mean function
  result <- resolve_duplicates(test_df)
  expect_equal(nrow(result), 6)

  # Test with custom duplicate identification fields
  result <- resolve_duplicates(
    test_df, fields = c("ID", "ANALYTE", "NTIME", "TRTDY"))
  expect_equal(nrow(result), 6)

})
