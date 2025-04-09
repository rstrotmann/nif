# context("Remove Duplicates")

test_that("remove_duplicates works with default fields and mean function", {
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
  result <- remove_duplicates(test_df)

  # Should have one row per unique ID/TIME/ANALYTE combination
  expect_equal(nrow(result), 3)

  # Should apply mean to duplicate values
  expect_equal(result$DV, c(15, 35, 60))

  # Should maintain all columns
  expect_equal(names(result), names(test_df))
})

test_that("remove_duplicates works with custom fields and sum function", {
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
  result <- remove_duplicates(
    test_df, fields = c("ID", "TIME"), duplicate_function = sum,
    dependent_variable = "VALUE")

  # Should have one row per unique ID/TIME combination
  expect_equal(nrow(result), 3)

  # Should apply sum to duplicate values
  expect_equal(result$VALUE, c(30, 70, 180))
})

test_that("remove_duplicates works with custom function to keep first value", {
  test_df <- tribble(
    ~ID, ~TIME, ~DV,
    1,   0,     10,
    1,   0,     20,
    2,   1,     30,
    2,   1,     40
  )

  result <- remove_duplicates(
    test_df, fields = c("ID", "TIME"), duplicate_function = function(x) x[1])
  expect_equal(result$DV, c(10, 30))
})

test_that("remove_duplicates works with custom function to keep last value", {
  test_df <- tribble(
    ~ID, ~TIME, ~DV,
    1,   0,     10,
    1,   0,     20,
    2,   1,     30,
    2,   1,     40
  )

  result <- remove_duplicates(
    test_df, fields = c("ID", "TIME"),
    duplicate_function = function(x) x[length(x)])
  expect_equal(result$DV, c(20, 40))
})

test_that("remove_duplicates works with custom function to keep max value", {
  test_df <- tribble(
    ~ID, ~TIME, ~DV,
    1,   0,     10,
    1,   0,     20,
    2,   1,     30,
    2,   1,     40
  )

  result <- remove_duplicates(test_df, fields = c("ID", "TIME"),
                              duplicate_function = max)
  expect_equal(result$DV, c(20, 40))
})

test_that("remove_duplicates handles non-existent fields", {
  test_df <- tribble(
    ~ID, ~TIME,
    1,   0,
    2,   1
  )

  # Should error when field doesn't exist
  expect_error(
    remove_duplicates(test_df, fields = c("NONEXISTENT")),
    "The following fields do not exist in the data frame: NONEXISTENT"
  )
})

test_that("remove_duplicates handles empty data frame", {
  empty_df <- tribble(
    ~ID, ~TIME, ~ANALYTE, ~DV
  )

  result <- remove_duplicates(empty_df)
  expect_equal(nrow(result), 0)
  expect_equal(names(result), names(empty_df))
})

test_that("remove_duplicates handles data frame with no duplicates", {
  no_dups_df <- tribble(
    ~ID, ~TIME, ~ANALYTE, ~DV,
    1,   1,     "A", 10,
    2,   2,     "B", 20,
    3,   3,     "C", 30
  )

  result <- remove_duplicates(no_dups_df)
  expect_equal(nrow(result), 3)
  expect_equal(result, as.data.frame(no_dups_df))
})

test_that("remove_duplicates handles NA values", {
  na_df <- tribble(
    ~ID, ~TIME, ~ANALYTE, ~DV,
    1,   0,     "A",      10,
    1,   0,     "A",      20,
    2,   NA,    "B",      30,
    2,   NA,    "B",      40
  )

  result <- remove_duplicates(na_df)
  expect_equal(nrow(result), 2)  # NA values should be treated as a unique value
  expect_equal(result$DV, c(15, 35))  # Mean of values for each group
})

test_that("remove_duplicates works with different data types", {
  test_df <- tribble(
    ~ID, ~TIME, ~DV,
    1,   "A",   TRUE,
    1,   "A",   FALSE,
    2,   "B",   TRUE,
    2,   "B",   FALSE
  )

  result <- remove_duplicates(test_df, fields = c("ID", "TIME"))
  expect_equal(nrow(result), 2)
  expect_equal(result$DV, c(0.5, 0.5))  # Mean of TRUE/FALSE (1/0)

})
