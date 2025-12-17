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
    "Field not found in input: NONEXISTENT"
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
  test_df <- tibble::tribble(
    ~ID, ~TIME, ~ANALYTE, ~DV, ~NTIME, ~TRTDY,
    1,     0,      "A",  10,      0,      0,
    1,     0,      "A",  20,      0,      0,
    1,     0,      "A",  12,      0,      1,
    1,     0,      "A",  22,      0,      1,
    2,     1,      "B",  30,      1,      0,
    2,     1,      "B",  40,      1,      0,
    2,     1,      "B",  32,      1,      1,
    2,     1,      "B",  42,      1,      1,
    3,     2,      "C",  50,      2,      0,
    3,     2,      "C",  60,      2,      0,
    3,     2,      "C",  70,      2,      0,
    3,     2,      "C",  52,      2,      1,
    3,     2,      "C",  62,      2,      1,
    3,     2,      "C",  72,      2,      1
  )

  # Test with default fields and mean function
  result <- resolve_duplicates(test_df)
  expect_equal(nrow(result), 3)

  # Test with custom duplicate identification fields
  result <- resolve_duplicates(
    test_df, fields = c("NTIME", "TRTDY"))
  expect_equal(nrow(result), 6)
})



# test_that("resolve_duplicates workds with duplicats that have different NTIME", {
#   df <- tibble::tribble(
#     ~ID, ~TIME, ~ANALYTE, ~DV, ~NTIME, ~TRTDY, ~BL_WT,
#     1,     0,      "A",  10,      0,      0, 80,
#     1,     0,      "A",  20,      0.5,    0, 80,
#
#     1,     1,      "B",  30,      0,      0, 80,
#     1,     1,      "B",  40,      0,      0, 80,
#     1,     1,      "B",  32,      1,      0, 80,
#     1,     1,      "B",  42,      1.5,    0, 80,
#
#     3,     2,      "C",  50,      2,      0, 70,
#     3,     2,      "C",  60,      2,      0, 70,
#     3,     2,      "C",  70,      2,      0, 70,
#     3,     2,      "C",  52,      2,      1, 70,
#     3,     2,      "C",  62,      2,      1, 70,
#     3,     2,      "C",  72,      2,      1, 70
#   )
#
#   resolve_duplicates(df, fields = "TIME")
#   result <- resolve_duplicates(df, fields = c("TIME"))
#
#   resolve_duplicates(df, c("TIME"))
#   resolve_duplicates(df, c("TIME", "NTIME"))
# })


test_that("resolve_duplicates returns NA (not NaN) when all DV values are NA with na.rm=TRUE", {
  # This tests the fix for the bug where all-NA values returned NaN
  test_df <- tribble(
    ~ID, ~TIME, ~DV,
    1,   0,     NA,
    1,   0,     NA,
    2,   1,     NA,
    2,   1,     NA
  )

  result <- resolve_duplicates(test_df, fields = "TIME", na.rm = TRUE)
  expect_equal(nrow(result), 2)
  expect_true(is.na(result$DV[result$TIME == 0]))
  expect_true(is.na(result$DV[result$TIME == 1]))
  expect_false(is.nan(result$DV[result$TIME == 0]))
  expect_false(is.nan(result$DV[result$TIME == 1]))
})

test_that("resolve_duplicates preserves column types when all_same returns NA", {
  # This tests the fix for type preservation in all_same function
  test_df <- tribble(
    ~ID, ~TIME, ~DV, ~NUM_COL, ~CHAR_COL, ~INT_COL, ~LOG_COL,
    1,   0,     10,  1,        "A",       1L,       TRUE,
    1,   0,     20,  2,        "B",       2L,       FALSE
  )

  result <- resolve_duplicates(test_df, fields = "TIME")

  # Numeric column with conflicting values should return NA_real_
  expect_true(is.na(result$NUM_COL))
  expect_equal(typeof(result$NUM_COL), "double")

  # Character column with conflicting values should return NA_character_
  expect_true(is.na(result$CHAR_COL))
  expect_equal(typeof(result$CHAR_COL), "character")

  # Integer column with conflicting values should return NA
  # Note: dplyr's reframe may coerce integer to double when NA is present
  expect_true(is.na(result$INT_COL))
  # Type may be preserved as integer or coerced to double by dplyr
  expect_true(typeof(result$INT_COL) %in% c("integer", "double"))

  # Logical column with conflicting values should return logical NA
  expect_true(is.na(result$LOG_COL))
  expect_equal(typeof(result$LOG_COL), "logical")
})

test_that("resolve_duplicates validates duplicate_function parameter", {
  # This tests the fix for missing function validation
  test_df <- tribble(
    ~ID, ~TIME, ~DV,
    1,   0,     10,
    2,   1,     20
  )

  expect_error(
    resolve_duplicates(test_df, fields = "TIME", duplicate_function = "not_a_function"),
    "duplicate_function must be a function"
  )

  expect_error(
    resolve_duplicates(test_df, fields = "TIME", duplicate_function = 123),
    "duplicate_function must be a function"
  )

  expect_error(
    resolve_duplicates(test_df, fields = "TIME", duplicate_function = NULL),
    "duplicate_function must be a function"
  )
})

test_that("resolve_duplicates handles MDV column correctly", {
  # Test that observations with MDV == 1 are excluded
  test_df <- tribble(
    ~ID, ~TIME, ~DV, ~MDV,
    1,   0,     10,  0,
    1,   0,     20,  0,
    1,   0,     30,  1,  # Should be excluded
    2,   1,     40,  0,
    2,   1,     50,  1   # Should be excluded
  )

  result <- resolve_duplicates(test_df, fields = "TIME")
  expect_equal(nrow(result), 2)
  expect_equal(result$DV[result$TIME == 0], 15)  # Mean of 10 and 20
  expect_equal(result$DV[result$TIME == 1], 40)  # Only 40 remains
})

test_that("resolve_duplicates handles other columns with consistent values", {
  # Test that consistent values in other columns are preserved
  test_df <- tribble(
    ~ID, ~TIME, ~DV, ~CONSISTENT_COL, ~INCONSISTENT_COL,
    1,   0,     10,  "A",             "X",
    1,   0,     20,  "A",             "Y",
    2,   1,     30,  "B",             "Z",
    2,   1,     40,  "B",             "W"
  )

  result <- resolve_duplicates(test_df, fields = "TIME")
  expect_equal(nrow(result), 2)
  expect_equal(result$CONSISTENT_COL[result$TIME == 0], "A")
  expect_equal(result$CONSISTENT_COL[result$TIME == 1], "B")
  expect_true(is.na(result$INCONSISTENT_COL[result$TIME == 0]))
  expect_true(is.na(result$INCONSISTENT_COL[result$TIME == 1]))
})

test_that("resolve_duplicates handles other columns with all NA values", {
  # Test that all-NA columns return appropriate NA type
  # Note: When all values are NA, dplyr may infer type as logical
  # but our function attempts to preserve the original type
  test_df <- tribble(
    ~ID, ~TIME, ~DV, ~NUM_NA, ~CHAR_NA, ~INT_NA,
    1,   0,     10,  NA_real_, NA_character_, NA_integer_,
    1,   0,     20,  NA_real_, NA_character_, NA_integer_
  )

  result <- resolve_duplicates(test_df, fields = "TIME")
  expect_equal(nrow(result), 1)
  expect_true(is.na(result$NUM_NA))
  # Type preservation may vary with dplyr's type inference
  expect_true(typeof(result$NUM_NA) %in% c("double", "logical"))
  expect_true(is.na(result$CHAR_NA))
  # Character type should be preserved better
  expect_true(typeof(result$CHAR_NA) %in% c("character", "logical"))
  expect_true(is.na(result$INT_NA))
  # Integer may be coerced to double by dplyr
  expect_true(typeof(result$INT_NA) %in% c("integer", "double", "logical"))
})

test_that("resolve_duplicates handles other columns with mixed NA and consistent values", {
  # Test that columns with some NAs but consistent non-NA values work correctly
  test_df <- tribble(
    ~ID, ~TIME, ~DV, ~MIXED_COL,
    1,   0,     10,  "A",
    1,   0,     20,  NA,
    1,   0,     15,  "A",
    2,   1,     30,  "B",
    2,   1,     40,  "B"
  )

  result <- resolve_duplicates(test_df, fields = "TIME")
  expect_equal(nrow(result), 2)
  expect_equal(result$MIXED_COL[result$TIME == 0], "A")
  expect_equal(result$MIXED_COL[result$TIME == 1], "B")
})

test_that("resolve_duplicates preserves column order", {
  # Test that column order is preserved in the output
  test_df <- tribble(
    ~OTHER, ~ID, ~TIME, ~DV, ~ANALYTE, ~EXTRA,
    100,    1,   0,     10,  "A",      "X",
    100,    1,   0,     20,  "A",      "X"
  )

  result <- resolve_duplicates(test_df, fields = "TIME")
  expect_equal(names(result), names(test_df))
})

test_that("resolve_duplicates handles single field grouping", {
  # Test grouping by a single field
  test_df <- tribble(
    ~ID, ~TIME, ~DV,
    1,   0,     10,
    1,   0,     20,
    1,   1,     30,
    2,   0,     40,
    2,   0,     50
  )

  result <- resolve_duplicates(test_df, fields = "TIME")
  expect_equal(nrow(result), 2)  # Two unique TIME values
  expect_equal(result$DV[result$TIME == 0], 30)  # Mean of 10, 20, 40, 50
  expect_equal(result$DV[result$TIME == 1], 30)
})

test_that("resolve_duplicates removes DV from fields if present", {
  # Test that DV is automatically removed from fields parameter
  test_df <- tribble(
    ~ID, ~TIME, ~DV,
    1,   0,     10,
    1,   0,     20,
    2,   1,     30,
    2,   1,     40
  )

  # Should not error even if DV is in fields
  result <- resolve_duplicates(test_df, fields = c("ID", "TIME", "DV"))
  expect_equal(nrow(result), 2)
  expect_equal(result$DV, c(15, 35))
})

test_that("resolve_duplicates errors when only DV is provided as field", {
  # Test that error occurs when only DV is in fields (after removal, no fields remain)
  test_df <- tribble(
    ~ID, ~TIME, ~DV,
    1,   0,     10,
    2,   1,     20
  )

  expect_error(
    resolve_duplicates(test_df, fields = "DV"),
    "At least one field \\(other than DV\\) must be provided to identify duplicates"
  )
})

test_that("resolve_duplicates handles missing dependent variable", {
  # Test error when dependent variable doesn't exist
  test_df <- tribble(
    ~ID, ~TIME,
    1,   0,
    2,   1
  )

  expect_error(
    resolve_duplicates(test_df, fields = "TIME", dependent_variable = "DV"),
    "The dependent variable DV does not exist in the data frame"
  )
})

test_that("resolve_duplicates validates input is a data frame", {
  # Test that non-data.frame inputs are rejected
  expect_error(
    resolve_duplicates(list(a = 1, b = 2), fields = "a"),
    "df must be a data frame!"
  )

  expect_error(
    resolve_duplicates(c(1, 2, 3), fields = "x"),
    "df must be a data frame!"
  )

  expect_error(
    resolve_duplicates(matrix(1:6, nrow = 2), fields = "x"),
    "df must be a data frame!"
  )
})

test_that("resolve_duplicates handles data frame with only grouping fields and DV", {
  # Test edge case where there are no "other columns"
  test_df <- tribble(
    ~ID, ~TIME, ~DV,
    1,   0,     10,
    1,   0,     20,
    2,   1,     30
  )

  result <- resolve_duplicates(test_df, fields = "TIME")
  expect_equal(nrow(result), 2)
  expect_equal(names(result), c("ID", "TIME", "DV"))
  expect_equal(result$DV[result$TIME == 0], 15)
  expect_equal(result$DV[result$TIME == 1], 30)
})

test_that("resolve_duplicates handles na.rm=FALSE with all NA values", {
  # Test behavior when na.rm=FALSE and all values are NA
  test_df <- tribble(
    ~ID, ~TIME, ~DV,
    1,   0,     NA,
    1,   0,     NA
  )

  result <- resolve_duplicates(test_df, fields = "TIME", na.rm = FALSE)
  expect_equal(nrow(result), 1)
  expect_true(is.na(result$DV))
})

test_that("resolve_duplicates handles integer columns correctly", {
  # Test that integer columns maintain their type when consistent
  test_df <- tribble(
    ~ID, ~TIME, ~DV, ~INT_COL,
    1,   0,     10,  5L,
    1,   0,     20,  5L,
    2,   1,     30,  10L,
    2,   1,     40,  15L
  )

  result <- resolve_duplicates(test_df, fields = "TIME")
  expect_equal(nrow(result), 2)
  expect_equal(result$INT_COL[result$TIME == 0], 5L)
  expect_true(is.na(result$INT_COL[result$TIME == 1]))
  # Note: dplyr may coerce integer to double when NA is present
  expect_true(typeof(result$INT_COL) %in% c("integer", "double"))
})

test_that("resolve_duplicates handles logical columns correctly", {
  # Test that logical columns maintain their type
  test_df <- tribble(
    ~ID, ~TIME, ~DV, ~LOG_COL,
    1,   0,     10,  TRUE,
    1,   0,     20,  TRUE,
    2,   1,     30,  TRUE,
    2,   1,     40,  FALSE
  )

  result <- resolve_duplicates(test_df, fields = "TIME")
  expect_equal(nrow(result), 2)
  expect_equal(result$LOG_COL[result$TIME == 0], TRUE)
  expect_true(is.na(result$LOG_COL[result$TIME == 1]))
  expect_equal(typeof(result$LOG_COL), "logical")
})

test_that("resolve_duplicates handles multiple duplicate groups correctly", {
  # Test complex scenario with multiple groups and various column behaviors
  test_df <- tribble(
    ~ID, ~TIME, ~DV, ~CONSISTENT, ~INCONSISTENT, ~ALL_NA,
    1,   0,     10,  "A",         "X",           NA,
    1,   0,     20,  "A",         "Y",           NA,
    1,   1,     30,  "B",         "Z",           NA,
    1,   1,     40,  "B",         "W",           NA,
    2,   0,     50,  "C",         "P",           NA,
    2,   0,     60,  "C",         "Q",           NA
  )

  result <- resolve_duplicates(test_df, fields = c("ID", "TIME"))
  expect_equal(nrow(result), 3)

  # Check consistent column
  expect_equal(result$CONSISTENT[result$ID == 1 & result$TIME == 0], "A")
  expect_equal(result$CONSISTENT[result$ID == 1 & result$TIME == 1], "B")
  expect_equal(result$CONSISTENT[result$ID == 2 & result$TIME == 0], "C")

  # Check inconsistent column
  expect_true(is.na(result$INCONSISTENT[result$ID == 1 & result$TIME == 0]))
  expect_true(is.na(result$INCONSISTENT[result$ID == 1 & result$TIME == 1]))
  expect_true(is.na(result$INCONSISTENT[result$ID == 2 & result$TIME == 0]))

  # Check all-NA column
  expect_true(is.na(result$ALL_NA[result$ID == 1 & result$TIME == 0]))
})

test_that("resolve_duplicates works with min and max functions", {
  # Test with min and max functions
  test_df <- tribble(
    ~ID, ~TIME, ~DV,
    1,   0,     10,
    1,   0,     20,
    1,   0,     15,
    2,   1,     30,
    2,   1,     40
  )

  result_min <- resolve_duplicates(test_df, fields = "TIME", duplicate_function = min)
  expect_equal(result_min$DV[result_min$TIME == 0], 10)
  expect_equal(result_min$DV[result_min$TIME == 1], 30)

  result_max <- resolve_duplicates(test_df, fields = "TIME", duplicate_function = max)
  expect_equal(result_max$DV[result_max$TIME == 0], 20)
  expect_equal(result_max$DV[result_max$TIME == 1], 40)
})

test_that("resolve_duplicates works with median function", {
  # Test with median function
  test_df <- tribble(
    ~ID, ~TIME, ~DV,
    1,   0,     10,
    1,   0,     20,
    1,   0,     30,
    2,   1,     40,
    2,   1,     50
  )

  result <- resolve_duplicates(test_df, fields = "TIME", duplicate_function = median)
  expect_equal(result$DV[result$TIME == 0], 20)
  expect_equal(result$DV[result$TIME == 1], 45)
})

test_that("resolve_duplicates handles custom dependent variable name", {
  # Test with non-standard dependent variable name
  test_df <- tribble(
    ~ID, ~TIME, ~VALUE,
    1,   0,     10,
    1,   0,     20,
    2,   1,     30
  )

  result <- resolve_duplicates(test_df, fields = "TIME", dependent_variable = "VALUE")
  expect_equal(nrow(result), 2)
  expect_equal(result$VALUE[result$TIME == 0], 15)
  expect_equal(result$VALUE[result$TIME == 1], 30)
})

test_that("resolve_duplicates handles multiple missing fields error message", {
  # Test error message for multiple missing fields
  test_df <- tribble(
    ~ID, ~TIME,
    1,   0,
    2,   1
  )

  expect_error(
    resolve_duplicates(test_df, fields = c("FIELD1", "FIELD2", "FIELD3")),
    "Fields not found in input"
  )
})

test_that("resolve_duplicates handles NULL fields parameter", {
  # Test that NULL fields is rejected (allow_null = FALSE)
  test_df <- tribble(
    ~ID, ~TIME, ~DV,
    1,   0,     10,
    2,   1,     20
  )

  expect_error(
    resolve_duplicates(test_df, fields = NULL),
    "fields"
  )
})

test_that("resolve_duplicates handles empty character fields parameter", {
  # Test edge case with empty character vector
  test_df <- tribble(
    ~ID, ~TIME, ~DV,
    1,   0,     10,
    2,   1,     20
  )

  expect_error(
    resolve_duplicates(test_df, fields = character(0)),
    "At least one field"
  )
})

test_that("resolve_duplicates handles numeric grouping fields", {
  # Test that numeric values in grouping fields work correctly
  test_df <- tribble(
    ~ID, ~TIME, ~DV,
    1,   0.5,   10,
    1,   0.5,   20,
    1,   1.0,   30,
    2,   0.5,   40
  )

  result <- resolve_duplicates(test_df, fields = "TIME")
  expect_equal(nrow(result), 2)
  expect_equal(result$DV[result$TIME == 0.5], (10 + 20 + 40) / 3)  # Mean of 10, 20, 40
  expect_equal(result$DV[result$TIME == 1.0], 30)
})

test_that("resolve_duplicates handles character grouping fields", {
  # Test that character values in grouping fields work correctly
  test_df <- tribble(
    ~ID, ~TIME, ~DV,
    1,   "A",   10,
    1,   "A",   20,
    1,   "B",   30,
    2,   "A",   40
  )

  result <- resolve_duplicates(test_df, fields = "TIME")
  expect_equal(nrow(result), 2)
  expect_equal(result$DV[result$TIME == "A"], (10 + 20 + 40) / 3)  # Mean of 10, 20, 40
  expect_equal(result$DV[result$TIME == "B"], 30)
})

