test_that("identify_baseline_columns identifies constant columns per ID", {
  # Basic test with baseline columns (SEX, AGE) and varying columns (TIME, DV)
  df <- tibble::tribble(
    ~ID, ~SEX, ~AGE, ~TIME, ~DV,
    1,   "M",  25,   0,     10,
    1,   "M",  25,   1,     12,
    1,   "M",  25,   2,     15,
    2,   "F",  30,   0,     8,
    2,   "F",  30,   1,     9,
    2,   "F",  30,   2,     11
  )

  result <- identify_baseline_columns(df, id_col = "ID")
  expect_equal(sort(result), c("AGE", "SEX"))
})


test_that("identify_baseline_columns handles all columns as baseline", {
  # All columns except ID are constant per ID
  df <- tibble::tribble(
    ~ID, ~SEX, ~AGE, ~TREATMENT,
    1,   "M",  25,   "A",
    1,   "M",  25,   "A",
    1,   "M",  25,   "A",
    2,   "F",  30,   "B",
    2,   "F",  30,   "B",
    2,   "F",  30,   "B"
  )

  result <- identify_baseline_columns(df, id_col = "ID")
  expect_equal(sort(result), c("AGE", "SEX", "TREATMENT"))
})


test_that("identify_baseline_columns handles no baseline columns", {
  # All columns vary per ID
  df <- tibble::tribble(
    ~ID, ~TIME, ~DV,
    1,   0,     10,
    1,   1,     12,
    1,   2,     15,
    2,   0,     8,
    2,   1,     9,
    2,   2,     11
  )

  result <- identify_baseline_columns(df, id_col = "ID")
  expect_equal(result, character(0))
})


test_that("identify_baseline_columns handles single row per ID", {
  # Each ID has only one row
  df <- tibble::tribble(
    ~ID, ~SEX, ~AGE, ~TIME, ~DV,
    1,   "M",  25,   0,     10,
    2,   "F",  30,   0,     8,
    3,   "M",  35,   0,     12
  )

  result <- identify_baseline_columns(df, id_col = "ID")
  expect_equal(sort(result), c("AGE", "DV", "SEX", "TIME"))
})


test_that("identify_baseline_columns handles columns with all NA values", {
  # Column with all NAs should be considered baseline
  df <- tibble::tribble(
    ~ID, ~SEX, ~AGE, ~MISSING,
    1,   "M",  25,   NA,
    1,   "M",  25,   NA,
    2,   "F",  30,   NA,
    2,   "F",  30,   NA
  )

  result <- identify_baseline_columns(df, id_col = "ID")
  expect_equal(sort(result), c("AGE", "MISSING", "SEX"))
})


test_that("identify_baseline_columns handles columns with some NAs but constant non-NA values", {
  # Column with some NAs but constant non-NA values per ID
  df <- tibble::tribble(
    ~ID, ~SEX, ~AGE, ~PARTIAL_NA,
    1,   "M",  25,   100,
    1,   "M",  25,   NA,
    1,   "M",  25,   100,
    2,   "F",  30,   200,
    2,   "F",  30,   NA,
    2,   "F",  30,   200
  )

  result <- identify_baseline_columns(df, id_col = "ID")
  expect_equal(sort(result), c("AGE", "PARTIAL_NA", "SEX"))
})


test_that("identify_baseline_columns handles columns with varying NAs", {
  # Column where NAs vary but non-NA values are constant
  df <- tibble::tribble(
    ~ID, ~SEX, ~AGE, ~VARYING_NA,
    1,   "M",  25,   100,
    1,   "M",  25,   NA,
    1,   "M",  25,   100,
    2,   "F",  30,   200,
    2,   "F",  30,   200,
    2,   "F",  30,   200
  )

  result <- identify_baseline_columns(df, id_col = "ID")
  expect_equal(sort(result), c("AGE", "SEX", "VARYING_NA"))
})


test_that("identify_baseline_columns handles columns with different values per ID", {
  # Column that varies within an ID should not be baseline
  df <- tibble::tribble(
    ~ID, ~SEX, ~AGE, ~VARYING,
    1,   "M",  25,   10,
    1,   "M",  25,   12,
    1,   "M",  25,   15,
    2,   "F",  30,   8,
    2,   "F",  30,   9,
    2,   "F",  30,   11
  )

  result <- identify_baseline_columns(df, id_col = "ID")
  expect_equal(sort(result), c("AGE", "SEX"))
})


test_that("identify_baseline_columns handles empty data frame", {
  df <- tibble::tribble(
    ~ID, ~SEX, ~AGE
  )

  result <- identify_baseline_columns(df, id_col = "ID")
  expect_equal(result, character(0))
})


test_that("identify_baseline_columns handles data frame with only ID column", {
  df <- tibble::tribble(
    ~ID,
    1,
    2,
    3
  )

  result <- identify_baseline_columns(df, id_col = "ID")
  expect_equal(result, character(0))
})


test_that("identify_baseline_columns handles custom ID column name", {
  df <- tibble::tribble(
    ~USUBJID, ~SEX, ~AGE, ~TIME, ~DV,
    1,        "M",  25,   0,     10,
    1,        "M",  25,   1,     12,
    2,        "F",  30,   0,     8,
    2,        "F",  30,   1,     9
  )

  result <- identify_baseline_columns(df, id_col = "USUBJID")
  expect_equal(sort(result), c("AGE", "SEX"))
})


test_that("identify_baseline_columns handles numeric baseline columns", {
  df <- tibble::tribble(
    ~ID, ~AGE, ~WEIGHT, ~TIME, ~DV,
    1,   25,   70,      0,     10,
    1,   25,   70,      1,     12,
    2,   30,   80,      0,     8,
    2,   30,   80,      1,     9
  )

  result <- identify_baseline_columns(df, id_col = "ID")
  expect_equal(sort(result), c("AGE", "WEIGHT"))
})


test_that("identify_baseline_columns handles logical baseline columns", {
  df <- tibble::tribble(
    ~ID, ~TREATED, ~ACTIVE, ~TIME, ~DV,
    1,   TRUE,     TRUE,    0,     10,
    1,   TRUE,     TRUE,    1,     12,
    2,   FALSE,    FALSE,   0,     8,
    2,   FALSE,    FALSE,   1,     9
  )

  result <- identify_baseline_columns(df, id_col = "ID")
  expect_equal(sort(result), c("ACTIVE", "TREATED"))
})


test_that("identify_baseline_columns handles factor baseline columns", {
  df <- tibble::tribble(
    ~ID, ~TREATMENT, ~SEX, ~TIME, ~DV,
    1,   "A",        "M",  0,     10,
    1,   "A",        "M",  1,     12,
    2,   "B",        "F",  0,     8,
    2,   "B",        "F",  1,     9
  ) %>%
    mutate(TREATMENT = factor(TREATMENT))

  result <- identify_baseline_columns(df, id_col = "ID")
  expect_equal(sort(result), c("SEX", "TREATMENT"))
})


test_that("identify_baseline_columns handles date baseline columns", {
  df <- tibble::tribble(
    ~ID, ~BIRTHDATE, ~START_DATE, ~TIME, ~DV,
    1,   "2000-01-01", "2023-01-01", 0,   10,
    1,   "2000-01-01", "2023-01-01", 1,   12,
    2,   "1995-05-15", "2023-02-01", 0,   8,
    2,   "1995-05-15", "2023-02-01", 1,   9
  ) %>%
    mutate(
      BIRTHDATE = as.Date(BIRTHDATE),
      START_DATE = as.Date(START_DATE)
    )

  result <- identify_baseline_columns(df, id_col = "ID")
  expect_equal(sort(result), c("BIRTHDATE", "START_DATE"))
})


test_that("identify_baseline_columns handles mixed data types", {
  df <- tibble::tribble(
    ~ID, ~SEX, ~AGE, ~TREATED, ~START_DATE, ~TIME, ~DV,
    1,   "M",  25,   TRUE,     "2023-01-01", 0,     10,
    1,   "M",  25,   TRUE,     "2023-01-01", 1,     12,
    2,   "F",  30,   FALSE,    "2023-02-01", 0,     8,
    2,   "F",  30,   FALSE,    "2023-02-01", 1,     9
  ) %>%
    mutate(START_DATE = as.Date(START_DATE))

  result <- identify_baseline_columns(df, id_col = "ID")
  expect_equal(sort(result), c("AGE", "SEX", "START_DATE", "TREATED"))
})


test_that("identify_baseline_columns handles single ID with multiple rows", {
  df <- tibble::tribble(
    ~ID, ~SEX, ~AGE, ~TIME, ~DV,
    1,   "M",  25,   0,     10,
    1,   "M",  25,   1,     12,
    1,   "M",  25,   2,     15
  )

  result <- identify_baseline_columns(df, id_col = "ID")
  expect_equal(sort(result), c("AGE", "SEX"))
})


test_that("identify_baseline_columns handles column where one ID has varying values", {
  # One ID has constant values, another has varying values
  df <- tibble::tribble(
    ~ID, ~SEX, ~AGE, ~MIXED,
    1,   "M",  25,   100,
    1,   "M",  25,   100,
    1,   "M",  25,   100,
    2,   "F",  30,   200,
    2,   "F",  30,   201,
    2,   "F",  30,   202
  )

  result <- identify_baseline_columns(df, id_col = "ID")
  expect_equal(sort(result), c("AGE", "SEX"))
})


test_that("identify_baseline_columns handles column with all NAs for one ID but values for another", {
  df <- tibble::tribble(
    ~ID, ~SEX, ~AGE, ~PARTIAL,
    1,   "M",  25,   NA,
    1,   "M",  25,   NA,
    2,   "F",  30,   100,
    2,   "F",  30,   100
  )

  result <- identify_baseline_columns(df, id_col = "ID")
  expect_equal(sort(result), c("AGE", "PARTIAL", "SEX"))
})


test_that("identify_baseline_columns handles column with different NAs per ID", {
  # One ID has all NAs, another has constant non-NA values
  df <- tibble::tribble(
    ~ID, ~SEX, ~AGE, ~NA_COL,
    1,   "M",  25,   NA,
    1,   "M",  25,   NA,
    2,   "F",  30,   100,
    2,   "F",  30,   100
  )

  result <- identify_baseline_columns(df, id_col = "ID")
  expect_equal(sort(result), c("AGE", "NA_COL", "SEX"))
})


test_that("identify_baseline_columns handles column with varying values including NAs", {
  # Column has both varying non-NA values and NAs
  df <- tibble::tribble(
    ~ID, ~SEX, ~AGE, ~VARYING_WITH_NA,
    1,   "M",  25,   100,
    1,   "M",  25,   101,
    1,   "M",  25,   NA,
    2,   "F",  30,   200,
    2,   "F",  30,   201
  )

  result <- identify_baseline_columns(df, id_col = "ID")
  expect_equal(sort(result), c("AGE", "SEX"))
})


test_that("identify_baseline_columns handles default id_col parameter", {
  df <- tibble::tribble(
    ~ID, ~SEX, ~AGE, ~TIME, ~DV,
    1,   "M",  25,   0,     10,
    1,   "M",  25,   1,     12,
    2,   "F",  30,   0,     8,
    2,   "F",  30,   1,     9
  )

  result <- identify_baseline_columns(df)
  expect_equal(sort(result), c("AGE", "SEX"))
})


test_that("identify_baseline_columns handles large number of IDs", {
  # Test with many IDs to ensure performance
  df <- tibble::tribble(
    ~ID, ~SEX, ~AGE, ~TIME, ~DV
  )

  # Create data for 100 IDs
  for (i in 1:100) {
    df <- df %>%
      bind_rows(tibble::tribble(
        ~ID, ~SEX, ~AGE, ~TIME, ~DV,
        i,   ifelse(i %% 2 == 0, "F", "M"), 20 + (i %% 50), 0, 10 + i,
        i,   ifelse(i %% 2 == 0, "F", "M"), 20 + (i %% 50), 1, 12 + i
      ))
  }

  result <- identify_baseline_columns(df, id_col = "ID")
  expect_equal(sort(result), c("AGE", "SEX"))
})


test_that("identify_baseline_columns handles many columns", {
  # Test with many columns
  df <- tibble::tribble(
    ~ID, ~COL1, ~COL2, ~COL3, ~COL4, ~COL5, ~COL6, ~COL7, ~COL8, ~COL9, ~COL10,
    1,   "A",   "B",   "C",   "D",   "E",   "F",   "G",   "H",   "I",   "J",
    1,   "A",   "B",   "C",   "D",   "E",   "F",   "G",   "H",   "I",   "J",
    2,   "X",   "Y",   "Z",   "W",   "V",   "U",   "T",   "S",   "R",   "Q",
    2,   "X",   "Y",   "Z",   "W",   "V",   "U",   "T",   "S",   "R",   "Q"
  )

  result <- identify_baseline_columns(df, id_col = "ID")
  expect_equal(length(result), 10)
})


# Input validation tests

test_that("identify_baseline_columns errors on non-data.frame input", {
  expect_error(
    identify_baseline_columns(list(a = 1, b = 2), id_col = "ID"),
    "Input must be a data frame"
  )

  expect_error(
    identify_baseline_columns(c(1, 2, 3), id_col = "ID"),
    "Input must be a data frame"
  )

  expect_error(
    identify_baseline_columns("not a data frame", id_col = "ID"),
    "Input must be a data frame"
  )
})


test_that("identify_baseline_columns errors on invalid id_col parameter", {
  df <- tibble::tribble(
    ~ID, ~SEX, ~AGE,
    1,   "M",  25,
    2,   "F",  30
  )

  expect_error(
    identify_baseline_columns(df, id_col = 123),
    "id_col must be a single character string"
  )

  expect_error(
    identify_baseline_columns(df, id_col = c("ID", "SEX")),
    "id_col must be a single character string"
  )

  expect_error(
    identify_baseline_columns(df, id_col = NULL),
    "id_col must be a single character string"
  )
})


test_that("identify_baseline_columns errors on missing ID column", {
  df <- tibble::tribble(
    ~SEX, ~AGE,
    "M",  25,
    "F",  30
  )

  expect_error(
    identify_baseline_columns(df, id_col = "ID"),
    "ID column 'ID' not found in data frame"
  )

  expect_error(
    identify_baseline_columns(df, id_col = "USUBJID"),
    "ID column 'USUBJID' not found in data frame"
  )
})


test_that("identify_baseline_columns handles tibble input", {
  df <- tibble::tribble(
    ~ID, ~SEX, ~AGE, ~TIME, ~DV,
    1,   "M",  25,   0,     10,
    1,   "M",  25,   1,     12,
    2,   "F",  30,   0,     8,
    2,   "F",  30,   1,     9
  )

  result <- identify_baseline_columns(df, id_col = "ID")
  expect_equal(sort(result), c("AGE", "SEX"))
})


test_that("identify_baseline_columns handles data.frame input", {
  df <- data.frame(
    ID = c(1, 1, 2, 2),
    SEX = c("M", "M", "F", "F"),
    AGE = c(25, 25, 30, 30),
    TIME = c(0, 1, 0, 1),
    DV = c(10, 12, 8, 9)
  )

  result <- identify_baseline_columns(df, id_col = "ID")
  expect_equal(sort(result), c("AGE", "SEX"))
})

