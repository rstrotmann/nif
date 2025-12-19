test_that("add_tafd works with basic input", {
  # Create a simple test data frame
  test_data <- tibble::tribble(
    ~ID, ~TIME, ~EVID, ~PARENT, ~DV, ~CMT,
    1,   0,     1,     "DRUG",  NA,  1,
    1,   1,     0,     "DRUG",  10,  2,
    1,   2,     0,     "DRUG",  20,  2,
    2,   0,     1,     "DRUG",  NA,  1,
    2,   1,     0,     "DRUG",  30,  2,
    2,   2,     0,     "DRUG",  40,  2
  ) %>%
    new_nif()

  result <- add_tafd(test_data)

  # Check that TAFD was added
  expect_true("TAFD" %in% names(result))

  # Check TAFD values
  expect_equal(result$TAFD, c(0, 1, 2, 0, 1, 2))
})

test_that("add_tafd handles observations before first dose", {
  test_data <- tibble::tribble(
    ~ID, ~TIME, ~EVID, ~PARENT, ~DV, ~CMT,
    1,   -2,    0,     "DRUG",  5,   2,
    1,   -1,    0,     "DRUG",  8,   2,
    1,   0,     1,     "DRUG",  NA,  1,
    1,   1,     0,     "DRUG",  10,  2,
    1,   2,     0,     "DRUG",  20,  2
  ) %>%
    new_nif()

  result <- add_tafd(test_data)

  # Check that pre-dose observations have negative TAFD
  expect_equal(result$TAFD, c(-2, -1, 0, 1, 2))
})

test_that("add_tafd handles multiple administrations", {
  test_data <- tibble::tribble(
    ~ID, ~TIME, ~EVID, ~PARENT, ~DV, ~CMT,
    1,   0,     1,     "DRUG",  NA,  1,
    1,   1,     0,     "DRUG",  10,  2,
    1,   2,     1,     "DRUG",  NA,  1,
    1,   3,     0,     "DRUG",  20,  2,
    1,   4,     0,     "DRUG",  30,  2
  ) %>%
    new_nif()

  result <- add_tafd(test_data)

  # TAFD should be based on first dose only
  expect_equal(result$TAFD, c(0, 1, 2, 3, 4))
})

test_that("add_tafd handles multiple parent compounds", {
  test_data <- tibble::tribble(
    ~ID, ~TIME, ~EVID, ~PARENT, ~DV, ~CMT,
    1,   0,     1,     "DRUG1", NA,  1,
    1,   1,     0,     "DRUG1", 10,  2,
    1,   2,     0,     "DRUG1", 20,  2,
    1,   1,     1,     "DRUG2", NA,  3,
    1,   2,     0,     "DRUG2", 30,  4,
    1,   3,     0,     "DRUG2", 40,  4
  ) %>%
    new_nif()

  result <- add_tafd(test_data)

  # Check TAFD values for each parent
  expect_equal(result$TAFD, c(0, 1, 2, 0, 1, 2))
})

test_that("add_tafd handles different first dose times", {
  test_data <- tibble::tribble(
    ~ID, ~TIME, ~EVID, ~PARENT, ~DV, ~CMT,
    1,   0,     1,     "DRUG",  NA,  1,
    1,   1,     0,     "DRUG",  10,  2,
    1,   2,     0,     "DRUG",  20,  2,
    2,   5,     1,     "DRUG",  NA,  1,
    2,   6,     0,     "DRUG",  30,  2,
    2,   7,     0,     "DRUG",  40,  2
  ) %>%
    new_nif()

  result <- add_tafd(test_data)

  # Each subject's TAFD should be relative to their own first dose
  expect_equal(result$TAFD, c(0, 1, 2, 0, 1, 2))
})

test_that("add_tafd handles empty data frame", {
  test_data <- tibble::tribble(
    ~ID, ~TIME, ~EVID, ~PARENT, ~DV, ~CMT
  ) %>%
    new_nif()

  result <- add_tafd(test_data)

  expect_equal(nrow(result), 0)
  expect_true("TAFD" %in% names(result))
})

test_that("add_tafd handles missing required columns", {
  test_data <- tibble::tribble(
    ~ID, ~TIME,
    1,   0,
    1,   1
  ) %>%
    new_nif()

  expect_error(add_tafd(test_data), "Missing required columns")
})

test_that("add_tafd validates input is a nif object", {
  test_data <- tibble::tribble(
    ~ID, ~TIME, ~EVID, ~PARENT, ~CMT,
    1,   0,     1,     "DRUG",  1,
    1,   1,     0,     "DRUG",  2
  )

  expect_error(add_tafd(test_data), "Input must be a NIF object")
})

test_that("add_tafd preserves original data", {
  test_data <- tibble::tribble(
    ~ID, ~TIME, ~EVID, ~PARENT, ~DV, ~EXTRA, ~CMT,
    1,   0,     1,     "DRUG",  NA,  "A",    1,
    1,   1,     0,     "DRUG",  10,  "B",    2,
    1,   2,     0,     "DRUG",  20,  "C",    2
  ) %>%
    new_nif()

  result <- add_tafd(test_data)

  # Check that original columns are preserved
  expect_true(all(names(test_data) %in% names(result)))
  expect_equal(result$EXTRA, test_data$EXTRA)
})

test_that("add_tafd validates numeric data types", {
  # Test with non-numeric TIME
  test_data <- tibble::tribble(
    ~ID, ~TIME, ~EVID, ~PARENT, ~CMT,
    1,   "0",   1,     "DRUG",  1,
    1,   "1",   0,     "DRUG",  2
  ) %>%
    new_nif()

  expect_error(add_tafd(test_data), "TIME column must contain numeric values")

  # Test with non-numeric EVID
  test_data <- tibble::tribble(
    ~ID, ~TIME, ~EVID, ~PARENT, ~CMT,
    1,   0,     "1",   "DRUG",  1,
    1,   1,     "0",   "DRUG",  2
  ) %>%
    new_nif()

  expect_error(add_tafd(test_data), "EVID column must contain numeric values")

  # Test with non-numeric ID
  test_data <- tibble::tribble(
    ~ID,  ~TIME, ~EVID, ~PARENT, ~CMT,
    "1",  0,     1,     "DRUG",  1,
    "1",  1,     0,     "DRUG",  2
  ) %>%
    new_nif()

  expect_error(add_tafd(test_data), "ID column must contain numeric values")
})

test_that("add_tafd handles data with no dosing events", {
  test_data <- tibble::tribble(
    ~ID, ~TIME, ~EVID, ~PARENT, ~DV, ~CMT,
    1,   0,     0,     "DRUG",  5,   2,
    1,   1,     0,     "DRUG",  10,  2,
    1,   2,     0,     "DRUG",  20,  2
  ) %>%
    new_nif()

  # Should throw an error since there are no dosing events
  expect_error(
    result <- add_tafd(test_data),
    "No dosing event, TAFD cannot be calculated"
  )
})

test_that("add_tafd returns a nif object", {
  test_data <- tibble::tribble(
    ~ID, ~TIME, ~EVID, ~PARENT, ~DV, ~CMT,
    1,   0,     1,     "DRUG",  NA,  1,
    1,   1,     0,     "DRUG",  10,  2
  ) %>%
    new_nif()

  result <- add_tafd(test_data)

  expect_s3_class(result, "nif")
})

# NEW TESTS

test_that("add_tafd handles missing PARENT column by creating it", {
  # Create a test data frame without PARENT column
  test_data <- tibble::tribble(
    ~ID, ~TIME, ~EVID, ~CMT, ~DV,
    1,   0,     1,     1,    NA,
    1,   1,     0,     2,    10,
    1,   2,     0,     2,    20
  ) %>%
    new_nif()

  result <- add_tafd(test_data)

  # Check that PARENT was created and TAFD calculated correctly
  expect_true("PARENT" %in% names(result))
  expect_equal(result$TAFD, c(0, 1, 2))
})

test_that("add_tafd properly ungroups the result", {
  test_data <- tibble::tribble(
    ~ID, ~TIME, ~EVID, ~PARENT, ~DV, ~CMT,
    1,   0,     1,     "DRUG",  NA,  1,
    1,   1,     0,     "DRUG",  10,  2,
    2,   0,     1,     "DRUG",  NA,  1,
    2,   1,     0,     "DRUG",  20,  2
  ) %>%
    new_nif()

  result <- add_tafd(test_data)

  # Check that result is ungrouped
  expect_false(dplyr::is_grouped_df(result))

  # Verify we can perform operations that would fail on a grouped dataframe
  expect_no_error(result %>% dplyr::mutate(NewCol = 1))
})

test_that("add_tafd handles NA values in TIME correctly", {
  test_data <- tibble::tribble(
    ~ID, ~TIME, ~EVID, ~PARENT, ~CMT,
    1,   0,     1,     "DRUG",  1,
    1,   1,     0,     "DRUG",  2,
    1,   NA,    0,     "DRUG",  2
  ) %>%
    new_nif()

  result <- add_tafd(test_data)

  # Check TAFD values (NA for the row with NA TIME)
  expect_equal(result$TAFD[1:2], c(0, 1))
  expect_true(is.na(result$TAFD[3]))
})

test_that("add_tafd correctly handles NA values in ID column", {
  test_data <- tibble::tribble(
    ~ID, ~TIME, ~EVID, ~PARENT, ~CMT,
    1,   0,     1,     "DRUG",  1,
    1,   1,     0,     "DRUG",  2,
    NA,  2,     0,     "DRUG",  2
  ) %>%
    new_nif()

  # Should handle NA in ID column without error
  expect_error(
    result <- add_tafd(test_data),
    "ID colum must not contain NA values!"
  )
})

test_that("add_tafd respects parent grouping with mixed dosing times", {
  test_data <- tibble::tribble(
    ~ID, ~TIME, ~EVID, ~PARENT, ~CMT,
    1,   0,     1,     "DRUG1", 1,
    1,   1,     0,     "DRUG1", 2,
    1,   3,     1,     "DRUG2", 3, # Later first dose for DRUG2
    1,   4,     0,     "DRUG2", 4
  ) %>%
    new_nif()

  result <- add_tafd(test_data)

  # TAFD for DRUG1 should be based on time 0, for DRUG2 on time 3
  expect_equal(result$TAFD, c(0, 1, 0, 1))
})

test_that("add_tafd works with CMT column but no PARENT column", {
  test_data <- tibble::tribble(
    ~ID, ~TIME, ~EVID, ~CMT, ~DV,
    1,   0,     1,     1,    NA,
    1,   1,     0,     2,    10,
    1,   2,     0,     2,    20,
    2,   0,     1,     1,    NA,
    2,   1,     0,     2,    30
  ) %>%
    new_nif()

  result <- add_tafd(test_data)

  # Check TAFD values are correctly calculated
  expect_equal(result$TAFD, c(0, 1, 2, 0, 1))
  # Check PARENT was generated
  expect_true("PARENT" %in% names(result))
})
