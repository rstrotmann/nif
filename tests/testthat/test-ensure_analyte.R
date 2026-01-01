# Test file for ensure_analyte function

test_that("ensure_analyte creates ANALYTE from CMT when missing", {
  # Create test data with CMT but no ANALYTE
  test_data <- tibble::tribble(
    ~ID, ~TIME, ~CMT, ~DV, ~EVID, ~AMT,
    1,   0,     1,    0,   1,     100,
    1,   1,     1,    10,  0,     0,
    1,   2,     1,    20,  0,     0,
    2,   0,     2,    0,   1,     100,
    2,   1,     2,    15,  0,     0
  ) %>% nif()

  result <- ensure_analyte(test_data)

  # Check that ANALYTE was created
  expect_true("ANALYTE" %in% names(result))
  # Check that ANALYTE values match CMT values
  expect_equal(result$ANALYTE, paste0("CMT", as.character(result$CMT)))
})

test_that("ensure_analyte preserves existing ANALYTE values", {
  # Create test data with both CMT and ANALYTE
  test_data <- tibble::tribble(
    ~ID, ~TIME, ~CMT, ~ANALYTE,    ~DV, ~EVID, ~AMT,
    1,   0,     1,    "DRUG",      0,   1,     100,
    1,   1,     1,    "DRUG",      10,  0,     0,
    1,   2,     1,    "DRUG",      20,  0,     0,
    2,   0,     2,    "METABOLITE", 0,   1,     100,
    2,   1,     2,    "METABOLITE", 15,  0,     0
  ) %>% nif()

  result <- ensure_analyte(test_data)

  # Check that original ANALYTE values are preserved
  expect_equal(result$ANALYTE, test_data$ANALYTE)
})

test_that("ensure_analyte handles NA values in CMT", {
  # Create test data with NA in CMT
  test_data <- tibble::tribble(
    ~ID, ~TIME, ~CMT, ~DV, ~EVID, ~AMT,
    1,   0,     1,    0,   1,     100,
    1,   1,     NA,   10,  0,     0,
    1,   2,     1,    20,  0,     0
  ) %>% nif()

  result <- ensure_analyte(test_data)

  # Check that NA in CMT becomes NA in ANALYTE
  expect_true(is.na(result$ANALYTE[2]))
  # Check that non-NA values are properly converted
  expect_equal(result$ANALYTE[1], "CMT1")
  expect_equal(result$ANALYTE[3], "CMT1")
})

test_that("ensure_analyte returns NIF object", {
  # Create test data
  test_data <- tibble::tribble(
    ~ID, ~TIME, ~CMT, ~DV, ~EVID, ~AMT,
    1,   0,     1,    0,   1,     100,
    1,   1,     1,    10,  0,     0
  ) %>% nif()

  result <- ensure_analyte(test_data)

  # Check that result is a NIF object
  expect_true(inherits(result, "nif"))
  expect_true(inherits(result, "data.frame"))
})

test_that("ensure_analyte errors on non-NIF input", {
  # Create regular data.frame
  test_data <- tibble::tribble(
    ~ID, ~TIME, ~CMT, ~DV, ~EVID, ~AMT,
    1,   0,     1,    0,   1,     100,
    1,   1,     1,    10,  0,     0
  )

  # Check that error is thrown for non-NIF input
  expect_error(ensure_analyte(test_data), "Input must be a NIF object")
})

test_that("ensure_analyte errors when CMT is missing", {
  # Create test data without CMT
  test_data <- tibble::tribble(
    ~ID, ~TIME, ~DV, ~EVID, ~AMT, ~CMT,
    1,   0,     0,   1,     100,  1,
    1,   1,     10,  0,     0,    1
  ) %>% nif() |>
    select(-CMT)

  # Check that error is thrown when CMT is missing
  expect_error(ensure_analyte(test_data), "CMT column is required when ANALYTE is not present")
})

test_that("ensure_analyte handles empty data frame", {
  # Create empty NIF object
  test_data <- nif()

  result <- ensure_analyte(test_data)

  # Check that result is still a NIF object
  expect_true(inherits(result, "nif"))
  # Check that ANALYTE column exists
  expect_true("ANALYTE" %in% names(result))
  # Check that result is empty
  expect_equal(nrow(result), 0)
})

test_that("ensure_analyte preserves other columns", {
  # Create test data with additional columns
  test_data <- tibble::tribble(
    ~ID, ~TIME, ~CMT, ~DV, ~EVID, ~DOSE, ~WEIGHT, ~AMT,
    1,   0,     1,    0,   1,     100,   70,      100,
    1,   1,     1,    10,  0,     100,   70,      0
  ) %>% nif()

  result <- ensure_analyte(test_data)

  # Check that all original columns are preserved
  expect_true(all(names(test_data) %in% names(result)))
  # Check that values in other columns are unchanged
  expect_equal(result$DOSE, test_data$DOSE)
  expect_equal(result$WEIGHT, test_data$WEIGHT)
})
