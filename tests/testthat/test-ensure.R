# Test file for ensure_analyte function

test_that("ensure_analyte creates ANALYTE from CMT when missing", {
  # Create test data with CMT but no ANALYTE
  test_data <- tibble::tribble(
    ~ID, ~TIME, ~CMT, ~DV, ~EVID,
    1,     0,    1,   0,     1,
    1,     1,    1,  10,     0,
    1,     2,    1,  20,     0,
    2,     0,    2,   0,     1,
    2,     1,    2,  15,     0
  ) %>% new_nif()

  result <- ensure_analyte(test_data)

  # Check that ANALYTE was created
  expect_true("ANALYTE" %in% names(result))
  # Check that ANALYTE values match CMT values
  expect_equal(result$ANALYTE, as.character(result$CMT))
})

test_that("ensure_analyte preserves existing ANALYTE values", {
  # Create test data with both CMT and ANALYTE
  test_data <- tibble::tribble(
    ~ID, ~TIME, ~CMT,     ~ANALYTE, ~DV, ~EVID,
    1,     0,    1,       "DRUG",   0,     1,
    1,     1,    1,       "DRUG",  10,     0,
    1,     2,    1,       "DRUG",  20,     0,
    2,     0,    2, "METABOLITE",   0,     1,
    2,     1,    2, "METABOLITE",  15,     0
  ) %>% new_nif()

  result <- ensure_analyte(test_data)

  # Check that original ANALYTE values are preserved
  expect_equal(result$ANALYTE, test_data$ANALYTE)
})

test_that("ensure_analyte handles NA values in CMT", {
  # Create test data with NA in CMT
  test_data <- tibble::tribble(
    ~ID, ~TIME, ~CMT, ~DV, ~EVID,
    1,     0,    1,   0,     1,
    1,     1,   NA,  10,     0,
    1,     2,    1,  20,     0
  ) %>% new_nif()

  result <- ensure_analyte(test_data)

  # Check that NA in CMT becomes NA in ANALYTE
  expect_true(is.na(result$ANALYTE[2]))
  # Check that non-NA values are properly converted
  expect_equal(result$ANALYTE[1], "1")
  expect_equal(result$ANALYTE[3], "1")
})

test_that("ensure_analyte handles non-numeric CMT values", {
  # Create test data with character CMT values
  test_data <- tibble::tribble(
    ~ID, ~TIME, ~CMT, ~DV, ~EVID,
    1, 0, "DRUG", 0, 1,
    1, 1, "DRUG", 10, 0,
    1, 2, "DRUG", 20, 0
  ) %>% new_nif()

  result <- ensure_analyte(test_data)

  # Check that character CMT values are preserved
  expect_equal(result$ANALYTE, test_data$CMT)
})

test_that("ensure_analyte returns NIF object", {
  # Create test data
  test_data <- tibble::tribble(
    ~ID, ~TIME, ~CMT, ~DV, ~EVID,
    1, 0, 1, 0, 1,
    1, 1, 1, 10, 0
  ) %>% new_nif()

  result <- ensure_analyte(test_data)

  # Check that result is a NIF object
  expect_true(inherits(result, "nif"))
  expect_true(inherits(result, "data.frame"))
})

test_that("ensure_analyte errors on non-NIF input", {
  # Create regular data.frame
  test_data <- tibble::tribble(
    ~ID, ~TIME, ~CMT, ~DV, ~EVID,
    1,     0,    1,   0,     1,
    1,     1,    1,  10,     0
  )

  # Check that error is thrown for non-NIF input
  expect_error(ensure_analyte(test_data), "Input must be a NIF object")
})

test_that("ensure_analyte errors when CMT is missing", {
  # Create test data without CMT
  test_data <- tibble::tribble(
    ~ID, ~TIME, ~DV, ~EVID,
    1, 0, 0, 1,
    1, 1, 10, 0
  ) %>% new_nif()

  # Check that error is thrown when CMT is missing
  expect_error(ensure_analyte(test_data), "CMT column is required when ANALYTE is not present")
})

test_that("ensure_analyte handles empty data frame", {
  # Create empty NIF object
  test_data <- new_nif()

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
    ~ID, ~TIME, ~CMT, ~DV, ~EVID, ~DOSE, ~WEIGHT,
    1, 0, 1, 0, 1, 100, 70,
    1, 1, 1, 10, 0, 100, 70
  ) %>% new_nif()

  result <- ensure_analyte(test_data)

  # Check that all original columns are preserved
  expect_true(all(names(test_data) %in% names(result)))
  # Check that values in other columns are unchanged
  expect_equal(result$DOSE, test_data$DOSE)
  expect_equal(result$WEIGHT, test_data$WEIGHT)
})

test_that("ensure_parent() works correctly", {
  # Create test data
  test_data <- tibble::tribble(
    ~ID, ~TIME, ~EVID, ~CMT, ~DV,
    1,     0,     1,    1, 100,
    1,     1,     0,    1,  50,
    1,     2,     0,    1,  25,
    2,     0,     1,    1, 100,
    2,     1,     0,    1,  50,
    2,     2,     0,    1,  25
  )
  test_nif <- test_data %>%
    new_nif()

  # Test basic functionality
  result <- ensure_parent(test_nif)
  expect_true("PARENT" %in% names(result))
  expect_equal(unique(result$PARENT), "1")
  expect_true(inherits(result, "nif"))

  # Test with multiple CMT values
  multi_cmt_nif <- tibble::tribble(
    ~ID, ~TIME, ~EVID, ~CMT, ~DV,
    1,     0,     1,    1, 100,
    1,     1,     0,    1,  50,
    1,     2,     0,    1,  25,
    2,     0,     1,    2, 100,
    2,     1,     0,    2,  50,
    2,     2,     0,    2,  25
  ) %>% new_nif()

  result <- ensure_parent(multi_cmt_nif)
  expect_true("PARENT" %in% names(result))
  expect_equal(unique(result$PARENT), "1") # Should use most common CMT

  # Test with existing PARENT column
  existing_parent_data <- test_data
  existing_parent_data$PARENT <- "test"
  existing_parent_nif <- new_nif(existing_parent_data)
  result <- ensure_parent(existing_parent_nif)
  expect_equal(unique(result$PARENT), "test")

  # Test with no administrations
  no_admin_nif <- new_nif(tibble::tribble(
    ~ID, ~TIME, ~EVID, ~CMT, ~DV,
    1,     0,     0,    1,  50,
    1,     1,     0,    1,  25,
    1,     2,     0,    1,  12
  ))

  expect_warning(result <- ensure_parent(no_admin_nif))
  expect_false("PARENT" %in% names(result))

  # Test error conditions
  # Missing EVID column
  missing_evid_data <- test_data[, -which(names(test_data) == "EVID")]
  missing_evid_nif <- new_nif(missing_evid_data)
  expect_error(ensure_parent(missing_evid_nif), "Missing required columns: EVID")

  # Missing CMT column
  missing_cmt_data <- test_data[, -which(names(test_data) == "CMT")]
  missing_cmt_nif <- new_nif(missing_cmt_data)
  expect_error(ensure_parent(missing_cmt_nif), "Missing required columns: CMT")

  # Not a NIF object
  expect_error(ensure_parent(as.data.frame(test_nif)), "Input must be a NIF object")
})
