test_that("ensure_dose creates DOSE field correctly", {
  # Create test data
  test_nif <- tibble::tribble(
    ~ID, ~TIME, ~AMT, ~EVID,
    1,   0,     100,  1,
    1,   1,     0,    0,
    1,   2,     0,    0,
    2,   0,     200,  1,
    2,   1,     0,    0,
    2,   2,     0,    0
  ) %>% new_nif()

  # Apply ensure_dose
  result <- ensure_dose(test_nif)

  # Check results
  expect_true("DOSE" %in% names(result))
  expect_equal(result$DOSE, c(100, 100, 100, 200, 200, 200))
  expect_true(inherits(result, "nif"))
})

test_that("ensure_dose handles existing DOSE field", {
  # Create test data with existing DOSE
  test_nif <- tibble::tribble(
    ~ID, ~TIME, ~AMT, ~EVID, ~DOSE,
    1,   0,     100,  1,     50,
    1,   1,     0,    0,     50
  ) %>% new_nif()

  # Apply ensure_dose
  result <- ensure_dose(test_nif)

  # Check that original DOSE values are preserved
  expect_equal(result$DOSE, c(50, 50))
})

test_that("ensure_dose handles multiple doses per subject", {
  # Create test data with multiple doses
  test_nif <- tibble::tribble(
    ~ID, ~TIME, ~AMT, ~EVID,
    1,   0,     100,  1,
    1,   1,     0,    0,
    1,   2,     200,  1,
    1,   3,     0,    0
  ) %>% new_nif()

  # Apply ensure_dose
  result <- ensure_dose(test_nif)

  # Check results
  expect_equal(result$DOSE, c(100, 100, 200, 200))
})

test_that("ensure_dose handles NA values correctly", {
  # Create test data with NAs
  test_nif <- tibble::tribble(
    ~ID, ~TIME, ~AMT, ~EVID,
    1,   0,     100,  1,
    1,   1,     NA,   0,
    1,   2,     0,    0
  ) %>% new_nif()

  # Apply ensure_dose
  result <- ensure_dose(test_nif)

  # Check results
  expect_equal(result$DOSE, c(100, 100, 100))
})

test_that("ensure_dose handles empty data frame", {
  # Create empty NIF object
  test_nif <- new_nif()

  # Apply ensure_dose
  result <- ensure_dose(test_nif)

  # Check results
  expect_true("DOSE" %in% names(result))
  expect_equal(nrow(result), 0)
})

test_that("ensure_dose handles missing required columns", {
  # Create test data missing required columns
  test_nif <- tibble::tribble(
    ~ID, ~TIME,
    1,   0,
    1,   1
  ) %>% new_nif()

  # Check error message
  expect_error(
    ensure_dose(test_nif),
    "Missing required columns: EVID, AMT"
  )
})

test_that("ensure_dose handles non-NIF input", {
  # Create regular data frame
  test_df <- tibble::tribble(
    ~ID, ~TIME, ~AMT, ~EVID,
    1,   0,     100,  1,
    1,   1,     0,    0
  )

  # Check error message
  expect_error(
    ensure_dose(test_df),
    "Input must be a NIF object"
  )
})

test_that("ensure_dose handles unsorted data correctly", {
  # Create unsorted test data
  test_nif <- tibble::tribble(
    ~ID, ~TIME, ~AMT, ~EVID,
    1,   2,     0,    0,
    1,   0,     100,  1,
    1,   1,     0,    0,
    2,   1,     0,    0,
    2,   0,     200,  1,
    2,   2,     0,    0
  ) %>% new_nif()

  # Apply ensure_dose
  result <- ensure_dose(test_nif)

  # Check results
  expect_equal(result$DOSE, c(100, 100, 100, 200, 200, 200))
})

test_that("ensure_dose handles zero doses correctly", {
  # Create test data with zero doses
  test_nif <- tibble::tribble(
    ~ID, ~TIME, ~AMT, ~EVID,
    1,   0,     0,    1,
    1,   1,     0,    0,
    1,   2,     0,    0
  ) %>% new_nif()

  # Apply ensure_dose
  result <- ensure_dose(test_nif)

  # Check results
  expect_equal(result$DOSE, c(0, 0, 0))
})
