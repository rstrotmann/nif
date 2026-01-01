test_that("ensure_dose creates DOSE field correctly", {
  # Create test data
  test_nif <- tibble::tribble(
    ~ID, ~TIME, ~AMT, ~EVID, ~CMT, ~DV,
    1,   0,     100,  1,     1,    NA,
    1,   1,     0,    0,     1,    10,
    1,   2,     0,    0,     1,    20,
    2,   0,     200,  1,     1,    NA,
    2,   1,     0,    0,     1,    30,
    2,   2,     0,    0,     1,    40
  ) %>% nif()

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
    ~ID, ~TIME, ~AMT, ~EVID, ~DOSE, ~CMT, ~DV,
    1,   0,     100,  1,     50,    1,    NA,
    1,   1,     0,    0,     50,    1,    10
  ) %>% nif()

  # Apply ensure_dose
  result <- ensure_dose(test_nif)

  # Check that original DOSE values are preserved
  expect_equal(result$DOSE, c(50, 50))
})

test_that("ensure_dose handles multiple doses per subject", {
  # Create test data with multiple doses
  test_nif <- tibble::tribble(
    ~ID, ~TIME, ~AMT, ~EVID, ~CMT, ~DV,
    1,   0,     100,  1,     1,    NA,
    1,   1,     0,    0,     1,    10,
    1,   2,     200,  1,     1,    NA,
    1,   3,     0,    0,     1,    20
  ) %>% nif()

  # Apply ensure_dose
  result <- ensure_dose(test_nif)

  # Check results
  expect_equal(result$DOSE, c(100, 100, 200, 200))
})

test_that("ensure_dose handles NA values correctly", {
  # Create test data with NAs
  test_nif <- tibble::tribble(
    ~ID, ~TIME, ~AMT, ~EVID, ~CMT, ~DV,
    1,   0,     100,  1,     1,    NA,
    1,   1,     NA,   0,     1,    10,
    1,   2,     0,    0,     1,    20
  ) %>% nif()

  # Apply ensure_dose
  result <- ensure_dose(test_nif)

  # Check results
  expect_equal(result$DOSE, c(100, 100, 100))
})

test_that("ensure_dose handles empty data frame", {
  # Create empty NIF object
  test_nif <- nif()

  # Apply ensure_dose
  result <- ensure_dose(test_nif)

  # Check results
  expect_true("DOSE" %in% names(result))
  expect_equal(nrow(result), 0)
})

test_that("ensure_dose handles missing required columns", {
  # Create test data missing required columns
  test_nif <- tibble::tribble(
    ~ID, ~TIME, ~AMT, ~CMT, ~EVID, ~DV,
    1,   0,     0,    1,    0,     NA,
    1,   1,     0,    1,    0,     NA
  ) %>% nif()

  # Check error message
  expect_error(
    ensure_dose(
      select(test_nif, -c("EVID", "AMT"))
    ),
    "Missing required columns: EVID, AMT"
  )
})

test_that("ensure_dose handles non-NIF input", {
  # Create regular data frame
  test_df <- tibble::tribble(
    ~ID, ~TIME, ~AMT, ~EVID, ~CMT, ~DV,
    1,   0,     100,  1,     1,    NA,
    1,   1,     0,    0,     1,    10
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
    ~ID, ~TIME, ~AMT, ~EVID, ~CMT, ~DV,
    1,   2,     0,    0,     1,    20,
    1,   0,     100,  1,     1,    NA,
    1,   1,     0,    0,     1,    10,
    2,   1,     0,    0,     1,    30,
    2,   0,     200,  1,     1,    NA,
    2,   2,     0,    0,     1,    40
  ) %>% nif()

  # Apply ensure_dose
  result <- ensure_dose(test_nif)

  # Check results
  expect_equal(result$DOSE, c(100, 100, 100, 200, 200, 200))
})

test_that("ensure_dose handles zero doses correctly", {
  # Create test data with zero doses
  test_nif <- tibble::tribble(
    ~ID, ~TIME, ~AMT, ~EVID, ~CMT, ~DV,
    1,   0,     0,    1,     1,    NA,
    1,   1,     0,    0,     1,    10,
    1,   2,     0,    0,     1,    20
  ) %>% nif()

  # Apply ensure_dose
  result <- ensure_dose(test_nif)

  # Check results
  expect_equal(result$DOSE, c(0, 0, 0))
})
