test_that("ensure_tad works with basic input", {
  # Create a simple test data frame
  test_data <- tibble::tribble(
    ~ID, ~TIME, ~EVID, ~CMT, ~PARENT, ~DV,
    1,   0,     1,     1,    "DRUG",  NA,
    1,   1,     0,     2,    "DRUG",  10,
    1,   2,     0,     2,    "DRUG",  20,
    2,   0,     1,     1,    "DRUG",  NA,
    2,   1,     0,     2,    "DRUG",  30,
    2,   2,     0,     2,    "DRUG",  40
  ) %>%
    new_nif()

  result <- ensure_tad(test_data)

  # Check that TAD was added
  expect_true("TAD" %in% names(result))

  # Check TAD values
  expect_equal(result$TAD, c(0, 1, 2, 0, 1, 2))
})


test_that("ensure_tad handles multiple administrations", {
  test_data <- tibble::tribble(
    ~ID, ~TIME, ~EVID, ~CMT, ~PARENT, ~DV,
    1,   0,     1,     1,    "DRUG",  NA,
    1,   1,     0,     2,    "DRUG",  10,
    1,   2,     1,     1,    "DRUG",  NA,
    1,   3,     0,     2,    "DRUG",  20,
    1,   4,     0,     2,    "DRUG",  30
  ) %>%
    new_nif()

  result <- ensure_tad(test_data)

  # Check TAD values after second administration
  expect_equal(result$TAD, c(0, 1, 0, 1, 2))
})


test_that("ensure_tad handles observations before first dose", {
  test_data <- tibble::tribble(
    ~ID, ~TIME, ~EVID, ~CMT, ~PARENT, ~DV,
    1,   -1,    0,     2,    "DRUG",  5,
    1,   0,     1,     1,    "DRUG",  NA,
    1,   1,     0,     2,    "DRUG",  10,
    1,   2,     0,     2,    "DRUG",  20
  ) %>%
    new_nif()

  result <- ensure_tad(test_data)

  # Check that pre-dose observation has negative TAD
  expect_equal(result$TAD, c(-1, 0, 1, 2))
})


test_that("ensure_tad handles multiple parent compounds", {
  test_data <- tibble::tribble(
    ~ID, ~TIME, ~EVID, ~CMT, ~PARENT, ~DV,
    1,   0,     1,     1,    "DRUG1", NA,
    1,   1,     0,     2,    "DRUG1", 10,
    1,   2,     0,     2,    "DRUG1", 20,
    1,   0,     1,     3,    "DRUG2", NA,
    1,   1,     0,     4,    "DRUG2", 30,
    1,   2,     0,     4,    "DRUG2", 40
  ) %>%
    new_nif()

  result <- ensure_tad(test_data)

  # Check TAD values for each parent
  expect_equal(result$TAD, c(0, 1, 2, 0, 1, 2))
})


test_that("ensure_tad handles empty data frame", {
  test_data <- tibble::tribble(
    ~ID, ~TIME, ~EVID, ~CMT, ~PARENT, ~DV
  ) %>%
    new_nif()

  expect_warning(
    result <- ensure_tad(test_data),
    "No administration records found"
  )

  expect_equal(nrow(result), 0)
  expect_true("TAD" %in% names(result))
})


test_that("ensure_tad handles missing required columns", {
  test_data <- tibble::tribble(
    ~ID, ~TIME, ~CMT,
    1,   0,     1,
    1,   1,     2
  ) %>%
    new_nif()

  expect_error(ensure_tad(test_data), "Missing required columns for TAD calculation")
})


test_that("ensure_tad preserves original data", {
  test_data <- tibble::tribble(
    ~ID, ~TIME, ~EVID, ~CMT, ~PARENT, ~DV, ~EXTRA,
    1,   0,     1,     1,    "DRUG",  NA,  "A",
    1,   1,     0,     2,    "DRUG",  10,  "B",
    1,   2,     0,     2,    "DRUG",  20,  "C"
  ) %>%
    new_nif()

  result <- ensure_tad(test_data)

  # Check that original columns are preserved
  expect_true(all(names(test_data) %in% names(result)))
  expect_equal(result$EXTRA, test_data$EXTRA)
})


test_that("ensure_tad handles NA values in TIME", {
  test_data <- tibble::tribble(
    ~ID, ~TIME, ~EVID, ~CMT, ~PARENT, ~DV,
    1,   0,     1,     1,    "DRUG",  NA,
    1,   NA,    0,     2,    "DRUG",  10,
    1,   2,     0,     2,    "DRUG",  20
  ) %>%
    new_nif()

  result <- ensure_tad(test_data)

  # Check that NA in TIME results in NA in TAD
  expect_true(is.na(result$TAD[3]))
})


test_that("ensure_tad returns a nif object", {
  test_data <- tibble::tribble(
    ~ID, ~TIME, ~EVID, ~CMT, ~PARENT, ~DV,
    1,   0,     1,     1,    "DRUG",  NA,
    1,   1,     0,     2,    "DRUG",  10
  ) %>%
    new_nif()

  result <- ensure_tad(test_data)

  expect_s3_class(result, "nif")
})


test_that("ensure_tad handles non-nif input", {
  test_data <- data.frame(
    ID = 1,
    TIME = 0,
    EVID = 1,
    CMT = 1,
    PARENT = "DRUG",
    DV = NA
  )

  expect_error(ensure_tad(test_data), "Input must be a NIF object")
})


test_that("ensure_tad handles existing TAD column", {
  test_data <- tibble::tribble(
    ~ID, ~TIME, ~EVID, ~CMT, ~PARENT, ~DV, ~TAD,
    1,   0,     1,     1,    "DRUG",  NA,  0,
    1,   1,     0,     2,    "DRUG",  10,  1
  ) %>%
    new_nif()

  result <- ensure_tad(test_data)

  # Check that original TAD values are preserved
  expect_equal(result$TAD, c(0, 1))
})
