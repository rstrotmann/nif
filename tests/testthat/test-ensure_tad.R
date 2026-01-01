test_that("ensure_tad works with basic input", {
  # Create a simple test data frame
  test_data <- tibble::tribble(
    ~ID, ~TIME, ~EVID, ~CMT, ~PARENT, ~DV,  ~AMT,
    1,   0,     1,     1,    "DRUG",  NA,  100,
    1,   1,     0,     2,    "DRUG",  10,  0,
    1,   2,     0,     2,    "DRUG",  20,  0,
    2,   0,     1,     1,    "DRUG",  NA,  100,
    2,   1,     0,     2,    "DRUG",  30,  0,
    2,   2,     0,     2,    "DRUG",  40,  0
  ) %>%
    nif()

  result <- ensure_tad(test_data)

  # Check that TAD was added
  expect_true("TAD" %in% names(result))

  # Check TAD values
  expect_equal(result$TAD, c(0, 1, 2, 0, 1, 2))
})


test_that("ensure_tad handles multiple administrations", {
  test_data <- tibble::tribble(
    ~ID, ~TIME, ~EVID, ~CMT, ~PARENT, ~DV,  ~AMT,
    1,   0,     1,     1,    "DRUG",  NA,  100,
    1,   1,     0,     2,    "DRUG",  10,  0,
    1,   2,     1,     1,    "DRUG",  NA,  100,
    1,   3,     0,     2,    "DRUG",  20,  0,
    1,   4,     0,     2,    "DRUG",  30,  0
  ) %>%
    nif()

  result <- ensure_tad(test_data)

  # Check TAD values after second administration
  expect_equal(result$TAD, c(0, 1, 0, 1, 2))
})


test_that("ensure_tad handles observations before first dose", {
  test_data <- tibble::tribble(
    ~ID, ~TIME, ~EVID, ~CMT, ~PARENT, ~DV, ~AMT,
    1,   -1,    0,     2,    "DRUG",  5,   0,
    1,   0,     1,     1,    "DRUG",  NA,  100,
    1,   1,     0,     2,    "DRUG",  10,  0,
    1,   2,     0,     2,    "DRUG",  20,  0
  ) %>%
    nif()

  result <- ensure_tad(test_data)

  # Check that pre-dose observation has negative TAD
  expect_equal(result$TAD, c(-1, 0, 1, 2))
})


test_that("ensure_tad handles multiple parent compounds", {
  test_data <- tibble::tribble(
    ~ID, ~TIME, ~EVID, ~CMT, ~PARENT, ~DV,  ~AMT,
    1,   0,     1,     1,    "DRUG1", NA,  100,
    1,   1,     0,     2,    "DRUG1", 10,  0,
    1,   2,     0,     2,    "DRUG1", 20,  0,
    1,   0,     1,     3,    "DRUG2", NA,  100,
    1,   1,     0,     4,    "DRUG2", 30,  0,
    1,   2,     0,     4,    "DRUG2", 40,  0
  ) %>%
    nif()

  result <- ensure_tad(test_data)

  # Check TAD values for each parent
  expect_equal(result$TAD, c(0, 1, 2, 0, 1, 2))
})


test_that("ensure_tad handles empty data frame", {
  test_data <- tibble::tribble(
    ~ID, ~TIME, ~EVID, ~CMT, ~PARENT, ~DV, ~AMT
  ) %>%
    nif()

    result <- ensure_tad(test_data)

  expect_equal(nrow(result), 0)
  expect_true("TAD" %in% names(result))
})


test_that("ensure_tad preserves original data", {
  test_data <- tibble::tribble(
    ~ID, ~TIME, ~EVID, ~CMT, ~PARENT, ~DV,  ~EXTRA, ~AMT,
    1,   0,     1,     1,    "DRUG",  NA,   "A",    100,
    1,   1,     0,     2,    "DRUG",  10,   "B",    0,
    1,   2,     0,     2,    "DRUG",  20,   "C",    0
  ) %>%
    nif()

  result <- ensure_tad(test_data)

  # Check that original columns are preserved
  expect_true(all(names(test_data) %in% names(result)))
  expect_equal(result$EXTRA, test_data$EXTRA)
})


test_that("ensure_tad handles NA values in TIME", {
  test_data <- tibble::tribble(
    ~ID, ~TIME, ~EVID, ~CMT, ~PARENT, ~DV,  ~AMT,
    1,   0,     1,     1,    "DRUG",  NA,   100,
    1,   NA,    0,     2,    "DRUG",  10,   0,
    1,   2,     0,     2,    "DRUG",  20,   0
  ) %>%
    nif()

  result <- ensure_tad(test_data)

  # Check that NA in TIME results in NA in TAD
  expect_true(is.na(result$TAD[3]))
})


test_that("ensure_tad returns a nif object", {
  test_data <- tibble::tribble(
    ~ID, ~TIME, ~EVID, ~CMT, ~PARENT, ~DV,  ~AMT,
    1,   0,     1,     1,    "DRUG",  NA,   100,
    1,   1,     0,     2,    "DRUG",  10,   0
  ) %>%
    nif()

  result <- ensure_tad(test_data)

  expect_s3_class(result, "nif")
})


test_that("ensure_tad handles non-nif input", {
  test_data <- tibble::tribble(
    ~ID, ~TIME, ~EVID, ~CMT, ~PARENT, ~DV,  ~AMT,
    1,   0,     1,     1,    "DRUG",  NA,   100
  )

  expect_error(ensure_tad(test_data), "Input must be a NIF object")
})


test_that("ensure_tad handles existing TAD column", {
  test_data <- tibble::tribble(
    ~ID, ~TIME, ~EVID, ~CMT, ~PARENT, ~DV,  ~TAD, ~AMT,
    1,   0,     1,     1,    "DRUG",  NA,   0,    100,
    1,   1,     0,     2,    "DRUG",  10,   1,    0
  ) %>%
    nif()

  result <- ensure_tad(test_data)

  # Check that original TAD values are preserved
  expect_equal(result$TAD, c(0, 1))
})

