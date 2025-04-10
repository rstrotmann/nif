test_that("add_tad works with basic input", {
  # Create a simple test data frame
  test_data <- tibble::tribble(
    ~ID, ~TIME, ~EVID, ~PARENT, ~DV,
    1,   0,     1,     "DRUG",  NA,
    1,   1,     0,     "DRUG",  10,
    1,   2,     0,     "DRUG",  20,
    2,   0,     1,     "DRUG",  NA,
    2,   1,     0,     "DRUG",  30,
    2,   2,     0,     "DRUG",  40
  )

  class(test_data) <- c("nif", "data.frame")

  result <- add_tad(test_data)

  # Check that TAD was added
  expect_true("TAD" %in% names(result))

  # Check TAD values
  expect_equal(result$TAD, c(0, 1, 2, 0, 1, 2))
})

test_that("add_tad handles multiple administrations", {
  test_data <- tibble::tribble(
    ~ID, ~TIME, ~EVID, ~PARENT, ~DV,
    1,   0,     1,     "DRUG",  NA,
    1,   1,     0,     "DRUG",  10,
    1,   2,     1,     "DRUG",  NA,
    1,   3,     0,     "DRUG",  20,
    1,   4,     0,     "DRUG",  30
  )
  class(test_data) <- c("nif", "data.frame")

  result <- add_tad(test_data)

  # Check TAD values after second administration
  expect_equal(result$TAD, c(0, 1, 0, 1, 2))
})

test_that("add_tad handles observations before first dose", {
  test_data <- tibble::tribble(
    ~ID, ~TIME, ~EVID, ~PARENT, ~DV,
    1,   -1,    0,     "DRUG",  5,
    1,   0,     1,     "DRUG",  NA,
    1,   1,     0,     "DRUG",  10,
    1,   2,     0,     "DRUG",  20
  )
  class(test_data) <- c("nif", "data.frame")

  result <- add_tad(test_data)

  # Check that pre-dose observation has negative TAD
  expect_equal(result$TAD, c(NA, 0, 1, 2))
})

test_that("add_tad handles multiple parent compounds", {
  test_data <- tibble::tribble(
    ~ID, ~TIME, ~EVID, ~PARENT, ~DV,
    1,   0,     1,     "DRUG1", NA,
    1,   1,     0,     "DRUG1", 10,
    1,   2,     0,     "DRUG1", 20,
    1,   0,     1,     "DRUG2", NA,
    1,   1,     0,     "DRUG2", 30,
    1,   2,     0,     "DRUG2", 40
  )
  class(test_data) <- c("nif", "data.frame")

  result <- add_tad(test_data)

  # Check TAD values for each parent
  expect_equal(result$TAD, c(0, 1, 2, 0, 1, 2))
})

test_that("add_tad handles empty data frame", {
  test_data <- tibble::tribble(
    ~ID, ~TIME, ~EVID, ~PARENT, ~DV
  )
  class(test_data) <- c("nif", "data.frame")

  result <- add_tad(test_data)

  expect_equal(nrow(result), 0)
  expect_true("TAD" %in% names(result))
})

test_that("add_tad handles missing required columns", {
  test_data <- tibble::tribble(
    ~ID, ~TIME,
    1,   0,
    1,   1
  )
  class(test_data) <- c("nif", "data.frame")

  expect_error(add_tad(test_data), "Missing required columns")
})

test_that("add_tad preserves original data", {
  test_data <- tibble::tribble(
    ~ID, ~TIME, ~EVID, ~PARENT, ~DV, ~EXTRA,
    1,   0,     1,     "DRUG",  NA,  "A",
    1,   1,     0,     "DRUG",  10,  "B",
    1,   2,     0,     "DRUG",  20,  "C"
  )
  class(test_data) <- c("nif", "data.frame")

  result <- add_tad(test_data)

  # Check that original columns are preserved
  expect_true(all(names(test_data) %in% names(result)))
  expect_equal(result$EXTRA, test_data$EXTRA)
})

test_that("add_tad handles NA values in TIME", {
  test_data <- tibble::tribble(
    ~ID, ~TIME, ~EVID, ~PARENT, ~DV,
    1,   0,     1,     "DRUG",  NA,
    1,   NA,    0,     "DRUG",  10,
    1,   2,     0,     "DRUG",  20
  )
  class(test_data) <- c("nif", "data.frame")

  result <- add_tad(test_data)

  # Check that NA in TIME results in NA in TAD
  expect_true(is.na(result$TAD[3]))
})

test_that("add_tad returns a nif object", {
  test_data <- tibble::tribble(
    ~ID, ~TIME, ~EVID, ~PARENT, ~DV,
    1,   0,     1,     "DRUG",  NA,
    1,   1,     0,     "DRUG",  10
  )
  class(test_data) <- c("nif", "data.frame")

  result <- add_tad(test_data)

  expect_s3_class(result, "nif")
})
