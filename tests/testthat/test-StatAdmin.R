test_that("StatAdmin works with valid inputs", {
  # Create test data
  test_data <- tibble::tribble(
    ~x, ~admin,
    1,  TRUE,
    2,  FALSE,
    3,  TRUE,
    4,  FALSE,
    5,  TRUE
  )

  # Test with logical admin column
  result <- StatAdmin$compute_group(test_data, NULL)
  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 3)
  expect_equal(result$xintercept, c(1, 3, 5))

  # Test with numeric admin column (0/1)
  test_data_numeric <- test_data
  test_data_numeric$admin <- as.numeric(test_data$admin)
  result_numeric <- StatAdmin$compute_group(test_data_numeric, NULL)
  expect_equal(result_numeric$xintercept, c(1, 3, 5))
})


test_that("StatAdmin handles empty data correctly", {
  # Empty data frame with correct columns
  empty_data <- tibble::tribble(
    ~x, ~admin
  )

  result <- StatAdmin$compute_group(empty_data, NULL)
  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 0)
  expect_equal(names(result), "xintercept")
})


test_that("StatAdmin handles NA values correctly", {
  # Data with NA values
  test_data <- tibble::tribble(
    ~x, ~admin,
    1,  TRUE,
    2,  NA,
    3,  TRUE,
    4,  NA,
    5,  TRUE
  )

  # Should warn about NA values
  expect_warning(
    result <- StatAdmin$compute_group(test_data, NULL),
    "NA values found in admin column"
  )

  expect_equal(result$xintercept, c(1, 3, 5))
})


test_that("StatAdmin handles no administration points", {
  # Data with no TRUE values
  test_data <- tibble::tribble(
    ~x, ~admin,
    1,  FALSE,
    2,  FALSE,
    3,  FALSE
  )

  result <- StatAdmin$compute_group(test_data, NULL)
  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 0)
  expect_equal(names(result), "xintercept")
})


test_that("StatAdmin throws appropriate errors", {
  # Test with non-data.frame input
  expect_error(
    StatAdmin$compute_group(list(x = 1, admin = TRUE), NULL),
    "data must be a data frame"
  )

  # Test with missing required columns
  test_data_missing_x <- tibble::tribble(
    ~admin,
    TRUE
  )
  expect_error(
    StatAdmin$compute_group(test_data_missing_x, NULL),
    "Missing required columns: x"
  )

  test_data_missing_admin <- tibble::tribble(
    ~x,
    1
  )
  expect_error(
    StatAdmin$compute_group(test_data_missing_admin, NULL),
    "Missing required columns: admin"
  )

  # Test with invalid admin column type
  test_data_invalid_admin <- tibble::tribble(
    ~x, ~admin,
    1,  "TRUE"
  )
  expect_error(
    StatAdmin$compute_group(test_data_invalid_admin, NULL),
    "admin column must be logical or numeric"
  )
})


test_that("StatAdmin works with different numeric admin values", {
  # Test with various numeric values
  test_data <- tibble::tribble(
    ~x, ~admin,
    1,  1,
    2,  0,
    3,  0.5,
    4,  0,
    5,  1
  )

  result <- StatAdmin$compute_group(test_data, NULL)
  expect_equal(result$xintercept, c(1, 3, 5))  # Only 1s should be treated as TRUE
})
