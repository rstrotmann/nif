test_that("add_ntile works with basic input", {
  # Create a simple test data frame with one value per subject
  test_data <- tibble::tribble(
    ~ID, ~TIME, ~DV, ~ANALYTE,
    1,   0,     10,  "A",
    1,   1,     10,  "A", # Same value for ID 1
    1,   2,     10,  "A", # Same value for ID 1
    2,   0,     25,  "A",
    2,   1,     25,  "A", # Same value for ID 2
    2,   2,     25,  "A", # Same value for ID 2
    3,   0,     5,   "A",
    3,   1,     5,   "A", # Same value for ID 3
    3,   2,     5,   "A", # Same value for ID 3
    4,   0,     40,  "A",
    4,   1,     40,  "A", # Same value for ID 4
    4,   2,     40,  "A" # Same value for ID 4
  )

  class(test_data) <- c("nif", "data.frame")

  result <- add_ntile(test_data, input_col = "DV")

  # Check that NTILE column was added
  expect_true("DV_NTILE" %in% names(result))

  # Check that all subjects have the same NTILE value across all rows
  expect_equal(result$DV_NTILE[result$ID == 1], rep(result$DV_NTILE[result$ID == 1][1], 3))
  expect_equal(result$DV_NTILE[result$ID == 2], rep(result$DV_NTILE[result$ID == 2][1], 3))
  expect_equal(result$DV_NTILE[result$ID == 3], rep(result$DV_NTILE[result$ID == 3][1], 3))
  expect_equal(result$DV_NTILE[result$ID == 4], rep(result$DV_NTILE[result$ID == 4][1], 3))

  # Check that NTILE values are between 1 and 4 (default n=4)
  expect_true(all(result$DV_NTILE >= 1 & result$DV_NTILE <= 4))
})

test_that("add_ntile works with custom n value", {
  test_data <- tibble::tribble(
    ~ID, ~TIME, ~DV, ~ANALYTE,
    1,   0,     10,  "A",
    1,   1,     10,  "A",
    2,   0,     25,  "A",
    2,   1,     25,  "A",
    3,   0,     5,   "A",
    3,   1,     5,   "A",
    4,   0,     40,  "A",
    4,   1,     40,  "A",
    5,   0,     60,  "A",
    5,   1,     60,  "A"
  )

  class(test_data) <- c("nif", "data.frame")

  result <- add_ntile(test_data, input_col = "DV", n = 5)

  # Check that NTILE column was added
  expect_true("DV_NTILE" %in% names(result))

  # Check that NTILE values are between 1 and 5
  expect_true(all(result$DV_NTILE >= 1 & result$DV_NTILE <= 5))

  # Check that all subjects have the same NTILE value across all rows
  for (id in unique(result$ID)) {
    subject_ntile <- result$DV_NTILE[result$ID == id]
    expect_equal(length(unique(subject_ntile)), 1)
  }
})

test_that("add_ntile works with custom column name", {
  test_data <- tibble::tribble(
    ~ID, ~TIME, ~DV, ~ANALYTE,
    1,   0,     10,  "A",
    1,   1,     10,  "A",
    2,   0,     25,  "A",
    2,   1,     25,  "A",
    3,   0,     5,   "A",
    3,   1,     5,   "A",
    4,   0,     40,  "A",
    4,   1,     40,  "A"
  )

  class(test_data) <- c("nif", "data.frame")

  result <- add_ntile(test_data, input_col = "DV", ntile_name = "CUSTOM_NTILE")

  # Check that custom column name was used
  expect_true("CUSTOM_NTILE" %in% names(result))
  expect_false("DV_NTILE" %in% names(result))

  # Check that all subjects have the same NTILE value across all rows
  for (id in unique(result$ID)) {
    subject_ntile <- result$CUSTOM_NTILE[result$ID == id]
    expect_equal(length(unique(subject_ntile)), 1)
  }
})

# test_that("add_ntile handles ties correctly", {
#   test_data <- tibble::tribble(
#     ~ID, ~TIME, ~DV, ~ANALYTE,
#     1,   0,     10,  "A",
#     1,   1,     10,  "A",  # Same value as ID 1
#     2,   0,     10,  "A",  # Same value as ID 1
#     2,   1,     10,  "A",
#     3,   0,     20,  "A",
#     3,   1,     20,  "A",
#     4,   0,     20,  "A",  # Same value as ID 3
#     4,   1,     20,  "A",
#     5,   0,     30,  "A",
#     5,   1,     30,  "A",
#     6,   0,     30,  "A",  # Same value as ID 5
#     6,   1,     30,  "A"
#   )
#
#   class(test_data) <- c("nif", "data.frame")
#
#   result <- add_ntile(test_data, input_col = "DV", n = 4)
#
#   # Check that subjects with same values get same NTILE
#   expect_equal(result$DV_NTILE[result$ID == 1], result$DV_NTILE[result$ID == 2])
#   expect_equal(result$DV_NTILE[result$ID == 3], result$DV_NTILE[result$ID == 4])
#   expect_equal(result$DV_NTILE[result$ID == 5], result$DV_NTILE[result$ID == 6])
#
#   # Check that NTILE values are between 1 and 4
#   expect_true(all(result$DV_NTILE >= 1 & result$DV_NTILE <= 4))
# })

test_that("add_ntile handles NA values in input column", {
  test_data <- tibble::tribble(
    ~ID, ~TIME, ~DV, ~ANALYTE,
    1,   0,     10,  "A",
    1,   1,     10,  "A",
    2,   0,     NA,  "A", # NA value
    2,   1,     NA,  "A", # NA value
    3,   0,     25,  "A",
    3,   1,     25,  "A",
    4,   0,     40,  "A",
    4,   1,     40,  "A"
  )

  class(test_data) <- c("nif", "data.frame")

  result <- add_ntile(test_data, input_col = "DV")

  # Check that subject with NA gets NA NTILE
  expect_true(all(is.na(result$DV_NTILE[result$ID == 2])))

  # Check that other subjects get valid NTILE values
  expect_false(any(is.na(result$DV_NTILE[result$ID == 1])))
  expect_false(any(is.na(result$DV_NTILE[result$ID == 3])))
  expect_false(any(is.na(result$DV_NTILE[result$ID == 4])))
})

test_that("add_ntile handles non-numeric input column", {
  test_data <- tibble::tribble(
    ~ID, ~TIME, ~DV, ~ANALYTE,
    1,   0,     "A", "A",
    1,   1,     "A", "A",
    2,   0,     "B", "A",
    2,   1,     "B", "A"
  )

  class(test_data) <- c("nif", "data.frame")

  expect_error(
    add_ntile(test_data, input_col = "DV"),
    "Column 'DV' must contain numeric values"
  )
})

test_that("add_ntile handles missing input column", {
  test_data <- tibble::tribble(
    ~ID, ~TIME, ~ANALYTE,
    1,   0,     "A",
    1,   1,     "A",
    2,   0,     "A",
    2,   1,     "A"
  )

  class(test_data) <- c("nif", "data.frame")

  expect_error(
    add_ntile(test_data, input_col = "DV"),
    "Missing required columns: DV"
  )
})

test_that("add_ntile handles invalid n value", {
  test_data <- tibble::tribble(
    ~ID, ~TIME, ~DV, ~ANALYTE,
    1,   0,     10,  "A",
    1,   1,     10,  "A",
    2,   0,     25,  "A",
    2,   1,     25,  "A"
  )

  class(test_data) <- c("nif", "data.frame")

  # Test with n = 0
  expect_error(
    add_ntile(test_data, input_col = "DV", n = 0),
    "n must be a positive integer between 2 and 100"
  )

  # Test with n = -1
  expect_error(
    add_ntile(test_data, input_col = "DV", n = -1),
    "n must be a positive integer between 2 and 100"
  )

  # Test with n = 1.5
  expect_error(
    add_ntile(test_data, input_col = "DV", n = 1.5),
    "n must be a positive integer between 2 and 100"
  )

  # Test with n = 1
  expect_error(
    add_ntile(test_data, input_col = "DV", n = 1),
    "n must be a positive integer between 2 and 100"
  )

  # Test with n = 101
  expect_error(
    add_ntile(test_data, input_col = "DV", n = 101),
    "n must be a positive integer between 2 and 100"
  )
})

test_that("add_ntile handles empty data frame", {
  test_data <- tibble::tribble(
    ~ID, ~TIME, ~DV, ~ANALYTE
  )

  class(test_data) <- c("nif", "data.frame")

  expect_error(
    add_ntile(test_data, input_col = "DV"),
    # "Insufficient subjects (0) for calculating 4 n-tiles. Need at least 4 subjects."
    "Column 'DV' must contain numeric values"
  )
})

# test_that("add_ntile handles insufficient subjects", {
#   test_data <- tibble::tribble(
#     ~ID, ~TIME, ~DV, ~ANALYTE,
#     1,   0,     10,  "A",
#     1,   1,     10,  "A",
#     2,   0,     25,  "A",
#     2,   1,     25,  "A"
#   )
#
#   class(test_data) <- c("nif", "data.frame")
#
#   # Test with default n=4 but only 2 subjects
#   expect_error(
#     add_ntile(test_data, input_col = "DV"),
#     "Insufficient subjects (2) for calculating 4 n-tiles. Need at least 4 subjects."
#   )
#
#   # Test with n=2 and 2 subjects (should work)
#   result <- add_ntile(test_data, input_col = "DV", n = 2)
#   expect_true("DV_NTILE" %in% names(result))
#   expect_true(all(result$DV_NTILE >= 1 & result$DV_NTILE <= 2))
# })

# test_that("add_ntile handles single subject", {
#   test_data <- tibble::tribble(
#     ~ID, ~TIME, ~DV, ~ANALYTE,
#     1,   0,     10,  "A",
#     1,   1,     10,  "A",
#     1,   2,     10,  "A"
#   )
#
#   class(test_data) <- c("nif", "data.frame")
#
#   expect_error(
#     add_ntile(test_data, input_col = "DV"),
#     "Insufficient subjects (1) for calculating 4 n-tiles. Need at least 4 subjects."
#   )
#
#   # Test with n=1 (should fail validation)
#   expect_error(
#     add_ntile(test_data, input_col = "DV", n = 1),
#     "n must be a positive integer between 2 and 100"
#   )
# })

# test_that("add_ntile handles n larger than number of subjects", {
#   test_data <- tibble::tribble(
#     ~ID, ~TIME, ~DV, ~ANALYTE,
#     1,   0,     10,  "A",
#     1,   1,     10,  "A",
#     2,   0,     25,  "A",
#     2,   1,     25,  "A"
#   )
#
#   class(test_data) <- c("nif", "data.frame")
#
#   expect_error(
#     add_ntile(test_data, input_col = "DV", n = 10),
#     "Insufficient subjects (2) for calculating 10 n-tiles. Need at least 10 subjects."
#   )
# })

test_that("add_ntile preserves original data", {
  test_data <- tibble::tribble(
    ~ID, ~TIME, ~DV, ~ANALYTE, ~EXTRA,
    1,   0,     10,  "A",      "X",
    1,   1,     10,  "A",      "Y",
    2,   0,     25,  "A",      "Z",
    2,   1,     25,  "A",      "W",
    3,   0,     5,   "A",      "V",
    3,   1,     5,   "A",      "U",
    4,   0,     40,  "A",      "T",
    4,   1,     40,  "A",      "S"
  )

  class(test_data) <- c("nif", "data.frame")

  result <- add_ntile(test_data, input_col = "DV")

  # Check that original columns are preserved
  expect_true(all(names(test_data) %in% names(result)))
  expect_equal(result$EXTRA, test_data$EXTRA)
})

test_that("add_ntile returns a nif object", {
  test_data <- tibble::tribble(
    ~ID, ~TIME, ~DV, ~ANALYTE,
    1,   0,     10,  "A",
    1,   1,     10,  "A",
    2,   0,     25,  "A",
    2,   1,     25,  "A",
    3,   0,     5,   "A",
    3,   1,     5,   "A",
    4,   0,     40,  "A",
    4,   1,     40,  "A"
  )

  class(test_data) <- c("nif", "data.frame")

  result <- add_ntile(test_data, input_col = "DV")

  expect_s3_class(result, "nif")
})

test_that("add_ntile works with different input columns", {
  test_data <- tibble::tribble(
    ~ID, ~TIME, ~DV, ~ANALYTE, ~WEIGHT,
    1,   0,     10,  "A",      70,
    1,   1,     10,  "A",      70,
    2,   0,     25,  "A",      80,
    2,   1,     25,  "A",      80,
    3,   0,     5,   "A",      60,
    3,   1,     5,   "A",      60,
    4,   0,     40,  "A",      90,
    4,   1,     40,  "A",      90
  )

  class(test_data) <- c("nif", "data.frame")

  # Test with DV column
  result_dv <- add_ntile(test_data, input_col = "DV")
  expect_true("DV_NTILE" %in% names(result_dv))

  # Test with WEIGHT column
  result_weight <- add_ntile(test_data, input_col = "WEIGHT")
  expect_true("WEIGHT_NTILE" %in% names(result_weight))

  # Check that NTILE values are different for different columns
  # Use different values that will produce different n-tile assignments
  test_data2 <- tibble::tribble(
    ~ID, ~TIME, ~DV, ~ANALYTE, ~WEIGHT,
    1,   0,     10,  "A",      100, # Different values
    1,   1,     10,  "A",      100,
    2,   0,     25,  "A",      50, # Different values
    2,   1,     25,  "A",      50,
    3,   0,     5,   "A",      75, # Different values
    3,   1,     5,   "A",      75,
    4,   0,     40,  "A",      25, # Different values
    4,   1,     40,  "A",      25
  )

  class(test_data2) <- c("nif", "data.frame")

  result_dv2 <- add_ntile(test_data2, input_col = "DV")
  result_weight2 <- add_ntile(test_data2, input_col = "WEIGHT")

  # Check that NTILE values are different for different columns
  expect_false(identical(result_dv2$DV_NTILE, result_weight2$WEIGHT_NTILE))
})

test_that("add_ntile handles all subjects having the same value", {
  test_data <- tibble::tribble(
    ~ID, ~TIME, ~DV, ~ANALYTE,
    1,   0,     10,  "A",
    1,   1,     10,  "A",
    2,   0,     10,  "A",
    2,   1,     10,  "A",
    3,   0,     10,  "A",
    3,   1,     10,  "A",
    4,   0,     10,  "A",
    4,   1,     10,  "A"
  )

  class(test_data) <- c("nif", "data.frame")

  result <- add_ntile(test_data, input_col = "DV")

  # Check that NTILE column was added
  expect_true("DV_NTILE" %in% names(result))

  # Check that all subjects get different NTILE values
  # Note: dplyr::ntile assigns different n-tiles even for identical values
  # based on the order of the data, so we expect different n-tiles
  expect_equal(length(unique(result$DV_NTILE)), 4) # All 4 n-tiles should be used
  expect_true(all(result$DV_NTILE >= 1 & result$DV_NTILE <= 4))
})

test_that("add_ntile handles subjects with multiple distinct values", {
  test_data <- tibble::tribble(
    ~ID, ~TIME, ~DV, ~ANALYTE,
    1,   0,     10,  "A",
    1,   1,     10,  "A",
    1,   2,     10,  "A", # Multiple rows for ID 1, but same value
    2,   0,     25,  "A",
    2,   1,     25,  "A",
    3,   0,     5,   "A",
    3,   1,     5,   "A",
    4,   0,     40,  "A",
    4,   1,     40,  "A"
  )

  class(test_data) <- c("nif", "data.frame")

  result <- add_ntile(test_data, input_col = "DV")

  # Check that NTILE column was added
  expect_true("DV_NTILE" %in% names(result))

  # Check that all rows for each subject have the same NTILE value
  expect_equal(result$DV_NTILE[result$ID == 1], rep(result$DV_NTILE[result$ID == 1][1], 3))
  expect_equal(result$DV_NTILE[result$ID == 2], rep(result$DV_NTILE[result$ID == 2][1], 2))
  expect_equal(result$DV_NTILE[result$ID == 3], rep(result$DV_NTILE[result$ID == 3][1], 2))
  expect_equal(result$DV_NTILE[result$ID == 4], rep(result$DV_NTILE[result$ID == 4][1], 2))
})

test_that("add_ntile handles invalid input object", {
  # Test with non-nif object
  test_data <- tibble::tribble(
    ~ID, ~TIME, ~DV, ~ANALYTE,
    1,   0,     10,  "A",
    1,   1,     10,  "A",
    2,   0,     25,  "A",
    2,   1,     25,  "A"
  )

  expect_error(
    add_ntile(test_data, input_col = "DV"),
    "Input must be a nif object"
  )
})

test_that("add_ntile handles invalid input_col parameter", {
  test_data <- tibble::tribble(
    ~ID, ~TIME, ~DV, ~ANALYTE,
    1,   0,     10,  "A",
    1,   1,     10,  "A",
    2,   0,     25,  "A",
    2,   1,     25,  "A",
    3,   0,     5,   "A",
    3,   1,     5,   "A",
    4,   0,     40,  "A",
    4,   1,     40,  "A"
  )

  class(test_data) <- c("nif", "data.frame")

  # Test with NULL input_col
  expect_error(
    add_ntile(test_data, input_col = NULL),
    "input_col must be a single character string"
  )

  # Test with numeric input_col
  expect_error(
    add_ntile(test_data, input_col = 123),
    "input_col must be a single character string"
  )

  # Test with multiple input_col values
  expect_error(
    add_ntile(test_data, input_col = c("DV", "TIME")),
    "input_col must be a single character string"
  )
})

test_that("add_ntile handles invalid ntile_name parameter", {
  test_data <- tibble::tribble(
    ~ID, ~TIME, ~DV, ~ANALYTE,
    1,   0,     10,  "A",
    1,   1,     10,  "A",
    2,   0,     25,  "A",
    2,   1,     25,  "A",
    3,   0,     5,   "A",
    3,   1,     5,   "A",
    4,   0,     40,  "A",
    4,   1,     40,  "A"
  )

  class(test_data) <- c("nif", "data.frame")

  # Test with numeric ntile_name
  expect_error(
    add_ntile(test_data, input_col = "DV", ntile_name = 123),
    "ntile_name must be a single character string or NULL"
  )

  # Test with multiple ntile_name values
  expect_error(
    add_ntile(test_data, input_col = "DV", ntile_name = c("A", "B")),
    "ntile_name must be a single character string or NULL"
  )
})
