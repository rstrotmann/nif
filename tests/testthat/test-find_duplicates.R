test_that("find_duplicates works with default fields", {
  # Create test data
  df <- tibble::tribble(
    ~ID, ~TIME, ~ANALYTE, ~DV,
    "001", 0, "A", 10,
    "001", 0, "A", 10,
    "002", 24, "B", 20,
    "002", 24, "B", 20,
    "003", 48, "C", 30
  )

  # Test finding duplicates
  result <- find_duplicates(df)

  # Check structure of result
  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 4) # 2 duplicate pairs
  expect_true(all(
    c("ID", "TIME", "ANALYTE", "DV", "count") %in% names(result)
  ))

  # Check specific values
  expect_equal(result$count, c(2, 2, 2, 2))
  expect_equal(unique(result$ID), c("001", "002"))
})

test_that("find_duplicates works with custom fields", {
  # Create test data
  df <- tibble::tribble(
    ~ID,      ~TIME, ~DV,
    "001",    0,     10,
    "001",    0,     10,
    "002",    24,    20,
    "002",    24,    20,
    "003",    48,    30
  )

  # Test finding duplicates
  result <- find_duplicates(df, fields = c("ID", "TIME"))

  # Check structure of result
  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 4) # 2 duplicate pairs
  expect_true(all(c("ID", "TIME", "count", "DV") %in% names(result)))
})


# test_that("find_duplicates returns count only when requested", {
#   # Create test data
#   df <- tibble::tribble(
#     ~ID,  ~TIME, ~ANALYTE,
#     "001", 0,     "A",
#     "001", 0,     "A",
#     "002", 24,    "B",
#     "002", 24,    "B",
#     "003", 48,    "C"
#   )
#
#   # Test count only
#   result <- find_duplicates(df, count_only = TRUE)
#   expect_equal(result, 2)  # 2 duplicate pairs
# })


test_that("find_duplicates returns NULL when no duplicates exist", {
  # Create test data with no duplicates
  df <- tibble::tribble(
    ~ID, ~TIME, ~ANALYTE,
    "001", 0, "A",
    "002", 24, "B",
    "003", 48, "C"
  )

  # Test no duplicates
  result <- find_duplicates(df)
  expect_null(result)
})


# test_that("find_duplicates works with return_all_cols = FALSE", {
#   # Create test data
#   df <- tibble::tribble(
#     ~ID,  ~TIME, ~ANALYTE, ~DV, ~DOSE,
#     "001", 0,     "A",      10,  100,
#     "001", 0,     "A",      10,  100,
#     "002", 24,    "B",      20,  200,
#     "002", 24,    "B",      20,  200,
#     "003", 48,    "C",      30,  300
#   )
#
#   # Test with return_all_cols = FALSE and additional columns
#   result <- find_duplicates(df, return_all_cols = FALSE, additional_cols = "DOSE")
#
#   # Check structure of result
#   expect_s3_class(result, "data.frame")
#   expect_equal(nrow(result), 4)  # 2 duplicate pairs
#   expect_true(all(c("ID", "TIME", "ANALYTE", "count", "DOSE") %in% names(result)))
#   expect_false("DV" %in% names(result))  # DV should not be included
# })


test_that("find_duplicates handles missing fields", {
  # Create test data
  df <- tibble::tribble(
    ~ID, ~TIME,
    "001", 0,
    "001", 0,
    "002", 24,
    "002", 24,
    "003", 48
  )

  # Test with missing field
  expect_error(
    find_duplicates(df),
    "Field not found in input: ANALYTE"
  )
})


test_that("find_duplicates handles NA values", {
  # Create test data with NA values
  df <- tibble::tribble(
    ~ID, ~TIME, ~ANALYTE,
    "001", 0, "A",
    "001", 0, "A",
    "002", 24, "B",
    "002", 24, "B",
    "003", NA, "C"
  )

  # Test with NA values
  result <- find_duplicates(df)

  # Check structure of result
  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 4) # 2 duplicate pairs
  expect_true(all(c("ID", "TIME", "ANALYTE", "count") %in% names(result)))
})


test_that("find_duplicates handles empty data frame", {
  # Create empty data frame
  df <- tibble::tribble(
    ~ID, ~TIME, ~ANALYTE
  )

  # Test empty data frame
  result <- find_duplicates(df)
  expect_null(result)
})
