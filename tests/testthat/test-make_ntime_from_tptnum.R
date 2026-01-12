test_that("make_ntime_from_tptnum works with domain specified", {
  test_data <- tibble::tribble(
    ~USUBJID, ~DOMAIN, ~PCTPTNUM,
    1, "PC", 1,
    1, "PC", 2,
    1, "PC", 3,
    2, "PC", 1,
    2, "PC", 4
  )

  result <- make_ntime_from_tptnum(test_data, domain = "PC")

  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 4) # Should have 4 unique values: 1, 2, 3, 4
  expect_equal(names(result), c("PCTPTNUM", "NTIME"))
  expect_equal(result$PCTPTNUM, c(1, 2, 3, 4))
  expect_equal(result$NTIME, c(1, 2, 3, 4))
})


test_that("make_ntime_from_tptnum auto-detects domain when NULL", {
  test_data <- tibble::tribble(
    ~USUBJID, ~DOMAIN, ~PCTPTNUM,
    1, "PC", 1,
    1, "PC", 2,
    2, "PC", 3
  )

  result <- make_ntime_from_tptnum(test_data, domain = NULL)

  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 3)
  expect_equal(names(result), c("PCTPTNUM", "NTIME"))
  expect_equal(result$PCTPTNUM, c(1, 2, 3))
  expect_equal(result$NTIME, c(1, 2, 3))
})


test_that("make_ntime_from_tptnum works with different domains", {
  # Test with LB domain
  test_data_lb <- tibble::tribble(
    ~USUBJID, ~DOMAIN, ~LBTPTNUM,
    1, "LB", 1,
    1, "LB", 2,
    2, "LB", 1
  )

  result_lb <- make_ntime_from_tptnum(test_data_lb, domain = "LB")

  expect_s3_class(result_lb, "data.frame")
  expect_equal(nrow(result_lb), 2) # Unique values: 1, 2
  expect_equal(names(result_lb), c("LBTPTNUM", "NTIME"))
  expect_equal(result_lb$LBTPTNUM, c(1, 2))
  expect_equal(result_lb$NTIME, c(1, 2))

  # Test with lowercase domain (should be uppercased)
  test_data_pc_lower <- tibble::tribble(
    ~USUBJID, ~DOMAIN, ~PCTPTNUM,
    1, "pc", 1,
    1, "pc", 2
  )

  result_pc_lower <- make_ntime_from_tptnum(test_data_pc_lower, domain = "pc")

  expect_s3_class(result_pc_lower, "data.frame")
  expect_equal(names(result_pc_lower), c("PCTPTNUM", "NTIME"))
})


test_that("make_ntime_from_tptnum handles unsorted values correctly", {
  test_data <- tibble::tribble(
    ~USUBJID, ~DOMAIN, ~PCTPTNUM,
    1, "PC", 5,
    1, "PC", 1,
    1, "PC", 3,
    2, "PC", 2,
    2, "PC", 4
  )

  result <- make_ntime_from_tptnum(test_data, domain = "PC")

  # Should be sorted
  expect_equal(result$PCTPTNUM, c(1, 2, 3, 4, 5))
  expect_equal(result$NTIME, c(1, 2, 3, 4, 5))
})


test_that("make_ntime_from_tptnum handles duplicate values correctly", {
  test_data <- tibble::tribble(
    ~USUBJID, ~DOMAIN, ~PCTPTNUM,
    1, "PC", 1,
    1, "PC", 1,
    1, "PC", 1,
    2, "PC", 2,
    2, "PC", 2,
    3, "PC", 3
  )

  result <- make_ntime_from_tptnum(test_data, domain = "PC")

  # Should only have unique values
  expect_equal(nrow(result), 3)
  expect_equal(result$PCTPTNUM, c(1, 2, 3))
  expect_equal(result$NTIME, c(1, 2, 3))
})


test_that("make_ntime_from_tptnum returns NULL when TPTNUM column doesn't exist", {
  test_data <- tibble::tribble(
    ~USUBJID, ~DOMAIN,
    1, "PC",
    1, "PC",
    2, "PC"
  )

  result <- make_ntime_from_tptnum(test_data, domain = "PC")

  # If column doesn't exist, [[tpt_name]] returns NULL
  # and all(is.null(NULL)) is TRUE, so should return NULL
  expect_null(result)
})


test_that("make_ntime_from_tptnum handles all NA values in TPTNUM", {
  test_data <- tibble::tribble(
    ~USUBJID, ~DOMAIN, ~PCTPTNUM,
    1, "PC", NA,
    1, "PC", NA,
    2, "PC", NA
  )

  result <- make_ntime_from_tptnum(test_data, domain = "PC")

  # Note: all(is.null(NA)) is FALSE, so function won't return NULL
  # It will create a data frame with NA values
  # The function checks all(is.null(tptnum)), which is TRUE only if
  # the column doesn't exist (returns NULL), not if values are NA
  expect_s3_class(result, "data.frame")
  expect_true(any(is.na(result$PCTPTNUM)))
})


test_that("make_ntime_from_tptnum handles mixed NULL and valid values", {
  test_data <- tibble::tribble(
    ~USUBJID, ~DOMAIN, ~PCTPTNUM,
    1, "PC", 1,
    1, "PC", NA,
    1, "PC", 2,
    2, "PC", NA,
    2, "PC", 3
  )

  result <- make_ntime_from_tptnum(test_data, domain = "PC")

  expect_s3_class(result, "data.frame")
  # Should include NA if it's a unique value
  expect_true(any(is.na(result$PCTPTNUM)) || nrow(result) == 3)
  # Valid values should be present
  expect_true(all(c(1, 2, 3) %in% result$PCTPTNUM))
})


test_that("make_ntime_from_tptnum handles single value correctly", {
  test_data <- tibble::tribble(
    ~USUBJID, ~DOMAIN, ~PCTPTNUM,
    1, "PC", 5,
    2, "PC", 5,
    3, "PC", 5
  )

  result <- make_ntime_from_tptnum(test_data, domain = "PC")

  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 1)
  expect_equal(result$PCTPTNUM, 5)
  expect_equal(result$NTIME, 5)
})


test_that("make_ntime_from_tptnum handles empty data frame correctly", {
  test_data <- tibble::tibble(
    USUBJID = character(0),
    DOMAIN = character(0),
    PCTPTNUM = numeric(0)
  )

  result <- make_ntime_from_tptnum(test_data, domain = "PC")

  # Should return NULL or empty data frame depending on implementation
  # Based on the function logic, unique() of empty vector would be empty,
  # and all(is.null(empty)) would be FALSE, so it might return empty df
  # But let's test what actually happens
  expect_true(is.null(result) || nrow(result) == 0)
})


test_that("make_ntime_from_tptnum handles multiple domains correctly when domain is NULL", {
  test_data <- tibble::tribble(
    ~USUBJID, ~DOMAIN, ~PCTPTNUM, ~LBTPTNUM,
    1, "PC", 1, NA,
    1, "PC", 2, NA,
    2, "LB", NA, 10,
    2, "LB", NA, 20
  )

  result <- make_ntime_from_tptnum(test_data, domain = NULL)

  # Should use first unique domain (PC)
  expect_s3_class(result, "data.frame")
  expect_equal(names(result), c("PCTPTNUM", "NTIME"))
  expect_equal(result$PCTPTNUM, c(1, 2, NA))
})


test_that("make_ntime_from_tptnum handles numeric TPTNUM values correctly", {
  test_data <- tibble::tribble(
    ~USUBJID, ~DOMAIN, ~PCTPTNUM,
    1, "PC", 0,
    1, "PC", 0.5,
    1, "PC", 1,
    1, "PC", 1.5,
    2, "PC", 2
  )

  result <- make_ntime_from_tptnum(test_data, domain = "PC")

  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 5)
  expect_equal(result$PCTPTNUM, c(0, 0.5, 1, 1.5, 2))
  expect_equal(result$NTIME, c(0, 0.5, 1, 1.5, 2))
})


test_that("make_ntime_from_tptnum handles negative values correctly", {
  test_data <- tibble::tribble(
    ~USUBJID, ~DOMAIN, ~PCTPTNUM,
    1, "PC", -1,
    1, "PC", 0,
    1, "PC", 1
  )

  result <- make_ntime_from_tptnum(test_data, domain = "PC")

  expect_s3_class(result, "data.frame")
  expect_equal(result$PCTPTNUM, c(-1, 0, 1))
  expect_equal(result$NTIME, c(-1, 0, 1))
})


test_that("make_ntime_from_tptnum handles character TPTNUM values correctly", {
  test_data <- tibble::tribble(
    ~USUBJID, ~DOMAIN, ~PCTPTNUM,
    1, "PC", "1",
    1, "PC", "2",
    1, "PC", "3"
  )

  result <- make_ntime_from_tptnum(test_data, domain = "PC")

  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 3)
  expect_equal(result$PCTPTNUM, c("1", "2", "3"))
  expect_equal(result$NTIME, c("1", "2", "3"))
})


test_that("make_ntime_from_tptnum preserves NTIME as identical to TPTNUM", {
  test_data <- tibble::tribble(
    ~USUBJID, ~DOMAIN, ~PCTPTNUM,
    1, "PC", 10,
    1, "PC", 20,
    1, "PC", 30
  )

  result <- make_ntime_from_tptnum(test_data, domain = "PC")

  # NTIME should be identical to TPTNUM (not converted)
  expect_identical(result$PCTPTNUM, result$NTIME)
})


test_that("make_ntime_from_tptnum handles missing DOMAIN column when domain is NULL", {
  test_data <- tibble::tribble(
    ~USUBJID, ~PCTPTNUM,
    1, 1,
    1, 2
  )

  # Should error or handle gracefully
  expect_warning(
    make_ntime_from_tptnum(test_data, domain = NULL)
  )
})


test_that("make_ntime_from_tptnum handles zero-length unique values", {
  # Test with empty vector after unique()
  test_data <- tibble::tibble(
    USUBJID = character(0),
    DOMAIN = character(0),
    PCTPTNUM = numeric(0)
  )

  result <- make_ntime_from_tptnum(test_data, domain = "PC")

  # unique() of empty vector returns empty vector of same type
  # all(is.null(empty_numeric_vector)) would be FALSE
  # So function would try to create data.frame, which might work or error
  # Let's test what actually happens
  expect_true(is.null(result) || (is.data.frame(result) && nrow(result) == 0))
})

