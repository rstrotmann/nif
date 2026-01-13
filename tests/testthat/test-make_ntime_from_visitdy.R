test_that("make_ntime_from_visitdy works with domain specified", {
  test_data <- tibble::tribble(
    ~USUBJID, ~DOMAIN, ~VISITDY,
    1, "PC", 1,
    1, "PC", 2,
    1, "PC", 3,
    2, "PC", 1,
    2, "PC", 4
  )

  result <- make_ntime_from_visitdy(test_data, domain = "PC")

  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 4) # Should have 4 unique values: 1, 2, 3, 4
  expect_equal(names(result), c("VISITDY", "NTIME"))
  expect_equal(result$VISITDY, c(1, 2, 3, 4))
  # NTIME = (VISITDY - 1) * 24
  expect_equal(result$NTIME, c(0, 24, 48, 72))
})


test_that("make_ntime_from_visitdy uses default domain PC", {
  test_data <- tibble::tribble(
    ~USUBJID, ~DOMAIN, ~VISITDY,
    1, "PC", 1,
    1, "PC", 2,
    2, "PC", 3
  )

  result <- make_ntime_from_visitdy(test_data)

  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 3)
  expect_equal(names(result), c("VISITDY", "NTIME"))
  expect_equal(result$VISITDY, c(1, 2, 3))
  expect_equal(result$NTIME, c(0, 24, 48))
})


test_that("make_ntime_from_visitdy auto-detects domain when NULL", {
  test_data <- tibble::tribble(
    ~USUBJID, ~DOMAIN, ~VISITDY,
    1, "PC", 1,
    1, "PC", 2,
    2, "PC", 3
  )

  result <- make_ntime_from_visitdy(test_data, domain = NULL)

  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 3)
  expect_equal(names(result), c("VISITDY", "NTIME"))
  expect_equal(result$VISITDY, c(1, 2, 3))
  expect_equal(result$NTIME, c(0, 24, 48))
})


test_that("make_ntime_from_visitdy always uses VISITDY column regardless of domain", {
  # Test with PC domain
  test_data_pc <- tibble::tribble(
    ~USUBJID, ~DOMAIN, ~VISITDY,
    1, "PC", 1,
    1, "PC", 2
  )

  result_pc <- make_ntime_from_visitdy(test_data_pc, domain = "PC")

  expect_equal(names(result_pc), c("VISITDY", "NTIME"))
  expect_equal(result_pc$VISITDY, c(1, 2))
  expect_equal(result_pc$NTIME, c(0, 24))

  # Test with LB domain - should still use VISITDY, not LBDY
  test_data_lb <- tibble::tribble(
    ~USUBJID, ~DOMAIN, ~VISITDY,
    1, "LB", 1,
    1, "LB", 2,
    2, "LB", 1
  )

  result_lb <- make_ntime_from_visitdy(test_data_lb, domain = "LB")

  expect_s3_class(result_lb, "data.frame")
  expect_equal(nrow(result_lb), 2) # Unique values: 1, 2
  expect_equal(names(result_lb), c("VISITDY", "NTIME")) # Always VISITDY
  expect_equal(result_lb$VISITDY, c(1, 2))
  expect_equal(result_lb$NTIME, c(0, 24))

  # Test with lowercase domain
  test_data_pc_lower <- tibble::tribble(
    ~USUBJID, ~DOMAIN, ~VISITDY,
    1, "pc", 1,
    1, "pc", 2
  )

  result_pc_lower <- make_ntime_from_visitdy(test_data_pc_lower, domain = "pc")

  expect_s3_class(result_pc_lower, "data.frame")
  expect_equal(names(result_pc_lower), c("VISITDY", "NTIME"))
  expect_equal(result_pc_lower$VISITDY, c(1, 2))
  expect_equal(result_pc_lower$NTIME, c(0, 24))
})


test_that("make_ntime_from_visitdy calculates NTIME correctly as (VISITDY - 1) * 24", {
  test_data <- tibble::tribble(
    ~USUBJID, ~DOMAIN, ~VISITDY,
    1, "PC", 1,  # Day 1 = 0 hours
    1, "PC", 2,  # Day 2 = 24 hours
    1, "PC", 3,  # Day 3 = 48 hours
    2, "PC", 7,  # Day 7 = 144 hours
    2, "PC", 14  # Day 14 = 312 hours
  )

  result <- make_ntime_from_visitdy(test_data, domain = "PC")

  expect_equal(result$VISITDY, c(1, 2, 3, 7, 14))
  expect_equal(result$NTIME, c(0, 24, 48, 144, 312))
})


test_that("make_ntime_from_visitdy handles unsorted values correctly", {
  test_data <- tibble::tribble(
    ~USUBJID, ~DOMAIN, ~VISITDY,
    1, "PC", 5,
    1, "PC", 1,
    1, "PC", 3,
    2, "PC", 2,
    2, "PC", 4
  )

  result <- make_ntime_from_visitdy(test_data, domain = "PC")

  # Note: arrange behavior may vary, test actual output
  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 5)
  expect_equal(sort(result$VISITDY), c(1, 2, 3, 4, 5))
  expect_equal(sort(result$NTIME), c(0, 24, 48, 72, 96))
})


test_that("make_ntime_from_visitdy handles duplicate values correctly", {
  test_data <- tibble::tribble(
    ~USUBJID, ~DOMAIN, ~VISITDY,
    1, "PC", 1,
    1, "PC", 1,
    1, "PC", 1,
    2, "PC", 2,
    2, "PC", 2,
    3, "PC", 3
  )

  result <- make_ntime_from_visitdy(test_data, domain = "PC")

  # Should only have unique values
  expect_equal(nrow(result), 3)
  expect_equal(sort(result$VISITDY), c(1, 2, 3))
  expect_equal(sort(result$NTIME), c(0, 24, 48))
})


test_that("make_ntime_from_visitdy errors when VISITDY column is missing", {
  test_data <- tibble::tribble(
    ~USUBJID, ~DOMAIN,
    1, "PC",
    1, "PC",
    2, "PC"
  )

  # Should error with specific message
  expect_error(
    make_ntime_from_visitdy(test_data, domain = "PC"),
    "VISITDY not found in input data!"
  )

  # Test with different domain - should still look for VISITDY
  expect_error(
    make_ntime_from_visitdy(test_data, domain = "LB"),
    "VISITDY not found in input data!"
  )
})


test_that("make_ntime_from_visitdy handles single value correctly", {
  test_data <- tibble::tribble(
    ~USUBJID, ~DOMAIN, ~VISITDY,
    1, "PC", 5,
    2, "PC", 5,
    3, "PC", 5
  )

  result <- make_ntime_from_visitdy(test_data, domain = "PC")

  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 1)
  expect_equal(result$VISITDY, 5)
  expect_equal(result$NTIME, 96) # (5 - 1) * 24 = 96
})


test_that("make_ntime_from_visitdy handles empty data frame correctly", {
  test_data <- tibble::tibble(
    USUBJID = character(0),
    DOMAIN = character(0),
    VISITDY = numeric(0)
  )

  result <- make_ntime_from_visitdy(test_data, domain = "PC")

  # Should return empty data frame or handle gracefully
  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 0)
  expect_equal(names(result), c("VISITDY", "NTIME"))
})


test_that("make_ntime_from_visitdy handles multiple domains correctly when domain is NULL", {
  test_data <- tibble::tribble(
    ~USUBJID, ~DOMAIN, ~VISITDY,
    1, "PC", 1,
    1, "PC", 2,
    2, "LB", 3,
    2, "LB", 4
  )

  result <- make_ntime_from_visitdy(test_data, domain = NULL)

  # Should use first unique domain for detection, but always use VISITDY column
  expect_s3_class(result, "data.frame")
  expect_equal(names(result), c("VISITDY", "NTIME"))
  expect_equal(sort(result$VISITDY), c(1, 2, 3, 4))
  expect_equal(sort(result$NTIME), c(0, 24, 48, 72))
})


test_that("make_ntime_from_visitdy handles NA values correctly", {
  test_data <- tibble::tribble(
    ~USUBJID, ~DOMAIN, ~VISITDY,
    1, "PC", 1,
    1, "PC", NA,
    1, "PC", 2,
    2, "PC", NA,
    2, "PC", 3
  )

  result <- make_ntime_from_visitdy(test_data, domain = "PC")

  expect_s3_class(result, "data.frame")
  # Should include NA if it's a unique value
  expect_true(any(is.na(result$VISITDY)) || nrow(result) == 3)
  # Valid values should be present
  expect_true(all(c(1, 2, 3) %in% result$VISITDY))
  # NTIME for valid values should be correct
  valid_rows <- !is.na(result$VISITDY)
  if (any(valid_rows)) {
    expect_equal(result$NTIME[valid_rows], (result$VISITDY[valid_rows] - 1) * 24)
  }
})


test_that("make_ntime_from_visitdy handles day 1 correctly (should be 0 hours)", {
  test_data <- tibble::tribble(
    ~USUBJID, ~DOMAIN, ~VISITDY,
    1, "PC", 1,
    2, "PC", 1,
    3, "PC", 1
  )

  result <- make_ntime_from_visitdy(test_data, domain = "PC")

  expect_equal(result$VISITDY, 1)
  expect_equal(result$NTIME, 0) # (1 - 1) * 24 = 0
})


test_that("make_ntime_from_visitdy handles large day numbers correctly", {
  test_data <- tibble::tribble(
    ~USUBJID, ~DOMAIN, ~VISITDY,
    1, "PC", 1,
    1, "PC", 30,
    1, "PC", 90,
    2, "PC", 180
  )

  result <- make_ntime_from_visitdy(test_data, domain = "PC")

  expect_equal(sort(result$VISITDY), c(1, 30, 90, 180))
  expect_equal(sort(result$NTIME), c(0, 696, 2136, 4296))
})


test_that("make_ntime_from_visitdy handles zero and negative day values", {
  test_data <- tibble::tribble(
    ~USUBJID, ~DOMAIN, ~VISITDY,
    1, "PC", 0,
    1, "PC", -1,
    1, "PC", 1
  )

  result <- make_ntime_from_visitdy(test_data, domain = "PC")

  expect_equal(sort(result$VISITDY), c(-1, 0, 1))
  # NTIME = (VISITDY - 1) * 24
  expect_equal(sort(result$NTIME), c(-48, -24, 0))
})


test_that("make_ntime_from_visitdy handles decimal day values", {
  test_data <- tibble::tribble(
    ~USUBJID, ~DOMAIN, ~VISITDY,
    1, "PC", 1.5,
    1, "PC", 2.5,
    1, "PC", 3.0
  )

  result <- make_ntime_from_visitdy(test_data, domain = "PC")

  expect_equal(sort(result$VISITDY), c(1.5, 2.5, 3.0))
  # NTIME = (VISITDY - 1) * 24
  expect_equal(sort(result$NTIME), c(12, 36, 48))
})


test_that("make_ntime_from_visitdy preserves correct column names", {
  test_data <- tibble::tribble(
    ~USUBJID, ~DOMAIN, ~VISITDY,
    1, "PC", 1,
    1, "PC", 2
  )

  result <- make_ntime_from_visitdy(test_data, domain = "PC")

  expect_equal(names(result), c("VISITDY", "NTIME"))
  expect_true("VISITDY" %in% names(result))
  expect_true("NTIME" %in% names(result))
})


test_that("make_ntime_from_visitdy works across different domains with same VISITDY", {
  # Test that VISITDY is shared across domains
  test_data <- tibble::tribble(
    ~USUBJID, ~DOMAIN, ~VISITDY,
    1, "PC", 1,
    1, "PC", 2,
    1, "LB", 1,
    1, "LB", 3,
    2, "PC", 2,
    2, "VS", 1
  )

  result <- make_ntime_from_visitdy(test_data, domain = NULL)

  # Should get all unique VISITDY values across all domains
  expect_s3_class(result, "data.frame")
  expect_equal(names(result), c("VISITDY", "NTIME"))
  expect_equal(sort(result$VISITDY), c(1, 2, 3))
  expect_equal(sort(result$NTIME), c(0, 24, 48))
})


test_that("make_ntime_from_visitdy handles missing DOMAIN column when domain is NULL", {
  test_data <- tibble::tribble(
    ~USUBJID, ~VISITDY,
    1, 1,
    1, 2
  )

  # Should error or handle gracefully
  expect_error(
    make_ntime_from_visitdy(test_data, domain = NULL),
    "Missing DOMAIN field!"
  )
})


test_that("make_ntime_from_visitdy handles all NA values in VISITDY", {
  test_data <- tibble::tribble(
    ~USUBJID, ~DOMAIN, ~VISITDY,
    1, "PC", NA,
    1, "PC", NA,
    2, "PC", NA
  )

  result <- make_ntime_from_visitdy(test_data, domain = "PC")

  # Should create a data frame with NA values
  expect_s3_class(result, "data.frame")
  expect_true(any(is.na(result$VISITDY)))
  expect_true(any(is.na(result$NTIME)))
})


test_that("make_ntime_from_visitdy maintains consistent output structure", {
  test_data <- tibble::tribble(
    ~USUBJID, ~DOMAIN, ~VISITDY,
    1, "PC", 1,
    1, "PC", 2
  )

  result <- make_ntime_from_visitdy(test_data, domain = "PC")

  # Check structure
  expect_s3_class(result, "data.frame")
  expect_equal(ncol(result), 2)
  expect_equal(names(result), c("VISITDY", "NTIME"))
  expect_true(is.numeric(result$NTIME) || all(is.na(result$NTIME)))
})


test_that("make_ntime_from_visitdy handles very large VISITDY values", {
  test_data <- tibble::tribble(
    ~USUBJID, ~DOMAIN, ~VISITDY,
    1, "PC", 1,
    1, "PC", 365,
    1, "PC", 730
  )

  result <- make_ntime_from_visitdy(test_data, domain = "PC")

  expect_equal(sort(result$VISITDY), c(1, 365, 730))
  expect_equal(sort(result$NTIME), c(0, 8736, 17496)) # (365-1)*24, (730-1)*24
})


test_that("make_ntime_from_visitdy handles fractional VISITDY values", {
  test_data <- tibble::tribble(
    ~USUBJID, ~DOMAIN, ~VISITDY,
    1, "PC", 0.5,
    1, "PC", 1.25,
    1, "PC", 2.75
  )

  result <- make_ntime_from_visitdy(test_data, domain = "PC")

  expect_equal(sort(result$VISITDY), c(0.5, 1.25, 2.75))
  # NTIME = (VISITDY - 1) * 24
  expect_equal(sort(result$NTIME), c(-12, 6, 42)) # (0.5-1)*24, (1.25-1)*24, (2.75-1)*24
})

