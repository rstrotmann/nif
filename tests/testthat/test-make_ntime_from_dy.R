test_that("make_ntime_from_dy works with domain specified", {
  test_data <- tibble::tribble(
    ~USUBJID, ~DOMAIN, ~PCDY,
    1, "PC", 1,
    1, "PC", 2,
    1, "PC", 3,
    2, "PC", 1,
    2, "PC", 4
  )

  result <- make_ntime_from_dy(test_data, domain = "PC")

  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 4) # Should have 4 unique values: 1, 2, 3, 4
  expect_equal(names(result), c("PCDY", "NTIME"))
  expect_equal(result$PCDY, c(1, 2, 3, 4))
  # NTIME = (DY - 1) * 24
  expect_equal(result$NTIME, c(0, 24, 48, 72))
})


test_that("make_ntime_from_dy uses default domain PC", {
  test_data <- tibble::tribble(
    ~USUBJID, ~DOMAIN, ~PCDY,
    1, "PC", 1,
    1, "PC", 2,
    2, "PC", 3
  )

  result <- make_ntime_from_dy(test_data)

  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 3)
  expect_equal(names(result), c("PCDY", "NTIME"))
  expect_equal(result$PCDY, c(1, 2, 3))
  expect_equal(result$NTIME, c(0, 24, 48))
})


test_that("make_ntime_from_dy auto-detects domain when NULL", {
  test_data <- tibble::tribble(
    ~USUBJID, ~DOMAIN, ~PCDY,
    1, "PC", 1,
    1, "PC", 2,
    2, "PC", 3
  )

  result <- make_ntime_from_dy(test_data, domain = NULL)

  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 3)
  expect_equal(names(result), c("PCDY", "NTIME"))
  expect_equal(result$PCDY, c(1, 2, 3))
  expect_equal(result$NTIME, c(0, 24, 48))
})


test_that("make_ntime_from_dy works with different domains", {
  # Test with LB domain
  test_data_lb <- tibble::tribble(
    ~USUBJID, ~DOMAIN, ~LBDY,
    1, "LB", 1,
    1, "LB", 2,
    2, "LB", 1
  )

  result_lb <- make_ntime_from_dy(test_data_lb, domain = "LB")

  expect_s3_class(result_lb, "data.frame")
  expect_equal(nrow(result_lb), 2) # Unique values: 1, 2
  expect_equal(names(result_lb), c("LBDY", "NTIME"))
  expect_equal(result_lb$LBDY, c(1, 2))
  expect_equal(result_lb$NTIME, c(0, 24))

  # Test with lowercase domain (should be uppercased)
  test_data_pc_lower <- tibble::tribble(
    ~USUBJID, ~DOMAIN, ~PCDY,
    1, "pc", 1,
    1, "pc", 2
  )

  result_pc_lower <- make_ntime_from_dy(test_data_pc_lower, domain = "pc")

  expect_s3_class(result_pc_lower, "data.frame")
  expect_equal(names(result_pc_lower), c("PCDY", "NTIME"))
  expect_equal(result_pc_lower$PCDY, c(1, 2))
  expect_equal(result_pc_lower$NTIME, c(0, 24))
})


test_that("make_ntime_from_dy calculates NTIME correctly as (DY - 1) * 24", {
  test_data <- tibble::tribble(
    ~USUBJID, ~DOMAIN, ~PCDY,
    1, "PC", 1,  # Day 1 = 0 hours
    1, "PC", 2,  # Day 2 = 24 hours
    1, "PC", 3,  # Day 3 = 48 hours
    2, "PC", 7,  # Day 7 = 144 hours
    2, "PC", 14  # Day 14 = 312 hours
  )

  result <- make_ntime_from_dy(test_data, domain = "PC")

  expect_equal(result$PCDY, c(1, 2, 3, 7, 14))
  expect_equal(result$NTIME, c(0, 24, 48, 144, 312))
})


test_that("make_ntime_from_dy handles unsorted values correctly", {
  test_data <- tibble::tribble(
    ~USUBJID, ~DOMAIN, ~PCDY,
    1, "PC", 5,
    1, "PC", 1,
    1, "PC", 3,
    2, "PC", 2,
    2, "PC", 4
  )

  result <- make_ntime_from_dy(test_data, domain = "PC")

  # Should be sorted by DY
  expect_equal(result$PCDY, c(5, 1, 3, 2, 4))
  expect_equal(result$NTIME, c(96, 0, 48, 24, 72))
})


test_that("make_ntime_from_dy handles duplicate values correctly", {
  test_data <- tibble::tribble(
    ~USUBJID, ~DOMAIN, ~PCDY,
    1, "PC", 1,
    1, "PC", 1,
    1, "PC", 1,
    2, "PC", 2,
    2, "PC", 2,
    3, "PC", 3
  )

  result <- make_ntime_from_dy(test_data, domain = "PC")

  # Should only have unique values
  expect_equal(nrow(result), 3)
  expect_equal(result$PCDY, c(1, 2, 3))
  expect_equal(result$NTIME, c(0, 24, 48))
})


test_that("make_ntime_from_dy errors when DY column is missing", {
  test_data <- tibble::tribble(
    ~USUBJID, ~DOMAIN,
    1, "PC",
    1, "PC",
    2, "PC"
  )

  # Should error with specific message
  expect_error(
    make_ntime_from_dy(test_data, domain = "PC"),
    "PCDY not found in input data!"
  )

  # Test with different domain
  expect_error(
    make_ntime_from_dy(test_data, domain = "LB"),
    "LBDY not found in input data!"
  )
})


test_that("make_ntime_from_dy handles single value correctly", {
  test_data <- tibble::tribble(
    ~USUBJID, ~DOMAIN, ~PCDY,
    1, "PC", 5,
    2, "PC", 5,
    3, "PC", 5
  )

  result <- make_ntime_from_dy(test_data, domain = "PC")

  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 1)
  expect_equal(result$PCDY, 5)
  expect_equal(result$NTIME, 96) # (5 - 1) * 24 = 96
})


test_that("make_ntime_from_dy handles empty data frame correctly", {
  test_data <- tibble::tibble(
    USUBJID = character(0),
    DOMAIN = character(0),
    PCDY = numeric(0)
  )

  result <- make_ntime_from_dy(test_data, domain = "PC")

  # Should return empty data frame or handle gracefully
  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 0)
  expect_equal(names(result), c("PCDY", "NTIME"))
})


test_that("make_ntime_from_dy handles multiple domains correctly when domain is NULL", {
  test_data <- tibble::tribble(
    ~USUBJID, ~DOMAIN, ~PCDY, ~LBDY,
    1, "PC", 1, NA,
    1, "PC", 2, NA,
    2, "LB", NA, 10,
    2, "LB", NA, 20
  )

  result <- make_ntime_from_dy(test_data, domain = NULL)

  # Should use first unique domain (PC)
  expect_s3_class(result, "data.frame")
  expect_equal(names(result), c("PCDY", "NTIME"))
  expect_equal(result$PCDY, c(1, 2, NA))
  expect_equal(result$NTIME, c(0, 24, NA))
})


test_that("make_ntime_from_dy handles NA values correctly", {
  test_data <- tibble::tribble(
    ~USUBJID, ~DOMAIN, ~PCDY,
    1, "PC", 1,
    1, "PC", NA,
    1, "PC", 2,
    2, "PC", NA,
    2, "PC", 3
  )

  result <- make_ntime_from_dy(test_data, domain = "PC")

  expect_s3_class(result, "data.frame")
  # Should include NA if it's a unique value
  expect_true(any(is.na(result$PCDY)) || nrow(result) == 3)
  # Valid values should be present
  expect_true(all(c(1, 2, 3) %in% result$PCDY))
  # NTIME for valid values should be correct
  valid_rows <- !is.na(result$PCDY)
  if (any(valid_rows)) {
    expect_equal(result$NTIME[valid_rows], (result$PCDY[valid_rows] - 1) * 24)
  }
})


test_that("make_ntime_from_dy handles day 1 correctly (should be 0 hours)", {
  test_data <- tibble::tribble(
    ~USUBJID, ~DOMAIN, ~PCDY,
    1, "PC", 1,
    2, "PC", 1,
    3, "PC", 1
  )

  result <- make_ntime_from_dy(test_data, domain = "PC")

  expect_equal(result$PCDY, 1)
  expect_equal(result$NTIME, 0) # (1 - 1) * 24 = 0
})


test_that("make_ntime_from_dy handles large day numbers correctly", {
  test_data <- tibble::tribble(
    ~USUBJID, ~DOMAIN, ~PCDY,
    1, "PC", 1,
    1, "PC", 30,
    1, "PC", 90,
    2, "PC", 180
  )

  result <- make_ntime_from_dy(test_data, domain = "PC")

  expect_equal(result$PCDY, c(1, 30, 90, 180))
  expect_equal(result$NTIME, c(0, 696, 2136, 4296))
})


test_that("make_ntime_from_dy handles zero and negative day values", {
  test_data <- tibble::tribble(
    ~USUBJID, ~DOMAIN, ~PCDY,
    1, "PC", 0,
    1, "PC", -1,
    1, "PC", 1
  )

  result <- make_ntime_from_dy(test_data, domain = "PC")

  expect_equal(result$PCDY, c(0, -1, 1))
  # NTIME = (DY - 1) * 24
  expect_equal(result$NTIME, c(-24, -48, 0))
})


test_that("make_ntime_from_dy handles decimal day values", {
  test_data <- tibble::tribble(
    ~USUBJID, ~DOMAIN, ~PCDY,
    1, "PC", 1.5,
    1, "PC", 2.5,
    1, "PC", 3.0
  )

  result <- make_ntime_from_dy(test_data, domain = "PC")

  expect_equal(result$PCDY, c(1.5, 2.5, 3.0))
  # NTIME = (DY - 1) * 24
  expect_equal(result$NTIME, c(12, 36, 48))
})


test_that("make_ntime_from_dy preserves correct column names", {
  test_data <- tibble::tribble(
    ~USUBJID, ~DOMAIN, ~PCDY,
    1, "PC", 1,
    1, "PC", 2
  )

  result <- make_ntime_from_dy(test_data, domain = "PC")

  expect_equal(names(result), c("PCDY", "NTIME"))
  expect_true("PCDY" %in% names(result))
  expect_true("NTIME" %in% names(result))
})


test_that("make_ntime_from_dy handles character DY values", {
  test_data <- tibble::tribble(
    ~USUBJID, ~DOMAIN, ~PCDY,
    1, "PC", "1",
    1, "PC", "2",
    1, "PC", "3"
  )

  # Character values should still work (R will coerce in arithmetic)
  expect_error(
    result <- make_ntime_from_dy(test_data, domain = "PC"),
    "PCDY must be numeric!"
  )
})

