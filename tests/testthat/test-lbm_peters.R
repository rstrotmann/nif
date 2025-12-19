test_that("lbm_peters calculates lean body mass correctly", {
  # Test with valid inputs
  expect_equal(
    lbm_peters(weight = 70, height = 170, sex = "M"),
    3.8 * (0.0215 * 70^0.6469 * 170^0.7236),
    tolerance = 1e-10
  )

  # Test with different sex encoding
  expect_equal(
    lbm_peters(weight = 70, height = 170, sex = 0),
    3.8 * (0.0215 * 70^0.6469 * 170^0.7236),
    tolerance = 1e-10
  )

  # Test with female
  expect_equal(
    lbm_peters(weight = 60, height = 160, sex = "F"),
    3.8 * (0.0215 * 60^0.6469 * 160^0.7236),
    tolerance = 1e-10
  )

  # Test with different sex encoding for female
  expect_equal(
    lbm_peters(weight = 60, height = 160, sex = 1),
    3.8 * (0.0215 * 60^0.6469 * 160^0.7236),
    tolerance = 1e-10
  )
})


test_that("lbm_peters handles edge cases", {
  # Test with NA values
  expect_true(is.na(lbm_peters(weight = NA, height = 170, sex = "M")))
  expect_true(is.na(lbm_peters(weight = 70, height = NA, sex = "M")))
  expect_true(is.na(lbm_peters(weight = 70, height = 170, sex = NA)))

  # Test with zero or negative values
  expect_true(is.na(lbm_peters(weight = 0, height = 170, sex = "M")))
  expect_true(is.na(lbm_peters(weight = -70, height = 170, sex = "M")))
  expect_true(is.na(lbm_peters(weight = 70, height = 0, sex = "M")))
  expect_true(is.na(lbm_peters(weight = 70, height = -170, sex = "M")))

  # Test with invalid sex values
  expect_true(is.na(lbm_peters(weight = 70, height = 170, sex = "X")))
  expect_true(is.na(lbm_peters(weight = 70, height = 170, sex = 2)))
})


test_that("lbm_peters handles vectorized inputs correctly", {
  # Test with vectors of valid inputs
  weights <- c(70, 60, 80)
  heights <- c(170, 160, 180)
  sexes <- c("M", "F", "M")

  expected <- c(
    3.8 * (0.0215 * 70^0.6469 * 170^0.7236),
    3.8 * (0.0215 * 60^0.6469 * 160^0.7236),
    3.8 * (0.0215 * 80^0.6469 * 180^0.7236)
  )

  expect_equal(
    lbm_peters(weight = weights, height = heights, sex = sexes),
    expected,
    tolerance = 1e-10
  )

  # Test with mixed valid and invalid inputs
  weights <- c(70, NA, 80)
  heights <- c(170, 160, NA)
  sexes <- c("M", "F", "M")

  result <- lbm_peters(weight = weights, height = heights, sex = sexes)
  expect_true(is.na(result[2]))
  expect_true(is.na(result[3]))
  expect_equal(
    result[1],
    3.8 * (0.0215 * 70^0.6469 * 170^0.7236),
    tolerance = 1e-10
  )
})
