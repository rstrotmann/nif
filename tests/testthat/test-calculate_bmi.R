test_that("calculate_bmi works correctly for valid inputs", {
  # Test single values
  expect_equal(calculate_bmi(170, 70), 24.221453, tolerance = 1e-6)
  expect_equal(calculate_bmi(180, 80), 24.691358, tolerance = 1e-6)

  # Test vectorized inputs
  heights <- c(170, 180, 160)
  weights <- c(70, 80, 60)
  expected <- c(24.221453, 24.691358, 23.437500)
  expect_equal(calculate_bmi(heights, weights), expected, tolerance = 1e-6)
})


test_that("calculate_bmi handles NA values correctly", {
  # Test single NA
  expect_true(is.na(calculate_bmi(NA, 70)))
  expect_true(is.na(calculate_bmi(170, NA)))

  # Test vectorized with NA
  heights <- c(170, NA, 160)
  weights <- c(70, 80, NA)
  result <- calculate_bmi(heights, weights)
  expect_true(is.na(result[2]))
  expect_true(is.na(result[3]))
  expect_equal(result[1], 24.221453, tolerance = 1e-6)
})


test_that("calculate_bmi handles invalid inputs correctly", {
  # Test zero or negative values
  expect_true(is.na(calculate_bmi(0, 70)))
  expect_true(is.na(calculate_bmi(170, 0)))
  expect_true(is.na(calculate_bmi(-170, 70)))
  expect_true(is.na(calculate_bmi(170, -70)))
})


test_that("calculate_bmi handles type errors correctly", {
  # Test non-numeric inputs
  expect_error(calculate_bmi("170", 70), "Height and weight must be numeric values")
  expect_error(calculate_bmi(170, "70"), "Height and weight must be numeric values")
})


test_that("calculate_bmi handles length mismatch correctly", {
  # Test different length vectors
  expect_error(
    calculate_bmi(c(170, 180), c(70, 80, 60)),
    "Height and weight vectors must have the same length"
  )
})
