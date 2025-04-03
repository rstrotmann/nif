test_that("lbm_boer calculates correct values for valid inputs", {
  # Test male calculations
  expect_equal(lbm_boer(weight = 70, height = 170, sex = "M"), 58.9, tolerance = 0.1)
  expect_equal(lbm_boer(weight = 80, height = 180, sex = 0), 67.8, tolerance = 0.1)

  # Test female calculations
  expect_equal(lbm_boer(weight = 60, height = 165, sex = "F"), 47.7, tolerance = 0.1)
  expect_equal(lbm_boer(weight = 65, height = 170, sex = 1), 50.2, tolerance = 0.1)

  # Test case-insensitive sex input
  expect_equal(lbm_boer(weight = 70, height = 170, sex = "m"), 58.9, tolerance = 0.1)
  expect_equal(lbm_boer(weight = 60, height = 165, sex = "f"), 47.7, tolerance = 0.1)
})

test_that("lbm_boer handles NA inputs correctly", {
  expect_true(is.na(lbm_boer(NA, 170, "M")))
  expect_true(is.na(lbm_boer(70, NA, "M")))
  expect_true(is.na(lbm_boer(70, 170, NA)))
  expect_true(is.na(lbm_boer(NA, NA, "M")))
  expect_true(is.na(lbm_boer(NA, 170, NA)))
  expect_true(is.na(lbm_boer(70, NA, NA)))
  expect_true(is.na(lbm_boer(NA, NA, NA)))
})

test_that("lbm_boer handles invalid numeric inputs correctly", {
  # Test non-numeric inputs
  expect_error(lbm_boer("70", 170, "M"))
  expect_error(lbm_boer(70, "170", "M"))

  # Test out of range values
  expect_true(is.na(lbm_boer(0, 170, "M")))
  expect_true(is.na(lbm_boer(-1, 170, "M")))
  expect_true(is.na(lbm_boer(70, 0, "M")))
  expect_true(is.na(lbm_boer(70, -1, "M")))
})


test_that("lbm_boer handles invalid sex inputs correctly", {
  expect_true(is.na(lbm_boer(70, 170, "X")))
  expect_true(is.na(lbm_boer(70, 170, 2)))
  expect_true(is.na(lbm_boer(70, 170, "male")))
  expect_true(is.na(lbm_boer(70, 170, "female")))
})


test_that("lbm_boer produces consistent results for same inputs", {
  # Test that same inputs always produce same results
  result1 <- lbm_boer(70, 170, "M")
  result2 <- lbm_boer(70, 170, "M")
  expect_equal(result1, result2)

  # Test that different valid sex encodings produce same results
  expect_equal(lbm_boer(70, 170, "M"), lbm_boer(70, 170, 0))
  expect_equal(lbm_boer(70, 170, "F"), lbm_boer(70, 170, 1))
})
