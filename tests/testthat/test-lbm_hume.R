# Test file for lbm_hume function

test_that("lbm_hume calculates correct values for valid inputs", {
  # Test male subjects
  expect_equal(
    lbm_hume(weight = 70, height = 170, sex = "M"),
    51.1,
    tolerance = 0.1
  )
  expect_equal(
    lbm_hume(weight = 85, height = 180, sex = 0),
    59.4,
    tolerance = 0.1
  )

  # Test female subjects
  expect_equal(
    lbm_hume(weight = 65, height = 165, sex = "F"),
    44.9,
    tolerance = 0.1
  )
  expect_equal(
    lbm_hume(weight = 75, height = 175, sex = 1),
    52.1,
    tolerance = 0.1
  )
})


test_that("lbm_hume handles edge cases correctly", {
  # Test zero or negative values
  expect_true(is.na(lbm_hume(weight = 0, height = 170, sex = "M")))
  expect_true(is.na(lbm_hume(weight = 70, height = 0, sex = "M")))
  expect_true(is.na(lbm_hume(weight = -70, height = 170, sex = "M")))
  expect_true(is.na(lbm_hume(weight = 70, height = -170, sex = "M")))

  # Test missing values
  expect_true(is.na(lbm_hume(weight = NA, height = 170, sex = "M")))
  expect_true(is.na(lbm_hume(weight = 70, height = NA, sex = "M")))
  expect_true(is.na(lbm_hume(weight = 70, height = 170, sex = NA)))
})


test_that("lbm_hume handles different sex input formats", {
  # Test character inputs
  expect_equal(
    lbm_hume(weight = 70, height = 170, sex = "M"),
    lbm_hume(weight = 70, height = 170, sex = "m")
  )
  expect_equal(
    lbm_hume(weight = 70, height = 170, sex = "F"),
    lbm_hume(weight = 70, height = 170, sex = "f")
  )

  # Test numeric inputs
  expect_equal(
    lbm_hume(weight = 70, height = 170, sex = 0),
    lbm_hume(weight = 70, height = 170, sex = "M")
  )
  expect_equal(
    lbm_hume(weight = 70, height = 170, sex = 1),
    lbm_hume(weight = 70, height = 170, sex = "F")
  )

  # Test invalid sex values
  expect_true(is.na(lbm_hume(weight = 70, height = 170, sex = "X")))
  expect_true(is.na(lbm_hume(weight = 70, height = 170, sex = 2)))
})


test_that("lbm_hume maintains consistency with other LBM formulas", {
  # Test that results are within reasonable range of other formulas
  weight <- 70
  height <- 170
  sex <- "M"

  hume_lbm <- lbm_hume(weight, height, sex)
  boer_lbm <- lbm_boer(weight, height, sex)
  peters_lbm <- lbm_peters(weight, height, sex)

  # Results should be within 20% of each other
  expect_true(abs(hume_lbm - boer_lbm) / boer_lbm < 0.2)
  expect_true(abs(hume_lbm - peters_lbm) / peters_lbm < 0.2)
})

