test_that("egfr_cg calculates correct value for male with known inputs", {
  # Known test case: Male, age 45, weight 70kg, creat 0.8 mg/dl
  # CG formula: (140 - age) * weight * sex_factor / 72 / crea
  # Male: (140 - 45) * 70 * 1 / 72 / 0.8 = 95 * 70 / 72 / 0.8 = 115.45 ml/min
  result <- egfr_cg(crea = 0.8, age = 45, sex = 0, weight = 70)

  expected <- (140 - 45) * 70 * 1 / 72 / 0.8
  attr(expected, 'unit') <- "ml/min"
  expect_equal(result, expected, tolerance = 0.01)
  expect_equal(attr(result, "unit"), "ml/min")
})


test_that("egfr_cg calculates correct value for female with known inputs", {
  # Female, age 45, weight 60kg, creat 0.8 mg/dl
  # CG formula: (140 - age) * weight * sex_factor / 72 / crea
  # Female: (140 - 45) * 60 * 0.85 / 72 / 0.8 = 95 * 60 * 0.85 / 72 / 0.8 = 84.11 ml/min
  result <- egfr_cg(crea = 0.8, age = 45, sex = 1, weight = 60)

  expected <- (140 - 45) * 60 * 0.85 / 72 / 0.8
  attr(expected, 'unit') <- "ml/min"
  expect_equal(result, expected, tolerance = 0.01)
  expect_equal(attr(result, "unit"), "ml/min")
})


test_that("egfr_cg handles numeric sex encoding (0 = male, 1 = female)", {
  # Male (sex = 0)
  result_male <- as.numeric(egfr_cg(crea = 1.0, age = 50, sex = 0, weight = 70))

  # Female (sex = 1)
  result_female <- as.numeric(egfr_cg(crea = 1.0, age = 50, sex = 1, weight = 70))

  # Female should have lower eGFR (0.85 factor)
  expect_true(result_female < result_male)
  expect_equal(result_female / result_male, 0.85, tolerance = 0.01)
})


test_that("egfr_cg handles character sex encoding (M = male, F = female)", {
  # Male (sex = "M")
  result_male <- as.numeric(egfr_cg(crea = 1.0, age = 50, sex = "M", weight = 70))

  # Female (sex = "F")
  result_female <- as.numeric(egfr_cg(crea = 1.0, age = 50, sex = "F", weight = 70))

  # Female should have lower eGFR (0.85 factor)
  expect_true(result_female < result_male)
  expect_equal(result_female / result_male, 0.85, tolerance = 0.01)

  # Should be same as numeric encoding
  result_male_num <- as.numeric(egfr_cg(crea = 1.0, age = 50, sex = 0, weight = 70))
  result_female_num <- as.numeric(egfr_cg(crea = 1.0, age = 50, sex = 1, weight = 70))

  expect_equal(result_male, result_male_num)
  expect_equal(result_female, result_female_num)
})


test_that("egfr_cg converts molar units correctly (umol/l to mg/dl)", {
  # Creatinine 70.72 umol/l = 0.8 mg/dl (70.72 / 88.4 = 0.8)
  result_molar <- egfr_cg(crea = 70.72, age = 45, sex = 0, weight = 70, molar = TRUE)

  result_mgdl <- egfr_cg(crea = 0.8, age = 45, sex = 0, weight = 70, molar = FALSE)

  # Results should be identical
  expect_equal(result_molar, result_mgdl, tolerance = 0.01)
})


test_that("egfr_cg handles vectorized inputs correctly", {
  # Multiple subjects
  crea <- c(0.8, 1.0, 1.2)
  age <- c(45, 50, 55)
  sex <- c(0, 1, 0)
  weight <- c(70, 60, 80)

  result <- egfr_cg(crea = crea, age = age, sex = sex, weight = weight)

  expect_length(result, 3)
  expect_equal(attr(result, "unit"), "ml/min")

  # Check individual calculations
  expected1 <- (140 - 45) * 70 * 1 / 72 / 0.8
  expected2 <- (140 - 50) * 60 * 0.85 / 72 / 1.0
  expected3 <- (140 - 55) * 80 * 1 / 72 / 1.2

  expect_equal(result[1], expected1, tolerance = 0.01)
  expect_equal(result[2], expected2, tolerance = 0.01)
  expect_equal(result[3], expected3, tolerance = 0.01)
})


test_that("egfr_cg handles different age values", {
  # Younger person should have higher eGFR (all else equal)
  result_young <- as.numeric(egfr_cg(crea = 1.0, age = 30, sex = 0, weight = 70))
  result_old <- as.numeric(egfr_cg(crea = 1.0, age = 70, sex = 0, weight = 70))

  expect_true(result_young > result_old)

  # Verify the relationship: (140 - 30) / (140 - 70) = 110 / 70 = 1.571
  expect_equal(result_young / result_old, (140 - 30) / (140 - 70), tolerance = 0.01)
})


test_that("egfr_cg handles different weight values", {
  # Heavier person should have higher eGFR (all else equal)
  result_light <- as.numeric(egfr_cg(crea = 1.0, age = 50, sex = 0, weight = 50))
  result_heavy <- as.numeric(egfr_cg(crea = 1.0, age = 50, sex = 0, weight = 90))

  expect_true(result_heavy > result_light)

  # Verify the relationship: 90 / 50 = 1.8
  expect_equal(result_heavy / result_light, 90 / 50, tolerance = 0.01)
})


test_that("egfr_cg handles different creatinine values", {
  # Lower creatinine should result in higher eGFR (all else equal)
  result_low_crea <- as.numeric(egfr_cg(crea = 0.6, age = 50, sex = 0, weight = 70))
  result_high_crea <- as.numeric(egfr_cg(crea = 1.5, age = 50, sex = 0, weight = 70))

  expect_true(result_low_crea > result_high_crea)

  # Verify the relationship: 1.5 / 0.6 = 2.5
  expect_equal(result_high_crea / result_low_crea, 0.6 / 1.5, tolerance = 0.01)
})


test_that("egfr_cg handles edge case with very high creatinine", {
  # Very high creatinine (e.g., severe renal impairment)
  result <- egfr_cg(crea = 5.0, age = 50, sex = 0, weight = 70)

  # Should still calculate (though value will be low)
  expect_true(is.numeric(result))
  expect_true(result > 0)
  expect_true(result < 20)  # Should be very low
  expect_equal(attr(result, "unit"), "ml/min")
})


test_that("egfr_cg handles edge case with very low creatinine", {
  # Very low creatinine
  result <- egfr_cg(crea = 0.3, age = 50, sex = 0, weight = 70)

  # Should still calculate (though value will be high)
  expect_true(is.numeric(result))
  expect_true(result > 0)
  expect_true(result > 100)  # Should be high
  expect_equal(attr(result, "unit"), "ml/min")
})


test_that("egfr_cg handles edge case with very old age", {
  # Very old age (close to 140)
  result <- egfr_cg(crea = 1.0, age = 135, sex = 0, weight = 70)

  # Should still calculate (though value will be very low)
  expect_true(is.numeric(result))
  expect_true(result > 0)
  expect_true(result < 10)  # Should be very low
  expect_equal(attr(result, "unit"), "ml/min")
})


test_that("egfr_cg handles edge case with very young age", {
  # Very young age
  result <- egfr_cg(crea = 1.0, age = 20, sex = 0, weight = 70)

  # Should calculate high eGFR
  expect_true(is.numeric(result))
  expect_true(result > 0)
  expect_true(result > 100)  # Should be high
  expect_equal(attr(result, "unit"), "ml/min")
})


test_that("egfr_cg ignores race parameter", {
  # Race should not affect calculation (just for compatibility)
  result_white <- egfr_cg(crea = 1.0, age = 50, sex = 0, weight = 70, race = "WHITE")
  result_black <- egfr_cg(crea = 1.0, age = 50, sex = 0, weight = 70, race = "BLACK")
  result_empty <- egfr_cg(crea = 1.0, age = 50, sex = 0, weight = 70, race = "")

  # All should be identical
  expect_equal(result_white, result_black)
  expect_equal(result_white, result_empty)
})


test_that("egfr_cg handles NA weight parameter", {
  # Weight = NA should be accepted (for compatibility) but result will be NA
  result <- egfr_cg(crea = 1.0, age = 50, sex = 0, weight = NA)

  expect_true(is.na(result))
})


test_that("egfr_cg returns correct unit attribute", {
  result <- egfr_cg(crea = 1.0, age = 50, sex = 0, weight = 70)

  expect_equal(attr(result, "unit"), "ml/min")
  expect_false(is.null(attr(result, "unit")))
})


test_that("egfr_cg handles mixed vector inputs with some NA values", {
  # Vector with some NA values
  crea <- c(0.8, 1.0, NA)
  age <- c(45, 50, 55)
  sex <- c(0, 1, 0)
  weight <- c(70, 60, 80)

  result <- egfr_cg(crea = crea, age = age, sex = sex, weight = weight)

  expect_length(result, 3)
  expect_true(is.na(result[3]))  # Should be NA where crea is NA
  expect_false(is.na(result[1]))  # Should calculate where inputs are valid
  expect_false(is.na(result[2]))
})


test_that("egfr_cg handles zero creatinine (edge case)", {
  # Zero creatinine would cause division by zero
  result <- egfr_cg(crea = 0, age = 50, sex = 0, weight = 70)

  # Should return Inf or very large number
  expect_true(is.infinite(result) || result > 1e10)
})


test_that("egfr_cg handles negative age (edge case)", {
  # Negative age (invalid but function should handle it)
  result <- egfr_cg(crea = 1.0, age = -10, sex = 0, weight = 70)

  # Should calculate: (140 - (-10)) * 70 / 72 / 1.0 = 150 * 70 / 72 = 145.83
  expected <- (140 - (-10)) * 70 * 1 / 72 / 1.0
  expect_equal(as.numeric(result), expected, tolerance = 0.01)
})


test_that("egfr_cg handles very high weight", {
  # Very high weight
  result <- egfr_cg(crea = 1.0, age = 50, sex = 0, weight = 200)

  # Should calculate high eGFR
  expect_true(is.numeric(result))
  expect_true(result > 0)
  expect_true(result > 200)  # Should be high
  expect_equal(attr(result, "unit"), "ml/min")
})


test_that("egfr_cg handles very low weight", {
  # Very low weight
  result <- egfr_cg(crea = 1.0, age = 50, sex = 0, weight = 30)

  # Should calculate low eGFR
  expect_true(is.numeric(result))
  expect_true(result > 0)
  expect_true(result < 50)  # Should be low
  expect_equal(attr(result, "unit"), "ml/min")
})


test_that("egfr_cg formula matches published Cockcroft-Gault equation", {
  # Standard test case from literature
  # Male, age 40, weight 70kg, creat 1.0 mg/dl
  # Expected: (140 - 40) * 70 * 1 / 72 / 1.0 = 100 * 70 / 72 = 97.22 ml/min
  result <- as.numeric(egfr_cg(crea = 1.0, age = 40, sex = 0, weight = 70))

  expected <- (140 - 40) * 70 * 1 / 72 / 1.0
  expect_equal(result, expected, tolerance = 0.01)

  # Female, same parameters
  # Expected: (140 - 40) * 70 * 0.85 / 72 / 1.0 = 100 * 70 * 0.85 / 72 = 82.64 ml/min
  result_female <- as.numeric(egfr_cg(crea = 1.0, age = 40, sex = 1, weight = 70))

  expected_female <- (140 - 40) * 70 * 0.85 / 72 / 1.0
  expect_equal(result_female, expected_female, tolerance = 0.01)
})


test_that("egfr_cg handles sex encoding correctly (case-sensitive for F)", {
  # Test that "F" is treated as female (case-sensitive)
  result_f_upper <- egfr_cg(crea = 1.0, age = 50, sex = "F", weight = 70)
  result_f_lower <- egfr_cg(crea = 1.0, age = 50, sex = "f", weight = 70)
  result_female_num <- egfr_cg(crea = 1.0, age = 50, sex = 1, weight = 70)
  result_male <- egfr_cg(crea = 1.0, age = 50, sex = 0, weight = 70)

  # "F" (uppercase) should be treated as female
  expect_equal(result_f_upper, result_female_num)

  # "f" (lowercase) should be treated as male (not female, since check is case-sensitive)
  expect_equal(result_f_lower, result_male)

  # Female should have 0.85 factor compared to male
  expect_equal(as.numeric(result_f_upper / result_male), 0.85, tolerance = 0.01)
})


test_that("egfr_cg handles molar conversion with vectorized inputs", {
  # Multiple subjects with molar units
  crea_umol <- c(70.72, 88.4, 106.08)  # 0.8, 1.0, 1.2 mg/dl
  age <- c(45, 50, 55)
  sex <- c(0, 1, 0)
  weight <- c(70, 60, 80)

  result_molar <- egfr_cg(crea = crea_umol, age = age, sex = sex, weight = weight, molar = TRUE)

  crea_mgdl <- c(0.8, 1.0, 1.2)
  result_mgdl <- egfr_cg(crea = crea_mgdl, age = age, sex = sex, weight = weight, molar = FALSE)

  # Results should be identical
  expect_equal(result_molar, result_mgdl, tolerance = 0.01)
})

