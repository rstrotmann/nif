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


test_that("egfr_mdrd calculates correct value for male with known inputs", {
  # Known test case: Male, age 50, creat 1.0 mg/dl, white
  # MDRD formula: 175 * crea^-1.154 * age^-0.203 * female_factor * race_factor
  # Male white: 175 * 1.0^-1.154 * 50^-0.203 * 1 * 1
  result <- egfr_mdrd(crea = 1.0, age = 50, sex = 0, race = "WHITE")

  expected <- 175 * (1.0^-1.154) * (50^-0.203) * 1 * 1
  attr(expected, 'unit') <- "ml/min/1.73 m^2"
  expect_equal(result, expected, tolerance = 0.01)
  expect_equal(attr(result, "unit"), "ml/min/1.73 m^2")
})


test_that("egfr_mdrd calculates correct value for female with known inputs", {
  # Female, age 50, creat 1.0 mg/dl, white
  # MDRD formula: 175 * crea^-1.154 * age^-0.203 * 0.742 * 1
  result <- egfr_mdrd(crea = 1.0, age = 50, sex = 1, race = "WHITE")

  expected <- 175 * (1.0^-1.154) * (50^-0.203) * 0.742 * 1
  attr(expected, 'unit') <- "ml/min/1.73 m^2"
  expect_equal(result, expected, tolerance = 0.01)
  expect_equal(attr(result, "unit"), "ml/min/1.73 m^2")
})


test_that("egfr_mdrd handles numeric sex encoding (0 = male, 1 = female)", {
  # Male (sex = 0)
  result_male <- as.numeric(egfr_mdrd(crea = 1.0, age = 50, sex = 0, race = "WHITE"))

  # Female (sex = 1)
  result_female <- as.numeric(egfr_mdrd(crea = 1.0, age = 50, sex = 1, race = "WHITE"))

  # Female should have lower eGFR (0.742 factor)
  expect_true(result_female < result_male)
  expect_equal(result_female / result_male, 0.742, tolerance = 0.01)
})


test_that("egfr_mdrd handles character sex encoding (M = male, F = female)", {
  # Male (sex = "M")
  result_male <- as.numeric(egfr_mdrd(crea = 1.0, age = 50, sex = "M", race = "WHITE"))

  # Female (sex = "F")
  result_female <- as.numeric(egfr_mdrd(crea = 1.0, age = 50, sex = "F", race = "WHITE"))

  # Female should have lower eGFR (0.742 factor)
  expect_true(result_female < result_male)
  expect_equal(result_female / result_male, 0.742, tolerance = 0.01)

  # Should be same as numeric encoding
  result_male_num <- as.numeric(egfr_mdrd(crea = 1.0, age = 50, sex = 0, race = "WHITE"))
  result_female_num <- as.numeric(egfr_mdrd(crea = 1.0, age = 50, sex = 1, race = "WHITE"))

  expect_equal(result_male, result_male_num)
  expect_equal(result_female, result_female_num)
})


test_that("egfr_mdrd converts molar units correctly (umol/l to mg/dl)", {
  # Creatinine 88.4 umol/l = 1.0 mg/dl (88.4 / 88.4 = 1.0)
  result_molar <- egfr_mdrd(crea = 88.4, age = 50, sex = 0, race = "WHITE", molar = TRUE)

  result_mgdl <- egfr_mdrd(crea = 1.0, age = 50, sex = 0, race = "WHITE", molar = FALSE)

  # Results should be identical
  expect_equal(result_molar, result_mgdl, tolerance = 0.01)
})


test_that("egfr_mdrd handles black race correctly", {
  # Black race should have higher eGFR (1.212 factor)
  result_black <- as.numeric(egfr_mdrd(crea = 1.0, age = 50, sex = 0, race = "BLACK"))
  result_white <- as.numeric(egfr_mdrd(crea = 1.0, age = 50, sex = 0, race = "WHITE"))

  # Black should have higher eGFR
  expect_true(result_black > result_white)
  expect_equal(result_black / result_white, 1.212, tolerance = 0.01)
})


test_that("egfr_mdrd handles race case-insensitively", {
  # Race matching should be case-insensitive (uses str_to_lower)
  result_black_upper <- as.numeric(egfr_mdrd(crea = 1.0, age = 50, sex = 0, race = "BLACK"))
  result_black_lower <- as.numeric(egfr_mdrd(crea = 1.0, age = 50, sex = 0, race = "black"))
  result_black_mixed <- as.numeric(egfr_mdrd(crea = 1.0, age = 50, sex = 0, race = "Black"))

  # All should be identical
  expect_equal(result_black_upper, result_black_lower)
  expect_equal(result_black_upper, result_black_mixed)
})


test_that("egfr_mdrd handles race with 'black' substring", {
  # Race matching uses grepl, so "AFRICAN AMERICAN BLACK" should match
  result_substring <- as.numeric(egfr_mdrd(crea = 1.0, age = 50, sex = 0, race = "AFRICAN AMERICAN BLACK"))
  result_black <- as.numeric(egfr_mdrd(crea = 1.0, age = 50, sex = 0, race = "BLACK"))

  # Should be identical (both contain "black")
  expect_equal(result_substring, result_black)
})


test_that("egfr_mdrd ignores weight parameter", {
  # Weight should not affect calculation (for compatibility only)
  result_with_weight <- as.numeric(egfr_mdrd(crea = 1.0, age = 50, sex = 0, race = "WHITE", weight = 70))
  result_without_weight <- as.numeric(egfr_mdrd(crea = 1.0, age = 50, sex = 0, race = "WHITE", weight = NA))
  result_different_weight <- as.numeric(egfr_mdrd(crea = 1.0, age = 50, sex = 0, race = "WHITE", weight = 100))

  # All should be identical
  expect_equal(result_with_weight, result_without_weight)
  expect_equal(result_with_weight, result_different_weight)
})


test_that("egfr_mdrd handles vectorized inputs correctly", {
  # Multiple subjects
  crea <- c(0.8, 1.0, 1.2)
  age <- c(45, 50, 55)
  sex <- c(0, 1, 0)
  race <- c("WHITE", "BLACK", "WHITE")

  result <- egfr_mdrd(crea = crea, age = age, sex = sex, race = race)

  expect_length(result, 3)
  expect_equal(attr(result, "unit"), "ml/min/1.73 m^2")

  # Check individual calculations
  expected1 <- 175 * (0.8^-1.154) * (45^-0.203) * 1 * 1
  expected2 <- 175 * (1.0^-1.154) * (50^-0.203) * 0.742 * 1.212
  expected3 <- 175 * (1.2^-1.154) * (55^-0.203) * 1 * 1

  expect_equal(as.numeric(result[1]), expected1, tolerance = 0.01)
  expect_equal(as.numeric(result[2]), expected2, tolerance = 0.01)
  expect_equal(as.numeric(result[3]), expected3, tolerance = 0.01)
})


test_that("egfr_mdrd handles different age values", {
  # Younger person should have higher eGFR (all else equal)
  result_young <- as.numeric(egfr_mdrd(crea = 1.0, age = 30, sex = 0, race = "WHITE"))
  result_old <- as.numeric(egfr_mdrd(crea = 1.0, age = 70, sex = 0, race = "WHITE"))

  expect_true(result_young > result_old)

  # Verify the relationship: age^-0.203
  age_factor_young <- 30^-0.203
  age_factor_old <- 70^-0.203
  expect_equal(result_young / result_old, age_factor_young / age_factor_old, tolerance = 0.01)
})


test_that("egfr_mdrd handles different creatinine values", {
  # Lower creatinine should result in higher eGFR (all else equal)
  result_low_crea <- as.numeric(egfr_mdrd(crea = 0.6, age = 50, sex = 0, race = "WHITE"))
  result_high_crea <- as.numeric(egfr_mdrd(crea = 1.5, age = 50, sex = 0, race = "WHITE"))

  expect_true(result_low_crea > result_high_crea)

  # Verify the relationship: crea^-1.154
  crea_factor_low <- 0.6^-1.154
  crea_factor_high <- 1.5^-1.154
  expect_equal(result_low_crea / result_high_crea, crea_factor_low / crea_factor_high, tolerance = 0.01)
})


test_that("egfr_mdrd handles edge case with very high creatinine", {
  # Very high creatinine (e.g., severe renal impairment)
  result <- egfr_mdrd(crea = 5.0, age = 50, sex = 0, race = "WHITE")

  # Should still calculate (though value will be low)
  expect_true(is.numeric(result))
  expect_true(as.numeric(result) > 0)
  expect_true(as.numeric(result) < 20)  # Should be very low
  expect_equal(attr(result, "unit"), "ml/min/1.73 m^2")
})


test_that("egfr_mdrd handles edge case with very low creatinine", {
  # Very low creatinine
  result <- egfr_mdrd(crea = 0.3, age = 50, sex = 0, race = "WHITE")

  # Should still calculate (though value will be high)
  expect_true(is.numeric(result))
  expect_true(as.numeric(result) > 0)
  expect_true(as.numeric(result) > 150)  # Should be high
  expect_equal(attr(result, "unit"), "ml/min/1.73 m^2")
})


test_that("egfr_mdrd handles edge case with very old age", {
  # Very old age
  result <- egfr_mdrd(crea = 1.0, age = 90, sex = 0, race = "WHITE")

  # Should still calculate (though value will be low)
  expect_true(is.numeric(result))
  expect_true(as.numeric(result) > 0)
  expect_true(as.numeric(result) < 100)  # Should be low
  expect_equal(attr(result, "unit"), "ml/min/1.73 m^2")
})


test_that("egfr_mdrd handles empty race parameter", {
  # Empty race should be treated as non-black
  result_empty <- as.numeric(egfr_mdrd(crea = 1.0, age = 50, sex = 0, race = ""))
  result_white <- as.numeric(egfr_mdrd(crea = 1.0, age = 50, sex = 0, race = "WHITE"))

  # Should be identical (both non-black)
  expect_equal(result_empty, result_white)
})


test_that("egfr_mdrd returns correct unit attribute", {
  result <- egfr_mdrd(crea = 1.0, age = 50, sex = 0, race = "WHITE")

  expect_equal(attr(result, "unit"), "ml/min/1.73 m^2")
  expect_false(is.null(attr(result, "unit")))
})


test_that("egfr_mdrd handles mixed vector inputs with some NA values", {
  # Vector with some NA values
  crea <- c(0.8, 1.0, NA)
  age <- c(45, 50, 55)
  sex <- c(0, 1, 0)
  race <- c("WHITE", "BLACK", "WHITE")

  result <- egfr_mdrd(crea = crea, age = age, sex = sex, race = race)

  expect_length(result, 3)
  expect_true(is.na(result[3]))  # Should be NA where crea is NA
  expect_false(is.na(result[1]))  # Should calculate where inputs are valid
  expect_false(is.na(result[2]))
})


test_that("egfr_mdrd handles zero creatinine (edge case)", {
  # Zero creatinine would cause issues with negative exponent
  result <- egfr_mdrd(crea = 0, age = 50, sex = 0, race = "WHITE")

  # Should return Inf or very large number
  expect_true(is.infinite(result) || as.numeric(result) > 1e10)
})


test_that("egfr_mdrd handles zero age (edge case)", {
  # Zero age
  result <- egfr_mdrd(crea = 1.0, age = 0, sex = 0, race = "WHITE")

  # Should calculate: 175 * 1.0^-1.154 * 0^-0.203 * 1 * 1
  # 0^-0.203 = Inf, so result should be Inf
  expect_true(is.infinite(result) || as.numeric(result) > 1e10)
})


test_that("egfr_mdrd formula matches published MDRD equation", {
  # Standard test case from literature
  # Male, age 50, creat 1.0 mg/dl, white
  # Expected: 175 * 1.0^-1.154 * 50^-0.203 * 1 * 1
  result <- as.numeric(egfr_mdrd(crea = 1.0, age = 50, sex = 0, race = "WHITE"))

  expected <- 175 * (1.0^-1.154) * (50^-0.203) * 1 * 1
  expect_equal(result, expected, tolerance = 0.01)

  # Female, same parameters
  # Expected: 175 * 1.0^-1.154 * 50^-0.203 * 0.742 * 1
  result_female <- as.numeric(egfr_mdrd(crea = 1.0, age = 50, sex = 1, race = "WHITE"))

  expected_female <- 175 * (1.0^-1.154) * (50^-0.203) * 0.742 * 1
  expect_equal(result_female, expected_female, tolerance = 0.01)

  # Black male, same parameters
  # Expected: 175 * 1.0^-1.154 * 50^-0.203 * 1 * 1.212
  result_black <- as.numeric(egfr_mdrd(crea = 1.0, age = 50, sex = 0, race = "BLACK"))

  expected_black <- 175 * (1.0^-1.154) * (50^-0.203) * 1 * 1.212
  expect_equal(result_black, expected_black, tolerance = 0.01)
})


test_that("egfr_mdrd handles sex encoding correctly (case-sensitive for F)", {
  # Test that "F" is treated as female (case-sensitive)
  result_f_upper <- as.numeric(egfr_mdrd(crea = 1.0, age = 50, sex = "F", race = "WHITE"))
  result_f_lower <- as.numeric(egfr_mdrd(crea = 1.0, age = 50, sex = "f", race = "WHITE"))
  result_female_num <- as.numeric(egfr_mdrd(crea = 1.0, age = 50, sex = 1, race = "WHITE"))
  result_male <- as.numeric(egfr_mdrd(crea = 1.0, age = 50, sex = 0, race = "WHITE"))

  # "F" (uppercase) should be treated as female
  expect_equal(result_f_upper, result_female_num)

  # "f" (lowercase) should be treated as male (not female, since check is case-sensitive)
  expect_equal(result_f_lower, result_male)

  # Female should have 0.742 factor compared to male
  expect_equal(result_f_upper / result_male, 0.742, tolerance = 0.01)
})


test_that("egfr_mdrd handles molar conversion with vectorized inputs", {
  # Multiple subjects with molar units
  crea_umol <- c(70.72, 88.4, 106.08)  # 0.8, 1.0, 1.2 mg/dl
  age <- c(45, 50, 55)
  sex <- c(0, 1, 0)
  race <- c("WHITE", "BLACK", "WHITE")

  result_molar <- egfr_mdrd(crea = crea_umol, age = age, sex = sex, race = race, molar = TRUE)

  crea_mgdl <- c(0.8, 1.0, 1.2)
  result_mgdl <- egfr_mdrd(crea = crea_mgdl, age = age, sex = sex, race = race, molar = FALSE)

  # Results should be identical
  expect_equal(result_molar, result_mgdl, tolerance = 0.01)
})


test_that("egfr_mdrd handles combination of female and black race", {
  # Female black should have both factors applied
  result_female_black <- as.numeric(egfr_mdrd(crea = 1.0, age = 50, sex = 1, race = "BLACK"))
  result_male_white <- as.numeric(egfr_mdrd(crea = 1.0, age = 50, sex = 0, race = "WHITE"))

  # Female black: 0.742 * 1.212 = 0.899
  # Male white: 1 * 1 = 1
  # Ratio should be 0.899
  expect_equal(result_female_black / result_male_white, 0.742 * 1.212, tolerance = 0.01)
})


test_that("egfr_raynaud calculates correct value for male with known inputs", {
  # Known test case: Male, age 50, creat 1.0 mg/dl
  # Raynaud formula: exp(4.4275492 - 0.8230475 * log(crea) - 0.0124264 * crea^2 - 0.0055068 * age + 0.1806494 * male)
  # Male: exp(4.4275492 - 0.8230475 * log(1.0) - 0.0124264 * 1.0^2 - 0.0055068 * 50 + 0.1806494 * 1)
  result <- egfr_raynaud(crea = 1.0, age = 50, sex = 0)

  expected <- exp(4.4275492 - 0.8230475 * log(1.0) - 0.0124264 * 1.0^2 - 0.0055068 * 50 + 0.1806494 * 1)
  attr(expected, 'unit') <- "ml/min/1.73 m^2"
  expect_equal(result, expected, tolerance = 0.01)
  expect_equal(attr(result, "unit"), "ml/min/1.73 m^2")
})


test_that("egfr_raynaud calculates correct value for female with known inputs", {
  # Female, age 50, creat 1.0 mg/dl
  # Raynaud formula: exp(4.4275492 - 0.8230475 * log(crea) - 0.0124264 * crea^2 - 0.0055068 * age + 0.1806494 * male)
  # Female: exp(4.4275492 - 0.8230475 * log(1.0) - 0.0124264 * 1.0^2 - 0.0055068 * 50 + 0.1806494 * 0)
  result <- egfr_raynaud(crea = 1.0, age = 50, sex = 1)

  expected <- exp(4.4275492 - 0.8230475 * log(1.0) - 0.0124264 * 1.0^2 - 0.0055068 * 50 + 0.1806494 * 0)
  attr(expected, 'unit') <- "ml/min/1.73 m^2"
  expect_equal(result, expected, tolerance = 0.01)
  expect_equal(attr(result, "unit"), "ml/min/1.73 m^2")
})


test_that("egfr_raynaud handles numeric sex encoding (0 = male, 1 = female)", {
  # Male (sex = 0)
  result_male <- as.numeric(egfr_raynaud(crea = 1.0, age = 50, sex = 0))

  # Female (sex = 1)
  result_female <- as.numeric(egfr_raynaud(crea = 1.0, age = 50, sex = 1))

  # Male should have higher eGFR (due to +0.1806494 factor)
  expect_true(result_male > result_female)

  # Verify the relationship: exp(0.1806494) ≈ 1.198
  expected_ratio <- exp(0.1806494)
  expect_equal(result_male / result_female, expected_ratio, tolerance = 0.01)
})


test_that("egfr_raynaud handles character sex encoding (M = male, F = female)", {
  # Male (sex = "M")
  result_male <- as.numeric(egfr_raynaud(crea = 1.0, age = 50, sex = "M"))

  # Female (sex = "F")
  result_female <- as.numeric(egfr_raynaud(crea = 1.0, age = 50, sex = "F"))

  # Male should have higher eGFR
  expect_true(result_male > result_female)

  # Should be same as numeric encoding
  result_male_num <- as.numeric(egfr_raynaud(crea = 1.0, age = 50, sex = 0))
  result_female_num <- as.numeric(egfr_raynaud(crea = 1.0, age = 50, sex = 1))

  expect_equal(result_male, result_male_num)
  expect_equal(result_female, result_female_num)
})


test_that("egfr_raynaud converts molar units correctly (umol/l to mg/dl)", {
  # Creatinine 88.4 umol/l = 1.0 mg/dl (88.4 / 88.4 = 1.0)
  result_molar <- egfr_raynaud(crea = 88.4, age = 50, sex = 0, molar = TRUE)

  result_mgdl <- egfr_raynaud(crea = 1.0, age = 50, sex = 0, molar = FALSE)

  # Results should be identical
  expect_equal(result_molar, result_mgdl, tolerance = 0.01)
})


test_that("egfr_raynaud ignores race parameter", {
  # Race should not affect calculation (just for compatibility)
  result_white <- as.numeric(egfr_raynaud(crea = 1.0, age = 50, sex = 0, race = "WHITE"))
  result_black <- as.numeric(egfr_raynaud(crea = 1.0, age = 50, sex = 0, race = "BLACK"))
  result_empty <- as.numeric(egfr_raynaud(crea = 1.0, age = 50, sex = 0, race = ""))

  # All should be identical
  expect_equal(result_white, result_black)
  expect_equal(result_white, result_empty)
})


test_that("egfr_raynaud ignores weight parameter", {
  # Weight should not affect calculation (for compatibility only)
  result_with_weight <- as.numeric(egfr_raynaud(crea = 1.0, age = 50, sex = 0, weight = 70))
  result_without_weight <- as.numeric(egfr_raynaud(crea = 1.0, age = 50, sex = 0, weight = NA))
  result_different_weight <- as.numeric(egfr_raynaud(crea = 1.0, age = 50, sex = 0, weight = 100))

  # All should be identical
  expect_equal(result_with_weight, result_without_weight)
  expect_equal(result_with_weight, result_different_weight)
})


test_that("egfr_raynaud handles vectorized inputs correctly", {
  # Multiple subjects
  crea <- c(0.8, 1.0, 1.2)
  age <- c(45, 50, 55)
  sex <- c(0, 1, 0)

  result <- egfr_raynaud(crea = crea, age = age, sex = sex)

  expect_length(result, 3)
  expect_equal(attr(result, "unit"), "ml/min/1.73 m^2")

  # Check individual calculations
  expected1 <- exp(4.4275492 - 0.8230475 * log(0.8) - 0.0124264 * 0.8^2 - 0.0055068 * 45 + 0.1806494 * 1)
  expected2 <- exp(4.4275492 - 0.8230475 * log(1.0) - 0.0124264 * 1.0^2 - 0.0055068 * 50 + 0.1806494 * 0)
  expected3 <- exp(4.4275492 - 0.8230475 * log(1.2) - 0.0124264 * 1.2^2 - 0.0055068 * 55 + 0.1806494 * 1)

  expect_equal(as.numeric(result[1]), expected1, tolerance = 0.01)
  expect_equal(as.numeric(result[2]), expected2, tolerance = 0.01)
  expect_equal(as.numeric(result[3]), expected3, tolerance = 0.01)
})


test_that("egfr_raynaud handles different age values", {
  # Younger person should have higher eGFR (all else equal)
  result_young <- as.numeric(egfr_raynaud(crea = 1.0, age = 30, sex = 0))
  result_old <- as.numeric(egfr_raynaud(crea = 1.0, age = 70, sex = 0))

  expect_true(result_young > result_old)

  # Verify the relationship: age coefficient is -0.0055068
  age_diff <- 70 - 30
  expected_ratio <- exp(-0.0055068 * age_diff)
  expect_equal(result_old / result_young, expected_ratio, tolerance = 0.01)
})


test_that("egfr_raynaud handles different creatinine values", {
  # Lower creatinine should result in higher eGFR (all else equal)
  result_low_crea <- as.numeric(egfr_raynaud(crea = 0.6, age = 50, sex = 0))
  result_high_crea <- as.numeric(egfr_raynaud(crea = 1.5, age = 50, sex = 0))

  expect_true(result_low_crea > result_high_crea)
})


test_that("egfr_raynaud handles edge case with very high creatinine", {
  # Very high creatinine (e.g., severe renal impairment)
  result <- egfr_raynaud(crea = 5.0, age = 50, sex = 0)

  # Should still calculate (though value will be low)
  expect_true(is.numeric(result))
  expect_true(as.numeric(result) > 0)
  expect_true(as.numeric(result) < 30)  # Should be very low
  expect_equal(attr(result, "unit"), "ml/min/1.73 m^2")
})


test_that("egfr_raynaud handles edge case with very low creatinine", {
  # Very low creatinine
  result <- egfr_raynaud(crea = 0.3, age = 50, sex = 0)

  # Should still calculate (though value will be high)
  expect_true(is.numeric(result))
  expect_true(as.numeric(result) > 0)
  expect_true(as.numeric(result) > 120)  # Should be high
  expect_equal(attr(result, "unit"), "ml/min/1.73 m^2")
})


test_that("egfr_raynaud handles edge case with very old age", {
  # Very old age
  result <- egfr_raynaud(crea = 1.0, age = 90, sex = 0)

  # Should still calculate (though value will be low)
  expect_true(is.numeric(result))
  expect_true(as.numeric(result) > 0)
  expect_true(as.numeric(result) < 100)  # Should be low
  expect_equal(attr(result, "unit"), "ml/min/1.73 m^2")
})



test_that("egfr_raynaud handles empty race parameter", {
  # Empty race should be treated the same as any other race
  result_empty <- as.numeric(egfr_raynaud(crea = 1.0, age = 50, sex = 0, race = ""))
  result_white <- as.numeric(egfr_raynaud(crea = 1.0, age = 50, sex = 0, race = "WHITE"))

  # Should be identical (race not used)
  expect_equal(result_empty, result_white)
})


test_that("egfr_raynaud returns correct unit attribute", {
  result <- egfr_raynaud(crea = 1.0, age = 50, sex = 0)

  expect_equal(attr(result, "unit"), "ml/min/1.73 m^2")
  expect_false(is.null(attr(result, "unit")))
})


test_that("egfr_raynaud handles mixed vector inputs with some NA values", {
  # Vector with some NA values
  crea <- c(0.8, 1.0, NA)
  age <- c(45, 50, 55)
  sex <- c(0, 1, 0)

  result <- egfr_raynaud(crea = crea, age = age, sex = sex)

  expect_length(result, 3)
  expect_true(is.na(result[3]))  # Should be NA where crea is NA
  expect_false(is.na(result[1]))  # Should calculate where inputs are valid
  expect_false(is.na(result[2]))
})



test_that("egfr_raynaud formula matches published Raynaud equation", {
  # Standard test case from literature
  # Male, age 50, creat 1.0 mg/dl
  # Expected: exp(4.4275492 - 0.8230475 * log(1.0) - 0.0124264 * 1.0^2 - 0.0055068 * 50 + 0.1806494 * 1)
  result <- as.numeric(egfr_raynaud(crea = 1.0, age = 50, sex = 0))

  expected <- exp(4.4275492 - 0.8230475 * log(1.0) - 0.0124264 * 1.0^2 - 0.0055068 * 50 + 0.1806494 * 1)
  expect_equal(result, expected, tolerance = 0.01)

  # Female, same parameters
  # Expected: exp(4.4275492 - 0.8230475 * log(1.0) - 0.0124264 * 1.0^2 - 0.0055068 * 50 + 0.1806494 * 0)
  result_female <- as.numeric(egfr_raynaud(crea = 1.0, age = 50, sex = 1))

  expected_female <- exp(4.4275492 - 0.8230475 * log(1.0) - 0.0124264 * 1.0^2 - 0.0055068 * 50 + 0.1806494 * 0)
  expect_equal(result_female, expected_female, tolerance = 0.01)
})


test_that("egfr_raynaud handles sex encoding correctly (case-sensitive for M)", {
  # Test that "M" is treated as male (case-sensitive)
  result_m_upper <- as.numeric(egfr_raynaud(crea = 1.0, age = 50, sex = "M"))
  result_m_lower <- as.numeric(egfr_raynaud(crea = 1.0, age = 50, sex = "m"))
  result_male_num <- as.numeric(egfr_raynaud(crea = 1.0, age = 50, sex = 0))
  result_female <- as.numeric(egfr_raynaud(crea = 1.0, age = 50, sex = 1))

  # "M" (uppercase) should be treated as male
  expect_equal(result_m_upper, result_male_num)

  # "m" (lowercase) should be treated as female (not male, since check is case-sensitive)
  expect_equal(result_m_lower, result_female)

  # Male should have higher eGFR than female
  expect_true(result_m_upper > result_female)
})


test_that("egfr_raynaud handles molar conversion with vectorized inputs", {
  # Multiple subjects with molar units
  crea_umol <- c(70.72, 88.4, 106.08)  # 0.8, 1.0, 1.2 mg/dl
  age <- c(45, 50, 55)
  sex <- c(0, 1, 0)

  result_molar <- egfr_raynaud(crea = crea_umol, age = age, sex = sex, molar = TRUE)

  crea_mgdl <- c(0.8, 1.0, 1.2)
  result_mgdl <- egfr_raynaud(crea = crea_mgdl, age = age, sex = sex, molar = FALSE)

  # Results should be identical
  expect_equal(result_molar, result_mgdl, tolerance = 0.01)
})


test_that("egfr_raynaud handles non-standard sex values", {
  # Non-standard values should be treated as female (not 0 or "M")
  result_unknown <- as.numeric(egfr_raynaud(crea = 1.0, age = 50, sex = 2))
  result_female <- as.numeric(egfr_raynaud(crea = 1.0, age = 50, sex = 1))

  # Should be treated as female
  expect_equal(result_unknown, result_female)

  # Character non-M should also be treated as female
  result_char_unknown <- as.numeric(egfr_raynaud(crea = 1.0, age = 50, sex = "X"))
  expect_equal(result_char_unknown, result_female)
})

