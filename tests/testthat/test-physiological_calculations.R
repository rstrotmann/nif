test_that("BMI calculation handles edge cases correctly", {
  # Create test data with various edge cases
  test_dm <- tibble::tribble(
    ~USUBJID, ~SEX, ~ACTARMCD, ~RFSTDTC,
    "001", "M", "TRT", "2023-01-01",
    "002", "F", "TRT", "2023-01-01",
    "003", "M", "TRT", "2023-01-01",
    "004", "F", "TRT", "2023-01-01",
    "005", "M", "TRT", "2023-01-01",
    "006", "F", "TRT", "2023-01-01"
  )

  test_vs <- tibble::tribble(
    ~USUBJID, ~VSTESTCD, ~VSSTRESN, ~VSBLFL, ~VSDTC,
    # Normal case - should calculate BMI correctly
    "001", "HEIGHT", 170, "Y", "2022-12-20",
    "001", "WEIGHT", 70, "Y", "2022-12-20",

    # Missing HEIGHT - should result in NA BMI
    "002", "WEIGHT", 65, "Y", "2022-12-20",

    # Missing WEIGHT - should result in NA BMI
    "003", "HEIGHT", 180, "Y", "2022-12-20",

    # Zero HEIGHT - should result in NA BMI (would cause division by zero)
    "004", "HEIGHT", 0, "Y", "2022-12-20",
    "004", "WEIGHT", 75, "Y", "2022-12-20",

    # Negative HEIGHT - should result in NA BMI (physically impossible)
    "005", "HEIGHT", -10, "Y", "2022-12-20",
    "005", "WEIGHT", 80, "Y", "2022-12-20",

    # Zero WEIGHT - should result in NA BMI
    "006", "HEIGHT", 165, "Y", "2022-12-20",
    "006", "WEIGHT", 0, "Y", "2022-12-20"
  )

  # Run make_subjects with our test data
  result <- make_subjects(test_dm, test_vs)

  # Check that the results are as expected
  expect_equal(nrow(result), 6) # "Should return all 6 subjects")

  # Test normal case - BMI calculation should work
  normal_bmi <- 70 / (170 / 100)^2 # Expected BMI for subject 001
  expect_equal(result$BMI[result$USUBJID == "001"], normal_bmi)

  # Test all edge cases - BMI should be NA
  expect_true(is.na(result$BMI[result$USUBJID == "002"]), "Missing HEIGHT should result in NA BMI")
  expect_true(is.na(result$BMI[result$USUBJID == "003"]), "Missing WEIGHT should result in NA BMI")
  expect_true(is.na(result$BMI[result$USUBJID == "004"]), "Zero HEIGHT should result in NA BMI")
  expect_true(is.na(result$BMI[result$USUBJID == "005"]), "Negative HEIGHT should result in NA BMI")
  expect_true(is.na(result$BMI[result$USUBJID == "006"]), "Zero WEIGHT should result in NA BMI")
})

test_that("validate_lbw_parameters validates inputs correctly", {
  # Test with valid inputs
  expect_true(validate_lbw_parameters(weight = 70, height = 170, sex = "M"))
  expect_true(validate_lbw_parameters(weight = 70, height = 170, sex = 0))
  expect_true(validate_lbw_parameters(weight = 60, height = 160, sex = "F"))
  expect_true(validate_lbw_parameters(weight = 60, height = 160, sex = 1))
  expect_true(validate_lbw_parameters(weight = 70, height = 170, sex = "m"))
  expect_true(validate_lbw_parameters(weight = 60, height = 160, sex = "f"))
})

test_that("validate_lbw_parameters handles NA values correctly", {
  # Test with NA values
  expect_false(validate_lbw_parameters(weight = NA, height = 170, sex = "M"))
  expect_false(validate_lbw_parameters(weight = 70, height = NA, sex = "M"))
  expect_false(validate_lbw_parameters(weight = 70, height = 170, sex = NA))
  expect_false(validate_lbw_parameters(weight = NA, height = NA, sex = "M"))
  expect_false(validate_lbw_parameters(weight = NA, height = 170, sex = NA))
  expect_false(validate_lbw_parameters(weight = 70, height = NA, sex = NA))
  expect_false(validate_lbw_parameters(weight = NA, height = NA, sex = NA))
})

test_that("validate_lbw_parameters handles invalid numeric inputs correctly", {
  # Test with zero or negative values
  expect_false(validate_lbw_parameters(weight = 0, height = 170, sex = "M"))
  expect_false(validate_lbw_parameters(weight = -70, height = 170, sex = "M"))
  expect_false(validate_lbw_parameters(weight = 70, height = 0, sex = "M"))
  expect_false(validate_lbw_parameters(weight = 70, height = -170, sex = "M"))
})

test_that("validate_lbw_parameters handles invalid sex values correctly", {
  # Test with invalid sex values
  expect_false(validate_lbw_parameters(weight = 70, height = 170, sex = "X"))
  expect_false(validate_lbw_parameters(weight = 70, height = 170, sex = 2))
  expect_false(validate_lbw_parameters(weight = 70, height = 170, sex = "male"))
  expect_false(validate_lbw_parameters(weight = 70, height = 170, sex = "female"))
})

test_that("validate_lbw_parameters handles vectorized inputs correctly", {
  # Test with vectors of valid inputs
  weights <- c(70, 60, 80)
  heights <- c(170, 160, 180)
  sexes <- c("M", "F", "M")

  result <- validate_lbw_parameters(weight = weights, height = heights, sex = sexes)
  expect_true(all(result))

  # Test with mixed valid and invalid inputs
  weights <- c(70, NA, 80)
  heights <- c(170, 160, NA)
  sexes <- c("M", "F", "M")

  result <- validate_lbw_parameters(weight = weights, height = heights, sex = sexes)
  expect_true(result[1])
  expect_false(result[2])
  expect_false(result[3])
})

test_that("validate_lbw_parameters handles length mismatches correctly", {
  # Test with mismatched vector lengths
  expect_error(
    validate_lbw_parameters(weight = c(70, 60), height = 170, sex = "M"),
    "Height and weight vectors must have the same length"
  )

  expect_error(
    validate_lbw_parameters(weight = 70, height = c(170, 160), sex = "M"),
    "Height and weight vectors must have the same length"
  )

  expect_error(
    validate_lbw_parameters(weight = 70, height = 170, sex = c("M", "F")),
    "Height and weight vectors must have the same length"
  )
})

test_that("validate_lbw_parameters handles non-numeric inputs correctly", {
  # Test with non-numeric inputs
  expect_error(
    validate_lbw_parameters(weight = "70", height = 170, sex = "M"),
    "Height and weight must be numeric values"
  )

  expect_error(
    validate_lbw_parameters(weight = 70, height = "170", sex = "M"),
    "Height and weight must be numeric values"
  )
})

test_that("is_male correctly identifies male sex values", {
  # Test with numeric male values
  expect_true(is_male(0))

  # Test with character male values (case-insensitive)
  expect_true(is_male("M"))
  expect_true(is_male("m"))

  # Test with vector inputs
  expect_true(all(is_male(c(0, "M", "m"))))
})

test_that("is_male correctly identifies non-male sex values", {
  # Test with numeric female values
  expect_false(is_male(1))

  # Test with character female values (case-insensitive)
  expect_false(is_male("F"))
  expect_false(is_male("f"))

  # Test with vector inputs
  expect_false(any(is_male(c(1, "F", "f"))))
})

test_that("is_male handles NA values correctly", {
  # Test with NA values
  expect_false(is_male(NA))
})

test_that("is_male handles invalid sex values correctly", {
  # Test with invalid sex values
  expect_false(is_male("X"))
  expect_false(is_male(2))
  expect_false(is_male("male"))
  expect_false(is_male("female"))

  # Test with vector containing invalid values
  expect_false(any(is_male(c("X", 2, "male", "female"))))
})
