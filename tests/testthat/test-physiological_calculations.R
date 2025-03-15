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
  normal_bmi <- 70 / (170/100)^2  # Expected BMI for subject 001
  expect_equal(result$BMI[result$USUBJID == "001"], normal_bmi)

  # Test all edge cases - BMI should be NA
  expect_true(is.na(result$BMI[result$USUBJID == "002"]), "Missing HEIGHT should result in NA BMI")
  expect_true(is.na(result$BMI[result$USUBJID == "003"]), "Missing WEIGHT should result in NA BMI")
  expect_true(is.na(result$BMI[result$USUBJID == "004"]), "Zero HEIGHT should result in NA BMI")
  expect_true(is.na(result$BMI[result$USUBJID == "005"]), "Negative HEIGHT should result in NA BMI")
  expect_true(is.na(result$BMI[result$USUBJID == "006"]), "Zero WEIGHT should result in NA BMI")
})
