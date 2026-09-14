test_that("obesity_class maps NHLBI BMI cutoffs", {
  test_data <- tibble::tribble(
    ~bmi,  ~expected,
    16,    "underweight",
    18.49, "underweight",
    18.5,  "normal",
    22,    "normal",
    24.99, "normal",
    25,    "overweight",
    28,    "overweight",
    29.99, "overweight",
    30,    "obese class 1",
    33,    "obese class 1",
    34.99, "obese class 1",
    35,    "obese class 2",
    38,    "obese class 2",
    39.99, "obese class 2",
    40,    "obese class 3",
    45,    "obese class 3"
  )

  expect_equal(obesity_class(test_data$bmi), test_data$expected)
})


test_that("obesity_class is vectorized and preserves length", {
  bmi <- c(17, 22, 27, 32, 37, 42)

  result <- obesity_class(bmi)

  expect_length(result, length(bmi))
  expect_type(result, "character")
  expect_equal(
    result,
    c(
      "underweight", "normal", "overweight",
      "obese class 1", "obese class 2", "obese class 3"
    )
  )
})


test_that("obesity_class accepts integer BMI", {
  expect_equal(obesity_class(22L), "normal")
  expect_equal(obesity_class(c(16L, 30L)), c("underweight", "obese class 1"))
})


test_that("obesity_class returns NA for missing BMI without erroring", {
  expect_identical(obesity_class(NA_real_), NA_character_)
  expect_identical(
    obesity_class(c(22, NA_real_, 31)),
    c("normal", NA_character_, "obese class 1")
  )
  expect_identical(
    obesity_class(c(NA_real_, NA_real_)),
    c(NA_character_, NA_character_)
  )
})


test_that("obesity_class returns NA for non-positive BMI", {
  expect_identical(obesity_class(0), NA_character_)
  expect_identical(obesity_class(-1), NA_character_)
  expect_identical(
    obesity_class(c(-5, 0, 22)),
    c(NA_character_, NA_character_, "normal")
  )
})


test_that("obesity_class treats NaN like missing BMI", {
  expect_identical(obesity_class(NaN), NA_character_)
  expect_identical(
    obesity_class(c(22, NaN, 40)),
    c("normal", NA_character_, "obese class 3")
  )
})


test_that("obesity_class returns NA for infinite BMI", {
  expect_identical(obesity_class(Inf), NA_character_)
  expect_identical(obesity_class(-Inf), NA_character_)
  expect_identical(
    obesity_class(c(22, Inf, 40)),
    c("normal", NA_character_, "obese class 3")
  )
})


test_that("obesity_class returns empty character for empty input", {
  expect_identical(obesity_class(numeric(0)), character(0))
  expect_identical(obesity_class(integer(0)), character(0))
})


test_that("obesity_class errors for non-numeric input", {
  expect_error(obesity_class("22"), "BMI must be numeric!")
  expect_error(obesity_class(TRUE), "BMI must be numeric!")
  expect_error(obesity_class(factor(22)), "BMI must be numeric!")
  expect_error(obesity_class(NULL), "BMI must be numeric!")
  expect_error(obesity_class(list(22)), "BMI must be numeric!")
  expect_error(obesity_class(NA), "BMI must be numeric!")
})
