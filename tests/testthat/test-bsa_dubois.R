# Test file for bsa_dubois function

test_that("bsa_dubois matches the Du Bois formula for known inputs", {
  test_data <- tibble::tribble(
    ~weight, ~height, ~expected,
    70,      170,     0.007184 * 170^0.725 * 70^0.425,
    60,      160,     0.007184 * 160^0.725 * 60^0.425,
    80,      180,     0.007184 * 180^0.725 * 80^0.425
  )

  result <- bsa_dubois(test_data$weight, test_data$height)

  expect_equal(result, test_data$expected)
})


test_that("bsa_dubois returns a numeric scalar for single inputs", {
  result <- bsa_dubois(weight = 70, height = 170)

  expect_type(result, "double")
  expect_length(result, 1)
  expect_false(is.na(result))
})


test_that("bsa_dubois is vectorized over weight and height", {
  test_data <- tibble::tribble(
    ~weight, ~height,
    70,      170,
    60,      160,
    90,      185
  )

  result <- bsa_dubois(test_data$weight, test_data$height)

  expect_length(result, 3)
  expect_equal(
    result,
    0.007184 * test_data$height^0.725 * test_data$weight^0.425
  )
})


test_that("bsa_dubois returns NA for missing weight or height", {
  test_data <- tibble::tribble(
    ~weight, ~height,
    NA_real_, 170,
    70,       NA_real_,
    NA_real_, NA_real_,
    70,       170
  )

  result <- bsa_dubois(test_data$weight, test_data$height)

  expect_true(is.na(result[1]))
  expect_true(is.na(result[2]))
  expect_true(is.na(result[3]))
  expect_false(is.na(result[4]))
})


test_that("bsa_dubois returns NA for non-positive weight or height", {
  test_data <- tibble::tribble(
    ~weight, ~height,
    0,       170,
    -70,     170,
    70,      0,
    70,      -170,
    70,      170
  )

  result <- bsa_dubois(test_data$weight, test_data$height)

  expect_true(is.na(result[1]))
  expect_true(is.na(result[2]))
  expect_true(is.na(result[3]))
  expect_true(is.na(result[4]))
  expect_false(is.na(result[5]))
})


test_that("bsa_dubois handles mixed valid and invalid vector inputs", {
  test_data <- tibble::tribble(
    ~weight,  ~height,
    70,       170,
    NA_real_, 160,
    0,        180,
    65,       165,
    80,       -10
  )

  result <- bsa_dubois(test_data$weight, test_data$height)

  expect_equal(
    result[c(1, 4)],
    0.007184 * test_data$height[c(1, 4)]^0.725 *
      test_data$weight[c(1, 4)]^0.425
  )
  expect_true(all(is.na(result[c(2, 3, 5)])))
})


test_that("bsa_dubois accepts integer weight and height", {
  result <- bsa_dubois(weight = 70L, height = 170L)
  expected <- 0.007184 * 170^0.725 * 70^0.425

  expect_equal(result, expected)
})


test_that("bsa_dubois returns numeric(0) for empty inputs", {
  result <- bsa_dubois(weight = numeric(0), height = numeric(0))

  expect_equal(result, numeric(0))
})


test_that("bsa_dubois errors when weight and height lengths differ", {
  expect_error(
    bsa_dubois(weight = c(70, 60), height = 170),
    "Height and weight vectors must have the same length"
  )

  expect_error(
    bsa_dubois(weight = 70, height = c(170, 160)),
    "Height and weight vectors must have the same length"
  )
})


test_that("bsa_dubois errors when weight or height is non-numeric", {
  expect_error(
    bsa_dubois(weight = "70", height = 170),
    "Height and weight must be numeric values"
  )

  expect_error(
    bsa_dubois(weight = 70, height = "170"),
    "Height and weight must be numeric values"
  )
})


test_that("bsa_dubois increases with higher weight at fixed height", {
  test_data <- tibble::tribble(
    ~weight, ~height,
    50,      170,
    70,      170,
    90,      170
  )

  result <- bsa_dubois(test_data$weight, test_data$height)

  expect_true(result[1] < result[2])
  expect_true(result[2] < result[3])
})


test_that("bsa_dubois increases with higher height at fixed weight", {
  test_data <- tibble::tribble(
    ~weight, ~height,
    70,      150,
    70,      170,
    70,      190
  )

  result <- bsa_dubois(test_data$weight, test_data$height)

  expect_true(result[1] < result[2])
  expect_true(result[2] < result[3])
})


test_that("bsa_dubois returns values in a plausible adult range", {
  test_data <- tibble::tribble(
    ~weight, ~height,
    50,      150,
    70,      170,
    100,     190
  )

  result <- bsa_dubois(test_data$weight, test_data$height)

  expect_true(all(result > 1))
  expect_true(all(result < 3))
})
