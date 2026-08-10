# Test file for crea_mdrd function

test_that("crea_mdrd matches the inverse MDRD formula for known inputs", {
  test_data <- tibble::tribble(
    ~egfr, ~age, ~sex, ~race,    ~female_factor, ~race_factor,
    90,    50,   0,    "WHITE",  1,              1,
    90,    50,   1,    "WHITE",  0.742,          1,
    90,    50,   0,    "BLACK",  1,              1.212,
    60,    70,   1,    "BLACK",  0.742,          1.212
  )

  expected <- (test_data$egfr /
    (175 * test_data$age^-0.203 * test_data$female_factor *
       test_data$race_factor))^(-1 / 1.154)

  result <- as.numeric(crea_mdrd(
    test_data$egfr,
    test_data$age,
    test_data$sex,
    test_data$race
  ))

  expect_equal(result, expected)
})


test_that("crea_mdrd returns mg/dl unit attribute", {
  result <- crea_mdrd(egfr = 90, age = 50, sex = 0)

  expect_equal(attr(result, "unit"), "mg/dl")
})


test_that("crea_mdrd is the inverse of egfr_mdrd", {
  test_data <- tibble::tribble(
    ~crea, ~age, ~sex, ~race,
    1.0,   50,   0,    "WHITE",
    0.8,   45,   1,    "WHITE",
    1.2,   60,   0,    "BLACK",
    0.9,   55,   1,    "BLACK"
  )

  egfr <- as.numeric(egfr_mdrd(
    test_data$crea,
    test_data$age,
    test_data$sex,
    test_data$race
  ))

  result <- as.numeric(crea_mdrd(
    egfr,
    test_data$age,
    test_data$sex,
    test_data$race
  ))

  expect_equal(result, test_data$crea)
})


test_that("crea_mdrd handles numeric sex encoding", {
  result_male <- as.numeric(crea_mdrd(egfr = 90, age = 50, sex = 0))
  result_female <- as.numeric(crea_mdrd(egfr = 90, age = 50, sex = 1))

  expect_true(result_female < result_male)
  expect_equal(
    result_female,
    as.numeric(crea_mdrd(egfr = 90, age = 50, sex = "F"))
  )
  expect_equal(
    result_male,
    as.numeric(crea_mdrd(egfr = 90, age = 50, sex = "M"))
  )
})


test_that("crea_mdrd handles character sex encoding as F or M", {
  result_f <- as.numeric(crea_mdrd(egfr = 90, age = 50, sex = "F"))
  result_m <- as.numeric(crea_mdrd(egfr = 90, age = 50, sex = "M"))

  expect_true(result_f < result_m)
})


test_that("crea_mdrd treats lowercase f as non-female", {
  # Current behavior: only sex == 1 or sex == "F" applies female factor
  result_upper <- as.numeric(crea_mdrd(egfr = 90, age = 50, sex = "F"))
  result_lower <- as.numeric(crea_mdrd(egfr = 90, age = 50, sex = "f"))
  result_male <- as.numeric(crea_mdrd(egfr = 90, age = 50, sex = 0))

  expect_equal(result_lower, result_male)
  expect_false(isTRUE(all.equal(result_lower, result_upper)))
})


test_that("crea_mdrd applies black race factor", {
  result_white <- as.numeric(crea_mdrd(egfr = 90, age = 50, sex = 0, race = "WHITE"))
  result_black <- as.numeric(crea_mdrd(egfr = 90, age = 50, sex = 0, race = "BLACK"))

  expect_true(result_black > result_white)
})


test_that("crea_mdrd handles race case-insensitively", {
  result_upper <- as.numeric(crea_mdrd(egfr = 90, age = 50, sex = 0, race = "BLACK"))
  result_lower <- as.numeric(crea_mdrd(egfr = 90, age = 50, sex = 0, race = "black"))
  result_mixed <- as.numeric(crea_mdrd(egfr = 90, age = 50, sex = 0, race = "Black"))

  expect_equal(result_lower, result_upper)
  expect_equal(result_mixed, result_upper)
})


test_that("crea_mdrd detects black race by substring", {
  result_substring <- as.numeric(
    crea_mdrd(egfr = 90, age = 50, sex = 0, race = "AFRICAN AMERICAN BLACK")
  )
  result_black <- as.numeric(
    crea_mdrd(egfr = 90, age = 50, sex = 0, race = "BLACK")
  )

  expect_equal(result_substring, result_black)
})


test_that("crea_mdrd defaults race to non-black when omitted", {
  result_default <- as.numeric(crea_mdrd(egfr = 90, age = 50, sex = 0))
  result_empty <- as.numeric(crea_mdrd(egfr = 90, age = 50, sex = 0, race = ""))
  result_white <- as.numeric(crea_mdrd(egfr = 90, age = 50, sex = 0, race = "WHITE"))

  expect_equal(result_default, result_empty)
  expect_equal(result_default, result_white)
})


test_that("crea_mdrd is vectorized over inputs", {
  test_data <- tibble::tribble(
    ~egfr, ~age, ~sex, ~race,
    90,    50,   0,    "WHITE",
    80,    60,   1,    "BLACK",
    70,    40,   0,    "ASIAN"
  )

  result <- as.numeric(crea_mdrd(
    test_data$egfr,
    test_data$age,
    test_data$sex,
    test_data$race
  ))

  expect_length(result, 3)
  expect_false(any(is.na(result)))
})


test_that("crea_mdrd returns NA when egfr or age is NA", {
  test_data <- tibble::tribble(
    ~egfr,    ~age,     ~sex,
    NA_real_, 50,       0,
    90,       NA_real_, 0,
    90,       50,       0
  )

  result <- as.numeric(crea_mdrd(
    test_data$egfr,
    test_data$age,
    test_data$sex
  ))

  expect_true(is.na(result[1]))
  expect_true(is.na(result[2]))
  expect_false(is.na(result[3]))
})


test_that("crea_mdrd decreases as egfr increases", {
  test_data <- tibble::tribble(
    ~egfr, ~age, ~sex,
    30,    50,   0,
    60,    50,   0,
    90,    50,   0
  )

  result <- as.numeric(crea_mdrd(
    test_data$egfr,
    test_data$age,
    test_data$sex
  ))

  expect_true(result[1] > result[2])
  expect_true(result[2] > result[3])
})


test_that("crea_mdrd decreases with age at fixed egfr", {
  test_data <- tibble::tribble(
    ~egfr, ~age, ~sex,
    90,    30,   0,
    90,    50,   0,
    90,    70,   0
  )

  result <- as.numeric(crea_mdrd(
    test_data$egfr,
    test_data$age,
    test_data$sex
  ))

  expect_true(result[1] > result[2])
  expect_true(result[2] > result[3])
})


test_that("crea_mdrd returns Inf when egfr is zero", {
  result <- as.numeric(crea_mdrd(egfr = 0, age = 50, sex = 0))

  expect_true(is.infinite(result))
})


test_that("crea_mdrd returns values in a plausible serum creatinine range", {
  test_data <- tibble::tribble(
    ~egfr, ~age, ~sex, ~race,
    90,    40,   0,    "WHITE",
    60,    55,   1,    "WHITE",
    45,    65,   0,    "BLACK"
  )

  result <- as.numeric(crea_mdrd(
    test_data$egfr,
    test_data$age,
    test_data$sex,
    test_data$race
  ))

  expect_true(all(result > 0.4))
  expect_true(all(result < 3))
})
