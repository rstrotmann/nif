test_that("pctpt_to_hours returns NA for NA input", {
  expect_true(is.na(pctpt_to_hours(NA)))
  expect_true(is.na(pctpt_to_hours(NA_character_)))
})


test_that("pctpt_to_hours returns 0 for PRE-DOSE variations", {
  test <- tribble(
    ~input,      ~expected,
    "PRE-DOSE",  0,
    "PREDOSE",   0,
    "PRE DOSE",  0
  )
  expect_equal(pctpt_to_hours(test$input), test$expected)
})


test_that("pctpt_to_hours does not match partial PRE-DOSE strings", {
  test <- tribble(
    ~input,
    "PREDOSING",
    "NOT PREDOSE",
    "PRE-DOSE EXTRA",
    "MY PREDOSE"
  )
  expect_true(all(is.na(pctpt_to_hours(test$input))))
})


test_that("pctpt_to_hours extracts hours with H suffix", {
  test <- tribble(
    ~input,            ~expected,
    "1H POST-DOSE",    1,
    "2H POST-DOSE",    2,
    "4H POST-DOSE",    4,
    "24H POST-DOSE",   24,
    "72H POST-DOSE",   72,
    "0.5H POST-DOSE",  0.5,
    "1.5H POST-DOSE",  1.5
  )
  expect_equal(pctpt_to_hours(test$input), test$expected)
})


test_that("pctpt_to_hours extracts hours with HR and HRS suffix", {
  test <- tribble(
    ~input,              ~expected,
    "1HR POST-DOSE",     1,
    "2HRS POST-DOSE",    2,
    "4 HR POST-DOSE",    4,
    "24 HRS POST-DOSE",  24,
    "0.5HR POST-DOSE",   0.5
  )
  expect_equal(pctpt_to_hours(test$input), test$expected)
})


test_that("pctpt_to_hours extracts hours with HOUR and HOURS suffix", {
  test <- tribble(
    ~input,                ~expected,
    "1 HOUR POST-DOSE",    1,
    "2 HOURS POST-DOSE",   2,
    "4HOUR POST-DOSE",     4,
    "24HOURS POST-DOSE",   24,
    "0.5 HOUR POST-DOSE",  0.5,
    "1.5 HOURS POST-DOSE", 1.5
  )
  expect_equal(pctpt_to_hours(test$input), test$expected)
})


test_that("pctpt_to_hours extracts minutes with M suffix", {
  test <- tribble(
    ~input,           ~expected,
    "5M POST-DOSE",   5/60,
    "15M POST-DOSE",  15/60,
    "30M POST-DOSE",  30/60
  )
  expect_equal(pctpt_to_hours(test$input), test$expected)
})


test_that("pctpt_to_hours extracts minutes with MIN and MINS suffix", {
  test <- tribble(
    ~input,              ~expected,
    "15MIN POST-DOSE",   15/60,
    "30 MIN POST-DOSE",  30/60,
    "45MINS POST-DOSE",  45/60,
    "90 MINS POST-DOSE", 90/60
  )
  expect_equal(pctpt_to_hours(test$input), test$expected)
})


test_that("pctpt_to_hours extracts minutes with MINUTE and MINUTES suffix", {
  test <- tribble(
    ~input,                  ~expected,
    "15 MINUTE POST-DOSE",   15/60,
    "30 MINUTES POST-DOSE",  30/60,
    "90 MINUTES POST-DOSE",  90/60,
    "0.5 MINUTES POST-DOSE", 0.5/60
  )
  expect_equal(pctpt_to_hours(test$input), test$expected)
})


test_that("pctpt_to_hours combines hours and minutes via row sum", {
  test <- tribble(
    ~input,                     ~expected,
    "2H 30MIN POST-DOSE",       2.5,
    "1H 15MIN POST-DOSE",       75/60,
    "4 HOURS 30 MINUTES",       4.5,
    "1HR 45MINS POST-DOSE",     105/60,
    "0.5H 15 MINUTES POST-DOSE", 45/60
  )
  expect_equal(pctpt_to_hours(test$input), test$expected)
})


test_that("pctpt_to_hours returns NA for unrecognized patterns", {
  test <- tribble(
    ~input,
    "RANDOM TEXT",
    "POST-DOSE",
    "DOSE",
    "",
    "SOME TIME POINT",
    "DAY 1"
  )
  expect_true(all(is.na(pctpt_to_hours(test$input))))
})


test_that("pctpt_to_hours works without POST-DOSE suffix", {
  test <- tribble(
    ~input,       ~expected,
    "2H",         2,
    "4 HOURS",    4,
    "1HR",        1,
    "30MIN",      0.5,
    "15 MINUTES", 15/60
  )
  expect_equal(pctpt_to_hours(test$input), test$expected)
})


test_that("pctpt_to_hours handles spacing variations between number and unit", {
  test <- tribble(
    ~input,       ~expected,
    "2H",         2,
    "2 H",        2,
    "30MIN",      30/60,
    "30 MIN",     30/60,
    "4HOURS",     4,
    "4 HOURS",    4
  )
  expect_equal(pctpt_to_hours(test$input), test$expected)
})


test_that("pctpt_to_hours is vectorized", {
  input <- c("PREDOSE", "1H POST-DOSE", "2H POST-DOSE", "30MIN POST-DOSE", NA)
  expected <- c(0, 1, 2, 30/60, NA)
  expect_equal(pctpt_to_hours(input), expected)
})


test_that("pctpt_to_hours returns numeric vector", {
  expect_type(pctpt_to_hours("2H POST-DOSE"), "double")
  expect_type(pctpt_to_hours("30MIN POST-DOSE"), "double")
  expect_type(pctpt_to_hours("PREDOSE"), "double")
  expect_type(pctpt_to_hours(c("1H", "2H", "PREDOSE")), "double")
})


test_that("pctpt_to_hours handles single-element vector", {
  expect_equal(pctpt_to_hours("4H POST-DOSE"), 4)
  expect_equal(pctpt_to_hours("PREDOSE"), 0)
  expect_true(is.na(pctpt_to_hours("UNKNOWN")))
})


test_that("pctpt_to_hours handles mixed recognized and unrecognized inputs", {
  test <- tribble(
    ~input,            ~expected,
    "PREDOSE",         0,
    "1H POST-DOSE",    1,
    "RANDOM TEXT",     NA,
    "30MIN POST-DOSE", 30/60,
    "UNKNOWN",         NA,
    "24H POST-DOSE",   24
  )
  expect_equal(pctpt_to_hours(test$input), test$expected)
})

