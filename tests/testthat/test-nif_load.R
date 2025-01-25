test_that("is_char_datetime works correctly", {
  expect_true(all(is_char_datetime(
    c("2009-12-02T11:45", "2009-12-02T12:00:10", "2025-01-17T08:00")
  )))
  expect_false(is_char_datetime("2009-12-02"))
  expect_false(is_char_datetime("2009-12-02T"))
  expect_false(is_char_datetime("2009-12-02 8:00"))
  expect_false(is_char_datetime("2025-01-17T8:00"))
})


test_that("is_likely_datetime works as intended", {
  test <- c(
    "2025-01-20 08:04:43",
    "2025-01-20 08:04",
    "2025-01-20T08:04:43",
    "2025-01-20T08:04",
    "2025-01-20",
    "25-01-20 08:04",       # wrong year format
    "A"                     # not a date
  )

  expect_false(is_likely_datetime(c(1, 2, 3, 4)))
  expect_true(is_likely_datetime(test[1:5]))
  expect_false(is_likely_datetime(test[1:6]))
  expect_false(is_likely_datetime(test[c(1, 2, 3, 4, 5, 7)]))
  expect_true(is_likely_datetime("2025-01-22 08:35"))
})


test_that("convert_char_datetime works correctly", {
  silent <- TRUE

  test <- c(
    "2025-01-20 08:04:43",
    "2025-01-20 08:04",
    "2025-01-20T08:04:43",
    "2025-01-20T08:04",
    "2025-01-20 08:04",
    "2025-01-20",
    "A"
  )

  convert_char_datetime(test, min_prob = 0.8, silent = silent)
  expect_error(convert_char_datetime(test, min_prob = 0.9, silent = silent))
  expect_s3_class(
    convert_char_datetime("2025-01-22 08:48", silent = silent),
    "POSIXct")
})


test_that("import from connection works", {
  test <- r"(
  USUBJID  TIME EVID DV RACE
  # comment line
  0001     0    0    .  WHITE
  0001     1    0    1. ASIAN

  0001     2    0    2  WHITE
  )"

  connection <- textConnection(test, open="r")
  expect_no_error(temp <- import_from_connection(connection, silent = TRUE))
})


