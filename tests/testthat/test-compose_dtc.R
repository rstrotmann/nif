## Characterization tests for compose_dtc()
##
## Pins parse rules so the helper can be vectorized without allocating a
## data.frame: date+time, date-only (NA/empty time), recycling, UTC POSIXct,
## and failed parses (message + NULL).

compose_dtc <- nif:::compose_dtc

utc <- function(x) {
  as.POSIXct(x, tz = "UTC")
}


# ---- successful parses -------------------------------------------------------

test_that("compose_dtc combines date and time into UTC POSIXct", {
  result <- compose_dtc("2022-09-29", "09:30")

  expect_s3_class(result, "POSIXct")
  expect_equal(attr(result, "tzone"), "UTC")
  expect_equal(result, utc("2022-09-29 09:30:00"))
})


test_that("compose_dtc treats NA time as date-only midnight UTC", {
  result <- compose_dtc("2022-09-29", NA_character_)

  expect_equal(result, utc("2022-09-29"))
})


test_that("compose_dtc treats empty time like NA time", {
  expect_equal(
    compose_dtc("2022-09-29", ""),
    compose_dtc("2022-09-29", NA_character_)
  )
})


test_that("compose_dtc treats logical NA time like character NA time", {
  expect_equal(
    compose_dtc("2022-09-29", NA),
    compose_dtc("2022-09-29", NA_character_)
  )
})


test_that("compose_dtc treats 00:00 as midnight", {
  expect_equal(
    compose_dtc("2022-09-29", "00:00"),
    utc("2022-09-29")
  )
})


test_that("compose_dtc trims surrounding whitespace on the pasted stamp", {
  expect_equal(
    compose_dtc("2022-09-29", " 09:30 "),
    utc("2022-09-29 09:30:00")
  )
  expect_equal(
    compose_dtc(" 2022-09-29 ", "09:30"),
    utc("2022-09-29 09:30:00")
  )
})


test_that("compose_dtc accepts Date input", {
  expect_equal(
    compose_dtc(as.Date("2022-09-29"), "09:30"),
    utc("2022-09-29 09:30:00")
  )
})


test_that("compose_dtc accepts midnight POSIXct date input", {
  expect_equal(
    compose_dtc(utc("2022-09-29"), "09:30"),
    utc("2022-09-29 09:30:00")
  )
})


test_that("compose_dtc accepts factor date and time", {
  expect_equal(
    compose_dtc(factor("2022-09-29"), factor("09:30")),
    utc("2022-09-29 09:30:00")
  )
})


test_that("compose_dtc parses a leap-day date", {
  expect_equal(
    compose_dtc("2024-02-29", "08:00"),
    utc("2024-02-29 08:00:00")
  )
  expect_equal(
    compose_dtc("2024-02-29", NA_character_),
    utc("2024-02-29")
  )
})


# ---- vectors -----------------------------------------------------------------

test_that("compose_dtc is vectorized over mixed times and NA times", {
  result <- compose_dtc(
    c("2022-09-29", "2022-09-30", "2022-10-01"),
    c("09:30", NA_character_, "00:00")
  )

  expect_equal(
    result,
    c(
      utc("2022-09-29 09:30:00"),
      utc("2022-09-30"),
      utc("2022-10-01")
    )
  )
})


test_that("compose_dtc recycles a length-1 time across dates", {
  result <- compose_dtc(
    c("2022-09-29", "2022-09-30"),
    "09:30"
  )

  expect_equal(
    result,
    utc(c("2022-09-29 09:30:00", "2022-09-30 09:30:00"))
  )
})


test_that("compose_dtc recycles a length-1 date across times", {
  result <- compose_dtc(
    "2022-09-29",
    c("09:30", "10:00")
  )

  expect_equal(
    result,
    utc(c("2022-09-29 09:30:00", "2022-09-29 10:00:00"))
  )
})


test_that("compose_dtc returns empty POSIXct for empty inputs", {
  result <- compose_dtc(character(), character())

  expect_s3_class(result, "POSIXct")
  expect_equal(length(result), 0)
  expect_equal(attr(result, "tzone"), "UTC")
})


test_that("compose_dtc errors when date and time lengths do not match", {
  expect_error(
    compose_dtc(
      c("2022-09-29", "2022-09-30", "2022-10-01"),
      c("09:30", "10:00")
    ),
    "arguments imply differing number of rows: 3, 2"
  )
})


# ---- failed parses -----------------------------------------------------------

test_that("compose_dtc messages and returns NULL for an unparseable date", {
  expect_message(
    result <- compose_dtc("not-a-date", "09:30"),
    "Warning from composing DTC"
  )
  expect_null(result)
})


test_that("compose_dtc messages and returns NULL for an unparseable time", {
  expect_message(
    result <- compose_dtc("2022-09-29", "25:00"),
    "Warning from composing DTC"
  )
  expect_null(result)
})


test_that("compose_dtc messages and returns NULL for NA date", {
  expect_message(
    result <- compose_dtc(NA_character_, "09:30"),
    "Warning from composing DTC"
  )
  expect_null(result)
})


test_that("compose_dtc messages and returns NULL for seconds in the time", {
  expect_message(
    result <- compose_dtc("2022-09-29", "09:30:00"),
    "Warning from composing DTC"
  )
  expect_null(result)
})


test_that("compose_dtc messages and returns NULL when any vector element fails", {
  expect_message(
    result <- compose_dtc(
      c("2022-09-29", "not-a-date"),
      c("09:30", NA_character_)
    ),
    "Warning from composing DTC"
  )
  expect_null(result)
})
