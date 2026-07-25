## Tests for is_parseable_dtc and filter_parseable_dtc


# ---- is_parseable_dtc --------------------------------------------------------

test_that("is_parseable_dtc accepts all dtc_formats", {
  expect_equal(
    is_parseable_dtc(c(
      "2025-01-14T08:15",
      "2025-01-14",
      "2025-01",
      "2025-01-14T08:15:30",
      "2025",
      "2025-01-14 08:15",
      "2025-01-14 08:15:30"
    )),
    rep(TRUE, 7)
  )
})


test_that("is_parseable_dtc returns NA for missing and empty strings", {
  expect_equal(
    is_parseable_dtc(c(NA_character_, "")),
    c(NA, NA)
  )
  expect_true(all(is.na(is_parseable_dtc(character(0)))))
  expect_true(all(is.na(is_parseable_dtc(c(NA_character_, NA_character_)))))
})


test_that("is_parseable_dtc rejects unparseable values", {
  expect_equal(
    is_parseable_dtc(c(
      "2025-01-14T08",
      "2025-01-14T",
      "2025-01-14T08:15:00.000",
      "2025-01-14T08:15:00Z",
      "2025-01-14T08:15 ",
      " ",
      "UNK",
      "2025/01/14T08:15"
    )),
    rep(FALSE, 8)
  )
})


test_that("is_parseable_dtc is vectorized and preserves length", {
  x <- c("2025-01-14T08:00", NA, "2025-01-14T08", "", "2025-01-14")
  result <- is_parseable_dtc(x)

  expect_length(result, 5L)
  expect_type(result, "logical")
  expect_equal(result, c(TRUE, NA, FALSE, NA, TRUE))
})


test_that("is_parseable_dtc coerces non-character input", {
  expect_equal(is_parseable_dtc(2025), TRUE)
  expect_equal(is_parseable_dtc(factor("2025-01-14")), TRUE)
})


test_that("is_parseable_dtc does not emit lubridate parse warnings", {
  expect_no_warning(
    result <- is_parseable_dtc(c("2025-01-14T08", "bad", "2025-01-14"))
  )
  expect_equal(result, c(FALSE, FALSE, TRUE))
})


# ---- filter_parseable_dtc ----------------------------------------------------

test_that("filter_parseable_dtc validates inputs", {
  expect_error(filter_parseable_dtc("not a df", "PCRFTDTC"), "data frame")
  expect_error(filter_parseable_dtc(123, "PCRFTDTC"), "data frame")
  expect_error(
    filter_parseable_dtc(tibble::tibble(PCRFTDTC = "2025-01-14"), "MISSING"),
    "Column not found"
  )
  expect_error(
    filter_parseable_dtc(tibble::tibble(PCRFTDTC = "2025-01-14"), c("A", "B")),
    "single value|character"
  )
})


test_that("filter_parseable_dtc returns empty data frame unchanged", {
  empty <- tibble::tibble(USUBJID = character(), PCRFTDTC = character())
  result <- filter_parseable_dtc(empty, "PCRFTDTC", silent = TRUE)

  expect_equal(nrow(result), 0L)
  expect_equal(names(result), names(empty))
})


test_that("filter_parseable_dtc returns POSIXct columns unchanged", {
  obj <- tibble::tribble(
    ~USUBJID, ~PCRFTDTC,
         "1", as.POSIXct("2025-01-14 08:00:00", tz = "UTC"),
         "2", as.POSIXct("2025-01-15 09:00:00", tz = "UTC")
  )

  result <- filter_parseable_dtc(obj, "PCRFTDTC", silent = TRUE)

  expect_identical(result, obj)
  expect_s3_class(result$PCRFTDTC, "POSIXct")
})


test_that("filter_parseable_dtc keeps parseable DTC values", {
  obj <- tibble::tribble(
    ~USUBJID,          ~PCRFTDTC,
         "1", "2025-01-14T08:00",
         "2",       "2025-01-14",
         "3", "2025-01-14T08:15:30"
  )

  result <- filter_parseable_dtc(obj, "PCRFTDTC", silent = TRUE)

  expect_equal(nrow(result), 3L)
  expect_equal(result$USUBJID, c("1", "2", "3"))
})


test_that("filter_parseable_dtc trims trailing space so datetime becomes parseable", {
  obj <- tibble::tribble(
    ~USUBJID,           ~PCRFTDTC,
         "1", "2025-01-14T08:15 "
  )

  result <- filter_parseable_dtc(obj, "PCRFTDTC", silent = TRUE)

  expect_equal(nrow(result), 1L)
  expect_equal(result$PCRFTDTC, "2025-01-14T08:15")
})


test_that("filter_parseable_dtc coerces blank and empty strings to NA", {
  obj <- tibble::tribble(
    ~USUBJID, ~PCRFTDTC,
         "1",        "",
         "2",       " ",
         "3", "2025-01-14T08:00"
  )

  result <- filter_parseable_dtc(obj, "PCRFTDTC", silent = TRUE)

  expect_equal(nrow(result), 3L)
  expect_true(is.na(result$PCRFTDTC[1]))
  expect_true(is.na(result$PCRFTDTC[2]))
  expect_equal(result$PCRFTDTC[3], "2025-01-14T08:00")
})


test_that("filter_parseable_dtc drops unparseable rows", {
  obj <- tibble::tribble(
    ~USUBJID,             ~PCRFTDTC,
         "1",    "2025-01-14T08:00",
         "2",       "2025-01-14T08",
         "3", "2025-01-14T08:00:00Z",
         "4",          "2025-01-14"
  )

  result <- filter_parseable_dtc(obj, "PCRFTDTC", silent = TRUE)

  expect_equal(nrow(result), 2L)
  expect_equal(result$USUBJID, c("1", "4"))
  expect_equal(result$PCRFTDTC, c("2025-01-14T08:00", "2025-01-14"))
})


test_that("filter_parseable_dtc messages about dropped unparseable values", {
  obj <- tibble::tribble(
    ~USUBJID,          ~PCRFTDTC,
         "1", "2025-01-14T08:00",
         "2",    "2025-01-14T08",
         "3",             "UNK"
  )

  expect_message(
    result <- filter_parseable_dtc(obj, "PCRFTDTC", silent = FALSE),
    "unparseable PCRFTDTC"
  )
  expect_equal(nrow(result), 1L)

  expect_no_message(
    filter_parseable_dtc(obj, "PCRFTDTC", silent = TRUE)
  )
})


test_that("filter_parseable_dtc message uses singular and plural row wording", {
  one_bad <- tibble::tribble(
    ~USUBJID,       ~PCRFTDTC,
         "1", "2025-01-14T08"
  )
  two_bad <- tibble::tribble(
    ~USUBJID,       ~PCRFTDTC,
         "1", "2025-01-14T08",
         "2",          "UNK"
  )

  expect_message(
    filter_parseable_dtc(one_bad, "PCRFTDTC", silent = FALSE),
    "1 row with unparseable"
  )
  expect_message(
    filter_parseable_dtc(two_bad, "PCRFTDTC", silent = FALSE),
    "2 rows with unparseable"
  )
})


test_that("filter_parseable_dtc preserves non-target columns", {
  obj <- tibble::tribble(
    ~USUBJID, ~PCTESTCD,             ~PCDTC,          ~PCRFTDTC,
         "1",       "A", "2025-01-14T09:00", "2025-01-14T08:00",
         "2",       "A", "2025-01-14T10:00",    "2025-01-14T08"
  )

  result <- filter_parseable_dtc(obj, "PCRFTDTC", silent = TRUE)

  expect_equal(names(result), names(obj))
  expect_equal(nrow(result), 1L)
  expect_equal(result$PCDTC, "2025-01-14T09:00")
  expect_equal(result$PCTESTCD, "A")
})


test_that("filter_parseable_dtc works on PCDTC as well as PCRFTDTC", {
  obj <- tibble::tribble(
    ~USUBJID,             ~PCDTC,
         "1", "2025-01-14T09:00",
         "2",    "2025-01-14T09"
  )

  expect_message(
    result <- filter_parseable_dtc(obj, "PCDTC", silent = FALSE),
    "unparseable PCDTC"
  )
  expect_equal(nrow(result), 1L)
  expect_equal(result$PCDTC, "2025-01-14T09:00")
})


test_that("filter_parseable_dtc keeps rows with NA in the target column", {
  obj <- tibble::tribble(
    ~USUBJID,          ~PCRFTDTC,
         "1",                NA,
         "2", "2025-01-14T08:00",
         "3",    "2025-01-14T08"
  )

  result <- filter_parseable_dtc(obj, "PCRFTDTC", silent = TRUE)

  expect_equal(nrow(result), 2L)
  expect_equal(result$USUBJID, c("1", "2"))
  expect_true(is.na(result$PCRFTDTC[1]))
})


test_that("filter_parseable_dtc does not emit lubridate parse warnings", {
  obj <- tibble::tribble(
    ~USUBJID,                    ~PCRFTDTC,
         "1",           "2025-01-14T08:00",
         "2",              "2025-01-14T08",
         "3", "2025-01-14T08:00:00.000",
         "4",                     " "
  )

  expect_no_warning(
    result <- filter_parseable_dtc(obj, "PCRFTDTC", silent = TRUE)
  )
  expect_equal(result$USUBJID, c("1", "4"))
})
