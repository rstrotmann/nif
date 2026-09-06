## Characterization tests for expand_ex()
##
## Pins expansion length, first/last-day times, IMPUTATION labels, EXDY, and
## per-episode independence so the second group_by(row_number()) pass can be
## replaced with .offset / .n_days without changing results.

expand_ex <- nif:::expand_ex
lubrify_dates <- nif:::lubrify_dates


# ---- validation --------------------------------------------------------------

test_that("expand_ex requires a data frame", {
  expect_error(expand_ex("not a data frame"), "Input must be a data frame")
  expect_error(expand_ex(123), "Input must be a data frame")
  expect_error(expand_ex(list()), "Input must be a data frame")
  expect_error(expand_ex(NULL), "Input must be a data frame")
})


test_that("expand_ex validates each required field", {
  base <- tibble::tribble(
    ~USUBJID, ~EXTRT,     ~EXSTDTC,    ~EXENDTC,
         "A", "DRUG", "2025-01-01", "2025-01-01"
  )

  for (col in c("USUBJID", "EXTRT", "EXSTDTC", "EXENDTC")) {
    obj <- base
    obj[[col]] <- NULL
    expect_error(expand_ex(obj), col, info = col)
  }
})


test_that("expand_ex reports multiple missing fields together", {
  expect_error(
    expand_ex(tibble::tibble(USUBJID = "A", EXTRT = "DRUG")),
    "Missing fields: EXSTDTC and EXENDTC!"
  )
})


test_that("expand_ex errors when end date is before start date", {
  ex <- tibble::tribble(
    ~USUBJID, ~EXTRT,           ~EXSTDTC,     ~EXENDTC,
         "A", "DRUG", "2025-01-03T07:00", "2025-01-01"
  ) |>
    lubrify_dates()

  expect_error(expand_ex(ex), "End date before start date for row\\(s\\): 1")
})


test_that("expand_ex lists all rows with end date before start date", {
  ex <- tibble::tribble(
    ~USUBJID, ~EXTRT,     ~EXSTDTC,    ~EXENDTC,
         "A", "DRUG", "2025-01-03", "2025-01-01",
         "B", "DRUG", "2025-01-05", "2025-01-04"
  ) |>
    lubrify_dates()

  expect_error(expand_ex(ex), "End date before start date for row\\(s\\): 1, 2")
})


test_that("expand_ex errors when EXENDY is before EXSTDY", {
  ex <- tibble::tribble(
    ~USUBJID, ~EXTRT,           ~EXSTDTC,    ~EXENDTC, ~EXSTDY, ~EXENDY,
         "A", "DRUG", "2025-01-01T07:00", "2025-01-03",       3,       1
  ) |>
    lubrify_dates()

  expect_error(expand_ex(ex), "End day before start day for row\\(s\\): 1")
})


test_that("expand_ex lists the original row index for EXENDY before EXSTDY", {
  ex <- tibble::tribble(
    ~USUBJID, ~EXTRT,     ~EXSTDTC,    ~EXENDTC, ~EXSTDY, ~EXENDY,
         "A", "DRUG", "2025-01-01", "2025-01-02",       1,       2,
         "B", "DRUG", "2025-01-01", "2025-01-02",       5,       3
  ) |>
    lubrify_dates()

  expect_error(expand_ex(ex), "End day before start day for row\\(s\\): 2")
})


# ---- expansion length and dates ---------------------------------------------

test_that("expand_ex keeps a single-day episode as one row", {
  ex <- tibble::tribble(
    ~USUBJID, ~EXTRT,           ~EXSTDTC,           ~EXENDTC,
         "A", "DRUG", "2025-01-01T07:00", "2025-01-01T08:00"
  ) |>
    lubrify_dates()

  result <- expand_ex(ex)

  expect_equal(nrow(result), 1)
  expect_equal(result$DTC_date, "2025-01-01")
})


test_that("expand_ex expands inclusive day count from start to end date", {
  ex <- tibble::tribble(
    ~USUBJID, ~EXTRT,           ~EXSTDTC,           ~EXENDTC,
         "A", "DRUG", "2025-01-01T07:00", "2025-01-05T08:00"
  ) |>
    lubrify_dates()

  result <- expand_ex(ex)

  expect_equal(
    result$DTC_date,
    c("2025-01-01", "2025-01-02", "2025-01-03", "2025-01-04", "2025-01-05")
  )
})


test_that("expand_ex crosses month and year boundaries", {
  ex <- tibble::tribble(
    ~USUBJID, ~EXTRT,           ~EXSTDTC,           ~EXENDTC,
         "A", "DRUG", "2024-12-30T07:00", "2025-01-02T08:00"
  ) |>
    lubrify_dates()

  result <- expand_ex(ex)

  expect_equal(
    result$DTC_date,
    c("2024-12-30", "2024-12-31", "2025-01-01", "2025-01-02")
  )
})


test_that("expand_ex includes Feb 29 in a leap year", {
  ex <- tibble::tribble(
    ~USUBJID, ~EXTRT,           ~EXSTDTC,           ~EXENDTC,
         "A", "DRUG", "2024-02-28T07:00", "2024-03-01T08:00"
  ) |>
    lubrify_dates()

  result <- expand_ex(ex)

  expect_equal(result$DTC_date, c("2024-02-28", "2024-02-29", "2024-03-01"))
})


test_that("expand_ex treats missing EXENDTC as a single day at EXSTDTC", {
  ex <- tibble::tribble(
    ~USUBJID, ~EXTRT,           ~EXSTDTC, ~EXENDTC,
         "A", "DRUG", "2025-01-01T07:00",       NA
  ) |>
    lubrify_dates()

  result <- expand_ex(ex)

  expect_equal(nrow(result), 1)
  expect_equal(result$DTC_date, "2025-01-01")
  expect_equal(result$DTC_time, "07:00")
  expect_equal(result$IMPUTATION, "time copied from EXSTDTC")
})


test_that("expand_ex expands each subject and treatment independently", {
  ex <- tibble::tribble(
    ~USUBJID, ~EXTRT,           ~EXSTDTC,    ~EXENDTC,
         "A", "DRUG1", "2025-01-01T07:00", "2025-01-02",
         "A", "DRUG2", "2025-01-01T08:00", "2025-01-01",
         "B", "DRUG1", "2025-01-01T09:00", "2025-01-03"
  ) |>
    lubrify_dates()

  result <- expand_ex(ex)

  expect_equal(nrow(result), 2 + 1 + 3)
  expect_equal(sum(result$USUBJID == "A" & result$EXTRT == "DRUG1"), 2)
  expect_equal(sum(result$USUBJID == "A" & result$EXTRT == "DRUG2"), 1)
  expect_equal(sum(result$USUBJID == "B"), 3)
})


# ---- DTC_time: first day, last day, middle days -----------------------------

test_that("expand_ex copies EXSTDTC time to the first day and EXENDTC time to the last", {
  ex <- tibble::tribble(
    ~USUBJID, ~EXTRT,           ~EXSTDTC,           ~EXENDTC,
         "A", "DRUG", "2025-01-01T07:00", "2025-01-04T08:00"
  ) |>
    lubrify_dates()

  result <- expand_ex(ex)

  expect_equal(result$DTC_time[1], "07:00")
  expect_true(is.na(result$DTC_time[2]))
  expect_true(is.na(result$DTC_time[3]))
  expect_equal(result$DTC_time[4], "08:00")
})


test_that("expand_ex prefers EXSTDTC time on a single-day episode with both times", {
  ex <- tibble::tribble(
    ~USUBJID, ~EXTRT,           ~EXSTDTC,           ~EXENDTC,
         "A", "DRUG", "2025-01-01T07:00", "2025-01-01T20:00"
  ) |>
    lubrify_dates()

  result <- expand_ex(ex)

  expect_equal(nrow(result), 1)
  expect_equal(result$DTC_time, "07:00")
  expect_equal(result$IMPUTATION, "time copied from EXSTDTC")
})


test_that("expand_ex uses EXENDTC time on a single-day episode with only end time", {
  ex <- tibble::tribble(
    ~USUBJID, ~EXTRT,     ~EXSTDTC,           ~EXENDTC,
         "A", "DRUG", "2025-01-01", "2025-01-01T08:00"
  ) |>
    lubrify_dates()

  result <- expand_ex(ex)

  expect_equal(nrow(result), 1)
  expect_equal(result$DTC_time, "08:00")
  expect_equal(result$IMPUTATION, "time copied from EXENDTC")
})


test_that("expand_ex leaves middle and last DTC_time NA when EXENDTC has no time", {
  ex <- tibble::tribble(
    ~USUBJID, ~EXTRT,           ~EXSTDTC,    ~EXENDTC,
         "A", "DRUG", "2025-01-01T07:00", "2025-01-03"
  ) |>
    lubrify_dates()

  result <- expand_ex(ex)

  expect_equal(result$DTC_time[1], "07:00")
  expect_true(is.na(result$DTC_time[2]))
  expect_true(is.na(result$DTC_time[3]))
  expect_equal(result$IMPUTATION, c("time copied from EXSTDTC", "", ""))
})


test_that("expand_ex copies only EXENDTC time when start has no time", {
  ex <- tibble::tribble(
    ~USUBJID, ~EXTRT,     ~EXSTDTC,           ~EXENDTC,
         "A", "DRUG", "2025-01-01", "2025-01-03T08:00"
  ) |>
    lubrify_dates()

  result <- expand_ex(ex)

  expect_equal(result$DTC_time, c(NA, NA, "08:00"))
  expect_equal(result$IMPUTATION, c("", "", "time copied from EXENDTC"))
})


test_that("expand_ex leaves all DTC_time NA when neither datetime has a time", {
  ex <- tibble::tribble(
    ~USUBJID, ~EXTRT,     ~EXSTDTC,    ~EXENDTC,
         "A", "DRUG", "2025-01-01", "2025-01-03"
  ) |>
    lubrify_dates()

  result <- expand_ex(ex)

  expect_true(all(is.na(result$DTC_time)))
  expect_equal(unique(result$IMPUTATION), "")
})


# ---- IMPUTATION --------------------------------------------------------------

test_that("expand_ex creates IMPUTATION when it is missing", {
  ex <- tibble::tribble(
    ~USUBJID, ~EXTRT,           ~EXSTDTC,    ~EXENDTC,
         "A", "DRUG", "2025-01-01T07:00", "2025-01-02"
  ) |>
    lubrify_dates()

  result <- expand_ex(ex)

  expect_true("IMPUTATION" %in% names(result))
  expect_equal(result$IMPUTATION, c("time copied from EXSTDTC", ""))
})


test_that("expand_ex appends expansion labels to existing IMPUTATION", {
  ex <- tibble::tribble(
    ~USUBJID, ~EXTRT,           ~EXSTDTC,           ~EXENDTC, ~IMPUTATION,
         "A", "DRUG", "2025-01-01T07:00", "2025-01-03T08:00",  "original"
  ) |>
    lubrify_dates()

  result <- expand_ex(ex)

  expect_equal(
    result$IMPUTATION,
    c(
      "original; time copied from EXSTDTC",
      "original",
      "original; time copied from EXENDTC"
    )
  )
})


test_that("expand_ex treats blank IMPUTATION like a missing label", {
  ex <- tibble::tribble(
    ~USUBJID, ~EXTRT,           ~EXSTDTC,           ~EXENDTC, ~IMPUTATION,
         "A", "DRUG", "2025-01-01T07:00", "2025-01-03T08:00",          ""
  ) |>
    lubrify_dates()

  result <- expand_ex(ex)

  expect_equal(
    result$IMPUTATION,
    c("time copied from EXSTDTC", "", "time copied from EXENDTC")
  )
})


# ---- EXDY --------------------------------------------------------------------

test_that("expand_ex derives EXDY from EXSTDY plus day offset", {
  ex <- tibble::tribble(
    ~USUBJID, ~EXTRT,           ~EXSTDTC,           ~EXENDTC, ~EXSTDY, ~EXENDY,
         "A", "DRUG", "2025-01-01T07:00", "2025-01-03T08:00",       1,       3
  ) |>
    lubrify_dates()

  result <- expand_ex(ex)

  expect_equal(result$EXDY, c(1, 2, 3))
})


test_that("expand_ex converts character EXSTDY and EXENDY to numeric EXDY", {
  ex <- tibble::tribble(
    ~USUBJID, ~EXTRT,           ~EXSTDTC,           ~EXENDTC, ~EXSTDY, ~EXENDY,
         "A", "DRUG", "2025-01-01T07:00", "2025-01-03T08:00",     "1",     "3"
  ) |>
    lubrify_dates()

  result <- expand_ex(ex)

  expect_equal(result$EXDY, c(1, 2, 3))
})


test_that("expand_ex does not create EXDY unless both EXSTDY and EXENDY exist", {
  start_only <- tibble::tribble(
    ~USUBJID, ~EXTRT,           ~EXSTDTC,           ~EXENDTC, ~EXSTDY,
         "A", "DRUG", "2025-01-01T07:00", "2025-01-03T08:00",       1
  ) |>
    lubrify_dates()

  end_only <- tibble::tribble(
    ~USUBJID, ~EXTRT,           ~EXSTDTC,           ~EXENDTC, ~EXENDY,
         "A", "DRUG", "2025-01-01T07:00", "2025-01-03T08:00",       3
  ) |>
    lubrify_dates()

  expect_false("EXDY" %in% names(expand_ex(start_only)))
  expect_false("EXDY" %in% names(expand_ex(end_only)))
})


test_that("expand_ex computes EXDY per episode, not across concatenated episodes", {
  ex <- tibble::tribble(
    ~USUBJID, ~EXTRT,           ~EXSTDTC,    ~EXENDTC, ~EXSTDY, ~EXENDY,
         "A", "DRUG", "2025-01-01T07:00", "2025-01-02",       1,       2,
         "A", "DRUG", "2025-01-04T08:00", "2025-01-05",       4,       5
  ) |>
    lubrify_dates()

  result <- expand_ex(ex)

  expect_equal(result$EXDY, c(1, 2, 4, 5))
})


test_that("expand_ex yields NA EXDY when EXSTDY is NA", {
  ex <- tibble::tribble(
    ~USUBJID, ~EXTRT,     ~EXSTDTC,    ~EXENDTC, ~EXSTDY, ~EXENDY,
         "A", "DRUG", "2025-01-01", "2025-01-03",      NA,       3
  ) |>
    lubrify_dates()

  result <- expand_ex(ex)

  expect_true(all(is.na(result$EXDY)))
})


# ---- per-episode independence ------------------------------------------------

test_that("expand_ex expands two episodes with the same date span independently", {
  ex <- tibble::tribble(
    ~USUBJID, ~EXTRT,           ~EXSTDTC,           ~EXENDTC, ~EXDOSE,
         "A", "DRUG", "2025-01-01T07:00", "2025-01-02T08:00",     100,
         "A", "DRUG", "2025-01-01T09:00", "2025-01-02T10:00",     200
  ) |>
    lubrify_dates()

  result <- expand_ex(ex)

  expect_equal(nrow(result), 4)
  expect_equal(result$EXDOSE, c(100, 100, 200, 200))
  expect_equal(
    result$DTC_date,
    c("2025-01-01", "2025-01-02", "2025-01-01", "2025-01-02")
  )
  expect_equal(result$DTC_time, c("07:00", "08:00", "09:00", "10:00"))
  expect_equal(
    result$IMPUTATION,
    c(
      "time copied from EXSTDTC",
      "time copied from EXENDTC",
      "time copied from EXSTDTC",
      "time copied from EXENDTC"
    )
  )
})


test_that("expand_ex does not carry offsets from a prior subject's episode", {
  ex <- tibble::tribble(
    ~USUBJID, ~EXTRT,           ~EXSTDTC,           ~EXENDTC,
         "A", "DRUG", "2025-01-01T07:00", "2025-01-03T08:00",
         "B", "DRUG", "2025-01-01T09:00", "2025-01-01T10:00"
  ) |>
    lubrify_dates()

  result <- expand_ex(ex)
  b <- result[result$USUBJID == "B", ]

  expect_equal(nrow(b), 1)
  expect_equal(b$DTC_date, "2025-01-01")
  expect_equal(b$DTC_time, "09:00")
  expect_equal(b$IMPUTATION, "time copied from EXSTDTC")
})


# ---- shape and passthrough ---------------------------------------------------

test_that("expand_ex does not leak helper columns and is ungrouped", {
  ex <- tibble::tribble(
    ~USUBJID, ~EXTRT,           ~EXSTDTC,           ~EXENDTC,
         "A", "DRUG", "2025-01-01T07:00", "2025-01-03T08:00"
  ) |>
    lubrify_dates()

  result <- expand_ex(ex)

  expect_false(dplyr::is_grouped_df(result))
  expect_false(
    any(
      c(".start_date", ".end_date", ".n_days", ".offset", ".day", ".expand_imp") %in%
        names(result)
    )
  )
})


test_that("expand_ex keeps decomposed date/time fields and original EX columns", {
  ex <- tibble::tribble(
    ~USUBJID, ~EXTRT,           ~EXSTDTC,           ~EXENDTC,
         "A", "DRUG", "2025-01-01T07:00", "2025-01-03T08:00"
  ) |>
    lubrify_dates()

  result <- expand_ex(ex)

  expect_true(
    all(
      c(
        "USUBJID", "EXTRT", "EXSTDTC", "EXENDTC",
        "EXSTDTC_date", "EXSTDTC_time", "EXENDTC_date", "EXENDTC_time",
        "DTC_date", "DTC_time", "IMPUTATION"
      ) %in% names(result)
    )
  )
  expect_equal(unique(result$EXSTDTC_time), "07:00")
  expect_equal(unique(result$EXENDTC_time), "08:00")
})


test_that("expand_ex copies extra columns onto every expanded day", {
  ex <- tibble::tribble(
    ~NOTE, ~USUBJID, ~EXTRT,           ~EXSTDTC,    ~EXENDTC, ~EXDOSE, ~EXROUTE,
   "keep",      "A", "DRUG", "2025-01-01T07:00", "2025-01-02",     100,   "ORAL"
  ) |>
    lubrify_dates()

  result <- expand_ex(ex)

  expect_equal(result$NOTE, c("keep", "keep"))
  expect_equal(result$EXDOSE, c(100, 100))
  expect_equal(result$EXROUTE, c("ORAL", "ORAL"))
})


test_that("expand_ex returns a tibble for data.frame input", {
  ex <- data.frame(
    USUBJID = "A",
    EXTRT   = "DRUG",
    EXSTDTC = "2025-01-01T07:00",
    EXENDTC = "2025-01-02T08:00",
    stringsAsFactors = FALSE
  )

  result <- expand_ex(ex)

  expect_s3_class(result, "tbl_df")
  expect_equal(nrow(result), 2)
})


test_that("expand_ex handles an empty EX domain", {
  ex <- suppressWarnings(
    tibble::tribble(~USUBJID, ~EXTRT, ~EXSTDTC, ~EXENDTC) |>
      lubrify_dates()
  )

  result <- suppressWarnings(expand_ex(ex))

  expect_equal(nrow(result), 0)
  expect_true(all(c("DTC_date", "DTC_time", "IMPUTATION") %in% names(result)))
})


test_that("expand_ex accepts already-POSIXct DTC columns", {
  ex <- tibble::tribble(
    ~USUBJID, ~EXTRT,           ~EXSTDTC,           ~EXENDTC,
         "A", "DRUG", "2025-01-01T07:00", "2025-01-02T08:00"
  ) |>
    lubrify_dates()

  result <- expand_ex(ex)

  expect_equal(result$DTC_date, c("2025-01-01", "2025-01-02"))
  expect_equal(result$DTC_time, c("07:00", "08:00"))
})
