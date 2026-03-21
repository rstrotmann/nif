test_that("check.nif rejects non-nif input", {
  expect_error(
    check.nif(mtcars),
    "Input must be a nif object"
  )
})

test_that("check.nif adds empty CHECK when missing and no deviation", {
  raw <- tibble::tribble(
    ~ID, ~TIME, ~AMT, ~CMT, ~EVID, ~DV, ~NTIME, ~TAD, ~ANALYTE,
    1L, 0, 100, 1, 1L, NA_real_, 0, 0, "DRUG",
    1L, 1, 0, 2, 0L, 10, 10, 10, "DRUG"
  )
  obj <- nif(raw)

  out <- check.nif(obj, silent = TRUE)

  expect_true("CHECK" %in% names(out))
  expect_equal(out$CHECK[out$EVID == 0], "")
  expect_false(any(out$.time_deviation_flag, na.rm = TRUE))
})

test_that("check.nif flags when TAD exceeds NTIME by more than relative threshold", {
  raw <- tibble::tribble(
    ~ID, ~TIME, ~AMT, ~CMT, ~EVID, ~DV, ~NTIME, ~TAD, ~ANALYTE,
    1L, 0, 100, 1, 1L, NA_real_, 0, 0, "DRUG",
    1L, 1, 0, 2, 0L, 10, 10, 13, "DRUG"
  )
  obj <- nif(raw)

  out <- check.nif(obj, ntime_threshold = 0.2, silent = TRUE)

  obs <- out[out$EVID == 0, ]
  expect_equal(obs$CHECK, "TAD inconsistent with NTIME")
})

test_that("check.nif does not flag at exact threshold (strict inequality)", {
  # TAD - NTIME = 2; NTIME * 0.2 = 2 -> must not flag (needs > 2)
  raw <- tibble::tribble(
    ~ID, ~TIME, ~AMT, ~CMT, ~EVID, ~DV, ~NTIME, ~TAD, ~ANALYTE,
    1L, 0, 100, 1, 1L, NA_real_, 0, 0, "DRUG",
    1L, 1, 0, 2, 0L, 10, 10, 12, "DRUG"
  )
  obj <- nif(raw)

  out <- check.nif(obj, ntime_threshold = 0.2, silent = TRUE)

  expect_equal(out$CHECK[out$EVID == 0], "")
})

test_that("check.nif respects custom ntime_threshold", {
  raw <- tibble::tribble(
    ~ID, ~TIME, ~AMT, ~CMT, ~EVID, ~DV, ~NTIME, ~TAD, ~ANALYTE,
    1L, 0, 100, 1, 1L, NA_real_, 0, 0, "DRUG",
    1L, 1, 0, 2, 0L, 10, 10, 12, "DRUG"
  )
  obj <- nif(raw)

  out_strict <- check.nif(obj, ntime_threshold = 0.2, silent = TRUE)
  expect_equal(out_strict$CHECK[out_strict$EVID == 0], "")

  out_loose <- check.nif(obj, ntime_threshold = 0.19, silent = TRUE)
  expect_equal(out_loose$CHECK[out_loose$EVID == 0], "TAD inconsistent with NTIME")
})

test_that("check.nif uses ref_time column for deviation", {
  raw <- tibble::tribble(
    ~ID, ~TIME, ~AMT, ~CMT, ~EVID, ~DV, ~NTIME, ~TAD, ~TAFD, ~ANALYTE,
    1L, 0, 100, 1, 1L, NA_real_, 0, 0, 0, "DRUG",
    1L, 1, 0, 2, 0L, 10, 10, 10, 13, "DRUG"
  )
  obj <- nif(raw)

  out <- check.nif(obj, ref_time = "TAFD", ntime_threshold = 0.2, silent = TRUE)

  expect_equal(out$CHECK[out$EVID == 0], "TAFD inconsistent with NTIME")
})

test_that("check.nif only flags analytes in the analyte filter", {
  raw <- tibble::tribble(
    ~ID, ~TIME, ~AMT, ~CMT, ~EVID, ~DV, ~NTIME, ~TAD, ~ANALYTE,
    1L, 0, 100, 1, 1L, NA_real_, 0, 0, "DRUG",
    1L, 1, 0, 2, 0L, 10, 10, 13, "DRUG",
    1L, 2, 0, 2, 0L, 20, 10, 13, "OTHER"
  )
  obj <- nif(raw)

  out <- check.nif(obj, analyte = "DRUG", ntime_threshold = 0.2, silent = TRUE)

  expect_equal(out$CHECK[out$ANALYTE == "DRUG" & out$EVID == 0],
    "TAD inconsistent with NTIME")
  expect_equal(out$CHECK[out$ANALYTE == "OTHER" & out$EVID == 0], "")
})

test_that("check.nif uses analytes() default (EVID == 0 distinct ANALYTE)", {
  raw <- tibble::tribble(
    ~ID, ~TIME, ~AMT, ~CMT, ~EVID, ~DV, ~NTIME, ~TAD, ~ANALYTE,
    1L, 0, 100, 1, 1L, NA_real_, 0, 0, "DRUG",
    1L, 1, 0, 2, 0L, 10, 10, 13, "DRUG",
    1L, 2, 0, 2, 0L, 20, 10, 13, "MET"
  )
  obj <- nif(raw)

  out <- check.nif(obj, analyte = NULL, ntime_threshold = 0.2, silent = TRUE)

  expect_setequal(analytes(obj), c("DRUG", "MET"))
  expect_true(all(out$CHECK[out$EVID == 0 & out$ANALYTE %in% c("DRUG", "MET")] ==
    "TAD inconsistent with NTIME"))
})

test_that("check.nif leaves rows without observation analytes unflagged when analytes() is empty", {
  raw <- tibble::tribble(
    ~ID, ~TIME, ~AMT, ~CMT, ~EVID, ~DV, ~NTIME, ~TAD, ~ANALYTE,
    1L, 0, 100, 1, 1L, NA_real_, 0, 5, "DRUG"
  )
  obj <- nif(raw)

  out <- check.nif(obj, ntime_threshold = 0.2, silent = TRUE)

  expect_equal(length(analytes(obj)), 0L)
  expect_equal(out$CHECK, "")
  expect_false(any(out$.time_deviation_flag))
})

test_that("check.nif preserves existing CHECK when no new flag", {
  raw <- tibble::tribble(
    ~ID, ~TIME, ~AMT, ~CMT, ~EVID, ~DV, ~NTIME, ~TAD, ~ANALYTE, ~CHECK,
    1L, 0, 100, 1, 1L, NA_real_, 0, 0, "DRUG", "",
    1L, 1, 0, 2, 0L, 10, 10, 10, "DRUG", "prior note"
  )
  obj <- nif(raw)

  out <- check.nif(obj, silent = TRUE)

  expect_equal(out$CHECK[out$EVID == 0], "prior note")
})

test_that("check.nif overwrites CHECK when time deviation flag is true", {
  raw <- tibble::tribble(
    ~ID, ~TIME, ~AMT, ~CMT, ~EVID, ~DV, ~NTIME, ~TAD, ~ANALYTE, ~CHECK,
    1L, 0, 100, 1, 1L, NA_real_, 0, 0, "DRUG", "",
    1L, 1, 0, 2, 0L, 10, 10, 13, "DRUG", "prior note"
  )
  obj <- nif(raw)

  out <- check.nif(obj, ntime_threshold = 0.2, silent = TRUE)

  expect_equal(out$CHECK[out$EVID == 0], "TAD inconsistent with NTIME")
})

test_that("check.nif errors when minimal empty nif lacks ref_time, NTIME, and ANALYTE", {
  obj <- nif()

  expect_error(check.nif(obj, silent = TRUE), "not found in nif object")
})

test_that("check.nif errors with clear message when a required column is missing", {
  raw <- tibble::tribble(
    ~ID, ~TIME, ~AMT, ~CMT, ~EVID, ~DV, ~TAD, ~ANALYTE,
    1L, 0, 100, 1, 1L, NA_real_, 0, "DRUG"
  )
  obj <- nif(raw)

  expect_error(check.nif(obj, silent = TRUE), "NTIME")
})

test_that("check.nif works on zero-row nif with required time columns", {
  raw <- tibble::tribble(
    ~ID, ~TIME, ~AMT, ~CMT, ~EVID, ~DV, ~NTIME, ~TAD, ~ANALYTE,
    1L, 0, 100, 1, 1L, NA_real_, 0, 0, "DRUG"
  )
  obj <- nif(raw)[integer(0), ]

  expect_no_error(out <- check.nif(obj, silent = TRUE))

  expect_equal(nrow(out), 0L)
  expect_true("CHECK" %in% names(out))
})

test_that("check() dispatches to check.nif", {
  raw <- tibble::tribble(
    ~ID, ~TIME, ~AMT, ~CMT, ~EVID, ~DV, ~NTIME, ~TAD, ~ANALYTE,
    1L, 0, 100, 1, 1L, NA_real_, 0, 0, "DRUG",
    1L, 1, 0, 2, 0L, 10, 10, 10, "DRUG"
  )
  obj <- nif(raw)

  out <- check(obj, silent = TRUE)

  expect_s3_class(out, "nif")
  expect_true("CHECK" %in% names(out))
})

test_that("check.nif silent = TRUE suppresses cli output", {
  raw <- tibble::tribble(
    ~ID, ~TIME, ~AMT, ~CMT, ~EVID, ~DV, ~NTIME, ~TAD, ~ANALYTE,
    1L, 0, 100, 1, 1L, NA_real_, 0, 0, "DRUG",
    1L, 1, 0, 2, 0L, 10, 10, 13, "DRUG"
  )
  obj <- nif(raw)

  expect_silent(check.nif(obj, ntime_threshold = 0.2, silent = TRUE))
})

test_that("check.nif accepts multiple analytes in analyte argument", {
  raw <- tibble::tribble(
    ~ID, ~TIME, ~AMT, ~CMT, ~EVID, ~DV, ~NTIME, ~TAD, ~ANALYTE,
    1L, 0, 100, 1, 1L, NA_real_, 0, 0, "DRUG",
    1L, 1, 0, 2, 0L, 10, 10, 13, "DRUG",
    1L, 2, 0, 2, 0L, 20, 10, 13, "MET"
  )
  obj <- nif(raw)

  out <- check.nif(obj, analyte = c("DRUG", "MET"), ntime_threshold = 0.2, silent = TRUE)

  expect_true(all(out$CHECK[out$EVID == 0] == "TAD inconsistent with NTIME"))
})

test_that("check.nif flags positive TAD when NTIME is zero (threshold uses strict > 0)", {
  raw <- tibble::tribble(
    ~ID, ~TIME, ~AMT, ~CMT, ~EVID, ~DV, ~NTIME, ~TAD, ~ANALYTE,
    1L, 0, 100, 1, 1L, NA_real_, 0, 0, "DRUG",
    1L, 1, 0, 2, 0L, 5, 0, 1, "DRUG"
  )
  obj <- nif(raw)

  out <- check.nif(obj, ntime_threshold = 0.2, silent = TRUE)

  expect_equal(out$CHECK[out$EVID == 0 & out$NTIME == 0], "TAD inconsistent with NTIME")
})

test_that("check.nif validates ntime_threshold", {
  raw <- tibble::tribble(
    ~ID, ~TIME, ~AMT, ~CMT, ~EVID, ~DV, ~NTIME, ~TAD, ~ANALYTE,
    1L, 0, 100, 1, 1L, NA_real_, 0, 0, "DRUG",
    1L, 1, 0, 2, 0L, 10, 10, 10, "DRUG"
  )
  obj <- nif(raw)

  expect_error(check.nif(obj, ntime_threshold = -0.1, silent = TRUE), "negative")
  expect_error(check.nif(obj, ntime_threshold = Inf, silent = TRUE), "finite")
  expect_error(check.nif(obj, ntime_threshold = "0.2", silent = TRUE), "numeric")
  expect_no_error(check.nif(obj, ntime_threshold = 0, silent = TRUE))
})

test_that("check.nif validates silent", {
  raw <- tibble::tribble(
    ~ID, ~TIME, ~AMT, ~CMT, ~EVID, ~DV, ~NTIME, ~TAD, ~ANALYTE,
    1L, 0, 100, 1, 1L, NA_real_, 0, 0, "DRUG",
    1L, 1, 0, 2, 0L, 10, 10, 10, "DRUG"
  )
  obj <- nif(raw)

  expect_error(check.nif(obj, silent = "yes"), "logical")
})

test_that("check.nif validates analyte when not NULL", {
  raw <- tibble::tribble(
    ~ID, ~TIME, ~AMT, ~CMT, ~EVID, ~DV, ~NTIME, ~TAD, ~ANALYTE,
    1L, 0, 100, 1, 1L, NA_real_, 0, 0, "DRUG",
    1L, 1, 0, 2, 0L, 10, 10, 10, "DRUG"
  )
  obj <- nif(raw)

  expect_error(check.nif(obj, analyte = NA_character_, silent = TRUE), "NA")
  expect_error(check.nif(obj, analyte = 1L, silent = TRUE), "character")
})


