# ---- Input validation --------------------------------------------------------

test_that("check.nif rejects non-nif input", {
  expect_error(
    check.nif(mtcars),
    "Input must be a nif object"
  )
})


test_that("check.nif errors when empty nif lacks NTIME, ANALYTE, and TAD", {
  expect_error(
    check.nif(nif(), silent = TRUE),
    "Missing required fields: NTIME, ANALYTE and TAD"
  )
})


test_that("check.nif errors when NTIME is missing", {
  obj <- as_nif_test(tibble::tribble(
    ~ID, ~TIME, ~AMT, ~CMT, ~EVID,        ~DV, ~TAD, ~ANALYTE,
     1L,     0,  100,    1,    1L,   NA_real_,    0,   "DRUG"
  ))

  expect_error(check.nif(obj, silent = TRUE), "NTIME")
})


test_that("check.nif errors when ANALYTE is missing", {
  obj <- as_nif_test(tibble::tribble(
    ~ID, ~TIME, ~AMT, ~CMT, ~EVID,        ~DV, ~NTIME, ~TAD,
     1L,     0,  100,    1,    1L,   NA_real_,      0,    0
  ))

  expect_error(check.nif(obj, silent = TRUE), "ANALYTE")
})


test_that("check.nif errors when default ref_time TAD is missing", {
  obj <- as_nif_test(tibble::tribble(
    ~ID, ~TIME, ~AMT, ~CMT, ~EVID,        ~DV, ~NTIME, ~ANALYTE,
     1L,     0,  100,    1,    1L,   NA_real_,      0,   "DRUG"
  ))

  expect_error(check.nif(obj, silent = TRUE), "TAD")
})


test_that("check.nif errors when custom ref_time column is missing", {
  obj <- as_nif_test(tibble::tribble(
    ~ID, ~TIME, ~AMT, ~CMT, ~EVID,        ~DV, ~NTIME, ~TAD, ~ANALYTE,
     1L,     0,  100,    1,    1L,   NA_real_,      0,    0,   "DRUG"
  ))

  expect_error(
    check.nif(obj, ref_time = "TAFD", silent = TRUE),
    "TAFD"
  )
})


test_that("check.nif validates ntime_threshold", {
  obj <- as_nif_test(tibble::tribble(
    ~ID, ~TIME, ~AMT, ~CMT, ~EVID,        ~DV, ~NTIME, ~TAD, ~ANALYTE,
     1L,     0,  100,    1,    1L,   NA_real_,      0,    0,   "DRUG",
     1L,     1,    0,    2,    0L,         10,     10,   10,   "DRUG"
  ))

  expect_error(
    check.nif(obj, ntime_threshold = "0.2", silent = TRUE),
    "numeric"
  )
  expect_error(
    check.nif(obj, ntime_threshold = NA_real_, silent = TRUE),
    "NA"
  )
  expect_error(
    check.nif(obj, ntime_threshold = Inf, silent = TRUE),
    "finite"
  )
  expect_error(
    check.nif(obj, ntime_threshold = -Inf, silent = TRUE),
    "finite"
  )
  expect_error(
    check.nif(obj, ntime_threshold = NaN, silent = TRUE),
    "NA"
  )
  expect_error(
    check.nif(obj, ntime_threshold = -0.1, silent = TRUE),
    "negative"
  )
  expect_error(
    check.nif(obj, ntime_threshold = c(0.1, 0.2), silent = TRUE),
    "single value"
  )
  expect_no_error(check.nif(obj, ntime_threshold = 0, silent = TRUE))
})


test_that("check.nif validates silent", {
  obj <- as_nif_test(tibble::tribble(
    ~ID, ~TIME, ~AMT, ~CMT, ~EVID,        ~DV, ~NTIME, ~TAD, ~ANALYTE,
     1L,     0,  100,    1,    1L,   NA_real_,      0,    0,   "DRUG",
     1L,     1,    0,    2,    0L,         10,     10,   10,   "DRUG"
  ))

  expect_error(check.nif(obj, silent = "yes"), "logical")
  expect_error(check.nif(obj, silent = NA), "NA")
  expect_error(check.nif(obj, silent = c(TRUE, FALSE)), "single value")
})


test_that("check.nif validates ref_time", {
  obj <- as_nif_test(tibble::tribble(
    ~ID, ~TIME, ~AMT, ~CMT, ~EVID,        ~DV, ~NTIME, ~TAD, ~TAFD, ~ANALYTE,
     1L,     0,  100,    1,    1L,   NA_real_,      0,    0,     0,   "DRUG",
     1L,     1,    0,    2,    0L,         10,     10,   10,    10,   "DRUG"
  ))

  expect_error(
    check.nif(obj, ref_time = 1, silent = TRUE),
    "Missing required fields: 1"
  )
  expect_error(
    check.nif(obj, ref_time = NULL, silent = TRUE),
    "NULL"
  )
  expect_error(
    check.nif(obj, ref_time = "", silent = TRUE),
    "non-empty"
  )
  expect_error(
    check.nif(obj, ref_time = c("TAD", "TAFD"), silent = TRUE),
    "single value"
  )
})


test_that("check.nif validates analyte when not NULL", {
  obj <- as_nif_test(tibble::tribble(
    ~ID, ~TIME, ~AMT, ~CMT, ~EVID,        ~DV, ~NTIME, ~TAD, ~ANALYTE,
     1L,     0,  100,    1,    1L,   NA_real_,      0,    0,   "DRUG",
     1L,     1,    0,    2,    0L,         10,     10,   10,   "DRUG"
  ))

  expect_error(
    check.nif(obj, analyte = 1L, silent = TRUE),
    "character"
  )
  expect_error(
    check.nif(obj, analyte = NA_character_, silent = TRUE),
    "NA"
  )
  expect_error(
    check.nif(obj, analyte = "", silent = TRUE),
    "non-empty"
  )
})


test_that("check.nif errors when requested analyte is not in the nif", {
  obj <- as_nif_test(tibble::tribble(
    ~ID, ~TIME, ~AMT, ~CMT, ~EVID,        ~DV, ~NTIME, ~TAD, ~ANALYTE,
     1L,     0,  100,    1,    1L,   NA_real_,      0,    0,   "DRUG",
     1L,     1,    0,    2,    0L,         10,     10,   10,   "DRUG"
  ))

  expect_error(
    check.nif(obj, analyte = "NOPE", silent = TRUE),
    "Missing analyte: NOPE"
  )
  expect_error(
    check.nif(obj, analyte = c("NOPE", "ALSO"), silent = TRUE),
    "Missing analytes: NOPE and ALSO"
  )
})


# ---- CHECK column ------------------------------------------------------------

test_that("check.nif adds empty CHECK when missing and no deviation", {
  obj <- as_nif_test(tibble::tribble(
    ~ID, ~TIME, ~AMT, ~CMT, ~EVID,        ~DV, ~NTIME, ~TAD, ~ANALYTE,
     1L,     0,  100,    1,    1L,   NA_real_,      0,    0,   "DRUG",
     1L,     1,    0,    2,    0L,         10,     10,   10,   "DRUG"
  ))

  out <- check.nif(obj, silent = TRUE)

  expect_true("CHECK" %in% names(out))
  expect_type(out$CHECK, "character")
  expect_equal(out$CHECK, c("", ""))
})


test_that("check.nif preserves existing CHECK when no new flag", {
  obj <- as_nif_test(tibble::tribble(
    ~ID, ~TIME, ~AMT, ~CMT, ~EVID,        ~DV, ~NTIME, ~TAD, ~ANALYTE,      ~CHECK,
     1L,     0,  100,    1,    1L,   NA_real_,      0,    0,   "DRUG",          "",
     1L,     1,    0,    2,    0L,         10,     10,   10,   "DRUG", "prior note"
  ))

  out <- check.nif(obj, silent = TRUE)

  expect_equal(out$CHECK, c("", "prior note"))
})


test_that("check.nif preserves NA CHECK when row is not flagged", {
  obj <- as_nif_test(tibble::tribble(
    ~ID, ~TIME, ~AMT, ~CMT, ~EVID,        ~DV, ~NTIME, ~TAD, ~ANALYTE,          ~CHECK,
     1L,     0,  100,    1,    1L,   NA_real_,      0,    0,   "DRUG",              "",
     1L,     1,    0,    2,    0L,         10,     10,   10,   "DRUG", NA_character_
  ))

  out <- check.nif(obj, silent = TRUE)

  expect_equal(out$CHECK, c("", NA_character_))
})


test_that("check.nif overwrites CHECK when time deviation flag is true", {
  obj <- as_nif_test(tibble::tribble(
    ~ID, ~TIME, ~AMT, ~CMT, ~EVID,        ~DV, ~NTIME, ~TAD, ~ANALYTE,      ~CHECK,
     1L,     0,  100,    1,    1L,   NA_real_,      0,    0,   "DRUG",          "",
     1L,     1,    0,    2,    0L,         10,     10,   13,   "DRUG", "prior note"
  ))

  out <- check.nif(obj, ntime_threshold = 0.2, silent = TRUE)

  expect_equal(out$CHECK, c("", "TAD inconsistent with NTIME"))
})


test_that("check.nif does not leave the internal flag column in the result", {
  obj <- as_nif_test(tibble::tribble(
    ~ID, ~TIME, ~AMT, ~CMT, ~EVID,        ~DV, ~NTIME, ~TAD, ~ANALYTE,
     1L,     0,  100,    1,    1L,   NA_real_,      0,    0,   "DRUG",
     1L,     1,    0,    2,    0L,         10,     10,   13,   "DRUG"
  ))

  out <- check.nif(obj, silent = TRUE)

  expect_false(".time_deviation_flag" %in% names(out))
})


test_that("check.nif returns a nif object with the same rows and extra columns", {
  obj <- as_nif_test(tibble::tribble(
    ~ID, ~TIME, ~AMT, ~CMT, ~EVID,        ~DV, ~NTIME, ~TAD, ~ANALYTE, ~USUBJID,
     1L,     0,  100,    1,    1L,   NA_real_,      0,    0,   "DRUG",    "S-1",
     1L,     1,    0,    2,    0L,         10,     10,   13,   "DRUG",    "S-1"
  ))

  out <- check.nif(obj, silent = TRUE)

  expect_s3_class(out, "nif")
  expect_equal(nrow(out), nrow(obj))
  expect_equal(out$USUBJID, obj$USUBJID)
  expect_equal(out$ID, obj$ID)
  expect_equal(out$EVID, obj$EVID)
})


# ---- Flagging rule -----------------------------------------------------------

test_that("check.nif flags when TAD exceeds NTIME by more than the relative threshold", {
  obj <- as_nif_test(tibble::tribble(
    ~ID, ~TIME, ~AMT, ~CMT, ~EVID,        ~DV, ~NTIME, ~TAD, ~ANALYTE,
     1L,     0,  100,    1,    1L,   NA_real_,      0,    0,   "DRUG",
     1L,     1,    0,    2,    0L,         10,     10,   13,   "DRUG"
  ))

  out <- check.nif(obj, ntime_threshold = 0.2, silent = TRUE)

  expect_equal(out$CHECK, c("", "TAD inconsistent with NTIME"))
})


test_that("check.nif does not flag at the exact threshold (strict inequality)", {
  # TAD - NTIME = 2; NTIME * 0.2 = 2 -> must not flag
  obj <- as_nif_test(tibble::tribble(
    ~ID, ~TIME, ~AMT, ~CMT, ~EVID,        ~DV, ~NTIME, ~TAD, ~ANALYTE,
     1L,     0,  100,    1,    1L,   NA_real_,      0,    0,   "DRUG",
     1L,     1,    0,    2,    0L,         10,     10,   12,   "DRUG"
  ))

  out <- check.nif(obj, ntime_threshold = 0.2, silent = TRUE)

  expect_equal(out$CHECK, c("", ""))
})


test_that("check.nif uses a default ntime_threshold of 0.2", {
  at_threshold <- as_nif_test(tibble::tribble(
    ~ID, ~TIME, ~AMT, ~CMT, ~EVID,        ~DV, ~NTIME, ~TAD, ~ANALYTE,
     1L,     0,  100,    1,    1L,   NA_real_,      0,    0,   "DRUG",
     1L,     1,    0,    2,    0L,         10,     10,   12,   "DRUG"
  ))
  above_threshold <- as_nif_test(tibble::tribble(
    ~ID, ~TIME, ~AMT, ~CMT, ~EVID,        ~DV, ~NTIME, ~TAD, ~ANALYTE,
     1L,     0,  100,    1,    1L,   NA_real_,      0,    0,   "DRUG",
     1L,     1,    0,    2,    0L,         10,     10, 12.1,   "DRUG"
  ))

  expect_equal(check.nif(at_threshold, silent = TRUE)$CHECK[2], "")
  expect_equal(
    check.nif(above_threshold, silent = TRUE)$CHECK[2],
    "TAD inconsistent with NTIME"
  )
})


test_that("check.nif respects a custom ntime_threshold", {
  obj <- as_nif_test(tibble::tribble(
    ~ID, ~TIME, ~AMT, ~CMT, ~EVID,        ~DV, ~NTIME, ~TAD, ~ANALYTE,
     1L,     0,  100,    1,    1L,   NA_real_,      0,    0,   "DRUG",
     1L,     1,    0,    2,    0L,         10,     10,   12,   "DRUG"
  ))

  out_strict <- check.nif(obj, ntime_threshold = 0.2, silent = TRUE)
  expect_equal(out_strict$CHECK[2], "")

  out_loose <- check.nif(obj, ntime_threshold = 0.19, silent = TRUE)
  expect_equal(out_loose$CHECK[2], "TAD inconsistent with NTIME")
})


test_that("check.nif with ntime_threshold 0 flags any positive TAD - NTIME", {
  obj <- as_nif_test(tibble::tribble(
    ~ID, ~TIME, ~AMT, ~CMT, ~EVID,        ~DV, ~NTIME,  ~TAD, ~ANALYTE,
     1L,     0,  100,    1,    1L,   NA_real_,      0,     0,   "DRUG",
     1L,     1,    0,    2,    0L,         10,     10,    10,   "DRUG",
     1L,     2,    0,    2,    0L,         20,     10, 10.01,   "DRUG"
  ))

  out <- check.nif(obj, ntime_threshold = 0, silent = TRUE)

  expect_equal(out$CHECK, c("", "", "TAD inconsistent with NTIME"))
})


test_that("check.nif does not flag when TAD is behind NTIME", {
  obj <- as_nif_test(tibble::tribble(
    ~ID, ~TIME, ~AMT, ~CMT, ~EVID,        ~DV, ~NTIME, ~TAD, ~ANALYTE,
     1L,     0,  100,    1,    1L,   NA_real_,      0,    0,   "DRUG",
     1L,     1,    0,    2,    0L,         10,     10,    1,   "DRUG"
  ))

  out <- check.nif(obj, ntime_threshold = 0.2, silent = TRUE)

  expect_equal(out$CHECK, c("", ""))
})


test_that("check.nif flags positive TAD when NTIME is zero", {
  obj <- as_nif_test(tibble::tribble(
    ~ID, ~TIME, ~AMT, ~CMT, ~EVID,        ~DV, ~NTIME, ~TAD, ~ANALYTE,
     1L,     0,  100,    1,    1L,   NA_real_,      0,    0,   "DRUG",
     1L,     1,    0,    2,    0L,          5,      0,    1,   "DRUG"
  ))

  out <- check.nif(obj, ntime_threshold = 0.2, silent = TRUE)

  expect_equal(out$CHECK, c("", "TAD inconsistent with NTIME"))
})


test_that("check.nif does not flag zero TAD when NTIME is zero", {
  obj <- as_nif_test(tibble::tribble(
    ~ID, ~TIME, ~AMT, ~CMT, ~EVID,        ~DV, ~NTIME, ~TAD, ~ANALYTE,
     1L,     0,  100,    1,    1L,   NA_real_,      0,    0,   "DRUG",
     1L,     1,    0,    2,    0L,          5,      0,    0,   "DRUG"
  ))

  out <- check.nif(obj, ntime_threshold = 0.2, silent = TRUE)

  expect_equal(out$CHECK, c("", ""))
})


test_that("check.nif does not flag rows with missing TAD or NTIME", {
  obj <- as_nif_test(tibble::tribble(
    ~ID, ~TIME, ~AMT, ~CMT, ~EVID,        ~DV,    ~NTIME,      ~TAD, ~ANALYTE,
     1L,     0,  100,    1,    1L,   NA_real_,         0,         0,   "DRUG",
     1L,     1,    0,    2,    0L,         10,        10, NA_real_,   "DRUG",
     1L,     2,    0,    2,    0L,         20, NA_real_,        13,   "DRUG"
  ))

  out <- check.nif(obj, ntime_threshold = 0.2, silent = TRUE)

  expect_equal(out$CHECK, c("", "", ""))
})


test_that("check.nif flags infinite TAD as inconsistent", {
  obj <- as_nif_test(tibble::tribble(
    ~ID, ~TIME, ~AMT, ~CMT, ~EVID,        ~DV, ~NTIME,  ~TAD, ~ANALYTE,
     1L,     0,  100,    1,    1L,   NA_real_,      0,     0,   "DRUG",
     1L,     1,    0,    2,    0L,         10,     10,   Inf,   "DRUG"
  ))

  out <- check.nif(obj, ntime_threshold = 0.2, silent = TRUE)

  expect_equal(out$CHECK[2], "TAD inconsistent with NTIME")
})


test_that("check.nif flags matching dose rows, not only observations", {
  obj <- as_nif_test(tibble::tribble(
    ~ID, ~TIME, ~AMT, ~CMT, ~EVID,        ~DV, ~NTIME, ~TAD, ~ANALYTE,
     1L,     0,  100,    1,    1L,   NA_real_,     10,   13,   "DRUG",
     1L,     1,    0,    2,    0L,         10,     10,   10,   "DRUG"
  ))

  out <- check.nif(obj, ntime_threshold = 0.2, silent = TRUE)

  expect_equal(out$CHECK, c("TAD inconsistent with NTIME", ""))
})


test_that("check.nif flags only the rows that exceed the threshold", {
  obj <- as_nif_test(tibble::tribble(
    ~ID, ~TIME, ~AMT, ~CMT, ~EVID,        ~DV, ~NTIME, ~TAD, ~ANALYTE,
     1L,     0,  100,    1,    1L,   NA_real_,      0,    0,   "DRUG",
     1L,     1,    0,    2,    0L,         10,     10,   10,   "DRUG",
     1L,     2,    0,    2,    0L,         20,     10,   13,   "DRUG",
     2L,     3,    0,    2,    0L,         30,     10,   11,   "DRUG"
  ))

  out <- check.nif(obj, ntime_threshold = 0.2, silent = TRUE)

  expect_equal(
    out$CHECK,
    c("", "", "TAD inconsistent with NTIME", "")
  )
})


# ---- Analyte filter ----------------------------------------------------------

test_that("check.nif only flags analytes in the analyte filter", {
  obj <- as_nif_test(tibble::tribble(
    ~ID, ~TIME, ~AMT, ~CMT, ~EVID,        ~DV, ~NTIME, ~TAD, ~ANALYTE,
     1L,     0,  100,    1,    1L,   NA_real_,      0,    0,   "DRUG",
     1L,     1,    0,    2,    0L,         10,     10,   13,   "DRUG",
     1L,     2,    0,    2,    0L,         20,     10,   13,  "OTHER"
  ))

  out <- check.nif(obj, analyte = "DRUG", ntime_threshold = 0.2, silent = TRUE)

  expect_equal(out$CHECK[out$ANALYTE == "DRUG"], c("", "TAD inconsistent with NTIME"))
  expect_equal(out$CHECK[out$ANALYTE == "OTHER"], "")
})


test_that("check.nif accepts multiple analytes in analyte argument", {
  obj <- as_nif_test(tibble::tribble(
    ~ID, ~TIME, ~AMT, ~CMT, ~EVID,        ~DV, ~NTIME, ~TAD, ~ANALYTE,
     1L,     0,  100,    1,    1L,   NA_real_,      0,    0,   "DRUG",
     1L,     1,    0,    2,    0L,         10,     10,   13,   "DRUG",
     1L,     2,    0,    2,    0L,         20,     10,   13,    "MET"
  ))

  out <- check.nif(
    obj,
    analyte = c("DRUG", "MET"),
    ntime_threshold = 0.2,
    silent = TRUE
  )

  expect_equal(
    out$CHECK[out$EVID == 0],
    c("TAD inconsistent with NTIME", "TAD inconsistent with NTIME")
  )
})


test_that("check.nif defaults analyte to analytes() (EVID == 0 distinct ANALYTE)", {
  obj <- as_nif_test(tibble::tribble(
    ~ID, ~TIME, ~AMT, ~CMT, ~EVID,        ~DV, ~NTIME, ~TAD, ~ANALYTE,
     1L,     0,  100,    1,    1L,   NA_real_,      0,    0,   "DRUG",
     1L,     1,    0,    2,    0L,         10,     10,   13,   "DRUG",
     1L,     2,    0,    2,    0L,         20,     10,   13,    "MET"
  ))

  out <- check.nif(obj, analyte = NULL, ntime_threshold = 0.2, silent = TRUE)

  expect_setequal(analytes(obj), c("DRUG", "MET"))
  expect_true(all(
    out$CHECK[out$EVID == 0 & out$ANALYTE %in% c("DRUG", "MET")] ==
      "TAD inconsistent with NTIME"
  ))
})


test_that("check.nif leaves rows unflagged when analytes() is empty", {
  obj <- as_nif_test(tibble::tribble(
    ~ID, ~TIME, ~AMT, ~CMT, ~EVID,        ~DV, ~NTIME, ~TAD, ~ANALYTE,
     1L,     0,  100,    1,    1L,   NA_real_,      0,    5,   "DRUG"
  ))

  out <- check.nif(obj, ntime_threshold = 0.2, silent = TRUE)

  expect_equal(length(analytes(obj)), 0L)
  expect_equal(out$CHECK, "")
})


test_that("check.nif flags nothing when analyte is an empty character vector", {
  obj <- as_nif_test(tibble::tribble(
    ~ID, ~TIME, ~AMT, ~CMT, ~EVID,        ~DV, ~NTIME, ~TAD, ~ANALYTE,
     1L,     0,  100,    1,    1L,   NA_real_,      0,    0,   "DRUG",
     1L,     1,    0,    2,    0L,         10,     10,   13,   "DRUG"
  ))

  out <- check.nif(obj, analyte = character(0), silent = TRUE)

  expect_equal(out$CHECK, c("", ""))
})


test_that("check.nif does not flag rows whose ANALYTE is NA", {
  obj <- as_nif_test(tibble::tribble(
    ~ID, ~TIME, ~AMT, ~CMT, ~EVID,        ~DV, ~NTIME, ~TAD,     ~ANALYTE,
     1L,     0,  100,    1,    1L,   NA_real_,      0,    0,       "DRUG",
     1L,     1,    0,    2,    0L,         10,     10,   13,       "DRUG",
     1L,     2,    0,    2,    0L,         20,     10,   13, NA_character_
  ))

  out <- check.nif(obj, analyte = "DRUG", ntime_threshold = 0.2, silent = TRUE)

  expect_equal(out$CHECK, c("", "TAD inconsistent with NTIME", ""))
})


# ---- Reference time ----------------------------------------------------------

test_that("check.nif uses ref_time column for the deviation and CHECK text", {
  obj <- as_nif_test(tibble::tribble(
    ~ID, ~TIME, ~AMT, ~CMT, ~EVID,        ~DV, ~NTIME, ~TAD, ~TAFD, ~ANALYTE,
     1L,     0,  100,    1,    1L,   NA_real_,      0,    0,     0,   "DRUG",
     1L,     1,    0,    2,    0L,         10,     10,   10,    13,   "DRUG"
  ))

  out <- check.nif(obj, ref_time = "TAFD", ntime_threshold = 0.2, silent = TRUE)

  expect_equal(out$CHECK, c("", "TAFD inconsistent with NTIME"))
})


test_that("check.nif can use TIME as ref_time", {
  obj <- as_nif_test(tibble::tribble(
    ~ID, ~TIME, ~AMT, ~CMT, ~EVID,        ~DV, ~NTIME, ~TAD, ~ANALYTE,
     1L,     0,  100,    1,    1L,   NA_real_,      0,    0,   "DRUG",
     1L,    13,    0,    2,    0L,         10,     10,   10,   "DRUG"
  ))

  out <- check.nif(obj, ref_time = "TIME", ntime_threshold = 0.2, silent = TRUE)

  expect_equal(out$CHECK[2], "TIME inconsistent with NTIME")
})


test_that("check.nif does not flag TAD deviation when a different ref_time is used", {
  obj <- as_nif_test(tibble::tribble(
    ~ID, ~TIME, ~AMT, ~CMT, ~EVID,        ~DV, ~NTIME, ~TAD, ~TAFD, ~ANALYTE,
     1L,     0,  100,    1,    1L,   NA_real_,      0,    0,     0,   "DRUG",
     1L,     1,    0,    2,    0L,         10,     10,   13,    10,   "DRUG"
  ))

  out <- check.nif(obj, ref_time = "TAFD", ntime_threshold = 0.2, silent = TRUE)

  expect_equal(out$CHECK, c("", ""))
})


# ---- Empty and dispatch ------------------------------------------------------

test_that("check.nif works on a zero-row nif with required time columns", {
  obj <- as_nif_test(tibble::tribble(
    ~ID, ~TIME, ~AMT, ~CMT, ~EVID,        ~DV, ~NTIME, ~TAD, ~ANALYTE,
     1L,     0,  100,    1,    1L,   NA_real_,      0,    0,   "DRUG"
  ))[integer(0), ]

  expect_no_error(out <- check.nif(obj, silent = TRUE))

  expect_equal(nrow(out), 0L)
  expect_true("CHECK" %in% names(out))
  expect_s3_class(out, "nif")
})


test_that("check() dispatches to check.nif", {
  obj <- as_nif_test(tibble::tribble(
    ~ID, ~TIME, ~AMT, ~CMT, ~EVID,        ~DV, ~NTIME, ~TAD, ~ANALYTE,
     1L,     0,  100,    1,    1L,   NA_real_,      0,    0,   "DRUG",
     1L,     1,    0,    2,    0L,         10,     10,   10,   "DRUG"
  ))

  out <- check(obj, silent = TRUE)

  expect_s3_class(out, "nif")
  expect_true("CHECK" %in% names(out))
  expect_equal(out$CHECK, c("", ""))
})


# ---- Messaging ---------------------------------------------------------------

test_that("check.nif silent = TRUE suppresses cli output", {
  obj <- as_nif_test(tibble::tribble(
    ~ID, ~TIME, ~AMT, ~CMT, ~EVID,        ~DV, ~NTIME, ~TAD, ~ANALYTE,
     1L,     0,  100,    1,    1L,   NA_real_,      0,    0,   "DRUG",
     1L,     1,    0,    2,    0L,         10,     10,   13,   "DRUG"
  ))

  expect_silent(check.nif(obj, ntime_threshold = 0.2, silent = TRUE))
})


test_that("check.nif silent = FALSE reports flagged rows", {
  obj <- as_nif_test(tibble::tribble(
    ~ID, ~TIME, ~AMT, ~CMT, ~EVID,        ~DV, ~NTIME, ~TAD, ~ANALYTE,
     1L,     0,  100,    1,    1L,   NA_real_,      0,    0,   "DRUG",
     1L,     1,    0,    2,    0L,         10,     10,   13,   "DRUG"
  ))

  expect_message(
    check.nif(obj, ntime_threshold = 0.2, silent = FALSE),
    "analyte DRUG: 1 row with TAD deviating from NTIME by >20%"
  )
})


test_that("check.nif message uses plural rows and percent from ntime_threshold", {
  obj <- as_nif_test(tibble::tribble(
    ~ID, ~TIME, ~AMT, ~CMT, ~EVID,        ~DV, ~NTIME, ~TAD, ~ANALYTE,
     1L,     0,  100,    1,    1L,   NA_real_,      0,    0,   "DRUG",
     1L,     1,    0,    2,    0L,         10,     10,   13,   "DRUG",
     1L,     2,    0,    2,    0L,         20,     10,   13,   "DRUG"
  ))

  expect_message(
    check.nif(obj, ntime_threshold = 0.1, silent = FALSE),
    "2 rows with TAD deviating from NTIME by >10%"
  )
})


test_that("check.nif message lists multiple analytes with or", {
  obj <- as_nif_test(tibble::tribble(
    ~ID, ~TIME, ~AMT, ~CMT, ~EVID,        ~DV, ~NTIME, ~TAD, ~ANALYTE,
     1L,     0,  100,    1,    1L,   NA_real_,      0,    0,   "DRUG",
     1L,     1,    0,    2,    0L,         10,     10,   13,   "DRUG",
     1L,     2,    0,    2,    0L,         20,     10,   13,    "MET"
  ))

  expect_message(
    check.nif(
      obj,
      analyte = c("DRUG", "MET"),
      ntime_threshold = 0.2,
      silent = FALSE
    ),
    "analytes DRUG or MET"
  )
})


test_that("check.nif is silent when no rows are flagged", {
  obj <- as_nif_test(tibble::tribble(
    ~ID, ~TIME, ~AMT, ~CMT, ~EVID,        ~DV, ~NTIME, ~TAD, ~ANALYTE,
     1L,     0,  100,    1,    1L,   NA_real_,      0,    0,   "DRUG",
     1L,     1,    0,    2,    0L,         10,     10,   10,   "DRUG"
  ))

  expect_silent(check.nif(obj, silent = FALSE))
})


test_that("check.nif silent = NULL follows nif_option('silent')", {
  obj <- as_nif_test(tibble::tribble(
    ~ID, ~TIME, ~AMT, ~CMT, ~EVID,        ~DV, ~NTIME, ~TAD, ~ANALYTE,
     1L,     0,  100,    1,    1L,   NA_real_,      0,    0,   "DRUG",
     1L,     1,    0,    2,    0L,         10,     10,   13,   "DRUG"
  ))
  old_silent <- nif_option_value("silent")
  on.exit(nif_option(silent = old_silent), add = TRUE)

  nif_option(silent = TRUE)
  expect_silent(check.nif(obj, ntime_threshold = 0.2, silent = NULL))

  nif_option(silent = FALSE)
  expect_message(
    check.nif(obj, ntime_threshold = 0.2, silent = NULL),
    "deviating from NTIME"
  )
})
