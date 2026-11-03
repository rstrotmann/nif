dtc <- function(x) as.POSIXct(x, tz = "UTC")


make_existing_nif <- function() {
  as_nif_test(
    tibble::tribble(
      ~ID, ~USUBJID, ~DTC,                    ~TIME, ~EVID, ~ANALYTE, ~AMT, ~DV, ~CMT,
      1,   "S-1",    dtc("2024-01-01 08:00"), 0,     1,     "DRUG",   100,  NA,  1,
      1,   "S-1",    dtc("2024-01-02 08:00"), 24,    1,     "DRUG",   100,  NA,  1,
      1,   "S-1",    dtc("2024-01-01 09:00"), 1,     0,     "DRUG",   0,    5,   2,
      2,   "S-2",    dtc("2024-01-01 08:30"), 0,     1,     "DRUG",   100,  NA,  1
    )
  )
}


test_that("stop_if_already_added() aborts on an administration already present", {
  existing <- make_existing_nif()
  new <- tibble::tribble(
    ~USUBJID, ~DTC,                    ~EVID, ~ANALYTE, ~AMT,
    "S-1",    dtc("2024-01-01 08:00"), 1,     "DRUG",   100
  )

  expect_error(
    nif:::stop_if_already_added(existing, new, evid = 1),
    class = "nif_already_added"
  )
  expect_error(
    nif:::stop_if_already_added(existing, new, evid = 1),
    "1 administration row\\(s\\) for analyte DRUG are already in the nif object"
  )
})


test_that("stop_if_already_added() aborts on an observation already present", {
  existing <- make_existing_nif()
  new <- tibble::tribble(
    ~USUBJID, ~DTC,                    ~EVID, ~ANALYTE, ~DV,
    "S-1",    dtc("2024-01-01 09:00"), 0,     "DRUG",   5
  )

  expect_error(
    nif:::stop_if_already_added(existing, new, evid = 0),
    class = "nif_already_added"
  )
  expect_error(
    nif:::stop_if_already_added(existing, new, evid = 0),
    "1 observation row\\(s\\) for analyte DRUG are already in the nif object"
  )
})


test_that("stop_if_already_added() counts all overlapping rows", {
  existing <- make_existing_nif()
  new <- tibble::tribble(
    ~USUBJID, ~DTC,                    ~EVID, ~ANALYTE, ~AMT,
    "S-1",    dtc("2024-01-01 08:00"), 1,     "DRUG",   100,
    "S-1",    dtc("2024-01-02 08:00"), 1,     "DRUG",   100,
    "S-2",    dtc("2024-01-01 08:30"), 1,     "DRUG",   100,
    "S-2",    dtc("2024-01-08 08:30"), 1,     "DRUG",   100
  )

  expect_error(
    nif:::stop_if_already_added(existing, new, evid = 1),
    "3 administration row\\(s\\)"
  )
})


test_that("stop_if_already_added() lists each affected analyte once", {
  existing <- as_nif_test(
    tibble::tribble(
      ~ID, ~USUBJID, ~DTC,                    ~TIME, ~EVID, ~ANALYTE, ~AMT,
      1,   "S-1",    dtc("2024-01-01 08:00"), 0,     1,     "DRUG-A", 100,
      1,   "S-1",    dtc("2024-01-02 08:00"), 24,    1,     "DRUG-A", 100,
      1,   "S-1",    dtc("2024-01-01 08:00"), 0,     1,     "DRUG-B", 50
    )
  )
  new <- tibble::tribble(
    ~USUBJID, ~DTC,                    ~EVID, ~ANALYTE, ~AMT,
    "S-1",    dtc("2024-01-01 08:00"), 1,     "DRUG-A", 100,
    "S-1",    dtc("2024-01-02 08:00"), 1,     "DRUG-A", 100,
    "S-1",    dtc("2024-01-01 08:00"), 1,     "DRUG-B", 50
  )

  err <- expect_error(
    nif:::stop_if_already_added(existing, new, evid = 1),
    class = "nif_already_added"
  )
  expect_match(conditionMessage(err), "3 administration row\\(s\\)")
  expect_match(conditionMessage(err), "DRUG-A")
  expect_match(conditionMessage(err), "DRUG-B")
})


test_that("stop_if_already_added() accepts rows that differ in time", {
  existing <- make_existing_nif()
  new <- tibble::tribble(
    ~USUBJID, ~DTC,                    ~EVID, ~ANALYTE, ~AMT,
    "S-1",    dtc("2024-01-03 08:00"), 1,     "DRUG",   100
  )

  expect_no_error(nif:::stop_if_already_added(existing, new, evid = 1))
  expect_null(nif:::stop_if_already_added(existing, new, evid = 1))
})


test_that("stop_if_already_added() accepts rows of another subject", {
  existing <- make_existing_nif()
  new <- tibble::tribble(
    ~USUBJID, ~DTC,                    ~EVID, ~ANALYTE, ~AMT,
    "S-3",    dtc("2024-01-01 08:00"), 1,     "DRUG",   100
  )

  expect_no_error(nif:::stop_if_already_added(existing, new, evid = 1))
})


test_that("stop_if_already_added() accepts rows of another analyte", {
  existing <- make_existing_nif()
  new <- tibble::tribble(
    ~USUBJID, ~DTC,                    ~EVID, ~ANALYTE, ~AMT,
    "S-1",    dtc("2024-01-01 08:00"), 1,     "OTHER",  100
  )

  expect_no_error(nif:::stop_if_already_added(existing, new, evid = 1))
})


test_that("stop_if_already_added() only compares rows with the given EVID", {
  existing <- make_existing_nif()

  # An observation at the time of an existing dose is not an administration
  # duplicate, and vice versa.
  new_obs <- tibble::tribble(
    ~USUBJID, ~DTC,                    ~EVID, ~ANALYTE, ~DV,
    "S-1",    dtc("2024-01-01 08:00"), 0,     "DRUG",   1
  )
  expect_no_error(nif:::stop_if_already_added(existing, new_obs, evid = 0))

  new_admin <- tibble::tribble(
    ~USUBJID, ~DTC,                    ~EVID, ~ANALYTE, ~AMT,
    "S-1",    dtc("2024-01-01 09:00"), 1,     "DRUG",   100
  )
  expect_no_error(nif:::stop_if_already_added(existing, new_admin, evid = 1))
})


test_that("stop_if_already_added() ignores new rows with a different EVID", {
  existing <- make_existing_nif()

  # Mixed block: only the EVID == 1 row overlaps, the observation does not
  # count when checking observations.
  new <- tibble::tribble(
    ~USUBJID, ~DTC,                    ~EVID, ~ANALYTE, ~AMT, ~DV,
    "S-1",    dtc("2024-01-01 08:00"), 1,     "DRUG",   100,  NA,
    "S-1",    dtc("2024-01-05 09:00"), 0,     "DRUG",   0,    3
  )

  expect_error(
    nif:::stop_if_already_added(existing, new, evid = 1),
    "1 administration row\\(s\\)"
  )
  expect_no_error(nif:::stop_if_already_added(existing, new, evid = 0))
})


test_that("stop_if_already_added() skips an empty nif", {
  new <- tibble::tribble(
    ~USUBJID, ~DTC,                    ~EVID, ~ANALYTE, ~AMT,
    "S-1",    dtc("2024-01-01 08:00"), 1,     "DRUG",   100
  )

  expect_no_error(nif:::stop_if_already_added(nif(silent = TRUE), new, evid = 1))
  expect_null(nif:::stop_if_already_added(nif(silent = TRUE), new, evid = 1))
})


test_that("stop_if_already_added() skips a nif without DTC", {
  # Without DTC the key degrades; the check must not produce false positives
  # for doses of the same analyte at a different time.
  existing <- as_nif_test(
    tibble::tribble(
      ~ID, ~USUBJID, ~TIME, ~EVID, ~ANALYTE, ~AMT,
      1,   "S-1",    0,     1,     "DRUG",   100
    )
  )
  new <- tibble::tribble(
    ~USUBJID, ~DTC,                    ~EVID, ~ANALYTE, ~AMT,
    "S-1",    dtc("2024-01-08 08:00"), 1,     "DRUG",   100
  )

  expect_no_error(nif:::stop_if_already_added(existing, new, evid = 1))
})


test_that("stop_if_already_added() skips new rows without the key columns", {
  existing <- make_existing_nif()

  no_dtc <- tibble::tribble(
    ~USUBJID, ~EVID, ~ANALYTE, ~AMT,
    "S-1",    1,     "DRUG",   100
  )
  no_analyte <- tibble::tribble(
    ~USUBJID, ~DTC,                    ~EVID, ~AMT,
    "S-1",    dtc("2024-01-01 08:00"), 1,     100
  )
  no_usubjid <- tibble::tribble(
    ~DTC,                    ~EVID, ~ANALYTE, ~AMT,
    dtc("2024-01-01 08:00"), 1,     "DRUG",   100
  )

  expect_no_error(nif:::stop_if_already_added(existing, no_dtc, evid = 1))
  expect_no_error(nif:::stop_if_already_added(existing, no_analyte, evid = 1))
  expect_no_error(nif:::stop_if_already_added(existing, no_usubjid, evid = 1))
})


test_that("stop_if_already_added() accepts zero new rows", {
  existing <- make_existing_nif()
  new <- tibble::tibble(
    USUBJID = character(0),
    DTC     = dtc(character(0)),
    EVID    = numeric(0),
    ANALYTE = character(0)
  )

  expect_no_error(nif:::stop_if_already_added(existing, new, evid = 1))
  expect_no_error(nif:::stop_if_already_added(existing, new, evid = 0))
})


test_that("stop_if_already_added() reports the caller in the error", {
  existing <- make_existing_nif()
  new <- tibble::tribble(
    ~USUBJID, ~DTC,                    ~EVID, ~ANALYTE, ~AMT,
    "S-1",    dtc("2024-01-01 08:00"), 1,     "DRUG",   100
  )

  err <- expect_error(
    nif:::stop_if_already_added(existing, new, evid = 1),
    class = "nif_already_added"
  )
  expect_s3_class(err, "rlang_error")
})


test_that("add_administration() succeeds on an empty nif", {
  result <- add_administration(
    nif(silent = TRUE), examplinib_sad, "EXAMPLINIB", silent = TRUE
  )

  expect_s3_class(result, "nif")
  expect_equal(sum(result$EVID == 1), 48)
})


test_that("add_administration() twice with the same extrt aborts", {
  once <- add_administration(
    nif(silent = TRUE), examplinib_sad, "EXAMPLINIB", silent = TRUE
  )

  expect_error(
    add_administration(once, examplinib_sad, "EXAMPLINIB", silent = TRUE),
    class = "nif_already_added"
  )
  expect_error(
    add_administration(once, examplinib_sad, "EXAMPLINIB", silent = TRUE),
    "48 administration row\\(s\\) for analyte EXAMPLINIB"
  )
})


test_that("a failed add_administration() leaves the input untouched", {
  once <- add_administration(
    nif(silent = TRUE), examplinib_sad, "EXAMPLINIB", silent = TRUE
  )
  before <- once

  try(
    add_administration(once, examplinib_sad, "EXAMPLINIB", silent = TRUE),
    silent = TRUE
  )

  expect_identical(once, before)
  expect_equal(sum(once$EVID == 1), 48)
})


test_that("add_administration() accepts disjoint subject groups of one analyte", {
  c1 <- add_administration(
    nif(silent = TRUE), examplinib_sad, "EXAMPLINIB",
    subject_filter = "ACTARMCD == 'C1'", silent = TRUE
  )
  c1_n <- sum(c1$EVID == 1)

  both <- add_administration(
    c1, examplinib_sad, "EXAMPLINIB",
    subject_filter = "ACTARMCD == 'C2'", silent = TRUE
  )

  expect_gt(sum(both$EVID == 1), c1_n)
  expect_equal(
    sum(both$EVID == 1),
    c1_n + sum(
      add_administration(
        nif(silent = TRUE), examplinib_sad, "EXAMPLINIB",
        subject_filter = "ACTARMCD == 'C2'", silent = TRUE
      )$EVID == 1
    )
  )
})


test_that("add_administration() rejects overlapping subject groups", {
  c1 <- add_administration(
    nif(silent = TRUE), examplinib_sad, "EXAMPLINIB",
    subject_filter = "ACTARMCD %in% c('C1', 'C2')", silent = TRUE
  )

  expect_error(
    add_administration(
      c1, examplinib_sad, "EXAMPLINIB",
      subject_filter = "ACTARMCD == 'C2'", silent = TRUE
    ),
    class = "nif_already_added"
  )
})


test_that("add_administration() accepts another analyte name", {
  once <- add_administration(
    nif(silent = TRUE), examplinib_sad, "EXAMPLINIB", silent = TRUE
  )

  # Documented behavior: the analyte name is part of the identity, so the same
  # treatment under a different analyte name is a different administration.
  other <- add_administration(
    once, examplinib_sad, "EXAMPLINIB", analyte = "OTHER", silent = TRUE
  )

  expect_equal(sum(other$EVID == 1), 2 * 48)
  expect_setequal(unique(other$ANALYTE[other$EVID == 1]), c("EXAMPLINIB", "OTHER"))
})


test_that("add_observation() twice with the same testcd aborts", {
  base <- add_administration(
    nif(silent = TRUE), examplinib_sad, "EXAMPLINIB", silent = TRUE
  )
  once <- add_observation(
    base, examplinib_sad, "pc", "RS2023", silent = TRUE
  )

  expect_error(
    add_observation(once, examplinib_sad, "pc", "RS2023", silent = TRUE),
    class = "nif_already_added"
  )
  expect_error(
    add_observation(once, examplinib_sad, "pc", "RS2023", silent = TRUE),
    "observation row\\(s\\) for analyte RS2023"
  )
})


test_that("a failed add_observation() leaves the input untouched", {
  base <- add_administration(
    nif(silent = TRUE), examplinib_sad, "EXAMPLINIB", silent = TRUE
  )
  once <- add_observation(base, examplinib_sad, "pc", "RS2023", silent = TRUE)
  before <- once

  try(
    add_observation(once, examplinib_sad, "pc", "RS2023", silent = TRUE),
    silent = TRUE
  )

  expect_identical(once, before)
})


test_that("add_observation() accepts a different testcd", {
  base <- add_administration(
    nif(silent = TRUE), examplinib_sad, "EXAMPLINIB", silent = TRUE
  )
  one <- add_observation(base, examplinib_sad, "pc", "RS2023", silent = TRUE)
  two <- add_observation(
    one, examplinib_sad, "pc", "RS2023487A", silent = TRUE
  )

  expect_setequal(
    unique(two$ANALYTE[two$EVID == 0]), c("RS2023", "RS2023487A")
  )
  expect_gt(sum(two$EVID == 0), sum(one$EVID == 0))
})


test_that("add_observation() accepts disjoint subject groups of one analyte", {
  base <- add_administration(
    nif(silent = TRUE), examplinib_sad, "EXAMPLINIB", silent = TRUE
  )

  c1 <- add_observation(
    base, examplinib_sad, "pc", "RS2023",
    subject_filter = "ACTARMCD == 'C1'", silent = TRUE
  )
  both <- add_observation(
    c1, examplinib_sad, "pc", "RS2023",
    subject_filter = "ACTARMCD == 'C2'", silent = TRUE
  )

  expect_gt(sum(both$EVID == 0), sum(c1$EVID == 0))
  expect_equal(anyDuplicated(both[both$EVID == 0, c("USUBJID", "ANALYTE", "DTC")]), 0)
})


test_that("add_observation() on a nif without administrations still fails first", {
  expect_error(
    add_observation(
      nif(silent = TRUE), examplinib_sad, "pc", "RS2023", silent = TRUE
    ),
    "no treatment records"
  )
})


test_that("add_* calls in a single pipeline still work", {
  result <- nif(silent = TRUE) |>
    add_administration(examplinib_sad, "EXAMPLINIB", silent = TRUE) |>
    add_observation(examplinib_sad, "pc", "RS2023", silent = TRUE)

  expect_s3_class(result, "nif")
  expect_equal(sum(result$EVID == 1), 48)
  expect_equal(sum(result$EVID == 0), 816)
})
