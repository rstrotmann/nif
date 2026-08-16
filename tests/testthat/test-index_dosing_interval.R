## Tests for index_dosing_interval
##
## Contract: add DI per ID × PARENT from distinct admin TIMEs (dense_rank).
## Default parent = all parents. Predose observations join the first interval.


test_that("index_dosing_interval numbers administrations and assigns observations", {
  nif_obj <- tibble::tribble(
    ~ID, ~TIME, ~AMT, ~EVID, ~DV, ~CMT, ~MDV, ~PARENT, ~ANALYTE,
      1,     0,  100,     1,  NA,    1,    1,     "A",      "A",
      1,     1,    0,     0,   1,    2,    0,     "A",      "A",
      1,     2,    0,     0,   2,    2,    0,     "A",      "A",
      1,    24,  100,     1,  NA,    1,    1,     "A",      "A",
      1,    25,    0,     0,   3,    2,    0,     "A",      "A",
      1,    48,  100,     1,  NA,    1,    1,     "A",      "A",
      1,    49,    0,     0,   4,    2,    0,     "A",      "A"
  ) |>
    nif()

  result <- index_dosing_interval(nif_obj) |>
    as.data.frame()

  expect_s3_class(index_dosing_interval(nif_obj), "nif")
  expect_equal(result$DI, c(1, 1, 1, 2, 2, 3, 3))
  expect_equal(max(result$DI), 3)
})


test_that("index_dosing_interval assigns predose observations to the first interval", {
  nif_obj <- tibble::tribble(
    ~ID, ~TIME, ~AMT, ~EVID,  ~DV, ~CMT, ~MDV, ~PARENT, ~ANALYTE,
      1,    -1,    0,     0, 0.1,    2,    0,     "A",      "A",
      1,  -0.5,    0,     0, 0.2,    2,    0,     "A",      "A",
      1,     0,  100,     1,  NA,    1,    1,     "A",      "A",
      1,     1,    0,     0,   1,    2,    0,     "A",      "A",
      1,    24,  100,     1,  NA,    1,    1,     "A",      "A",
      1,    25,    0,     0,   2,    2,    0,     "A",      "A"
  ) |>
    nif()

  result <- index_dosing_interval(nif_obj) |>
    as.data.frame()

  expect_true(all(result$DI[result$TIME < 0] == 1))
  expect_equal(result$DI[result$TIME == 0], 1)
  expect_equal(result$DI[result$TIME == 25], 2)
})


test_that("index_dosing_interval indexes separately per ID", {
  nif_obj <- tibble::tribble(
    ~ID, ~TIME, ~AMT, ~EVID, ~DV, ~CMT, ~MDV, ~PARENT, ~ANALYTE,
      1,     0,  100,     1,  NA,    1,    1,     "A",      "A",
      1,     1,    0,     0,   1,    2,    0,     "A",      "A",
      1,    24,  100,     1,  NA,    1,    1,     "A",      "A",
      1,    25,    0,     0,   2,    2,    0,     "A",      "A",
      2,     0,  100,     1,  NA,    1,    1,     "A",      "A",
      2,     1,    0,     0,   1,    2,    0,     "A",      "A"
  ) |>
    nif()

  result <- index_dosing_interval(nif_obj) |>
    as.data.frame()

  expect_equal(max(result$DI[result$ID == 1]), 2)
  expect_equal(max(result$DI[result$ID == 2]), 1)
})


test_that("index_dosing_interval indexes separately per PARENT by default", {
  nif_obj <- tibble::tribble(
    ~ID, ~TIME, ~AMT, ~EVID, ~DV, ~CMT, ~MDV, ~ANALYTE, ~PARENT, ~expected_DI,
      1,     0,  100,     1,  NA,    1,    1, "DRUG A",     "A",            1,
      1,    24,  100,     1,  NA,    1,    1, "DRUG B",     "B",            1,
      1,    48,  100,     1,  NA,    1,    1, "DRUG B",     "B",            2,
      1,    72,  100,     1,  NA,    1,    1, "DRUG A",     "A",            2
  ) |>
    nif()

  result <- index_dosing_interval(nif_obj)

  expect_equal(result$DI, result$expected_DI)
})


test_that("index_dosing_interval shares intervals across analytes of the same PARENT", {
  nif_obj <- tibble::tribble(
    ~ID, ~TIME, ~AMT, ~EVID,  ~DV, ~CMT, ~MDV, ~PARENT, ~ANALYTE,
      1,     0,  100,     1,   NA,    1,    1,     "A",      "A",
      1,     1,    0,     0,    1,    2,    0,     "A",      "A",
      1,     1,    0,     0,  0.5,    3,    0,     "A",      "M",
      1,    24,  100,     1,   NA,    1,    1,     "A",      "A",
      1,    25,    0,     0,    2,    2,    0,     "A",      "A",
      1,    25,    0,     0,    1,    3,    0,     "A",      "M"
  ) |>
    nif()

  result <- index_dosing_interval(nif_obj) |>
    as.data.frame()

  expect_equal(result$DI[result$ANALYTE == "M"], c(1, 2))
  expect_equal(result$DI[result$ANALYTE == "A"], c(1, 1, 2, 2))
})


test_that("index_dosing_interval with parent restricts which administrations are indexed", {
  nif_obj <- tibble::tribble(
    ~ID, ~TIME, ~AMT, ~EVID, ~DV, ~CMT, ~MDV, ~PARENT, ~ANALYTE,
      1,     0,  100,     1,  NA,    1,    1,     "A",      "A",
      1,     1,    0,     0,   1,    2,    0,     "A",      "A",
      1,    24,  100,     1,  NA,    1,    1,     "A",      "A",
      1,     0,   50,     1,  NA,    1,    1,     "B",      "B",
      1,     1,    0,     0, 0.5,    3,    0,     "B",      "B"
  ) |>
    nif()

  result <- index_dosing_interval(nif_obj, parent = "A") |>
    as.data.frame()

  expect_equal(result$DI[result$PARENT == "A"], c(1, 1, 2))
  expect_true(all(is.na(result$DI[result$PARENT == "B"])))
})


test_that("index_dosing_interval accepts multiple parents", {
  nif_obj <- tibble::tribble(
    ~ID, ~TIME, ~AMT, ~EVID, ~DV, ~CMT, ~MDV, ~PARENT, ~ANALYTE,
      1,     0,  100,     1,  NA,    1,    1,     "A",      "A",
      1,     1,    0,     0,   1,    2,    0,     "A",      "A",
      1,    24,  100,     1,  NA,    1,    1,     "A",      "A",
      1,     0,   50,     1,  NA,    3,    1,     "B",      "B",
      1,     1,    0,     0, 0.5,    4,    0,     "B",      "B",
      1,    48,   50,     1,  NA,    3,    1,     "B",      "B",
      1,     0,   25,     1,  NA,    5,    1,     "C",      "C",
      1,     1,    0,     0, 0.2,    6,    0,     "C",      "C"
  ) |>
    nif()

  result <- index_dosing_interval(nif_obj, parent = c("A", "B")) |>
    as.data.frame()

  expect_equal(result$DI[result$PARENT == "A"], c(1, 1, 2))
  expect_equal(result$DI[result$PARENT == "B"], c(1, 1, 2))
  expect_true(all(is.na(result$DI[result$PARENT == "C"])))
})


test_that("index_dosing_interval with all parents matches the default", {
  nif_obj <- tibble::tribble(
    ~ID, ~TIME, ~AMT, ~EVID, ~DV, ~CMT, ~MDV, ~PARENT, ~ANALYTE,
      1,     0,  100,     1,  NA,    1,    1,     "A",      "A",
      1,     1,    0,     0,   1,    2,    0,     "A",      "A",
      1,    24,  100,     1,  NA,    1,    1,     "A",      "A",
      1,     0,   50,     1,  NA,    3,    1,     "B",      "B",
      1,     1,    0,     0, 0.5,    4,    0,     "B",      "B"
  ) |>
    nif()

  default_di <- index_dosing_interval(nif_obj)$DI
  explicit_di <- index_dosing_interval(nif_obj, parent = c("A", "B"))$DI

  expect_equal(default_di, explicit_di)
})


test_that("index_dosing_interval leaves DI as NA for an unknown parent filter", {
  nif_obj <- tibble::tribble(
    ~ID, ~TIME, ~AMT, ~EVID, ~DV, ~CMT, ~MDV, ~PARENT, ~ANALYTE,
      1,     0,  100,     1,  NA,    1,    1,     "A",      "A",
      1,     1,    0,     0,   1,    2,    0,     "A",      "A"
  ) |>
    nif()

  result <- index_dosing_interval(nif_obj, parent = "Z") |>
    as.data.frame()

  expect_true(all(is.na(result$DI)))
})


test_that("index_dosing_interval leaves DI as NA when a subject has no administrations", {
  nif_obj <- tibble::tribble(
    ~ID, ~TIME, ~AMT, ~EVID, ~DV, ~CMT, ~MDV, ~PARENT, ~ANALYTE,
      1,     0,    0,     0,   1,    2,    0,     "A",      "A",
      1,     1,    0,     0,   2,    2,    0,     "A",      "A",
      2,     0,  100,     1,  NA,    1,    1,     "A",      "A",
      2,     1,    0,     0,   1,    2,    0,     "A",      "A"
  ) |>
    nif()

  result <- index_dosing_interval(nif_obj) |>
    as.data.frame()

  expect_true(all(is.na(result$DI[result$ID == 1])))
  expect_equal(result$DI[result$ID == 2], c(1, 1))
})


test_that("index_dosing_interval gives the same DI to simultaneous administrations", {
  nif_obj <- tibble::tribble(
    ~ID, ~TIME, ~AMT, ~EVID, ~DV, ~CMT, ~MDV, ~PARENT, ~ANALYTE,
      1,     0,   50,     1,  NA,    1,    1,     "A",      "A",
      1,     0,   50,     1,  NA,    1,    1,     "A",      "B",
      1,     1,    0,     0,   1,    2,    0,     "A",      "A"
  ) |>
    nif()

  result <- index_dosing_interval(nif_obj) |>
    as.data.frame()

  expect_equal(result$DI[result$EVID == 1], c(1, 1))
  expect_equal(result$DI[result$EVID == 0], 1)
  expect_equal(max(result$DI), 1)
})


test_that("index_dosing_interval counts EVID == 1 with AMT == 0 as an interval", {
  nif_obj <- tibble::tribble(
    ~ID, ~TIME, ~AMT, ~EVID, ~DV, ~CMT, ~MDV, ~PARENT, ~ANALYTE,
      1,     0,    0,     1,  NA,    1,    1,     "A",      "A",
      1,     1,    0,     0,   1,    2,    0,     "A",      "A",
      1,    24,  100,     1,  NA,    1,    1,     "A",      "A",
      1,    25,    0,     0,   2,    2,    0,     "A",      "A"
  ) |>
    nif()

  result <- index_dosing_interval(nif_obj) |>
    as.data.frame()

  expect_equal(result$DI, c(1, 1, 2, 2))
})


test_that("index_dosing_interval is idempotent when re-applied", {
  nif_obj <- tibble::tribble(
    ~ID, ~TIME, ~AMT, ~EVID, ~DV, ~CMT, ~MDV, ~PARENT, ~ANALYTE,
      1,     0,  100,     1,  NA,    1,    1,     "A",      "A",
      1,     1,    0,     0,   1,    2,    0,     "A",      "A",
      1,    24,  100,     1,  NA,    1,    1,     "A",      "A",
      1,    25,    0,     0,   2,    2,    0,     "A",      "A"
  ) |>
    nif()

  once <- index_dosing_interval(nif_obj) |>
    as.data.frame()
  twice <- index_dosing_interval(nif_obj) |>
    index_dosing_interval() |>
    as.data.frame()

  expect_equal(once$DI, twice$DI)
  expect_equal(once$DI, c(1, 1, 2, 2))
})


test_that("index_dosing_interval replaces an existing DI column", {
  nif_obj <- tibble::tribble(
    ~ID, ~TIME, ~AMT, ~EVID, ~DV, ~CMT, ~MDV, ~PARENT, ~ANALYTE, ~DI,
      1,     0,  100,     1,  NA,    1,    1,     "A",      "A",  99,
      1,     1,    0,     0,   1,    2,    0,     "A",      "A",  99,
      1,    24,  100,     1,  NA,    1,    1,     "A",      "A",  99,
      1,    25,    0,     0,   2,    2,    0,     "A",      "A",  99
  ) |>
    nif()

  result <- index_dosing_interval(nif_obj) |>
    as.data.frame()

  expect_equal(result$DI, c(1, 1, 2, 2))
})


test_that("index_dosing_interval ensures PARENT when missing", {
  nif_obj <- tibble::tribble(
    ~ID, ~TIME, ~AMT, ~EVID, ~DV, ~CMT, ~MDV, ~ANALYTE,
      1,     0,  100,     1,  NA,    1,    1,      "A",
      1,     1,    0,     0,   1,    2,    0,      "A",
      1,    24,  100,     1,  NA,    1,    1,      "A",
      1,    25,    0,     0,   2,    2,    0,      "A"
  ) |>
    nif()

  result <- index_dosing_interval(nif_obj) |>
    as.data.frame()

  expect_true("PARENT" %in% names(result))
  expect_equal(unique(result$PARENT), "A")
  expect_equal(result$DI, c(1, 1, 2, 2))
})


test_that("index_dosing_interval works without ANALYTE when PARENT is present", {
  nif_obj <- tibble::tribble(
    ~ID, ~TIME, ~AMT, ~EVID, ~DV, ~CMT, ~MDV, ~PARENT,
      1,     0,  100,     1,  NA,    1,    1,     "A",
      1,     1,    0,     0,   1,    2,    0,     "A",
      1,    24,  100,     1,  NA,    1,    1,     "A",
      1,    25,    0,     0,   2,    2,    0,     "A"
  ) |>
    nif()

  result <- index_dosing_interval(nif_obj) |>
    as.data.frame()

  expect_equal(result$DI, c(1, 1, 2, 2))
})


test_that("index_dosing_interval adds REF and returns rows in REF order", {
  nif_obj <- tibble::tribble(
    ~ID, ~TIME, ~AMT, ~EVID, ~DV, ~CMT, ~MDV, ~PARENT, ~ANALYTE,
      1,    24,  100,     1,  NA,    1,    1,     "A",      "A",
      1,     0,  100,     1,  NA,    1,    1,     "A",      "A",
      1,     1,    0,     0,   1,    2,    0,     "A",      "A",
      1,    25,    0,     0,   2,    2,    0,     "A",      "A"
  ) |>
    nif()

  result <- index_dosing_interval(nif_obj) |>
    as.data.frame()

  expect_true("REF" %in% names(result))
  expect_equal(result$REF, seq_len(nrow(result)))
  expect_equal(result$TIME, c(0, 1, 24, 25))
  expect_equal(result$DI, c(1, 1, 2, 2))
})


test_that("index_dosing_interval validates inputs", {
  nif_obj <- tibble::tribble(
    ~ID, ~TIME, ~AMT, ~EVID, ~DV, ~CMT, ~MDV, ~PARENT, ~ANALYTE,
      1,     0,  100,     1,  NA,    1,    1,     "A",      "A",
      1,     1,    0,     0,   1,    2,    0,     "A",      "A"
  ) |>
    nif()

  expect_error(
    index_dosing_interval(data.frame(ID = 1)),
    "Input must be a nif object"
  )

  expect_error(
    index_dosing_interval(nif_obj, parent = 1),
    "parent must be a character value"
  )

  expect_error(
    index_dosing_interval(nif()),
    "Cannot determine PARENT"
  )
})


test_that("index_dosing_interval works with examplinib data", {
  result <- index_dosing_interval(examplinib_poc_min_nif)

  expect_true("DI" %in% names(result))
  expect_true(all(result$DI >= 1 | is.na(result$DI)))
  expect_gt(max(result$DI, na.rm = TRUE), 0)

  expect_no_error(index_dosing_interval(examplinib_poc_nif))
  expect_no_error(index_dosing_interval(examplinib_fe_nif))
})
