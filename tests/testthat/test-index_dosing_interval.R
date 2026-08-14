# Comprehensive tests for index_dosing_interval().
# DI is numbered by distinct administration TIME within ID x PARENT.


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

  expect_equal(result$DI, c(1, 1, 1, 2, 2, 3, 3))
  expect_equal(max(result$DI), 3)
  expect_s3_class(index_dosing_interval(nif_obj), "nif")
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

  predose <- result[result$TIME < 0, ]
  expect_true(all(predose$DI == 1))
  expect_equal(result$DI[result$TIME == 0], 1)
  expect_equal(result$DI[result$TIME == 25], 2)
})


test_that("index_dosing_interval indexes separately per PARENT", {
  nif_obj <- tibble::tribble(
    ~ID, ~TIME, ~AMT, ~EVID, ~DV, ~CMT, ~MDV, ~PARENT, ~ANALYTE,
      1,     0,  100,     1,  NA,    1,    1,     "A",      "A",
      1,     1,    0,     0,   1,    2,    0,     "A",      "A",
      1,    24,  100,     1,  NA,    1,    1,     "A",      "A",
      1,    25,    0,     0,   2,    2,    0,     "A",      "A",
      1,    48,  100,     1,  NA,    1,    1,     "A",      "A",
      1,    49,    0,     0,   3,    2,    0,     "A",      "A",
      1,     0,   50,     1,  NA,    1,    1,     "B",      "B",
      1,     1,    0,     0, 0.5,    3,    0,     "B",      "B",
      1,    48,   50,     1,  NA,    1,    1,     "B",      "B",
      1,    49,    0,     0,   1,    3,    0,     "B",      "B"
  ) |>
    nif()

  result <- index_dosing_interval(nif_obj) |>
    as.data.frame()

  max_di <- result |>
    dplyr::group_by(.data$PARENT) |>
    dplyr::summarise(max_di = max(.data$DI), .groups = "drop")

  expect_equal(max_di$max_di, c(3, 2))
  expect_equal(
    nrow(dplyr::filter(result, .data$PARENT == "A", .data$DI == 3, .data$EVID == 0)),
    1
  )
  expect_equal(
    nrow(dplyr::filter(result, .data$PARENT == "B", .data$DI == 2, .data$EVID == 0)),
    1
  )
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

  expect_equal(
    result$DI[result$ANALYTE == "M"],
    c(1, 2)
  )
  expect_equal(
    result$DI[result$ANALYTE == "A"],
    c(1, 1, 2, 2)
  )
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


test_that("index_dosing_interval advances DI only when administration TIME changes", {
  nif_obj <- tibble::tribble(
    ~ID, ~TIME, ~AMT, ~EVID,  ~DV, ~CMT, ~MDV, ~PARENT, ~ANALYTE,
      1,     0,  100,     1,   NA,    1,    1, "COMBO",      "A",
      1,     0,   50,     1,   NA,    1,    1, "COMBO",      "B",
      1,     1,    0,     0,    1,    2,    0, "COMBO",      "A",
      1,    12,   50,     1,   NA,    1,    1, "COMBO",      "B",
      1,    13,    0,     0,  0.5,    3,    0, "COMBO",      "B",
      1,    24,  100,     1,   NA,    1,    1, "COMBO",      "A",
      1,    24,   50,     1,   NA,    1,    1, "COMBO",      "B",
      1,    25,    0,     0,    2,    2,    0, "COMBO",      "A"
  ) |>
    nif()

  result <- index_dosing_interval(nif_obj) |>
    as.data.frame()

  expect_equal(result$DI, c(1, 1, 1, 2, 2, 3, 3, 3))
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
    index_dosing_interval(nif_obj, parent = c("A", "B")),
    "parent must be a single value"
  )

  expect_error(
    index_dosing_interval(nif()),
    "Cannot determine PARENT"
  )
})
