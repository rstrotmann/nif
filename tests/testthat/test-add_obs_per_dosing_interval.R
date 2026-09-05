## Tests for add_obs_per_dosing_interval
##
## OPDI = number of EVID == 0 rows per ID × ANALYTE × DI.
## Metabolite analytes are counted separately from the parent analyte.


test_that("add_obs_per_dosing_interval adds OPDI with correct observation counts", {
  nif_obj <- tibble::tribble(
    ~ID, ~TIME, ~AMT, ~EVID, ~DV, ~CMT, ~PARENT, ~ANALYTE,
      1,     0,    0,     0,   0,    2,     "A",      "A",
      1,     0,  100,     1,  NA,    1,     "A",      "A",
      1,     1,    0,     0,   1,    2,     "A",      "A",
      1,     2,    0,     0,   2,    2,     "A",      "A",
      1,     4,    0,     0,   3,    2,     "A",      "A",
      1,    24,  100,     1,  NA,    1,     "A",      "A",
      1,    48,  100,     1,  NA,    1,     "A",      "A",
      1,    72,  100,     1,  NA,    1,     "A",      "A",
      1,    73,    0,     0,   1,    2,     "A",      "A",
      1,    74,    0,     0,   2,    2,     "A",      "A",
      1,    76,    0,     0,   3,    2,     "A",      "A"
  ) |>
    nif()

  result <- add_obs_per_dosing_interval(nif_obj) |>
    as.data.frame()

  expect_true("OPDI" %in% names(result))
  expect_true("DI" %in% names(result))
  expect_s3_class(add_obs_per_dosing_interval(nif_obj), "nif")

  # DI 1 includes predose + 3 post-dose observations
  expect_equal(unique(result$OPDI[result$DI == 1]), 4)
  # DI 2 and 3 are dose-only intervals
  expect_equal(unique(result$OPDI[result$DI == 2]), 0)
  expect_equal(unique(result$OPDI[result$DI == 3]), 0)
  # DI 4 has 3 observations
  expect_equal(unique(result$OPDI[result$DI == 4]), 3)
})


test_that("add_obs_per_dosing_interval shares OPDI across admin and obs of the same ANALYTE", {
  nif_obj <- tibble::tribble(
    ~ID, ~TIME, ~AMT, ~EVID, ~DV, ~CMT, ~PARENT, ~ANALYTE,
      1,     0,  100,     1,  NA,    1,     "A",      "A",
      1,     1,    0,     0,  10,    2,     "A",      "A",
      1,     2,    0,     0,  20,    2,     "A",      "A",
      1,    24,  100,     1,  NA,    1,     "A",      "A",
      1,    25,    0,     0,  15,    2,     "A",      "A"
  ) |>
    nif()

  result <- add_obs_per_dosing_interval(nif_obj) |>
    as.data.frame()

  di1 <- result[result$DI == 1, ]
  expect_equal(unique(di1$OPDI), 2)
  expect_true(all(di1$OPDI == 2))

  di2 <- result[result$DI == 2, ]
  expect_equal(unique(di2$OPDI), 1)
  expect_true(all(di2$OPDI == 1))
})


test_that("add_obs_per_dosing_interval counts separately by ANALYTE", {
  nif_obj <- tibble::tribble(
    ~ID, ~TIME, ~AMT, ~EVID, ~DV, ~CMT, ~PARENT, ~ANALYTE,
      1,     0,  100,     1,  NA,    1,     "A",      "A",
      1,     1,    0,     0,   1,    2,     "A",      "A",
      1,     2,    0,     0,   2,    2,     "A",      "A",
      1,    24,  100,     1,  NA,    1,     "A",      "A",
      1,     0,   50,     1,  NA,    3,     "B",      "B",
      1,     1,    0,     0,   5,    4,     "B",      "B",
      1,    48,   50,     1,  NA,    3,     "B",      "B",
      1,    49,    0,     0,   6,    4,     "B",      "B",
      1,    50,    0,     0,   7,    4,     "B",      "B"
  ) |>
    nif()

  result <- add_obs_per_dosing_interval(nif_obj) |>
    as.data.frame()

  expect_equal(unique(result$OPDI[result$ANALYTE == "A" & result$DI == 1]), 2)
  expect_equal(unique(result$OPDI[result$ANALYTE == "A" & result$DI == 2]), 0)
  expect_equal(unique(result$OPDI[result$ANALYTE == "B" & result$DI == 1]), 1)
  expect_equal(unique(result$OPDI[result$ANALYTE == "B" & result$DI == 2]), 2)
})


test_that("add_obs_per_dosing_interval does not let metabolites inflate parent OPDI", {
  nif_obj <- tibble::tribble(
    ~ID, ~TIME, ~AMT, ~EVID, ~DV, ~CMT, ~PARENT, ~ANALYTE, ~METABOLITE,
      1,     0,  100,     1,  NA,    1,     "A",      "A",      FALSE,
      1,     1,    0,     0,  10,    2,     "A",      "A",      FALSE,
      1,     2,    0,     0,  20,    2,     "A",      "A",      FALSE,
      1,     3,    0,     0,  30,    2,     "A",      "A",      FALSE,
      1,     4,    0,     0,  40,    2,     "A",      "A",      FALSE,
      1,     1,    0,     0,   1,    3,     "A",      "M",       TRUE,
      1,     2,    0,     0,   2,    3,     "A",      "M",       TRUE,
      1,    24,  100,     1,  NA,    1,     "A",      "A",      FALSE,
      1,    25,    0,     0,   5,    2,     "A",      "A",      FALSE
  ) |>
    nif()

  result <- add_obs_per_dosing_interval(nif_obj) |>
    as.data.frame()

  di1 <- result[result$DI == 1, ]
  expect_equal(unique(di1$OPDI[di1$ANALYTE == "A"]), 4)
  expect_equal(unique(di1$OPDI[di1$ANALYTE == "M"]), 2)

  di2 <- result[result$DI == 2, ]
  expect_equal(unique(di2$OPDI[di2$ANALYTE == "A"]), 1)
})


test_that("add_obs_per_dosing_interval counts separately by subject", {
  nif_obj <- tibble::tribble(
    ~ID, ~TIME, ~AMT, ~EVID, ~DV, ~CMT, ~PARENT, ~ANALYTE,
      1,     0,  100,     1,  NA,    1,     "A",      "A",
      1,     1,    0,     0,   1,    2,     "A",      "A",
      1,     2,    0,     0,   2,    2,     "A",      "A",
      2,     0,  100,     1,  NA,    1,     "A",      "A",
      2,     1,    0,     0,   5,    2,     "A",      "A"
  ) |>
    nif()

  result <- add_obs_per_dosing_interval(nif_obj) |>
    as.data.frame()

  expect_equal(unique(result$OPDI[result$ID == 1]), 2)
  expect_equal(unique(result$OPDI[result$ID == 2]), 1)
})


test_that("add_obs_per_dosing_interval replaces an existing DI column", {
  nif_obj <- tibble::tribble(
    ~ID, ~TIME, ~AMT, ~EVID, ~DV, ~CMT, ~PARENT, ~ANALYTE, ~DI,
      1,     0,  100,     1,  NA,    1,     "A",      "A",  99,
      1,     1,    0,     0,   1,    2,     "A",      "A",  99,
      1,    24,  100,     1,  NA,    1,     "A",      "A",  99,
      1,    25,    0,     0,   2,    2,     "A",      "A",  99
  ) |>
    nif()

  result <- add_obs_per_dosing_interval(nif_obj) |>
    as.data.frame()

  expect_equal(sort(unique(result$DI)), c(1, 2))
  expect_equal(unique(result$OPDI[result$DI == 1]), 1)
  expect_equal(unique(result$OPDI[result$DI == 2]), 1)
})


test_that("add_obs_per_dosing_interval derives CMT-based ANALYTE and counts per analyte", {
  nif_obj <- tibble::tribble(
    ~ID, ~TIME, ~AMT, ~EVID, ~DV, ~CMT, ~PARENT,
      1,     0,  100,     1,  NA,    1,     "A",
      1,     1,    0,     0,   1,    2,     "A",
      1,     2,    0,     0,   2,    2,     "A",
      1,    24,  100,     1,  NA,    1,     "A",
      1,    25,    0,     0,   3,    2,     "A"
  ) |>
    nif()

  result <- add_obs_per_dosing_interval(nif_obj)

  expect_true("ANALYTE" %in% names(result))
  expect_true(all(c("CMT1", "CMT2") %in% result$ANALYTE))

  # Admin rows are CMT1 and have no observations of that analyte
  expect_equal(unique(result$OPDI[result$ANALYTE == "CMT1"]), 0)
  # Observation rows are CMT2
  expect_equal(unique(result$OPDI[result$ANALYTE == "CMT2" & result$DI == 1]), 2)
  expect_equal(unique(result$OPDI[result$ANALYTE == "CMT2" & result$DI == 2]), 1)
})


test_that("add_obs_per_dosing_interval rejects non-nif input", {
  expect_error(
    add_obs_per_dosing_interval(data.frame(ID = 1, TIME = 0)),
    "Input must be a nif object"
  )
})


test_that("add_obs_per_dosing_interval works with examplinib data", {
  result <- add_obs_per_dosing_interval(examplinib_poc_min_nif)

  expect_true("OPDI" %in% names(result))
  expect_true("DI" %in% names(result))
  expect_true(all(result$OPDI >= 0, na.rm = TRUE))
  expect_gt(max(result$OPDI, na.rm = TRUE), 0)

  expect_no_error(add_obs_per_dosing_interval(examplinib_poc_nif))
})
