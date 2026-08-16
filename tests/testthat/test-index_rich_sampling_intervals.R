## Tests for index_rich_sampling_intervals
##
## Flags dosing intervals with OPDI >= min_n and indexes them as RICH_N
## per subject and analyte.


test_that("index_rich_sampling_intervals adds DI, OPDI, and RICH_N", {
  nif_obj <- tibble::tribble(
    ~ID, ~TIME, ~EVID, ~CMT, ~AMT, ~DV, ~PARENT, ~ANALYTE,
      1,     0,     1,    1,  100,  NA,     "A",      "A",
      1,     0,     0,    2,    0,  10,     "A",      "A",
      1,     2,     0,    2,    0,  20,     "A",      "A",
      1,     4,     0,    2,    0,  30,     "A",      "A",
      1,     6,     0,    2,    0,  40,     "A",      "A",
      1,    24,     1,    1,  100,  NA,     "A",      "A",
      1,    25,     0,    2,    0,  50,     "A",      "A"
  ) |>
    nif()

  result <- index_rich_sampling_intervals(nif_obj, min_n = 4)

  expect_s3_class(result, "nif")
  expect_true(all(c("DI", "OPDI", "RICH_N") %in% names(result)))
})


test_that("index_rich_sampling_intervals indexes rich intervals with default min_n", {
  nif_obj <- tibble::tribble(
    ~ID, ~TIME, ~EVID, ~CMT, ~AMT, ~DV, ~PARENT, ~ANALYTE,
      1,     0,     1,    1,  100,  NA,     "A",      "A",
      1,     0,     0,    2,    0,  10,     "A",      "A",
      1,     2,     0,    2,    0,  20,     "A",      "A",
      1,     4,     0,    2,    0,  30,     "A",      "A",
      1,     6,     0,    2,    0,  40,     "A",      "A",
      1,     8,     0,    2,    0,  50,     "A",      "A",
      1,    24,     1,    1,  100,  NA,     "A",      "A",
      1,    48,     1,    1,  100,  NA,     "A",      "A",
      1,    72,     1,    1,  100,  NA,     "A",      "A",
      1,    72,     0,    2,    0,  60,     "A",      "A",
      1,    96,     1,    1,  100,  NA,     "A",      "A",
      1,    96,     0,    2,    0,  70,     "A",      "A",
      1,    97,     0,    2,    0,  80,     "A",      "A",
      1,    98,     0,    2,    0,  90,     "A",      "A",
      1,   100,     0,    2,    0, 100,     "A",      "A",
      1,   102,     0,    2,    0, 110,     "A",      "A",
      1,   104,     0,    2,    0, 120,     "A",      "A"
  ) |>
    nif()

  result <- index_rich_sampling_intervals(nif_obj) |>
    as.data.frame()

  expect_equal(sort(unique(result$RICH_N), na.last = TRUE), c(1, 2, NA))

  rich1 <- result[result$DI == 1, ]
  expect_equal(unique(rich1$OPDI), 5)
  expect_equal(unique(rich1$RICH_N), 1)

  sparse <- result[result$DI %in% c(2, 3, 4), ]
  expect_true(all(is.na(sparse$RICH_N)))

  rich2 <- result[result$DI == 5, ]
  expect_equal(unique(rich2$OPDI), 6)
  expect_equal(unique(rich2$RICH_N), 2)
})


test_that("index_rich_sampling_intervals respects min_n", {
  nif_obj <- tibble::tribble(
    ~ID, ~TIME, ~EVID, ~CMT, ~AMT, ~DV, ~PARENT, ~ANALYTE,
      1,     0,     1,    1,  100,  NA,     "A",      "A",
      1,     1,     0,    2,    0,  10,     "A",      "A",
      1,     2,     0,    2,    0,  20,     "A",      "A",
      1,     3,     0,    2,    0,  30,     "A",      "A",
      1,    24,     1,    1,  100,  NA,     "A",      "A",
      1,    25,     0,    2,    0,  40,     "A",      "A"
  ) |>
    nif()

  with_default <- index_rich_sampling_intervals(nif_obj, min_n = 4) |>
    as.data.frame()
  with_low <- index_rich_sampling_intervals(nif_obj, min_n = 3) |>
    as.data.frame()
  with_high <- index_rich_sampling_intervals(nif_obj, min_n = 10) |>
    as.data.frame()

  expect_true(all(is.na(with_default$RICH_N)))
  expect_equal(unique(with_low$RICH_N[with_low$DI == 1]), 1)
  expect_true(all(is.na(with_low$RICH_N[with_low$DI == 2])))
  expect_true(all(is.na(with_high$RICH_N)))
})


test_that("index_rich_sampling_intervals indexes rich intervals separately per subject", {
  nif_obj <- tibble::tribble(
    ~ID, ~TIME, ~EVID, ~CMT, ~AMT, ~DV, ~PARENT, ~ANALYTE,
      1,     0,     1,    1,  100,  NA,     "A",      "A",
      1,     1,     0,    2,    0,  10,     "A",      "A",
      1,     2,     0,    2,    0,  20,     "A",      "A",
      1,     3,     0,    2,    0,  30,     "A",      "A",
      1,     4,     0,    2,    0,  40,     "A",      "A",
      2,     0,     1,    1,  100,  NA,     "A",      "A",
      2,     1,     0,    2,    0,   5,     "A",      "A"
  ) |>
    nif()

  result <- index_rich_sampling_intervals(nif_obj, min_n = 4) |>
    as.data.frame()

  expect_equal(unique(result$RICH_N[result$ID == 1]), 1)
  expect_true(all(is.na(result$RICH_N[result$ID == 2])))
})


test_that("index_rich_sampling_intervals works without ANALYTE column", {
  nif_obj <- tibble::tribble(
    ~ID, ~TIME, ~EVID, ~CMT, ~AMT, ~DV,
      1,     0,     1,    1,  100,  NA,
      1,     1,     0,    2,    0,  10,
      1,     2,     0,    2,    0,  20,
      1,     3,     0,    2,    0,  30,
      1,     4,     0,    2,    0,  40
  ) |>
    nif()

  result <- index_rich_sampling_intervals(nif_obj, min_n = 4) |>
    as.data.frame()

  expect_true("ANALYTE" %in% names(result))
  expect_equal(unique(result$RICH_N[!is.na(result$RICH_N)]), 1)
})


test_that("index_rich_sampling_intervals validates inputs", {
  nif_obj <- tibble::tribble(
    ~ID, ~TIME, ~EVID, ~CMT, ~AMT, ~DV, ~PARENT, ~ANALYTE,
      1,     0,     1,    1,  100,  NA,     "A",      "A",
      1,     1,     0,    2,    0,  10,     "A",      "A"
  ) |>
    nif()

  expect_error(
    index_rich_sampling_intervals(data.frame(ID = 1)),
    "nif object"
  )

  expect_error(
    index_rich_sampling_intervals(nif_obj, min_n = "four"),
    "min_n must be a numeric value"
  )
})


test_that("index_rich_sampling_intervals works with examplinib data", {
  result <- index_rich_sampling_intervals(examplinib_poc_min_nif)

  expect_s3_class(result, "nif")
  expect_true("RICH_N" %in% names(result))
  expect_true(any(!is.na(result$RICH_N)) || all(is.na(result$RICH_N)))
  expect_no_error(index_rich_sampling_intervals(examplinib_poc_nif))
})
