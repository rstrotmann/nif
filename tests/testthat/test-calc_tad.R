tad_fixture <- function() {
  # Doses at 0 and 12 h, and a trough at 12 h (same time as dose 2)
  tibble::tribble(
    ~ID, ~TIME, ~EVID, ~PARENT, ~ANALYTE, ~AMT, ~DV, ~CMT,
    1,   0,     1,     "A",     "A",      100,  NA,  1,
    1,   0,     0,     "A",     "A",      0,    0,   2,
    1,   6,     0,     "A",     "A",      0,    4,   2,
    1,   12,    0,     "A",     "A",      0,    2,   2,
    1,   12,    1,     "A",     "A",      100,  NA,  1,
    1,   18,    0,     "A",     "A",      0,    5,   2,
    1,   24,    0,     "A",     "A",      0,    1,   2
  )
}


test_that("calc_tad() relates a coincident sample to the previous dose", {
  time <- c(0, 0, 6, 12, 12, 18, 24)
  evid <- c(1, 0, 0, 0,  1,  0,  0)

  expect_equal(
    nif:::calc_tad(time, evid),
    c(0, 0, 6, 12, 0, 6, 12)
  )
})


test_that("calc_tad() does not depend on the order of the records", {
  time <- c(0, 0, 6, 12, 12, 18, 24)
  evid <- c(1, 0, 0, 0,  1,  0,  0)
  expected <- c(0, 0, 6, 12, 0, 6, 12)

  # dose before sample at coincident times
  idx <- order(time, -evid)
  expect_equal(nif:::calc_tad(time[idx], evid[idx]), expected[idx])

  # sample before dose at coincident times
  idx <- order(time, evid)
  expect_equal(nif:::calc_tad(time[idx], evid[idx]), expected[idx])

  # reversed
  idx <- rev(seq_along(time))
  expect_equal(nif:::calc_tad(time[idx], evid[idx]), expected[idx])
})


test_that("calc_tad() gives 0 for a sample at the first dose", {
  expect_equal(nif:::calc_tad(c(0, 0, 5), c(0, 1, 0)), c(0, 0, 5))
  expect_equal(nif:::calc_tad(c(0, 0, 5), c(1, 0, 0)), c(0, 0, 5))
})


test_that("calc_tad() is negative before the first dose", {
  expect_equal(nif:::calc_tad(c(-2, 0, 4), c(0, 1, 0)), c(-2, 0, 4))
})


test_that("calc_tad() is NA without any dose", {
  expect_equal(nif:::calc_tad(c(0, 1, 2), c(0, 0, 0)), rep(NA_real_, 3))
  expect_equal(nif:::calc_tad(numeric(0), numeric(0)), numeric(0))
})


test_that("calc_tad() gives 0 to all doses, also at the same time", {
  expect_equal(nif:::calc_tad(c(0, 0, 12), c(1, 1, 1)), c(0, 0, 0))
})


test_that("calc_tad() treats a sample at several coincident doses as trough", {
  # two doses at 12 h: sample at 12 h relates to the dose at 0 h
  expect_equal(
    nif:::calc_tad(c(0, 12, 12, 12, 15), c(1, 0, 1, 1, 0)),
    c(0, 12, 0, 0, 3)
  )
})


test_that("calc_tad() ignores EVID other than 1 as dose", {
  expect_equal(
    nif:::calc_tad(c(0, 6, 6, 12), c(1, 2, 0, 0)),
    c(0, 6, 6, 12)
  )
})


test_that("calc_tad() keeps NA time and NA EVID as NA or non-dose", {
  expect_equal(
    nif:::calc_tad(c(0, NA, 12), c(1, 0, 0)),
    c(0, NA, 12)
  )
  expect_equal(
    nif:::calc_tad(c(0, 6, 12), c(1, NA, 0)),
    c(0, 6, 12)
  )
  expect_equal(
    nif:::calc_tad(c(0, NA, 12), c(1, 1, 0)),
    c(0, NA, 12)
  )
})


test_that("calc_tad() handles fractional times", {
  expect_equal(
    nif:::calc_tad(c(0, 11.5, 12, 12.25), c(1, 0, 1, 0)),
    c(0, 11.5, 0, 0.25)
  )
})


test_that("add_tad() relates a coincident sample to the previous dose", {
  result <- nif:::add_tad(as_nif_test(tad_fixture()))

  expect_equal(result$TAD, c(0, 0, 6, 12, 0, 6, 12))
})


test_that("make_time_from_time() relates a coincident sample to the previous dose", {
  result <- nif:::make_time_from_time(as_nif_test(tad_fixture()))

  expect_equal(result$TAD, c(0, 0, 6, 12, 0, 6, 12))
  expect_equal(result$TAFD, c(0, 0, 6, 12, 12, 18, 24))
})


test_that("make_time() relates a coincident sample to the previous dose", {
  t0 <- as.POSIXct("2024-01-01 08:00", tz = "UTC")
  d <- tibble::tribble(
    ~ID, ~DTC,                 ~EVID, ~PARENT, ~ANALYTE, ~AMT, ~DV, ~CMT,
    1,   t0,                   1,     "A",     "A",      100,  NA,  1,
    1,   t0,                   0,     "A",     "A",      0,    0,   2,
    1,   t0 + 6 * 3600,        0,     "A",     "A",      0,    4,   2,
    1,   t0 + 12 * 3600,       0,     "A",     "A",      0,    2,   2,
    1,   t0 + 12 * 3600,       1,     "A",     "A",      100,  NA,  1,
    1,   t0 + 18 * 3600,       0,     "A",     "A",      0,    5,   2,
    1,   t0 + 24 * 3600,       0,     "A",     "A",      0,    1,   2
  )

  result <- make_time(as_nif_test(d))

  expect_equal(result$TIME, c(0, 0, 6, 12, 12, 18, 24))
  expect_equal(result$TAD, c(0, 0, 6, 12, 0, 6, 12))
})


test_that("all three code paths give the same TAD for coincident records", {
  t0 <- as.POSIXct("2024-01-01 08:00", tz = "UTC")
  with_time <- tad_fixture()
  with_dtc <- with_time |>
    dplyr::mutate(DTC = t0 + .data$TIME * 3600) |>
    dplyr::select(-"TIME")

  tad_add <- nif:::add_tad(as_nif_test(with_time))$TAD
  tad_from_time <- nif:::make_time_from_time(as_nif_test(with_time))$TAD
  tad_dtc <- make_time(as_nif_test(with_dtc))$TAD

  expect_equal(tad_add, tad_from_time)
  expect_equal(tad_add, tad_dtc)
})


test_that("TAD does not depend on the row order of the nif", {
  base <- tad_fixture()
  expected <- c(0, 0, 6, 12, 0, 6, 12)

  # sample before dose at coincident times
  sample_first <- base[c(1, 2, 3, 4, 5, 6, 7), ]
  # dose before sample at coincident times
  dose_first <- base[c(1, 2, 3, 5, 4, 6, 7), ]

  for (f in list(nif:::add_tad, nif:::make_time_from_time)) {
    expect_equal(f(as_nif_test(sample_first))$TAD, expected)
    expect_equal(
      f(as_nif_test(dose_first))$TAD,
      expected[c(1, 2, 3, 5, 4, 6, 7)]
    )
  }
})


test_that("TAD keeps the row order of the input", {
  base <- tad_fixture()
  shuffled <- base[c(7, 3, 5, 1, 6, 2, 4), ]

  result <- nif:::add_tad(as_nif_test(shuffled))

  expect_equal(result$TIME, shuffled$TIME)
  expect_equal(result$EVID, shuffled$EVID)
  expect_equal(
    result$TAD,
    c(12, 6, 0, 0, 6, 0, 12)
  )
})


test_that("TAD is calculated per ID and PARENT", {
  d <- tibble::tribble(
    ~ID, ~TIME, ~EVID, ~PARENT, ~ANALYTE, ~AMT, ~DV, ~CMT,
    1,   0,     1,     "A",     "A",      100,  NA,  1,
    1,   12,    0,     "A",     "A",      0,    2,   2,
    1,   12,    1,     "A",     "A",      100,  NA,  1,
    1,   12,    1,     "B",     "B",      50,   NA,  3,
    1,   12,    0,     "B",     "B",      0,    7,   4,
    1,   16,    0,     "B",     "B",      0,    6,   4,
    2,   0,     1,     "A",     "A",      100,  NA,  1,
    2,   12,    0,     "A",     "A",      0,    3,   2
  )

  expect_equal(
    nif:::add_tad(as_nif_test(d))$TAD,
    # subject 1, parent A: trough at 12 relates to the dose at 0
    # subject 1, parent B: first dose at 12, sample at 12 is at first dose
    c(0, 12, 0, 0, 0, 4, 0, 12)
  )
  expect_equal(
    nif:::make_time_from_time(as_nif_test(d))$TAD,
    c(0, 12, 0, 0, 0, 4, 0, 12)
  )
})


test_that("TAD is NA for a subject without administrations", {
  d <- tibble::tribble(
    ~ID, ~TIME, ~EVID, ~PARENT, ~ANALYTE, ~AMT, ~DV, ~CMT,
    1,   0,     1,     "A",     "A",      100,  NA,  1,
    1,   12,    0,     "A",     "A",      0,    2,   2,
    2,   0,     0,     "A",     "A",      0,    3,   2
  )

  expect_equal(nif:::add_tad(as_nif_test(d))$TAD, c(0, 12, NA))
  expect_equal(nif:::make_time_from_time(as_nif_test(d))$TAD, c(0, 12, NA))
})


test_that("TAD of an empty nif is empty", {
  empty <- nif(silent = TRUE)

  expect_equal(nrow(nif:::make_time_from_time(dplyr::mutate(empty, PARENT = character(0)))), 0)
})
