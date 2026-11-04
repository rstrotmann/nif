time_fixture <- function() {
  # two subjects, subject 1 with two parents, a trough at the second dose of
  # parent A, and subject 2 with a late first dose of parent B
  tibble::tribble(
    ~ID, ~TIME, ~EVID, ~PARENT, ~ANALYTE, ~AMT, ~DV, ~CMT,
    1,   0,     1,     "A",     "A",      100,  NA,  1,
    1,   0,     0,     "A",     "A",      0,    0,   2,
    1,   12,    0,     "A",     "A",      0,    2,   2,
    1,   12,    1,     "A",     "A",      100,  NA,  1,
    1,   12,    1,     "B",     "B",      50,   NA,  3,
    1,   12,    0,     "B",     "B",      0,    7,   4,
    1,   16,    0,     "B",     "B",      0,    6,   4,
    2,   0,     1,     "A",     "A",      100,  NA,  1,
    2,   12,    0,     "A",     "A",      0,    3,   2,
    2,   5,     1,     "B",     "B",      50,   NA,  3,
    2,   9,     0,     "B",     "B",      0,    1,   4
  )
}


time_fixture_expected <- function() {
  list(
    TAFD = c(0, 0, 12, 12, 0, 0, 4, 0, 12, 0, 4),
    TAD  = c(0, 0, 12, 0,  0, 0, 4, 0, 12, 0, 4)
  )
}


test_that("calc_tafd() relates all records to the first dose", {
  expect_equal(
    nif:::calc_tafd(c(0, 6, 12, 12), c(1, 0, 0, 1)),
    c(0, 6, 12, 12)
  )
})


test_that("calc_tafd() does not depend on the order of the records", {
  time <- c(0, 6, 12, 12, 24)
  evid <- c(1, 0, 0,  1,  0)
  expected <- c(0, 6, 12, 12, 24)

  for (idx in list(rev(seq_along(time)), c(3, 1, 5, 2, 4), order(time, evid))) {
    expect_equal(nif:::calc_tafd(time[idx], evid[idx]), expected[idx])
  }
})


test_that("calc_tafd() is negative before the first dose", {
  expect_equal(nif:::calc_tafd(c(-2, 0, 4), c(0, 1, 0)), c(-2, 0, 4))
})


test_that("calc_tafd() is NA without any dose", {
  expect_equal(nif:::calc_tafd(c(0, 1, 2), c(0, 0, 0)), rep(NA_real_, 3))
  expect_equal(nif:::calc_tafd(numeric(0), numeric(0)), numeric(0))
})


test_that("calc_tafd() ignores doses without time and non-dose events", {
  expect_equal(nif:::calc_tafd(c(NA, 5, 8), c(1, 1, 0)), c(NA, 0, 3))
  expect_equal(nif:::calc_tafd(c(0, 6, 12), c(2, 1, 0)), c(-6, 0, 6))
  expect_equal(nif:::calc_tafd(c(0, 6, 12), c(NA, 1, 0)), c(-6, 0, 6))
})


test_that("calc_tafd() rounds to three digits", {
  expect_equal(nif:::calc_tafd(c(0, 1.00049, 2.0006), c(1, 0, 0)), c(0, 1, 2.001))
})


test_that("add_time_fields() calculates TAFD and TAD per ID and PARENT", {
  result <- nif:::add_time_fields(as_nif_test(time_fixture()))

  expect_equal(result$TAFD, time_fixture_expected()$TAFD)
  expect_equal(result$TAD, time_fixture_expected()$TAD)
})


test_that("add_time_fields() does not depend on the row order", {
  fixture <- time_fixture()
  expected <- time_fixture_expected()

  set.seed(20261104)
  for (i in 1:50) {
    perm <- sample(nrow(fixture))
    result <- nif:::add_time_fields(as_nif_test(fixture[perm, ]))

    expect_equal(result$TAFD, expected$TAFD[perm])
    expect_equal(result$TAD, expected$TAD[perm])
  }
})


test_that("add_time_fields() keeps the row order and all other columns", {
  fixture <- time_fixture()
  perm <- c(11, 3, 5, 1, 9, 2, 10, 4, 7, 6, 8)
  input <- as_nif_test(fixture[perm, ])

  result <- nif:::add_time_fields(input)

  expect_equal(result$ID, input$ID)
  expect_equal(result$TIME, input$TIME)
  expect_equal(result$EVID, input$EVID)
  expect_equal(result$PARENT, input$PARENT)
  expect_equal(result$DV, input$DV)
  expect_s3_class(result, "nif")
  expect_false(dplyr::is_grouped_df(result))
})


test_that("add_time_fields() calculates only the requested fields", {
  input <- as_nif_test(time_fixture())

  only_tad <- nif:::add_time_fields(input, "TAD")
  expect_true("TAD" %in% names(only_tad))
  expect_false("TAFD" %in% names(only_tad))

  only_tafd <- nif:::add_time_fields(input, "TAFD")
  expect_true("TAFD" %in% names(only_tafd))
  expect_false("TAD" %in% names(only_tafd))

  expect_equal(only_tad$TAD, time_fixture_expected()$TAD)
  expect_equal(only_tafd$TAFD, time_fixture_expected()$TAFD)
})


test_that("add_time_fields() replaces existing fields", {
  input <- as_nif_test(
    dplyr::mutate(time_fixture(), TAFD = -99, TAD = -99)
  )

  result <- nif:::add_time_fields(input)

  expect_equal(result$TAFD, time_fixture_expected()$TAFD)
  expect_equal(result$TAD, time_fixture_expected()$TAD)
})


test_that("add_time_fields() rejects unknown fields", {
  expect_error(
    nif:::add_time_fields(as_nif_test(time_fixture()), "TIME"),
    "should be one of"
  )
})


test_that("add_time_fields() does not mix subjects with identical times", {
  d <- tibble::tribble(
    ~ID, ~TIME, ~EVID, ~PARENT, ~ANALYTE, ~AMT, ~DV, ~CMT,
    1,   0,     1,     "A",     "A",      100,  NA,  1,
    1,   10,    0,     "A",     "A",      0,    2,   2,
    2,   5,     1,     "A",     "A",      100,  NA,  1,
    2,   10,    0,     "A",     "A",      0,    3,   2
  )

  result <- nif:::add_time_fields(as_nif_test(d))

  expect_equal(result$TAFD, c(0, 10, 0, 5))
  expect_equal(result$TAD, c(0, 10, 0, 5))
})


test_that("add_time_fields() does not mix parents of one subject", {
  d <- tibble::tribble(
    ~ID, ~TIME, ~EVID, ~PARENT, ~ANALYTE, ~AMT, ~DV, ~CMT,
    1,   0,     1,     "A",     "A",      100,  NA,  1,
    1,   6,     1,     "B",     "B",      50,   NA,  3,
    1,   10,    0,     "A",     "A",      0,    2,   2,
    1,   10,    0,     "B",     "B",      0,    4,   4
  )

  result <- nif:::add_time_fields(as_nif_test(d))

  expect_equal(result$TAFD, c(0, 0, 10, 4))
  expect_equal(result$TAD, c(0, 0, 10, 4))
})


test_that("add_time_fields() gives NA for a subject without administrations", {
  d <- tibble::tribble(
    ~ID, ~TIME, ~EVID, ~PARENT, ~ANALYTE, ~AMT, ~DV, ~CMT,
    1,   0,     1,     "A",     "A",      100,  NA,  1,
    1,   12,    0,     "A",     "A",      0,    2,   2,
    2,   0,     0,     "A",     "A",      0,    3,   2
  )

  result <- nif:::add_time_fields(as_nif_test(d))

  expect_equal(result$TAFD, c(0, 12, NA))
  expect_equal(result$TAD, c(0, 12, NA))
})


test_that("add_time_from_dtc() relates TIME to the first record of each subject", {
  t0 <- as.POSIXct("2024-01-01 08:00", tz = "UTC")
  d <- tibble::tribble(
    ~ID, ~DTC,                       ~EVID, ~PARENT, ~ANALYTE, ~AMT, ~DV, ~CMT,
    1,   t0 + 12 * 3600,             0,     "A",     "A",      0,    2,   2,
    1,   t0,                         1,     "A",     "A",      100,  NA,  1,
    2,   t0 + 48 * 3600,             1,     "A",     "A",      100,  NA,  1,
    2,   t0 + 48 * 3600 + 1800,      0,     "A",     "A",      0,    3,   2
  )

  result <- nif:::add_time_from_dtc(as_nif_test(d))

  expect_equal(result$TIME, c(12, 0, 0, 0.5))
  expect_s3_class(result, "nif")
})


test_that("add_time_from_dtc() rounds TIME to three digits", {
  t0 <- as.POSIXct("2024-01-01 08:00:00", tz = "UTC")
  d <- tibble::tribble(
    ~ID, ~DTC,       ~EVID, ~PARENT, ~ANALYTE, ~AMT, ~DV, ~CMT,
    1,   t0,         1,     "A",     "A",      100,  NA,  1,
    1,   t0 + 1,     0,     "A",     "A",      0,    2,   2
  )

  result <- nif:::add_time_from_dtc(as_nif_test(d))

  # one second is 0.000278 hours
  expect_equal(result$TIME, c(0, 0))
  expect_equal(nif:::add_time_from_dtc(as_nif_test(
    dplyr::mutate(d, DTC = t0 + c(0, 1801))
  ))$TIME, c(0, 0.5))
})


test_that("make_time() and make_time_from_time() give the same fields", {
  for (nif_data in list(
    examplinib_sad_nif, examplinib_fe_nif, examplinib_poc_nif
  )) {
    base <- dplyr::select(nif_data, -dplyr::any_of(c("TAD", "TAFD")))

    from_dtc <- make_time(base)
    from_time <- nif:::make_time_from_time(base)

    expect_equal(from_dtc$TIME, from_time$TIME)
    expect_equal(from_dtc$TAFD, from_time$TAFD)
    expect_equal(from_dtc$TAD, from_time$TAD)
  }
})


test_that("add_tad() and add_tafd() agree with make_time()", {
  base <- dplyr::select(examplinib_poc_nif, -dplyr::any_of(c("TAD", "TAFD")))

  full <- make_time(base)

  expect_equal(nif:::add_tad(base)$TAD, full$TAD)
  expect_equal(nif:::add_tafd(base)$TAFD, full$TAFD)
})


test_that("add_tafd() keeps the row order", {
  fixture <- time_fixture()
  perm <- c(11, 3, 5, 1, 9, 2, 10, 4, 7, 6, 8)

  result <- nif:::add_tafd(as_nif_test(fixture[perm, ]))

  expect_equal(result$TIME, fixture$TIME[perm])
  expect_equal(result$TAFD, time_fixture_expected()$TAFD[perm])
})


test_that("time functions return empty nif objects for empty input", {
  empty <- as_nif_test(
    tibble::tibble(
      ID = numeric(0), TIME = numeric(0), EVID = numeric(0),
      PARENT = character(0), ANALYTE = character(0), AMT = numeric(0),
      DV = numeric(0), CMT = numeric(0)
    )
  )

  expect_equal(nrow(nif:::add_tad(empty)), 0)
  expect_true("TAD" %in% names(nif:::add_tad(empty)))
  expect_equal(nrow(nif:::add_tafd(empty)), 0)
  expect_true("TAFD" %in% names(nif:::add_tafd(empty)))
  expect_true(all(c("TAFD", "TAD") %in% names(nif:::make_time_from_time(empty))))
  expect_s3_class(nif:::add_tad(empty), "nif")
})
