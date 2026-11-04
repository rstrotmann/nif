## Tests for the tie-break convention for records with identical time keys:
## a sample (EVID 0) at the same time as a dose (EVID 1) is a trough, it is
## ordered before the dose and relates to the previous dose.


test_that("arrange_events() puts samples before doses at identical times", {
  df <- tibble::tribble(
    ~ID, ~TIME, ~EVID, ~DV,
    1,   12,    1,     NA,
    1,   12,    0,     5,
    1,   0,     1,     NA,
    1,   0,     0,     0
  )

  result <- nif:::arrange_events(df, ID, TIME)

  expect_equal(result$TIME, c(0, 0, 12, 12))
  expect_equal(result$EVID, c(0, 1, 0, 1))
  expect_equal(result$DV, c(0, NA, 5, NA))
})


test_that("arrange_events() sorts by the time keys before EVID", {
  df <- tibble::tribble(
    ~ID, ~TIME, ~EVID,
    1,   1,     0,
    1,   0,     1,
    1,   2,     1,
    1,   2,     0
  )

  result <- nif:::arrange_events(df, ID, TIME)

  expect_equal(result$TIME, c(0, 1, 2, 2))
  expect_equal(result$EVID, c(1, 0, 0, 1))
})


test_that("arrange_events() works without an EVID column", {
  df <- tibble::tribble(
    ~ID, ~TIME,
    2,   1,
    1,   2,
    1,   0
  )

  result <- nif:::arrange_events(df, ID, TIME)

  expect_equal(result$ID, c(1, 1, 2))
  expect_equal(result$TIME, c(0, 2, 1))
})


test_that("arrange_events() does not depend on the input order", {
  df <- tibble::tribble(
    ~ID, ~TIME, ~EVID, ~DV,
    1,   0,     0,     1,
    1,   0,     1,     NA,
    1,   12,    0,     2,
    1,   12,    1,     NA,
    1,   24,    0,     3
  )
  expected <- nif:::arrange_events(df, ID, TIME)

  set.seed(1)
  for (i in 1:20) {
    shuffled <- df[sample(nrow(df)), ]
    expect_equal(nif:::arrange_events(shuffled, ID, TIME), expected)
  }
})


test_that("nif() orders a trough before the dose and TAD agrees", {
  test_data <- tibble::tribble(
    ~ID, ~USUBJID, ~DTC,                  ~TIME, ~EVID, ~PARENT, ~ANALYTE, ~AMT, ~DV, ~CMT,
    1,   "S1",     "2024-01-01 20:00:00", 12,    1,     "A",     "A",      100,  NA,  1,
    1,   "S1",     "2024-01-01 08:00:00", 0,     1,     "A",     "A",      100,  NA,  1,
    1,   "S1",     "2024-01-01 22:00:00", 14,    0,     "A",     "A",      0,    9,   2,
    1,   "S1",     "2024-01-01 20:00:00", 12,    0,     "A",     "A",      0,    5,   2
  ) |>
    dplyr::mutate(DTC = as.POSIXct(DTC, tz = "UTC"))

  result <- nif(test_data, silent = TRUE) |>
    add_tad() |>
    as.data.frame()

  # the trough at 12 h is listed before the dose at 12 h ...
  expect_equal(result$TIME, c(0, 12, 12, 14))
  expect_equal(result$EVID, c(1, 0, 1, 0))

  # ... and relates to the previous dose, i.e. TAD is 12 and not 0
  expect_equal(result$TAD, c(0, 12, 0, 2))
})


test_that("the written NONMEM file lists the trough before the dose", {
  test_data <- tibble::tribble(
    ~ID, ~TIME, ~EVID, ~PARENT, ~ANALYTE, ~AMT, ~DV, ~CMT,
    1,   12,    1,     "A",     "A",      100,  NA,  1,
    1,   0,     1,     "A",     "A",      100,  NA,  1,
    1,   12,    0,     "A",     "A",      0,    5,   2,
    1,   13,    0,     "A",     "A",      0,    9,   2
  ) |>
    nif(silent = TRUE)

  file <- tempfile(fileext = ".csv")
  on.exit(unlink(file))
  write_nonmem(
    test_data, filename = file, fields = c("ID", "TIME", "EVID", "AMT", "DV")
  )
  lines <- readLines(file)

  # locate the columns by name, the writer decides on the column order
  written <- utils::read.table(
    text = lines, header = TRUE, na.strings = ".", stringsAsFactors = FALSE
  )
  evid_at_12 <- written$EVID[written$TIME == 12]

  # sample (EVID 0) first, then dose (EVID 1)
  expect_equal(evid_at_12, c(0, 1))
})


test_that("ensure_parent() relates a trough to the previous dose", {
  test_data <- tibble::tribble(
    ~ID, ~TIME, ~EVID, ~ANALYTE, ~AMT, ~DV, ~CMT,
    1,   0,     1,     "A",      100,  NA,  1,
    1,   12,    1,     "B",      50,   NA,  3,
    1,   12,    0,     "A",      0,    2,   2,
    1,   24,    0,     "A",      0,    1,   2
  ) |>
    nif(silent = TRUE)

  result <- as.data.frame(nif:::ensure_parent(test_data))

  # the sample at 12 h comes before the dose of B and has parent A
  expect_equal(result$EVID, c(1, 0, 1, 0))
  expect_equal(result$PARENT, c("A", "A", "B", "B"))
})


test_that("add_time_deviation() gives a trough with NTIME 0 a deviation of 0", {
  test_data <- tibble::tribble(
    ~ID, ~USUBJID, ~DTC,                  ~TIME, ~NTIME, ~EVID, ~PARENT, ~ANALYTE, ~AMT, ~DV, ~CMT,
    1,   "S1",     "2024-01-01 08:00:00", 0,     0,      1,     "A",     "A",      100,  NA,  1,
    1,   "S1",     "2024-01-01 20:00:00", 12,    0,      1,     "A",     "A",      100,  NA,  1,
    1,   "S1",     "2024-01-01 20:00:00", 12,    0,      0,     "A",     "A",      0,    5,   2,
    1,   "S1",     "2024-01-01 22:00:00", 14,    2,      0,     "A",     "A",      0,    9,   2
  ) |>
    dplyr::mutate(DTC = as.POSIXct(DTC, tz = "UTC"))

  result <- nif(test_data, silent = TRUE) |>
    add_tad() |>
    add_time_deviation(silent = TRUE) |>
    as.data.frame()

  expect_equal(result$EVID, c(1, 0, 1, 0))
  expect_equal(result$TIME_DEV[2], 0)
})
