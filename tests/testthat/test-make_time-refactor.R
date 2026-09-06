## Characterization tests for make_time(), add_tad(), and make_time_from_time()
##
## Pins TIME / TAFD / TAD, nif class, and creation_date so those helpers can
## stop calling as.data.frame() and nif().

add_tad <- nif:::add_tad
make_time_from_time <- nif:::make_time_from_time


utc <- function(x) {
  as.POSIXct(x, tz = "UTC")
}


nif_with_date <- function(df, creation_date = as.Date("2020-01-15")) {
  nif:::new_nif(df, creation_date = creation_date)
}


# ---- make_time ---------------------------------------------------------------

test_that("make_time sets TIME from the subject's first DTC", {
  obj <- nif_with_date(tibble::tribble(
     ~ID,                   ~DTC, ~AMT, ~CMT, ~EVID, ~DV, ~ANALYTE, ~PARENT,
       1, utc("2023-01-01 08:00"),  100,    1,     1,  NA,   "DRUG",  "DRUG",
       1, utc("2023-01-01 10:00"),    0,    2,     0,  10,   "DRUG",  "DRUG",
       2, utc("2023-01-01 12:00"),  100,    1,     1,  NA,   "DRUG",  "DRUG",
       2, utc("2023-01-01 15:00"),    0,    2,     0,  20,   "DRUG",  "DRUG"
  ))

  result <- make_time(obj)

  expect_equal(result$TIME, c(0, 2, 0, 3))
})


test_that("make_time sets TAFD from the first dose of each PARENT", {
  obj <- nif_with_date(tibble::tribble(
     ~ID,                   ~DTC, ~AMT, ~CMT, ~EVID, ~DV, ~ANALYTE, ~PARENT,
       1, utc("2023-01-01 07:00"),    0,    2,     0,   5,  "DRUG1", "DRUG1",
       1, utc("2023-01-01 08:00"),  100,    1,     1,  NA,  "DRUG1", "DRUG1",
       1, utc("2023-01-01 10:00"),  100,    3,     1,  NA,  "DRUG2", "DRUG2",
       1, utc("2023-01-01 12:00"),    0,    4,     0,  20,  "DRUG2", "DRUG2"
  ))

  result <- make_time(obj)

  expect_equal(result$TAFD, c(-1, 0, 0, 2))
})


test_that("make_time sets TAD from the most recent dose of each PARENT", {
  obj <- nif_with_date(tibble::tribble(
     ~ID,                   ~DTC, ~AMT, ~CMT, ~EVID, ~DV, ~ANALYTE, ~PARENT,
       1, utc("2023-01-01 08:00"),  100,    1,     1,  NA,   "DRUG",  "DRUG",
       1, utc("2023-01-01 09:00"),    0,    2,     0,  10,   "DRUG",  "DRUG",
       1, utc("2023-01-01 11:00"),  100,    1,     1,  NA,   "DRUG",  "DRUG",
       1, utc("2023-01-01 12:00"),    0,    2,     0,  15,   "DRUG",  "DRUG"
  ))

  result <- make_time(obj)

  expect_equal(result$TAD, c(0, 1, 0, 1))
})


test_that("make_time uses negative TAD and TAFD for predose rows", {
  obj <- nif_with_date(tibble::tribble(
     ~ID,                   ~DTC, ~AMT, ~CMT, ~EVID, ~DV, ~ANALYTE, ~PARENT,
       1, utc("2023-01-01 07:00"),    0,    2,     0,   5,   "DRUG",  "DRUG",
       1, utc("2023-01-01 08:00"),    0,    2,     0,   8,   "DRUG",  "DRUG",
       1, utc("2023-01-01 09:00"),  100,    1,     1,  NA,   "DRUG",  "DRUG",
       1, utc("2023-01-01 10:00"),    0,    2,     0,  10,   "DRUG",  "DRUG"
  ))

  result <- make_time(obj)

  expect_equal(result$TIME, c(0, 1, 2, 3))
  expect_equal(result$TAFD, c(-2, -1, 0, 1))
  expect_equal(result$TAD, c(-2, -1, 0, 1))
})


test_that("make_time leaves TAFD and TAD as NA when a parent has no dose", {
  obj <- nif_with_date(tibble::tribble(
     ~ID,                   ~DTC, ~AMT, ~CMT, ~EVID, ~DV, ~ANALYTE, ~PARENT,
       1, utc("2023-01-01 08:00"),    0,    2,     0,   5,   "DRUG",  "DRUG",
       1, utc("2023-01-01 09:00"),    0,    2,     0,  10,   "DRUG",  "DRUG"
  ))

  result <- make_time(obj)

  expect_equal(result$TIME, c(0, 1))
  expect_equal(result$TAFD, c(NA_real_, NA_real_))
  expect_equal(result$TAD, c(NA_real_, NA_real_))
})


test_that("make_time keeps original row order", {
  obj <- nif_with_date(tibble::tribble(
     ~ID,                   ~DTC, ~AMT, ~CMT, ~EVID, ~DV, ~ANALYTE, ~PARENT,
       1, utc("2023-01-01 10:00"),    0,    2,     0,  20,  "DRUG2", "DRUG2",
       1, utc("2023-01-01 08:00"),  100,    1,     1,  NA,  "DRUG1", "DRUG1",
       1, utc("2023-01-01 09:00"),  100,    3,     1,  NA,  "DRUG2", "DRUG2"
  ))

  result <- make_time(obj)

  expect_equal(result$PARENT, c("DRUG2", "DRUG1", "DRUG2"))
  expect_equal(result$TIME, c(2, 0, 1))
  expect_equal(result$TAD, c(1, 0, 0))
})


test_that("make_time keeps nif class and creation_date", {
  obj <- nif_with_date(tibble::tribble(
     ~ID,                   ~DTC, ~AMT, ~CMT, ~EVID, ~DV, ~ANALYTE, ~PARENT,
       1, utc("2023-01-01 08:00"),  100,    1,     1,  NA,   "DRUG",  "DRUG",
       1, utc("2023-01-01 09:00"),    0,    2,     0,  10,   "DRUG",  "DRUG"
  ))

  result <- make_time(obj)

  expect_s3_class(result, "nif")
  expect_s3_class(result, "tbl_df")
  expect_equal(attr(result, "creation_date"), as.Date("2020-01-15"))
  expect_false(dplyr::is_grouped_df(result))
  expect_false(
    any(c("FIRSTDTC", "FIRSTADMIN", "admin_time", ".row") %in% names(result))
  )
})


test_that("make_time adds empty TIME, TAFD, and TAD on empty input", {
  obj <- nif_with_date(tibble::tibble(
    ID      = integer(),
    DTC     = as.POSIXct(character(), tz = "UTC"),
    AMT     = numeric(),
    CMT     = numeric(),
    EVID    = integer(),
    DV      = numeric(),
    ANALYTE = character(),
    PARENT  = character(),
    TIME    = numeric()
  ))

  result <- make_time(obj)

  expect_equal(nrow(result), 0)
  expect_true(all(c("TIME", "TAFD", "TAD") %in% names(result)))
  expect_s3_class(result, "nif")
  expect_equal(attr(result, "creation_date"), as.Date("2020-01-15"))
})


# ---- add_tad -----------------------------------------------------------------

test_that("add_tad is hours since the most recent parent dose", {
  obj <- nif(tibble::tribble(
     ~ID, ~TIME, ~AMT, ~CMT, ~EVID, ~DV, ~PARENT,
       1,     0,  100,    1,     1,  NA,  "DRUG",
       1,     1,    0,    2,     0,  10,  "DRUG",
       1,     3,  100,    1,     1,  NA,  "DRUG",
       1,     5,    0,    2,     0,  20,  "DRUG"
  ))

  result <- add_tad(obj)

  expect_equal(result$TAD, c(0, 1, 0, 2))
})


test_that("add_tad is negative before the first dose", {
  obj <- nif(tibble::tribble(
     ~ID, ~TIME, ~AMT, ~CMT, ~EVID, ~DV, ~PARENT,
       1,    -2,    0,    2,     0,   5,  "DRUG",
       1,     0,  100,    1,     1,  NA,  "DRUG",
       1,     1,    0,    2,     0,  10,  "DRUG"
  ))

  result <- add_tad(obj)

  expect_equal(result$TAD, c(-2, 0, 1))
})


test_that("add_tad does not mix doses across PARENT or ID", {
  obj <- nif_with_date(tibble::tribble(
     ~ID, ~TIME, ~AMT, ~CMT, ~EVID, ~DV, ~PARENT,
       1,     0,  100,    1,     1,  NA, "DRUG1",
       1,     2,    0,    2,     0,  10, "DRUG1",
       1,     0,   50,    3,     1,  NA, "DRUG2",
       1,     4,    0,    4,     0,  20, "DRUG2",
       2,     0,  100,    1,     1,  NA, "DRUG1",
       2,     3,    0,    2,     0,  30, "DRUG1"
  ))

  result <- add_tad(obj)

  expect_equal(result$TAD, c(0, 2, 0, 4, 0, 3))
})


test_that("add_tad at a shared TIME attributes the observation to the earlier dose", {
  obj <- nif_with_date(tibble::tribble(
     ~ID, ~TIME, ~AMT, ~CMT, ~EVID, ~DV, ~PARENT,
       1,     0,  100,    1,     1,  NA,  "DRUG",
       1,     1,    0,    2,     0,  10,  "DRUG",
       1,     1,  100,    1,     1,  NA,  "DRUG"
  ))

  result <- add_tad(obj)

  expect_equal(result$TAD, c(0, 1, 0))
})


test_that("add_tad keeps original row order, class, and creation_date", {
  obj <- nif_with_date(tibble::tribble(
     ~ID, ~TIME, ~AMT, ~CMT, ~EVID, ~DV, ~PARENT,
       1,     2,    0,    2,     0,  20,  "DRUG",
       1,     0,  100,    1,     1,  NA,  "DRUG",
       1,     1,    0,    2,     0,  10,  "DRUG"
  ))

  result <- add_tad(obj)

  expect_equal(result$TIME, c(2, 0, 1))
  expect_equal(result$TAD, c(2, 0, 1))
  expect_s3_class(result, "nif")
  expect_equal(attr(result, "creation_date"), as.Date("2020-01-15"))
  expect_false(dplyr::is_grouped_df(result))
  expect_false(any(c("admin_time", ".row") %in% names(result)))
})


test_that("add_tad is NA where TIME is NA", {
  obj <- nif_with_date(tibble::tribble(
     ~ID, ~TIME, ~AMT, ~CMT, ~EVID, ~DV, ~PARENT,
       1,     0,  100,    1,     1,  NA,  "DRUG",
       1,    NA,    0,    2,     0,  10,  "DRUG",
       1,     2,    0,    2,     0,  20,  "DRUG"
  ))

  result <- add_tad(obj)

  expect_equal(result$TAD[1], 0)
  expect_true(is.na(result$TAD[2]))
  expect_equal(result$TAD[3], 2)
})


# ---- make_time_from_time -----------------------------------------------------

test_that("make_time_from_time derives TAFD and TAD from TIME", {
  obj <- nif(tibble::tribble(
     ~ID, ~TIME, ~AMT, ~CMT, ~EVID, ~DV, ~ANALYTE, ~PARENT,
       1,    -1,    0,    2,     0,   5,   "DRUG",  "DRUG",
       1,     0,  100,    1,     1,  NA,   "DRUG",  "DRUG",
       1,     2,    0,    2,     0,  10,   "DRUG",  "DRUG",
       1,     3,  100,    1,     1,  NA,   "DRUG",  "DRUG",
       1,     4,    0,    2,     0,  15,   "DRUG",  "DRUG"
  ))

  result <- make_time_from_time(obj)

  expect_equal(result$TIME, c(-1, 0, 2, 3, 4))
  expect_equal(result$TAFD, c(-1, 0, 2, 3, 4))
  expect_equal(result$TAD, c(-1, 0, 2, 0, 1))
})


test_that("make_time_from_time keeps original row order", {
  obj <- nif_with_date(tibble::tribble(
     ~ID, ~TIME, ~AMT, ~CMT, ~EVID, ~DV, ~ANALYTE, ~PARENT,
       1,     3,    0,    2,     0,  15,   "DRUG",  "DRUG",
       1,     0,  100,    1,     1,  NA,   "DRUG",  "DRUG",
       1,     1,    0,    2,     0,  10,   "DRUG",  "DRUG"
  ))

  result <- make_time_from_time(obj)

  expect_equal(result$TIME, c(3, 0, 1))
  expect_equal(result$TAFD, c(3, 0, 1))
  expect_equal(result$TAD, c(3, 0, 1))
})


test_that("make_time_from_time keeps nif class and creation_date", {
  obj <- nif_with_date(tibble::tribble(
     ~ID, ~TIME, ~AMT, ~CMT, ~EVID, ~DV, ~ANALYTE, ~PARENT,
       1,     0,  100,    1,     1,  NA,   "DRUG",  "DRUG",
       1,     1,    0,    2,     0,  10,   "DRUG",  "DRUG"
  ))

  result <- make_time_from_time(obj)

  expect_s3_class(result, "nif")
  expect_equal(attr(result, "creation_date"), as.Date("2020-01-15"))
  expect_false(dplyr::is_grouped_df(result))
  expect_false(
    any(c(".first_admin", ".admin_time", ".row") %in% names(result))
  )
})
