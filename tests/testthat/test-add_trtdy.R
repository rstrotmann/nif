## Tests for add_trtdy
##
## TRTDY is calendar days from the subject's first administration DTC date,
## with day of first dose = 1 (negative days before first dose are unchanged).


test_that("add_trtdy adds TRTDY and preserves nif class", {
  nif_obj <- tibble::tribble(
    ~ID, ~TIME, ~AMT, ~CMT, ~EVID, ~DV,                    ~DTC,
      1,     0,  100,    1,     1,  NA, "2024-12-06 08:00:00",
      1,     1,    0,    2,     0,  10, "2024-12-06 09:00:00",
      1,    24,  100,    1,     1,  NA, "2024-12-07 08:00:00"
  ) |>
    dplyr::mutate(DTC = as.POSIXct(.data$DTC, tz = "UTC")) |>
    nif()

  result <- add_trtdy(nif_obj)

  expect_s3_class(result, "nif")
  expect_true("TRTDY" %in% names(result))
  expect_false("FIRSTTRTDTC" %in% names(result))
})


test_that("add_trtdy sets day of first dose to 1", {
  nif_obj <- tibble::tribble(
    ~ID, ~TIME, ~AMT, ~CMT, ~EVID, ~DV,                    ~DTC,
      1,     0,  100,    1,     1,  NA, "2024-12-06 08:00:00",
      1,     1,    0,    2,     0,  10, "2024-12-06 09:00:00",
      1,     2,    0,    2,     0,  20, "2024-12-06 10:00:00"
  ) |>
    dplyr::mutate(DTC = as.POSIXct(.data$DTC, tz = "UTC")) |>
    nif()

  result <- add_trtdy(nif_obj)

  expect_equal(result$TRTDY, c(1, 1, 1))
})


test_that("add_trtdy increments by calendar day after first dose", {
  nif_obj <- tibble::tribble(
    ~ID, ~TIME, ~AMT, ~CMT, ~EVID, ~DV,                    ~DTC,
      1,     0,  100,    1,     1,  NA, "2024-12-06 08:00:00",
      1,     1,    0,    2,     0,  10, "2024-12-06 09:00:00",
      1,    24,  100,    1,     1,  NA, "2024-12-07 08:00:00",
      1,    48,    0,    2,     0,  20, "2024-12-08 08:00:00"
  ) |>
    dplyr::mutate(DTC = as.POSIXct(.data$DTC, tz = "UTC")) |>
    nif()

  result <- add_trtdy(nif_obj)

  expect_equal(result$TRTDY, c(1, 1, 2, 3))
})


test_that("add_trtdy keeps negative days for predose DTCs", {
  nif_obj <- tibble::tribble(
    ~ID, ~TIME, ~AMT, ~CMT, ~EVID, ~DV,                    ~DTC,
      1,   -24,    0,    2,     0,   1, "2024-12-05 08:00:00",
      1,     0,  100,    1,     1,  NA, "2024-12-06 08:00:00",
      1,     1,    0,    2,     0,  10, "2024-12-06 09:00:00"
  ) |>
    dplyr::mutate(DTC = as.POSIXct(.data$DTC, tz = "UTC")) |>
    nif()

  result <- add_trtdy(nif_obj)

  expect_equal(result$TRTDY, c(-1, 1, 1))
})


test_that("add_trtdy calculates TRTDY separately per subject", {
  nif_obj <- tibble::tribble(
    ~ID, ~TIME, ~AMT, ~CMT, ~EVID, ~DV,                    ~DTC,
      1,     0,  100,    1,     1,  NA, "2024-12-06 08:00:00",
      1,    24,    0,    2,     0,  10, "2024-12-07 08:00:00",
      2,     0,  100,    1,     1,  NA, "2024-12-08 08:00:00",
      2,    24,    0,    2,     0,  20, "2024-12-09 08:00:00"
  ) |>
    dplyr::mutate(DTC = as.POSIXct(.data$DTC, tz = "UTC")) |>
    nif()

  result <- add_trtdy(nif_obj)

  expect_equal(result$TRTDY[result$ID == 1], c(1, 2))
  expect_equal(result$TRTDY[result$ID == 2], c(1, 2))
})


test_that("add_trtdy uses the earliest administration DTC as day 1 anchor", {
  nif_obj <- tibble::tribble(
    ~ID, ~TIME, ~AMT, ~CMT, ~EVID, ~DV,                    ~DTC,
      1,    24,  100,    1,     1,  NA, "2024-12-07 08:00:00",
      1,     0,  100,    1,     1,  NA, "2024-12-06 08:00:00",
      1,    48,    0,    2,     0,  10, "2024-12-08 08:00:00"
  ) |>
    dplyr::mutate(DTC = as.POSIXct(.data$DTC, tz = "UTC")) |>
    nif()

  result <- add_trtdy(nif_obj) |>
    as.data.frame() |>
    dplyr::arrange(.data$TIME)

  expect_equal(result$TRTDY, c(1, 2, 3))
})


test_that("add_trtdy matches expected values from the historical fixture", {
  nif_obj <- tibble::tribble(
    ~ID, ~TIME, ~AMT, ~CMT, ~EVID,                        ~DTC, ~DV,
      1,     0,  100,    1,     1, "2024-12-06 07:31:35.14839",  NA,
      1,     0,   NA,    1,     0, "2024-12-06 07:31:35.14839",  10,
      1,     1,   NA,    2,     0, "2024-12-06 08:31:35.14839",  20,
      1,     2,   NA,    2,     0, "2024-12-06 09:31:35.14839",  30,
      1,    24,   90,    1,     1, "2024-12-07 07:31:35.14839",  NA,
      1,    48,   80,    1,     1, "2024-12-08 07:31:35.14839",  NA,
      1,    50,   NA,    2,     0, "2024-12-08 09:31:35.14839",  40,
      2,    10,  100,    1,     1, "2024-12-06 17:31:35.14839",  NA,
      2,    11,   NA,    2,     0, "2024-12-06 18:31:35.14839",  50,
      2,    12,   NA,    2,     0, "2024-12-06 19:31:35.14839",  60,
      2,    34,   90,    1,     1, "2024-12-07 17:31:35.14839",  NA,
      2,    58,   80,    1,     1, "2024-12-08 17:31:35.14839",  NA,
      2,    60,   NA,    2,     0, "2024-12-08 19:31:35.14839",  70
  ) |>
    dplyr::mutate(DTC = as.POSIXct(.data$DTC)) |>
    nif()

  expect_equal(
    add_trtdy(nif_obj)$TRTDY,
    c(1, 1, 1, 1, 2, 3, 3, 1, 1, 1, 2, 3, 3)
  )
})


test_that("add_trtdy validates that input is a nif object", {
  expect_error(
    add_trtdy(data.frame(
      ID = 1,
      TIME = 0,
      EVID = 1,
      DTC = as.POSIXct("2024-12-06")
    )),
    "nif object"
  )
})


test_that("add_trtdy works with examplinib data", {
  result <- add_trtdy(examplinib_poc_nif)

  expect_s3_class(result, "nif")
  expect_true("TRTDY" %in% names(result))
  expect_true(all(result$TRTDY[!is.na(result$TRTDY)] >= 1) ||
                any(result$TRTDY < 0, na.rm = TRUE))
})
