test_that("gather_duplicates returns nif object", {
  # CMT must vary within ID so it is not treated as baseline (avoid CMT.x/CMT.y from join)
  obj <- tibble::tribble(
    ~ID,   ~USUBJID, ~ANALYTE, ~EVID, ~DOSE, ~AMT, ~TIME, ~NTIME, ~DV,  ~CMT,
    1,     "1",      "DRUG",   0,     100,   0,    0,     0,      10,   1,
    1,     "1",      "DRUG",   0,     100,   0,    1,     1,      20,   2
  ) |>
    nif()

  result <- gather_duplicates(obj, silent = TRUE)
  expect_s3_class(result, "nif")
  expect_s3_class(result, "data.frame")
})


test_that("gather_duplicates collapses triplicates by NTIME with mean", {
  # CMT 2 and 3 so CMT varies per ID (not baseline)
  obj <- tibble::tribble(
    ~ID,   ~USUBJID, ~ANALYTE, ~EVID, ~DOSE, ~AMT, ~TIME, ~NTIME, ~DV,  ~CMT,
    1,     "1",      "QT",     0,     100,   0,    0,     0,      450,  2,
    1,     "1",      "QT",     0,     100,   0,    0.1,   0,      451,  2,
    1,     "1",      "QT",     0,     100,   0,    0.2,   0,      452,  2,
    1,     "1",      "QT",     0,     100,   0,    1,     1,      455,  2,
    1,     "1",      "QT",     0,     100,   0,    1.1,   1,      456,  2,
    1,     "1",      "QT",     0,     100,   0,    1.2,   1,      457,  2,
    1,     "1",      "HR",     0,     100,   0,    0,     0,      50,   3
  ) |>
    nif()

  result <- as.data.frame(gather_duplicates(obj, silent = TRUE))

  expect_equal(nrow(result), 3L)  # QT NTIME 0, QT NTIME 1, HR NTIME 0
  expect_equal(result$DV[result$ANALYTE == "QT" & result$NTIME == 0], 451)   # mean(450, 451, 452)
  expect_equal(result$DV[result$ANALYTE == "QT" & result$NTIME == 1], 456)   # mean(455, 456, 457)
  expect_equal(result$DV[result$ANALYTE == "HR"], 50)
  expect_true(all(c("ID", "ANALYTE", "CMT", "EVID", "AMT", "NTIME", "DV") %in% names(result)))
})


test_that("gather_duplicates works with no duplicates", {
  # CMT 2 and 3 so CMT varies per ID
  obj <- tibble::tribble(
    ~ID,   ~USUBJID, ~ANALYTE, ~EVID, ~DOSE, ~AMT, ~TIME, ~NTIME, ~DV,  ~CMT,
    1,     "1",      "QT",     0,     100,   0,    0,     0,      450,  2,
    1,     "1",      "QT",     0,     100,   0,    1,     1,      455,  2,
    1,     "1",      "QT",     0,     100,   0,    2,     2,      460,  3
  ) |>
    nif()

  result <- as.data.frame(gather_duplicates(obj, silent = TRUE))
  expect_equal(nrow(result), 3L)
  expect_equal(result$DV, c(450, 455, 460))
})

test_that("gather_duplicates works with custom duplicate_function (sum)", {
  # CMT 1 and 2 so CMT varies per ID
  obj <- tibble::tribble(
    ~ID,   ~USUBJID, ~ANALYTE, ~EVID, ~DOSE, ~AMT, ~TIME, ~NTIME, ~DV,  ~CMT,
    1,     "1",      "X",      0,     100,   0,    0,     0,      10,   1,
    1,     "1",      "X",      0,     100,   0,    0.1,   0,      20,   1,
    1,     "1",      "X",      0,     100,   0,    0.2,   0,      30,   1,
    1,     "1",      "Y",      0,     100,   0,    0,     0,      5,    2
  ) |>
    nif()

  result <- as.data.frame(gather_duplicates(obj, duplicate_function = sum, silent = TRUE))
  expect_equal(nrow(result), 2L)
  expect_equal(result$DV[result$ANALYTE == "X"], 60)
  expect_equal(result$DV[result$ANALYTE == "Y"], 5)
})

test_that("gather_duplicates works with custom duplicate_function (median)", {
  # CMT 1 and 2 so CMT varies per ID
  obj <- tibble::tribble(
    ~ID,   ~USUBJID, ~ANALYTE, ~EVID, ~DOSE, ~AMT, ~TIME, ~NTIME, ~DV,  ~CMT,
    1,     "1",      "X",      0,     100,   0,    0,     0,      10,   1,
    1,     "1",      "X",      0,     100,   0,    0.1,   0,      20,   1,
    1,     "1",      "X",      0,     100,   0,    0.2,   0,      30,   1,
    1,     "1",      "Y",      0,     100,   0,    0,     0,      99,   2
  ) |>
    nif()

  result <- as.data.frame(gather_duplicates(obj, duplicate_function = median, silent = TRUE))
  expect_equal(nrow(result), 2L)
  expect_equal(result$DV[result$ANALYTE == "X"], 20)
  expect_equal(result$DV[result$ANALYTE == "Y"], 99)
})

test_that("gather_duplicates with na_rm = TRUE uses only non-NA values", {
  # With na_rm=TRUE, one NA in a group is ignored: mean(10, NA, 30) = 20. Two groups (ANALYTE X and Y), CMT varies.
  obj <- tibble::tribble(
    ~ID,   ~USUBJID, ~ANALYTE, ~EVID, ~DOSE, ~AMT, ~TIME, ~NTIME, ~DV,  ~CMT,
    1,     "1",      "X",      0,     100,   0,    0,     0,      10,   1,
    1,     "1",      "X",      0,     100,   0,    0.1,   0,      NA,   1,
    1,     "1",      "X",      0,     100,   0,    0.2,   0,      30,   1,
    1,     "1",      "Y",      0,     100,   0,    1,     1,      7,    2
  ) |>
    nif()

  result <- as.data.frame(gather_duplicates(obj, na_rm = TRUE, silent = TRUE))
  expect_equal(nrow(result), 2L)
  expect_equal(result$DV[result$ANALYTE == "X"], 20)   # mean(10, 30)
  expect_equal(result$DV[result$ANALYTE == "Y"], 7)
})


test_that("gather_duplicates returns NA (not NaN) when all DV in group are NA with na_rm = TRUE", {
  # CMT 1 and 2 so CMT varies per ID; all-NA group in CMT=1
  obj <- tibble::tribble(
    ~ID,   ~USUBJID, ~ANALYTE, ~EVID, ~DOSE, ~AMT, ~TIME, ~NTIME, ~DV,  ~CMT,
    1,     "1",      "X",      0,     100,   0,    0,     0,      NA,   1,
    1,     "1",      "X",      0,     100,   0,    0.1,   0,      NA,   1,
    1,     "1",      "X",      0,     100,   0,    0.2,   0,      NA,   1,
    1,     "1",      "Y",      0,     100,   0,    0,     0,      5,    2
  ) |>
    nif()

  result <- as.data.frame(gather_duplicates(obj, na_rm = TRUE, silent = TRUE))

  expect_equal(nrow(result), 2L)
  expect_true(is.na(result$DV[result$ANALYTE == "X"]))
  expect_false(is.nan(result$DV[result$ANALYTE == "X"]))
  expect_equal(result$DV[result$ANALYTE == "Y"], 5)
})


test_that("gather_duplicates excludes MDV == 1 rows before gathering", {
  # After dropping MDV=1, CMT 1 and 2 remain so CMT varies per ID
  obj <- tibble::tribble(
    ~ID,   ~USUBJID, ~ANALYTE, ~EVID, ~DOSE, ~AMT, ~TIME, ~NTIME, ~DV,  ~CMT, ~MDV,
    1,     "1",      "X",      0,     100,   0,    0,     0,      10,   1,    0,
    1,     "1",      "X",      0,     100,   0,    0.1,   0,      20,   1,    0,
    1,     "1",      "X",      1,     100,   100,  0,     0,      999,   0,    1,
    1,     "1",      "Y",      0,     100,   0,    0,     0,      100,  2,    0
  ) |>
    nif()

  result <- as.data.frame(gather_duplicates(obj, silent = TRUE))
  # Only MDV=0 rows: CMT=1 triplicate -> mean 15, CMT=2 single -> 100
  expect_equal(nrow(result), 2L)
  expect_equal(result$DV[result$CMT == 1], 15)
  expect_equal(result$DV[result$CMT == 2], 100)
})

test_that("gather_duplicates with custom id_field", {
  # id_field = "TIME": duplicates at same TIME are collapsed. CMT 1 and 2 so CMT varies per ID
  obj2 <- tibble::tribble(
    ~ID,   ~USUBJID, ~ANALYTE, ~EVID, ~DOSE, ~AMT, ~TIME, ~NTIME, ~DV,  ~CMT,
    1,     "1",      "X",      0,     100,   0,    0,     0,      10,   1,
    1,     "1",      "X",      0,     100,   0,    0,     0,      20,   1,
    1,     "1",      "X",      0,     100,   0,    1,     1,      30,   1,
    1,     "1",      "Y",      0,     100,   0,    0,     0,      8,    2
  ) |>
    nif()

  result <- as.data.frame(gather_duplicates(obj2, id_field = "TIME", silent = TRUE))
  expect_equal(nrow(result), 3L)  # X TIME 0 (collapsed), X TIME 1, Y TIME 0
  expect_equal(result$DV[result$ANALYTE == "X" & result$TIME == 0], 15)
  expect_equal(result$DV[result$ANALYTE == "X" & result$TIME == 1], 30)
  expect_equal(result$DV[result$ANALYTE == "Y"], 8)
})

test_that("gather_duplicates errors when obj is not a nif object", {
  df <- tibble::tribble(
    ~ID,   ~TIME, ~AMT, ~CMT, ~EVID, ~DV,  ~ANALYTE, ~NTIME,
    1,     0,     0,    1,    0,     10,   "X",      0
  )

  expect_error(
    gather_duplicates(df, silent = TRUE),
    "Input must be a nif object"
  )
})

test_that("gather_duplicates errors when id_field column is missing", {
  # NTIME missing; default id_field = "NTIME". CMT 1 and 2 so CMT varies per ID
  obj <- tibble::tribble(
    ~ID,   ~USUBJID, ~ANALYTE, ~EVID, ~DOSE, ~AMT, ~TIME, ~DV,  ~CMT,
    1,     "1",      "X",      0,     100,   0,    0,     10,   1,
    1,     "1",      "X",      0,     100,   0,    1,     20,   2
  ) |>
    nif()

  expect_error(
    gather_duplicates(obj, silent = TRUE),
    "not found in nif object"
  )
})

test_that("gather_duplicates errors when duplicate_function is not a function", {
  # Two rows so CMT varies (1 and 2); error happens before join
  obj <- tibble::tribble(
    ~ID,   ~USUBJID, ~ANALYTE, ~EVID, ~DOSE, ~AMT, ~TIME, ~NTIME, ~DV,  ~CMT,
    1,     "1",      "X",      0,     100,   0,    0,     0,      10,   1,
    1,     "1",      "Y",      0,     100,   0,    0,     0,      20,   2
  ) |>
    nif()

  expect_error(
    gather_duplicates(obj, duplicate_function = "mean", silent = TRUE),
    "duplicate_function must be a function"
  )

  expect_error(
    gather_duplicates(obj, duplicate_function = 123L, silent = TRUE),
    "duplicate_function must be a function"
  )
})

test_that("gather_duplicates errors when baseline is inconsistent per ID", {
  # USUBJID = "1", "1", NA for ID 1: n_distinct(na.rm=TRUE)=1 so USUBJID in bl_col, but distinct(ID, USUBJID) has 2 rows
  obj <- tibble::tribble(
    ~ID,   ~USUBJID, ~ANALYTE, ~EVID, ~DOSE, ~AMT, ~TIME, ~NTIME, ~DV,  ~CMT,
    1,     "1",      "X",      0,     100,   0,    0,     0,      10,   1,
    1,     "1",      "X",      0,     100,   0,    0.1,   0,      20,   1,
    1,     NA_character_, "X", 0,    100,   0,    0.2,   0,      30,   1
  ) |>
    nif()

  expect_error(
    gather_duplicates(obj, silent = TRUE),
    "Baseline .* must be unique per ID"
  )
})

test_that("gather_duplicates works with multiple ANALYTEs", {
  obj <- tibble::tribble(
    ~ID,   ~USUBJID, ~ANALYTE, ~EVID, ~DOSE, ~AMT, ~TIME, ~NTIME, ~DV,  ~CMT,
    1,     "1",      "QT",     0,     100,   0,    0,     0,      450,  2,
    1,     "1",      "QT",     0,     100,   0,    0.1,   0,      451,  2,
    1,     "1",      "QT",     0,     100,   0,    0.2,   0,      452,  2,
    1,     "1",      "HR",     0,     100,   0,    0,     0,      50,   3,
    1,     "1",      "HR",     0,     100,   0,    0.1,   0,      51,   3,
    1,     "1",      "HR",     0,     100,   0,    0.2,   0,      52,   3
  ) |>
    nif()

  result <- as.data.frame(gather_duplicates(obj, silent = TRUE))

  expect_equal(nrow(result), 2L)
  qt_row <- result[result$ANALYTE == "QT", ]
  hr_row <- result[result$ANALYTE == "HR", ]
  expect_equal(qt_row$DV, 451)
  expect_equal(hr_row$DV, 51)
})

test_that("gather_duplicates works with multiple IDs", {
  # CMT 1 and 2 per ID so CMT varies within each ID
  obj <- tibble::tribble(
    ~ID,   ~USUBJID, ~ANALYTE, ~EVID, ~DOSE, ~AMT, ~TIME, ~NTIME, ~DV,  ~CMT,
    1,     "1",      "X",      0,     100,   0,    0,     0,      10,   1,
    1,     "1",      "X",      0,     100,   0,    0.1,   0,      20,   1,
    1,     "1",      "Y",      0,     100,   0,    0,     0,      1,    2,
    2,     "2",      "X",      0,     100,   0,    0,     0,      30,   1,
    2,     "2",      "X",      0,     100,   0,    0.1,   0,      40,   1,
    2,     "2",      "Y",      0,     100,   0,    0,     0,      2,    2
  ) |>
    nif()

  result <- as.data.frame(gather_duplicates(obj, silent = TRUE))

  expect_equal(nrow(result), 4L)  # ID 1 X, ID 1 Y, ID 2 X, ID 2 Y
  expect_equal(result$DV[result$ID == 1 & result$ANALYTE == "X"], 15)
  expect_equal(result$DV[result$ID == 1 & result$ANALYTE == "Y"], 1)
  expect_equal(result$DV[result$ID == 2 & result$ANALYTE == "X"], 35)
  expect_equal(result$DV[result$ID == 2 & result$ANALYTE == "Y"], 2)
})

test_that("gather_duplicates accepts silent = TRUE and silent = NULL", {
  # CMT 1 and 2 so CMT varies per ID
  obj <- tibble::tribble(
    ~ID,   ~USUBJID, ~ANALYTE, ~EVID, ~DOSE, ~AMT, ~TIME, ~NTIME, ~DV,  ~CMT,
    1,     "1",      "X",      0,     100,   0,    0,     0,      10,   1,
    1,     "1",      "X",      0,     100,   0,    0.1,   0,      20,   1,
    1,     "1",      "Y",      0,     100,   0,    0,     0,      5,    2
  ) |>
    nif()

  expect_no_error(gather_duplicates(obj, silent = TRUE))
  expect_message(expect_no_error(gather_duplicates(obj, silent = NULL)))
})

test_that("gather_duplicates with dosing row (EVID=1) and observations", {
  # Realistic: one dosing row, then triplicate observations at NTIME 0 (CMT 1 and 2 so CMT varies per ID)
  obj <- tibble::tribble(
    ~ID,   ~USUBJID, ~ANALYTE, ~EVID, ~DOSE, ~AMT, ~TIME, ~NTIME, ~DV,  ~CMT,
    1,     "1",      "DRUG",   1,     100,   100,  0,     1,      0,    0,
    1,     "1",      "DRUG",   0,     100,   0,    -1,    0,      0,    1,
    1,     "1",      "QT",     0,     100,   0,    0,     0,      450,  2,
    1,     "1",      "QT",     0,     100,   0,    0.1,   0,      451,  2,
    1,     "1",      "QT",     0,     100,   0,    0.2,   0,      452,  2
  ) |>
    nif()

  result <- as.data.frame(gather_duplicates(obj, silent = TRUE))

  # Triplicate at (ID=1, ANALYTE=QT, CMT=2, NTIME=0) collapsed to mean DV = 451
  expect_equal(result$DV[result$CMT == 2], 451)
})

test_that("gather_duplicates validates na_rm is logical", {
  # Two rows so CMT can vary (1 and 2)
  obj <- tibble::tribble(
    ~ID,   ~USUBJID, ~ANALYTE, ~EVID, ~DOSE, ~AMT, ~TIME, ~NTIME, ~DV,  ~CMT,
    1,     "1",      "X",      0,     100,   0,    0,     0,      10,   1,
    1,     "1",      "Y",      0,     100,   0,    0,     0,      20,   2
  ) |>
    nif()

  expect_error(
    gather_duplicates(obj, na_rm = 1L, silent = TRUE),
    "must be a logical value"
  )
})

