make_nca_summary_table_input <- function() {
  tibble::tribble(
    ~ID, ~DOSE, ~PPTESTCD, ~PPORRES, ~exclude,
      1,   100,    "cmax",       10,        NA,
      2,   100,    "cmax",       40,        NA,
      1,   100,    "tmax",        1,        NA,
      2,   100,    "tmax",        5,        NA,
      3,   200,    "cmax",       20,        NA,
      3,   200,    "tmax",        2,        NA,
      1,   100, "auclast",      100,        NA,
      2,   100, "auclast",      400,        NA,
      3,   200, "auclast",      200,        NA
  )
}


test_that("nca_summary_table returns expected wide structure", {
  result <- nca_summary_table(
    make_nca_summary_table_input(),
    parameters = c("cmax", "tmax", "auclast"),
    group = "DOSE",
    digits = 2
  )

  expect_s3_class(result, "data.frame")
  expect_true(all(c("DOSE", "n", "cmax", "tmax", "auclast") %in% names(result)))
  expect_equal(nrow(result), 2)
  expect_equal(sort(result$DOSE), c(100, 200))
  expect_true(is.character(result$cmax))
  expect_true(is.character(result$tmax))
})


test_that("nca_summary_table formats geomean parameters as center (geocv)", {
  result <- nca_summary_table(
    make_nca_summary_table_input(),
    parameters = "cmax",
    group = "DOSE",
    digits = 2
  ) |>
    as.data.frame()

  dose100 <- result[result$DOSE == 100, ]
  expect_equal(dose100$n, 2)
  expect_equal(
    dose100$cmax,
    paste0(
      as.character(round(PKNCA::geomean(c(10, 40), na.rm = TRUE), digits = 2)),
      " (",
      as.character(round(PKNCA::geocv(c(10, 40), na.rm = TRUE))),
      ")"
    )
  )
})


test_that("nca_summary_table formats median parameters as median (min; max)", {
  result <- nca_summary_table(
    make_nca_summary_table_input(),
    parameters = "tmax",
    group = "DOSE",
    digits = 2
  ) |>
    as.data.frame()

  dose100 <- result[result$DOSE == 100, ]
  expect_equal(dose100$tmax, "3 (1; 5)")

  dose200 <- result[result$DOSE == 200, ]
  expect_equal(dose200$tmax, "2 (2; 2)")
})


test_that("nca_summary_table formats SDTM TMAX as median-style", {
  nca_data <- tibble::tribble(
    ~DOSE, ~PPTESTCD, ~PPSTRESN,
      100,    "TMAX",         1,
      100,    "TMAX",         5,
      100,    "CMAX",        10,
      100,    "CMAX",        40
  )

  result <- nca_summary_table(
    nca_data,
    parameters = c("TMAX", "CMAX"),
    group = "DOSE",
    digits = 1
  ) |>
    as.data.frame()

  expect_equal(nrow(result), 1)
  expect_equal(result$TMAX, "3 (1; 5)")
  expect_match(result$CMAX, "^20 \\(")
})


test_that("nca_summary_table digits affects center but not geocv rounding", {
  nca_data <- tibble::tribble(
    ~DOSE, ~PPTESTCD, ~PPORRES,
      100,    "cmax",       10,
      100,    "cmax",       40
  )

  result_2 <- nca_summary_table(
    nca_data, parameters = "cmax", group = "DOSE", digits = 2
  )
  result_0 <- nca_summary_table(
    nca_data, parameters = "cmax", group = "DOSE", digits = 0
  )

  # geomean of 10 and 40 is 20 exactly; digits still applied to center
  expect_equal(result_2$cmax, "20 (127)")
  expect_equal(result_0$cmax, "20 (127)")

  # Non-integer geomean shows digits difference
  nca_data2 <- tibble::tribble(
    ~DOSE, ~PPTESTCD, ~PPORRES,
      100,    "cmax",       10,
      100,    "cmax",       30
  )
  r2 <- nca_summary_table(
    nca_data2, parameters = "cmax", group = "DOSE", digits = 2
  )
  r0 <- nca_summary_table(
    nca_data2, parameters = "cmax", group = "DOSE", digits = 0
  )
  expect_equal(
    r2$cmax,
    paste0(
      as.character(round(PKNCA::geomean(c(10, 30), na.rm = TRUE), 2)),
      " (",
      as.character(round(PKNCA::geocv(c(10, 30), na.rm = TRUE))),
      ")"
    )
  )
  expect_equal(
    r0$cmax,
    paste0(
      as.character(round(PKNCA::geomean(c(10, 30), na.rm = TRUE), 0)),
      " (",
      as.character(round(PKNCA::geocv(c(10, 30), na.rm = TRUE))),
      ")"
    )
  )
  expect_false(identical(r2$cmax, r0$cmax))
})


test_that("nca_summary_table with group = NULL returns overall table", {
  result <- nca_summary_table(
    make_nca_summary_table_input(),
    parameters = "cmax",
    group = NULL,
    digits = 2
  )

  expect_false("DOSE" %in% names(result))
  expect_true(all(c("n", "cmax") %in% names(result)))
  expect_equal(nrow(result), 1)
  expect_equal(result$n, 3)
})


test_that("nca_summary_table supports multiple grouping variables", {
  nca_data <- tibble::tribble(
    ~DOSE, ~SEX, ~PPTESTCD, ~PPORRES,
      100,  "M",    "cmax",       10,
      100,  "F",    "cmax",       40,
      200,  "M",    "cmax",       20,
      200,  "F",    "cmax",       80
  )

  result <- nca_summary_table(
    nca_data,
    parameters = "cmax",
    group = c("DOSE", "SEX"),
    digits = 2
  )

  expect_true(all(c("DOSE", "SEX", "n", "cmax") %in% names(result)))
  expect_equal(nrow(result), 4)
  expect_equal(
    result$cmax[result$DOSE == 100 & result$SEX == "M"],
    "10 (NA)"
  )
  expect_equal(
    result$cmax[result$DOSE == 200 & result$SEX == "F"],
    "80 (NA)"
  )
})


test_that("nca_summary_table splits rows when parameter n differs within group", {
  nca_data <- tibble::tribble(
    ~DOSE, ~PPTESTCD, ~PPORRES,
      100,    "cmax",       10,
      100,    "cmax",       40,
      100,    "tmax",        1,
      100,    "tmax",        3,
      100,    "tmax",        5
  )

  result <- nca_summary_table(
    nca_data,
    parameters = c("cmax", "tmax"),
    group = "DOSE",
    digits = 2
  )

  # n is part of id_cols, so different n values become separate rows
  expect_equal(nrow(result), 2)
  expect_equal(sort(result$n), c(2, 3))
  expect_true(any(is.na(result$cmax)))
  expect_true(any(is.na(result$tmax)))
})


test_that("nca_summary_table respects exclude via nca_summary", {
  nca_data <- tibble::tribble(
    ~DOSE, ~PPTESTCD, ~PPORRES, ~exclude,
      100,    "cmax",       10,        NA,
      100,    "cmax",       40,     "bad",
      100,    "cmax",       90,        NA
  )

  result <- nca_summary_table(
    nca_data,
    parameters = "cmax",
    group = "DOSE",
    digits = 2
  )

  expect_equal(result$n, 2)
  expect_equal(
    result$cmax,
    paste0(
      as.character(round(PKNCA::geomean(c(10, 90), na.rm = TRUE), 2)),
      " (",
      as.character(round(PKNCA::geocv(c(10, 90), na.rm = TRUE))),
      ")"
    )
  )
})


test_that("nca_summary_table returns empty table for unmatched parameters", {
  expect_warning(
    result <- nca_summary_table(
      make_nca_summary_table_input(),
      parameters = "NOT_A_PARAMETER",
      group = "DOSE"
    ),
    "no non-missing arguments to min"
  )

  expect_equal(nrow(result), 0)
  expect_true(all(c("DOSE", "n") %in% names(result)))
})


test_that("nca_summary_table validates digits and parameters", {
  nca_data <- make_nca_summary_table_input()

  expect_error(
    nca_summary_table(nca_data, parameters = 1),
    "parameters must be a character value"
  )

  expect_error(
    nca_summary_table(nca_data, digits = "x"),
    "digits must be a numeric value"
  )

  expect_error(
    nca_summary_table(nca_data, digits = c(1, 2)),
    "digits must be a single value"
  )
})


test_that("nca_summary_table propagates nca_summary input validation", {
  expect_error(
    nca_summary_table(NULL),
    "Input must be a data frame!"
  )

  expect_error(
    nca_summary_table(data.frame(DOSE = 100, PPORRES = 1), group = "DOSE"),
    "Missing fields in nca: PPTESTCD"
  )

  expect_error(
    nca_summary_table(
      data.frame(PPTESTCD = "cmax", PPORRES = 1),
      group = "DOSE"
    ),
    "Missing fields in nca: DOSE"
  )
})


test_that("nca_summary_table works with nca() output", {
  suppressWarnings({
    nca_res <- nca(examplinib_sad_nif, analyte = "RS2023", silent = TRUE)
  })

  result <- nca_summary_table(
    nca_res,
    parameters = c("cmax", "tmax", "auclast"),
    group = "DOSE",
    digits = 2
  )

  expect_s3_class(result, "data.frame")
  expect_true(nrow(result) > 0)
  expect_true(all(c("DOSE", "n", "cmax", "tmax", "auclast") %in% names(result)))
  expect_true(all(grepl("\\(", result$cmax)))
  expect_true(all(grepl(";", result$tmax) | grepl("\\(", result$tmax)))
})


test_that("nca_summary_table works with nca_from_pp output", {
  nif_obj <- as_nif_test(tibble::tribble(
      ~ID, ~USUBJID, ~TIME, ~AMT, ~CMT, ~EVID, ~DV, ~ANALYTE, ~DOSE,
       1,   "SUBJ1",     0,  100,    1,     1,   0,   "DRUG",  100,
       1,   "SUBJ1",     1,    0,    2,     0,  10,   "DRUG",  100,
       2,   "SUBJ2",     0,  200,    1,     1,   0,   "DRUG",  200,
       2,   "SUBJ2",     1,    0,    2,     0,  20,   "DRUG",  200
    ))
  sdtm_data <- sdtm(list(
    pp = tibble::tribble(
      ~USUBJID, ~PPTESTCD, ~PPSTRESN, ~PPCAT,
      "SUBJ1",     "CMAX",        50, "DRUG",
      "SUBJ2",     "CMAX",       100, "DRUG",
      "SUBJ1",      "AUC",       200, "DRUG",
      "SUBJ2",      "AUC",       400, "DRUG"
    )
  ))

  from_pp <- nca_from_pp(
    nif_obj, sdtm_data,
    analyte = "DRUG",
    ppcat = "DRUG",
    silent = TRUE
  )

  result <- nca_summary_table(
    from_pp,
    parameters = c("CMAX", "AUC"),
    group = NULL,
    digits = 2
  )

  expect_equal(nrow(result), 1)
  expect_true(all(c("n", "CMAX", "AUC") %in% names(result)))
  expect_match(result$CMAX, "^70\\.71")
})


test_that("nca_summary_table works with nca_from_pp when grouping by DOSE", {
  nif_obj <- as_nif_test(tibble::tribble(
      ~ID, ~USUBJID, ~TIME, ~AMT, ~CMT, ~EVID, ~DV, ~ANALYTE, ~DOSE,
       1,   "SUBJ1",     0,  100,    1,     1,   0,   "DRUG",  100,
       1,   "SUBJ1",     1,    0,    2,     0,  10,   "DRUG",  100,
       2,   "SUBJ2",     0,  200,    1,     1,   0,   "DRUG",  200,
       2,   "SUBJ2",     1,    0,    2,     0,  20,   "DRUG",  200
    ))
  sdtm_data <- sdtm(list(
    pp = tibble::tribble(
      ~USUBJID, ~PPTESTCD, ~PPSTRESN, ~PPCAT,
      "SUBJ1",     "CMAX",        50, "DRUG",
      "SUBJ2",     "CMAX",       100, "DRUG"
    )
  ))

  from_pp <- nca_from_pp(
    nif_obj, sdtm_data,
    analyte = "DRUG",
    ppcat = "DRUG",
    keep = "DOSE",
    silent = TRUE
  )

  result <- nca_summary_table(
    from_pp,
    parameters = "CMAX",
    group = "DOSE",
    digits = 2
  )

  expect_equal(nrow(result), 2)
  expect_equal(sort(result$DOSE), c(100, 200))
  expect_equal(result$CMAX[result$DOSE == 100], "50 (NA)")
  expect_equal(result$CMAX[result$DOSE == 200], "100 (NA)")
})

