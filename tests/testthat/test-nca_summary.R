make_nca_summary_input <- function() {
  tibble::tribble(
    ~ID, ~DOSE, ~PPTESTCD, ~PPORRES, ~exclude,
      1,   100,    "cmax",       10,        NA,
      2,   100,    "cmax",       40,        NA,
      3,   200,    "cmax",       20,        NA,
      1,   100, "auclast",      100,        NA,
      2,   100, "auclast",      400,        NA,
      3,   200, "auclast",      200,        NA
  )
}


test_that("nca_summary returns expected structure for valid input", {
  result <- nca_summary(
    make_nca_summary_input(),
    parameters = c("cmax", "auclast"),
    group = "DOSE"
  )

  expect_s3_class(result, "data.frame")
  expect_true(all(c(
    "DOSE", "PPTESTCD", "geomean", "geocv", "median", "iqr", "min", "max", "n"
  ) %in% names(result)))
  expect_equal(sort(unique(result$PPTESTCD)), c("auclast", "cmax"))
  expect_equal(sort(unique(result$DOSE)), c(100, 200))
  expect_equal(nrow(result), 4)
})


test_that("nca_summary computes statistics correctly by group", {
  result <- nca_summary(
    make_nca_summary_input(),
    parameters = "cmax",
    group = "DOSE"
  ) |>
    as.data.frame()

  dose100 <- result[result$DOSE == 100, ]
  dose200 <- result[result$DOSE == 200, ]

  expect_equal(dose100$n, 2)
  expect_equal(dose100$geomean, PKNCA::geomean(c(10, 40), na.rm = TRUE))
  expect_equal(dose100$geocv, PKNCA::geocv(c(10, 40), na.rm = TRUE))
  expect_equal(dose100$median, 25)
  expect_equal(dose100$min, 10)
  expect_equal(dose100$max, 40)
  expect_equal(dose100$iqr, IQR(c(10, 40)))

  expect_equal(dose200$n, 1)
  expect_equal(dose200$geomean, 20)
  expect_equal(dose200$median, 20)
  expect_equal(dose200$min, 20)
  expect_equal(dose200$max, 20)
})


test_that("nca_summary with group = NULL summarizes overall", {
  result <- nca_summary(
    make_nca_summary_input(),
    parameters = "cmax",
    group = NULL
  )

  expect_false("DOSE" %in% names(result))
  expect_equal(nrow(result), 1)
  expect_equal(result$PPTESTCD, "cmax")
  expect_equal(result$n, 3)
  expect_equal(result$min, 10)
  expect_equal(result$max, 40)
})


test_that("nca_summary supports multiple grouping variables", {
  nca_data <- tibble::tribble(
    ~DOSE, ~SEX, ~PPTESTCD, ~PPORRES,
      100,  "M",    "cmax",       10,
      100,  "F",    "cmax",       40,
      200,  "M",    "cmax",       20,
      200,  "F",    "cmax",       80
  )

  result <- nca_summary(
    nca_data,
    parameters = "cmax",
    group = c("DOSE", "SEX")
  )

  expect_true(all(c("DOSE", "SEX", "PPTESTCD") %in% names(result)))
  expect_equal(nrow(result), 4)
  expect_equal(
    result$geomean[result$DOSE == 100 & result$SEX == "M"],
    10
  )
  expect_equal(
    result$geomean[result$DOSE == 200 & result$SEX == "F"],
    80
  )
})


test_that("nca_summary prefers PPSTRESN over PPORRES", {
  nca_data <- tibble::tribble(
    ~PPTESTCD, ~PPSTRESN, ~PPORRES,
       "cmax",        10,       99
  )

  result <- nca_summary(nca_data, parameters = "cmax")

  expect_equal(result$geomean, 10)
  expect_equal(result$median, 10)
})


test_that("nca_summary uses PPORRES when PPSTRESN is absent", {
  nca_data <- tibble::tribble(
    ~PPTESTCD, ~PPORRES,
       "cmax",       25,
       "cmax",      100
  )

  result <- nca_summary(nca_data, parameters = "cmax")

  expect_equal(result$geomean, PKNCA::geomean(c(25, 100), na.rm = TRUE))
  expect_equal(result$n, 2)
})


test_that("nca_summary errors when neither PPSTRESN nor PPORRES is present", {
  nca_data <- tibble::tribble(
    ~PPTESTCD, ~DOSE,
       "cmax",  100
  )

  expect_error(
    nca_summary(nca_data, parameters = "cmax"),
    "Neither PPSTRESN nor PPORRES found in input!"
  )
})


test_that("nca_summary excludes rows with non-missing exclude", {
  nca_data <- tibble::tribble(
    ~PPTESTCD, ~PPORRES, ~exclude,
       "cmax",       10,        NA,
       "cmax",       40,     "bad",
       "cmax",       90,        NA
  )

  result <- nca_summary(nca_data, parameters = "cmax")

  expect_equal(result$n, 2)
  expect_equal(result$geomean, PKNCA::geomean(c(10, 90), na.rm = TRUE))
  expect_equal(result$min, 10)
  expect_equal(result$max, 90)
})


test_that("nca_summary ignores exclude column when absent", {
  nca_data <- tibble::tribble(
    ~PPTESTCD, ~PPORRES,
       "cmax",       10,
       "cmax",       40
  )

  result <- nca_summary(nca_data, parameters = "cmax")

  expect_equal(result$n, 2)
  expect_equal(result$min, 10)
  expect_equal(result$max, 40)
})


test_that("nca_summary returns empty result for unmatched parameters", {
  expect_warning(
    result <- nca_summary(
      make_nca_summary_input(),
      parameters = "NOT_A_PARAMETER"
    ),
    "no non-missing arguments to min"
  )

  expect_equal(nrow(result), 0)
  expect_true(all(c(
    "PPTESTCD", "geomean", "geocv", "median", "iqr", "min", "max", "n"
  ) %in% names(result)))
})


test_that("nca_summary validates input is a data frame", {
  expect_error(nca_summary(NULL), "Input must be a data frame!")
  expect_error(nca_summary(list(PPTESTCD = "cmax")), "Input must be a data frame!")
  expect_error(nca_summary("not a data frame"), "Input must be a data frame!")
})


test_that("nca_summary validates required fields", {
  expect_error(
    nca_summary(data.frame(DOSE = 100, PPORRES = 1)),
    "Missing fields in nca: PPTESTCD"
  )

  expect_error(
    nca_summary(
      data.frame(PPTESTCD = "cmax", PPORRES = 1),
      group = "DOSE"
    ),
    "Missing fields in nca: DOSE"
  )

  expect_error(
    nca_summary(
      data.frame(PPTESTCD = "cmax", PPORRES = 1),
      group = c("DOSE", "SEX")
    ),
    "Missing fields in nca"
  )
})


test_that("nca_summary validates argument types", {
  nca_data <- make_nca_summary_input()

  expect_error(
    nca_summary(nca_data, parameters = 1),
    "parameters must be a character value"
  )

  expect_error(
    nca_summary(nca_data, group = 1),
    "group must be a character value"
  )
})


test_that("nca_summary works with nca() output", {
  suppressWarnings({
    nca_res <- nca(examplinib_sad_nif, analyte = "RS2023", silent = TRUE)
  })

  result <- nca_summary(
    nca_res,
    parameters = c("cmax", "auclast"),
    group = "DOSE"
  )

  expect_s3_class(result, "data.frame")
  expect_true(nrow(result) > 0)
  expect_true(all(result$PPTESTCD %in% c("cmax", "auclast")))
  expect_true("DOSE" %in% names(result))
  expect_true(all(result$n >= 1))
})


test_that("nca_summary works with nca_from_pp output", {
  nif_obj <- structure(
    tibble::tribble(
      ~ID, ~USUBJID, ~TIME, ~AMT, ~CMT, ~EVID, ~DV, ~ANALYTE, ~DOSE,
       1,   "SUBJ1",     0,  100,    1,     1,   0,   "DRUG",  100,
       1,   "SUBJ1",     1,    0,    2,     0,  10,   "DRUG",  100,
       2,   "SUBJ2",     0,  200,    1,     1,   0,   "DRUG",  200,
       2,   "SUBJ2",     1,    0,    2,     0,  20,   "DRUG",  200
    ),
    class = c("nif", "data.frame")
  )
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

  result <- nca_summary(from_pp, parameters = c("CMAX", "AUC"))

  expect_equal(nrow(result), 2)
  expect_equal(sort(result$PPTESTCD), c("AUC", "CMAX"))
  expect_equal(result$geomean[result$PPTESTCD == "CMAX"],
               PKNCA::geomean(c(50, 100), na.rm = TRUE))
})


test_that("nca_summary works with nca_from_pp when grouping by DOSE", {
  nif_obj <- structure(
    tibble::tribble(
      ~ID, ~USUBJID, ~TIME, ~AMT, ~CMT, ~EVID, ~DV, ~ANALYTE, ~DOSE,
       1,   "SUBJ1",     0,  100,    1,     1,   0,   "DRUG",  100,
       1,   "SUBJ1",     1,    0,    2,     0,  10,   "DRUG",  100,
       2,   "SUBJ2",     0,  200,    1,     1,   0,   "DRUG",  200,
       2,   "SUBJ2",     1,    0,    2,     0,  20,   "DRUG",  200
    ),
    class = c("nif", "data.frame")
  )
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

  result <- nca_summary(
    from_pp,
    parameters = "CMAX",
    group = "DOSE"
  )

  expect_equal(nrow(result), 2)
  expect_equal(sort(result$DOSE), c(100, 200))
  expect_equal(result$geomean[result$DOSE == 100], 50)
  expect_equal(result$geomean[result$DOSE == 200], 100)
})
