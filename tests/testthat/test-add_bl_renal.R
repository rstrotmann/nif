## Tests for add_bl_renal
##
## Classifies BL_CRCL into BL_RENAL:
##   severe <30, moderate 30–60, mild 60–90, normal ≥90
## If BL_CRCL is missing, it is derived via add_bl_crcl first.


test_that("add_bl_renal classifies existing BL_CRCL values", {
  nif_obj <- tibble::tribble(
    ~ID, ~TIME, ~AMT, ~CMT, ~EVID, ~DV, ~BL_CRCL,
      1,     0,    0,    1,     0,  NA,      25,
      2,     0,    0,    1,     0,  NA,      45,
      3,     0,    0,    1,     0,  NA,      75,
      4,     0,    0,    1,     0,  NA,     100
  ) |>
    nif()

  result <- add_bl_renal(nif_obj)

  expect_s3_class(result, "nif")
  expect_true("BL_RENAL" %in% names(result))
  expect_equal(
    as.character(result$BL_RENAL),
    c("severe", "moderate", "mild", "normal")
  )
})


test_that("add_bl_renal returns an ordered factor with expected levels", {
  nif_obj <- tibble::tribble(
    ~ID, ~TIME, ~AMT, ~CMT, ~EVID, ~DV, ~BL_CRCL,
      1,     0,    0,    1,     0,  NA,      25,
      2,     0,    0,    1,     0,  NA,     100
  ) |>
    nif()

  result <- add_bl_renal(nif_obj)

  expect_s3_class(result$BL_RENAL, "factor")
  expect_equal(
    levels(result$BL_RENAL),
    c("normal", "mild", "moderate", "severe")
  )
})


test_that("add_bl_renal uses closed-on-the-right cut boundaries", {
  nif_obj <- tibble::tribble(
    ~ID, ~TIME, ~AMT, ~CMT, ~EVID, ~DV, ~BL_CRCL,
      1,     0,    0,    1,     0,  NA,      30,
      2,     0,    0,    1,     0,  NA,      60,
      3,     0,    0,    1,     0,  NA,      90
  ) |>
    nif()

  result <- add_bl_renal(nif_obj)

  expect_equal(
    as.character(result$BL_RENAL),
    c("severe", "moderate", "mild")
  )
})


test_that("add_bl_renal keeps BL_RENAL constant within a subject", {
  nif_obj <- tibble::tribble(
    ~ID, ~TIME, ~AMT, ~CMT, ~EVID, ~DV, ~BL_CRCL,
      1,     0,  100,    1,     1,  NA,      45,
      1,     1,    0,    2,     0,  10,      45,
      1,    24,  100,    1,     1,  NA,      45
  ) |>
    nif()

  result <- add_bl_renal(nif_obj)

  expect_equal(unique(as.character(result$BL_RENAL)), "moderate")
  expect_equal(length(unique(result$BL_RENAL)), 1)
})


test_that("add_bl_renal calculates BL_CRCL when it is missing", {
  nif_obj <- tibble::tribble(
    ~ID, ~USUBJID,   ~TIME, ~AMT, ~CMT, ~EVID, ~DV, ~BL_CREAT, ~AGE, ~SEX, ~RACE,    ~WEIGHT,
      1, "SUBJ-001",     0,    0,    1,     0,  NA,       0.8,   45,    0, "WHITE",      70,
      1, "SUBJ-001",    24,    0,    1,     0,  NA,       0.8,   45,    0, "WHITE",      70
  ) |>
    nif()

  result <- add_bl_renal(nif_obj, molar = FALSE)

  expect_true("BL_CRCL" %in% names(result))
  expect_true("BL_RENAL" %in% names(result))
  expect_false(any(is.na(result$BL_CRCL)))
  expect_false(any(is.na(result$BL_RENAL)))
  expect_equal(length(unique(result$BL_RENAL)), 1)
})


test_that("add_bl_renal can use an alternate eGFR method when deriving BL_CRCL", {
  nif_obj <- tibble::tribble(
    ~ID, ~USUBJID,   ~TIME, ~AMT, ~CMT, ~EVID, ~DV, ~BL_CREAT, ~AGE, ~SEX, ~RACE,    ~WEIGHT,
      1, "SUBJ-001",     0,    0,    1,     0,  NA,       0.8,   45,    0, "WHITE",      70
  ) |>
    nif()

  cg <- add_bl_renal(nif_obj, method = egfr_cg, molar = FALSE)
  mdrd <- add_bl_renal(nif_obj, method = egfr_mdrd, molar = FALSE)

  expect_true("BL_RENAL" %in% names(cg))
  expect_true("BL_RENAL" %in% names(mdrd))
  expect_false(is.na(cg$BL_CRCL[1]))
  expect_false(is.na(mdrd$BL_CRCL[1]))
})


test_that("add_bl_renal does not recompute BL_CRCL when already present", {
  nif_obj <- tibble::tribble(
    ~ID, ~TIME, ~AMT, ~CMT, ~EVID, ~DV, ~BL_CRCL, ~BL_CREAT, ~AGE, ~SEX, ~RACE, ~WEIGHT,
      1,     0,    0,    1,     0,  NA,      120,       5.0,   80,    1, "WHITE",     50
  ) |>
    nif()

  result <- add_bl_renal(nif_obj)

  expect_equal(result$BL_CRCL, 120)
  expect_equal(as.character(result$BL_RENAL), "normal")
})


test_that("add_bl_renal errors when BL_CRCL cannot be derived", {
  nif_obj <- tibble::tribble(
    ~ID, ~TIME, ~AMT, ~CMT, ~EVID, ~DV,
      1,     0,    0,    1,     0,  NA
  ) |>
    nif()

  expect_error(
    add_bl_renal(nif_obj),
    "Missing columns"
  )
})


test_that("add_bl_renal classifies BL_CRCL on a plain data frame", {
  df <- data.frame(
    ID = 1,
    TIME = 0,
    AMT = 0,
    CMT = 1,
    EVID = 0,
    DV = NA_real_,
    BL_CRCL = 50
  )

  result <- add_bl_renal(df)

  expect_equal(as.character(result$BL_RENAL), "moderate")
})


test_that("add_bl_renal works with examplinib data", {
  skip_if_not(all(c("BL_CREAT", "AGE", "SEX", "RACE", "WEIGHT") %in% names(examplinib_poc_nif)) ||
                "BL_CRCL" %in% names(examplinib_poc_nif))

  result <- if ("BL_CRCL" %in% names(examplinib_poc_nif)) {
    add_bl_renal(examplinib_poc_nif)
  } else {
    add_bl_renal(examplinib_poc_nif, molar = TRUE)
  }

  expect_s3_class(result, "nif")
  expect_true("BL_RENAL" %in% names(result))
})
