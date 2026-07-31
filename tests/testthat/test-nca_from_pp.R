make_nca_from_pp_nif <- function() {
  structure(
    tibble::tribble(
      ~ID, ~USUBJID, ~TIME, ~AMT, ~CMT, ~EVID, ~DV, ~ANALYTE, ~DOSE, ~AGE, ~SEX, ~WEIGHT, ~BL_CRCL, ~CUSTOM,
      1,   "SUBJ1",  0,     100,  1,    1,     0,  "DRUG",   100,   30,   "M",  70,      90,       "A",
      1,   "SUBJ1",  1,     0,    2,    0,     10, "DRUG",   100,   30,   "M",  70,      90,       "A",
      2,   "SUBJ2",  0,     200,  1,    1,     0,  "DRUG",   200,   40,   "F",  60,      80,       "B",
      2,   "SUBJ2",  1,     0,    2,    0,     20, "DRUG",   200,   40,   "F",  60,      80,       "B",
      1,   "SUBJ1",  1,     0,    3,    0,     5,  "METAB",  100,   30,   "M",  70,      90,       "A"
    ),
    class = c("nif", "data.frame")
  )
}


make_nca_from_pp_sdtm <- function() {
  sdtm(list(
    pp = tibble::tribble(
      ~USUBJID, ~PPTESTCD, ~PPSTRESN, ~PPSPEC, ~PPCAT,  ~PPSCAT,       ~PPRFTDTC,    ~DOMAIN,
      "SUBJ1",  "AUC",     100,       "PLASMA", "DRUG",  "SINGLE DOSE", "2023-01-01", "PP",
      "SUBJ1",  "CMAX",    50,        "PLASMA", "DRUG",  "SINGLE DOSE", "2023-01-01", "PP",
      "SUBJ2",  "AUC",     200,       "PLASMA", "DRUG",  "SINGLE DOSE", "2023-01-02", "PP",
      "SUBJ2",  "CMAX",    100,       "URINE",  "DRUG",  "SINGLE DOSE", "2023-01-02", "PP",
      "SUBJ1",  "AUC",     10,        "PLASMA", "METAB", "SINGLE DOSE", "2023-01-01", "PP"
    )
  ))
}


test_that("nca_from_pp returns expected structure for valid inputs", {
  nif_obj <- make_nca_from_pp_nif()
  sdtm_data <- make_nca_from_pp_sdtm()

  result <- nca_from_pp(
    nif_obj, sdtm_data,
    analyte = "DRUG",
    ppcat = "DRUG",
    silent = TRUE
  )

  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 4)
  expect_true(all(c(
    "USUBJID", "PPTESTCD", "PPSTRESN", "PPSPEC", "PPCAT", "PPRFTDTC",
    "ANALYTE", "ID"
  ) %in% names(result)))
  expect_equal(unique(result$ANALYTE), "DRUG")
  expect_equal(unique(result$PPCAT), "DRUG")
  expect_equal(sort(unique(result$USUBJID)), c("SUBJ1", "SUBJ2"))
})


test_that("nca_from_pp joins subject-level covariates from nif", {
  nif_obj <- make_nca_from_pp_nif()
  sdtm_data <- make_nca_from_pp_sdtm()

  result <- nca_from_pp(
    nif_obj, sdtm_data,
    analyte = "DRUG",
    ppcat = "DRUG",
    silent = TRUE
  )

  expect_true(all(c("AGE", "SEX", "WEIGHT", "BL_CRCL") %in% names(result)))
  expect_equal(result$AGE[result$USUBJID == "SUBJ1"][1], 30)
  expect_equal(result$SEX[result$USUBJID == "SUBJ2"][1], "F")
})


test_that("nca_from_pp keep adds custom columns from nif", {
  nif_obj <- make_nca_from_pp_nif()
  sdtm_data <- make_nca_from_pp_sdtm()

  result <- nca_from_pp(
    nif_obj, sdtm_data,
    analyte = "DRUG",
    ppcat = "DRUG",
    keep = "CUSTOM",
    silent = TRUE
  )
  expect_true("CUSTOM" %in% names(result))
  expect_equal(result$CUSTOM[result$USUBJID == "SUBJ1"][1], "A")

  result_multi <- nca_from_pp(
    nif_obj, sdtm_data,
    analyte = "DRUG",
    ppcat = "DRUG",
    keep = c("CUSTOM", "AGE"),
    silent = TRUE
  )
  expect_true(all(c("CUSTOM", "AGE") %in% names(result_multi)))

  result_missing <- nca_from_pp(
    nif_obj, sdtm_data,
    analyte = "DRUG",
    ppcat = "DRUG",
    keep = "NONEXISTENT",
    silent = TRUE
  )
  expect_false("NONEXISTENT" %in% names(result_missing))
})


test_that("nca_from_pp observation_filter subsets PP rows", {
  nif_obj <- make_nca_from_pp_nif()
  sdtm_data <- make_nca_from_pp_sdtm()

  result <- nca_from_pp(
    nif_obj, sdtm_data,
    analyte = "DRUG",
    ppcat = "DRUG",
    observation_filter = "PPSPEC == 'PLASMA' & PPTESTCD == 'AUC'",
    silent = TRUE
  )

  expect_equal(nrow(result), 2)
  expect_true(all(result$PPTESTCD == "AUC"))
  expect_true(all(result$PPSPEC == "PLASMA"))
})


test_that("nca_from_pp group retains grouping column from PP", {
  nif_obj <- make_nca_from_pp_nif()
  sdtm_data <- make_nca_from_pp_sdtm()

  result <- nca_from_pp(
    nif_obj, sdtm_data,
    analyte = "DRUG",
    ppcat = "DRUG",
    group = "PPSPEC",
    silent = TRUE
  )

  expect_true("PPSPEC" %in% names(result))
  expect_equal(sort(unique(result$PPSPEC)), c("PLASMA", "URINE"))
})


test_that("nca_from_pp guesses analyte when NULL", {
  nif_obj <- make_nca_from_pp_nif()
  sdtm_data <- make_nca_from_pp_sdtm()

  result <- nca_from_pp(
    nif_obj, sdtm_data,
    ppcat = "DRUG",
    silent = TRUE
  )

  expect_equal(unique(result$ANALYTE), "DRUG")
  expect_equal(nrow(result), 4)
})


test_that("nca_from_pp silent suppresses analyte warning", {
  nif_obj <- make_nca_from_pp_nif()
  sdtm_data <- make_nca_from_pp_sdtm()

  expect_message(
      expect_message(
      nca_from_pp(nif_obj, sdtm_data, ppcat = "DRUG", silent = FALSE),
      "No analyte specified"
    ),
    "Keep fields"
  )

  expect_no_message(
    nca_from_pp(nif_obj, sdtm_data, ppcat = "DRUG", silent = TRUE)
  )
})


test_that("nca_from_pp filters by ppcat", {
  nif_obj <- make_nca_from_pp_nif()
  sdtm_data <- make_nca_from_pp_sdtm()

  result <- nca_from_pp(
    nif_obj, sdtm_data,
    analyte = "METAB",
    ppcat = "METAB",
    silent = TRUE
  )

  expect_equal(nrow(result), 1)
  expect_equal(result$PPCAT, "METAB")
  expect_equal(result$ANALYTE, "METAB")
  expect_equal(result$PPSTRESN, 10)
})


test_that("nca_from_pp filters by ppscat", {
  nif_obj <- make_nca_from_pp_nif()
  sdtm_data <- make_nca_from_pp_sdtm()

  result <- nca_from_pp(
    nif_obj, sdtm_data,
    analyte = "DRUG",
    ppcat = "DRUG",
    ppscat = "SINGLE DOSE",
    silent = TRUE
  )

  expect_equal(nrow(result), 4)
})


test_that("nca_from_pp errors on multiple PPCAT in result", {
  nif_obj <- make_nca_from_pp_nif()
  sdtm_data <- make_nca_from_pp_sdtm()

  expect_error(
    nca_from_pp(nif_obj, sdtm_data, analyte = "DRUG", silent = TRUE),
    "Multiple PPCAT in result"
  )
})


test_that("nca_from_pp validates ppcat against PP domain", {
  nif_obj <- make_nca_from_pp_nif()
  sdtm_data <- make_nca_from_pp_sdtm()

  expect_error(
    nca_from_pp(
      nif_obj, sdtm_data,
      analyte = "DRUG",
      ppcat = "NOPE",
      silent = TRUE
    ),
    "PPCAT of NOPE not found in PP domain"
  )

  sdtm_no_ppcat <- sdtm(list(
    pp = tibble::tribble(
      ~USUBJID, ~PPTESTCD, ~PPSTRESN,
      "SUBJ1",  "AUC",     100
    )
  ))

  expect_error(
    nca_from_pp(
      nif_obj, sdtm_no_ppcat,
      analyte = "DRUG",
      ppcat = "DRUG",
      silent = TRUE
    ),
    "PPCAT not found in PP domain"
  )
})


test_that("nca_from_pp validates ppscat against PP domain", {
  nif_obj <- make_nca_from_pp_nif()
  sdtm_data <- make_nca_from_pp_sdtm()

  expect_error(
    nca_from_pp(
      nif_obj, sdtm_data,
      analyte = "DRUG",
      ppcat = "DRUG",
      ppscat = "NOPE",
      silent = TRUE
    ),
    "PPSCAT of NOPE not found in PP domain"
  )

  sdtm_no_ppscat <- sdtm(list(
    pp = tibble::tribble(
      ~USUBJID, ~PPTESTCD, ~PPSTRESN, ~PPCAT,
      "SUBJ1",  "AUC",     100,       "DRUG"
    )
  ))

  expect_error(
    nca_from_pp(
      nif_obj, sdtm_no_ppscat,
      analyte = "DRUG",
      ppcat = "DRUG",
      ppscat = "SINGLE DOSE",
      silent = TRUE
    ),
    "PPSCAT not found in PP domain"
  )
})


test_that("nca_from_pp warns when filters leave no rows", {
  nif_obj <- make_nca_from_pp_nif()
  sdtm_data <- make_nca_from_pp_sdtm()

  expect_warning(
    result <- nca_from_pp(
      nif_obj, sdtm_data,
      analyte = "DRUG",
      ppcat = "DRUG",
      observation_filter = "PPSPEC == 'SALIVA'",
      silent = TRUE
    ),
    "No data found after applying filters"
  )
  expect_equal(nrow(result), 0)
})


test_that("nca_from_pp validates nif and sdtm inputs", {
  nif_obj <- make_nca_from_pp_nif()
  sdtm_data <- make_nca_from_pp_sdtm()

  expect_error(
    nca_from_pp(),
    'argument "obj" is missing, with no default'
  )

  expect_error(
    nca_from_pp(data.frame(), sdtm_data, silent = TRUE),
    "Input must be a nif object"
  )

  incomplete_nif <- structure(
    tibble::tribble(
      ~ID, ~USUBJID, ~ANALYTE,
      1,   "SUBJ1",  "DRUG"
    ),
    class = c("nif", "data.frame")
  )
  expect_error(
    nca_from_pp(incomplete_nif, sdtm_data, silent = TRUE),
    "Missing essential fields in nif object"
  )

  expect_error(
    nca_from_pp(nif_obj, list(), silent = TRUE),
    "Input must be a sdtm object"
  )

  sdtm_no_pp <- sdtm(list(
    dm = tibble::tribble(
      ~USUBJID, ~DOMAIN,
      "SUBJ1",  "DM"
    )
  ))
  expect_error(
    nca_from_pp(nif_obj, sdtm_no_pp, silent = TRUE),
    "Expected domain missing in sdtm object: pp"
  )
})


test_that("nca_from_pp validates argument types", {
  nif_obj <- make_nca_from_pp_nif()
  sdtm_data <- make_nca_from_pp_sdtm()

  expect_error(
    nca_from_pp(nif_obj, sdtm_data, analyte = 1, silent = TRUE),
    "analyte must be a character value"
  )

  expect_error(
    nca_from_pp(nif_obj, sdtm_data, analyte = c("DRUG", "METAB"), silent = TRUE),
    "analyte must be a single value"
  )

  expect_error(
    nca_from_pp(
      nif_obj, sdtm_data,
      analyte = "DRUG",
      ppcat = "DRUG",
      keep = 1,
      silent = TRUE
    ),
    "keep must be a character value"
  )

  expect_error(
    nca_from_pp(
      nif_obj, sdtm_data,
      analyte = "DRUG",
      ppcat = "DRUG",
      group = c("PPSPEC", "PPCAT"),
      silent = TRUE
    ),
    "group must be a single value"
  )

  expect_error(
    nca_from_pp(
      nif_obj, sdtm_data,
      analyte = "DRUG",
      ppcat = "DRUG",
      observation_filter = NULL,
      silent = TRUE
    ),
    "observation_filter must not be NULL"
  )
})


test_that("nca_from_pp validates observation_filter expression", {
  nif_obj <- make_nca_from_pp_nif()
  sdtm_data <- make_nca_from_pp_sdtm()

  expect_error(
    nca_from_pp(
      nif_obj, sdtm_data,
      analyte = "DRUG",
      ppcat = "DRUG",
      observation_filter = "invalid filter",
      silent = TRUE
    ),
    "Failed to parse filter expression"
  )

  expect_error(
    nca_from_pp(
      nif_obj, sdtm_data,
      analyte = "DRUG",
      ppcat = "DRUG",
      observation_filter = "system('rm -rf /')",
      silent = TRUE
    ),
    "Disallowed construct in filter expression"
  )

  expect_error(
    nca_from_pp(
      nif_obj, sdtm_data,
      analyte = "DRUG",
      ppcat = "DRUG",
      observation_filter = "NOT_A_COLUMN == 1",
      silent = TRUE
    ),
    "Column 'NOT_A_COLUMN' not found in data"
  )
})


test_that("nca_from_pp leaves PP subjects missing from nif unmatched", {
  nif_obj <- make_nca_from_pp_nif()
  sdtm_data <- sdtm(list(
    pp = tibble::tribble(
      ~USUBJID, ~PPTESTCD, ~PPSTRESN, ~PPSPEC, ~PPCAT, ~PPRFTDTC,
      "SUBJ1",  "AUC",     100,       "PLASMA", "DRUG", "2023-01-01",
      "SUBJ9",  "AUC",     999,       "PLASMA", "DRUG", "2023-01-01"
    )
  ))

  result <- nca_from_pp(
    nif_obj, sdtm_data,
    analyte = "DRUG",
    ppcat = "DRUG",
    silent = TRUE
  )

  expect_equal(nrow(result), 2)
  expect_true(is.na(result$ID[result$USUBJID == "SUBJ9"]))
  expect_equal(result$ID[result$USUBJID == "SUBJ1"], 1)
})


test_that("nca_from_pp analyte with no nif rows still returns PP data", {
  nif_obj <- make_nca_from_pp_nif()
  sdtm_data <- make_nca_from_pp_sdtm()

  result <- nca_from_pp(
    nif_obj, sdtm_data,
    analyte = "UNKNOWN",
    ppcat = "DRUG",
    silent = TRUE
  )

  expect_equal(nrow(result), 4)
  expect_equal(unique(result$ANALYTE), "UNKNOWN")
  expect_true(all(is.na(result$ID)))
  expect_false("DOSE" %in% names(result))
})


test_that("nca_from_pp errors when keep columns vary within subject", {
  nif_obj <- structure(
    tibble::tribble(
      ~ID, ~USUBJID, ~TIME, ~AMT, ~CMT, ~EVID, ~DV, ~ANALYTE, ~DOSE, ~CUSTOM,
      1,   "SUBJ1",  0,     100,  1,    1,     0,  "DRUG",   100,   "A",
      1,   "SUBJ1",  1,     0,    2,    0,     10, "DRUG",   100,   "A",
      1,   "SUBJ1",  2,     0,    2,    0,     8,  "DRUG",   100,   "B"
    ),
    class = c("nif", "data.frame")
  )
  sdtm_data <- sdtm(list(
    pp = tibble::tribble(
      ~USUBJID, ~PPTESTCD, ~PPSTRESN, ~PPCAT,
      "SUBJ1",  "AUC",     100,       "DRUG"
    )
  ))

  expect_error(
    nca_from_pp(
      nif_obj, sdtm_data,
      analyte = "DRUG",
      ppcat = "DRUG",
      keep = "CUSTOM",
      silent = TRUE
    ),
    "Multiple keep values for subjects"
  )
})


test_that("nca_from_pp validates filter when ppcat and ppscat leave no rows", {
  nif_obj <- make_nca_from_pp_nif()
  sdtm_data <- sdtm(list(
    pp = tibble::tribble(
      ~USUBJID, ~PPTESTCD, ~PPSTRESN, ~PPCAT,  ~PPSCAT,
      "SUBJ1",  "AUC",     100,       "DRUG",  "SINGLE DOSE",
      "SUBJ1",  "AUC",     50,        "METAB", "STEADY STATE"
    )
  ))

  # PPCAT and PPSCAT each exist in PP, but not together
  expect_warning(
    result <- nca_from_pp(
      nif_obj, sdtm_data,
      analyte = "DRUG",
      ppcat = "DRUG",
      ppscat = "STEADY STATE",
      silent = TRUE
    ),
    "No data found after applying filters"
  )
  expect_equal(nrow(result), 0)
})


test_that("nca_from_pp works when PP has no PPCAT column", {
  nif_obj <- make_nca_from_pp_nif()
  sdtm_data <- sdtm(list(
    pp = tibble::tribble(
      ~USUBJID, ~PPTESTCD, ~PPSTRESN, ~PPSPEC, ~PPRFTDTC,
      "SUBJ1",  "AUC",     100,       "PLASMA", "2023-01-01",
      "SUBJ2",  "CMAX",    50,        "PLASMA", "2023-01-02"
    )
  ))

  result <- nca_from_pp(
    nif_obj, sdtm_data,
    analyte = "DRUG",
    silent = TRUE
  )

  expect_equal(nrow(result), 2)
  expect_false("PPCAT" %in% names(result))
  expect_equal(result$ID[result$USUBJID == "SUBJ1"], 1)
})


test_that("nca_from_pp keep can add DOSE when constant per subject", {
  nif_obj <- make_nca_from_pp_nif()
  sdtm_data <- make_nca_from_pp_sdtm()

  result <- nca_from_pp(
    nif_obj, sdtm_data,
    analyte = "DRUG",
    ppcat = "DRUG",
    keep = "DOSE",
    silent = TRUE
  )

  expect_true("DOSE" %in% names(result))
  expect_equal(result$DOSE[result$USUBJID == "SUBJ1"][1], 100)
  expect_equal(result$DOSE[result$USUBJID == "SUBJ2"][1], 200)
})

