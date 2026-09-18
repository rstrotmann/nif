test_that("resolve_imputation_rules() maps names and NULL to lists", {
  expect_identical(
    nif:::resolve_imputation_rules(NULL),
    imputation_rules_standard
  )
  expect_identical(
    nif:::resolve_imputation_rules("imputation_rules_void"),
    imputation_rules_void
  )
  expect_identical(
    nif:::resolve_imputation_rules(imputation_rules_minimal),
    imputation_rules_minimal
  )
  expect_error(
    nif:::resolve_imputation_rules("not_a_rule_set"),
    "Unknown imputation rule set"
  )
})


test_that("nif() stores a resolved imputation_rules list", {
  empty <- nif(silent = TRUE)
  expect_identical(
    attr(empty, "imputation_rules"),
    imputation_rules_standard
  )

  by_name <- nif(imputation_rules = "imputation_rules_void", silent = TRUE)
  expect_identical(
    attr(by_name, "imputation_rules"),
    imputation_rules_void
  )

  by_list <- nif(imputation_rules = imputation_rules_minimal, silent = TRUE)
  expect_identical(
    attr(by_list, "imputation_rules"),
    imputation_rules_minimal
  )
})


test_that("nif() from a data frame uses the constructor imputation_rules", {
  from_df <- nif(
    tibble::tribble(
      ~ID, ~TIME, ~AMT, ~CMT, ~EVID, ~DV,
      1,   0,     100,  1,    1,     NA_real_
    ),
    imputation_rules = imputation_rules_void
  )

  expect_identical(
    attr(from_df, "imputation_rules"),
    imputation_rules_void
  )
})


test_that("nif() wrapping a nif object keeps imputation_rules", {
  from_df <- nif(
    tibble::tribble(
      ~ID, ~TIME, ~AMT, ~CMT, ~EVID, ~DV,
      1,   0,     100,  1,    1,     NA_real_
    ),
    imputation_rules = imputation_rules_void
  )

  expect_identical(
    attr(nif(from_df), "imputation_rules"),
    imputation_rules_void
  )
})


test_that("dplyr verbs keep imputation_rules via reconstruct", {
  obj <- nif(imputation_rules = imputation_rules_void, silent = TRUE)
  out <- dplyr::filter(obj, ID > 0)

  expect_s3_class(out, "nif")
  expect_identical(
    attr(out, "imputation_rules"),
    imputation_rules_void
  )
})


test_that("index_id() and normalize_id() keep imputation_rules", {
  obj <- nif(
    tibble::tribble(
      ~ID, ~TIME, ~AMT, ~CMT, ~EVID, ~DV,
      1,   0,     100,  1,    1,     NA_real_,
      1,   1,     0,    1,    0,     10
    ),
    imputation_rules = imputation_rules_void
  )

  expect_identical(
    attr(nif:::index_id(obj), "imputation_rules"),
    imputation_rules_void
  )
  expect_identical(
    attr(nif:::normalize_id(obj), "imputation_rules"),
    imputation_rules_void
  )
})


test_that("add_administration() uses nif imputation_rules when imputation is NULL", {
  sdtm_obj <- sdtm(list(
    dm = tibble::tribble(
      ~USUBJID, ~DOMAIN, ~SEX, ~ACTARMCD,             ~RFXSTDTC,              ~RFSTDTC,              ~RFENDTC, ~ACTARM,  ~STUDYID,
      "1",      "DM",    "M",  "A",       "2024-01-01T08:00:00", "2024-01-01T08:00:00", "2024-01-02T08:00:00", "Arm A", "Study 1"
    ),
    vs = tibble::tribble(
      ~USUBJID, ~DOMAIN, ~VSTESTCD, ~VSSTRESN,              ~VSDTC,
      "1",      "VS",    "HEIGHT",       170, "2024-01-01T08:00:00"
    ),
    ex = tibble::tribble(
      ~USUBJID, ~DOMAIN, ~EXDOSE, ~EXTRT,              ~EXSTDTC,              ~EXENDTC, ~EXSEQ,
      "1",      "EX",          1,    "A", "2024-01-01T08:00:00", "2024-01-01T08:00:00",     1L
    )
  ))

  from_attr <- add_administration(
    nif(imputation_rules = imputation_rules_void, silent = TRUE),
    sdtm_obj,
    "A",
    silent = TRUE
  )
  from_arg <- add_administration(
    nif(silent = TRUE),
    sdtm_obj,
    "A",
    imputation = imputation_rules_void,
    silent = TRUE
  )

  expect_identical(
    attr(from_attr, "imputation_rules"),
    imputation_rules_void
  )
  expect_identical(
    attr(from_arg, "imputation_rules"),
    imputation_rules_standard
  )
  expect_equal(from_attr, from_arg, ignore_attr = TRUE)
})


test_that("add_observation() uses nif imputation_rules when imputation is NULL", {
  sdtm_obj <- sdtm(list(
    dm = tibble::tribble(
      ~USUBJID, ~DOMAIN, ~SEX, ~ACTARMCD,             ~RFXSTDTC,              ~RFSTDTC,              ~RFENDTC, ~ACTARM,  ~STUDYID,
      "1",      "DM",    "M",  "A",       "2024-01-01T08:00:00", "2024-01-01T08:00:00", "2024-01-02T08:00:00", "Arm A", "Study 1"
    ),
    vs = tibble::tribble(
      ~USUBJID, ~DOMAIN, ~VSTESTCD, ~VSSTRESN,              ~VSDTC,
      "1",      "VS",    "HEIGHT",       170, "2024-01-01T08:00:00"
    ),
    ex = tibble::tribble(
      ~USUBJID, ~DOMAIN, ~EXDOSE, ~EXTRT,              ~EXSTDTC,              ~EXENDTC, ~EXSEQ,
      "1",      "EX",          1,    "A", "2024-01-01T08:00:00", "2024-01-01T08:00:00",     1L
    ),
    pc = tibble::tribble(
      ~USUBJID, ~DOMAIN, ~PCTESTCD,              ~PCDTC, ~PCSTRESN, ~PCSPEC,     ~PCTEST, ~PCELTM,
      "1",      "PC",          "A", "2024-01-01T08:00:00",       100,  "Spec", "Analyte A",  "PT0H"
    )
  ))

  nif_obj <- add_administration(
    nif(imputation_rules = imputation_rules_void, silent = TRUE),
    sdtm_obj,
    "A",
    silent = TRUE
  )
  out <- add_observation(nif_obj, sdtm_obj, "pc", "A", silent = TRUE)

  expect_identical(
    attr(out, "imputation_rules"),
    imputation_rules_void
  )
})
