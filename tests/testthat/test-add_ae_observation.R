# Test file for add_ae_observation function

test_that("add_ae_observation handles basic case correctly", {
  # Create test SDTM data
  test_ae <- tibble::tribble(
    ~USUBJID, ~AEDECOD, ~AESTDTC, ~AETOXGR, ~AESEQ,
    "SUBJ-001", "Headache", "2023-01-01T08:00", 2, 1,
    "SUBJ-002", "Headache", "2023-01-02T09:00", 1, 1,
    "SUBJ-003", "Nausea", "2023-01-01T10:00", 3, 1
  )

  test_dm <- tibble::tribble(
    ~USUBJID, ~RFSTDTC, ~ACTARMCD, ~SEX, ~RFENDTC,
    "SUBJ-001", "2023-01-01T00:00", "TRT1", "F", "2023-01-02T00:00",
    "SUBJ-002", "2023-01-01T00:00", "TRT1", "F", "2023-01-02T00:00",
    "SUBJ-003", "2023-01-01T00:00", "TRT1", "F", "2023-01-02T00:00"
  )

  test_vs <- tibble::tribble(
    ~USUBJID, ~VSTESTCD, ~VSSTRESN, ~VSBLFL,
    "SUBJ-001", "HEIGHT", 170, "Y",
    "SUBJ-001", "WEIGHT", 70, "Y",
    "SUBJ-002", "HEIGHT", 165, "Y",
    "SUBJ-002", "WEIGHT", 65, "Y",
    "SUBJ-003", "HEIGHT", 175, "Y",
    "SUBJ-003", "WEIGHT", 75, "Y"
  )

  test_ex <- tibble::tribble(
    ~USUBJID, ~EXTRT, ~EXDOSE, ~EXSTDTC, ~EXENDTC, ~EXSEQ,
    "SUBJ-001", "DRUG", 100, "2023-01-01T00:00", "2023-01-01T01:00", 1,
    "SUBJ-002", "DRUG", 100, "2023-01-01T00:00", "2023-01-01T01:00", 1,
    "SUBJ-003", "DRUG", 100, "2023-01-01T00:00", "2023-01-01T01:00", 1
  )

  test_sdtm <- new_sdtm(list(ae = test_ae, dm = test_dm, vs = test_vs, ex = test_ex))

  # Create base nif object
  base_nif <- new_nif() %>%
    add_administration(test_sdtm, "DRUG", analyte = "DRUG", silent = T)

  # Test adding AE observation
  result <- add_ae_observation(
    base_nif,
    test_sdtm,
    "Headache",
    parent = "DRUG",
    cmt = 2
  )

  # Check structure
  expect_s3_class(result, "nif")
  expect_true(all(c(
    "USUBJID", "DTC", "DV", "ANALYTE", "TIME", "CMT",
    "AMT", "PARENT", "EVID", "MDV"
  ) %in% names(result)))

  # Check content
  ae_rows <- result %>% filter(ANALYTE == "AE_Headache")
  expect_equal(nrow(ae_rows), 2) # Should have 2 Headache events
  expect_equal(ae_rows$DV, c(2, 1)) # Toxicity grades
  expect_equal(unique(ae_rows$PARENT), "DRUG")
  expect_equal(unique(ae_rows$CMT), 2)
})


test_that("add_ae_observation handles different ae_fields correctly", {
  test_ae <- tibble::tribble(
    ~USUBJID, ~AEDECOD, ~AELLT, ~AEHLT, ~AESTDTC, ~AETOXGR,
    "SUBJ-001", "Term1", "LLT1", "HLT1", "2023-01-01T08:00", 2,
    "SUBJ-002", "Term2", "LLT1", "HLT2", "2023-01-02T09:00", 1
  )

  test_dm <- tibble::tribble(
    ~USUBJID, ~RFSTDTC, ~ACTARMCD, ~SEX, ~RFENDTC,
    "SUBJ-001", "2023-01-01T00:00", "TRT1", "M", "2023-01-02T00:00",
    "SUBJ-002", "2023-01-01T00:00", "TRT1", "M", "2023-01-02T00:00"
  )

  test_vs <- tibble::tribble(
    ~USUBJID, ~VSTESTCD, ~VSSTRESN, ~VSBLFL,
    "SUBJ-001", "HEIGHT", 170, "Y",
    "SUBJ-001", "WEIGHT", 70, "Y",
    "SUBJ-002", "HEIGHT", 165, "Y",
    "SUBJ-002", "WEIGHT", 65, "Y"
  )

  test_ex <- tibble::tribble(
    ~USUBJID, ~EXTRT, ~EXDOSE, ~EXSTDTC, ~EXENDTC, ~EXSEQ,
    "SUBJ-001", "DRUG", 100, "2023-01-01T00:00", "2023-01-01T01:00", 1,
    "SUBJ-002", "DRUG", 100, "2023-01-01T00:00", "2023-01-01T01:00", 1
  )

  test_sdtm <- new_sdtm(list(ae = test_ae, dm = test_dm, vs = test_vs, ex = test_ex))
  base_nif <- new_nif() %>%
    add_administration(test_sdtm, "DRUG", analyte = "DRUG", silent = T)

  # Test with AELLT field
  result_llt <- add_ae_observation(
    base_nif,
    test_sdtm,
    "LLT1",
    ae_field = "AELLT",
    parent = "DRUG",
    cmt = 2
  )
  expect_equal(nrow(result_llt %>% filter(ANALYTE == "AE_LLT1")), 2)

  # Test with AEHLT field
  result_hlt <- add_ae_observation(
    base_nif,
    test_sdtm,
    "HLT1",
    ae_field = "AEHLT",
    parent = "DRUG",
    cmt = 2
  )
  expect_equal(nrow(result_hlt %>% filter(ANALYTE == "AE_HLT1")), 1)
})


test_that("add_ae_observation handles filters correctly", {
  test_ae <- tibble::tribble(
    ~USUBJID, ~AEDECOD, ~AESTDTC, ~AETOXGR, ~AESEQ,
    "SUBJ-001", "Headache", "2023-01-01T08:00", 2, 1,
    "SUBJ-002", "Headache", "2023-01-02T09:00", 1, 1
  )

  test_dm <- tibble::tribble(
    ~USUBJID, ~RFSTDTC, ~ACTARMCD, ~SEX, ~RFENDTC,
    "SUBJ-001", "2023-01-01T00:00", "SCRNFAIL", "M", "2023-01-02T00:00",
    "SUBJ-002", "2023-01-01T00:00", "TRT1", "M", "2023-01-02T00:00"
  )

  test_vs <- tibble::tribble(
    ~USUBJID, ~VSTESTCD, ~VSSTRESN, ~VSBLFL,
    "SUBJ-001", "HEIGHT", 170, "Y",
    "SUBJ-001", "WEIGHT", 70, "Y",
    "SUBJ-002", "HEIGHT", 165, "Y",
    "SUBJ-002", "WEIGHT", 65, "Y"
  )

  test_ex <- tibble::tribble(
    ~USUBJID, ~EXTRT, ~EXDOSE, ~EXSTDTC, ~EXENDTC, ~EXSEQ,
    "SUBJ-001", "DRUG", 100, "2023-01-01T00:00", "2023-01-01T01:00", 1,
    "SUBJ-002", "DRUG", 100, "2023-01-01T00:00", "2023-01-01T01:00", 1
  )

  test_sdtm <- new_sdtm(list(ae = test_ae, dm = test_dm, vs = test_vs, ex = test_ex))
  base_nif <- new_nif() %>%
    add_administration(test_sdtm, "DRUG", analyte = "DRUG", silent = T)

  # Test subject filter
  result <- add_ae_observation(
    base_nif,
    test_sdtm,
    "Headache",
    parent = "DRUG",
    cmt = 2
  )
  expect_equal(nrow(result %>% filter(ANALYTE == "AE_Headache")), 1) # Should exclude SCRNFAIL subject
  expect_equal(unique(result$USUBJID[result$ANALYTE == "AE_Headache"]), "SUBJ-002")

  # Test custom observation filter
  result_filtered <- add_ae_observation(
    base_nif,
    test_sdtm,
    "Headache",
    parent = "DRUG",
    cmt = 2,
    observation_filter = "AETOXGR > 1"
  )
  expect_equal(nrow(result_filtered %>% filter(ANALYTE == "AE_Headache")), 0) # No events with AETOXGR > 1 for non-SCRNFAIL subjects
})


test_that("add_ae_observation handles debug mode correctly", {
  test_ae <- tibble::tribble(
    ~USUBJID, ~AEDECOD, ~AESTDTC, ~AETOXGR, ~AESEQ,
    "SUBJ-001", "Headache", "2023-01-01T08:00", 2, 1
  )

  test_dm <- tibble::tribble(
    ~USUBJID, ~RFSTDTC, ~ACTARMCD, ~SEX, ~RFENDTC,
    "SUBJ-001", "2023-01-01T00:00", "TRT1", "F", "2023-01-02T00:00"
  )

  test_vs <- tibble::tribble(
    ~USUBJID, ~VSTESTCD, ~VSSTRESN, ~VSBLFL,
    "SUBJ-001", "HEIGHT", 170, "Y",
    "SUBJ-001", "WEIGHT", 70, "Y"
  )

  test_ex <- tibble::tribble(
    ~USUBJID, ~EXTRT, ~EXDOSE, ~EXSTDTC, ~EXENDTC, ~EXSEQ,
    "SUBJ-001", "DRUG", 100, "2023-01-01T00:00", "2023-01-01T01:00", 1
  )

  test_sdtm <- new_sdtm(list(ae = test_ae, dm = test_dm, vs = test_vs, ex = test_ex))
  base_nif <- new_nif() %>%
    add_administration(test_sdtm, "DRUG", analyte = "DRUG", silent = T)

  # Test with debug = TRUE
  result_debug <- add_ae_observation(
    base_nif,
    test_sdtm,
    "Headache",
    parent = "DRUG",
    cmt = 2,
    debug = TRUE
  )

  # Check if debug columns are present
  expect_true("SRC_DOMAIN" %in% names(result_debug))
  expect_true("SRC_SEQ" %in% names(result_debug))
  expect_equal(unique(result_debug$SRC_DOMAIN[result_debug$ANALYTE == "AE_Headache"]), "AE")
})


test_that("add_ae_observation handles keep parameter correctly", {
  test_ae <- tibble::tribble(
    ~USUBJID, ~AEDECOD, ~AESTDTC, ~AETOXGR, ~AESEQ, ~CUSTOM_COL,
    "SUBJ-001", "Headache", "2023-01-01T08:00", 2, 1, "Value1"
  )

  test_dm <- tibble::tribble(
    ~USUBJID, ~RFSTDTC, ~ACTARMCD, ~SEX, ~RFENDTC,
    "SUBJ-001", "2023-01-01T00:00", "TRT1", "F", "2023-01-02T00:00"
  )

  test_vs <- tibble::tribble(
    ~USUBJID, ~VSTESTCD, ~VSSTRESN, ~VSBLFL,
    "SUBJ-001", "HEIGHT", 170, "Y",
    "SUBJ-001", "WEIGHT", 70, "Y"
  )

  test_ex <- tibble::tribble(
    ~USUBJID, ~EXTRT, ~EXDOSE, ~EXSTDTC, ~EXENDTC, ~EXSEQ,
    "SUBJ-001", "DRUG", 100, "2023-01-01T00:00", "2023-01-01T01:00", 1
  )

  test_sdtm <- new_sdtm(list(ae = test_ae, dm = test_dm, vs = test_vs, ex = test_ex))
  base_nif <- new_nif() %>%
    add_administration(test_sdtm, "DRUG", analyte = "DRUG", silent = T)

  result <- add_ae_observation(
    base_nif,
    test_sdtm,
    "Headache",
    parent = "DRUG",
    cmt = 2,
    keep = "CUSTOM_COL"
  )

  expect_true("CUSTOM_COL" %in% names(result))
  expect_equal(result$CUSTOM_COL[result$ANALYTE == "AE_Headache"], "Value1")
})


test_that("add_ae_observation handles automatic parent and cmt assignment", {
  test_ae <- tibble::tribble(
    ~USUBJID, ~AEDECOD, ~AESTDTC, ~AETOXGR, ~AESEQ,
    "SUBJ-001", "Headache", "2023-01-01T08:00", 2, 1
  )

  test_dm <- tibble::tribble(
    ~USUBJID, ~RFSTDTC, ~ACTARMCD, ~SEX, ~RFENDTC,
    "SUBJ-001", "2023-01-01T00:00", "TRT1", "F", "2023-01-02T00:00"
  )

  test_vs <- tibble::tribble(
    ~USUBJID, ~VSTESTCD, ~VSSTRESN, ~VSBLFL,
    "SUBJ-001", "HEIGHT", 170, "Y",
    "SUBJ-001", "WEIGHT", 70, "Y"
  )

  test_ex <- tibble::tribble(
    ~USUBJID, ~EXTRT, ~EXDOSE, ~EXSTDTC, ~EXENDTC, ~EXSEQ,
    "SUBJ-001", "DRUG", 100, "2023-01-01T00:00", "2023-01-01T01:00", 1
  )

  test_sdtm <- new_sdtm(list(ae = test_ae, dm = test_dm, vs = test_vs, ex = test_ex))
  base_nif <- new_nif() %>%
    add_administration(test_sdtm, "DRUG", analyte = "DRUG", silent = T)

  # Test with NULL parent and cmt
  result <- add_ae_observation(
    base_nif,
    test_sdtm,
    "Headache",
    silent = T
  )

  # Check if parent was automatically assigned
  expect_equal(unique(result$PARENT[result$ANALYTE == "AE_Headache"]), "DRUG")

  # Check if cmt was automatically assigned
  expect_true(!is.na(unique(result$CMT[result$ANALYTE == "AE_Headache"])))
})
