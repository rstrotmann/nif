# Test file for make_ae function

test_that("make_ae handles basic case correctly", {
  # Create minimal test SDTM data
  test_ae <- tibble::tribble(
    ~USUBJID, ~AEDECOD, ~AESTDTC, ~AETOXGR, ~AESEQ,
    "SUBJ-001", "Headache", "2023-01-01T08:00", 2, 1,
    "SUBJ-002", "Headache", "2023-01-02T09:00", 1, 1,
    "SUBJ-003", "Nausea", "2023-01-01T10:00", 3, 1
  )

  test_dm <- tibble::tribble(
    ~USUBJID, ~RFSTDTC, ~ACTARMCD, ~SEX,
    "SUBJ-001", "2023-01-01T00:00", "TRT1", "F",
    "SUBJ-002", "2023-01-01T00:00", "TRT1", "F",
    "SUBJ-003", "2023-01-01T00:00", "TRT1", "F"
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

  test_sdtm <- sdtm(list(ae = test_ae, dm = test_dm, vs = test_vs))

  # Test basic functionality
  result <- make_ae(test_sdtm, "Headache")

  # Check structure
  expect_s3_class(result, "nif")
  expect_true(all(c(
    "USUBJID", "DTC", "DV", "ANALYTE", "TIME", "CMT",
    "AMT", "PARENT", "EVID", "MDV"
  ) %in% names(result)))

  # Check content
  expect_equal(nrow(result), 2) # Should have 2 Headache events
  expect_equal(unique(result$ANALYTE), "AE_Headache")
  expect_equal(result$DV, c(2, 1)) # Toxicity grades
})


test_that("make_ae handles different ae_fields correctly", {
  test_ae <- tibble::tribble(
    ~USUBJID, ~AEDECOD, ~AELLT, ~AEHLT, ~AESTDTC, ~AETOXGR,
    "SUBJ-001", "Term1", "LLT1", "HLT1", "2023-01-01T08:00", 2,
    "SUBJ-002", "Term2", "LLT1", "HLT2", "2023-01-02T09:00", 1
  )

  test_dm <- tibble::tribble(
    ~USUBJID, ~RFSTDTC, ~ACTARMCD, ~SEX,
    "SUBJ-001", "2023-01-01T00:00", "TRT1", "M",
    "SUBJ-002", "2023-01-01T00:00", "TRT1", "M"
  )

  test_vs <- tibble::tribble(
    ~USUBJID, ~VSTESTCD, ~VSSTRESN, ~VSBLFL,
    "SUBJ-001", "HEIGHT", 170, "Y",
    "SUBJ-001", "WEIGHT", 70, "Y",
    "SUBJ-002", "HEIGHT", 165, "Y",
    "SUBJ-002", "WEIGHT", 65, "Y"
  )

  test_sdtm <- sdtm(list(ae = test_ae, dm = test_dm, vs = test_vs))

  # Test with AELLT field
  result_llt <- make_ae(test_sdtm, "LLT1", ae_field = "AELLT")
  expect_equal(nrow(result_llt), 2)
  expect_equal(unique(result_llt$ANALYTE), "AE_LLT1")

  # Test with AEHLT field
  result_hlt <- make_ae(test_sdtm, "HLT1", ae_field = "AEHLT")
  expect_equal(nrow(result_hlt), 1)
  expect_equal(unique(result_hlt$ANALYTE), "AE_HLT1")
})


test_that("make_ae handles filters correctly", {
  test_ae <- tibble::tribble(
    ~USUBJID, ~AEDECOD, ~AESTDTC, ~AETOXGR, ~AESEQ,
    "SUBJ-001", "Headache", "2023-01-01T08:00", 2, 1,
    "SUBJ-002", "Headache", "2023-01-02T09:00", 1, 1
  )

  test_dm <- tibble::tribble(
    ~USUBJID, ~RFSTDTC, ~ACTARMCD,
    "SUBJ-001", "2023-01-01T00:00", "SCRNFAIL",
    "SUBJ-002", "2023-01-01T00:00", "TRT1"
  ) %>% mutate(SEX = "M")

  test_vs <- tibble::tribble(
    ~USUBJID, ~VSTESTCD, ~VSSTRESN,
    "SUBJ-001", "HEIGHT", 170,
    "SUBJ-001", "WEIGHT", 70,
    "SUBJ-002", "HEIGHT", 165,
    "SUBJ-002", "WEIGHT", 65
  ) %>% mutate(VSBLFL = "Y")

  test_sdtm <- sdtm(list(ae = test_ae, dm = test_dm, vs = test_vs))

  # Test subject filter
  result <- make_ae(test_sdtm, "Headache")
  expect_equal(nrow(result), 1) # Should exclude SCRNFAIL subject
  expect_equal(unique(result$USUBJID), "SUBJ-002")

  # Test custom observation filter
  result_filtered <- make_ae(test_sdtm, "Headache",
    observation_filter = "AETOXGR > 1"
  )
  expect_equal(nrow(result_filtered), 0) # No events with AETOXGR > 1 for non-SCRNFAIL subjects
})


test_that("make_ae handles missing data correctly", {
  test_ae <- tibble::tribble(
    ~USUBJID, ~AEDECOD, ~AESTDTC, ~AETOXGR,
    "SUBJ-001", "Headache", "2023-01-01T08:00", NA,
    "SUBJ-002", "Headache", "2023-01-02T09:00", 1
  )

  test_dm <- tibble::tribble(
    ~USUBJID, ~RFSTDTC, ~ACTARMCD,
    "SUBJ-001", "2023-01-01T00:00", "TRT1",
    "SUBJ-002", "2023-01-01T00:00", "TRT1"
  ) %>% mutate(SEX = "M")

  test_vs <- tibble::tribble(
    ~USUBJID, ~VSTESTCD, ~VSSTRESN,
    "SUBJ-001", "HEIGHT", 170,
    "SUBJ-001", "WEIGHT", 70,
    "SUBJ-002", "HEIGHT", 165,
    "SUBJ-002", "WEIGHT", 65
  ) %>% mutate(VSBLFL = "Y")

  test_sdtm <- sdtm(list(ae = test_ae, dm = test_dm, vs = test_vs))

  result <- make_ae(test_sdtm, "Headache")
  expect_true(is.na(result$DV[result$USUBJID == "SUBJ-001"]))
})


test_that("make_ae handles errors appropriately", {
  # Test missing AE domain
  test_sdtm_no_ae <- sdtm(list(
    dm = tibble::tibble(),
    vs = tibble::tibble()
  ))
  expect_error(
    make_ae(test_sdtm_no_ae, "Headache"),
    "Domain AE not found!"
  )

  # Test invalid ae_field
  test_ae <- tibble::tribble(
    ~USUBJID, ~AEDECOD, ~AESTDTC, ~AETOXGR,
    "SUBJ-001", "Headache", "2023-01-01T08:00", 2
  )

  test_sdtm <- sdtm(list(
    ae = test_ae,
    dm = tibble::tribble(
      ~USUBJID, ~RFSTDTC, ~ACTARMCD, ~SEX,
      "SUBJ-001", "2023-01-01T00:00", "TRT1", "F"
    ),
    vs = tibble::tribble(
      ~USUBJID, ~VSTESTCD, ~VSSTRESN, ~VSBLFL,
      "SUBJ-001", "HEIGHT", 170, "Y"
    )
  ))

  # Test non-existent AE term
  result_empty <- make_ae(test_sdtm, "NonexistentTerm")
  expect_equal(nrow(result_empty), 0)
})


test_that("make_ae handles compartment and parent parameters correctly", {
  test_ae <- tibble::tribble(
    ~USUBJID, ~AEDECOD, ~AESTDTC, ~AETOXGR,
    "SUBJ-001", "Headache", "2023-01-01T08:00", 2
  )

  test_dm <- tibble::tribble(
    ~USUBJID, ~RFSTDTC, ~ACTARMCD, ~SEX,
    "SUBJ-001", "2023-01-01T00:00", "TRT1", "F"
  )

  test_vs <- tibble::tribble(
    ~USUBJID, ~VSTESTCD, ~VSSTRESN, ~VSBLFL,
    "SUBJ-001", "HEIGHT", 170, "Y",
    "SUBJ-001", "WEIGHT", 70, "Y"
  )

  test_sdtm <- sdtm(list(ae = test_ae, dm = test_dm, vs = test_vs))

  # Test with specific compartment and parent
  result <- make_ae(test_sdtm, "Headache", cmt = 2, parent = "DRUG")
  expect_equal(unique(result$CMT), 2)
  expect_equal(unique(result$PARENT), "DRUG")
})


test_that("make_ae preserves specified columns with keep parameter", {
  test_ae <- tibble::tribble(
    ~USUBJID, ~AEDECOD, ~AESTDTC, ~AETOXGR, ~CUSTOM_COL,
    "SUBJ-001", "Headache", "2023-01-01T08:00", 2, "Value1"
  )

  test_dm <- tibble::tribble(
    ~USUBJID, ~RFSTDTC, ~ACTARMCD, ~SEX,
    "SUBJ-001", "2023-01-01T00:00", "TRT1", "F"
  )

  test_vs <- tibble::tribble(
    ~USUBJID, ~VSTESTCD, ~VSSTRESN, ~VSBLFL,
    "SUBJ-001", "HEIGHT", 170, "Y",
    "SUBJ-001", "WEIGHT", 70, "Y"
  )

  test_sdtm <- sdtm(list(ae = test_ae, dm = test_dm, vs = test_vs))

  result <- make_ae(test_sdtm, "Headache", keep = "CUSTOM_COL")
  expect_true("CUSTOM_COL" %in% names(result))
  expect_equal(result$CUSTOM_COL, "Value1")
})
