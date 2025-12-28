# Helper function to create basic test SDTM
create_basic_sdtm <- function(ae_data, dm_data = NULL, vs_data = NULL) {
  if (is.null(dm_data)) {
    dm_data <- tibble::tribble(
      ~USUBJID, ~RFSTDTC, ~ACTARMCD, ~SEX,
      "SUBJ-001", "2023-01-01T00:00", "TRT1", "F",
      "SUBJ-002", "2023-01-01T00:00", "TRT1", "F"
    )
  }
  if (is.null(vs_data)) {
    vs_data <- tibble::tribble(
      ~USUBJID, ~VSTESTCD, ~VSSTRESN, ~VSBLFL,
      "SUBJ-001", "HEIGHT", 170, "Y",
      "SUBJ-001", "WEIGHT", 70, "Y",
      "SUBJ-002", "HEIGHT", 165, "Y",
      "SUBJ-002", "WEIGHT", 65, "Y"
    )
  }
  sdtm(list(ae = ae_data, dm = dm_data, vs = vs_data))
}

test_that("make_ae handles basic case correctly", {
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

  result <- make_ae(test_sdtm, "Headache")

  # Check structure
  expect_s3_class(result, "nif")
  expect_true(all(c(
    "USUBJID", "DTC", "DV", "ANALYTE", "TIME", "CMT",
    "AMT", "DOSE", "PARENT", "METABOLITE", "EVID", "MDV", "IMPUTATION", "TRTDY"
  ) %in% names(result)))

  # Check content
  expect_equal(nrow(result), 2)
  expect_equal(unique(result$ANALYTE), "AE_Headache")
  expect_equal(result$DV, c(2, 1))
  expect_equal(unique(result$EVID), 0)
  expect_equal(unique(result$MDV), 0)
  expect_equal(unique(result$AMT), 0)
  expect_equal(unique(result$METABOLITE), FALSE)
  expect_equal(unique(result$IMPUTATION), "")
})

test_that("make_ae creates correct output structure with all required columns", {
  test_ae <- tibble::tribble(
    ~USUBJID, ~AEDECOD, ~AESTDTC, ~AETOXGR,
    "SUBJ-001", "Headache", "2023-01-01T08:00", 2
  )

  test_sdtm <- create_basic_sdtm(test_ae)
  result <- make_ae(test_sdtm, "Headache", cmt = 3, parent = "DRUG")

  # Check all required columns exist
  required_cols <- c(
    "USUBJID", "DTC", "DV", "ANALYTE", "TIME", "CMT", "AMT", "DOSE",
    "PARENT", "METABOLITE", "EVID", "MDV", "IMPUTATION", "TRTDY"
  )
  expect_true(all(required_cols %in% names(result)))

  # Check default values
  expect_true(all(is.na(result$TIME)))
  expect_equal(unique(result$CMT), 3)
  expect_equal(unique(result$AMT), 0)
  expect_true(all(is.na(result$DOSE)))
  expect_equal(unique(result$PARENT), "DRUG")
  expect_equal(unique(result$METABOLITE), FALSE)
  expect_equal(unique(result$EVID), 0)
  expect_equal(unique(result$MDV), 0)
  expect_equal(unique(result$IMPUTATION), "")
})

test_that("make_ae validates sdtm object", {
  test_ae <- tibble::tribble(
    ~USUBJID, ~AEDECOD, ~AESTDTC, ~AETOXGR,
    "SUBJ-001", "Headache", "2023-01-01T08:00", 2
  )

  # Test with non-sdtm object
  expect_error(
    make_ae(list(ae = test_ae), "Headache"),
    "sdtm must be an sdtm object"
  )

  # Test with NULL
  expect_error(
    make_ae(NULL, "Headache"),
    "sdtm must be an sdtm object"
  )
})

test_that("make_ae validates character parameters", {
  test_ae <- tibble::tribble(
    ~USUBJID, ~AEDECOD, ~AESTDTC, ~AETOXGR,
    "SUBJ-001", "Headache", "2023-01-01T08:00", 2
  )
  test_sdtm <- create_basic_sdtm(test_ae)

  # Test with numeric ae_term
  expect_error(
    make_ae(test_sdtm, 123),
    "ae_term"
  )

  # Test with NULL ae_term
  expect_error(
    make_ae(test_sdtm, NULL),
    "ae_term"
  )

  # Test with numeric ae_field
  expect_error(
    make_ae(test_sdtm, "Headache", ae_field = 123),
    "ae_field"
  )
})

test_that("make_ae validates missing domains", {
  test_ae <- tibble::tribble(
    ~USUBJID, ~AEDECOD, ~AESTDTC, ~AETOXGR,
    "SUBJ-001", "Headache", "2023-01-01T08:00", 2
  )

  # Test missing AE domain
  test_sdtm_no_ae <- sdtm(list(
    dm = tibble::tribble(~USUBJID, ~RFSTDTC, ~ACTARMCD, ~SEX,
      "SUBJ-001", "2023-01-01T00:00", "TRT1", "F"
    ),
    vs = tibble::tribble(~USUBJID, ~VSTESTCD, ~VSSTRESN, ~VSBLFL,
      "SUBJ-001", "HEIGHT", 170, "Y"
    )
  ))
  expect_error(
    make_ae(test_sdtm_no_ae, "Headache"),
    "Domain AE not found"
  )

  # Test missing DM domain
  test_sdtm_no_dm <- sdtm(list(
    ae = test_ae,
    vs = tibble::tribble(~USUBJID, ~VSTESTCD, ~VSSTRESN, ~VSBLFL,
      "SUBJ-001", "HEIGHT", 170, "Y"
    )
  ))
  expect_error(
    make_ae(test_sdtm_no_dm, "Headache"),
    "Domain DM not found"
  )

  # Test missing VS domain
  test_sdtm_no_vs <- sdtm(list(
    ae = test_ae,
    dm = tibble::tribble(~USUBJID, ~RFSTDTC, ~ACTARMCD, ~SEX,
      "SUBJ-001", "2023-01-01T00:00", "TRT1", "F"
    )
  ))
  expect_error(
    make_ae(test_sdtm_no_vs, "Headache"),
    "Domain VS not found"
  )
})

test_that("make_ae validates required fields in AE domain", {
  # Test missing AESTDTC
  test_ae_no_dtc <- tibble::tribble(
    ~USUBJID, ~AEDECOD, ~AETOXGR,
    "SUBJ-001", "Headache", 2
  )
  test_sdtm <- create_basic_sdtm(test_ae_no_dtc)
  expect_error(
    make_ae(test_sdtm, "Headache"),
    "Required field\\(s\\) missing in AE domain.*AESTDTC"
  )

  # Test missing ae_field
  test_ae_no_field <- tibble::tribble(
    ~USUBJID, ~AESTDTC, ~AETOXGR,
    "SUBJ-001", "2023-01-01T08:00", 2
  )
  test_sdtm <- create_basic_sdtm(test_ae_no_field)
  expect_error(
    make_ae(test_sdtm, "Headache", ae_field = "AEDECOD"),
    "Required field\\(s\\) missing in AE domain.*AEDECOD"
  )
})

test_that("make_ae handles different ae_fields correctly", {
  test_ae <- tibble::tribble(
    ~USUBJID, ~AEDECOD, ~AELLT, ~AEHLT, ~AEHLGT, ~AESOC, ~AEBODSYS, ~AESTDTC, ~AETOXGR,
    "SUBJ-001", "Term1", "LLT1", "HLT1", "HLGT1", "SOC1", "BODSYS1", "2023-01-01T08:00", 2,
    "SUBJ-002", "Term2", "LLT1", "HLT2", "HLGT2", "SOC2", "BODSYS2", "2023-01-02T09:00", 1,
    "SUBJ-003", "Term3", "LLT2", "HLT1", "HLGT1", "SOC1", "BODSYS1", "2023-01-03T10:00", 3
  )

  test_dm <- tibble::tribble(
    ~USUBJID, ~RFSTDTC, ~ACTARMCD, ~SEX,
    "SUBJ-001", "2023-01-01T00:00", "TRT1", "M",
    "SUBJ-002", "2023-01-01T00:00", "TRT1", "M",
    "SUBJ-003", "2023-01-01T00:00", "TRT1", "M"
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

  # Test with AEDECOD (default)
  result_decod <- make_ae(test_sdtm, "Term1", ae_field = "AEDECOD")
  expect_equal(nrow(result_decod), 1)
  expect_equal(unique(result_decod$ANALYTE), "AE_Term1")

  # Test with AELLT
  result_llt <- make_ae(test_sdtm, "LLT1", ae_field = "AELLT")
  expect_equal(nrow(result_llt), 2)
  expect_equal(unique(result_llt$ANALYTE), "AE_LLT1")

  # Test with AEHLT
  result_hlt <- make_ae(test_sdtm, "HLT1", ae_field = "AEHLT")
  expect_equal(nrow(result_hlt), 2)
  expect_equal(unique(result_hlt$ANALYTE), "AE_HLT1")

  # Test with AEHLGT
  result_hlgt <- make_ae(test_sdtm, "HLGT1", ae_field = "AEHLGT")
  expect_equal(nrow(result_hlgt), 2)
  expect_equal(unique(result_hlgt$ANALYTE), "AE_HLGT1")

  # Test with AESOC
  result_soc <- make_ae(test_sdtm, "SOC1", ae_field = "AESOC")
  expect_equal(nrow(result_soc), 2)
  expect_equal(unique(result_soc$ANALYTE), "AE_SOC1")

  # Test with AEBODSYS
  result_bodsys <- make_ae(test_sdtm, "BODSYS1", ae_field = "AEBODSYS")
  expect_equal(nrow(result_bodsys), 2)
  expect_equal(unique(result_bodsys$ANALYTE), "AE_BODSYS1")
})

test_that("make_ae generates correct analyte names", {
  test_ae <- tibble::tribble(
    ~USUBJID, ~AEDECOD, ~AESTDTC, ~AETOXGR,
    "SUBJ-001", "Headache", "2023-01-01T08:00", 2,
    "SUBJ-002", "Nausea and Vomiting", "2023-01-02T09:00", 1
  )

  test_sdtm <- create_basic_sdtm(test_ae)

  # Test default analyte naming
  result1 <- make_ae(test_sdtm, "Headache")
  expect_equal(unique(result1$ANALYTE), "AE_Headache")

  # Test analyte naming with spaces
  result2 <- make_ae(test_sdtm, "Nausea and Vomiting")
  expect_equal(unique(result2$ANALYTE), "AE_Nausea_and_Vomiting")

  # Test custom analyte name
  result3 <- make_ae(test_sdtm, "Headache", analyte = "CUSTOM_AE")
  expect_equal(unique(result3$ANALYTE), "CUSTOM_AE")
})

test_that("make_ae handles coding table correctly", {
  test_ae <- tibble::tribble(
    ~USUBJID, ~AEDECOD, ~AESTDTC, ~AESEV,
    "SUBJ-001", "Headache", "2023-01-01T08:00", "MILD",
    "SUBJ-002", "Headache", "2023-01-02T09:00", "MODERATE",
    "SUBJ-003", "Headache", "2023-01-03T10:00", "SEVERE"
  )

  coding_table <- tibble::tribble(
    ~AESEV, ~DV,
    "MILD", 1,
    "MODERATE", 2,
    "SEVERE", 3
  )

  test_sdtm <- create_basic_sdtm(test_ae)
  result <- make_ae(test_sdtm, "Headache", coding_table = coding_table)

  expect_equal(nrow(result), 2) # SUBJ-003 is not part of the other domains!
  expect_equal(result$DV, c(1, 2))
})

test_that("make_ae validates coding table structure", {
  test_ae <- tibble::tribble(
    ~USUBJID, ~AEDECOD, ~AESTDTC, ~AESEV,
    "SUBJ-001", "Headache", "2023-01-01T08:00", "MILD"
  )

  test_sdtm <- create_basic_sdtm(test_ae)

  # Test coding table must be data.frame
  expect_error(
    make_ae(test_sdtm, "Headache", coding_table = list(AESEV = "MILD", DV = 1)),
    "coding table must be a data frame"
  )

  # Test coding table must have DV column
  coding_table_no_dv <- tibble::tribble(
    ~AESEV,
    "MILD"
  )
  expect_error(
    make_ae(test_sdtm, "Headache", coding_table = coding_table_no_dv),
    "coding table must include a DV column"
  )

  # Test DV must be numeric
  coding_table_char_dv <- tibble::tribble(
    ~AESEV, ~DV,
    "MILD", "ONE"
  )
  expect_error(
    make_ae(test_sdtm, "Headache", coding_table = coding_table_char_dv),
    "DV in the coding table must be numeric"
  )

  # Test coding table fields must exist in AE domain
  coding_table_unknown <- tibble::tribble(
    ~UNKNOWN_FIELD, ~DV,
    "VALUE", 1
  )
  expect_error(
    make_ae(test_sdtm, "Headache", coding_table = coding_table_unknown),
    "Fields.*not found in AE domain"
  )
})

test_that("make_ae requires AETOXGR or coding table", {
  test_ae <- tibble::tribble(
    ~USUBJID, ~AEDECOD, ~AESTDTC, ~AESEV,
    "SUBJ-001", "Headache", "2023-01-01T08:00", "MILD"
  )

  test_sdtm <- create_basic_sdtm(test_ae)

  # Test error when AETOXGR missing and no coding table
  expect_error(
    make_ae(test_sdtm, "Headache"),
    "AETOXGR not found in AE. Use a coding table"
  )
})

test_that("make_ae handles coding table with multiple fields", {
  test_ae <- tibble::tribble(
    ~USUBJID, ~AEDECOD, ~AESTDTC, ~AESEV, ~AESER,
    "SUBJ-001", "Headache", "2023-01-01T08:00", "MILD", "N",
    "SUBJ-002", "Headache", "2023-01-02T09:00", "MILD", "Y"
  )

  coding_table <- tibble::tribble(
    ~AESEV, ~AESER, ~DV,
    "MILD", "N", 1,
    "MILD", "Y", 2
  )

  test_sdtm <- create_basic_sdtm(test_ae)
  result <- make_ae(test_sdtm, "Headache", coding_table = coding_table)

  expect_equal(nrow(result), 2)
  expect_equal(result$DV, c(1, 2))
})

test_that("make_ae handles subject filters correctly", {
  test_ae <- tibble::tribble(
    ~USUBJID, ~AEDECOD, ~AESTDTC, ~AETOXGR,
    "SUBJ-001", "Headache", "2023-01-01T08:00", 2,
    "SUBJ-002", "Headache", "2023-01-02T09:00", 1,
    "SUBJ-003", "Headache", "2023-01-03T10:00", 3
  )

  test_dm <- tibble::tribble(
    ~USUBJID, ~RFSTDTC, ~ACTARMCD, ~SEX,
    "SUBJ-001", "2023-01-01T00:00", "SCRNFAIL", "M",
    "SUBJ-002", "2023-01-01T00:00", "TRT1", "M",
    "SUBJ-003", "2023-01-01T00:00", "NOTTRT", "M"
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

  # Test default subject filter (excludes SCRNFAIL and NOTTRT)
  result <- make_ae(test_sdtm, "Headache")
  expect_equal(nrow(result), 1)
  expect_equal(unique(result$USUBJID), "SUBJ-002")

  # Test custom subject filter
  result_custom <- make_ae(test_sdtm, "Headache", subject_filter = "ACTARMCD == 'TRT1'")
  expect_equal(nrow(result_custom), 1)
  expect_equal(unique(result_custom$USUBJID), "SUBJ-002")

  # Test filter that includes all subjects
  result_all <- make_ae(test_sdtm, "Headache", subject_filter = "TRUE")
  expect_equal(nrow(result_all), 3)
})

test_that("make_ae handles observation filters correctly", {
  test_ae <- tibble::tribble(
    ~USUBJID, ~AEDECOD, ~AESTDTC, ~AETOXGR,
    "SUBJ-001", "Headache", "2023-01-01T08:00", 1,
    "SUBJ-001", "Headache", "2023-01-02T09:00", 2,
    "SUBJ-002", "Headache", "2023-01-03T10:00", 3,
    "SUBJ-002", "Headache", "2023-01-04T11:00", 1
  )

  test_sdtm <- create_basic_sdtm(test_ae)

  # Test default observation filter (includes all)
  result_all <- make_ae(test_sdtm, "Headache", observation_filter = "TRUE")
  expect_equal(nrow(result_all), 4)

  # Test filter by AETOXGR
  result_filtered <- make_ae(test_sdtm, "Headache", observation_filter = "AETOXGR > 1")
  expect_equal(nrow(result_filtered), 2)
  expect_true(all(result_filtered$DV > 1))

  # Test filter that excludes all
  result_none <- make_ae(test_sdtm, "Headache", observation_filter = "AETOXGR > 5")
  expect_equal(nrow(result_none), 0)
})

test_that("make_ae calculates TRTDY correctly", {
  test_ae <- tibble::tribble(
    ~USUBJID, ~AEDECOD, ~AESTDTC, ~AETOXGR,
    "SUBJ-001", "Headache", "2023-01-01T08:00", 2,
    "SUBJ-001", "Headache", "2023-01-05T10:00", 1,
    "SUBJ-002", "Headache", "2023-01-03T12:00", 3
  )

  test_dm <- tibble::tribble(
    ~USUBJID, ~RFSTDTC, ~ACTARMCD, ~SEX,
    "SUBJ-001", "2023-01-01T00:00", "TRT1", "F",
    "SUBJ-002", "2023-01-01T00:00", "TRT1", "F"
  )

  test_vs <- tibble::tribble(
    ~USUBJID, ~VSTESTCD, ~VSSTRESN, ~VSBLFL,
    "SUBJ-001", "HEIGHT", 170, "Y",
    "SUBJ-001", "WEIGHT", 70, "Y",
    "SUBJ-002", "HEIGHT", 165, "Y",
    "SUBJ-002", "WEIGHT", 65, "Y"
  )

  test_sdtm <- sdtm(list(ae = test_ae, dm = test_dm, vs = test_vs))
  result <- make_ae(test_sdtm, "Headache")

  # Check TRTDY calculation
  subj1_rows <- result[result$USUBJID == "SUBJ-001", ]
  expect_equal(subj1_rows$TRTDY[1], 1) # Same day as RFSTDTC
  expect_equal(subj1_rows$TRTDY[2], 5) # 4 days after RFSTDTC

  subj2_rows <- result[result$USUBJID == "SUBJ-002", ]
  expect_equal(subj2_rows$TRTDY, 3) # 2 days after RFSTDTC
})

test_that("make_ae handles missing AESEQ correctly", {
  test_ae <- tibble::tribble(
    ~USUBJID, ~AEDECOD, ~AESTDTC, ~AETOXGR,
    "SUBJ-001", "Headache", "2023-01-01T08:00", 2
  )

  test_sdtm <- create_basic_sdtm(test_ae)
  result <- make_ae(test_sdtm, "Headache", keep = "SRC_SEQ")

  # SRC_SEQ should be NA when AESEQ is missing
  expect_true("SRC_SEQ" %in% names(result))
  expect_true(all(is.na(result$SRC_SEQ)))
})

test_that("make_ae handles AESEQ when present", {
  test_ae <- tibble::tribble(
    ~USUBJID, ~AEDECOD, ~AESTDTC, ~AETOXGR, ~AESEQ,
    "SUBJ-001", "Headache", "2023-01-01T08:00", 2, 1,
    "SUBJ-001", "Headache", "2023-01-02T09:00", 1, 2
  )

  test_sdtm <- create_basic_sdtm(test_ae)
  result <- make_ae(test_sdtm, "Headache", keep = "SRC_SEQ")

  expect_true("SRC_SEQ" %in% names(result))
  expect_equal(result$SRC_SEQ, c(1, 2))
})

test_that("make_ae handles empty results correctly", {
  test_ae <- tibble::tribble(
    ~USUBJID, ~AEDECOD, ~AESTDTC, ~AETOXGR,
    "SUBJ-001", "Headache", "2023-01-01T08:00", 2
  )

  test_sdtm <- create_basic_sdtm(test_ae)

  # Test non-existent AE term
  result_empty <- make_ae(test_sdtm, "NonexistentTerm")
  expect_equal(nrow(result_empty), 0)
  expect_s3_class(result_empty, "nif")
})

test_that("make_ae handles multiple AEs per subject correctly", {
  test_ae <- tibble::tribble(
    ~USUBJID, ~AEDECOD, ~AESTDTC, ~AETOXGR, ~AESEQ,
    "SUBJ-001", "Headache", "2023-01-01T08:00", 2, 1,
    "SUBJ-001", "Headache", "2023-01-05T10:00", 1, 2,
    "SUBJ-001", "Headache", "2023-01-10T14:00", 3, 3,
    "SUBJ-002", "Headache", "2023-01-02T09:00", 1, 1
  )

  test_sdtm <- create_basic_sdtm(test_ae)
  result <- make_ae(test_sdtm, "Headache")

  expect_equal(nrow(result), 4)
  expect_equal(sum(result$USUBJID == "SUBJ-001"), 3)
  expect_equal(sum(result$USUBJID == "SUBJ-002"), 1)
})

test_that("make_ae handles missing AETOXGR values", {
  test_ae <- tibble::tribble(
    ~USUBJID, ~AEDECOD, ~AESTDTC, ~AETOXGR,
    "SUBJ-001", "Headache", "2023-01-01T08:00", NA,
    "SUBJ-002", "Headache", "2023-01-02T09:00", 1,
    "SUBJ-003", "Headache", "2023-01-03T10:00", NA
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
  result <- make_ae(test_sdtm, "Headache")

  expect_equal(nrow(result), 3)
  expect_true(is.na(result$DV[result$USUBJID == "SUBJ-001"]))
  expect_equal(result$DV[result$USUBJID == "SUBJ-002"], 1)
  expect_true(is.na(result$DV[result$USUBJID == "SUBJ-003"]))
})

test_that("make_ae preserves specified columns with keep parameter", {
  test_ae <- tibble::tribble(
    ~USUBJID, ~AEDECOD, ~AESTDTC, ~AETOXGR, ~CUSTOM_COL, ~ANOTHER_COL,
    "SUBJ-001", "Headache", "2023-01-01T08:00", 2, "Value1", "ValueA",
    "SUBJ-002", "Headache", "2023-01-02T09:00", 1, "Value2", "ValueB"
  )

  test_sdtm <- create_basic_sdtm(test_ae)

  # Test single column
  result1 <- make_ae(test_sdtm, "Headache", keep = "CUSTOM_COL")
  expect_true("CUSTOM_COL" %in% names(result1))
  expect_equal(result1$CUSTOM_COL, c("Value1", "Value2"))

  # Test multiple columns
  result2 <- make_ae(test_sdtm, "Headache", keep = c("CUSTOM_COL", "ANOTHER_COL"))
  expect_true("CUSTOM_COL" %in% names(result2))
  expect_true("ANOTHER_COL" %in% names(result2))
  expect_equal(result2$CUSTOM_COL, c("Value1", "Value2"))
  expect_equal(result2$ANOTHER_COL, c("ValueA", "ValueB"))
})

test_that("make_ae validates keep parameter columns exist", {
  test_ae <- tibble::tribble(
    ~USUBJID, ~AEDECOD, ~AESTDTC, ~AETOXGR,
    "SUBJ-001", "Headache", "2023-01-01T08:00", 2
  )

  test_sdtm <- create_basic_sdtm(test_ae)

  # Test missing column in keep
  expect_error(
    make_ae(test_sdtm, "Headache", keep = "NONEXISTENT_COL"),
    "Column\\(s\\) specified in 'keep' not found in AE domain"
  )

  # Test multiple missing columns
  expect_error(
    make_ae(test_sdtm, "Headache", keep = c("NONEXISTENT_COL1", "NONEXISTENT_COL2")),
    "Column\\(s\\) specified in 'keep' not found in AE domain"
  )
})

test_that("make_ae handles compartment and parent parameters correctly", {
  test_ae <- tibble::tribble(
    ~USUBJID, ~AEDECOD, ~AESTDTC, ~AETOXGR,
    "SUBJ-001", "Headache", "2023-01-01T08:00", 2
  )

  test_sdtm <- create_basic_sdtm(test_ae)

  # Test with specific compartment and parent
  result <- make_ae(test_sdtm, "Headache", cmt = 2, parent = "DRUG")
  expect_equal(unique(result$CMT), 2)
  expect_equal(unique(result$PARENT), "DRUG")

  # Test with default values
  result_default <- make_ae(test_sdtm, "Headache")
  expect_true(is.na(unique(result_default$CMT)))
  expect_equal(unique(result_default$PARENT), "")
})

test_that("make_ae sets SRC_DOMAIN correctly", {
  test_ae <- tibble::tribble(
    ~USUBJID, ~AEDECOD, ~AESTDTC, ~AETOXGR,
    "SUBJ-001", "Headache", "2023-01-01T08:00", 2
  )

  test_sdtm <- create_basic_sdtm(test_ae)
  result <- make_ae(test_sdtm, "Headache", keep = "SRC_DOMAIN")

  expect_true("SRC_DOMAIN" %in% names(result))
  expect_equal(unique(result$SRC_DOMAIN), "AE")
})

test_that("make_ae works with complex real-world scenarios", {
  # Complex scenario with multiple subjects, multiple AEs, filters, and coding table
  test_ae <- tibble::tribble(
      ~USUBJID,   ~AEDECOD,           ~AESTDTC,     ~AESEV, ~AESER, ~AESEQ,
    "SUBJ-001", "Headache", "2023-01-05T08:00",     "MILD",    "N",      1,
    "SUBJ-001",   "Nausea", "2023-01-10T10:00", "MODERATE",    "Y",      2,
    "SUBJ-002", "Headache", "2023-01-03T09:00",   "SEVERE",    "Y",      1,
    "SUBJ-003", "Headache", "2023-01-07T11:00",     "MILD",    "N",      1,
    "SUBJ-003", "Headache", "2023-01-15T14:00", "MODERATE",    "N",      2
  )

  test_dm <- tibble::tribble(
      ~USUBJID,           ~RFSTDTC,  ~ACTARMCD, ~SEX,
    "SUBJ-001", "2023-01-01T00:00",     "TRT1",  "F",
    "SUBJ-002", "2023-01-01T00:00",     "TRT1",  "M",
    "SUBJ-003", "2023-01-01T00:00", "SCRNFAIL",  "F"
  )

  test_vs <- tibble::tribble(
      ~USUBJID, ~VSTESTCD, ~VSSTRESN, ~VSBLFL,
    "SUBJ-001",  "HEIGHT",       170,     "Y",
    "SUBJ-001",  "WEIGHT",        70,     "Y",
    "SUBJ-002",  "HEIGHT",       165,     "Y",
    "SUBJ-002",  "WEIGHT",        65,     "Y",
    "SUBJ-003",  "HEIGHT",       175,     "Y",
    "SUBJ-003",  "WEIGHT",        75,     "Y"
  )

  coding_table <- tibble::tribble(
        ~AESEV, ~DV,
        "MILD",   1,
    "MODERATE",   2,
      "SEVERE",   3
  )

  test_sdtm <- sdtm(list(ae = test_ae, dm = test_dm, vs = test_vs))

  # Test with coding table, filters, and keep parameter
  result <- make_ae(
    test_sdtm,
    "Headache",
    coding_table = coding_table,
    observation_filter = "AESER == 'N'",
    keep = "AESER",
    cmt = 5,
    parent = "TEST_DRUG"
  )

  # Should exclude SUBJ-003 (SCRNFAIL) and only include AEs with AESER == 'N'
  expect_equal(nrow(result), 1)
  expect_equal(unique(result$USUBJID), c("SUBJ-001"))
  expect_true(all(result$AESER == "N"))
  expect_equal(unique(result$CMT), 5)
  expect_equal(unique(result$PARENT), "TEST_DRUG")
  expect_equal(result$DV, c(1)) # MILD from coding table
})
