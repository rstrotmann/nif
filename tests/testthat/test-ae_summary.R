# Test file for ae_summary function

test_that("ae_summary handles basic case correctly", {
  # Create test SDTM data with AE domain
  test_ae <- tibble::tribble(
    ~USUBJID, ~AETERM, ~AESOC, ~AEDECOD, ~AETOXGR,
    "SUBJ-001", "Headache", "Nervous system disorders", "Headache", 1,
    "SUBJ-001", "Nausea", "Gastrointestinal disorders", "Nausea", 2,
    "SUBJ-002", "Headache", "Nervous system disorders", "Headache", 1,
    "SUBJ-002", "Fatigue", "General disorders", "Fatigue", 1
  )

  test_sdtm <- sdtm(list(ae = test_ae))

  # Test default parameters (AESOC level)
  result <- ae_summary(test_sdtm)

  # Check output structure
  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 3) # Three unique AESOC terms
  expect_true(all(c("AESOC", "n", "n_subj") %in% names(result)))

  # Check counts
  expect_equal(result$n[result$AESOC == "Nervous system disorders"], 2)
  expect_equal(result$n_subj[result$AESOC == "Nervous system disorders"], 2)
})


test_that("ae_summary works with different levels", {
  # Create test SDTM data
  test_ae <- tibble::tribble(
    ~USUBJID, ~AETERM, ~AESOC, ~AEDECOD, ~AETOXGR,
    "SUBJ-001", "Headache", "Nervous system disorders", "Headache", 1,
    "SUBJ-001", "Nausea", "Gastrointestinal disorders", "Nausea", 2,
    "SUBJ-002", "Headache", "Nervous system disorders", "Headache", 1
  )

  test_sdtm <- sdtm(list(ae = test_ae))

  # Test AETERM level
  result_term <- ae_summary(test_sdtm, level = "AETERM")
  expect_equal(nrow(result_term), 2) # Two unique AETERM terms
  expect_equal(result_term$n[result_term$AETERM == "Headache"], 2)

  # Test AEDECOD level
  result_decod <- ae_summary(test_sdtm, level = "AEDECOD")
  expect_equal(nrow(result_decod), 2) # Two unique AEDECOD terms
})


test_that("ae_summary handles show_cd parameter", {
  # Create test SDTM data with codes
  test_ae <- tibble::tribble(
    ~USUBJID, ~AETERM, ~AESOC, ~AESOCCD, ~AEDECOD, ~AEDECODCD,
    "SUBJ-001", "Headache", "Nervous system disorders", "10029147", "Headache", "10019211",
    "SUBJ-002", "Headache", "Nervous system disorders", "10029147", "Headache", "10019211"
  )

  test_sdtm <- sdtm(list(ae = test_ae))

  # Test with show_cd = TRUE
  result <- ae_summary(test_sdtm, level = "AESOC", show_cd = TRUE)
  expect_true("AESOCCD" %in% names(result))
  expect_equal(result$AESOCCD[1], "10029147")
})


test_that("ae_summary handles grouping", {
  # Create test SDTM data with grouping variable
  test_ae <- tibble::tribble(
    ~USUBJID, ~AETERM, ~AESOC, ~SEX,
    "SUBJ-001", "Headache", "Nervous system disorders", "M",
    "SUBJ-001", "Nausea", "Gastrointestinal disorders", "M",
    "SUBJ-002", "Headache", "Nervous system disorders", "F"
  )

  test_sdtm <- sdtm(list(ae = test_ae))

  # Test with grouping
  result <- ae_summary(test_sdtm, group = "SEX")
  expect_true("SEX" %in% names(result))
  expect_equal(nrow(result), 3)
})


test_that("ae_summary handles ordering", {
  # Create test SDTM data
  test_ae <- tibble::tribble(
    ~USUBJID, ~AETERM, ~AESOC,
    "SUBJ-001", "Headache", "Nervous system disorders",
    "SUBJ-001", "Nausea", "Gastrointestinal disorders",
    "SUBJ-002", "Headache", "Nervous system disorders",
    "SUBJ-003", "Headache", "Nervous system disorders"
  )

  test_sdtm <- sdtm(list(ae = test_ae))

  # Test order_by_subj = TRUE
  result <- ae_summary(test_sdtm, order_by_subj = TRUE)
  expect_equal(result$AESOC[1], "Nervous system disorders") # Should be first as it affects most subjects
})


test_that("ae_summary handles filtering", {
  # Create test SDTM data
  test_ae <- tibble::tribble(
    ~USUBJID, ~AETERM, ~AESOC, ~AETOXGR,
    "SUBJ-001", "Headache", "Nervous system disorders", 1,
    "SUBJ-001", "Nausea", "Gastrointestinal disorders", 2,
    "SUBJ-002", "Headache", "Nervous system disorders", 3
  )

  test_sdtm <- sdtm(list(ae = test_ae))

  # Test with filter for severe AEs (AETOXGR > 1)
  result <- ae_summary(test_sdtm, ae_filter = "AETOXGR > 1")
  expect_equal(nrow(result), 2) # Only Nausea and severe Headache
  expect_true(all(result$n_subj <= 2)) # No more than 2 subjects per SOC
})


test_that("ae_summary handles invalid inputs", {
  # Create test SDTM data
  test_ae <- tibble::tribble(
    ~USUBJID, ~AETERM, ~AESOC,
    "SUBJ-001", "Headache", "Nervous system disorders"
  )

  test_sdtm <- sdtm(list(ae = test_ae))

  # Test with invalid level
  expect_error(
    ae_summary(test_sdtm, level = "INVALID"),
    "Invalid level\\(s\\): INVALID"
  )

  # Test with non-SDTM object
  expect_error(
    ae_summary(data.frame()),
    "Input must be an SDTM object"
  )
})


test_that("ae_summary handles empty data", {
  # Create empty AE domain
  test_ae <- tibble::tribble(
    ~USUBJID, ~AETERM, ~AESOC
  )

  test_sdtm <- sdtm(list(ae = test_ae))

  # Test with empty data
  result <- ae_summary(test_sdtm)
  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 0)
})


test_that("ae_summary validates SDTM object structure", {
  # Test with missing AE domain
  test_sdtm <- sdtm(list(dm = data.frame()))
  expect_error(
    ae_summary(test_sdtm),
    "AE domain not found in SDTM data"
  )

  # Test with missing required columns
  test_ae <- tibble::tribble(
    ~USUBJID, ~AETERM # Missing AESOC
  )
  test_sdtm <- sdtm(list(ae = test_ae))
  expect_error(
    ae_summary(test_sdtm),
    "Missing required columns in AE domain: AESOC"
  )

  # Test with invalid group variable
  test_ae <- tibble::tribble(
    ~USUBJID, ~AETERM, ~AESOC
  )
  test_sdtm <- sdtm(list(ae = test_ae))
  expect_error(
    ae_summary(test_sdtm, group = "INVALID_COL"),
    "Group variable 'INVALID_COL' not found in AE domain"
  )

  # Test with missing code columns when show_cd = TRUE
  test_ae <- tibble::tribble(
    ~USUBJID, ~AETERM, ~AESOC # Missing AESOCCD
  )
  test_sdtm <- sdtm(list(ae = test_ae))
  expect_error(
    ae_summary(test_sdtm, show_cd = TRUE),
    "Missing code columns: AESOCCD"
  )

  # Test with invalid filter expression
  test_ae <- tibble::tribble(
    ~USUBJID, ~AETERM, ~AESOC
  )
  test_sdtm <- sdtm(list(ae = test_ae))
  expect_error(
    ae_summary(test_sdtm, ae_filter = "invalid syntax"),
    "Invalid filter expression"
  )
})
