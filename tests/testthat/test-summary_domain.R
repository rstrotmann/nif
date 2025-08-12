# Test file for summary.domain() function

test_that("summary.domain handles valid domain objects correctly", {
  # Create a valid PC domain object for testing
  test_pc <- tibble::tribble(
    ~USUBJID, ~DOMAIN, ~STUDYID, ~PCSEQ, ~PCTESTCD, ~PCTEST, ~PCCAT, ~PCSCAT, ~PCTPT, ~PCTPTNUM, ~PCELTM, ~EPOCH, ~VISIT,
    "SUBJ-001", "PC", "STUDY-001", 1, "DRUGA", "Drug A", "PK", "PLASMA", "PRE", 0, "PT0H", "TREATMENT", "SCREENING",
    "SUBJ-001", "PC", "STUDY-001", 2, "DRUGA", "Drug A", "PK", "PLASMA", "1HR", 1, "PT1H", "TREATMENT", "DAY1",
    "SUBJ-002", "PC", "STUDY-001", 3, "DRUGA", "Drug A", "PK", "PLASMA", "2HR", 2, "PT2H", "TREATMENT", "DAY1"
  )
  class(test_pc) <- c("domain", "data.frame")

  # Call summary
  result <- summary(test_pc, silent = TRUE)

  # Check the summary object is created with correct class
  expect_s3_class(result, "summary_domain")

  # Check key properties
  expect_equal(result$domain, "PC")
  expect_equal(result$study, "STUDY-001")
  expect_equal(result$n_obs, 3)
  expect_equal(nrow(result$subjects), 2)
  expect_equal(nrow(result$test), 1)
  expect_equal(nrow(result$tpt), 3)
  expect_equal(nrow(result$epoch), 1)
  expect_equal(nrow(result$visit), 2)
  expect_equal(nrow(result$category), 1)
})

test_that("summary.domain handles domains without test fields gracefully", {
  # Create a DM domain object (no test fields)
  test_dm <- tibble::tribble(
    ~USUBJID, ~DOMAIN, ~STUDYID, ~AGE, ~SEX, ~RACE,
    "SUBJ-001", "DM", "STUDY-001", 25, "M", "WHITE",
    "SUBJ-002", "DM", "STUDY-001", 30, "F", "BLACK"
  )
  class(test_dm) <- c("domain", "data.frame")

  # Call summary
  result <- summary(test_dm, silent = TRUE)

  # Check the summary object is created correctly
  expect_s3_class(result, "summary_domain")
  expect_equal(result$domain, "DM")
  expect_equal(result$study, "STUDY-001")
  expect_equal(result$n_obs, 2)
  expect_equal(nrow(result$subjects), 2)

  # Test fields should be NULL for DM domain
  expect_null(result$test)
  expect_null(result$observations)
  expect_null(result$tpt)
  expect_null(result$category)
})

test_that("summary.domain handles domains with time points correctly", {
  # Create a PC domain object with time points
  test_pc <- tibble::tribble(
    ~USUBJID, ~DOMAIN, ~PCSEQ, ~PCTESTCD, ~PCTEST, ~PCTPT, ~PCTPTNUM, ~PCELTM,
    "SUBJ-001", "PC", 1, "DRUGA", "Drug A", "PRE", 0, "PT0H",
    "SUBJ-001", "PC", 2, "DRUGA", "Drug A", "1HR", 1, "PT1H",
    "SUBJ-001", "PC", 3, "DRUGA", "Drug A", "2HR", 2, "PT2H"
  )
  class(test_pc) <- c("domain", "data.frame")

  # Call summary
  result <- summary(test_pc, silent = TRUE)

  # Check time point information
  expect_equal(nrow(result$tpt), 3)
  expect_equal(result$tpt$PCTPT, c("PRE", "1HR", "2HR"))
  expect_equal(result$tpt$PCTPTNUM, c(0, 1, 2))
  expect_equal(result$tpt$PCELTM, c("PT0H", "PT1H", "PT2H"))
})

test_that("summary.domain handles domains with epochs correctly", {
  # Create a domain object with epochs
  test_domain <- tibble::tribble(
    ~USUBJID, ~DOMAIN, ~PCSEQ, ~PCTESTCD, ~PCTEST, ~EPOCH,
    "SUBJ-001", "PC", 1, "DRUGA", "Drug A", "SCREENING",
    "SUBJ-001", "PC", 2, "DRUGA", "Drug A", "TREATMENT",
    "SUBJ-002", "PC", 3, "DRUGA", "Drug A", "SCREENING"
  )
  class(test_domain) <- c("domain", "data.frame")

  # Call summary
  result <- summary(test_domain, silent = TRUE)

  # Check epoch information
  expect_equal(nrow(result$epoch), 2)
  expect_equal(sort(result$epoch$EPOCH), c("SCREENING", "TREATMENT"))
})

test_that("summary.domain handles domains with categories correctly", {
  # Create a domain object with multiple category fields
  test_domain <- tibble::tribble(
    ~USUBJID, ~DOMAIN, ~PCSEQ, ~PCTESTCD, ~PCTEST, ~PCCAT, ~PCSCAT, ~VISIT,
    "SUBJ-001", "PC", 1, "DRUGA", "Drug A", "PK", "PLASMA", "DAY1",
    "SUBJ-001", "PC", 2, "DRUGA", "Drug A", "PK", "URINE", "DAY1",
    "SUBJ-002", "PC", 3, "DRUGA", "Drug A", "PK", "PLASMA", "DAY1"
  )
  class(test_domain) <- c("domain", "data.frame")

  # Call summary
  result <- summary(test_domain, silent = TRUE)

  # Check category information
  expect_equal(nrow(result$category), 2)
  expect_equal(sort(result$category$PCCAT), c("PK", "PK"))
  expect_equal(sort(result$category$PCSCAT), c("PLASMA", "URINE"))
})

test_that("summary.domain handles domains with observations correctly", {
  # Create a domain object with test codes
  test_domain <- tibble::tribble(
    ~USUBJID, ~DOMAIN, ~PCSEQ, ~PCTESTCD, ~PCTEST, ~PCCAT,
    "SUBJ-001", "PC", 1, "DRUGA", "Drug A", "PK",
    "SUBJ-001", "PC", 2, "DRUGA", "Drug A", "PK",
    "SUBJ-002", "PC", 3, "DRUGA", "Drug A", "PK"
  )
  class(test_domain) <- c("domain", "data.frame")

  # Call summary
  result <- summary(test_domain, silent = TRUE)

  # Check observations summary
  expect_equal(nrow(result$observations), 1)
  expect_equal(result$observations$PCTESTCD, "DRUGA")
  expect_equal(result$observations$n, 3)
})

test_that("summary.domain handles domains with PCSPEC field", {
  # Create a domain object with PCSPEC field
  test_domain <- tibble::tribble(
    ~USUBJID, ~DOMAIN, ~PCSEQ, ~PCTESTCD, ~PCTEST, ~PCSPEC,
    "SUBJ-001", "PC", 1, "DRUGA", "Drug A", "PLASMA",
    "SUBJ-002", "PC", 2, "DRUGA", "Drug A", "PLASMA"
  )
  class(test_domain) <- c("domain", "data.frame")

  # Call summary
  result <- summary(test_domain, silent = TRUE)

  # Check that PCSPEC is included in test summary
  expect_equal(nrow(result$test), 1)
  expect_equal(result$test$PCSPEC, "PLASMA")
})

test_that("summary.domain handles domains with FAST field", {
  # Create a domain object with FAST field
  test_domain <- tibble::tribble(
    ~USUBJID, ~DOMAIN, ~PCSEQ, ~PCTESTCD, ~PCTEST, ~PCCAT, ~PCFAST,
    "SUBJ-001", "PC", 1, "DRUGA", "Drug A", "PK", "Y",
    "SUBJ-002", "PC", 2, "DRUGA", "Drug A", "PK", "N"
  )
  class(test_domain) <- c("domain", "data.frame")

  # Call summary
  result <- summary(test_domain, silent = TRUE)

  # Check that FAST field is included in test summary
  expect_equal(nrow(result$test), 2)
  expect_equal(sort(result$test$PCFAST), c("N", "Y"))
})

test_that("summary.domain handles domains with VISIT field", {
  # Create a domain object with VISIT field
  test_domain <- tibble::tribble(
    ~USUBJID, ~DOMAIN, ~PCSEQ, ~PCTESTCD, ~PCTEST, ~VISIT,
    "SUBJ-001", "PC", 1, "DRUGA", "Drug A", "SCREENING",
    "SUBJ-001", "PC", 2, "DRUGA", "Drug A", "DAY1",
    "SUBJ-002", "PC", 3, "DRUGA", "Drug A", "SCREENING"
  )
  class(test_domain) <- c("domain", "data.frame")

  # Call summary
  result <- summary(test_domain, silent = TRUE)

  # Check visit information
  expect_equal(nrow(result$visit), 2)
  expect_equal(sort(result$visit$VISIT), c("DAY1", "SCREENING"))
})

test_that("summary.domain handles domains with SUBJID field", {
  # Create a domain object with both USUBJID and SUBJID
  test_domain <- tibble::tribble(
    ~USUBJID, ~SUBJID, ~DOMAIN, ~PCSEQ, ~PCTESTCD, ~PCTEST,
    "STUDY-001-001", "001", "PC", 1, "DRUGA", "Drug A",
    "STUDY-001-002", "002", "PC", 2, "DRUGA", "Drug A"
  )
  class(test_domain) <- c("domain", "data.frame")

  # Call summary
  result <- summary(test_domain, silent = TRUE)

  # Check subjects information includes both ID fields
  expect_equal(nrow(result$subjects), 2)
  expect_equal(ncol(result$subjects), 2)
  expect_true("USUBJID" %in% names(result$subjects))
  expect_true("SUBJID" %in% names(result$subjects))
})

test_that("summary.domain handles domains with only USUBJID field", {
  # Create a domain object with only USUBJID
  test_domain <- tibble::tribble(
    ~USUBJID, ~DOMAIN, ~PCSEQ, ~PCTESTCD, ~PCTEST,
    "STUDY-001-001", "PC", 1, "DRUGA", "Drug A",
    "STUDY-001-002", "PC", 2, "DRUGA", "Drug A"
  )
  class(test_domain) <- c("domain", "data.frame")

  # Call summary
  result <- summary(test_domain, silent = TRUE)

  # Check subjects information
  expect_equal(nrow(result$subjects), 2)
  expect_equal(ncol(result$subjects), 1)
  expect_true("USUBJID" %in% names(result$subjects))
})

test_that("summary.domain handles domains with multiple DOMAIN values", {
  # Create a domain object with multiple DOMAIN values (should error)
  test_domain <- tibble::tribble(
    ~USUBJID, ~DOMAIN, ~PCSEQ, ~PCTESTCD, ~PCTEST,
    "SUBJ-001", "PC", 1, "DRUGA", "Drug A",
    "SUBJ-002", "LB", 1, "DRUGA", "Drug A"
  )
  class(test_domain) <- c("domain", "data.frame")

  # Should error due to multiple domains
  expect_error(
    summary(test_domain, silent = TRUE),
    "Multiple domain values found"
  )
})

test_that("summary.domain handles domains with empty DOMAIN column", {
  # Create a domain object with empty DOMAIN column
  test_domain <- tibble::tribble(
    ~USUBJID, ~DOMAIN, ~PCSEQ, ~PCTESTCD, ~PCTEST,
    "SUBJ-001", "", 1, "DRUGA", "Drug A",
    "SUBJ-002", "", 2, "DRUGA", "Drug A"
  )
  class(test_domain) <- c("domain", "data.frame")

  # Should error due to empty domain
  expect_error(
    summary(test_domain),
    "The DOMAIN column is empty"
  )
})

test_that("summary.domain handles domains with missing DOMAIN column", {
  # Create a domain object without DOMAIN column
  test_domain <- tibble::tribble(
    ~USUBJID, ~PCSEQ, ~PCTESTCD, ~PCTEST,
    "SUBJ-001", 1, "DRUGA", "Drug A",
    "SUBJ-002", 2, "DRUGA", "Drug A"
  )
  class(test_domain) <- c("domain", "data.frame")

  # Should error due to missing DOMAIN column
  expect_error(
    summary(test_domain),
    "The data frame must have a DOMAIN column"
  )
})

test_that("summary.domain handles domains with missing optional fields gracefully", {
  # Create a minimal domain object
  test_domain <- tibble::tribble(
    ~USUBJID, ~DOMAIN, ~STUDYID, ~PCSEQ, ~PCTESTCD, ~PCTEST,
    "SUBJ-001", "PC", "STUDY-001", 1, "DRUGA", "Drug A",
    "SUBJ-002", "PC", "STUDY-001", 2, "DRUGA", "Drug A"
  )
  class(test_domain) <- c("domain", "data.frame")

  # Call summary
  result <- summary(test_domain, silent = TRUE)

  # Check that missing optional fields are handled gracefully
  expect_s3_class(result, "summary_domain")
  expect_equal(result$domain, "PC")
  expect_equal(result$study, "STUDY-001")
  expect_equal(result$n_obs, 2)
  expect_equal(nrow(result$subjects), 2)
  expect_equal(nrow(result$test), 1)
  expect_equal(nrow(result$observations), 1)

  # Optional fields should be NULL
  expect_null(result$tpt)
  expect_null(result$epoch)
  expect_null(result$category)
  expect_null(result$visit)
})

# test_that("summary.domain handles domains with all optional fields", {
#   # Create a comprehensive domain object with all optional fields
#   test_domain <- tibble::tribble(
#     ~USUBJID, ~DOMAIN, ~STUDYID, ~PCSEQ, ~PCTESTCD, ~PCTEST, ~PCCAT, ~PCSCAT, ~PCFAST, ~PCTPT, ~PCTPTNUM, ~PCELTM, ~EPOCH, ~VISIT, ~PCSPEC,
#     "SUBJ-001", "PC", "STUDY-001", 1, "DRUGA", "Drug A", "PK", "PLASMA", "Y", "PRE", 0, "PT0H", "TREATMENT", "DAY1", "PLASMA",
#     "SUBJ-001", "PC", "STUDY-001", 2, "DRUGA", "Drug A", "PK", "PLASMA", "Y", "1HR", 1, "PT1H", "TREATMENT", "DAY1", "PLASMA",
#     "SUBJ-002", "PC", "STUDY-001", 3, "DRUGA", "Drug A", "PK", "PLASMA", "N", "PRE", 0, "PT0H", "TREATMENT", "DAY1", "PLASMA"
#   )
#   class(test_domain) <- c("domain", "data.frame")
#
#   # Call summary
#   result <- summary(test_domain, silent = TRUE)
#
#   # Check that all fields are populated
#   expect_s3_class(result, "summary_domain")
#   expect_equal(result$domain, "PC")
#   expect_equal(result$study, "STUDY-001")
#   expect_equal(result$n_obs, 3)
#   expect_equal(nrow(result$subjects), 2)
#   expect_equal(nrow(result$test), 1)
#   expect_equal(nrow(result$observations), 1)
#   expect_equal(nrow(result$tpt), 2)
#   expect_equal(nrow(result$epoch), 1)
#   expect_equal(nrow(result$category), 1)
#   expect_equal(nrow(result$visit), 1)
#
#   # Check specific content
#   expect_equal(result$test$PCTESTCD, "DRUGA")
#   expect_equal(result$test$PCTEST, "Drug A")
#   expect_equal(result$test$PCCAT, "PK")
#   expect_equal(result$test$PCSCAT, "PLASMA")
#   expect_equal(result$test$PCFAST, "Y")
#   expect_equal(result$test$PCSPEC, "PLASMA")
#   expect_equal(result$epoch$EPOCH, "TREATMENT")
#   expect_equal(result$visit$VISIT, "DAY1")
# })

test_that("summary.domain handles domains with case-insensitive domain codes", {
  # Create a domain object with lowercase domain code
  test_domain <- tibble::tribble(
    ~USUBJID, ~DOMAIN, ~PCSEQ, ~PCTESTCD, ~PCTEST,
    "SUBJ-001", "pc", 1, "DRUGA", "Drug A",
    "SUBJ-002", "pc", 2, "DRUGA", "Drug A"
  )
  class(test_domain) <- c("domain", "data.frame")

  # Call summary
  result <- summary(test_domain, silent = TRUE)

  # Check that domain code is converted to uppercase
  expect_equal(result$domain, "PC")
  expect_equal(result$n_obs, 2)
})

test_that("summary.domain handles domains with numeric fields correctly", {
  # Create a domain object with numeric fields
  test_domain <- tibble::tribble(
    ~USUBJID, ~DOMAIN, ~PCSEQ, ~PCTESTCD, ~PCTEST, ~PCTPTNUM, ~PCSTRESN,
    "SUBJ-001", "PC", 1, "DRUGA", "Drug A", 0, 10.5,
    "SUBJ-001", "PC", 2, "DRUGA", "Drug A", 1, 15.2,
    "SUBJ-002", "PC", 3, "DRUGA", "Drug A", 0, 12.3
  )
  class(test_domain) <- c("domain", "data.frame")

  # Call summary
  result <- summary(test_domain, silent = TRUE)

  # Check that numeric fields are handled correctly
  expect_s3_class(result, "summary_domain")
  expect_equal(result$domain, "PC")
  expect_equal(result$n_obs, 3)
  expect_equal(result$tpt, NULL)
})

test_that("summary.domain handles domains with NA values gracefully", {
  # Create a domain object with NA values
  test_domain <- tibble::tribble(
    ~USUBJID, ~DOMAIN, ~PCSEQ, ~PCTESTCD, ~PCTEST, ~PCCAT,
    "SUBJ-001", "PC", 1, "DRUGA", "Drug A", "PK",
    "SUBJ-001", "PC", 2, "DRUGA", NA, "PK",
    "SUBJ-002", "PC", 3, "DRUGA", "Drug A", NA
  )
  class(test_domain) <- c("domain", "data.frame")

  # Call summary
  result <- summary(test_domain, silent = TRUE)

  # Check that NA values are handled gracefully
  expect_s3_class(result, "summary_domain")
  expect_equal(result$domain, "PC")
  expect_equal(result$n_obs, 3)
  expect_equal(nrow(result$test), 3)
  expect_equal(nrow(result$observations), 1)
  expect_equal(nrow(result$category), 2)
})

test_that("summary.domain handles domains with duplicate values correctly", {
  # Create a domain object with duplicate values
  test_domain <- tibble::tribble(
    ~USUBJID, ~DOMAIN, ~PCSEQ, ~PCTESTCD, ~PCTEST, ~PCCAT, ~PCTPT,
    "SUBJ-001", "PC", 1, "DRUGA", "Drug A", "PK", "PRE",
    "SUBJ-001", "PC", 2, "DRUGA", "Drug A", "PK", "PRE",
    "SUBJ-002", "PC", 3, "DRUGA", "Drug A", "PK", "PRE"
  )
  class(test_domain) <- c("domain", "data.frame")

  # Call summary
  result <- summary(test_domain, silent = TRUE)

  # Check that duplicates are handled correctly
  expect_s3_class(result, "summary_domain")
  expect_equal(result$domain, "PC")
  expect_equal(result$n_obs, 3)
  expect_equal(nrow(result$test), 1)  # Should deduplicate test info
  expect_equal(nrow(result$tpt), 1)   # Should deduplicate time points
  expect_equal(nrow(result$category), 1) # Should deduplicate categories
  expect_equal(result$observations$n, 3) # But keep count of observations
})

test_that("print.summary_domain displays basic information correctly", {
  # Create a basic domain object
  test_domain <- tibble::tribble(
    ~USUBJID, ~DOMAIN, ~STUDYID, ~PCSEQ, ~PCTESTCD, ~PCTEST, ~PCCAT, ~PCSCAT, ~PCTPT, ~PCTPTNUM, ~PCELTM, ~EPOCH, ~VISIT,
    "SUBJ-001", "PC", "STUDY-001", 1, "DRUGA", "Drug A", "PK", "PLASMA", "PRE", 0, "PT0H", "TREATMENT", "SCREENING",
    "SUBJ-001", "PC", "STUDY-001", 2, "DRUGA", "Drug A", "PK", "PLASMA", "1HR", 1, "PT1H", "TREATMENT", "DAY1",
    "SUBJ-002", "PC", "STUDY-001", 3, "DRUGA", "Drug A", "PK", "PLASMA", "2HR", 2, "PT2H", "TREATMENT", "DAY1"
  )
  class(test_domain) <- c("domain", "data.frame")

  # Create summary object
  summary_obj <- summary(test_domain, silent = TRUE)

  # Capture output
  output <- capture.output(print(summary_obj))

  # Check basic structure
  expect_true(any(grepl("SDTM domain summary", output)))
  expect_true(any(grepl("Study STUDY-001", output)))
  expect_true(any(grepl("Domain PC", output)))
  expect_true(any(grepl("2 subjects", output)))
  expect_true(any(grepl("3 observations", output)))
})

test_that("print.summary_domain displays categories when present", {
  # Create domain object with categories
  test_domain <- tibble::tribble(
    ~USUBJID, ~DOMAIN, ~PCSEQ, ~PCTESTCD, ~PCTEST, ~PCCAT, ~PCSCAT, ~PCFAST,
    "SUBJ-001", "PC", 1, "DRUGA", "Drug A", "PK", "PLASMA", "Y",
    "SUBJ-002", "PC", 2, "DRUGA", "Drug A", "PK", "URINE", "N"
  )
  class(test_domain) <- c("domain", "data.frame")

  # Create summary object
  summary_obj <- summary(test_domain, silent = TRUE)

  # Capture output
  output <- capture.output(print(summary_obj))

  # Check categories section
  expect_true(any(grepl("Categories", output)))
  expect_true(any(grepl("PCCAT", output)))
  expect_true(any(grepl("PCSCAT", output)))
  expect_true(any(grepl("PCFAST", output)))
})

test_that("print.summary_domain displays test codes when present", {
  # Create domain object with test codes
  test_domain <- tibble::tribble(
    ~USUBJID, ~DOMAIN, ~PCSEQ, ~PCTESTCD, ~PCTEST, ~PCCAT, ~PCSPEC,
    "SUBJ-001", "PC", 1, "DRUGA", "Drug A", "PK", "PLASMA",
    "SUBJ-002", "PC", 2, "DRUGB", "Drug B", "PK", "PLASMA"
  )
  class(test_domain) <- c("domain", "data.frame")

  # Create summary object
  summary_obj <- summary(test_domain, silent = TRUE)

  # Capture output
  output <- capture.output(print(summary_obj))

  # Check test codes section
  expect_true(any(grepl("Testcodes", output)))
  expect_true(any(grepl("PCTESTCD", output)))
  expect_true(any(grepl("PCTEST", output)))
  expect_true(any(grepl("PCSPEC", output)))
})

test_that("print.summary_domain displays time points when present", {
  # Create domain object with time points
  test_domain <- tibble::tribble(
    ~USUBJID, ~DOMAIN, ~PCSEQ, ~PCTESTCD, ~PCTEST, ~PCTPT, ~PCTPTNUM, ~PCELTM,
    "SUBJ-001", "PC", 1, "DRUGA", "Drug A", "PRE", 0, "PT0H",
    "SUBJ-001", "PC", 2, "DRUGA", "Drug A", "1HR", 1, "PT1H",
    "SUBJ-001", "PC", 3, "DRUGA", "Drug A", "2HR", 2, "PT2H"
  )
  class(test_domain) <- c("domain", "data.frame")

  # Create summary object
  summary_obj <- summary(test_domain, silent = TRUE)

  # Capture output
  output <- capture.output(print(summary_obj))

  # Check time points section
  expect_true(any(grepl("Observation time points", output)))
  expect_true(any(grepl("PCTPT", output)))
  expect_true(any(grepl("PCTPTNUM", output)))
  expect_true(any(grepl("PCELTM", output)))
})

test_that("print.summary_domain displays epochs when present", {
  # Create domain object with epochs
  test_domain <- tibble::tribble(
    ~USUBJID, ~DOMAIN, ~PCSEQ, ~PCTESTCD, ~PCTEST, ~EPOCH,
    "SUBJ-001", "PC", 1, "DRUGA", "Drug A", "SCREENING",
    "SUBJ-001", "PC", 2, "DRUGA", "Drug A", "TREATMENT",
    "SUBJ-002", "PC", 3, "DRUGA", "Drug A", "SCREENING"
  )
  class(test_domain) <- c("domain", "data.frame")

  # Create summary object
  summary_obj <- summary(test_domain, silent = TRUE)

  # Capture output
  output <- capture.output(print(summary_obj))

  # Check epochs section
  expect_true(any(grepl("Epochs", output)))
  expect_true(any(grepl("SCREENING", output)))
  expect_true(any(grepl("TREATMENT", output)))
})

test_that("print.summary_domain handles domains without optional fields gracefully", {
  # Create minimal domain object
  test_domain <- tibble::tribble(
    ~USUBJID, ~DOMAIN, ~STUDYID, ~PCSEQ, ~PCTESTCD, ~PCTEST,
    "SUBJ-001", "PC", "STUDY-001", 1, "DRUGA", "Drug A",
    "SUBJ-002", "PC", "STUDY-001", 2, "DRUGA", "Drug A"
  )
  class(test_domain) <- c("domain", "data.frame")

  # Create summary object
  summary_obj <- summary(test_domain, silent = TRUE)

  # Capture output
  output <- capture.output(print(summary_obj))

  # Check basic information is displayed
  expect_true(any(grepl("SDTM domain summary", output)))
  expect_true(any(grepl("Study STUDY-001", output)))
  expect_true(any(grepl("Domain PC", output)))
  expect_true(any(grepl("2 subjects", output)))
  expect_true(any(grepl("2 observations", output)))

  # Check that optional sections are not displayed
  expect_false(any(grepl("Observation time points", output)))
  expect_false(any(grepl("Epochs", output)))
})

test_that("print.summary_domain handles domains with only some optional fields", {
  # Create domain object with only categories, no test codes
  test_domain <- tibble::tribble(
    ~USUBJID, ~DOMAIN, ~PCSEQ, ~PCTESTCD, ~PCTEST, ~PCCAT, ~PCSCAT,
    "SUBJ-001", "PC", 1, "DRUGA", "Drug A", "PK", "PLASMA",
    "SUBJ-002", "PC", 2, "DRUGA", "Drug A", "PK", "URINE"
  )
  class(test_domain) <- c("domain", "data.frame")

  # Create summary object
  summary_obj <- summary(test_domain, silent = TRUE)

  # Capture output
  output <- capture.output(print(summary_obj))

  # Check that categories are displayed
  expect_true(any(grepl("Categories", output)))
  expect_true(any(grepl("PCCAT", output)))
  expect_true(any(grepl("PCSCAT", output)))

  # Check that test codes are displayed (since PCTESTCD is present)
  expect_true(any(grepl("Testcodes", output)))

  # Check that time points and epochs are not displayed
  expect_false(any(grepl("Observation time points", output)))
  expect_false(any(grepl("Epochs", output)))
})

test_that("print.summary_domain handles domains with multiple test codes correctly", {
  # Create domain object with multiple test codes
  test_domain <- tibble::tribble(
    ~USUBJID, ~DOMAIN, ~PCSEQ, ~PCTESTCD, ~PCTEST, ~PCCAT,
    "SUBJ-001", "PC", 1, "DRUGA", "Drug A", "PK",
    "SUBJ-001", "PC", 2, "DRUGB", "Drug B", "PK",
    "SUBJ-002", "PC", 3, "DRUGA", "Drug A", "PK",
    "SUBJ-002", "PC", 4, "DRUGC", "Drug C", "PD"
  )
  class(test_domain) <- c("domain", "data.frame")

  # Create summary object
  summary_obj <- summary(test_domain, silent = TRUE)

  # Capture output
  output <- capture.output(print(summary_obj))

  # Check that test codes section displays multiple codes
  expect_true(any(grepl("Testcodes", output)))
  expect_true(any(grepl("DRUGA", output)))
  expect_true(any(grepl("DRUGB", output)))
  expect_true(any(grepl("DRUGC", output)))

  # Check that categories section shows multiple categories
  expect_true(any(grepl("Categories", output)))
  expect_true(any(grepl("PK", output)))
  expect_true(any(grepl("PD", output)))
})

test_that("print.summary_domain handles domains with complex time point information", {
  # Create domain object with complex time points
  test_domain <- tibble::tribble(
    ~USUBJID, ~DOMAIN, ~PCSEQ, ~PCTESTCD, ~PCTEST, ~PCTPT, ~PCTPTNUM, ~PCELTM,
    "SUBJ-001", "PC", 1, "DRUGA", "Drug A", "PRE", 0, "PT0H",
    "SUBJ-001", "PC", 2, "DRUGA", "Drug A", "15MIN", 0.25, "PT15M",
    "SUBJ-001", "PC", 3, "DRUGA", "Drug A", "30MIN", 0.5, "PT30M",
    "SUBJ-001", "PC", 4, "DRUGA", "Drug A", "1HR", 1, "PT1H",
    "SUBJ-001", "PC", 5, "DRUGA", "Drug A", "2HR", 2, "PT2H",
    "SUBJ-001", "PC", 6, "DRUGA", "Drug A", "4HR", 4, "PT4H",
    "SUBJ-001", "PC", 7, "DRUGA", "Drug A", "8HR", 8, "PT8H",
    "SUBJ-001", "PC", 8, "DRUGA", "Drug A", "24HR", 24, "PT24H"
  )
  class(test_domain) <- c("domain", "data.frame")

  # Create summary object
  summary_obj <- summary(test_domain, silent = TRUE)

  # Capture output
  output <- capture.output(print(summary_obj))

  # Check that time points section displays all time points
  expect_true(any(grepl("Observation time points", output)))
  expect_true(any(grepl("PRE", output)))
  expect_true(any(grepl("15MIN", output)))
  expect_true(any(grepl("30MIN", output)))
  expect_true(any(grepl("1HR", output)))
  expect_true(any(grepl("2HR", output)))
  expect_true(any(grepl("4HR", output)))
  expect_true(any(grepl("8HR", output)))
  expect_true(any(grepl("24HR", output)))
})

test_that("print.summary_domain handles domains with mixed field types", {
  # Create domain object with mixed field types
  test_domain <- tibble::tribble(
    ~USUBJID, ~DOMAIN, ~PCSEQ, ~PCTESTCD, ~PCTEST, ~PCCAT, ~PCSCAT, ~PCFAST, ~PCSPEC, ~PCTPT, ~PCTPTNUM, ~PCELTM, ~EPOCH, ~VISIT,
    "SUBJ-001", "PC", 1, "DRUGA", "Drug A", "PK", "PLASMA", "Y", "PLASMA", "PRE", 0, "PT0H", "TREATMENT", "DAY1",
    "SUBJ-001", "PC", 2, "DRUGA", "Drug A", "PK", "PLASMA", "Y", "PLASMA", "1HR", 1, "PT1H", "TREATMENT", "DAY1",
    "SUBJ-002", "PC", 3, "DRUGA", "Drug A", "PK", "URINE", "N", "URINE", "PRE", 0, "PT0H", "TREATMENT", "DAY1"
  )
  class(test_domain) <- c("domain", "data.frame")

  # Create summary object
  summary_obj <- summary(test_domain, silent = TRUE)

  # Capture output
  output <- capture.output(print(summary_obj))

  # Check that all sections are displayed
  expect_true(any(grepl("Categories", output)))
  expect_true(any(grepl("Testcodes", output)))
  expect_true(any(grepl("Observation time points", output)))
  expect_true(any(grepl("Epochs", output)))

  # Check that all relevant fields are shown
  expect_true(any(grepl("PCCAT", output)))
  expect_true(any(grepl("PCSCAT", output)))
  expect_true(any(grepl("PCFAST", output)))
  expect_true(any(grepl("PCSPEC", output)))
  expect_true(any(grepl("PCTPT", output)))
  expect_true(any(grepl("PCTPTNUM", output)))
  expect_true(any(grepl("PCELTM", output)))
})

test_that("print.summary_domain handles domains with special characters in text fields", {
  # Create domain object with special characters
  test_domain <- tibble::tribble(
    ~USUBJID, ~DOMAIN, ~PCSEQ, ~PCTESTCD, ~PCTEST, ~PCCAT, ~PCSCAT,
    "SUBJ-001", "PC", 1, "DRUG-A", "Drug A (Parent)", "PK", "Plasma (EDTA)",
    "SUBJ-002", "PC", 2, "DRUG-B", "Drug B (Metabolite)", "PD", "Serum (Heparin)"
  )
  class(test_domain) <- c("domain", "data.frame")

  # Create summary object
  summary_obj <- summary(test_domain, silent = TRUE)

  # Capture output
  output <- capture.output(print(summary_obj))

  # Check that special characters are handled correctly
  expect_true(any(grepl("DRUG-A", output)))
  expect_true(any(grepl("DRUG-B", output)))
  expect_true(any(grepl("Drug A \\(Parent\\)", output)))
  expect_true(any(grepl("Drug B \\(Metabolite\\)", output)))
  expect_true(any(grepl("Plasma \\(EDTA\\)", output)))
  expect_true(any(grepl("Serum \\(Heparin\\)", output)))
})

test_that("print.summary_domain handles domains with numeric test codes", {
  # Create domain object with numeric test codes
  test_domain <- tibble::tribble(
    ~USUBJID, ~DOMAIN, ~PCSEQ, ~PCTESTCD, ~PCTEST, ~PCCAT,
    "SUBJ-001", "PC", 1, "001", "Test 1", "PK",
    "SUBJ-001", "PC", 2, "002", "Test 2", "PK",
    "SUBJ-002", "PC", 3, "003", "Test 3", "PD"
  )
  class(test_domain) <- c("domain", "data.frame")

  # Create summary object
  summary_obj <- summary(test_domain, silent = TRUE)

  # Capture output
  output <- capture.output(print(summary_obj))

  # Check that numeric test codes are displayed correctly
  expect_true(any(grepl("Testcodes", output)))
  expect_true(any(grepl("001", output)))
  expect_true(any(grepl("002", output)))
  expect_true(any(grepl("003", output)))
  expect_true(any(grepl("Test 1", output)))
  expect_true(any(grepl("Test 2", output)))
  expect_true(any(grepl("Test 3", output)))
})

test_that("print.summary_domain handles domains with missing values gracefully", {
  # Create domain object with missing values
  test_domain <- tibble::tribble(
    ~USUBJID, ~DOMAIN, ~PCSEQ, ~PCTESTCD, ~PCTEST, ~PCCAT, ~PCSCAT,
    "SUBJ-001", "PC", 1, "DRUGA", "Drug A", "PK", NA,
    "SUBJ-001", "PC", 2, "DRUGA", NA, "PK", "PLASMA",
    "SUBJ-002", "PC", 3, "DRUGA", "Drug A", NA, "PLASMA"
  )
  class(test_domain) <- c("domain", "data.frame")

  # Create summary object
  summary_obj <- summary(test_domain, silent = TRUE)

  # Capture output
  output <- capture.output(print(summary_obj))

  # Check that output is generated without errors
  expect_true(length(output) > 0)
  expect_true(any(grepl("SDTM domain summary", output)))
  expect_true(any(grepl("3 observations", output)))

  # Check that sections with missing values are still displayed
  expect_true(any(grepl("Categories", output)))
  expect_true(any(grepl("Testcodes", output)))
})

