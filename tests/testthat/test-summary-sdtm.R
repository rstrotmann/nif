# Test file for summary.sdtm() function

test_that("summary.sdtm handles valid SDTM objects correctly", {
  # Create a simple SDTM object for testing
  test_data <- list(
    domains = list(
      dm = tibble::tribble(
        ~USUBJID, ~DOMAIN, ~STUDYID, ~ACTARMCD, ~ACTARM,
        "SUBJ-001", "DM", "STUDY-001", "ARM1", "Treatment Arm 1",
        "SUBJ-002", "DM", "STUDY-001", "ARM2", "Treatment Arm 2"
      ),
      pc = tibble::tribble(
        ~USUBJID, ~DOMAIN, ~PCSPEC, ~PCTEST, ~PCTESTCD, ~PCTPT, ~PCTPTNUM,
        "SUBJ-001", "PC", "PLASMA", "Drug A", "DRUGA", "PRE", 0,
        "SUBJ-001", "PC", "PLASMA", "Drug A", "DRUGA", "1HR", 1,
        "SUBJ-002", "PC", "PLASMA", "Drug A", "DRUGA", "2HR", 2
      ),
      ex = tibble::tribble(
        ~USUBJID, ~DOMAIN, ~EXTRT, ~EXDOSE,
        "SUBJ-001", "EX", "Drug A", 100,
        "SUBJ-002", "EX", "Drug A", 200
      )
    ),
    analyte_mapping = data.frame(
      EXTRT = "Drug A",
      PCTESTCD = "DRUGA",
      ANALYTE = "DRUGA"
    ),
    metabolite_mapping = data.frame(),
    time_mapping = data.frame()
  )
  class(test_data) <- c("sdtm", "list")

  # Call summary
  result <- summary(test_data)

  # Check the summary object is created with correct class
  expect_s3_class(result, "summary_sdtm")

  # Check key properties
  expect_equal(result$study, "STUDY-001")
  expect_equal(length(result$subjects), 2)
  expect_equal(nrow(result$disposition), 3)
  expect_equal(length(result$treatments), 1)
  expect_equal(nrow(result$arms), 2)
  expect_equal(nrow(result$doses), 2)
  expect_equal(length(result$specimens), 1)
  expect_equal(nrow(result$analytes), 1)

  # Check print method works without error
  expect_output(print(result), "SDTM data set summary")
})


test_that("summary.sdtm handles missing domains gracefully", {
  # Create an SDTM object with missing domains
  test_data <- list(
    domains = list(
      dm = data.frame(USUBJID = c("SUBJ-001", "SUBJ-002"), DOMAIN = "DM")
    ),
    analyte_mapping = data.frame(),
    metabolite_mapping = data.frame(),
    time_mapping = data.frame()
  )
  class(test_data) <- c("sdtm", "list")

  # Call summary
  result <- summary(test_data)

  # Verify the result has default values for missing domains
  expect_s3_class(result, "summary_sdtm")
  expect_equal(length(result$treatments), 0)
  expect_equal(length(result$specimens), 0)
  expect_equal(nrow(result$analytes), 0)

  # Check print method works without error
  expect_output(print(result), "SDTM data set summary")
})


test_that("summary.sdtm handles missing fields in domains", {
  # Create an SDTM object with domains that are missing fields
  test_dm <- data.frame(
    USUBJID = c("SUBJ-001", "SUBJ-002"),
    DOMAIN = "DM"
    # Missing STUDYID, ACTARMCD, ACTARM
  )

  test_pc <- data.frame(
    USUBJID = c("SUBJ-001", "SUBJ-002"),
    DOMAIN = "PC"
    # Missing PCSPEC, PCTEST, PCTESTCD
  )

  test_data <- list(
    domains = list(
      dm = test_dm,
      pc = test_pc
    ),
    analyte_mapping = data.frame(),
    metabolite_mapping = data.frame(),
    time_mapping = data.frame()
  )
  class(test_data) <- c("sdtm", "list")

  # Call summary
  result <- summary(test_data)

  # Verify the result has default values for missing fields
  expect_s3_class(result, "summary_sdtm")
  expect_equal(length(result$study), 0)
  expect_equal(nrow(result$arms), 0)
  expect_equal(length(result$specimens), 0)
  expect_equal(nrow(result$analytes), 0)

  # Check print method works without error
  expect_output(print(result), "SDTM data set summary")
})


test_that("summary.sdtm correctly processes pc_timepoints", {
  # Create a simple SDTM object with PC timepoints
  test_data <- list(
    domains = list(
      pc = tibble::tribble(
        ~USUBJID, ~DOMAIN, ~PCTPT, ~PCTPTNUM,
        "SUBJ-001", "PC", "PRE", 0,
        "SUBJ-001", "PC", "1HR", 1,
        "SUBJ-002", "PC", "2HR", 2
      )
    )
  )

  class(test_data) <- c("sdtm", "list")

  # Call summary
  expect_no_error(
    result <- summary(test_data)
  )

  # Check that pc_timepoints is correctly included
  expect_s3_class(result, "summary_sdtm")
  expect_equal(nrow(result$pc_timepoints), 3)
  expect_equal(result$pc_timepoints$PCTPT, c("PRE", "1HR", "2HR"))
  expect_equal(result$pc_timepoints$PCTPTNUM, c(0, 1, 2))
})


test_that("summary.sdtm handles empty data frames", {
  # Create SDTM object with empty data frames
  test_data <- list(
    domains = list(
      dm = data.frame(USUBJID = character(0), DOMAIN = character(0), STUDYID = character(0)),
      pc = data.frame(USUBJID = character(0), DOMAIN = character(0))
    ),
    analyte_mapping = data.frame(),
    metabolite_mapping = data.frame(),
    time_mapping = data.frame()
  )
  class(test_data) <- c("sdtm", "list")

  # Call summary
  expect_no_error(
    result <- summary(test_data)
  )

  # Check that result is still properly structured
  expect_s3_class(result, "summary_sdtm")
  expect_equal(length(result$study), 0)
  expect_equal(length(result$subjects), 0)
  expect_equal(nrow(result$disposition), 2)

  # Check print method still works
  expect_output(print(result), "SDTM data set summary")
})


test_that("summary.sdtm handles NA values in fields", {
  # Create SDTM object with NA values
  test_data <- list(
    domains = list(
      dm = tibble::tribble(
        ~USUBJID, ~DOMAIN, ~STUDYID, ~ACTARMCD, ~ACTARM,
        "SUBJ-001", "DM", "STUDY-001", "ARM1", NA,
        "SUBJ-002", "DM", NA, NA, "Treatment Arm 2"
      ),
      pc = tibble::tribble(
        ~USUBJID, ~DOMAIN, ~PCSPEC, ~PCTEST, ~PCTESTCD,
        "SUBJ-001", "PC", "PLASMA", NA, "DRUGA",
        "SUBJ-002", "PC", NA, "Drug A", NA
      )
    )
  )
  class(test_data) <- c("sdtm", "list")

  # Call summary
  expect_no_error(
    result <- summary(test_data)
  )

  # Check that result handles NAs appropriately
  # expect_s3_class(result, "summary_sdtm")
  # expect_equal(result$study, "STUDY-001")  # NA should be filtered out
  # expect_equal(length(result$specimens), 1) # NA should be filtered out
  # expect_equal(nrow(result$analytes), 1)   # Rows with NA should be filtered out
  #
  # # Check print method still works
  # expect_output(print(result), "SDTM data set summary")
})


test_that("summary.sdtm handles multiple unique values appropriately", {
  # Create SDTM object with multiple study IDs and other unique values
  test_dm <- data.frame(
    USUBJID = c("SUBJ-001", "SUBJ-002", "SUBJ-003"),
    DOMAIN = c("DM", "DM", "DM"),
    STUDYID = c("STUDY-001", "STUDY-001", "STUDY-002"), # Multiple study IDs
    ACTARMCD = c("ARM1", "ARM2", "ARM3"),
    ACTARM = c("Treatment Arm 1", "Treatment Arm 2", "Treatment Arm 3")
  )

  test_pc <- data.frame(
    USUBJID = c("SUBJ-001", "SUBJ-002", "SUBJ-003"),
    DOMAIN = c("PC", "PC", "PC"),
    PCSPEC = c("PLASMA", "SERUM", "URINE"), # Multiple specimen types
    PCTEST = c("Drug A", "Drug B", "Drug C"), # Multiple tests
    PCTESTCD = c("DRUGA", "DRUGB", "DRUGC")
  )

  test_data <- list(
    domains = list(
      dm = test_dm,
      pc = test_pc
    ),
    analyte_mapping = data.frame(),
    metabolite_mapping = data.frame(),
    time_mapping = data.frame()
  )
  class(test_data) <- c("sdtm", "list")

  # Call summary
  result <- summary(test_data)

  # Check that result correctly processes multiple unique values
  expect_s3_class(result, "summary_sdtm")
  expect_equal(length(result$study), 2) # Both study IDs should be included
  expect_equal(length(result$specimens), 3) # All specimen types should be included
  expect_equal(nrow(result$analytes), 3) # All analytes should be included
  expect_equal(nrow(result$arms), 3) # All arms should be included

  # Check print method still works
  expect_output(print(result), "SDTM data set summary")
})
