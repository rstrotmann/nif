# Test file for make_subjects_sdtm function

test_that("make_subjects_sdtm creates a proper subject data frame", {
  # Create test DM domain
  dm_data <- tibble::tribble(
    ~DOMAIN, ~STUDYID,   ~USUBJID, ~SUBJID, ~SITEID, ~SEX, ~AGE,                      ~RACE,                     ~ETHNIC, ~ARMCD,              ~ARM, ~ACTARMCD,           ~ACTARM, ~COUNTRY, ~RFSTDTC,
       "DM", "STUDY1", "SUBJ-001",   "001",   "001",  "M",   45,                    "WHITE", "NOT HISPANIC OR LATINO", "ARM1", "Treatment arm 1",    "ARM1", "Treatment arm 1",    "USA", "2023-01-01",
       "DM", "STUDY1", "SUBJ-002",   "002",   "001",  "F",   38, "BLACK OR AFRICAN AMERICAN", "NOT HISPANIC OR LATINO", "ARM1", "Treatment arm 1",    "ARM1", "Treatment arm 1",    "USA", "2023-01-01"
  )

  # Create test VS domain with height/weight
  vs_data <- tibble::tribble(
    ~DOMAIN, ~STUDYID,   ~USUBJID,  ~VSTESTCD, ~VSSTRESN, ~VSSTRESU, ~VSBLFL,
       "VS", "STUDY1", "SUBJ-001",   "HEIGHT",       180,      "cm",     "Y",
       "VS", "STUDY1", "SUBJ-001",   "WEIGHT",        85,      "kg",     "Y",
       "VS", "STUDY1", "SUBJ-002",   "HEIGHT",       165,      "cm",     "Y",
       "VS", "STUDY1", "SUBJ-002",   "WEIGHT",        70,      "kg",     "Y"
  )

  # Create a new SDTM object
  test_sdtm <- new_sdtm(list(
    dm = dm_data,
    vs = vs_data
  ))

  # Test the function
  result <- make_subjects_sdtm(test_sdtm)

  # Check the output structure
  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 2)  # Should have 2 subjects

  # Check that required columns are present
  expect_true(all(c("ID", "USUBJID", "SEX", "AGE", "HEIGHT", "WEIGHT", "BMI") %in% names(result)))

  # Check that values were correctly transferred
  expect_equal(result$USUBJID, c("SUBJ-001", "SUBJ-002"))
  expect_equal(result$HEIGHT, c(180, 165))
  expect_equal(result$WEIGHT, c(85, 70))

  # Check that BMI was calculated correctly
  expect_equal(round(result$BMI, 1), c(round(85 / (1.8^2), 1), round(70 / (1.65^2), 1)))
})


test_that("make_subjects_sdtm works with missing height/weight", {
  # Create test DM domain with all required columns
  dm_data <- tibble::tribble(
    ~DOMAIN, ~STUDYID,   ~USUBJID, ~SUBJID, ~SEX, ~AGE, ~ARMCD, ~ARM, ~ACTARMCD, ~ACTARM, ~COUNTRY, ~RFSTDTC, ~SITEID,
       "DM", "STUDY1", "SUBJ-001", "001",   "M",   45, "ARM1", "Treatment arm 1", "ARM1", "Treatment arm 1", "USA", "2023-01-01", "001",
       "DM", "STUDY1", "SUBJ-002", "002",   "F",   38, "ARM1", "Treatment arm 1", "ARM1", "Treatment arm 1", "USA", "2023-01-01", "001"
  )

  # Create test VS domain with incomplete data
  vs_data <- tibble::tribble(
    ~DOMAIN, ~STUDYID,   ~USUBJID,  ~VSTESTCD, ~VSSTRESN, ~VSSTRESU, ~VSBLFL,
       "VS", "STUDY1", "SUBJ-001",   "HEIGHT",       180,      "cm",     "Y",
       "VS", "STUDY1", "SUBJ-002",    "PULSE",        72,     "bpm",     "Y"  # Missing weight for both subjects
  )

  # Create a new SDTM object
  test_sdtm <- new_sdtm(list(
    dm = dm_data,
    vs = vs_data
  ))

  # Test the function
  result <- make_subjects_sdtm(test_sdtm)

  # Check the structure of the results
  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 2)  # Should have 2 subjects

  # Check that we have the height column and the value is correct
  expect_true("HEIGHT" %in% names(result))
  expect_equal(result$HEIGHT, c(180, NA))

  # Check if we have the WEIGHT and BMI columns
  # (might not be present in the actual implementation when data is missing)
  if ("WEIGHT" %in% names(result)) {
    expect_true(all(is.na(result$WEIGHT)))
  }

  if ("BMI" %in% names(result)) {
    expect_true(all(is.na(result$BMI)))
  }
})


test_that("make_subjects_sdtm works with example data", {
  # Test with package example data if available
  skip_if_not_installed("nif")
  skip_if_not(exists("examplinib_fe"), "Example SDTM data not available")

  if (exists("examplinib_fe")) {
    # Should run without error
    expect_no_error(subjects <- make_subjects_sdtm(examplinib_fe))

    # Should return a data frame
    expect_s3_class(subjects, "data.frame")

    # Should have rows - might be different from USUBJID count due to filtering
    expect_gt(nrow(subjects), 0)
  }
})
