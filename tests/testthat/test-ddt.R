# Test file for ddt function

test_that("ddt returns data frame with correct structure", {
  # Create minimal test nif object
  test_data <- tibble::tribble(
    ~ID, ~TIME, ~AMT, ~CMT, ~EVID, ~DV,
    1,   0,     100,  1,    1,     NA,
    1,   1,     0,    2,    0,     10,
    2,   0,     100,  1,    1,     NA,
    2,   1,     0,    2,    0,     15
  ) %>% nif()

  result <- ddt(test_data, silent = TRUE)

  # Check structure
  expect_s3_class(result, "data.frame")
  expect_true(all(c("name", "definition", "type", "description", "unit", "source") %in% names(result)))
  expect_true(nrow(result) > 0)
})

test_that("ddt includes all standard fields present in nif object", {
  test_data <- tibble::tribble(
    ~ID, ~TIME, ~AMT, ~CMT, ~EVID, ~DV, ~STUDYID, ~USUBJID,
    1,   0,     100,  1,    1,     NA,  "STUDY1", "SUBJ-001",
    1,   1,     0,    2,    0,     10,  "STUDY1", "SUBJ-001"
  ) %>% nif()

  result <- ddt(test_data, silent = TRUE)

  # Check that standard fields are included
  expect_true("ID" %in% result$name)
  expect_true("TIME" %in% result$name)
  expect_true("AMT" %in% result$name)
  expect_true("CMT" %in% result$name)
  expect_true("EVID" %in% result$name)
  expect_true("DV" %in% result$name)
  expect_true("STUDYID" %in% result$name)
  expect_true("USUBJID" %in% result$name)
})

test_that("ddt handles CMT compartment descriptions correctly", {
  # Test with multiple compartments and analytes
  test_data <- tibble::tribble(
    ~ID, ~TIME, ~AMT, ~CMT, ~EVID, ~DV, ~ANALYTE,
    1,   0,     100,  1,    1,     NA,  "DRUG",
    1,   1,     0,    2,    0,     10,  "METABOLITE",
    1,   2,     0,    1,    0,     5,   "DRUG",
    2,   0,     150,  1,    1,     NA,  "DRUG",
    2,   1,     0,    2,    0,     20,  "METABOLITE"
  ) %>% nif()

  result <- ddt(test_data, silent = TRUE)

  # Find CMT row
  cmt_row <- result[result$name == "CMT", ]

  # Check that description contains compartment information
  expect_true(nrow(cmt_row) == 1)
  expect_true(grepl("=", cmt_row$description))
  expect_true(grepl("observation|administration", cmt_row$description))
})

test_that("ddt handles CMT when ANALYTE is missing", {
  # Test that ensure_analyte is called and creates ANALYTE from CMT
  test_data <- tibble::tribble(
    ~ID, ~TIME, ~AMT, ~CMT, ~EVID, ~DV,
    1,   0,     100,  1,    1,     NA,
    1,   1,     0,    2,    0,     10,
    2,   0,     100,  1,    1,     NA,
    2,   1,     0,    2,    0,     15
  ) %>% nif()

  result <- ddt(test_data, silent = TRUE)

  # Should not error and should have CMT description
  cmt_row <- result[result$name == "CMT", ]
  expect_true(nrow(cmt_row) == 1)
  expect_true(!is.na(cmt_row$description))
})

test_that("ddt handles CMT with NA values", {
  # Test with NA in CMT
  test_data <- tibble::tribble(
    ~ID, ~TIME, ~AMT, ~CMT, ~EVID, ~DV, ~ANALYTE,
    1,   0,     100,  1,    1,     NA,  "DRUG",
    1,   1,     0,    NA,   0,     10,  "METABOLITE",
    1,   2,     0,    1,    0,     5,   "DRUG"
  ) %>% nif()

  # Should not error
  expect_no_error(result <- ddt(test_data, silent = TRUE))

  cmt_row <- result[result$name == "CMT", ]
  expect_true(nrow(cmt_row) == 1)
})

test_that("ddt handles EVID with NA values", {
  # Test with NA in EVID (edge case)
  test_data <- tibble::tribble(
    ~ID, ~TIME, ~AMT, ~CMT, ~EVID, ~DV, ~ANALYTE,
    1,   0,     100,  1,    1,     NA,  "DRUG",
    1,   1,     0,    2,    NA,    10,  "METABOLITE",
    1,   2,     0,    1,    0,     5,   "DRUG"
  ) %>% nif()

  # Should handle NA in EVID gracefully (may produce NA in TYPE)
  expect_no_error(result <- ddt(test_data, silent = TRUE))
})

test_that("ddt handles numeric RACE correctly", {
  # Create test data with numeric RACE
  test_data <- tibble::tribble(
    ~ID, ~TIME, ~AMT, ~CMT, ~EVID, ~DV, ~RACE,
    1,   0,     100,  1,    1,     NA,  0,
    1,   1,     0,    2,    0,     10,  0,
    2,   0,     100,  1,    1,     NA,  1,
    2,   1,     0,    2,    0,     15,  1,
    3,   0,     100,  1,    1,     NA,  2,
    3,   1,     0,    2,    0,     20,  2
  ) %>% nif()

  result <- ddt(test_data, silent = TRUE)

  # Find RACE row
  race_row <- result[result$name == "RACE", ]

  # Check that type is set to numeric
  expect_true(nrow(race_row) == 1)
  expect_equal(race_row$type, "numeric")

  # Check that description contains race codes
  expect_true(grepl("0 =", race_row$description))
  expect_true(grepl("1 =", race_row$description))
  expect_true(grepl("2 =", race_row$description))
})

test_that("ddt handles numeric RACE with unmapped codes", {
  # Test with RACE codes that don't exist in race_coding
  test_data <- tibble::tribble(
    ~ID, ~TIME, ~AMT, ~CMT, ~EVID, ~DV, ~RACE,
    1,   0,     100,  1,    1,     NA,  99,  # Unmapped code
    1,   1,     0,    2,    0,     10,  99,
    2,   0,     100,  1,    1,     NA,  0,   # Valid code
    2,   1,     0,    2,    0,     15,  0
  ) %>% nif()

  # Should not error, but may produce NA in description
  expect_no_error(result <- ddt(test_data, silent = TRUE))

  race_row <- result[result$name == "RACE", ]
  expect_true(nrow(race_row) == 1)
})

test_that("ddt handles character RACE correctly", {
  # Create test data with character RACE
  test_data <- tibble::tribble(
    ~ID, ~TIME, ~AMT, ~CMT, ~EVID, ~DV, ~RACE,
    1,   0,     100,  1,    1,     NA,  "WHITE",
    1,   1,     0,    2,    0,     10,  "WHITE",
    2,   0,     100,  1,    1,     NA,  "ASIAN",
    2,   1,     0,    2,    0,     15,  "ASIAN"
  ) %>% nif()

  result <- ddt(test_data, silent = TRUE)

  # Find RACE row
  race_row <- result[result$name == "RACE", ]

  # Should not modify character RACE
  expect_true(nrow(race_row) == 1)
  # Type should remain character (from standard fields)
  expect_true(race_row$type == "character" || is.na(race_row$type))
})

test_that("ddt adds additional fields not in standard fields", {
  # Create test data with custom field
  test_data <- tibble::tribble(
    ~ID, ~TIME, ~AMT, ~CMT, ~EVID, ~DV, ~CUSTOM_FIELD,
    1,   0,     100,  1,    1,     NA,  "value1",
    1,   1,     0,    2,    0,     10,  "value2"
  ) %>% nif()

  result <- ddt(test_data, silent = TRUE)

  # Check that custom field is added
  expect_true("CUSTOM_FIELD" %in% result$name)

  custom_row <- result[result$name == "CUSTOM_FIELD", ]
  expect_true(nrow(custom_row) == 1)
  expect_equal(custom_row$type, "character")
})

test_that("ddt handles multiple additional fields", {
  # Create test data with multiple custom fields
  test_data <- tibble::tribble(
    ~ID, ~TIME, ~AMT, ~CMT, ~EVID, ~DV, ~FIELD1, ~FIELD2, ~FIELD3,
    1,   0,     100,  1,    1,     NA,  10,      "A",     TRUE,
    1,   1,     0,    2,    0,     10,  20,      "B",     FALSE
  ) %>% nif()

  result <- ddt(test_data, silent = TRUE)

  # Check that all custom fields are added
  expect_true("FIELD1" %in% result$name)
  expect_true("FIELD2" %in% result$name)
  expect_true("FIELD3" %in% result$name)

  # Check types
  field1_row <- result[result$name == "FIELD1", ]
  field2_row <- result[result$name == "FIELD2", ]
  field3_row <- result[result$name == "FIELD3", ]

  expect_equal(field1_row$type, "numeric")
  expect_equal(field2_row$type, "character")
  expect_equal(field3_row$type, "logical")
})

test_that("ddt handles POSIXct datetime fields correctly", {
  # Test class detection for POSIXct (which has multiple classes)
  test_data <- tibble::tribble(
    ~ID, ~TIME, ~AMT, ~CMT, ~EVID, ~DV, ~DTC,
    1,   0,     100,  1,    1,     NA,  as.POSIXct("2024-01-01 08:00:00"),
    1,   1,     0,    2,    0,     10,  as.POSIXct("2024-01-01 12:00:00")
  ) %>% nif()

  result <- ddt(test_data, silent = TRUE)

  # DTC should be in standard fields, but if it's additional, check class handling
  dtc_row <- result[result$name == "DTC", ]
  if (nrow(dtc_row) > 0) {
    # Should handle POSIXct class correctly
    expect_true(!is.null(dtc_row$type))
  }
})

# test_that("ddt filters out standard fields not present in object", {
#   # Create minimal nif object without all standard fields
#   test_data <- tibble::tribble(
#     ~ID, ~TIME, ~AMT, ~CMT, ~EVID, ~DV
#     # No STUDYID, USUBJID, etc.
#   ) %>% nif()
#
#   result <- ddt(test_data, silent = TRUE)
#
#   # Should only include fields that are in the object
#   expect_true(all(result$name %in% names(test_data)))
# })

test_that("ddt handles empty nif object", {
  # Create empty nif object
  test_data <- nif()

  # Should not error
  expect_no_error(result <- ddt(test_data, silent = TRUE))

  # Should return data frame with standard structure
  expect_s3_class(result, "data.frame")
  expect_true(all(c("name", "definition", "type", "description", "unit", "source") %in% names(result)))
})

test_that("ddt handles empty nif object with CMT processing", {
  # Create empty nif object
  test_data <- nif()

  result <- ddt(test_data, silent = TRUE)

  # CMT description should handle empty temp gracefully
  cmt_row <- result[result$name == "CMT", ]
  if (nrow(cmt_row) > 0) {
    # Description might be empty string or original value
    expect_true(is.character(cmt_row$description))
  }
})

test_that("ddt shows warning for additional fields when silent is FALSE", {
  # Create test data with custom field
  test_data <- tibble::tribble(
    ~ID, ~TIME, ~AMT, ~CMT, ~EVID, ~DV, ~CUSTOM_FIELD,
    1,   0,     100,  1,    1,     NA,  "value1"
  ) %>% nif()

  # Capture warnings
  old_silent <- nif_option_value("silent")
  nif_option("silent" = FALSE)

  expect_message(
    result <- ddt(test_data, silent = FALSE),
    "Some data definition fields could not be generated"
  )

  nif_option("silent" = old_silent)
})

test_that("ddt suppresses warning when silent is TRUE", {
  # Create test data with custom field
  test_data <- tibble::tribble(
    ~ID, ~TIME, ~AMT, ~CMT, ~EVID, ~DV, ~CUSTOM_FIELD,
    1,   0,     100,  1,    1,     NA,  "value1"
  ) %>% nif()

  # Should not show warning
  expect_no_warning(result <- ddt(test_data, silent = TRUE))
})

test_that("ddt uses nif_option silent setting when silent is NULL", {
  # Create test data with custom field
  test_data <- tibble::tribble(
    ~ID, ~TIME, ~AMT, ~CMT, ~EVID, ~DV, ~CUSTOM_FIELD,
    1,   0,     100,  1,    1,     NA,  "value1"
  ) %>% nif()

  # Test with nif_option setting
  old_silent <- nif_option_value("silent")
  nif_option("silent" = TRUE)

  expect_no_warning(result <- ddt(test_data, silent = NULL))

  nif_option("silent" = old_silent)
})

test_that("ddt validates input is nif object", {
  # Test with non-nif object
  test_data <- data.frame(
    ID = 1,
    TIME = 0,
    AMT = 100,
    CMT = 1,
    EVID = 1,
    DV = NA
  )

  expect_error(
    ddt(test_data, silent = TRUE),
    "Input must be a nif object"
  )
})

test_that("ddt handles multiple compartments with same analyte", {
  # Test with same analyte in different compartments
  test_data <- tibble::tribble(
    ~ID, ~TIME, ~AMT, ~CMT, ~EVID, ~DV, ~ANALYTE,
    1,   0,     100,  1,    1,     NA,  "DRUG",
    1,   1,     0,    1,    0,     10,  "DRUG",
    1,   2,     0,    2,    0,     5,   "DRUG"
  ) %>% nif()

  result <- ddt(test_data, silent = TRUE)

  cmt_row <- result[result$name == "CMT", ]
  expect_true(nrow(cmt_row) == 1)
  # Should show both compartments
  expect_true(grepl("1", cmt_row$description))
  expect_true(grepl("2", cmt_row$description))
})

test_that("ddt handles administrations and observations separately", {
  # Test that EVID 0 and 1 are distinguished
  test_data <- tibble::tribble(
    ~ID, ~TIME, ~AMT, ~CMT, ~EVID, ~DV, ~ANALYTE,
    1,   0,     100,  1,    1,     NA,  "DRUG",      # Administration
    1,   1,     0,    1,    0,     10,  "DRUG",      # Observation
    1,   2,     0,    2,    0,     5,   "METABOLITE" # Observation
  ) %>% nif()

  result <- ddt(test_data, silent = TRUE)

  cmt_row <- result[result$name == "CMT", ]
  expect_true(nrow(cmt_row) == 1)
  # Should distinguish between administration and observation
  expect_true(grepl("administration", cmt_row$description) ||
              grepl("observation", cmt_row$description))
})

test_that("ddt handles RACE with all valid codes", {
  # Test with all valid RACE codes
  test_data <- tibble::tribble(
    ~ID, ~TIME, ~AMT, ~CMT, ~EVID, ~DV, ~RACE,
    1,   0,     100,  1,    1,     NA,  0,
    2,   0,     100,  1,    1,     NA,  1,
    3,   0,     100,  1,    1,     NA,  2,
    4,   0,     100,  1,    1,     NA,  3,
    5,   0,     100,  1,    1,     NA,  4,
    6,   0,     100,  1,    1,     NA,  5,
    7,   0,     100,  1,    1,     NA,  6,
    8,   0,     100,  1,    1,     NA,  7
  ) %>% nif()

  result <- ddt(test_data, silent = TRUE)

  race_row <- result[result$name == "RACE", ]
  expect_true(nrow(race_row) == 1)
  expect_equal(race_row$type, "numeric")
  # Should have descriptions for all codes
  expect_true(grepl("WHITE|White", race_row$description) ||
              grepl("0 =", race_row$description))
})

test_that("ddt preserves standard field metadata", {
  # Test that standard fields keep their original metadata
  test_data <- tibble::tribble(
    ~ID, ~TIME, ~AMT, ~CMT, ~EVID, ~DV, ~STUDYID,
    1,   0,     100,  1,    1,     NA,  "STUDY1"
  ) %>% nif()

  result <- ddt(test_data, silent = TRUE)

  # Check that STUDYID has its standard definition
  studyid_row <- result[result$name == "STUDYID", ]
  expect_true(nrow(studyid_row) == 1)
  expect_true(!is.na(studyid_row$definition))
  expect_equal(studyid_row$definition, "Study")
})

test_that("ddt handles complex nif object with many fields", {
  # Create comprehensive test data
  test_data <- tibble::tribble(
    ~ID, ~TIME, ~AMT, ~CMT, ~EVID, ~DV, ~STUDYID, ~USUBJID, ~ANALYTE, ~RACE, ~AGE, ~SEX,
    1,   0,     100,  1,    1,     NA,  "STUDY1", "SUBJ-001", "DRUG", 0, 30, 0,
    1,   1,     0,    2,    0,     10,  "STUDY1", "SUBJ-001", "METABOLITE", 0, 30, 0,
    2,   0,     150,  1,    1,     NA,  "STUDY1", "SUBJ-002", "DRUG", 1, 25, 1,
    2,   1,     0,    2,    0,     15,  "STUDY1", "SUBJ-002", "METABOLITE", 1, 25, 1
  ) %>% nif()

  result <- ddt(test_data, silent = TRUE)

  # Should include all fields
  expect_true(all(c("ID", "TIME", "AMT", "CMT", "EVID", "DV", "STUDYID",
                    "USUBJID", "ANALYTE", "RACE", "AGE", "SEX") %in% result$name))

  # Should have correct number of rows
  expect_equal(nrow(result), length(unique(result$name)))
})

