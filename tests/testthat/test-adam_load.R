test_that("read_adam validates data_path parameter", {
  # Test invalid data_path
  expect_error(
    read_adam("nonexistent/path"),
    "data_path does not exist"
  )

  # Test NULL data_path
  expect_error(
    read_adam(NULL),
    "must not be NULL"
  )

  # Test non-character data_path
  expect_error(
    read_adam(123),
    "must be a character value"
  )
})


test_that("read_adam validates format parameter", {
  test_dir <- tempfile("adam_test")
  dir.create(test_dir)
  on.exit(unlink(test_dir, recursive = TRUE))

  # Test invalid format
  expect_error(
    read_adam(test_dir, format = "invalid"),
    "format must be one of: sas, xpt, csv"
  )

  # Test NULL format
  expect_error(
    read_adam(test_dir, format = NULL),
    "must not be NULL"
  )

  # Test non-character format
  expect_error(
    read_adam(test_dir, format = 123),
    "must be a character value"
  )
})


test_that("read_adam validates dataset parameter", {
  test_dir <- tempfile("adam_test")
  dir.create(test_dir)
  on.exit(unlink(test_dir, recursive = TRUE))

  # Test non-character dataset
  expect_error(
    read_adam(test_dir, dataset = 123),
    "must be a character value"
  )

  # NULL dataset should be allowed (auto-discovery)
  # This will fail because no files exist, but should not fail on validation
  expect_error(
    read_adam(test_dir, dataset = NULL),
    "no dataset found"
  )
})


test_that("read_adam reads SAS format correctly", {
  test_dir <- tempfile("adam_test")
  dir.create(test_dir)
  on.exit(unlink(test_dir, recursive = TRUE))

  # Create test data
  dm_data <- tibble::tribble(
    ~USUBJID, ~STUDYID, ~AGE, ~SEX,
    "SUBJ-001", "STUDY-001", 30, "M",
    "SUBJ-002", "STUDY-001", 25, "F"
  )

  # Write SAS file
  suppressWarnings(
    haven::write_sas(dm_data, file.path(test_dir, "DM.sas7bdat"))
  )

  # Test reading SAS format
  result <- read_adam(test_dir, dataset = "DM", format = "sas")
  expect_s3_class(result, "adam")
  expect_equal(length(result), 1)
  # SAS format converts names to lowercase
  expect_equal(names(result), "dm")
  expect_equal(nrow(result$dm), 2)
  expect_equal(result$dm$USUBJID, c("SUBJ-001", "SUBJ-002"))
})


test_that("read_adam reads XPT format correctly", {
  test_dir <- tempfile("adam_test")
  dir.create(test_dir)
  on.exit(unlink(test_dir, recursive = TRUE))

  # Create test data
  dm_data <- tibble::tribble(
    ~USUBJID, ~STUDYID, ~AGE,
    "SUBJ-001", "STUDY-001", 30,
    "SUBJ-002", "STUDY-001", 25
  )

  # Write XPT file
  haven::write_xpt(dm_data, file.path(test_dir, "DM.xpt"))

  # Test reading XPT format
  result <- read_adam(test_dir, dataset = "DM", format = "xpt")
  expect_s3_class(result, "adam")
  expect_equal(length(result), 1)
  # XPT format keeps original case
  expect_equal(names(result), "DM")
  expect_equal(nrow(result$DM), 2)
  expect_equal(result$DM$USUBJID, c("SUBJ-001", "SUBJ-002"))
})


test_that("read_adam reads CSV format correctly", {
  test_dir <- tempfile("adam_test")
  dir.create(test_dir)
  on.exit(unlink(test_dir, recursive = TRUE))

  # Create test data
  dm_data <- tibble::tribble(
    ~USUBJID, ~STUDYID, ~AGE,
    "SUBJ-001", "STUDY-001", 30,
    "SUBJ-002", "STUDY-001", 25
  )

  # Write CSV file
  write.csv(dm_data, file.path(test_dir, "DM.csv"), row.names = FALSE)

  # Test reading CSV format
  result <- read_adam(test_dir, dataset = "DM", format = "csv")
  expect_s3_class(result, "adam")
  expect_equal(length(result), 1)
  # CSV format keeps original case
  expect_equal(names(result), "DM")
  expect_equal(nrow(result$DM), 2)
  expect_equal(result$DM$USUBJID, c("SUBJ-001", "SUBJ-002"))
})


test_that("read_adam handles custom delimiters for CSV", {
  test_dir <- tempfile("adam_test")
  dir.create(test_dir)
  on.exit(unlink(test_dir, recursive = TRUE))

  # Create test data
  dm_data <- tibble::tribble(
    ~USUBJID, ~STUDYID, ~AGE,
    "SUBJ-001", "STUDY-001", 30,
    "SUBJ-002", "STUDY-001", 25
  )

  # Write CSV with semicolon delimiter
  write.csv2(dm_data, file.path(test_dir, "DM.csv"), row.names = FALSE)

  # Test reading with semicolon delimiter
  result <- read_adam(test_dir, dataset = "DM", format = "csv", delim = ";")
  expect_s3_class(result, "adam")
  expect_equal(nrow(result$DM), 2)
  expect_equal(result$DM$USUBJID, c("SUBJ-001", "SUBJ-002"))
})


test_that("read_adam handles multiple datasets", {
  test_dir <- tempfile("adam_test")
  dir.create(test_dir)
  on.exit(unlink(test_dir, recursive = TRUE))

  # Create test data
  dm_data <- tibble::tribble(
    ~USUBJID, ~STUDYID,
    "SUBJ-001", "STUDY-001"
  )

  ex_data <- tibble::tribble(
    ~USUBJID, ~STUDYID, ~EXTRT,
    "SUBJ-001", "STUDY-001", "DRUG A"
  )

  # Write test files
  suppressWarnings({
    haven::write_sas(dm_data, file.path(test_dir, "DM.sas7bdat"))
    haven::write_sas(ex_data, file.path(test_dir, "EX.sas7bdat"))
  })

  # Test reading multiple datasets
  result <- read_adam(test_dir, dataset = c("DM", "EX"), format = "sas")
  expect_s3_class(result, "adam")
  expect_equal(length(result), 2)
  expect_equal(names(result), c("dm", "ex"))
  expect_equal(nrow(result$dm), 1)
  expect_equal(nrow(result$ex), 1)
})


test_that("read_adam auto-discovers datasets when dataset is NULL", {
  test_dir <- tempfile("adam_test")
  dir.create(test_dir)
  on.exit(unlink(test_dir, recursive = TRUE))

  # Create test data
  dm_data <- tibble::tribble(
    ~USUBJID, ~STUDYID,
    "SUBJ-001", "STUDY-001"
  )

  ex_data <- tibble::tribble(
    ~USUBJID, ~STUDYID,
    "SUBJ-001", "STUDY-001"
  )

  # Write test files
  suppressWarnings({
    haven::write_sas(dm_data, file.path(test_dir, "DM.sas7bdat"))
    haven::write_sas(ex_data, file.path(test_dir, "EX.sas7bdat"))
  })

  # Test auto-discovery
  result <- read_adam(test_dir, format = "sas")
  expect_s3_class(result, "adam")
  expect_equal(length(result), 2)
  expect_true("dm" %in% names(result))
  expect_true("ex" %in% names(result))
})


test_that("read_adam omits files starting with underscore", {
  test_dir <- tempfile("adam_test")
  dir.create(test_dir)
  on.exit(unlink(test_dir, recursive = TRUE))

  # Create test data
  dm_data <- tibble::tribble(
    ~USUBJID, ~STUDYID,
    "SUBJ-001", "STUDY-001"
  )

  # Write test files including one starting with underscore
  suppressWarnings({
    haven::write_sas(dm_data, file.path(test_dir, "DM.sas7bdat"))
    haven::write_sas(dm_data, file.path(test_dir, "_ADSL.sas7bdat"))
  })

  # Test auto-discovery - should omit _ADSL
  result <- read_adam(test_dir, format = "sas")
  expect_s3_class(result, "adam")
  expect_equal(length(result), 1)
  expect_equal(names(result), "dm")
  expect_false("_adsl" %in% names(result))
})


test_that("read_adam handles missing files correctly", {
  test_dir <- tempfile("adam_test")
  dir.create(test_dir)
  on.exit(unlink(test_dir, recursive = TRUE))

  # Test missing file
  expect_error(
    read_adam(test_dir, dataset = "NONEXISTENT", format = "sas"),
    "The following files do not exist"
  )

  # Test multiple missing files
  expect_error(
    read_adam(test_dir, dataset = c("DM", "EX", "NONEXISTENT"), format = "sas"),
    "The following files do not exist"
  )
})


test_that("read_adam handles empty directory", {
  test_dir <- tempfile("adam_test")
  dir.create(test_dir)
  on.exit(unlink(test_dir, recursive = TRUE))

  # Test empty directory with auto-discovery
  expect_error(
    read_adam(test_dir, format = "sas"),
    "no dataset found"
  )
})


test_that("read_adam handles case sensitivity in dataset names for SAS", {
  test_dir <- tempfile("adam_test")
  dir.create(test_dir)
  on.exit(unlink(test_dir, recursive = TRUE))

  # Create test data
  dm_data <- tibble::tribble(
    ~USUBJID, ~STUDYID,
    "SUBJ-001", "STUDY-001"
  )

  # Write file with uppercase name
  suppressWarnings(
    haven::write_sas(dm_data, file.path(test_dir, "DM.sas7bdat"))
  )

  # Test reading with different case
  result <- read_adam(test_dir, dataset = "dm", format = "sas")
  expect_s3_class(result, "adam")
  expect_equal(names(result), "dm")

  # Test reading with uppercase
  result2 <- read_adam(test_dir, dataset = "DM", format = "sas")
  expect_s3_class(result2, "adam")
  expect_equal(names(result2), "dm")
})


test_that("read_adam preserves case for XPT format", {
  test_dir <- tempfile("adam_test")
  dir.create(test_dir)
  on.exit(unlink(test_dir, recursive = TRUE))

  # Create test data
  dm_data <- tibble::tribble(
    ~USUBJID, ~STUDYID,
    "SUBJ-001", "STUDY-001"
  )

  # Write file with uppercase name
  haven::write_xpt(dm_data, file.path(test_dir, "DM.xpt"))

  # Test reading - should preserve case
  result <- read_adam(test_dir, dataset = "DM", format = "xpt")
  expect_s3_class(result, "adam")
  expect_equal(names(result), "DM")
})


test_that("read_adam preserves case for CSV format", {
  test_dir <- tempfile("adam_test")
  dir.create(test_dir)
  on.exit(unlink(test_dir, recursive = TRUE))

  # Create test data
  dm_data <- tibble::tribble(
    ~USUBJID, ~STUDYID,
    "SUBJ-001", "STUDY-001"
  )

  # Write file with uppercase name
  write.csv(dm_data, file.path(test_dir, "DM.csv"), row.names = FALSE)

  # Test reading - should preserve case
  result <- read_adam(test_dir, dataset = "DM", format = "csv")
  expect_s3_class(result, "adam")
  expect_equal(names(result), "DM")
})


test_that("read_adam handles mixed case in auto-discovery for SAS", {
  test_dir <- tempfile("adam_test")
  dir.create(test_dir)
  on.exit(unlink(test_dir, recursive = TRUE))

  # Create test data
  dm_data <- tibble::tribble(
    ~USUBJID, ~STUDYID,
    "SUBJ-001", "STUDY-001"
  )

  ex_data <- tibble::tribble(
    ~USUBJID, ~STUDYID,
    "SUBJ-001", "STUDY-001"
  )

  # Write files with different cases
  suppressWarnings({
    haven::write_sas(dm_data, file.path(test_dir, "DM.sas7bdat"))
    haven::write_sas(ex_data, file.path(test_dir, "ex.sas7bdat"))
  })

  # Test auto-discovery - should convert to lowercase
  result <- read_adam(test_dir, format = "sas")
  expect_s3_class(result, "adam")
  expect_equal(length(result), 2)
  expect_equal(sort(names(result)), c("dm", "ex"))
})


test_that("read_adam handles multiple formats in same directory", {
  test_dir <- tempfile("adam_test")
  dir.create(test_dir)
  on.exit(unlink(test_dir, recursive = TRUE))

  # Create test data
  dm_data <- tibble::tribble(
    ~USUBJID, ~STUDYID,
    "SUBJ-001", "STUDY-001"
  )

  # Write files in different formats
  suppressWarnings(
    haven::write_sas(dm_data, file.path(test_dir, "DM.sas7bdat"))
  )
  haven::write_xpt(dm_data, file.path(test_dir, "DM.xpt"))
  write.csv(dm_data, file.path(test_dir, "DM.csv"), row.names = FALSE)

  # Test reading each format
  result_sas <- read_adam(test_dir, dataset = "DM", format = "sas")
  expect_s3_class(result_sas, "adam")
  expect_equal(names(result_sas), "dm")

  result_xpt <- read_adam(test_dir, dataset = "DM", format = "xpt")
  expect_s3_class(result_xpt, "adam")
  expect_equal(names(result_xpt), "DM")

  result_csv <- read_adam(test_dir, dataset = "DM", format = "csv")
  expect_s3_class(result_csv, "adam")
  expect_equal(names(result_csv), "DM")
})


test_that("read_adam handles files with special characters in names", {
  test_dir <- tempfile("adam_test")
  dir.create(test_dir)
  on.exit(unlink(test_dir, recursive = TRUE))

  # Create test data
  dm_data <- tibble::tribble(
    ~USUBJID, ~STUDYID,
    "SUBJ-001", "STUDY-001"
  )

  # Write file with special characters (if supported)
  suppressWarnings(
    haven::write_sas(dm_data, file.path(test_dir, "DM_TEST.sas7bdat"))
  )

  # Test reading
  result <- read_adam(test_dir, dataset = "DM_TEST", format = "sas")
  expect_s3_class(result, "adam")
  expect_equal(names(result), "dm_test")
})


test_that("read_adam returns adam object with correct structure", {
  test_dir <- tempfile("adam_test")
  dir.create(test_dir)
  on.exit(unlink(test_dir, recursive = TRUE))

  # Create test data
  dm_data <- tibble::tribble(
    ~USUBJID, ~STUDYID, ~AGE,
    "SUBJ-001", "STUDY-001", 30,
    "SUBJ-002", "STUDY-001", 25
  )

  # Write test file
  suppressWarnings(
    haven::write_sas(dm_data, file.path(test_dir, "DM.sas7bdat"))
  )

  # Test result structure
  result <- read_adam(test_dir, dataset = "DM", format = "sas")
  expect_s3_class(result, "adam")
  expect_s3_class(result, "list")
  expect_true(is.list(result))
  expect_equal(length(result), 1)
  expect_s3_class(result$dm, "data.frame")
})


test_that("read_adam handles empty datasets", {
  test_dir <- tempfile("adam_test")
  dir.create(test_dir)
  on.exit(unlink(test_dir, recursive = TRUE))

  # Create empty test data
  dm_data <- tibble::tribble(
    ~USUBJID, ~STUDYID
  )

  # Write test file
  suppressWarnings(
    haven::write_sas(dm_data, file.path(test_dir, "DM.sas7bdat"))
  )

  # Test reading empty dataset
  result <- read_adam(test_dir, dataset = "DM", format = "sas")
  expect_s3_class(result, "adam")
  expect_equal(nrow(result$dm), 0)
  expect_equal(names(result$dm), c("USUBJID", "STUDYID"))
})


test_that("read_adam handles large datasets", {
  test_dir <- tempfile("adam_test")
  dir.create(test_dir)
  on.exit(unlink(test_dir, recursive = TRUE))

  # Create larger test data
  dm_data <- tibble::tribble(
    ~USUBJID, ~STUDYID, ~AGE,
    "SUBJ-001", "STUDY-001", 30,
    "SUBJ-002", "STUDY-001", 25,
    "SUBJ-003", "STUDY-001", 35,
    "SUBJ-004", "STUDY-001", 28,
    "SUBJ-005", "STUDY-001", 32
  )

  # Write test file
  suppressWarnings(
    haven::write_sas(dm_data, file.path(test_dir, "DM.sas7bdat"))
  )

  # Test reading
  result <- read_adam(test_dir, dataset = "DM", format = "sas")
  expect_s3_class(result, "adam")
  expect_equal(nrow(result$dm), 5)
  expect_equal(ncol(result$dm), 3)
})


test_that("read_adam handles datasets with many columns", {
  test_dir <- tempfile("adam_test")
  dir.create(test_dir)
  on.exit(unlink(test_dir, recursive = TRUE))

  # Create test data with many columns
  dm_data <- tibble::tribble(
    ~USUBJID, ~STUDYID, ~AGE, ~SEX, ~RACE, ~ETHNIC, ~COUNTRY,
    "SUBJ-001", "STUDY-001", 30, "M", "WHITE", "NOT HISPANIC", "USA",
    "SUBJ-002", "STUDY-001", 25, "F", "ASIAN", "NOT HISPANIC", "USA"
  )

  # Write test file
  suppressWarnings(
    haven::write_sas(dm_data, file.path(test_dir, "DM.sas7bdat"))
  )

  # Test reading
  result <- read_adam(test_dir, dataset = "DM", format = "sas")
  expect_s3_class(result, "adam")
  expect_equal(ncol(result$dm), 7)
  expect_equal(
    names(result$dm),
    c("USUBJID", "STUDYID", "AGE", "SEX", "RACE", "ETHNIC", "COUNTRY"))
})


test_that("read_adam passes additional parameters to read functions", {
  test_dir <- tempfile("adam_test")
  dir.create(test_dir)
  on.exit(unlink(test_dir, recursive = TRUE))

  # Create test data
  dm_data <- tibble::tribble(
    ~USUBJID, ~STUDYID, ~AGE,
    "SUBJ-001", "STUDY-001", 30,
    "SUBJ-002", "STUDY-001", 25
  )

  # Write CSV file
  write.csv(dm_data, file.path(test_dir, "DM.csv"), row.names = FALSE)

  # Test reading with additional parameters (locale)
  result <- read_adam(test_dir,
    dataset = "DM", format = "csv",
    locale = readr::locale(encoding = "UTF-8")
  )
  expect_s3_class(result, "adam")
  expect_equal(nrow(result$DM), 2)
})


test_that("read_adam handles file path with spaces", {
  test_dir <- tempfile("adam test")
  dir.create(test_dir)
  on.exit(unlink(test_dir, recursive = TRUE))

  # Create test data
  dm_data <- tibble::tribble(
    ~USUBJID, ~STUDYID,
    "SUBJ-001", "STUDY-001"
  )

  # Write test file
  suppressWarnings(
    haven::write_sas(dm_data, file.path(test_dir, "DM.sas7bdat"))
  )

  # Test reading
  result <- read_adam(test_dir, dataset = "DM", format = "sas")
  expect_s3_class(result, "adam")
  expect_equal(nrow(result$dm), 1)
})
