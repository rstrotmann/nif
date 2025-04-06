test_that("import_from_connection handles CSV data correctly", {
  # Create CSV data
  csv_data <- c(
    "USUBJID,STUDYID,PARENT,ANALYTE,TIME,DV,CMT",
    "SUBJ-001,STUDY-001,DRUG-A,ANALYTE-X,0,0,1",
    "SUBJ-001,STUDY-001,DRUG-A,ANALYTE-X,1,10.5,2",
    "SUBJ-002,STUDY-001,DRUG-A,ANALYTE-X,0,0,1",
    "SUBJ-002,STUDY-001,DRUG-A,ANALYTE-X,2,15.3,2"
  )

  # Create a connection
  con <- textConnection(csv_data)

  # Import data
  result <- import_from_connection(con, format = "csv", silent = TRUE)

  # Close connection
  close(con)

  # Test expectations
  expect_s3_class(result, "nif")
  expect_equal(nrow(result), 4)
  expect_equal(ncol(result), 11)
  expect_equal(result$USUBJID[1], "SUBJ-001")
  expect_equal(result$DV[2], 10.5)
  expect_equal(result$CMT[3], 1)
})


test_that("import_from_connection handles fixed-width data correctly", {
  # Create fixed-width data
  fixed_data <- c(
    "USUBJID  STUDYID   PARENT  ANALYTE   TIME  DV    CMT",
    "SUBJ-001 STUDY-001 DRUG-A  ANALYTE-X 0     0     1  ",
    "SUBJ-001 STUDY-001 DRUG-A  ANALYTE-X 1     10.5  2  ",
    "SUBJ-002 STUDY-001 DRUG-A  ANALYTE-X 0     0     1  ",
    "SUBJ-002 STUDY-001 DRUG-A  ANALYTE-X 2     15.3  2  "
  )

  # Create a connection
  con <- textConnection(fixed_data)

  # Import data
  result <- import_from_connection(con, format = "fixed_width", silent = TRUE)

  # Close connection
  close(con)

  # Test expectations
  expect_s3_class(result, "nif")
  expect_equal(nrow(result), 4)
  expect_equal(ncol(result), 11)
  expect_equal(result$USUBJID[1], "SUBJ-001")
  expect_equal(result$DV[2], 10.5)
  expect_equal(result$CMT[3], 1)
})


test_that("import_from_connection auto-detects CSV format", {
  # Create CSV data
  csv_data <- c(
    "USUBJID,STUDYID,PARENT,ANALYTE,TIME,DV,CMT",
    "SUBJ-001,STUDY-001,DRUG-A,ANALYTE-X,0,0,1"
  )

  # Create a connection
  con <- textConnection(csv_data)

  # Import data
  result <- import_from_connection(con, silent = TRUE)

  # Close connection
  close(con)

  # Test expectations
  expect_s3_class(result, "nif")
  expect_equal(nrow(result), 1)
})


test_that("import_from_connection auto-detects fixed-width format", {
  # Create fixed-width data
  fixed_data <- c(
    "USUBJID  STUDYID   PARENT  ANALYTE   TIME  DV    CMT",
    "SUBJ-001 STUDY-001 DRUG-A  ANALYTE-X 0     0     1  "
  )

  # Create a connection
  con <- textConnection(fixed_data)

  # Import data
  result <- import_from_connection(con, silent = TRUE)

  # Close connection
  close(con)

  # Test expectations
  expect_s3_class(result, "nif")
  expect_equal(nrow(result), 1)
})


test_that("import_from_connection ignores comments and empty lines", {
  # Create CSV data with comments and empty lines
  csv_data <- c(
    "# This is a comment",
    "",
    "USUBJID,STUDYID,PARENT,ANALYTE,TIME,DV,CMT",
    "# Another comment",
    "",
    "SUBJ-001,STUDY-001,DRUG-A,ANALYTE-X,0,0,1",
    "# Yet another comment",
    "SUBJ-002,STUDY-001,DRUG-A,ANALYTE-X,1,10.5,2"
  )

  # Create a connection
  con <- textConnection(csv_data)

  # Import data
  result <- import_from_connection(con, silent = TRUE)

  # Close connection
  close(con)

  # Test expectations
  expect_s3_class(result, "nif")
  expect_equal(nrow(result), 2)
})


test_that("import_from_connection handles datetime conversion", {
  # Create CSV data with datetime
  csv_data <- c(
    "USUBJID,STUDYID,PARENT,ANALYTE,TIME,DV,CMT,DTC",
    "SUBJ-001,STUDY-001,DRUG-A,ANALYTE-X,0,0,1,2023-01-01T12:00:00",
    "SUBJ-002,STUDY-001,DRUG-A,ANALYTE-X,1,10.5,2,2023-01-02T14:30:00"
  )

  # Create a connection
  con <- textConnection(csv_data)

  # Import data
  result <- import_from_connection(con, silent = TRUE)

  # Close connection
  close(con)

  # Test expectations
  expect_s3_class(result, "nif")
  expect_s3_class(result$DTC, "POSIXct")
  expect_equal(format(result$DTC[1], "%Y-%m-%d %H:%M:%S"), "2023-01-01 12:00:00")
})


test_that("import_from_connection handles custom delimiters", {
  # Create CSV data with semicolon delimiter
  csv_data <- c(
    "USUBJID;STUDYID;PARENT;ANALYTE;TIME;DV;CMT",
    "SUBJ-001;STUDY-001;DRUG-A;ANALYTE-X;0;0;1",
    "SUBJ-002;STUDY-001;DRUG-A;ANALYTE-X;1;10.5;2"
  )

  # Create a connection
  con <- textConnection(csv_data)

  # Import data
  result <- import_from_connection(con, delimiter = ";", silent = TRUE)

  # Close connection
  close(con)

  # Test expectations
  expect_s3_class(result, "nif")
  expect_equal(nrow(result), 2)
  expect_equal(result$USUBJID[1], "SUBJ-001")
})


test_that("import_from_connection adds missing fields", {
  # Create CSV data with missing fields
  csv_data <- c(
    "USUBJID,STUDYID,DV",
    "SUBJ-001,STUDY-001,10.5"
  )

  # Create a connection
  con <- textConnection(csv_data)

  # Import data (suppress messages for test)
  result <- suppressMessages(import_from_connection(con, silent = TRUE))

  # Close connection
  close(con)

  # Test expectations
  expect_s3_class(result, "nif")
  expect_true("TIME" %in% names(result))
  expect_true(is.na(result$TIME[1]))
})


test_that("import_from_connection respects no_numeric parameter", {
  # Create CSV data
  csv_data <- c(
    "USUBJID,STUDYID,NUMERIC_ID,TIME,DV",
    "SUBJ-001,STUDY-001,12345,0,0",
    "SUBJ-002,STUDY-001,56789,1,10.5"
  )

  # Create a connection
  con <- textConnection(csv_data)

  # Import data with NUMERIC_ID kept as character
  result <- import_from_connection(
    con, no_numeric = c("USUBJID", "STUDYID", "NUMERIC_ID"), silent = TRUE)

  # Close connection
  close(con)

  # Test expectations
  expect_s3_class(result, "nif")
  expect_type(result$NUMERIC_ID, "character")
  expect_type(result$TIME, "integer")
})


test_that("import_from_connection handles errors correctly", {
  # Test with invalid connection
  expect_error(import_from_connection("not a connection"))

  # Test with empty data
  con <- textConnection("")
  expect_error(import_from_connection(con))
  close(con)

  # Test with ambiguous format
  ambiguous_data <- c("A B,C")
  con <- textConnection(ambiguous_data)
  expect_error(import_from_connection(con))
  close(con)
})


test_that("import_from_connection detects invalid fixed-width format", {
  # Create data with numeric headers which don't match the expected pattern
  invalid_fixed_data <- c(
    "123  456   789  ABC",
    "aaa  bbb   ccc  ddd"
  )

  # Create a connection
  con <- textConnection(invalid_fixed_data)

  # Import should fail with specific error message
  expect_error(
    import_from_connection(con, silent = TRUE),
    "Auto-detected fixed-width format, but couldn't properly identify column headers"
  )

  # Close connection
  close(con)
})


test_that("import_from_connection detects inconsistent CSV format", {
  # Create CSV data with inconsistent field counts
  inconsistent_csv_data <- c(
    "USUBJID,STUDYID,PARENT,ANALYTE,TIME,DV,CMT",
    "SUBJ-001,STUDY-001,DRUG-A,ANALYTE-X,0,0" # Missing one field
  )

  # Create a connection
  con <- textConnection(inconsistent_csv_data)

  # Import should fail with specific error message
  expect_error(
    import_from_connection(con, silent = TRUE),
    "Auto-detected CSV format, but the number of fields is inconsistent"
  )

  # Close connection
  close(con)
})
