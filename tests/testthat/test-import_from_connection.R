test_that("import_from_connection handles CSV data correctly", {
  # Create CSV data
  csv_data <- c(
    "USUBJID,STUDYID,PARENT,ANALYTE,TIME,DV,CMT",
    "SUBJ-001,STUDY-001,DRUG-A,ANALYTE-X,0,0,1",
    "SUBJ-001,STUDY-001,DRUG-A,ANALYTE-X,1,10.5,2",
    "SUBJ-002,STUDY-001,DRUG-A,ANALYTE-X,0,0,1",
    "SUBJ-002,STUDY-001,DRUG-A,ANALYTE-X,2,15.3,2"
  )

  # USUBJID, 	  STUDYID,	PARENT,	  ANALYTE,	TIME,	  DV,	   CMT
  # SUBJ-001,	STUDY-001,	DRUG-A,	ANALYTE-X,	0,	    0,	     1
  # SUBJ-001,	STUDY-001,	DRUG-A,	ANALYTE-X,	1,	    10.5,    2
  # SUBJ-002,	STUDY-001,	DRUG-A,	ANALYTE-X,	0,	    0,	     1
  # SUBJ-002,	STUDY-001,	DRUG-A,	ANALYTE-X,	2,	    15.3,	   2

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
    con,
    no_numeric = c("USUBJID", "STUDYID", "NUMERIC_ID"), silent = TRUE
  )

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


test_that("import_from_connection handles simple column renaming", {
  # Create CSV data with a column to rename
  csv_data <- c(
    "USUBJID,STUDYID,OLD_COL,TIME,DV",
    "SUBJ-001,STUDY-001,value1,0,10.5",
    "SUBJ-002,STUDY-001,value2,1,15.3"
  )

  # Create a connection
  con <- textConnection(csv_data)

  # Import data with renaming term
  result <- import_from_connection(
    con,
    NEW_COL ~ OLD_COL,
    format = "csv",
    silent = TRUE
  )

  # Close connection
  close(con)

  # Test expectations
  expect_s3_class(result, "nif")
  expect_true("NEW_COL" %in% names(result))
  expect_true("OLD_COL" %in% names(result))
  expect_equal(result$NEW_COL[1], "value1")
  expect_equal(result$NEW_COL[2], "value2")
})


test_that("import_from_connection handles renaming with expressions", {
  # Create CSV data
  csv_data <- c(
    "USUBJID,STUDYID,TIME,DV",
    "SUBJ-001,STUDY-001,0,10.5",
    "SUBJ-002,STUDY-001,1,15.3"
  )

  # Create a connection
  con <- textConnection(csv_data)

  # Import data with renaming term that includes an expression
  result <- import_from_connection(
    con,
    DV_DOUBLED ~ DV * 2,
    format = "csv",
    silent = TRUE
  )

  # Close connection
  close(con)

  # Test expectations
  expect_s3_class(result, "nif")
  expect_true("DV_DOUBLED" %in% names(result))
  expect_equal(result$DV_DOUBLED[1], 21.0)
  expect_equal(result$DV_DOUBLED[2], 30.6)
})


test_that("import_from_connection handles multiple renaming terms", {
  # Create CSV data
  csv_data <- c(
    "USUBJID,STUDYID,OLD_COL1,OLD_COL2,TIME,DV",
    "SUBJ-001,STUDY-001,val1,val2,0,10.5",
    "SUBJ-002,STUDY-001,val3,val4,1,15.3"
  )

  # Create a connection
  con <- textConnection(csv_data)

  # Import data with multiple renaming terms
  result <- import_from_connection(
    con,
    NEW_COL1 ~ OLD_COL1,
    NEW_COL2 ~ OLD_COL2,
    format = "csv",
    silent = TRUE
  )

  # Close connection
  close(con)

  # Test expectations
  expect_s3_class(result, "nif")
  expect_true("NEW_COL1" %in% names(result))
  expect_true("NEW_COL2" %in% names(result))
  expect_equal(result$NEW_COL1[1], "val1")
  expect_equal(result$NEW_COL2[1], "val2")
  expect_equal(result$NEW_COL1[2], "val3")
  expect_equal(result$NEW_COL2[2], "val4")
})


test_that("import_from_connection handles complex renaming expressions", {
  # Create CSV data
  csv_data <- c(
    "USUBJID,STUDYID,TIME,DV,CMT",
    "SUBJ-001,STUDY-001,0,10.5,1",
    "SUBJ-002,STUDY-001,1,15.3,2"
  )

  # Create a connection
  con <- textConnection(csv_data)

  # Import data with complex expression using multiple columns
  result <- import_from_connection(
    con,
    DV_TIMES_CMT ~ DV * CMT,
    DV_PLUS_TIME ~ DV + TIME,
    format = "csv",
    silent = TRUE
  )

  # Close connection
  close(con)

  # Test expectations
  expect_s3_class(result, "nif")
  expect_true("DV_TIMES_CMT" %in% names(result))
  expect_true("DV_PLUS_TIME" %in% names(result))
  expect_equal(result$DV_TIMES_CMT[1], 10.5)
  expect_equal(result$DV_TIMES_CMT[2], 30.6)
  expect_equal(result$DV_PLUS_TIME[1], 10.5)
  expect_equal(result$DV_PLUS_TIME[2], 16.3)
})


test_that("import_from_connection filters out non-formula arguments", {
  # Create CSV data
  csv_data <- c(
    "USUBJID,STUDYID,TIME,DV",
    "SUBJ-001,STUDY-001,0,10.5"
  )

  # Create a connection
  con <- textConnection(csv_data)

  # Import data with both formula and non-formula arguments
  result <- import_from_connection(
    con,
    NEW_COL ~ DV * 2,
    "not a formula",
    123,
    format = "csv",
    silent = TRUE
  )

  # Close connection
  close(con)

  # Test expectations - should only apply the formula, ignore non-formulas
  expect_s3_class(result, "nif")
  expect_true("NEW_COL" %in% names(result))
  expect_equal(result$NEW_COL[1], 21.0)
})


test_that("import_from_connection handles renaming with fixed-width format", {
  # Create fixed-width data
  fixed_data <- c(
    "USUBJID  STUDYID   OLD_COL  TIME  DV",
    "SUBJ-001 STUDY-001 value1   0     10.5",
    "SUBJ-002 STUDY-001 value2   1     15.3"
  )

  # Create a connection
  con <- textConnection(fixed_data)

  # Import data with renaming term
  result <- import_from_connection(
    con,
    NEW_COL ~ OLD_COL,
    format = "fixed_width",
    silent = TRUE
  )

  # Close connection
  close(con)

  # Test expectations
  expect_s3_class(result, "nif")
  expect_true("NEW_COL" %in% names(result))
  expect_true("OLD_COL" %in% names(result))
  expect_equal(result$NEW_COL[1], "value1")
  expect_equal(result$NEW_COL[2], "value2")
})


test_that("import_from_connection handles renaming with character expressions", {
  # Create CSV data
  csv_data <- c(
    "USUBJID,STUDYID,TIME,DV",
    "SUBJ-001,STUDY-001,0,10.5",
    "SUBJ-002,STUDY-001,1,15.3"
  )

  # Create a connection
  con <- textConnection(csv_data)

  # Import data with renaming term that creates character values
  result <- import_from_connection(
    con,
    SUBJECT_LABEL ~ paste0("Subject-", USUBJID),
    format = "csv",
    silent = TRUE
  )

  # Close connection
  close(con)

  # Test expectations
  expect_s3_class(result, "nif")
  expect_true("SUBJECT_LABEL" %in% names(result))
  expect_equal(result$SUBJECT_LABEL[1], "Subject-SUBJ-001")
  expect_equal(result$SUBJECT_LABEL[2], "Subject-SUBJ-002")
})


test_that("import_from_connection handles renaming with conditional expressions", {
  # Create CSV data
  csv_data <- c(
    "USUBJID,STUDYID,TIME,DV",
    "SUBJ-001,STUDY-001,0,10.5",
    "SUBJ-002,STUDY-001,1,15.3"
  )

  # Create a connection
  con <- textConnection(csv_data)

  # Import data with renaming term that includes conditional logic
  result <- import_from_connection(
    con,
    HIGH_DV ~ ifelse(DV > 12, "High", "Low"),
    format = "csv",
    silent = TRUE
  )

  # Close connection
  close(con)

  # Test expectations
  expect_s3_class(result, "nif")
  expect_true("HIGH_DV" %in% names(result))
  expect_equal(result$HIGH_DV[1], "Low")
  expect_equal(result$HIGH_DV[2], "High")
})


test_that("import_from_connection handles renaming that overwrites existing column", {
  # Create CSV data
  csv_data <- c(
    "USUBJID,STUDYID,TIME,DV",
    "SUBJ-001,STUDY-001,0,10.5",
    "SUBJ-002,STUDY-001,1,15.3"
  )

  # Create a connection
  con <- textConnection(csv_data)

  # Import data with renaming term that overwrites DV
  result <- import_from_connection(
    con,
    DV ~ DV * 2,
    format = "csv",
    silent = TRUE
  )

  # Close connection
  close(con)

  # Test expectations - DV should be doubled
  expect_s3_class(result, "nif")
  expect_equal(result$DV[1], 21.0)
  expect_equal(result$DV[2], 30.6)
})
