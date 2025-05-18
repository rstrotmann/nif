test_that("import_nif loads CSV files correctly", {
  csv_file <- tempfile(fileext = ".csv")
  on.exit(unlink(csv_file))

  csv_content <- "USUBJID,TIME,EVID,DV,ANALYTE\n1001,0,0,10.5,DRUG\n1001,1,0,8.2,DRUG\n1001,2,0,6.7,DRUG"
  writeLines(csv_content, csv_file)

  result <- import_nif(csv_file, format = "csv", silent = TRUE)

  expect_s3_class(result, "nif")
  expect_equal(nrow(result), 3)
  expect_equal(result$USUBJID, c("1001", "1001", "1001"))
  expect_equal(result$TIME, c(0, 1, 2))
  expect_equal(result$DV, c(10.5, 8.2, 6.7))
  expect_equal(result$ANALYTE, c("DRUG", "DRUG", "DRUG"))
})


test_that("import_nif loads fixed-width files correctly", {
  fw_file <- tempfile(fileext = ".txt")
  on.exit(unlink(fw_file))

  fw_content <- "USUBJID TIME EVID DV     ANALYTE\n1001    0    0    10.5   DRUG\n1001    1    0    8.2    DRUG\n1001    2    0    6.7    DRUG"
  writeLines(fw_content, fw_file)

  result <- import_nif(fw_file, format = "fixed_width", silent = TRUE)

  expect_s3_class(result, "nif")
  expect_equal(nrow(result), 3)
  expect_equal(result$USUBJID, c("1001", "1001", "1001"))
  expect_equal(result$TIME, c(0, 1, 2))
  expect_equal(result$DV, c(10.5, 8.2, 6.7))
  expect_equal(result$ANALYTE, c("DRUG", "DRUG", "DRUG"))
})


test_that("import_nif automatically detects CSV format", {
  csv_file <- tempfile(fileext = ".csv")
  on.exit(unlink(csv_file))

  csv_content <- "USUBJID,TIME,EVID,DV,ANALYTE\n1001,0,0,10.5,DRUG"
  writeLines(csv_content, csv_file)

  result <- import_nif(csv_file, silent = TRUE)

  expect_s3_class(result, "nif")
  expect_equal(nrow(result), 1)
})


test_that("import_nif automatically detects fixed-width format", {
  fw_file <- tempfile(fileext = ".txt")
  on.exit(unlink(fw_file))

  fw_content <- "USUBJID TIME EVID DV     ANALYTE\n1001    0    0    10.5   DRUG"
  writeLines(fw_content, fw_file)

  result <- import_nif(fw_file, silent = TRUE)

  expect_s3_class(result, "nif")
  expect_equal(nrow(result), 1)
})


test_that("import_nif handles custom delimiter", {
  csv_file <- tempfile(fileext = ".csv")
  on.exit(unlink(csv_file))

  csv_content <- "USUBJID;TIME;EVID;DV;ANALYTE\n1001;0;0;10.5;DRUG"
  writeLines(csv_content, csv_file)

  result <- import_nif(csv_file, delimiter = ";", silent = TRUE)

  expect_s3_class(result, "nif")
  expect_equal(nrow(result), 1)
})


test_that("import_nif handles no_numeric parameter correctly", {
  csv_file <- tempfile(fileext = ".csv")
  on.exit(unlink(csv_file))

  csv_content <- "USUBJID,TIME,EVID,DV,ANALYTE,ID\n1001,0,0,10.5,DRUG,123"
  writeLines(csv_content, csv_file)

  # Without including ID in no_numeric, it should be numeric
  result1 <- import_nif(csv_file, silent = TRUE)
  expect_type(result1$ID, "integer")

  # With ID in no_numeric, it should be character
  result2 <- import_nif(csv_file, no_numeric = c("USUBJID", "STUDYID", "ID"), silent = TRUE)
  expect_type(result2$ID, "character")
})


test_that("import_nif handles date/time conversion", {
  csv_file <- tempfile(fileext = ".csv")
  on.exit(unlink(csv_file))

  csv_content <- "USUBJID,TIME,EVID,DV,DATETIME\n1001,0,0,10.5,2023-01-01T12:00:00"
  writeLines(csv_content, csv_file)

  result <- import_nif(csv_file, silent = TRUE)

  expect_s3_class(result, "nif")
  expect_s3_class(result$DATETIME, "POSIXct")
})


test_that("import_nif handles comments and empty lines", {
  csv_file <- tempfile(fileext = ".csv")
  on.exit(unlink(csv_file))

  csv_content <- "# This is a comment\nUSUBJID,TIME,EVID,DV\n\n# Another comment\n1001,0,0,10.5\n\n1001,1,0,8.2"
  writeLines(csv_content, csv_file)

  result <- import_nif(csv_file, silent = TRUE)

  expect_s3_class(result, "nif")
  expect_equal(nrow(result), 2)
})


test_that("import_nif errors on file not found", {
  expect_error(
    import_nif("nonexistent-file.csv"),
    "File 'nonexistent-file.csv' not found"
  )
})



test_that("import_nif renames columns correctly", {
  csv_file <- tempfile(fileext = ".csv")
  on.exit(unlink(csv_file))

  csv_content <- "USUBJID,TIME,EVID,DV,ANALYTE\n0011001,0,0,10.5,DRUG\n0011001,1,0,8.2,DRUG\n0011001,2,0,6.7,DRUG"
  writeLines(csv_content, csv_file)

  result <- import_nif(
    csv_file,
    X ~ str_sub(USUBJID, -4, -1),
    Y ~ ANALYTE,
    Z ~ as.numeric(USUBJID),
    format = "csv",
    silent = TRUE)

  expect_s3_class(result, "nif")
  expect_equal(nrow(result), 3)
  expect_equal(result$X, c("1001", "1001", "1001"))
  expect_equal(result$Y, c("DRUG", "DRUG", "DRUG"))
  expect_equal(is.numeric(result$Z), TRUE)
})



