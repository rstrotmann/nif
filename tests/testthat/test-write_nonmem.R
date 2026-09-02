make_test_nif <- function(...) {
  obj <- tibble::tribble(...)
  obj <- as_nif_test(obj)
  obj
}


test_that("write_nonmem writes CSV with rounded values and NA as dots", {
  test_data <- make_test_nif(
    ~ID, ~TIME, ~AMT, ~CMT, ~EVID, ~DOSE, ~DV,
    1,   0,     100,  1,    1,     100,   NA_real_,
    1,   1,     0,    2,    0,     100,   10.12345,
    2,   0,     100,  1,    1,     100,   NA_real_
  )

  temp_file <- tempfile()
  on.exit(unlink(temp_file), add = TRUE)

  expect_null(write_nonmem(test_data, filename = temp_file, sep = ","))

  result <- read.csv(temp_file, colClasses = "character")
  expect_equal(nrow(result), 3)
  expect_equal(ncol(result), 7)
  expect_equal(colnames(result), colnames(test_data))
  expect_equal(result$DV[1], ".")
  expect_equal(result$DV[2], "10.1234")
  expect_equal(result$AMT[2], "0")
})


test_that("write_nonmem writes semicolon-separated files", {
  test_data <- make_test_nif(
    ~ID, ~TIME, ~AMT, ~CMT, ~EVID, ~DV,
    1,   0,     100,  1,    1,     NA_real_,
    1,   1,     0,    2,    0,     1.5
  )

  temp_file <- tempfile()
  on.exit(unlink(temp_file), add = TRUE)

  write_nonmem(test_data, filename = temp_file, sep = ";")

  lines <- readLines(temp_file)
  expect_equal(lines[1], "ID;TIME;AMT;CMT;EVID;DV")
  expect_equal(lines[2], "1;0;100;1;1;.")
  expect_equal(lines[3], "1;1;0;2;0;1.5")
})


test_that("write_nonmem writes space-separated fixed-width files by default", {
  test_data <- make_test_nif(
    ~ID, ~TIME, ~AMT, ~CMT, ~EVID, ~DV,
    1,   0,     100,  1,    1,     NA_real_,
    1,   1,     0,    2,    0,     10.12345
  )

  temp_file <- tempfile()
  on.exit(unlink(temp_file), add = TRUE)

  write_nonmem(test_data, filename = temp_file)

  lines <- readLines(temp_file)
  expect_equal(length(lines), 3)
  expect_match(lines[1], "^ID\\s+TIME\\s+AMT\\s+CMT\\s+EVID\\s+DV")
  expect_false(grepl(",", lines[1]))
  expect_match(lines[2], "\\.")
  expect_match(lines[3], "10\\.1234")
})


test_that("write_nonmem prints to console when filename is NULL", {
  test_data <- make_test_nif(
    ~ID, ~TIME, ~AMT, ~CMT, ~EVID, ~DV,
    1,   0,     100,  1,    1,     NA_real_,
    1,   1,     0,    2,    0,     2.5
  )

  csv_out <- utils::capture.output(write_nonmem(test_data, sep = ","))
  expect_equal(csv_out[1], "ID,TIME,AMT,CMT,EVID,DV")
  expect_equal(csv_out[2], "1,0,100,1,1,.")
  expect_equal(csv_out[3], "1,1,0,2,0,2.5")

  fwf_out <- utils::capture.output(write_nonmem(test_data))
  expect_match(fwf_out[1], "^ID\\s+TIME\\s+AMT\\s+CMT\\s+EVID\\s+DV")
  expect_equal(length(fwf_out), 3)
})


test_that("write_nonmem replaces zeros with dots in dot_columns", {
  test_data <- make_test_nif(
    ~ID, ~TIME, ~AMT, ~CMT, ~EVID, ~DV,
    1,   0,     100,  1,    1,     NA_real_,
    1,   1,     0,    2,    0,     0,
    1,   2,     0,    2,    0,     5
  )

  temp_file <- tempfile()
  on.exit(unlink(temp_file), add = TRUE)

  write_nonmem(
    test_data,
    filename = temp_file,
    sep = ",",
    dot_columns = c("DV", "AMT")
  )

  result <- read.csv(temp_file, colClasses = "character")
  expect_equal(result$AMT, c("100", ".", "."))
  expect_equal(result$DV, c(".", ".", "5"))
  expect_equal(result$TIME[1], "0")
})


test_that("write_nonmem keeps zeros outside dot_columns", {
  test_data <- make_test_nif(
    ~ID, ~TIME, ~AMT, ~CMT, ~EVID, ~DV,
    1,   0,     0,    1,    1,     0
  )

  temp_file <- tempfile()
  on.exit(unlink(temp_file), add = TRUE)

  write_nonmem(
    test_data,
    filename = temp_file,
    sep = ",",
    dot_columns = "DV"
  )

  result <- read.csv(temp_file, colClasses = "character")
  expect_equal(result$AMT, "0")
  expect_equal(result$DV, ".")
  expect_equal(result$TIME, "0")
})


test_that("write_nonmem exports only numeric fields when requested", {
  test_data <- make_test_nif(
    ~ID, ~TIME, ~AMT, ~CMT, ~EVID, ~DV,      ~SEX, ~ACTARMCD,
    1,   0,     100,  1,    1,     NA_real_, "M",  "TRT",
    1,   1,     0,    2,    0,     1.25,     "M",  "TRT"
  )

  temp_file <- tempfile()
  on.exit(unlink(temp_file), add = TRUE)

  write_nonmem(
    test_data,
    filename = temp_file,
    sep = ",",
    numeric_fields_only = TRUE
  )

  result <- read.csv(temp_file, colClasses = "character")
  expect_equal(
    colnames(result),
    c("ID", "TIME", "AMT", "CMT", "EVID", "DV")
  )
  expect_false("SEX" %in% colnames(result))
  expect_false("ACTARMCD" %in% colnames(result))
})


test_that("write_nonmem includes minimal fields plus requested fields", {
  test_data <- make_test_nif(
    ~ID, ~TIME, ~AMT, ~CMT, ~EVID, ~DV,      ~DOSE, ~SEX, ~RACE,
    1,   0,     100,  1,    1,     NA_real_, 100,   "M",  "WHITE",
    1,   1,     0,    2,    0,     3.2,      100,   "M",  "WHITE"
  )

  temp_file <- tempfile()
  on.exit(unlink(temp_file), add = TRUE)

  write_nonmem(
    test_data,
    filename = temp_file,
    sep = ",",
    fields = c("SEX", "DOSE")
  )

  result <- read.csv(temp_file, colClasses = "character")
  expect_equal(
    colnames(result),
    c("ID", "TIME", "AMT", "CMT", "EVID", "DV", "SEX", "DOSE")
  )
  expect_false("RACE" %in% colnames(result))
})


test_that("write_nonmem fields selection tolerates overlap with minimal fields", {
  test_data <- make_test_nif(
    ~ID, ~TIME, ~AMT, ~CMT, ~EVID, ~DV,      ~SEX,
    1,   0,     100,  1,    1,     NA_real_, "F"
  )

  temp_file <- tempfile()
  on.exit(unlink(temp_file), add = TRUE)

  write_nonmem(
    test_data,
    filename = temp_file,
    sep = ",",
    fields = c("ID", "SEX")
  )

  result <- read.csv(temp_file, colClasses = "character")
  expect_equal(
    colnames(result),
    c("ID", "TIME", "AMT", "CMT", "EVID", "DV", "SEX")
  )
})


test_that("write_nonmem applies fields before numeric_fields_only", {
  test_data <- make_test_nif(
    ~ID, ~TIME, ~AMT, ~CMT, ~EVID, ~DV,      ~DOSE, ~SEX,
    1,   0,     100,  1,    1,     NA_real_, 50,    "M"
  )

  temp_file <- tempfile()
  on.exit(unlink(temp_file), add = TRUE)

  write_nonmem(
    test_data,
    filename = temp_file,
    sep = ",",
    fields = c("DOSE", "SEX"),
    numeric_fields_only = TRUE
  )

  result <- read.csv(temp_file, colClasses = "character")
  expect_equal(
    colnames(result),
    c("ID", "TIME", "AMT", "CMT", "EVID", "DV", "DOSE")
  )
  expect_false("SEX" %in% colnames(result))
})


test_that("write_nonmem preserves column order", {
  test_data <- make_test_nif(
    ~DV,      ~ID, ~TIME, ~AMT, ~CMT, ~EVID, ~DOSE,
    NA_real_, 1,   0,     100,  1,    1,     100,
    10.12345, 1,   1,     0,    2,    0,     100
  )

  temp_file <- tempfile()
  on.exit(unlink(temp_file), add = TRUE)

  write_nonmem(test_data, filename = temp_file, sep = ",")

  result <- read.csv(temp_file, colClasses = "character")
  expect_equal(colnames(result), colnames(test_data))
})


test_that("write_nonmem handles empty nif objects", {
  empty_data <- data.frame(
    ID   = numeric(0),
    TIME = numeric(0),
    AMT  = numeric(0),
    CMT  = numeric(0),
    EVID = numeric(0),
    DV   = numeric(0)
  )
  empty_data <- as_nif_test(empty_data)
  temp_file <- tempfile()
  on.exit(unlink(temp_file), add = TRUE)

  expect_no_error(write_nonmem(empty_data, filename = temp_file, sep = ","))

  lines <- readLines(temp_file)
  expect_equal(lines[1], "ID,TIME,AMT,CMT,EVID,DV")
  expect_equal(length(lines), 1)
})


test_that("write_nonmem does not quote character values", {
  test_data <- make_test_nif(
    ~ID, ~TIME, ~AMT, ~CMT, ~EVID, ~DV,      ~SEX,
    1,   0,     100,  1,    1,     NA_real_, "M"
  )

  temp_file <- tempfile()
  on.exit(unlink(temp_file), add = TRUE)

  write_nonmem(test_data, filename = temp_file, sep = ",")

  lines <- readLines(temp_file)
  expect_equal(lines[2], "1,0,100,1,1,.,M")
  expect_false(grepl("\"", lines[2]))
})


test_that("write_nonmem rejects invalid inputs", {
  test_data <- make_test_nif(
    ~ID, ~TIME, ~AMT, ~CMT, ~EVID, ~DV,
    1,   0,     100,  1,    1,     NA_real_
  )

  expect_error(
    write_nonmem(as.data.frame(test_data)),
    "Input must be a nif object"
  )
  expect_error(
    write_nonmem(test_data, fields = "NOPE"),
    "Missing required fields: NOPE"
  )
  expect_error(
    write_nonmem(test_data, dot_columns = "NOPE"),
    "Missing required fields: NOPE"
  )
  expect_error(
    write_nonmem(test_data, filename = 1),
    "filename must be a character value"
  )
  expect_error(
    write_nonmem(test_data, sep = 1),
    "sep must be a character value"
  )
  expect_error(
    write_nonmem(test_data, sep = ""),
    "sep must be a non-empty string"
  )
  expect_error(
    write_nonmem(test_data, fields = ""),
    "fields must be a non-empty string"
  )
  expect_error(
    write_nonmem(test_data, numeric_fields_only = "yes"),
    "numeric_fields_only must be a logical value"
  )
  expect_error(
    write_nonmem(test_data, dot_columns = ""),
    "dot_columns must be a non-empty string"
  )
})


test_that("write_nonmem rejects nif objects missing essential fields", {
  incomplete <- tibble::tribble(
    ~ID, ~TIME,
    1,   0
  )
  incomplete <- as_nif_test(incomplete)
  expect_error(
    write_nonmem(incomplete),
    "Missing essential fields in nif object"
  )
})


test_that("write_nonmem works with examplinib example data", {
  expect_no_error(
    invisible(utils::capture.output(write_nonmem(examplinib_sad_nif)))
  )

  temp_file <- tempfile()
  on.exit(unlink(temp_file), add = TRUE)

  expect_no_error(
    write_nonmem(
      examplinib_sad_nif,
      filename = temp_file,
      sep = ",",
      numeric_fields_only = TRUE,
      dot_columns = c("DV", "AMT")
    )
  )

  result <- read.csv(temp_file, colClasses = "character")
  expect_true(nrow(result) > 0)
  expect_true(all(vapply(result, is.character, logical(1))))
  expect_true(all(c("ID", "TIME", "AMT", "CMT", "EVID", "DV") %in% names(result)))
  expect_true(any(result$DV == "." | result$AMT == "."))
})
