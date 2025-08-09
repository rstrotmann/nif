
test_that("read_sdtm validates inputs correctly", {
  # Test invalid data_path
  expect_error(
    read_sdtm("nonexistent/path"),
    "data_path does not exist"
  )

  # Test invalid format
  expect_error(
    read_sdtm(tempdir(), format = "invalid"),
    "format must be one of: sas, xpt, csv, xlsx"
  )

  # Test invalid domain
  expect_error(
    read_sdtm(tempdir(), domain = character(0)),
    "no domain data found"
  )
  expect_error(
    read_sdtm(tempdir(), domain = 123),
    "domain must be a string value"
  )
})


test_that("read_sdtm handles missing files correctly", {
  # Create a temporary directory for testing
  test_dir <- tempfile("sdtm_test")
  dir.create(test_dir)
  on.exit(unlink(test_dir, recursive = TRUE))

  # Test missing files
  expect_error(
    read_sdtm(test_dir),
    "no domain data found"
  )
})


test_that("read_sdtm reads different formats correctly", {
  # Create a temporary directory for testing
  test_dir <- tempfile("sdtm_test")
  dir.create(test_dir)
  on.exit(unlink(test_dir, recursive = TRUE))

  # Create test data
  test_data <- data.frame(
    USUBJID = c("001", "002"),
    SEX = c("M", "F")
  )

  # Test SAS format
  suppressWarnings(
    haven::write_sas(test_data, file.path(test_dir, "dm.sas7bdat"))
  )
  result_sas <- read_sdtm(test_dir, domain = "dm", format = "sas")
  expect_s3_class(result_sas, "sdtm")
  expect_equal(nrow(result_sas$domains$dm), 2)

  # Test XPT format
  haven::write_xpt(test_data, file.path(test_dir, "dm.xpt"))
  result_xpt <- read_sdtm(test_dir, domain = "dm", format = "xpt")
  expect_s3_class(result_xpt, "sdtm")
  expect_equal(nrow(result_xpt$domains$dm), 2)

  # Test CSV format
  write.csv(test_data, file.path(test_dir, "dm.csv"), row.names = FALSE)
  result_csv <- read_sdtm(test_dir, domain = "dm", format = "csv")
  expect_s3_class(result_csv, "sdtm")
  expect_equal(nrow(result_csv$domains$dm), 2)

  # Test XLSX format
  writexl::write_xlsx(test_data, file.path(test_dir, "dm.xlsx"))
  result_xlsx <- read_sdtm(test_dir, domain = "dm", format = "xlsx")
  expect_s3_class(result_xlsx, "sdtm")
  expect_equal(nrow(result_xlsx$domains$dm), 2)
})


test_that("read_sdtm handles multiple domains correctly", {
  # Create a temporary directory for testing
  test_dir <- tempfile("sdtm_test")
  dir.create(test_dir)
  on.exit(unlink(test_dir, recursive = TRUE))

  # Create test data for multiple domains
  dm_data <- data.frame(USUBJID = c("001", "002"), SEX = c("M", "F"))
  vs_data <- data.frame(USUBJID = c("001", "002"), VSTEST = c("BP", "HR"))

  # Write test files
  suppressWarnings({
    haven::write_sas(dm_data, file.path(test_dir, "dm.sas7bdat"))
    haven::write_sas(vs_data, file.path(test_dir, "vs.sas7bdat"))
  })

  # Test reading multiple domains
  result <- read_sdtm(test_dir, domain = c("dm", "vs"))
  expect_s3_class(result, "sdtm")
  expect_equal(names(result$domains), c("dm", "vs"))
  expect_equal(nrow(result$domains$dm), 2)
  expect_equal(nrow(result$domains$vs), 2)
})


test_that("read_sdtm handles custom delimiters for CSV", {
  # Create a temporary directory for testing
  test_dir <- tempfile("sdtm_test")
  dir.create(test_dir)
  on.exit(unlink(test_dir, recursive = TRUE))

  # Create test data
  test_data <- data.frame(
    USUBJID = c("001", "002"),
    SEX = c("M", "F")
  )

  # Write CSV with semicolon delimiter
  write.csv2(test_data, file.path(test_dir, "dm.csv"), row.names = FALSE)

  # Test reading with semicolon delimiter
  result <- read_sdtm(test_dir, domain = "dm", format = "csv", delim = ";")
  expect_s3_class(result, "sdtm")
  expect_equal(nrow(result$domains$dm), 2)
})


test_that("read_sdtm passes additional parameters to read functions", {
  # Create a temporary directory for testing
  test_dir <- tempfile("sdtm_test")
  dir.create(test_dir)
  on.exit(unlink(test_dir, recursive = TRUE))

  # Create test data
  test_data <- data.frame(
    USUBJID = c("001", "002"),
    SEX = c("M", "F")
  )

  # Write CSV with specific encoding
  write.csv(test_data, file.path(test_dir, "dm.csv"), row.names = FALSE)

  # Test reading with specific encoding
  result <- read_sdtm(test_dir, domain = "dm", format = "csv",
                      locale = readr::locale(encoding = "UTF-8"))
  expect_s3_class(result, "sdtm")
  expect_equal(nrow(result$domains$dm), 2)
})




