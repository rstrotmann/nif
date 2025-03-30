test_that("write_nif basic functionality works", {
  # Create a minimal test NIF object
  test_data <- data.frame(
    ID = c(1, 1, 2),
    TIME = c(0, 1, 0),
    AMT = c(100, 0, 100),
    CMT = c(1, 1, 1),
    EVID = c(1, 0, 1),
    DOSE = c(100, 100, 100),
    DV = c(NA, 10.12345, NA)
  )
  class(test_data) <- c("nif", "data.frame")

  # Test console output (no filename provided)
  expect_output(write_nif(test_data), "ID")
  expect_output(write_nif(test_data), "TIME")

  # Test file output with comma separator
  temp_file <- tempfile()
  write_nif(test_data, filename = temp_file, sep = ",")

  # Read back and verify
  result <- read.csv(temp_file, stringsAsFactors = FALSE)
  expect_equal(nrow(result), 3)
  expect_equal(ncol(result), 7)
  expect_equal(result$DV[2], "10.1234") # Should be rounded to 4 significant digits
  expect_equal(result$DV[1], ".") # NA should be converted to "."

  # Clean up
  unlink(temp_file)
})


test_that("write_nif handles fixed-width format", {
  test_data <- data.frame(
    ID = c(1, 1),
    TIME = c(0, 1),
    AMT = c(100, 0),
    CMT = c(1, 1),
    EVID = c(1, 0),
    DOSE = c(100, 100),
    DV = c(NA, 10.12345)
  )
  class(test_data) <- c("nif", "data.frame")

  # Test fixed-width output
  temp_file <- tempfile()
  write_nif(test_data, filename = temp_file)

  # Read back the first line to verify it's fixed-width
  lines <- readLines(temp_file)
  expect_true(length(lines) > 0)
  expect_true(grepl("^ID", lines[1])) # Should start with column names

  # Clean up
  unlink(temp_file)
})


test_that("write_nif handles empty dataframe", {
  empty_data <- data.frame(
    ID = numeric(0),
    TIME = numeric(0),
    AMT = numeric(0),
    CMT = numeric(0),
    EVID = numeric(0),
    DOSE = numeric(0),
    DV = numeric(0)
  )
  class(empty_data) <- c("nif", "data.frame")

  # Should not error with empty dataframe
  expect_output(write_nif(empty_data))

  # Test file output
  temp_file <- tempfile()
  expect_no_error(write_nif(empty_data, filename = temp_file, sep = ","))
  unlink(temp_file)
})


test_that("write_nif preserves column order", {
  test_data <- data.frame(
    DV = c(NA, 10.12345),
    ID = c(1, 1),
    TIME = c(0, 1),
    AMT = c(100, 0),
    CMT = c(1, 1),
    EVID = c(1, 0),
    DOSE = c(100, 100)
  )
  class(test_data) <- c("nif", "data.frame")

  temp_file <- tempfile()
  write_nif(test_data, filename = temp_file, sep = ",")

  # Read back and verify column order
  result <- read.csv(temp_file, stringsAsFactors = FALSE)
  expect_equal(colnames(result), colnames(test_data))

  unlink(temp_file)
})
