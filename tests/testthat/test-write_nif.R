test_that("write_nif basic functionality works", {
  # Create a minimal test NIF object
  test_data <- tibble::tribble(
    ~ID, ~TIME, ~AMT, ~CMT, ~EVID, ~DOSE, ~DV,
    1,   0,     100,  1,    1,     100,   NA,
    1,   1,     0,    1,    0,     100,   10.12345,
    2,   0,     100,  1,    1,     100,   NA
  )
  class(test_data) <- c("nif", "data.frame")

  # Test console output (no filename provided)
  # result <- write_nonmem(test_data)
  #
  # expect_contains(names(result), "ID")
  # expect_contains(names(result), "TIME")

  # Test file output with comma separator
  temp_file <- tempfile()
  write_nonmem(test_data, filename = temp_file, sep = ",")

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
  test_data <- tibble::tribble(
    ~ID, ~TIME, ~AMT, ~CMT, ~EVID, ~DOSE, ~DV,
    1,   0,     100,  1,    1,     100,   NA,
    1,   1,     0,    1,    0,     100,   10.12345
  )
  class(test_data) <- c("nif", "data.frame")

  # Test fixed-width output
  temp_file <- tempfile()
  write_nonmem(test_data, filename = temp_file)

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
  # result <- write_nonmem(empty_data)
  # expect_equal(nrow(result), 0)

  # Test file output
  temp_file <- tempfile()
  expect_no_error(write_nonmem(empty_data, filename = temp_file, sep = ","))
  unlink(temp_file)
})


test_that("write_nif preserves column order", {
  test_data <- tibble::tribble(
    ~DV,       ~ID, ~TIME, ~AMT, ~CMT, ~EVID, ~DOSE,
    NA,        1,   0,     100,  1,    1,     100,
    10.12345,  1,   1,     0,    1,    0,     100
  )
  class(test_data) <- c("nif", "data.frame")

  temp_file <- tempfile()
  write_nonmem(test_data, filename = temp_file, sep = ",")

  # Read back and verify column order
  result <- read.csv(temp_file, stringsAsFactors = FALSE)
  expect_equal(colnames(result), colnames(test_data))

  unlink(temp_file)
})

