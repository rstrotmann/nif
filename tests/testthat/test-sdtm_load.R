
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


test_that("pin_write.sdtm validates inputs correctly", {
  # Create a minimal sdtm object for testing
  test_sdtm <- new_sdtm(
    sdtm_data = list(
      dm = tribble(
        ~USUBJID, ~STUDYID, ~SEX,
        "001",    "TEST01", "M",
        "002",    "TEST01", "F"
      )
    )
  )

  # Test invalid sdtm object
  expect_error(
    pin_write.sdtm("not_sdtm"),
    "Input must be a sdtm object"
  )

  # Test invalid board parameter
  expect_error(
    pin_write.sdtm(test_sdtm, board = 123),
    "board must be a string value"
  )

  # Test invalid name parameter
  expect_error(
    pin_write.sdtm(test_sdtm, name = 456),
    "name must be a string value"
  )

  # Test invalid title parameter
  expect_error(
    pin_write.sdtm(test_sdtm, title = 789),
    "title must be a string value"
  )
})


test_that("pin_write.sdtm handles missing board correctly", {
  # Create a minimal sdtm object for testing
  test_sdtm <- new_sdtm(
    sdtm_data = list(
      dm = tribble(
        ~USUBJID, ~STUDYID, ~SEX,
        "001",    "TEST01", "M",
        "002",    "TEST01", "F"
      )
    )
  )

  # Test with NULL board and no nif option set
  expect_error(
    pin_write.sdtm(test_sdtm, board = NULL),
    "No pin board provided"
  )

  # Test with NA board
  expect_error(
    pin_write.sdtm(test_sdtm, board = NA_character_),
    "board must not contain NA"
  )
})


test_that("pin_write.sdtm uses arbitrary board", {
  # Create a minimal sdtm object for testing
  test_sdtm <- new_sdtm(
    sdtm_data = list(
      dm = tribble(
        ~USUBJID, ~STUDYID, ~SEX,
        "001",    "TEST01", "M",
        "002",    "TEST01", "F"
      )
    )
  )

  # Test with arbitrary directory
  test_board <- tempfile("test_board")
  expect_no_error(
    pin_write.sdtm(test_sdtm, board = test_board, silent = TRUE)
  )
})


test_that("pin_write.sdtm generates correct default names and titles", {
  # Create a temporary board directory
  test_board <- tempfile("test_board")
  dir.create(test_board)
  on.exit(unlink(test_board, recursive = TRUE))

  # Create a minimal sdtm object for testing
  test_sdtm <- new_sdtm(
    sdtm_data = list(
      dm = tribble(
        ~USUBJID, ~STUDYID, ~SEX,
        "001",    "TEST01", "M",
        "002",    "TEST01", "F"
      )
    )
  )

  # Test with NULL name and title (should use defaults)
  expect_no_error(
    pin_write.sdtm(test_sdtm, board = test_board, name = NULL, title = NULL,
                   silent = TRUE)
  )

  # Check that the pin was created with expected name
  board_obj <- pins::board_folder(test_board)
  pins_list <- pins::pin_list(board_obj)
  expect_true("TEST01_sdtm" %in% pins_list)
})


test_that("pin_write.sdtm uses custom name and title when provided", {
  # Create a temporary board directory
  test_board <- tempfile("test_board")
  dir.create(test_board)
  on.exit(unlink(test_board, recursive = TRUE))

  # Create a minimal sdtm object for testing
  test_sdtm <- new_sdtm(
    sdtm_data = list(
      dm = tribble(
        ~USUBJID, ~STUDYID, ~SEX,
        "001",    "TEST01", "M",
        "002",    "TEST01", "F"
      )
    )
  )

  # Test with custom name and title
  custom_name <- "my_custom_sdtm"
  custom_title <- "My Custom SDTM Data"

  expect_no_error(
    pin_write.sdtm(test_sdtm, board = test_board,
                   name = custom_name, title = custom_title, silent = TRUE)
  )

  # Check that the pin was created with custom name
  board_obj <- pins::board_folder(test_board)
  pins_list <- pins::pin_list(board_obj)
  expect_true(custom_name %in% pins_list)
})


test_that("pin_write.sdtm handles silent parameter correctly", {
  # Create a temporary board directory
  test_board <- tempfile("test_board")
  dir.create(test_board)
  on.exit(unlink(test_board, recursive = TRUE))

  # Create a minimal sdtm object for testing
  test_sdtm <- new_sdtm(
    sdtm_data = list(
      dm = tribble(
        ~USUBJID, ~STUDYID, ~SEX,
        "001",    "TEST01", "M",
        "002",    "TEST01", "F"
      )
    )
  )

  # Test with silent = TRUE (should not produce messages)
  expect_no_error(
    pin_write.sdtm(test_sdtm, board = test_board, silent = TRUE)
  )

  # Test with silent = FALSE (should produce messages)
  expect_message(expect_message(
    expect_no_error(
    pin_write.sdtm(test_sdtm, board = test_board, silent = FALSE)
  )))
})


test_that("pin_write.sdtm works with complex sdtm objects", {
  # Create a temporary board directory
  test_board <- tempfile("test_board")
  dir.create(test_board)
  on.exit(unlink(test_board, recursive = TRUE))

  # Create a more complex sdtm object with multiple domains
  complex_sdtm <- new_sdtm(
    sdtm_data = list(
      dm = tribble(
        ~USUBJID, ~STUDYID, ~SEX, ~AGE,
        "001",    "TEST01", "M",  25,
        "002",    "TEST01", "F",  30
      ),
      ex = tribble(
        ~USUBJID, ~EXTRT, ~EXDOSE, ~EXSTDTC,
        "001",    "DRUG", 100,     "2023-01-01",
        "002",    "DRUG", 200,     "2023-01-02"
      ),
      pc = tribble(
        ~USUBJID, ~PCTEST, ~PCSTRESN, ~PCDTC,
        "001",    "CONC",  10.5,     "2023-01-01T08:00",
        "002",    "CONC",  15.2,     "2023-01-02T09:00"
      )
    ),
    analyte_mapping = tribble(
      ~PCTEST, ~ANALYTE,
      "CONC",  "Parent"
    ),
    metabolite_mapping = tribble(
      ~PCTEST, ~METABOLITE,
      "CONC",  "M1"
    ),
    parent_mapping = tribble(
      ~PCTEST, ~PARENT,
      "CONC",  "Parent"
    ),
    time_mapping = tribble(
      ~PCTPT, ~TIME,
      "PRE",   0,
      "1H",   1
    )
  )

  # Test writing complex sdtm object
  expect_no_error(
    pin_write.sdtm(complex_sdtm, board = test_board, silent = TRUE,
                   name = "complex_test", title = "Complex SDTM Test")
  )

  # Verify the pin was created and can be read back
  board_obj <- pins::board_folder(test_board)
  pins_list <- pins::pin_list(board_obj)
  expect_true("complex_test" %in% pins_list)
})


test_that("pin_write.sdtm preserves sdtm object structure", {
  # Create a temporary board directory
  test_board <- tempfile("test_board")
  dir.create(test_board)
  on.exit(unlink(test_board, recursive = TRUE))

  # Create a test sdtm object
  original_sdtm <- new_sdtm(
    sdtm_data = list(
      dm = tribble(
        ~USUBJID, ~STUDYID, ~SEX,
        "001",    "TEST01", "M",
        "002",    "TEST01", "F"
      )
    ),
    analyte_mapping = tribble(
      ~PCTEST, ~ANALYTE,
      "CONC",  "Parent"
    )
  )

  # Write the sdtm object
  pin_write.sdtm(original_sdtm, board = test_board, name = "test_preserve",
                 silent = TRUE)

  # Read it back and verify structure is preserved
  board_obj <- pins::board_folder(test_board)
  read_back_sdtm <- pins::pin_read(board_obj, "test_preserve")

  # Check class
  expect_s3_class(read_back_sdtm, "sdtm")

  # Check domains
  expect_equal(names(read_back_sdtm$domains), names(original_sdtm$domains))
  expect_equal(read_back_sdtm$domains$dm, original_sdtm$domains$dm)

  # Check mappings
  expect_equal(read_back_sdtm$analyte_mapping, original_sdtm$analyte_mapping)
  expect_equal(read_back_sdtm$metabolite_mapping, original_sdtm$metabolite_mapping)
  expect_equal(read_back_sdtm$parent_mapping, original_sdtm$parent_mapping)
  expect_equal(read_back_sdtm$time_mapping, original_sdtm$time_mapping)
})

