test_that("pb_write.sdtm validates inputs correctly", {
  # Create a minimal sdtm object for testing
  test_sdtm <- sdtm(
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
    pb_write.sdtm("not_sdtm"),
    "Input must be a sdtm object"
  )

  # Test invalid board parameter
  expect_error(
    pb_write.sdtm(test_sdtm, board = 123),
    "board must be a string value"
  )

  # Test invalid name parameter
  expect_error(
    pb_write.sdtm(test_sdtm, name = 456),
    "name must be a string value"
  )

  # Test invalid title parameter
  expect_error(
    pb_write.sdtm(test_sdtm, title = 789),
    "title must be a string value"
  )
})


test_that("pb_write.sdtm handles missing board correctly", {
  # Create a minimal sdtm object for testing
  test_sdtm <- sdtm(
    sdtm_data = list(
      dm = tribble(
        ~USUBJID, ~STUDYID, ~SEX,
        "001",    "TEST01", "M",
        "002",    "TEST01", "F"
      )
    )
  )

  withr::local_envvar("NIF_PINBOARD" = "")

  # Test with NULL board and no nif option set
  expect_error(
    pb_write.sdtm(test_sdtm, board = NULL),
    "No pinboard found"
  )

  # Test with NA board
  expect_error(
    pb_write.sdtm(test_sdtm, board = NA_character_),
    "board must not contain NA"
  )
})


test_that("pb_write.sdtm uses arbitrary board", {
  # Create a minimal sdtm object for testing
  test_sdtm <- sdtm(
    sdtm_data = list(
      dm = tribble(
        ~USUBJID, ~STUDYID, ~SEX,
        "001",    "TEST01", "M",
        "002",    "TEST01", "F"
      )
    )
  )

  # Test with arbitrary directory
  test_board <- tempdir(check = TRUE)
  expect_no_error(
    pb_write.sdtm(test_sdtm, board = test_board, silent = TRUE)
  )
})


test_that("pb_write.sdtm generates correct default names and titles", {
  # Create a temporary board directory
  test_board <- tempfile("test_board")
  dir.create(test_board)
  on.exit(unlink(test_board, recursive = TRUE))

  # Create a minimal sdtm object for testing
  test_sdtm <- sdtm(
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
    pb_write.sdtm(test_sdtm,
      board = test_board, name = NULL, title = NULL,
      silent = TRUE
    )
  )

  # Check that the pin was created with expected name
  board_obj <- pins::board_folder(test_board)
  pins_list <- pins::pin_list(board_obj)
  expect_true("TEST01_sdtm" %in% pins_list)
})


test_that("pb_write.sdtm uses custom name and title when provided", {
  # Create a temporary board directory
  test_board <- tempfile("test_board")
  dir.create(test_board)
  on.exit(unlink(test_board, recursive = TRUE))

  # Create a minimal sdtm object for testing
  test_sdtm <- sdtm(
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
    pb_write.sdtm(test_sdtm,
      board = test_board,
      name = custom_name, title = custom_title, silent = TRUE
    )
  )

  # Check that the pin was created with custom name
  board_obj <- pins::board_folder(test_board)
  pins_list <- pins::pin_list(board_obj)
  expect_true(custom_name %in% pins_list)
})


test_that("pb_write.sdtm handles silent parameter correctly", {
  # Create a temporary board directory
  test_board <- tempfile("test_board")
  dir.create(test_board)
  on.exit(unlink(test_board, recursive = TRUE))

  # Create a minimal sdtm object for testing
  test_sdtm <- sdtm(
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
    pb_write.sdtm(test_sdtm, board = test_board, silent = TRUE)
  )

  # Test with silent = FALSE (should produce messages)
  expect_message(expect_message(
    expect_no_error(
      pb_write.sdtm(test_sdtm, board = test_board, silent = FALSE)
    )
  ))
})


test_that("pb_write.sdtm works with complex sdtm objects", {
  # Create a temporary board directory
  test_board <- tempfile("test_board")
  dir.create(test_board)
  on.exit(unlink(test_board, recursive = TRUE))

  # Create a more complex sdtm object with multiple domains
  complex_sdtm <- sdtm(
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
        "001", "CONC", 10.5, "2023-01-01T08:00",
        "002", "CONC", 15.2, "2023-01-02T09:00"
      )
    )
    # analyte_mapping = tribble(
    #   ~PCTEST, ~ANALYTE,
    #   "CONC",  "Parent"
    # ),
    # metabolite_mapping = tribble(
    #   ~PCTEST, ~METABOLITE,
    #   "CONC",  "M1"
    # ),
    # parent_mapping = tribble(
    #   ~PCTEST, ~PARENT,
    #   "CONC",  "Parent"
    # ),
    # time_mapping = tribble(
    #   ~PCTPT, ~TIME,
    #   "PRE",   0,
    #   "1H",   1
    # )
  )

  # Test writing complex sdtm object
  expect_no_error(
    pb_write.sdtm(complex_sdtm,
      board = test_board, silent = TRUE,
      name = "complex_test", title = "Complex SDTM Test"
    )
  )

  # Verify the pin was created and can be read back
  board_obj <- pins::board_folder(test_board)
  pins_list <- pins::pin_list(board_obj)
  expect_true("complex_test" %in% pins_list)
})


test_that("pb_write.sdtm preserves sdtm object structure", {
  # Create a temporary board directory
  test_board <- tempfile("test_board")
  dir.create(test_board)
  on.exit(unlink(test_board, recursive = TRUE))

  # Create a test sdtm object
  original_sdtm <- sdtm(
    sdtm_data = list(
      dm = tribble(
        ~USUBJID, ~STUDYID, ~SEX,
        "001",    "TEST01", "M",
        "002",    "TEST01", "F"
      )
    )
    # analyte_mapping = tribble(
    #   ~PCTEST, ~ANALYTE,
    #   "CONC",  "Parent"
    # )
  )

  # Write the sdtm object
  pb_write.sdtm(original_sdtm,
    board = test_board, name = "test_preserve",
    silent = TRUE
  )

  # Read it back and verify structure is preserved
  board_obj <- pins::board_folder(test_board)
  read_back_sdtm <- pins::pin_read(board_obj, "test_preserve")

  # Check class
  expect_s3_class(read_back_sdtm, "sdtm")

  # Check domains
  expect_equal(names(read_back_sdtm$domains), names(original_sdtm$domains))
  expect_equal(read_back_sdtm$domains$dm, original_sdtm$domains$dm)

  # Check mappings
  # expect_equal(read_back_sdtm$analyte_mapping, original_sdtm$analyte_mapping)
  # expect_equal(read_back_sdtm$metabolite_mapping, original_sdtm$metabolite_mapping)
  # expect_equal(read_back_sdtm$parent_mapping, original_sdtm$parent_mapping)
  # expect_equal(read_back_sdtm$time_mapping, original_sdtm$time_mapping)
})



# Helper function to create a minimal nif object for testing
create_test_nif <- function(study_id = "TEST01") {
  tibble::tribble(
    ~ID, ~STUDYID, ~TIME, ~EVID, ~AMT, ~CMT, ~DV, ~MDV,
    1,   study_id,  0,     1,     100,  1,    NA,  1,
    1,   study_id,  0,     0,     0,    2,    0,   0,
    1,   study_id,  1,     0,     0,    2,    5.2, 0,
    2,   study_id,  0,     1,     150,  1,    NA,  1,
    2,   study_id,  0,     0,     0,    2,    0,   0,
    2,   study_id,  1,     0,     0,    2,    8.7, 0
  ) %>%
    nif(silent = TRUE)
}


test_that("pb_read_nif validates inputs correctly", {
  # Create a temporary board directory
  test_board <- tempfile("test_board")
  dir.create(test_board)
  on.exit(unlink(test_board, recursive = TRUE))

  # Test invalid board parameter (non-character)
  expect_error(
    pb_read_nif("test_pin", board = 123),
    "board must be a string value"
  )

  # Test invalid name parameter (non-character)
  expect_error(
    pb_read_nif(name = 456, board = test_board),
    "name must be a string value"
  )

  # Test missing name parameter
  expect_error(
    pb_read_nif(board = test_board),
    "argument \"name\" is missing"
  )

  # Test NULL name (should error)
  expect_error(
    pb_read_nif(name = NULL, board = test_board),
    "name must not be NULL"
  )

  # Test empty name (should error)
  expect_error(
    pb_read_nif(name = "", board = test_board),
    "name must be a non-empty string"
  )

  # Test NA name
  expect_error(
    pb_read_nif(name = NA_character_, board = test_board),
    "name must not contain NA"
  )

  # Test NA board
  expect_error(
    pb_read_nif("test_pin", board = NA_character_),
    "board must not contain NA"
  )
})


test_that("pb_read_nif handles missing board correctly", {
  # Create a minimal nif object for testing
  test_nif <- create_test_nif()

  # Create a temporary board directory
  test_board <- tempfile("test_board")
  dir.create(test_board)
  on.exit(unlink(test_board, recursive = TRUE))

  # Write a pin first
  pb_write.nif(test_nif, board = test_board, name = "test_read", silent = TRUE)

  # Clear nif_option and env var
  withr::local_options(list(nif.pinboard = NULL))
  withr::local_envvar("NIF_PINBOARD" = "")

  # Clear any existing pinboard option
  nif_option(pinboard = "")

  # Test with NULL board and no nif option set
  expect_error(
    pb_read_nif("test_read", board = NULL),
    "No pinboard found"
  )
})


test_that("pb_read_nif reads nif object successfully", {
  # Create a temporary board directory
  test_board <- tempfile("test_board")
  dir.create(test_board)
  on.exit(unlink(test_board, recursive = TRUE))

  # Create and write a test nif object
  test_nif <- create_test_nif()
  pb_write.nif(test_nif, board = test_board, name = "test_read", silent = TRUE)

  # Read it back
  read_nif <- pb_read_nif("test_read", board = test_board)

  # Verify it's a nif object
  expect_s3_class(read_nif, "nif")

  # Verify structure is preserved
  essential_cols <- c("ID", "TIME", "EVID", "AMT", "CMT", "DV", "MDV")
  expect_true(all(essential_cols %in% names(read_nif)))

  # Verify data matches
  expect_equal(nrow(read_nif), nrow(test_nif))
  expect_equal(read_nif$ID, test_nif$ID)
  expect_equal(read_nif$TIME, test_nif$TIME)
  expect_equal(read_nif$EVID, test_nif$EVID)
  expect_equal(read_nif$AMT, test_nif$AMT)
  expect_equal(read_nif$CMT, test_nif$CMT)
  expect_equal(read_nif$DV, test_nif$DV)
  expect_equal(read_nif$MDV, test_nif$MDV)
})


test_that("pb_read_nif uses board from nif_option when board is NULL", {
  # Create a temporary board directory
  test_board <- tempfile("test_board")
  dir.create(test_board)
  on.exit(unlink(test_board, recursive = TRUE))

  # Create and write a test nif object
  test_nif <- create_test_nif()
  pb_write.nif(test_nif, board = test_board, name = "test_read", silent = TRUE)

  # Set pinboard via nif_option
  withr::local_options(list(nif.pinboard = NULL))
  withr::local_envvar("NIF_PINBOARD" = "")
  nif_option(pinboard = test_board)

  # Read without specifying board
  read_nif <- pb_read_nif("test_read", board = NULL)

  # Verify it worked
  expect_s3_class(read_nif, "nif")
  expect_equal(nrow(read_nif), nrow(test_nif))
})


test_that("pb_read_nif uses board from NIF_PINBOARD env var when board is NULL", {
  # Create a temporary board directory
  test_board <- tempfile("test_board")
  dir.create(test_board)
  on.exit(unlink(test_board, recursive = TRUE))

  # Create and write a test nif object
  test_nif <- create_test_nif()
  pb_write.nif(test_nif, board = test_board, name = "test_read", silent = TRUE)

  # Set pinboard via env var
  withr::local_options(list(nif.pinboard = NULL))
  withr::local_envvar("NIF_PINBOARD" = test_board)
  nif_option(pinboard = "")

  # Read without specifying board
  read_nif <- pb_read_nif("test_read", board = NULL)

  # Verify it worked
  expect_s3_class(read_nif, "nif")
  expect_equal(nrow(read_nif), nrow(test_nif))
})


test_that("pb_read_nif prioritizes board parameter over nif_option", {
  # Create two temporary board directories
  test_board1 <- tempfile("test_board1")
  test_board2 <- tempfile("test_board2")
  dir.create(test_board1)
  dir.create(test_board2)
  on.exit({
    unlink(test_board1, recursive = TRUE)
    unlink(test_board2, recursive = TRUE)
  })

  # Create and write test nif objects to both boards
  test_nif1 <- create_test_nif("STUDY01")
  test_nif2 <- create_test_nif("STUDY02")
  pb_write.nif(test_nif1, board = test_board1, name = "test_read", silent = TRUE)
  pb_write.nif(test_nif2, board = test_board2, name = "test_read", silent = TRUE)

  # Set nif_option to board1
  withr::local_options(list(nif.pinboard = NULL))
  withr::local_envvar("NIF_PINBOARD" = "")
  nif_option(pinboard = test_board1)

  # Read from board2 explicitly (should use board parameter, not nif_option)
  read_nif <- pb_read_nif("test_read", board = test_board2)

  # Verify it read from board2 (STUDY02)
  expect_s3_class(read_nif, "nif")
  expect_equal(unique(read_nif$STUDYID), "STUDY02")
})


test_that("pb_read_nif errors when board does not exist", {
  # Create a non-existent board path
  non_existent_board <- tempfile("non_existent")

  # Try to read from non-existent board
  expect_error(
    pb_read_nif("test_pin", board = non_existent_board),
    "Pinboard.*does not exist"
  )
})


test_that("pb_read_nif validates returned object is a nif object", {
  # Create a temporary board directory
  test_board <- tempfile("test_board")
  dir.create(test_board)
  on.exit(unlink(test_board, recursive = TRUE))

  # Write a non-nif object to the board (using pins directly)
  board_obj <- pins::board_folder(test_board)
  test_data <- data.frame(x = 1:5, y = 6:10)
  suppressMessages(
    pins::pin_write(board_obj, test_data, name = "not_nif", type = "rds")
  )

  # Try to read it with pb_read_nif - should error
  expect_error(
    pb_read_nif("not_nif", board = test_board),
    "Input must be a nif object"
  )
})


test_that("pb_read_nif works with complex nif objects", {
  # Create a temporary board directory
  test_board <- tempfile("test_board")
  dir.create(test_board)
  on.exit(unlink(test_board, recursive = TRUE))

  # Create a complex nif object
  complex_nif <- tibble::tribble(
    ~ID, ~STUDYID, ~TIME, ~EVID, ~AMT, ~CMT, ~DV, ~MDV, ~ANALYTE, ~PARENT, ~DOSE,
    1,   "TEST01",  0,     1,     100,  1,    NA,  1,    "DRUG",   "DRUG",  100,
    1,   "TEST01",  0,     0,     0,    2,    0,   0,    "DRUG",   "DRUG",  100,
    1,   "TEST01",  1,     0,     0,    2,    5.2, 0,    "DRUG",   "DRUG",  100,
    1,   "TEST01",  2,     0,     0,    2,    8.5, 0,    "DRUG",   "DRUG",  100,
    2,   "TEST01",  0,     1,     150,  1,    NA,  1,    "DRUG",   "DRUG",  150,
    2,   "TEST01",  0,     0,     0,    2,    0,   0,    "DRUG",   "DRUG",  150,
    2,   "TEST01",  1,     0,     0,    2,    10.3, 0,   "DRUG",   "DRUG",  150,
    2,   "TEST01",  2,     0,     0,    2,    12.1, 0,   "DRUG",   "DRUG",  150
  ) %>%
    nif(silent = TRUE)

  # Write the complex nif object
  pb_write.nif(complex_nif, board = test_board, name = "complex_read", silent = TRUE)

  # Read it back
  read_nif <- pb_read_nif("complex_read", board = test_board)

  # Verify structure
  expect_s3_class(read_nif, "nif")
  expect_equal(nrow(read_nif), nrow(complex_nif))
  expect_true("ANALYTE" %in% names(read_nif))
  expect_true("PARENT" %in% names(read_nif))
  expect_true("DOSE" %in% names(read_nif))

  # Verify data matches
  expect_equal(read_nif$ANALYTE, complex_nif$ANALYTE)
  expect_equal(read_nif$PARENT, complex_nif$PARENT)
  expect_equal(read_nif$DOSE, complex_nif$DOSE)
})


test_that("pb_read_nif works with multi-study nif objects", {
  # Create a temporary board directory
  test_board <- tempfile("test_board")
  dir.create(test_board)
  on.exit(unlink(test_board, recursive = TRUE))

  # Create a multi-study nif object
  nif1 <- create_test_nif("STUDY01")
  nif2 <- create_test_nif("STUDY02")
  multi_nif <- dplyr::bind_rows(nif1, nif2) %>%
    nif(silent = TRUE) |>
    index_id() |>
    arrange_and_add_ref()

  # Write the multi-study nif object
  pb_write.nif(multi_nif, board = test_board, name = "multi_read", silent = TRUE)

  # Read it back
  read_nif <- pb_read_nif("multi_read", board = test_board)

  # Verify structure
  expect_s3_class(read_nif, "nif")
  expect_equal(nrow(read_nif), nrow(multi_nif))
  expect_equal(sort(unique(read_nif$STUDYID)), c("STUDY01", "STUDY02"))
})


test_that("pb_read_nif works with empty nif objects", {
  # Create a temporary board directory
  test_board <- tempfile("test_board")
  dir.create(test_board)
  on.exit(unlink(test_board, recursive = TRUE))

  # Create an empty nif object
  empty_nif <- nif(silent = TRUE)

  # Write the empty nif object
  pb_write.nif(empty_nif, board = test_board, name = "empty_read", silent = TRUE)

  # Read it back
  read_nif <- pb_read_nif("empty_read", board = test_board)

  # Verify structure
  expect_s3_class(read_nif, "nif")
  expect_equal(nrow(read_nif), 0)
})


test_that("pb_read_nif works with nif objects without STUDYID", {
  # Create a temporary board directory
  test_board <- tempfile("test_board")
  dir.create(test_board)
  on.exit(unlink(test_board, recursive = TRUE))

  # Create a nif object without STUDYID
  test_nif <- tibble::tribble(
    ~ID, ~TIME, ~EVID, ~AMT, ~CMT, ~DV, ~MDV,
    1,   0,     1,     100,  1,    NA,  1,
    1,   0,     0,     0,    2,    0,   0,
    1,   1,     0,     0,    2,    5.2, 0
  ) %>%
    nif(silent = TRUE)

  # Write the nif object
  pb_write.nif(test_nif, board = test_board, name = "no_studyid_read", silent = TRUE)

  # Read it back
  read_nif <- pb_read_nif("no_studyid_read", board = test_board)

  # Verify structure
  expect_s3_class(read_nif, "nif")
  expect_equal(nrow(read_nif), nrow(test_nif))
  expect_equal(read_nif$ID, test_nif$ID)
})


test_that("pb_read_nif preserves all nif object attributes", {
  # Create a temporary board directory
  test_board <- tempfile("test_board")
  dir.create(test_board)
  on.exit(unlink(test_board, recursive = TRUE))

  # Create a test nif object
  original_nif <- create_test_nif()

  # Write the nif object
  pb_write.nif(original_nif, board = test_board, name = "attr_test", silent = TRUE)

  # Read it back
  read_nif <- pb_read_nif("attr_test", board = test_board)

  # Verify class is preserved
  expect_s3_class(read_nif, "nif")

  # Verify all columns are preserved
  expect_equal(names(read_nif), names(original_nif))

  # Verify data types are preserved
  expect_equal(sapply(read_nif, class), sapply(original_nif, class))
})


test_that("pb_read_nif handles multiple reads of the same pin", {
  # Create a temporary board directory
  test_board <- tempfile("test_board")
  dir.create(test_board)
  on.exit(unlink(test_board, recursive = TRUE))

  # Create and write a test nif object
  test_nif <- create_test_nif()
  pb_write.nif(test_nif, board = test_board, name = "multi_read_test", silent = TRUE)

  # Read multiple times
  read_nif1 <- pb_read_nif("multi_read_test", board = test_board)
  read_nif2 <- pb_read_nif("multi_read_test", board = test_board)
  read_nif3 <- pb_read_nif("multi_read_test", board = test_board)

  # All should be identical
  expect_equal(read_nif1, read_nif2)
  expect_equal(read_nif2, read_nif3)
  expect_equal(read_nif1, read_nif3)
})


test_that("pb_read_nif handles pin names with special characters", {
  # Create a temporary board directory
  test_board <- tempfile("test_board")
  dir.create(test_board)
  on.exit(unlink(test_board, recursive = TRUE))

  # Create a test nif object
  test_nif <- create_test_nif()

  # Write with a pin name containing special characters (if allowed by pins)
  pin_name <- "test-pin_with.special_chars"
  pb_write.nif(test_nif, board = test_board, name = pin_name, silent = TRUE)

  # Read it back
  read_nif <- pb_read_nif(pin_name, board = test_board)

  # Verify it worked
  expect_s3_class(read_nif, "nif")
  expect_equal(nrow(read_nif), nrow(test_nif))
})


test_that("pb_read_nif handles very long pin names", {
  # Create a temporary board directory
  test_board <- tempfile("test_board")
  dir.create(test_board)
  on.exit(unlink(test_board, recursive = TRUE))

  # Create a test nif object
  test_nif <- create_test_nif()

  # Create a very long pin name
  long_name <- paste(rep("a", 100), collapse = "")
  pb_write.nif(test_nif, board = test_board, name = long_name, silent = TRUE)

  # Read it back
  read_nif <- pb_read_nif(long_name, board = test_board)

  # Verify it worked
  expect_s3_class(read_nif, "nif")
  expect_equal(nrow(read_nif), nrow(test_nif))
})


test_that("pb_read_nif works with absolute board paths", {
  # Create a temporary board directory with absolute path
  test_board <- normalizePath(tempfile("test_board"), mustWork = FALSE)
  dir.create(test_board)
  on.exit(unlink(test_board, recursive = TRUE))
  test_board <- normalizePath(test_board)

  # Create and write a test nif object
  test_nif <- create_test_nif()
  pb_write.nif(test_nif, board = test_board, name = "abs_path_test", silent = TRUE)

  # Read it back
  read_nif <- pb_read_nif("abs_path_test", board = test_board)

  # Verify it worked
  expect_s3_class(read_nif, "nif")
  expect_equal(nrow(read_nif), nrow(test_nif))
})


test_that("pb_read_nif returns identical object to what was written", {
  # Create a temporary board directory
  test_board <- tempfile("test_board")
  dir.create(test_board)
  on.exit(unlink(test_board, recursive = TRUE))

  # Create a test nif object
  original_nif <- create_test_nif()

  # Write the nif object
  pb_write.nif(original_nif, board = test_board, name = "identical_test", silent = TRUE)

  # Read it back
  read_nif <- pb_read_nif("identical_test", board = test_board)

  # Should be identical (using all.equal for data frames)
  expect_equal(read_nif, original_nif, ignore_attr = TRUE)
})

