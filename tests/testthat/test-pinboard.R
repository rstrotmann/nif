test_that("pb_write.sdtm validates inputs correctly", {
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
  test_sdtm <- new_sdtm(
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
  original_sdtm <- new_sdtm(
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
