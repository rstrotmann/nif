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

# Helper function to create a multi-study nif object
create_multi_study_nif <- function() {
  nif1 <- create_test_nif("STUDY01")
  nif2 <- create_test_nif("STUDY02")

  dplyr::bind_rows(nif1, nif2) %>%
    nif(silent = TRUE)
}


test_that("pb_write.nif validates inputs correctly", {
  # Create a minimal nif object for testing
  test_nif <- create_test_nif()

  # Test invalid nif object
  expect_error(
    pb_write.nif("not_nif"),
    "Input must be a nif object"
  )

  # Test invalid board parameter
  expect_error(
    pb_write.nif(test_nif, board = 123),
    "board must be a string value"
  )

  # Test invalid name parameter
  expect_error(
    pb_write.nif(test_nif, name = 456),
    "name must be a string value"
  )

  # Test invalid title parameter
  expect_error(
    pb_write.nif(test_nif, title = 789),
    "title must be a string value"
  )

  # Test invalid dco parameter
  expect_error(
    pb_write.nif(test_nif, dco = 123),
    "dco must be a string value"
  )

  # Test invalid force parameter
  expect_error(
    pb_write.nif(test_nif, force = "TRUE"),
    "force must be a logical value"
  )

  # Test invalid force parameter (numeric)
  expect_error(
    pb_write.nif(test_nif, force = 1),
    "force must be a logical value"
  )

  # Test invalid force parameter (vector)
  expect_error(
    pb_write.nif(test_nif, force = c(TRUE, FALSE)),
    "force must be a single value"
  )
})


test_that("pb_write.nif handles missing board correctly", {
  # Create a minimal nif object for testing
  test_nif <- create_test_nif()

  withr::local_envvar("NIF_PINBOARD" = "")

  # Test with NULL board and no nif option set
  expect_error(
    pb_write.nif(test_nif, board = NULL),
    "No pinboard found"
  )

  # Test with NA board
  expect_error(
    pb_write.nif(test_nif, board = NA_character_),
    "board must not contain NA"
  )
})


test_that("pb_write.nif uses arbitrary board", {
  # Create a minimal nif object for testing
  test_nif <- create_test_nif()

  # Test with arbitrary directory
  test_board <- tempdir(check = TRUE)
  expect_no_error(
    pb_write.nif(test_nif, board = test_board, silent = TRUE)
  )
})


test_that("pb_write.nif generates correct default names and titles", {
  # Create a temporary board directory
  test_board <- tempfile("test_board")
  dir.create(test_board)
  on.exit(unlink(test_board, recursive = TRUE))

  # Create a minimal nif object for testing
  test_nif <- create_test_nif()

  # Test with NULL name and title (should use defaults)
  expect_no_error(
    pb_write.nif(test_nif,
      board = test_board, name = NULL, title = NULL,
      silent = TRUE
    )
  )

  # Check that the pin was created with expected name
  board_obj <- pins::board_folder(test_board)
  pins_list <- pins::pin_list(board_obj)
  expect_true("TEST01_nif" %in% pins_list)
})


test_that("pb_write.nif generates correct default names for multiple studies", {
  # Create a temporary board directory
  test_board <- tempfile("test_board")
  dir.create(test_board)
  on.exit(unlink(test_board, recursive = TRUE))

  # Create a multi-study nif object
  test_nif <- create_multi_study_nif()

  # Test with NULL name and title (should use defaults)
  expect_no_error(
    pb_write.nif(test_nif,
      board = test_board, name = NULL, title = NULL,
      silent = TRUE
    )
  )

  # Check that the pin was created with expected name (studies joined by _)
  board_obj <- pins::board_folder(test_board)
  pins_list <- pins::pin_list(board_obj)
  # Order might vary, so check for either possible combination
  # Extract the pin name that contains both studies
  matching_pins <- pins_list[grepl("STUDY01.*STUDY02|STUDY02.*STUDY01", pins_list)]
  expect_true(length(matching_pins) > 0)
  expect_true(grepl("_nif$", matching_pins[1]))
})


test_that("pb_write.nif handles nif objects without STUDYID", {
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

  # Test with NULL name and title (should handle missing STUDYID)
  expect_no_error(
    pb_write.nif(test_nif,
      board = test_board, name = "test_no_studyid",
      silent = TRUE
    )
  )

  # Check that the pin was created
  board_obj <- pins::board_folder(test_board)
  pins_list <- pins::pin_list(board_obj)
  expect_true("test_no_studyid" %in% pins_list)
})


test_that("pb_write.nif uses custom name and title when provided", {
  # Create a temporary board directory
  test_board <- tempfile("test_board")
  dir.create(test_board)
  on.exit(unlink(test_board, recursive = TRUE))

  # Create a minimal nif object for testing
  test_nif <- create_test_nif()

  # Test with custom name and title
  custom_name <- "my_custom_nif"
  custom_title <- "My Custom NIF Data"

  expect_no_error(
    pb_write.nif(test_nif,
      board = test_board,
      name = custom_name, title = custom_title, silent = TRUE
    )
  )

  # Check that the pin was created with custom name
  board_obj <- pins::board_folder(test_board)
  pins_list <- pins::pin_list(board_obj)
  expect_true(custom_name %in% pins_list)
})


test_that("pb_write.nif handles silent parameter correctly", {
  # Create a temporary board directory
  test_board <- tempfile("test_board")
  dir.create(test_board)
  on.exit(unlink(test_board, recursive = TRUE))

  # Create a minimal nif object for testing
  test_nif <- create_test_nif()

  # Test with silent = TRUE (should not produce messages)
  expect_no_error(
    pb_write.nif(test_nif, board = test_board, silent = TRUE)
  )

  # Test with silent = FALSE (should produce messages)
  expect_message(expect_message(
    expect_no_error(
      pb_write.nif(test_nif, board = test_board, silent = FALSE)
    )
  ))
})


test_that("pb_write.nif works with complex nif objects", {
  # Create a temporary board directory
  test_board <- tempfile("test_board")
  dir.create(test_board)
  on.exit(unlink(test_board, recursive = TRUE))

  # Create a more complex nif object with multiple subjects and observations
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

  # Test writing complex nif object
  expect_no_error(
    pb_write.nif(complex_nif,
      board = test_board, silent = TRUE,
      name = "complex_test", title = "Complex NIF Test"
    )
  )

  # Verify the pin was created and can be read back
  board_obj <- pins::board_folder(test_board)
  pins_list <- pins::pin_list(board_obj)
  expect_true("complex_test" %in% pins_list)
})


test_that("pb_write.nif preserves nif object structure", {
  # Create a temporary board directory
  test_board <- tempfile("test_board")
  dir.create(test_board)
  on.exit(unlink(test_board, recursive = TRUE))

  # Create a test nif object
  original_nif <- create_test_nif()

  # Write the nif object
  pb_write.nif(original_nif,
    board = test_board, name = "test_preserve",
    silent = TRUE
  )

  # Read it back and verify structure is preserved
  board_obj <- pins::board_folder(test_board)
  read_back_nif <- pins::pin_read(board_obj, "test_preserve")

  # Check class
  expect_s3_class(read_back_nif, "nif")

  # Check that essential columns are preserved
  essential_cols <- c("ID", "TIME", "EVID", "AMT", "CMT", "DV", "MDV")
  expect_true(all(essential_cols %in% names(read_back_nif)))

  # Check that data matches
  expect_equal(nrow(read_back_nif), nrow(original_nif))
  expect_equal(read_back_nif$ID, original_nif$ID)
  expect_equal(read_back_nif$TIME, original_nif$TIME)
  expect_equal(read_back_nif$EVID, original_nif$EVID)
  expect_equal(read_back_nif$AMT, original_nif$AMT)
  expect_equal(read_back_nif$CMT, original_nif$CMT)
})


test_that("pb_write.nif handles dco parameter correctly", {
  # Create a temporary board directory
  test_board <- tempfile("test_board")
  dir.create(test_board)
  on.exit(unlink(test_board, recursive = TRUE))

  # Create a minimal nif object for testing
  test_nif <- create_test_nif()

  # Test with dco parameter
  dco_value <- "2023-12-31"
  expect_no_error(
    pb_write.nif(test_nif,
      board = test_board, name = "test_dco",
      dco = dco_value, silent = TRUE
    )
  )

  # Read back and check metadata
  board_obj <- pins::board_folder(test_board)
  pin_meta <- pins::pin_meta(board_obj, "test_dco")
  expect_equal(pin_meta$user$dco, dco_value)
  expect_equal(pin_meta$user$type, "nif")
})


# test_that("pb_write.nif handles force parameter correctly", {
#   # Create a temporary board directory
#   test_board <- tempfile("test_board")
#   dir.create(test_board)
#   on.exit(unlink(test_board, recursive = TRUE))
#
#   # Create a minimal nif object for testing
#   test_nif <- create_test_nif()
#
#   # Write the nif object first time
#   expect_no_error(
#     pb_write.nif(test_nif,
#       board = test_board, name = "test_force",
#       silent = TRUE
#     )
#   )
#
#   # Try to write again without force (should error or warn)
#   expect_no_error(
#     pb_write.nif(test_nif,
#       board = test_board, name = "test_force",
#       force = FALSE, silent = TRUE
#     )
#   )
#
#   # Write again with force = TRUE (should succeed)
#   expect_no_error(
#     pb_write.nif(test_nif,
#       board = test_board, name = "test_force",
#       force = TRUE, silent = TRUE
#     )
#   )
# })


test_that("pb_write.nif includes description from summary", {
  # Create a temporary board directory
  test_board <- tempfile("test_board")
  dir.create(test_board)
  on.exit(unlink(test_board, recursive = TRUE))

  # Create a minimal nif object for testing
  test_nif <- create_test_nif()

  # Write the nif object
  expect_no_error(
    pb_write.nif(test_nif,
      board = test_board, name = "test_description",
      silent = TRUE
    )
  )

  # Read back and check that description exists
  board_obj <- pins::board_folder(test_board)
  pin_meta <- pins::pin_meta(board_obj, "test_description")
  expect_true(!is.null(pin_meta$description))
  expect_true(length(pin_meta$description) > 0)
})


test_that("pb_write.nif works with empty nif object", {
  # Create a temporary board directory
  test_board <- tempfile("test_board")
  dir.create(test_board)
  on.exit(unlink(test_board, recursive = TRUE))

  # Create an empty nif object
  empty_nif <- nif(silent = TRUE)

  # Test writing empty nif object
  expect_no_error(
    pb_write.nif(empty_nif,
      board = test_board, name = "empty_nif",
      silent = TRUE
    )
  )

  # Verify the pin was created
  board_obj <- pins::board_folder(test_board)
  pins_list <- pins::pin_list(board_obj)
  expect_true("empty_nif" %in% pins_list)

  # Read it back and verify it's still empty
  read_back_nif <- pins::pin_read(board_obj, "empty_nif")
  expect_s3_class(read_back_nif, "nif")
  expect_equal(nrow(read_back_nif), 0)
})


test_that("pb_write.nif works with generic pb_write function", {
  # Create a temporary board directory
  test_board <- tempfile("test_board")
  dir.create(test_board)
  on.exit(unlink(test_board, recursive = TRUE))

  # Create a minimal nif object for testing
  test_nif <- create_test_nif()

  # Test using generic pb_write function (should dispatch to pb_write.nif)
  expect_no_error(
    pb_write(test_nif,
      board = test_board, name = "test_generic",
      silent = TRUE
    )
  )

  # Verify the pin was created
  board_obj <- pins::board_folder(test_board)
  pins_list <- pins::pin_list(board_obj)
  expect_true("test_generic" %in% pins_list)
})

