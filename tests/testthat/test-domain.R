# Test file for domain() function

test_that("domain() returns correct data frames for existing domains", {
  # Create a simple SDTM object for testing
  test_dm <- data.frame(
    USUBJID = c("SUBJ-001", "SUBJ-002"),
    DOMAIN = "DM",
    AGE = c(25, 30)
  )

  test_pc <- data.frame(
    USUBJID = c("SUBJ-001", "SUBJ-001", "SUBJ-002"),
    DOMAIN = "PC",
    PCSTRESN = c(10.5, 15.2, 12.3)
  )

  test_data <- list(
    domains = list(
      dm = test_dm,
      pc = test_pc
    )
  )
  class(test_data) <- c("sdtm", "list")

  # Test domain() returns the correct data frames
  expect_identical(domain(test_data, "dm"), test_dm)
  expect_identical(domain(test_data, "pc"), test_pc)
})


test_that("domain() errors for non-existent domains", {
  # Create a simple SDTM object for testing
  test_data <- list(
    domains = list(
      dm = data.frame(USUBJID = c("SUBJ-001", "SUBJ-002"), DOMAIN = "DM")
    )
  )
  class(test_data) <- c("sdtm", "list")

  # Test with non-existing domain
  expect_error(domain(test_data, "lb"), "Domain 'lb' not found in SDTM object")
  expect_error(domain(test_data, "ae"), "Domain 'ae' not found in SDTM object")
  expect_error(domain(test_data, "vs"), "Domain 'vs' not found in SDTM object")
})



test_that("domain() is case-insensitive", {
  # Create a simple SDTM object for testing
  test_dm <- data.frame(USUBJID = c("SUBJ-001", "SUBJ-002"), DOMAIN = "DM")
  test_data <- list(
    domains = list(
      dm = test_dm
    )
  )
  class(test_data) <- c("sdtm", "list")

  # Test with different case variations
  expect_identical(domain(test_data, "dm"), test_dm)
  expect_identical(domain(test_data, "DM"), test_dm)
  expect_identical(domain(test_data, "Dm"), test_dm)
  expect_identical(domain(test_data, "dM"), test_dm)
})


test_that("domain() handles input validation correctly", {
  # Create a simple SDTM object for testing
  test_data <- list(
    domains = list(
      dm = data.frame(USUBJID = c("SUBJ-001", "SUBJ-002"), DOMAIN = "DM")
    )
  )
  class(test_data) <- c("sdtm", "list")

  # Test with invalid object
  invalid_obj <- list(domains = list())
  expect_error(
    domain(invalid_obj, "dm"),
    "Input must be a sdtm object")

  # Test with NULL object
  expect_error(
    domain(NULL, "dm"),
    "Input must be a sdtm object")

  # Test with NULL, numeric, or empty character as name
  expect_error(
    domain(test_data, NULL),
    "name must not be NULL")
  expect_error(
    domain(test_data, 123),
    "name must be a string value")
  expect_error(
    domain(test_data, character(0)),
    "name must be a single value")
})


test_that("domain() rejects vectors with multiple names", {
  # Create a simple SDTM object for testing
  test_data <- list(
    domains = list(
      dm = data.frame(USUBJID = c("SUBJ-001", "SUBJ-002"), DOMAIN = "DM"),
      pc = data.frame(USUBJID = c("SUBJ-001", "SUBJ-002"), DOMAIN = "PC")
    )
  )
  class(test_data) <- c("sdtm", "list")

  # Test with vector of multiple names
  expect_error(
    domain(test_data, c("dm", "pc")),
    "name must be a single value")
})

