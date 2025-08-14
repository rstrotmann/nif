# Test file for validate_testcd() function

test_that("validate_testcd works correctly with valid inputs", {
  # Create a simple SDTM object for testing
  test_dm <- tibble::tribble(
    ~USUBJID, ~DOMAIN, ~DMTESTCD, ~DMTEST,
    "SUBJ-001", "DM", "AGE", "Age",
    "SUBJ-002", "DM", "AGE", "Age"
  )

  test_pc <- tibble::tribble(
    ~USUBJID, ~DOMAIN, ~PCTESTCD, ~PCTEST, ~PCSTRESN,
    "SUBJ-001", "PC", "RS2023", "Concentration", 10.5,
    "SUBJ-001", "PC", "RS2024", "Concentration", 15.2,
    "SUBJ-002", "PC", "RS2023", "Concentration", 12.3
  )

  test_lb <- tibble::tribble(
    ~USUBJID, ~DOMAIN, ~LBTESTCD, ~LBTEST, ~LBSTRESN,
    "SUBJ-001", "LB", "CREAT", "Creatinine", 0.8,
    "SUBJ-002", "LB", "CREAT", "Creatinine", 0.9
  )

  test_sdtm <- list(
    domains = list(
      dm = test_dm,
      pc = test_pc,
      lb = test_lb
    )
  )
  class(test_sdtm) <- c("sdtm", "list")

  # Test successful validation without domain specification
  expect_equal(validate_testcd(test_sdtm, "AGE"), "AGE")
  expect_equal(validate_testcd(test_sdtm, "RS2023"), "RS2023")
  expect_equal(validate_testcd(test_sdtm, "CREAT"), "CREAT")
  expect_equal(validate_testcd(test_sdtm, c("AGE", "RS2023")), c("AGE", "RS2023"))

  # Test successful validation with domain specification
  expect_equal(validate_testcd(test_sdtm, "AGE", "dm"), "AGE")
  expect_equal(validate_testcd(test_sdtm, "RS2023", "pc"), "RS2023")
  expect_equal(validate_testcd(test_sdtm, "CREAT", "lb"), "CREAT")
  expect_equal(validate_testcd(test_sdtm, c("RS2023", "RS2024"), "pc"), c("RS2023", "RS2024"))
})

test_that("validate_testcd handles case-insensitive domain names", {
  # Create a simple SDTM object for testing
  test_pc <- tibble::tribble(
    ~USUBJID, ~DOMAIN, ~PCTESTCD, ~PCTEST, ~PCSTRESN,
    "SUBJ-001", "PC", "RS2023", "Concentration", 10.5
  )

  test_sdtm <- list(
    domains = list(
      pc = test_pc
    )
  )
  class(test_sdtm) <- c("sdtm", "list")

  # Test with different case variations
  expect_equal(validate_testcd(test_sdtm, "RS2023", "pc"), "RS2023")
  expect_equal(validate_testcd(test_sdtm, "RS2023", "PC"), "RS2023")
})

test_that("validate_testcd handles NULL domain parameter", {
  # Create a simple SDTM object for testing
  test_pc <- tibble::tribble(
    ~USUBJID, ~DOMAIN, ~PCTESTCD, ~PCTEST, ~PCSTRESN,
    "SUBJ-001", "PC", "RS2023", "Concentration", 10.5
  )

  test_sdtm <- list(
    domains = list(
      pc = test_pc
    )
  )
  class(test_sdtm) <- c("sdtm", "list")

  # Test with NULL domain (should validate across all domains)
  expect_equal(validate_testcd(test_sdtm, "RS2023", NULL), "RS2023")
  expect_equal(validate_testcd(test_sdtm, "RS2023"), "RS2023")  # NULL is default
})

test_that("validate_testcd handles multiple testcd values", {
  # Create a simple SDTM object for testing
  test_pc <- tibble::tribble(
    ~USUBJID, ~DOMAIN, ~PCTESTCD, ~PCTEST, ~PCSTRESN,
    "SUBJ-001", "PC", "RS2023", "Concentration", 10.5,
    "SUBJ-001", "PC", "RS2024", "Concentration", 15.2,
    "SUBJ-002", "PC", "RS2025", "Concentration", 12.3
  )

  test_sdtm <- list(
    domains = list(
      pc = test_pc
    )
  )
  class(test_sdtm) <- c("sdtm", "list")

  # Test with multiple testcd values
  expect_equal(validate_testcd(test_sdtm, c("RS2023", "RS2024", "RS2025")),
               c("RS2023", "RS2024", "RS2025"))
  expect_equal(validate_testcd(test_sdtm, c("RS2023", "RS2024", "RS2025"), "pc"),
               c("RS2023", "RS2024", "RS2025"))
})

test_that("validate_testcd errors for non-existent domain", {
  # Create a simple SDTM object for testing
  test_pc <- tibble::tribble(
    ~USUBJID, ~DOMAIN, ~PCTESTCD, ~PCTEST, ~PCSTRESN,
    "SUBJ-001", "PC", "RS2023", "Concentration", 10.5
  )

  test_sdtm <- list(
    domains = list(
      pc = test_pc
    )
  )
  class(test_sdtm) <- c("sdtm", "list")

  # Test with non-existent domain
  expect_error(validate_testcd(test_sdtm, "RS2023", "lb"),
               "Domain lb not found in sdtm object!")
  expect_error(validate_testcd(test_sdtm, "RS2023", "ae"),
               "Domain ae not found in sdtm object!")
})

test_that("validate_testcd errors for domain without TESTCD field", {
  # Create a simple SDTM object for testing with domain missing TESTCD field
  test_dm <- tibble::tribble(
    ~USUBJID, ~DOMAIN, ~AGE,
    "SUBJ-001", "DM", 25,
    "SUBJ-002", "DM", 30
  )

  test_sdtm <- list(
    domains = list(
      dm = test_dm
    )
  )
  class(test_sdtm) <- c("sdtm", "list")

  # Test with domain that has no TESTCD field
  expect_error(validate_testcd(test_sdtm, "AGE", "dm"),
               "DM has no DMTESTCD field!")
})

test_that("validate_testcd errors for missing testcd in specified domain", {
  # Create a simple SDTM object for testing
  test_pc <- tibble::tribble(
    ~USUBJID, ~DOMAIN, ~PCTESTCD, ~PCTEST, ~PCSTRESN,
    "SUBJ-001", "PC", "RS2023", "Concentration", 10.5,
    "SUBJ-002", "PC", "RS2024", "Concentration", 12.3
  )

  test_sdtm <- list(
    domains = list(
      pc = test_pc
    )
  )
  class(test_sdtm) <- c("sdtm", "list")

  # Test with testcd that doesn't exist in the specified domain
  expect_error(validate_testcd(test_sdtm, "RS2025", "pc"),
               "Testcd RS2025 not found in domain PC!")
  expect_error(validate_testcd(test_sdtm, c("RS2023", "RS2025"), "pc"),
               "Testcd RS2025 not found in domain PC!")
})

test_that("validate_testcd errors for missing testcd across all domains", {
  # Create a simple SDTM object for testing
  test_pc <- tibble::tribble(
    ~USUBJID, ~DOMAIN, ~PCTESTCD, ~PCTEST, ~PCSTRESN,
    "SUBJ-001", "PC", "RS2023", "Concentration", 10.5
  )

  test_sdtm <- list(
    domains = list(
      pc = test_pc
    )
  )
  class(test_sdtm) <- c("sdtm", "list")

  # Test with testcd that doesn't exist in any domain
  expect_error(validate_testcd(test_sdtm, "NONEXISTENT"),
               "Testcd NONEXISTENT not found in sdtm!")
  expect_error(validate_testcd(test_sdtm, c("RS2023", "NONEXISTENT")),
               "Testcd NONEXISTENT not found in sdtm!")
})

test_that("validate_testcd handles input validation correctly", {
  # Create a simple SDTM object for testing
  test_pc <- tibble::tribble(
    ~USUBJID, ~DOMAIN, ~PCTESTCD, ~PCTEST, ~PCSTRESN,
    "SUBJ-001", "PC", "RS2023", "Concentration", 10.5
  )

  test_sdtm <- list(
    domains = list(
      pc = test_pc
    )
  )
  class(test_sdtm) <- c("sdtm", "list")

  # Test with invalid sdtm object
  expect_error(validate_testcd("not_sdtm", "RS2023"),
               "Input must be a sdtm object")
  expect_error(validate_testcd(NULL, "RS2023"),
               "Input must be a sdtm object")

  # Test with invalid testcd parameter
  expect_error(validate_testcd(test_sdtm, 123),
               "must be a string value")
  expect_error(validate_testcd(test_sdtm, TRUE),
               "must be a string value")
  expect_error(validate_testcd(test_sdtm, NULL),
               "must not be NULL")

  # Test with invalid domain parameter
  expect_error(validate_testcd(test_sdtm, "RS2023", 123),
               "must be a string value")
  expect_error(validate_testcd(test_sdtm, "RS2023", TRUE),
               "must be a string value")
  expect_error(validate_testcd(test_sdtm, "RS2023", c("pc", "dm")),
               "must be a single value")
})

test_that("validate_testcd works with empty domains", {
  # Create a simple SDTM object with empty domain
  test_empty <- tibble::tribble(
    ~USUBJID, ~DOMAIN, ~PCTESTCD, ~PCTEST, ~PCSTRESN
  )

  test_sdtm <- list(
    domains = list(
      pc = test_empty
    )
  )
  class(test_sdtm) <- c("sdtm", "list")

  # Test with empty domain - should error since no testcd values exist
  expect_error(validate_testcd(test_sdtm, "RS2023", "pc"),
               "Testcd RS2023 not found in domain PC!")
})

test_that("validate_testcd handles mixed case testcd values", {
  # Create a simple SDTM object with mixed case testcd values
  test_pc <- tibble::tribble(
    ~USUBJID, ~DOMAIN, ~PCTESTCD, ~PCTEST, ~PCSTRESN,
    "SUBJ-001", "PC", "rs2023", "Concentration", 10.5,
    "SUBJ-002", "PC", "RS2024", "Concentration", 12.3
  )

  test_sdtm <- list(
    domains = list(
      pc = test_pc
    )
  )
  class(test_sdtm) <- c("sdtm", "list")

  # Test with exact case matching
  expect_equal(validate_testcd(test_sdtm, "rs2023", "pc"), "rs2023")
  expect_equal(validate_testcd(test_sdtm, "RS2024", "pc"), "RS2024")
  expect_equal(validate_testcd(test_sdtm, c("rs2023", "RS2024")), c("rs2023", "RS2024"))
})

test_that("validate_testcd works with single testcd in multiple domains", {
  # Create a simple SDTM object with same testcd in multiple domains
  test_dm <- tibble::tribble(
    ~USUBJID, ~DOMAIN, ~DMTESTCD, ~DMTEST,
    "SUBJ-001", "DM", "AGE", "Age"
  )

  test_vs <- tibble::tribble(
    ~USUBJID, ~DOMAIN, ~VSTESTCD, ~VSTEST, ~VSSTRESN,
    "SUBJ-001", "VS", "AGE", "Age", 25
  )

  test_sdtm <- list(
    domains = list(
      dm = test_dm,
      vs = test_vs
    )
  )
  class(test_sdtm) <- c("sdtm", "list")

  # Test with testcd that exists in multiple domains
  expect_equal(validate_testcd(test_sdtm, "AGE"), "AGE")
  expect_equal(validate_testcd(test_sdtm, "AGE", "dm"), "AGE")
  expect_equal(validate_testcd(test_sdtm, "AGE", "vs"), "AGE")
})

