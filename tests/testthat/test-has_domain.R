# Test file for has_domain() and domain() functions

test_that("has_domain correctly identifies existing domains", {
  # Create a simple SDTM object for testing
  test_data <- list(
    domains = list(
      dm = data.frame(USUBJID = c("SUBJ-001", "SUBJ-002"), DOMAIN = "DM"),
      pc = data.frame(USUBJID = c("SUBJ-001", "SUBJ-002"), DOMAIN = "PC"),
      ex = data.frame(USUBJID = c("SUBJ-001", "SUBJ-002"), DOMAIN = "EX")
    )
  )
  class(test_data) <- c("sdtm", "list")

  # Test with existing domains
  expect_true(has_domain(test_data, "dm"))
  expect_true(has_domain(test_data, "pc"))
  expect_true(has_domain(test_data, "ex"))

  # Test with non-existing domain
  expect_false(has_domain(test_data, "lb"))
  expect_false(has_domain(test_data, "ae"))
  expect_false(has_domain(test_data, "vs"))
})


test_that("has_domain is case-insensitive", {
  # Create a simple SDTM object for testing
  test_data <- list(
    domains = list(
      dm = data.frame(USUBJID = c("SUBJ-001", "SUBJ-002"), DOMAIN = "DM")
    )
  )
  class(test_data) <- c("sdtm", "list")

  # Test with different case variations
  expect_true(has_domain(test_data, "dm"))
  expect_true(has_domain(test_data, "DM"))
  expect_true(has_domain(test_data, "Dm"))
  expect_true(has_domain(test_data, "dM"))
})


test_that("has_domain handles input validation correctly", {
  # Create a simple SDTM object for testing
  test_data <- list(
    domains = list(
      dm = data.frame(USUBJID = c("SUBJ-001", "SUBJ-002"), DOMAIN = "DM")
    )
  )
  class(test_data) <- c("sdtm", "list")

  # Test with invalid object
  invalid_obj <- list(domains = list())
  expect_error(has_domain(invalid_obj, "dm"), "'obj' must be an SDTM object")

  # Test with NULL object
  expect_error(has_domain(NULL, "dm"), "'obj' must be an SDTM object")

  # Test with invalid name
  expect_error(has_domain(test_data, NULL), "'name' must be a non-empty character vector")
  expect_error(has_domain(test_data, 123), "'name' must be a non-empty character vector")
  expect_error(has_domain(test_data, character(0)), "'name' must be a non-empty character vector")
})


test_that("has_domain handles multiple domain names correctly", {
  # Create a simple SDTM object for testing
  test_data <- list(
    domains = list(
      dm = data.frame(USUBJID = c("SUBJ-001", "SUBJ-002"), DOMAIN = "DM"),
      pc = data.frame(USUBJID = c("SUBJ-001", "SUBJ-002"), DOMAIN = "PC"),
      ex = data.frame(USUBJID = c("SUBJ-001", "SUBJ-002"), DOMAIN = "EX")
    )
  )
  class(test_data) <- c("sdtm", "list")

  # Test with multiple existing domains
  expect_true(has_domain(test_data, c("dm", "pc")))
  expect_true(has_domain(test_data, c("dm", "pc", "ex")))
  
  # Test with mixture of existing and non-existing domains
  expect_false(has_domain(test_data, c("dm", "nonexistent")))
  expect_false(has_domain(test_data, c("nonexistent", "dm")))
  expect_false(has_domain(test_data, c("dm", "pc", "nonexistent")))
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

  # domain() rejects vectors with multiple names
  expect_error(domain(test_data, c("dm", "pc")),
               "'name' must be a single domain name, not a vector of multiple names")
})


test_that("has_domain works with example data", {
  # Test with package example data if available
  skip_if_not_installed("nif")
  skip_if_not(exists("examplinib_fe"), "Example SDTM data not available")

  # This assumes examplinib_fe is a sdtm object with at least dm domain
  if (exists("examplinib_fe")) {
    expect_true(has_domain(examplinib_fe, "dm"))
  }
})


test_that("domain function behaviors", {
  # Create a simple SDTM object for testing
  test_data <- list(
    domains = list(
      dm = data.frame(USUBJID = c("SUBJ-001", "SUBJ-002"), DOMAIN = "DM"),
      pc = data.frame(USUBJID = c("SUBJ-001", "SUBJ-002"), DOMAIN = "PC")
    )
  )
  class(test_data) <- c("sdtm", "list")

  # Test that domain() works with a single name
  expect_s3_class(domain(test_data, "dm"), "data.frame")

  # Test that domain() is case-insensitive
  expect_s3_class(domain(test_data, "DM"), "data.frame")
  expect_s3_class(domain(test_data, "Dm"), "data.frame")
})

