test_that("guess_ntime correctly parses various time formats", {
  mock_pc <- tibble::tribble(
    ~USUBJID, ~DOMAIN, ~PCTPT,
    "SUBJ-001", "PC", "HOUR 2",
    "SUBJ-001", "PC", "2 HOURS",
    "SUBJ-001", "PC", "2h",
    "SUBJ-001", "PC", "2 hr",
    "SUBJ-001", "PC", "2hrs",
    "SUBJ-001", "PC", "PREDOSE",
    "SUBJ-001", "PC", "PRE DOSE",
    "SUBJ-001", "PC", "PRE-DOSE 2h",
    "SUBJ-001", "PC", "2.5 HOURS",
    "SUBJ-001", "PC", "BASELINE"
  )

  mock_sdtm <- list(
    domains = list(pc = mock_pc)
  )
  class(mock_sdtm) <- c("sdtm", "list")

  expectation <- c(2, 2, 2, 2, 2, 0, 0, -2, 2.5, NA)

  result <- guess_ntime(mock_sdtm)

  # Test results
  expect_equal(nrow(result), 10)
  expect_equal(result$PCTPT, mock_pc$PCTPT)
  expect_equal(result$NTIME, expectation)
})


test_that("guess_ntime handles ISO 8601 dates with a warning", {
  # Create a mock SDTM object with ISO 8601 dates in PCTPT
  mock_pc <- data.frame(
    USUBJID = rep("SUBJ-001", 3),
    DOMAIN = rep("PC", 3),
    PCTPT = c(
      "2022-01-01", # ISO date
      "2022-01-01T12:00", # ISO datetime
      "HOUR 3" # Regular time
    )
  )

  mock_sdtm <- list(
    domains = list(pc = mock_pc)
  )
  class(mock_sdtm) <- c("sdtm", "list")

  # Should issue a warning
  expect_warning(
    result <- guess_ntime(mock_sdtm),
    "Some PCTPT entries are in ISO 8601 date format"
  )

  # Check results - ISO dates should have NA time
  expect_equal(nrow(result), 3)
  expect_true(is.na(result$NTIME[result$PCTPT == "2022-01-01"]))
  expect_true(is.na(result$NTIME[result$PCTPT == "2022-01-01T12:00"]))
  expect_equal(result$NTIME[result$PCTPT == "HOUR 3"], 3)
})


test_that("guess_ntime errors on missing PC domain", {
  # Create a mock SDTM object without PC domain
  mock_sdtm <- list(
    domains = list(dm = data.frame())
  )
  class(mock_sdtm) <- c("sdtm", "list")

  # Test that it errors correctly
  expect_error(
    guess_ntime(mock_sdtm),
    "PC domain not found in SDTM object"
  )
})


test_that("guess_ntime errors on missing PCTPT column", {
  # Create a mock SDTM object with PC domain but no PCTPT column
  mock_pc <- data.frame(
    USUBJID = "SUBJ-001",
    DOMAIN = "PC"
    # No PCTPT column
  )

  mock_sdtm <- list(
    domains = list(pc = mock_pc)
  )
  class(mock_sdtm) <- c("sdtm", "list")

  # Test that it errors correctly
  expect_error(
    guess_ntime(mock_sdtm),
    "PCTPT column not found in PC domain"
  )
})


test_that("guess_ntime handles additional predose variations", {
  # Create a mock SDTM object with various predose formats
  mock_pc <- data.frame(
    USUBJID = rep("SUBJ-001", 4),
    DOMAIN = rep("PC", 4),
    PCTPT = c(
      "Pre", # Just "Pre"
      "Pre-dose", # Pre with hyphen
      "Pre-dose 1hr", # Pre-dose with time
      "Pre-dose 1.5 hour" # Pre-dose with decimal time
    )
  )

  mock_sdtm <- list(
    domains = list(pc = mock_pc)
  )
  class(mock_sdtm) <- c("sdtm", "list")

  result <- guess_ntime(mock_sdtm)

  # Test results for predose variations
  expect_equal(result$NTIME[result$PCTPT == "Pre"], 0)
  expect_equal(result$NTIME[result$PCTPT == "Pre-dose"], 0)
  expect_equal(result$NTIME[result$PCTPT == "Pre-dose 1hr"], -1)
  expect_equal(result$NTIME[result$PCTPT == "Pre-dose 1.5 hour"], -1.5)
})
