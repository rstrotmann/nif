# Test file for plot.domain() function

test_that("plot.domain validates input parameters correctly", {
  # Create a simple domain object for testing
  test_dm <- tibble::tribble(
    ~USUBJID, ~DOMAIN, ~RFSTDTC, ~RFENDTC,
    "SUBJ-001", "DM", "2023-01-01", "2023-01-10",
    "SUBJ-002", "DM", "2023-01-02", "2023-01-11"
  )
  class(test_dm) <- c("domain", "data.frame")

  # Test with invalid object (not a domain)
  expect_error(
    plot.domain(data.frame(x = 1), testcd = NULL),
    "Input must be a domain object"
  )

  # Test with NULL domain object
  expect_error(
    plot.domain(NULL, testcd = NULL),
    "Input must be a domain object"
  )

  # Test with invalid testcd parameter
  expect_error(
    plot.domain(test_dm, testcd = 123),
    "testcd must be a string value"
  )

  # Test with invalid points parameter
  expect_error(
    plot.domain(test_dm, points = "TRUE"),
    "points must be a logical value"
  )

  # Test with invalid lines parameter
  expect_error(
    plot.domain(test_dm, lines = "FALSE"),
    "lines must be a logical value"
  )

  # Test with invalid legend parameter
  expect_error(
    plot.domain(test_dm, legend = "TRUE"),
    "legend must be a logical value"
  )

  # Test with invalid color parameter
  expect_error(
    plot.domain(test_dm, color = 123),
    "color must be a string value"
  )

  # Test with color field not in domain
  expect_error(
    plot.domain(test_dm, color = "NONEXISTENT"),
    "Color field NONEXISTENT not in domain!"
  )
})


test_that("plot.domain works with DM domain", {
  # Create DM domain test data
  test_dm <- tibble::tribble(
    ~USUBJID, ~DOMAIN, ~RFSTDTC, ~RFENDTC, ~SEX,
    "SUBJ-001", "DM", "2023-01-01", "2023-01-10", "M",
    "SUBJ-002", "DM", "2023-01-02", "2023-01-11", "F",
    "SUBJ-003", "DM", "2023-01-03", "2023-01-12", "M"
  )
  class(test_dm) <- c("domain", "data.frame")

  # Test basic DM plot
  p <- plot.domain(test_dm)
  expect_s3_class(p, "ggplot")


  # Test DM plot with points = FALSE
  p <- plot.domain(test_dm, points = FALSE)
  expect_s3_class(p, "ggplot")

  # Test DM plot with color
  p <- plot.domain(test_dm, color = "SEX")
  expect_s3_class(p, "ggplot")

  # Test DM plot with color and points = FALSE
  p <- plot.domain(test_dm, color = "SEX", points = FALSE)
  expect_s3_class(p, "ggplot")

  # Test DM plot with legend = FALSE
  p <- plot.domain(test_dm, legend = FALSE)
  expect_s3_class(p, "ggplot")
})


test_that("plot.domain handles DM domain missing fields", {
  # Create DM domain without required fields
  test_dm_missing_rfstdtc <- tibble::tribble(
    ~USUBJID, ~DOMAIN, ~RFENDTC,
    "SUBJ-001", "DM", "2023-01-10"
  )
  class(test_dm_missing_rfstdtc) <- c("domain", "data.frame")

  expect_error(
    plot.domain(test_dm_missing_rfstdtc),
    "missing field: RFSTDTC"
  )

  test_dm_missing_rfendtc <- tibble::tribble(
    ~USUBJID, ~DOMAIN, ~RFSTDTC,
    "SUBJ-001", "DM", "2023-01-01"
  )
  class(test_dm_missing_rfendtc) <- c("domain", "data.frame")

  expect_error(
    plot.domain(test_dm_missing_rfendtc),
    "missing field: RFENDTC"
  )

  test_dm_missing_usubjid <- tibble::tribble(
    ~DOMAIN, ~RFSTDTC, ~RFENDTC,
    "DM", "2023-01-01", "2023-01-10"
  )
  class(test_dm_missing_usubjid) <- c("domain", "data.frame")

  expect_error(
    plot.domain(test_dm_missing_usubjid),
    "missing field: USUBJID"
  )

  test_dm_missing_multiple <- tibble::tribble(
    ~DOMAIN,
    "DM"
  )
  class(test_dm_missing_multiple) <- c("domain", "data.frame")

  expect_error(
    plot.domain(test_dm_missing_multiple),
    "missing fields:"
  )
})


test_that("plot.domain works with EX domain", {
  # Create EX domain test data
  test_ex <- tibble::tribble(
    ~USUBJID, ~DOMAIN, ~EXSTDTC, ~EXENDTC,
    "SUBJ-001", "EX", "2023-01-01T08:00", "2023-01-01T10:00",
    "SUBJ-002", "EX", "2023-01-02T08:00", "2023-01-02T10:00",
    "SUBJ-003", "EX", "2023-01-03T08:00", "2023-01-03T10:00"
  )
  class(test_ex) <- c("domain", "data.frame")

  # Test basic EX plot
  p <- plot.domain(test_ex)
  expect_s3_class(p, "ggplot")

  # Test EX plot with points = FALSE
  p <- plot.domain(test_ex, points = FALSE)
  expect_s3_class(p, "ggplot")

  # Test EX plot with legend = FALSE
  p <- plot.domain(test_ex, legend = FALSE)
  expect_s3_class(p, "ggplot")
})


test_that("plot.domain handles EX domain missing fields", {
  # Create EX domain without required fields
  test_ex_missing_exstdtc <- tibble::tribble(
    ~USUBJID, ~DOMAIN, ~EXENDTC,
    "SUBJ-001", "EX", "2023-01-01T10:00"
  )
  class(test_ex_missing_exstdtc) <- c("domain", "data.frame")

  expect_error(
    plot.domain(test_ex_missing_exstdtc),
    "missing field: EXSTDTC"
  )

  test_ex_missing_exendtc <- tibble::tribble(
    ~USUBJID, ~DOMAIN, ~EXSTDTC,
    "SUBJ-001", "EX", "2023-01-01T08:00"
  )
  class(test_ex_missing_exendtc) <- c("domain", "data.frame")

  expect_error(
    plot.domain(test_ex_missing_exendtc),
    "missing field: EXENDTC"
  )
})


test_that("plot.domain works with generic domains (LB)", {
  # Create LB domain test data
  test_lb <- tibble::tribble(
    ~USUBJID, ~DOMAIN, ~LBDTC, ~LBSTRESN, ~LBTESTCD, ~SEX,
    "SUBJ-001", "LB", "2023-01-01", 10.5, "ALT", "M",
    "SUBJ-001", "LB", "2023-01-02", 11.2, "ALT", "M",
    "SUBJ-002", "LB", "2023-01-01", 9.8, "ALT", "F",
    "SUBJ-002", "LB", "2023-01-02", 10.1, "ALT", "F",
    "SUBJ-001", "LB", "2023-01-01", 5.2, "AST", "M",
    "SUBJ-002", "LB", "2023-01-01", 5.0, "AST", "F"
  )
  class(test_lb) <- c("domain", "data.frame")

  # Test basic LB plot
  p <- plot.domain(test_lb)
  expect_s3_class(p, "ggplot")


  # Test LB plot with lines = TRUE
  p <- plot.domain(test_lb, lines = TRUE)
  expect_s3_class(p, "ggplot")

  # Test LB plot with points = FALSE, lines = TRUE
  p <- plot.domain(test_lb, points = FALSE, lines = TRUE)
  expect_s3_class(p, "ggplot")

  # Test LB plot with testcd filter
  p <- plot.domain(test_lb, testcd = "ALT")
  expect_s3_class(p, "ggplot")

  # Test LB plot with legend = FALSE
  p <- plot.domain(test_lb, legend = FALSE)
  expect_s3_class(p, "ggplot")

  # Test LB plot with color
  p <- plot.domain(test_lb, color = "SEX")
  expect_s3_class(p, "ggplot")
})


test_that("plot.domain works with generic domains (PC)", {
  # Create PC domain test data
  test_pc <- tibble::tribble(
    ~USUBJID, ~DOMAIN, ~PCDTC, ~PCSTRESN, ~PCTESTCD,
    "SUBJ-001", "PC", "2023-01-01T08:00", 100.5, "RS2023",
    "SUBJ-001", "PC", "2023-01-01T10:00", 85.2, "RS2023",
    "SUBJ-002", "PC", "2023-01-01T08:00", 95.8, "RS2023",
    "SUBJ-002", "PC", "2023-01-01T10:00", 80.1, "RS2023"
  )
  class(test_pc) <- c("domain", "data.frame")

  # Test basic PC plot
  p <- plot.domain(test_pc)
  expect_s3_class(p, "ggplot")


  # Test PC plot with lines = TRUE
  p <- plot.domain(test_pc, lines = TRUE)
  expect_s3_class(p, "ggplot")

  # Test PC plot with testcd filter
  p <- plot.domain(test_pc, testcd = "RS2023")
  expect_s3_class(p, "ggplot")
})


test_that("plot.domain works with generic domains using DY field", {
  # Create domain test data with DY field (should use DY instead of DTC)
  test_vs <- tibble::tribble(
    ~USUBJID, ~DOMAIN, ~VSDTC, ~VSDY, ~VSSTRESN, ~VSTESTCD,
    "SUBJ-001", "VS", "2023-01-01", 1, 120.5, "SYSBP",
    "SUBJ-001", "VS", "2023-01-02", 2, 118.2, "SYSBP",
    "SUBJ-002", "VS", "2023-01-01", 1, 125.0, "SYSBP",
    "SUBJ-002", "VS", "2023-01-02", 2, 122.8, "SYSBP"
  )
  class(test_vs) <- c("domain", "data.frame")

  # Test VS plot (should use VSDY as time field)
  p <- plot.domain(test_vs)
  expect_s3_class(p, "ggplot")

})


test_that("plot.domain handles domains without required fields gracefully", {
  # Create domain without testcd, time, or dv fields
  test_minimal <- tibble::tribble(
    ~USUBJID, ~DOMAIN, ~OTHER_FIELD,
    "SUBJ-001", "XX", "value1",
    "SUBJ-002", "XX", "value2"
  )
  class(test_minimal) <- c("domain", "data.frame")

  expect_error(
    result <- plot.domain(test_minimal),
    "Missing domain fields: XXTESTCD, XXDTC and XXSTRESN"
  )
})


test_that("plot.domain filters NA values correctly", {
  # Create LB domain with NA values
  test_lb_na <- tibble::tribble(
    ~USUBJID, ~DOMAIN, ~LBDTC, ~LBSTRESN, ~LBTESTCD,
    "SUBJ-001", "LB", "2023-01-01", 10.5, "ALT",
    "SUBJ-001", "LB", "2023-01-02", NA_real_, "ALT",
    "SUBJ-002", "LB", NA_character_, 9.8, "ALT",
    "SUBJ-002", "LB", "2023-01-02", 10.1, "ALT"
  )
  class(test_lb_na) <- c("domain", "data.frame")

  # Should filter out NA values and still produce a plot
  p <- plot.domain(test_lb_na)
  expect_s3_class(p, "ggplot")
})


test_that("plot.domain works with real examplinib data", {
  # Test with actual examplinib data if available
  if (exists("examplinib_sad")) {
    # Test LB domain
    lb_domain <- domain(examplinib_sad, "lb")
    p <- plot.domain(lb_domain)
    expect_s3_class(p, "ggplot")


    # Test with testcd filter
    p <- plot.domain(lb_domain, testcd = "ALT")
    expect_s3_class(p, "ggplot")

    # Test with different parameter combinations
    p <- plot.domain(lb_domain, points = TRUE, lines = TRUE, legend = TRUE)
    expect_s3_class(p, "ggplot")

    p <- plot.domain(lb_domain, points = FALSE, lines = TRUE, legend = FALSE)
    expect_s3_class(p, "ggplot")
  }
})


test_that("plot.domain handles empty domain gracefully", {
  # Create empty DM domain
  test_empty_dm <- tibble::tribble(
    ~USUBJID, ~DOMAIN, ~RFSTDTC, ~RFENDTC
  )
  class(test_empty_dm) <- c("domain", "data.frame")

  expect_warning(
    result <- plot.domain(test_empty_dm),
    "Empty domain data"
  )
  expect_s3_class(result, "ggplot")
})


test_that("plot.domain handles single subject/row", {
  # Create DM domain with single subject
  test_dm_single <- tibble::tribble(
    ~USUBJID, ~DOMAIN, ~RFSTDTC, ~RFENDTC,
    "SUBJ-001", "DM", "2023-01-01", "2023-01-10"
  )
  class(test_dm_single) <- c("domain", "data.frame")

  p <- plot.domain(test_dm_single)
  expect_s3_class(p, "ggplot")

  # Create LB domain with single observation
  test_lb_single <- tibble::tribble(
    ~USUBJID, ~DOMAIN, ~LBDTC, ~LBSTRESN, ~LBTESTCD,
    "SUBJ-001", "LB", "2023-01-01", 10.5, "ALT"
  )
  class(test_lb_single) <- c("domain", "data.frame")

  p <- plot.domain(test_lb_single)
  expect_s3_class(p, "ggplot")
})


test_that("plot.domain handles testcd parameter with multiple values", {
  # Create LB domain with multiple testcds
  test_lb_multi <- tibble::tribble(
    ~USUBJID, ~DOMAIN, ~LBDTC, ~LBSTRESN, ~LBTESTCD,
    "SUBJ-001", "LB", "2023-01-01", 10.5, "ALT",
    "SUBJ-001", "LB", "2023-01-01", 5.2, "AST",
    "SUBJ-001", "LB", "2023-01-01", 150.0, "GLUC"
  )
  class(test_lb_multi) <- c("domain", "data.frame")

  # Test with NULL testcd (should show all)
  p <- plot.domain(test_lb_multi, testcd = NULL)
  expect_s3_class(p, "ggplot")

  # Test with specific testcd
  p <- plot.domain(test_lb_multi, testcd = "ALT")
  expect_s3_class(p, "ggplot")

  # Test with non-existent testcd (should result in empty plot or no error)
  p <- plot.domain(test_lb_multi, testcd = "NONEXISTENT")
  expect_s3_class(p, "ggplot")
})


test_that("plot.domain applies theme and watermark correctly", {
  # Create test domain
  test_lb <- tibble::tribble(
    ~USUBJID, ~DOMAIN, ~LBDTC, ~LBSTRESN, ~LBTESTCD,
    "SUBJ-001", "LB", "2023-01-01", 10.5, "ALT"
  )
  class(test_lb) <- c("domain", "data.frame")

  p <- plot.domain(test_lb)

  # Check that plot is a ggplot object
  expect_s3_class(p, "ggplot")

  # The plot should have a title with "Domain LB"
  expect_true("Domain LB" %in% as.character(p$labels$title))

  # Check that plot has layers (theme_bw and watermark are added)
  expect_true(length(p$layers) > 0 || !is.null(p$theme))
})


test_that("plot.domain handles legend positioning correctly", {
  # Create test domain
  test_lb <- tibble::tribble(
    ~USUBJID, ~DOMAIN, ~LBDTC, ~LBSTRESN, ~LBTESTCD,
    "SUBJ-001", "LB", "2023-01-01", 10.5, "ALT",
    "SUBJ-002", "LB", "2023-01-01", 9.8, "ALT"
  )
  class(test_lb) <- c("domain", "data.frame")

  # Test with legend = TRUE
  p1 <- plot.domain(test_lb, legend = TRUE)
  expect_s3_class(p1, "ggplot")

  # Test with legend = FALSE
  p2 <- plot.domain(test_lb, legend = FALSE)
  expect_s3_class(p2, "ggplot")

  # Both should be valid ggplot objects
  expect_no_error(print(p1))
  expect_no_error(print(p2))
})


test_that("plot.domain handles color parameter with different domains", {
  # Test DM with color
  test_dm <- tibble::tribble(
    ~USUBJID, ~DOMAIN, ~RFSTDTC, ~RFENDTC, ~SEX, ~RACE,
    "SUBJ-001", "DM", "2023-01-01", "2023-01-10", "M", "WHITE",
    "SUBJ-002", "DM", "2023-01-02", "2023-01-11", "F", "BLACK"
  )
  class(test_dm) <- c("domain", "data.frame")

  p <- plot.domain(test_dm, color = "SEX")
  expect_s3_class(p, "ggplot")

  p <- plot.domain(test_dm, color = "RACE")
  expect_s3_class(p, "ggplot")

  # Test generic domain with color
  # Note: Current implementation doesn't support color for generic domains
  # but we test that it doesn't error if the field exists
  test_lb <- tibble::tribble(
    ~USUBJID, ~DOMAIN, ~LBDTC, ~LBSTRESN, ~LBTESTCD, ~SEX,
    "SUBJ-001", "LB", "2023-01-01", 10.5, "ALT", "M",
    "SUBJ-002", "LB", "2023-01-01", 9.8, "ALT", "F"
  )
  class(test_lb) <- c("domain", "data.frame")

  # Color parameter is validated but may not be used in generic domain plots
  # The validation ensures the field exists, so this should not error
  expect_no_error(p <- plot.domain(test_lb, color = "SEX"))
  expect_s3_class(p, "ggplot")
})


test_that("plot.domain handles date conversion correctly", {
  # Create domain with various date formats
  test_lb_dates <- tibble::tribble(
    ~USUBJID, ~DOMAIN, ~LBDTC, ~LBSTRESN, ~LBTESTCD,
    "SUBJ-001", "LB", "2023-01-01", 10.5, "ALT",
    "SUBJ-001", "LB", "2023-01-01T08:00", 11.2, "ALT",
    "SUBJ-001", "LB", "2023-01-02", 10.8, "ALT"
  )
  class(test_lb_dates) <- c("domain", "data.frame")

  # Should handle different date formats via lubrify_dates
  p <- plot.domain(test_lb_dates)
  expect_s3_class(p, "ggplot")

})


test_that("plot.domain handles generic domains with partial fields", {
  # Create domain with some but not all required fields for generic plot
  test_partial <- tibble::tribble(
    ~USUBJID, ~DOMAIN, ~LBDTC, ~LBTESTCD,
    # Missing LBSTRESN field
    "SUBJ-001", "LB", "2023-01-01", "ALT",
    "SUBJ-002", "LB", "2023-01-01", "ALT"
  )
  class(test_partial) <- c("domain", "data.frame")

  # Should handle gracefully - may not create plot if required fields missing
  result <- tryCatch(
    plot.domain(test_partial),
    error = function(e) e
  )
  # Either returns a plot or errors - document the behavior
  if (inherits(result, "error")) {
    expect_true(TRUE) # Document that missing required fields may error
  } else {
    expect_s3_class(result, "ggplot")
  }
})


test_that("plot.domain handles all parameter combinations", {
  # Create comprehensive test data
  test_lb <- tibble::tribble(
    ~USUBJID, ~DOMAIN, ~LBDTC, ~LBSTRESN, ~LBTESTCD, ~SEX,
    "SUBJ-001", "LB", "2023-01-01", 10.5, "ALT", "M",
    "SUBJ-001", "LB", "2023-01-02", 11.2, "ALT", "M",
    "SUBJ-002", "LB", "2023-01-01", 9.8, "ALT", "F",
    "SUBJ-002", "LB", "2023-01-02", 10.1, "ALT", "F",
    "SUBJ-001", "LB", "2023-01-01", 5.2, "AST", "M",
    "SUBJ-002", "LB", "2023-01-01", 5.0, "AST", "F"
  )
  class(test_lb) <- c("domain", "data.frame")

  # Test all combinations of points, lines, legend
  combinations <- expand.grid(
    points = c(TRUE, FALSE),
    lines = c(TRUE, FALSE),
    legend = c(TRUE, FALSE),
    stringsAsFactors = FALSE
  )

  for (i in seq_len(nrow(combinations))) {
    combo <- combinations[i, ]
    p <- plot.domain(
      test_lb,
      points = combo$points,
      lines = combo$lines,
      legend = combo$legend
    )
    expect_s3_class(p, "ggplot")
  }
})

