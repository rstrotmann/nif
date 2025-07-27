# Test basic functionality
test_that("summary.nif returns the correct class and structure", {
  # Test with standard example data
  s <- summary(examplinib_poc_nif)

  # Check class
  expect_s3_class(s, "summary_nif")

  # Check structure - essential components
  expect_true(all(c("nif", "subjects", "n_subj", "n_males", "n_females",
                    "n_obs", "analytes", "drugs", "dose_levels",
                    "administration_duration") %in% names(s)))

  # Check that original NIF is preserved
  expect_identical(s$nif, examplinib_poc_nif)

  # Check that counts match
  expect_equal(s$n_subj, nrow(s$subjects))
  expect_equal(s$n_analytes, length(s$analytes))
})


# Test with missing columns
test_that("summary.nif errors appropriately with missing required columns", {
  # Missing DV
  invalid_nif <- new_nif(data.frame(
    ID = 1,
    TIME = 0,
    AMT = 100,
    EVID = 1
  ))

  expect_error(
    summary(invalid_nif),
    "missing required fields: CMT, DOSE and DV")
})


# Test sex calculations
test_that("summary.nif calculates sex distribution correctly", {
  # Create a NIF with known sex distribution
  test_nif <- examplinib_poc_nif %>%
    filter(ID %in% 1:10) %>%
    mutate(SEX = ifelse(ID <= 6, 0, 1))  # 6 males, 4 females

  s <- summary(test_nif)

  expect_equal(s$n_males, 6)
  expect_equal(s$n_females, 4)

  # Check output formatting
  output <- capture.output(print(s))
  expect_true(any(grepl("male     6   60", output)))
  expect_true(any(grepl("female   4   40", output)))
})


# Test with no sex data
test_that("summary.nif handles missing sex data correctly", {
  # Create a NIF without SEX column
  test_nif <- examplinib_poc_nif %>%
    filter(ID %in% 1:5) %>%
    select(-SEX)

  s <- summary(test_nif)

  expect_equal(s$n_males, 0)
  expect_equal(s$n_females, 0)

  # Check output formatting
  output <- capture.output(print(s))
  expect_true(any(grepl("male     0   0", output)))
  expect_true(any(grepl("female   0   0", output)))
  expect_true(any(grepl("NA       5   100", output)))
})


# Test renal function classification
test_that("summary.nif handles renal function classification correctly", {
  # Create a NIF with known renal function values
  test_nif <- examplinib_poc_nif %>%
    filter(ID %in% 1:4) %>%
    mutate(BL_CRCL = case_when(
      ID == 1 ~ 95,  # normal
      ID == 2 ~ 75,  # mild
      ID == 3 ~ 45,  # moderate
      ID == 4 ~ 20   # severe
    ))

  s <- summary(test_nif)

  # Check that renal function is calculated
  expect_false(is.null(s$renal_function))

  # Check counts by class
  rf_classes <- s$renal_function %>%
    # arrange(CLASS) %>%
    pull(N)
  rf_names <- s$renal_function %>%
    # arrange(CLASS) %>%
    pull(CLASS)

  expect_equal(rf_names, c("normal", "mild", "moderate", "severe"))
  expect_equal(rf_classes, c(1, 1, 1, 1))

  # Check output formatting
  output <- capture.output(print(s))
  expect_true(any(grepl("Renal impairment class:", output)))
})


# Test without renal function data
test_that("summary.nif handles missing renal function data correctly", {
  # Create a NIF without BL_CRCL column
  test_nif <- examplinib_poc_nif %>%
    filter(ID %in% 1:5) %>%
    select(-matches("BL_CRCL"))

  s <- summary(test_nif)

  expect_null(s$renal_function)

  # Check output doesn't contain renal function section
  output <- capture.output(print(s))
  expect_false(any(grepl("Renal impairment class:", output)))
})


# Test hepatic function (ODWG) classification
test_that("summary.nif handles hepatic function classification correctly", {
  # Create a NIF with known ODWG values
  test_nif <- examplinib_poc_nif %>%
    filter(ID %in% 1:4) %>%
    mutate(BL_ODWG = case_when(
      ID == 1 ~ "normal",
      ID == 2 ~ "mild",
      ID == 3 ~ "moderate",
      ID == 4 ~ "severe"
    ))

  s <- summary(test_nif)

  # Check that ODWG is calculated
  expect_false(is.null(s$odwg))

  # Check counts by class
  odwg_classes <- s$odwg %>%
    pull(N)
  odwg_names <- s$odwg %>%
    pull(CLASS)

  expect_equal(odwg_names, c("normal", "mild", "moderate", "severe"))
  expect_equal(odwg_classes, c(1, 1, 1, 1))

  # Check output formatting
  output <- capture.output(print(s))
  expect_true(any(grepl("NCI ODWG hepatic impairment class:", output)))
})


# Test subjects per dose level
test_that("summary.nif correctly counts subjects per dose level", {
  s <- summary(examplinib_poc_nif)

  # Check that dose levels are calculated
  expect_false(is.null(s$dose_levels))

  # Check that it matches with the direct calculation
  direct_count <- dose_levels(examplinib_poc_nif)
  expect_equal(s$dose_levels$N, direct_count$N)
})


# Test administration duration
test_that("summary.nif correctly calculates administration duration", {
  s <- summary(examplinib_poc_nif)

  # Check that administration duration is calculated
  expect_false(is.null(s$administration_duration))

  # Check that it has the correct columns
  expect_true(all(c("PARENT", "min", "max", "mean", "median") %in%
                 names(s$administration_duration)))

  # Check that the direct calculation matches
  direct_calc <- administration_summary(examplinib_poc_nif)
  expect_equal(s$administration_duration, direct_calc)
})


# Test plot method
test_that("plot.summary_nif produces plots", {
  s <- summary(examplinib_poc_nif)

  # Test plotting
  p <- plot(s, baseline = TRUE, analytes = TRUE)

  # Plot should return a list of ggplot objects
  expect_type(p, "list")
  expect_true(length(p) > 0)

  # Each element should be a ggplot object
  for (plot in p) {
    if (!is.null(plot)) {
      expect_s3_class(plot, "ggplot")
    }
  }
})


# Test that the weight column typo fix works
test_that("plot.summary_nif handles weight correctly", {
  # Create a NIF with WEIGHT and SEX columns
  test_nif <- examplinib_poc_nif %>%
    filter(ID %in% 1:10) %>%
    mutate(WEIGHT = 70, SEX = ifelse(ID <= 5, 0, 1))

  s <- summary(test_nif)

  # Test plotting - this would have failed with the WEIGTH typo
  p <- plot(s, baseline = TRUE, analytes = FALSE)

  # Should have a WT_SEX plot
  expect_true("WT_SEX" %in% names(p))
  expect_s3_class(p$WT_SEX, "ggplot")
})

