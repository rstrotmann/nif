test_that("guess_ntime works", {
  # Create a proper SDTM object for testing
  pc_data <- tribble(
    ~PCTPT, ~time,
    "0.5h Pre Dose", -0.5,
    "6h Post Dose", 6,
    "PRE-DOSE", 0,
    "4H POST-DOSE", 4,
    "END OF VISIT", NA,
    "DAY1 - 4 HOURS POST ADMINISTRATION", 4,
    "PREDOSE", 0,
    "8 HOUR POST DOSE", 8,
    "2-4 HOUR POST DOSE", 4, ### caution: only the right end of the interval
    "12.0 HRS POST DOSE", 12,
    "PRE DOSE", 0
  )

  # Create a SDTM object with proper structure
  test_sdtm <- sdtm(list(pc = pc_data))

  # Test with the proper domain() function
  result <- guess_ntime(test_sdtm)

  # Test that the times are correctly extracted
  expect_equal(pc_data$time, result$NTIME)

  # Test error handling when PC domain is missing
  test_sdtm_no_pc <- sdtm(list(dm = data.frame(USUBJID = "TEST")))
  expect_error(guess_ntime(test_sdtm_no_pc), "PC domain not found in SDTM object")
})


test_that("sdtm() returns an sdtm object that is a list of domains", {
  temp <- examplinib_sad
  out <- sdtm(list(
    dm = domain(temp, "dm"),
    vs = domain(temp, "vs"),
    lb = domain(temp, "lb"),
    ex = domain(temp, "ex"),
    pc = domain(temp, "pc")
  ))

  expect_s3_class(out, "sdtm")
  expect_setequal(names(out), c("dm", "vs", "lb", "ex", "pc"))
  expect_true(is.data.frame(out$dm))
  expect_null(out$domains)
  expect_null(out$analyte_mapping)
  expect_null(out$metabolite_mapping)
  expect_null(out$parent_mapping)
  expect_null(out$time_mapping)
})


test_that("package sdtm data is a flat list of domains", {
  expect_s3_class(examplinib_sad, "sdtm")
  expect_true(is.data.frame(examplinib_sad$dm))
  expect_null(examplinib_sad$domains)
  expect_setequal(names(examplinib_sad), c("dm", "vs", "ex", "pc", "lb", "ts", "pp"))
  expect_setequal(names(examplinib_poc), c("dm", "vs", "ex", "pc", "lb", "ts", "pp"))
  expect_setequal(names(examplinib_fe), c("dm", "vs", "ex", "pc", "lb", "ts", "pp"))
  expect_setequal(names(examplinib_iv), c("dm", "vs", "ex", "pc", "lb", "ts"))
})


test_that("sdtm() lowercases domain list keys", {
  dm <- tibble::tribble(
      ~USUBJID, ~DOMAIN,
    "SUBJ-001",    "DM"
  )
  ts <- tibble::tribble(
    ~TSPARMCD,            ~TSVAL,
      "TITLE", "Food effect study"
  )
  out <- sdtm(list(DM = dm, TS = ts))

  expect_equal(names(out), c("dm", "ts"))
  expect_true(has_domain(out, "dm"))
  expect_true(has_domain(out, "ts"))
  expect_equal(trial_title(out), "Food effect study")
})


test_that("sdtm() rejects a data frame, NULL, and non-list input", {
  expect_error(sdtm(mtcars), "Input must be a list of data frames!")
  expect_error(sdtm(NULL), "Input must be a list of data frames!")
  expect_error(sdtm("dm"), "Input must be a list of data frames!")
})


test_that("sdtm() rejects list elements that are not data frames", {
  dm <- tibble::tribble(
      ~USUBJID, ~DOMAIN,
    "SUBJ-001",    "DM"
  )

  expect_error(
    sdtm(list(dm = dm, pc = 1)),
    "Input is not a data frame: pc"
  )
  expect_error(
    sdtm(list(dm = dm, pc = 1, vs = "x")),
    "Input is not a data frame: pc and vs"
  )
})


test_that("sdtm summary", {
  expect_no_error(summary(examplinib_sad))
  expect_output(print(summary(examplinib_sad)))
})


test_that("print.sdtm returns the sdtm object invisibly", {
  output <- capture.output({
    result <- print(examplinib_sad)
  })

  expect_s3_class(result, "sdtm")
  expect_identical(result, examplinib_sad)
  expect_false(inherits(result, "summary_sdtm"))
  expect_true(any(grepl("SDTM data summary", output)))
})


test_that("print.sdtm shows the summary banner and study details", {
  output <- paste(capture.output(print(examplinib_sad)), collapse = "\n")

  expect_match(output, "SDTM data summary")
  expect_match(output, "Study 2023000001")
  expect_match(output, "Data disposition:")
  expect_match(output, "Treatments:")
  expect_match(output, "PK analytes:")
  expect_match(output, "Hash:")
})


test_that("print.sdtm output matches print of summary", {
  sdtm_out <- capture.output(print(examplinib_sad))
  summary_out <- capture.output(print(summary(examplinib_sad)))
  expect_equal(sdtm_out, summary_out)
})


test_that("print.sdtm omits treatments when EX is absent", {
  obj <- sdtm(list(
    dm = tibble::tribble(
      ~USUBJID, ~DOMAIN, ~STUDYID, ~ACTARMCD, ~ACTARM,
      "001",    "DM",    "S1",     "A",       "Arm A"
    )
  ))

  output <- paste(capture.output(print(obj)), collapse = "\n")
  expect_match(output, "SDTM data summary")
  expect_match(output, "Study S1")
  expect_false(grepl("Treatments:", output))
})


test_that("print.sdtm handles an empty sdtm object", {
  empty <- sdtm(list())
  output <- capture.output({
    result <- print(empty)
  })

  expect_s3_class(result, "sdtm")
  expect_identical(result, empty)
  expect_true(any(grepl("SDTM data summary", output)))
  expect_true(any(grepl("(empty)", output, fixed = TRUE)))
})


test_that("suggest_sdtm works", {
  suppressMessages(
    expect_message(suggest(examplinib_sad))
  )
})


test_that("suggest throws error when required domains are missing", {
  # Create test data with missing PC domain
  skip_if_not_installed("nif")
  # Import function if possible
  if (!exists("suggest", envir = .GlobalEnv)) {
    tryCatch(
      {
        # Try to make function available
        library(nif)
      },
      error = function(e) {
        skip("Package nif functions not available")
      }
    )
  }

  test_data <- sdtm(list(
      dm = data.frame(USUBJID = c("SUBJ-001"), DOMAIN = "DM"),
      ex = data.frame(USUBJID = c("SUBJ-001"), DOMAIN = "EX", EXTRT = "TEST")
    )
  )

  expect_error(
    suggest(test_data),
    "Expected domain missing in sdtm object: pc"
  )

  # Create test data with missing EX domain
  test_data <- sdtm(list(
      dm = data.frame(USUBJID = c("SUBJ-001"), DOMAIN = "DM"),
      pc = data.frame(USUBJID = c("SUBJ-001"), DOMAIN = "PC", PCTEST = "TEST", PCTESTCD = "TEST")
    )
  )

  expect_error(
    suggest(test_data),
    "Expected domain missing in sdtm object: ex"
  )
})


test_that("subjects, analytes, treatments, doses works for sdtm", {
  expect_s3_class(subjects(examplinib_poc), "data.frame")
  expect_type(analytes(examplinib_poc), "character")
  expect_type(treatments(examplinib_poc), "character")
  expect_type(doses(examplinib_poc), "double")
})


test_that("filter_subject works", {
  expect_s3_class(
    filter_subject(examplinib_poc, subjects(examplinib_poc)[1, 1]),
    "sdtm"
  )

  expect_s3_class(
    filter_subject(examplinib_poc, subjects(examplinib_poc)[1:3, 1]),
    "sdtm"
  )
})


test_that("derive_sld works", {
  tr <- tribble(
    ~DOMAIN, ~USUBJID, ~TRTESTCD, ~TRSTRESN, ~TRDTC,
    "TR",    1,        "LDIAM",   1,         "2025-02-25T08:00",
    "TR",    1,        "LDIAM",   1.5,       "2025-02-25T08:00",
    "TR",    1,        "LDIAM",   0.5,       "2025-02-25T08:00"
  )

  sdtm <- sdtm(list(tr = tr))
  expect_no_error(
    derive_sld(sdtm, testcd = "LDIAM", observation_filter = "TRUE") %>%
      domain("tr")
  )
})


test_that("derive_sld works with TR containing TRTEST", {
  tr <- tribble(
    ~DOMAIN, ~USUBJID, ~TRTEST,            ~TRTESTCD, ~TRSTRESN, ~TRDTC,
    "TR",    1,        "Longest diameter", "LDIAM",   1,         "2025-02-25T08:00",
    "TR",    1,        "Longest diameter", "LDIAM",   1.5,       "2025-02-25T08:00",
    "TR",    1,        "Longest diameter", "LDIAM",   0.5,       "2025-02-25T08:00"
  )

  sdtm <- sdtm(list(tr = tr))
  expect_no_error(
    derive_sld(sdtm, testcd = "LDIAM", observation_filter = "TRUE") %>%
      domain("tr")
  )
})


test_that("derive_sld works with multiple diagnostic methods", {
  tr <- tribble(
    ~DOMAIN, ~USUBJID, ~TRMETHOD, ~TRTESTCD, ~TRSTRESN, ~TRDTC,
    "TR", 1, "CT", "LDIAM", 1, "2025-02-25T08:00",
    "TR", 1, "CT", "LDIAM", 1.5, "2025-02-25T08:00",
    "TR", 1, "CT", "LDIAM", 0.5, "2025-02-25T08:00",
    "TR", 1, "MRT", "LDIAM", 1, "2025-02-25T08:00",
    "TR", 1, "MRT", "LDIAM", 1.5, "2025-02-25T08:00",
    "TR", 1, "MRT", "LDIAM", 0.5, "2025-02-25T08:00"
  )

  sdtm <- sdtm(list(tr = tr))
  expect_no_error(
    derive_sld(sdtm, testcd = "LDIAM", observation_filter = "TRUE") %>%
      domain("tr")
  )
})


test_that("guess_ntime warns about ISO 8601 date formats", {
  pc_data_with_dates <- tibble::tribble(
    ~USUBJID, ~PCTPT, ~PCSTRESN,
    "SUBJ-001", "2023-10-15", 120.5, # ISO 8601 date
    "SUBJ-001", "20231015", 125.2, # ISO 8601 basic date
    "SUBJ-001", "2023-10", 130.1, # ISO 8601 year-month
    "SUBJ-001", "PREDOSE", 0.1, # Normal PCTPT
    "SUBJ-001", "4H POST-DOSE", 190.5, # Normal PCTPT
    "SUBJ-001", "8 HOUR POST DOSE", 210.3 # Normal PCTPT
  )

  # Create a SDTM object with the test data
  test_sdtm_with_dates <- sdtm(list(pc = pc_data_with_dates))

  # Test that the function warns about ISO 8601 dates
  expect_warning(
    result <- guess_ntime(test_sdtm_with_dates),
    "ISO 8601 date format"
  )

  # Check that the function still produces results for non-date PCTPT values
  expect_equal(result$NTIME[result$PCTPT == "PREDOSE"], 0)
  expect_equal(result$NTIME[result$PCTPT == "4H POST-DOSE"], 4)
  expect_equal(result$NTIME[result$PCTPT == "8 HOUR POST DOSE"], 8)

  # Check that ISO 8601 date values have NA for NTIME (since they don't contain time info)
  expect_true(is.na(result$NTIME[result$PCTPT == "2023-10-15"]))
  expect_true(is.na(result$NTIME[result$PCTPT == "20231015"]))
  expect_true(is.na(result$NTIME[result$PCTPT == "2023-10"]))
})
