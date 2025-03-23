
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
  test_sdtm <- new_sdtm(list(pc = pc_data))

  # Test with the proper domain() function
  result <- guess_ntime(test_sdtm)

  # Test that the times are correctly extracted
  expect_equal(pc_data$time, result$NTIME)

  # Test error handling when PC domain is missing
  test_sdtm_no_pc <- new_sdtm(list(dm = data.frame(USUBJID = "TEST")))
  expect_error(guess_ntime(test_sdtm_no_pc), "PC domain not found in SDTM object")
})


test_that("new_sdtm", {
  temp <- examplinib_sad
  expect_no_error(
    new_sdtm(list(
      dm = domain(temp, "dm"),
      vs = domain(temp, "vs"),
      lb = domain(temp, "lb"),
      ex = domain(temp, "ex"),
      pc = domain(temp, "pc")
    ))
  )
})


test_that("sdtm summary", {
  expect_no_error(summary(examplinib_sad))
  expect_output(print(summary(examplinib_sad)))
  test <- examplinib_fe
  test$analyte_mapping <- NULL
  expect_no_error(summary(test))
})


test_that("sdtm_summary works with metabolite mapping", {
  test <- examplinib_fe %>%
    add_time_mapping(
     "PREDOSE" = 0,
     "HOUR 0.5" = 0.5,
     "HOUR 1" = 1,
     "HOUR 1.5" = 1.5,
     "HOUR 2" = 2,
     "HOUR 3" = 3,
     "HOUR 4" = 4,
     "HOUR 6" = 6,
     "HOUR 8" = 8,
     "HOUR 10" = 10,
     "HOUR 12" = 12,
     "HOUR 24" = 24,
     "HOUR 48" = 48,
     "HOUR 72" = 72,
     "HOUR 96" = 96,
     "HOUR 144" = 144,
     "HOUR 168" = 168
    )
  expect_no_error(summary(test))
})


test_that("suggest works with consider_nif_auto", {
  suppressMessages(
    expect_message(
      suggest(examplinib_poc, consider_nif_auto = TRUE)
    )
  )
})


test_that("suggest_sdtm works", {
  suppressMessages(
    expect_message(suggest(examplinib_sad))
  )
})



test_that("subject_info works", {
  expect_type(
    subject_info(examplinib_poc, subjects(examplinib_poc)[1, "USUBJID"]),
    "list")
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
    "sdtm")

  expect_s3_class(
    filter_subject(examplinib_poc, subjects(examplinib_poc)[1:3, 1]),
    "sdtm")
})


test_that("derive_sld works", {
  tr <- tribble(
    ~DOMAIN, ~USUBJID, ~TRTESTCD, ~TRSTRESN, ~TRDTC,
    "TR",    1,        "LDIAM",   1,         "2025-02-25T08:00",
    "TR",    1,        "LDIAM",   1.5,       "2025-02-25T08:00",
    "TR",    1,        "LDIAM",   0.5,       "2025-02-25T08:00"
  )

  sdtm <- new_sdtm(list(tr = tr))
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

  sdtm <- new_sdtm(list(tr = tr))
  expect_no_error(
    derive_sld(sdtm, testcd = "LDIAM", observation_filter = "TRUE") %>%
      domain("tr")
  )
})


test_that("derive_sld works with multiple diagnostic methods", {
  tr <- tribble(
    ~DOMAIN, ~USUBJID, ~TRMETHOD, ~TRTESTCD, ~TRSTRESN, ~TRDTC,
    "TR",    1,        "CT",     "LDIAM",    1,         "2025-02-25T08:00",
    "TR",    1,        "CT",     "LDIAM",    1.5,       "2025-02-25T08:00",
    "TR",    1,        "CT",     "LDIAM",    0.5,       "2025-02-25T08:00",
    "TR",    1,        "MRT",    "LDIAM",    1,         "2025-02-25T08:00",
    "TR",    1,        "MRT",    "LDIAM",    1.5,       "2025-02-25T08:00",
    "TR",    1,        "MRT",    "LDIAM",    0.5,       "2025-02-25T08:00"
  )

  sdtm <- new_sdtm(list(tr = tr))
  expect_no_error(
    derive_sld(sdtm, testcd = "LDIAM", observation_filter = "TRUE") %>%
      domain("tr")
  )
})


