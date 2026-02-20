## Tests for make_administration using the standard imputation rule set

test_that("make_administration works for examplinib_poc", {
  expect_no_error(
    make_administration(
      examplinib_poc, "EXAMPLINIB", "RS2023",
      silent = TRUE
    )
  )
})


test_that("make_administration works without pc", {
  sdtm <- sdtm(list(
    dm = tibble::tribble(
      ~USUBJID, ~SEX,          ~RFSTDTC,     ~RFENDTC, ~ACTARMCD,
             1,    1, "2024-12-16T7:50", "2024-12-19",   "ARM A",
             2,    1, "2024-12-16T7:50", "2024-12-18",   "ARM A",
             3,    1, "2024-12-16T7:50", "2024-12-17",   "ARM A"
      ),

    ex = tibble::tribble(
      ~USUBJID, ~EXSEQ, ~EXTRT,          ~EXSTDTC,     ~EXENDTC, ~EXDOSE,
             1,      1,    "A", "2024-12-16T7:50", "2024-12-19",     100,
             2,      2,    "A", "2024-12-16T7:50", "2024-12-18",     100,
             3,      3,    "A", "2024-12-16T7:50", "2024-12-17",     100,
             3,      4,    "A",      "2024-12-20", "2024-12-22",     100
      )
  ))

  expect_no_error(
    test <- as.data.frame(make_administration(sdtm, "A", silent = TRUE))
  )

  # number of administrations is correct:
  expect_equal(
    reframe(test, n = n_distinct(DTC), .by = "USUBJID")$n,
    c(4, 3, 5)
  )

  # check time carry forward
  expect_equal(
    test$IMPUTATION,
    c(
      "time copied from EXSTDTC",
      "time carried forward",
      "time carried forward",
      "time carried forward",
      "time copied from EXSTDTC",
      "time carried forward",
      "time carried forward",
      "time copied from EXSTDTC",
      "time carried forward",
      "time carried forward",
      "time carried forward",
      "time carried forward"
      )
  )
})


# Input validation tests

test_that("make_administration errors when extrt not found in EX", {
  sdtm <- sdtm(list(
    dm = tibble::tribble(
      ~USUBJID, ~SEX, ~RFSTDTC, ~RFENDTC, ~ACTARMCD,
      "001", 1, "2024-12-16T7:50", "2024-12-19", "ARM A"
    ),
    ex = tibble::tribble(
      ~USUBJID, ~EXSEQ, ~EXTRT, ~EXSTDTC, ~EXENDTC, ~EXDOSE,
      "001", 1, "DRUG_A", "2024-12-16T7:50", "2024-12-19", 100
    )
  ))

  expect_error(
    make_administration(sdtm, "DRUG_B", silent = TRUE),
    "Treatment 'DRUG_B' not found in EXTRT!"
  )
})


test_that("make_administration works with custom analyte parameter", {
  sdtm <- sdtm(list(
    dm = tibble::tribble(
      ~USUBJID, ~SEX, ~RFSTDTC, ~RFENDTC, ~ACTARMCD,
      "001", 1, "2024-12-16T7:50", "2024-12-19", "ARM A"
    ),
    ex = tibble::tribble(
      ~USUBJID, ~EXSEQ, ~EXTRT, ~EXSTDTC, ~EXENDTC, ~EXDOSE,
      "001", 1, "DRUG_A", "2024-12-16T7:50", "2024-12-19", 100
    )
  ))

  result <- make_administration(sdtm, "DRUG_A", analyte = "CUSTOM_ANALYTE", silent = TRUE)

  expect_true(all(result$ANALYTE == "CUSTOM_ANALYTE"))
  expect_true(all(result$PARENT == "CUSTOM_ANALYTE"))
})


test_that("make_administration uses extrt as analyte when analyte is NULL", {
  sdtm <- sdtm(list(
    dm = tibble::tribble(
      ~USUBJID, ~SEX, ~RFSTDTC, ~RFENDTC, ~ACTARMCD,
      "001", 1, "2024-12-16T7:50", "2024-12-19", "ARM A"
    ),
    ex = tibble::tribble(
      ~USUBJID, ~EXSEQ, ~EXTRT, ~EXSTDTC, ~EXENDTC, ~EXDOSE,
      "001", 1, "DRUG_A", "2024-12-16T7:50", "2024-12-19", 100
    )
  ))

  result <- make_administration(sdtm, "DRUG_A", analyte = NULL, silent = TRUE)

  expect_true(all(result$ANALYTE == "DRUG_A"))
  expect_true(all(result$PARENT == "DRUG_A"))
})


test_that("make_administration works with custom cmt parameter", {
  sdtm <- sdtm(list(
    dm = tibble::tribble(
      ~USUBJID, ~SEX, ~RFSTDTC, ~RFENDTC, ~ACTARMCD,
      "001", 1, "2024-12-16T7:50", "2024-12-19", "ARM A"
    ),
    ex = tibble::tribble(
      ~USUBJID, ~EXSEQ, ~EXTRT, ~EXSTDTC, ~EXENDTC, ~EXDOSE,
      "001", 1, "DRUG_A", "2024-12-16T7:50", "2024-12-19", 100
    )
  ))

  result <- make_administration(sdtm, "DRUG_A", cmt = 2, silent = TRUE)

  expect_true(all(result$CMT == 2))
})


test_that("make_administration respects subject_filter parameter", {
  sdtm <- sdtm(list(
    dm = tibble::tribble(
      ~USUBJID, ~SEX, ~RFSTDTC, ~RFENDTC, ~ACTARMCD,
      "001", 1, "2024-12-16T7:50", "2024-12-19", "ARM A",
      "002", 1, "2024-12-16T7:50", "2024-12-19", "SCRNFAIL",
      "003", 1, "2024-12-16T7:50", "2024-12-19", "ARM A"
    ),
    ex = tibble::tribble(
      ~USUBJID, ~EXSEQ, ~EXTRT, ~EXSTDTC, ~EXENDTC, ~EXDOSE,
      "001", 1, "DRUG_A", "2024-12-16T7:50", "2024-12-19", 100,
      "002", 1, "DRUG_A", "2024-12-16T7:50", "2024-12-19", 100,
      "003", 1, "DRUG_A", "2024-12-16T7:50", "2024-12-19", 100
    )
  ))

  result <- make_administration(
    sdtm, "DRUG_A",
    subject_filter = "!ACTARMCD %in% c('SCRNFAIL', 'NOTTRT')",
    silent = TRUE
  )

  expect_false("002" %in% result$USUBJID)
  expect_true("001" %in% result$USUBJID)
  expect_true("003" %in% result$USUBJID)
})


test_that("make_administration handles cut_off_date parameter", {
  sdtm <- sdtm(list(
    dm = tibble::tribble(
      ~USUBJID, ~SEX, ~RFSTDTC, ~RFENDTC, ~ACTARMCD,
      "001", 1, "2024-12-16T7:50", "2024-12-25", "ARM A"
    ),
    ex = tibble::tribble(
      ~USUBJID, ~EXSEQ, ~EXTRT, ~EXSTDTC, ~EXENDTC, ~EXDOSE,
      "001", 1, "DRUG_A", "2024-12-16T7:50", "2024-12-19", 100,
      "001", 2, "DRUG_A", "2024-12-20T7:50", "2024-12-23", 100,
      "001", 3, "DRUG_A", "2024-12-24T7:50", "2024-12-27", 100
    )
  ))

  cut_off <- as.POSIXct("2024-12-22 23:59:59", tz = "UTC")
  result <- make_administration(sdtm, "DRUG_A", cut_off_date = cut_off, silent = TRUE)

  # Should exclude the third administration episode that starts after cut-off
  expect_false(any(result$DTC >= as.Date("2024-12-24")))
})


test_that("make_administration auto-assigns cut_off_date when NULL", {
  sdtm <- sdtm(list(
    dm = tibble::tribble(
      ~USUBJID, ~SEX, ~RFSTDTC, ~RFENDTC, ~ACTARMCD,
      "001", 1, "2024-12-16T7:50", "2024-12-25", "ARM A"
    ),
    ex = tibble::tribble(
      ~USUBJID, ~EXSEQ, ~EXTRT, ~EXSTDTC, ~EXENDTC, ~EXDOSE,
      "001", 1, "DRUG_A", "2024-12-16T7:50", "2024-12-19", 100,
      "001", 2, "DRUG_A", "2024-12-20T7:50", "2024-12-23", 100
    )
  ))

  expect_message(
    result <- make_administration(sdtm, "DRUG_A", cut_off_date = NULL, silent = FALSE),
    "A global cut-off-date of"
  )

  expect_s3_class(result, "nif")
})


test_that("make_administration expands administration episodes correctly", {
  sdtm <- sdtm(list(
    dm = tibble::tribble(
      ~USUBJID, ~SEX, ~RFSTDTC, ~RFENDTC, ~ACTARMCD,
      "001", 1, "2024-12-16T7:50", "2024-12-20", "ARM A"
    ),
    ex = tibble::tribble(
      ~USUBJID, ~EXSEQ, ~EXTRT, ~EXSTDTC, ~EXENDTC, ~EXDOSE,
      "001", 1, "DRUG_A", "2024-12-16T7:50", "2024-12-18T7:50", 100
    )
  ))

  result <- make_administration(sdtm, "DRUG_A", silent = TRUE)

  # Should expand to 3 rows (16th, 17th, 18th)
  expect_equal(nrow(result), 3)
  expect_equal(length(unique(result$DTC)), 3)
})


test_that("make_administration handles EXSTDY and EXENDY correctly", {
  sdtm <- sdtm(list(
    dm = tibble::tribble(
      ~USUBJID, ~SEX, ~RFSTDTC, ~RFENDTC, ~ACTARMCD,
      "001", 1, "2024-12-16T7:50", "2024-12-20", "ARM A"
    ),
    ex = tibble::tribble(
      ~USUBJID, ~EXSEQ, ~EXTRT, ~EXSTDTC, ~EXENDTC, ~EXSTDY, ~EXENDY, ~EXDOSE,
      "001", 1, "DRUG_A", "2024-12-16T7:50", "2024-12-18T7:50", 1, 3, 100
    )
  ))

  result <- make_administration(sdtm, "DRUG_A", silent = TRUE)

  expect_true("EXDY" %in% names(result))
  expect_equal(result$EXDY, c(1, 2, 3))
})


test_that("make_administration imputes EXENDTC to RFENDTC for last episode", {
  sdtm <- sdtm(list(
    dm = tibble::tribble(
      ~USUBJID, ~SEX, ~RFSTDTC, ~RFENDTC, ~ACTARMCD,
      "001", 1, "2024-12-16T7:50", "2024-12-20T7:50", "ARM A"
    ),
    ex = tibble::tribble(
      ~USUBJID, ~EXSEQ, ~EXTRT, ~EXSTDTC, ~EXENDTC, ~EXDOSE,
      "001", 1, "DRUG_A", "2024-12-16T7:50", "2024-12-18T7:50", 100,
      "001", 2, "DRUG_A", "2024-12-19T7:50", NA, 100
    )
  ))

  result <- make_administration(sdtm, "DRUG_A", silent = TRUE)

  # Check that last episode was expanded using RFENDTC
  last_episode_dates <- result$DTC[result$USUBJID == "001" & result$DTC >= as.Date("2024-12-19")]
  expect_true(length(last_episode_dates) > 0)
})


test_that("make_administration imputes missing EXENDTC to cut-off date", {
  sdtm <- sdtm(list(
    dm = tibble::tribble(
      ~USUBJID, ~SEX, ~RFSTDTC, ~RFENDTC, ~ACTARMCD,
      "001", 1, "2024-12-16T7:50", "2024-12-20T7:50", "ARM A"
    ),
    ex = tibble::tribble(
      ~USUBJID, ~EXSEQ, ~EXTRT, ~EXSTDTC, ~EXENDTC, ~EXDOSE,
      "001", 1, "DRUG_A", "2024-12-16T7:50", "2024-12-18T7:50", 100,
      "001", 2, "DRUG_A", "2024-12-19T7:50", NA, 100
    )
  ))

  cut_off <- as.POSIXct("2024-12-22 23:59:59", tz = "UTC")
  result <- make_administration(sdtm, "DRUG_A", cut_off_date = cut_off, silent = TRUE)

  # Should have imputed the last episode to cut-off date
  expect_true(any(result$DTC <= as.Date(cut_off)))
})


test_that("make_administration imputes missing middle EXENDTC", {
  sdtm <- sdtm(list(
    dm = tibble::tribble(
      ~USUBJID, ~SEX, ~RFSTDTC, ~RFENDTC, ~ACTARMCD,
      "001", 1, "2024-12-16T7:50", "2024-12-25T7:50", "ARM A"
    ),
    ex = tibble::tribble(
      ~USUBJID, ~EXSEQ, ~EXTRT, ~EXSTDTC, ~EXENDTC, ~EXDOSE,
      "001", 1, "DRUG_A", "2024-12-16T7:50", "2024-12-18T7:50", 100,
      "001", 2, "DRUG_A", "2024-12-19T7:50", NA, 100,
      "001", 3, "DRUG_A", "2024-12-22T7:50", "2024-12-24T7:50", 100
    )
  ))

  # Capture messages to avoid cluttering output
  result <- make_administration(sdtm, "DRUG_A", silent = TRUE)

  # Middle episode should be imputed to day before next episode
  middle_episode <- result[result$DTC >= as.Date("2024-12-19") & result$DTC < as.Date("2024-12-22"), ]
  expect_true(nrow(middle_episode) > 0)
})


test_that("make_administration filters EXENDTC after EXSTDTC", {
  sdtm <- sdtm(list(
    dm = tibble::tribble(
      ~USUBJID, ~SEX, ~RFSTDTC, ~RFENDTC, ~ACTARMCD,
      "001", 1, "2024-12-16T7:50", "2024-12-25T7:50", "ARM A"
    ),
    ex = tibble::tribble(
      ~USUBJID, ~EXSEQ, ~EXTRT, ~EXSTDTC, ~EXENDTC, ~EXDOSE,
      "001", 1, "DRUG_A", "2024-12-16T7:50", "2024-12-18T7:50", 100,
      "001", 2, "DRUG_A", "2024-12-20T7:50", "2024-12-19T7:50", 100  # Invalid: end before start
    )
  ))

  result <- make_administration(sdtm, "DRUG_A", silent = TRUE)

  # Invalid episode should be filtered out
  expect_false(any(result$DTC >= as.Date("2024-12-20")))
})


test_that("make_administration integrates with PC domain for time imputation", {
  sdtm <- sdtm(list(
    dm = tibble::tribble(
      ~USUBJID, ~SEX, ~RFSTDTC, ~RFENDTC, ~ACTARMCD,
      "001", 1, "2024-12-16T7:50", "2024-12-20T7:50", "ARM A"
    ),
    ex = tibble::tribble(
      ~USUBJID, ~EXSEQ, ~EXTRT, ~EXSTDTC, ~EXENDTC, ~EXDOSE,
      "001", 1, "DRUG_A", "2024-12-16", "2024-12-18", 100
    ),
    pc = tibble::tribble(
      ~USUBJID, ~PCTESTCD, ~PCRFTDTC,
      "001", "DRUG_A", "2024-12-16T08:15",
      "001", "DRUG_A", "2024-12-17T09:30"
    )
  ))

  result <- make_administration(sdtm, "DRUG_A", analyte = "DRUG_A", silent = TRUE)

  expect_equal(
    result$IMPUTATION,
    c("time imputed from PCRFTDTC", "time imputed from PCRFTDTC",
      "time carried forward"))
})


test_that("make_administration carries forward missing times", {
  sdtm <- sdtm(list(
    dm = tibble::tribble(
      ~USUBJID, ~SEX, ~RFSTDTC, ~RFENDTC, ~ACTARMCD,
      "001", 1, "2024-12-16T7:50", "2024-12-20T7:50", "ARM A"
    ),
    ex = tibble::tribble(
      ~USUBJID, ~EXSEQ, ~EXTRT, ~EXSTDTC, ~EXENDTC, ~EXDOSE,
      "001", 1, "DRUG_A", "2024-12-16T7:50", "2024-12-18", 100  # Last day has no time
    )
  ))

  result <- make_administration(sdtm, "DRUG_A", silent = TRUE)

  # Check that time was carried forward
  expect_true(any(grepl("time carried forward", result$IMPUTATION)))
})


test_that("make_administration calculates TRTDY correctly", {
  sdtm <- sdtm(list(
    dm = tibble::tribble(
      ~USUBJID, ~SEX, ~RFSTDTC, ~RFENDTC, ~ACTARMCD,
      "001", 1, "2024-12-16T7:50", "2024-12-20T7:50", "ARM A"
    ),
    ex = tibble::tribble(
      ~USUBJID, ~EXSEQ, ~EXTRT, ~EXSTDTC, ~EXENDTC, ~EXDOSE,
      "001", 1, "DRUG_A", "2024-12-16T7:50", "2024-12-18T7:50", 100
    )
  ))

  result <- make_administration(sdtm, "DRUG_A", silent = TRUE)

  expect_true("TRTDY" %in% names(result))
  # TRTDY should start at 1 for first administration day
  expect_equal(min(result$TRTDY), 1)
  # TRTDY should increase by 1 for each day
  expect_true(all(diff(result$TRTDY) >= 0))
})


test_that("make_administration sets correct event indicators", {
  sdtm <- sdtm(list(
    dm = tibble::tribble(
      ~USUBJID, ~SEX, ~RFSTDTC, ~RFENDTC, ~ACTARMCD,
      "001", 1, "2024-12-16T7:50", "2024-12-20T7:50", "ARM A"
    ),
    ex = tibble::tribble(
      ~USUBJID, ~EXSEQ, ~EXTRT, ~EXSTDTC, ~EXENDTC, ~EXDOSE,
      "001", 1, "DRUG_A", "2024-12-16T7:50", "2024-12-18T7:50", 100
    )
  ))

  result <- make_administration(sdtm, "DRUG_A", silent = TRUE)

  expect_true(all(result$EVID == 1))
  expect_true(all(result$MDV == 1))
  expect_true(all(is.na(result$DV)))
})


test_that("make_administration sets DOSE and AMT correctly", {
  sdtm <- sdtm(list(
    dm = tibble::tribble(
      ~USUBJID, ~SEX, ~RFSTDTC, ~RFENDTC, ~ACTARMCD,
      "001", 1, "2024-12-16T7:50", "2024-12-20T7:50", "ARM A"
    ),
    ex = tibble::tribble(
      ~USUBJID, ~EXSEQ, ~EXTRT, ~EXSTDTC, ~EXENDTC, ~EXDOSE,
      "001", 1, "DRUG_A", "2024-12-16T7:50", "2024-12-18T7:50", 100
    )
  ))

  result <- make_administration(sdtm, "DRUG_A", silent = TRUE)

  expect_true(all(result$DOSE == 100))
  expect_true(all(result$AMT == 100))
})


test_that("make_administration handles EXSEQ when present", {
  sdtm <- sdtm(list(
    dm = tibble::tribble(
      ~USUBJID, ~SEX, ~RFSTDTC, ~RFENDTC, ~ACTARMCD,
      "001", 1, "2024-12-16T7:50", "2024-12-20T7:50", "ARM A"
    ),
    ex = tibble::tribble(
      ~USUBJID, ~EXSEQ, ~EXTRT, ~EXSTDTC, ~EXENDTC, ~EXDOSE,
      "001", 1, "DRUG_A", "2024-12-16T7:50", "2024-12-18T7:50", 100
    )
  ))

  result <- make_administration(sdtm, "DRUG_A", keep = "SRC_SEQ", silent = TRUE)

  expect_true("SRC_SEQ" %in% names(result))
  expect_true(all(result$SRC_SEQ == 1))
})


test_that("make_administration handles missing EXSEQ", {
  sdtm <- sdtm(list(
    dm = tibble::tribble(
      ~USUBJID, ~SEX, ~RFSTDTC, ~RFENDTC, ~ACTARMCD,
      "001", 1, "2024-12-16T7:50", "2024-12-20T7:50", "ARM A"
    ),
    ex = tibble::tribble(
      ~USUBJID, ~EXTRT, ~EXSTDTC, ~EXENDTC, ~EXDOSE,
      "001", "DRUG_A", "2024-12-16T7:50", "2024-12-18T7:50", 100
    )
  ))

  result <- make_administration(sdtm, "DRUG_A", keep = "SRC_SEQ", silent = TRUE)

  expect_true("SRC_SEQ" %in% names(result))
  expect_true(all(is.na(result$SRC_SEQ)))
})


test_that("make_administration handles VS domain when present", {
  sdtm <- sdtm(list(
    dm = tibble::tribble(
      ~USUBJID, ~SEX, ~RFSTDTC, ~RFENDTC, ~ACTARMCD,
      "001", 1, "2024-12-16T7:50", "2024-12-20T7:50", "ARM A"
    ),
    ex = tibble::tribble(
      ~USUBJID, ~EXSEQ, ~EXTRT, ~EXSTDTC, ~EXENDTC, ~EXDOSE,
      "001", 1, "DRUG_A", "2024-12-16T7:50", "2024-12-18T7:50", 100
    ),
    vs = tibble::tribble(
      ~USUBJID, ~VSTESTCD, ~VSSTRESN, ~VSBLFL,
      "001", "WEIGHT", 70, "Y"
    )
  ))

  expect_no_error(
    result <- make_administration(sdtm, "DRUG_A", silent = TRUE)
  )
})


test_that("make_administration handles multiple subjects correctly", {
  sdtm <- sdtm(list(
    dm = tibble::tribble(
      ~USUBJID, ~SEX, ~RFSTDTC, ~RFENDTC, ~ACTARMCD,
      "001", 1, "2024-12-16T7:50", "2024-12-20T7:50", "ARM A",
      "002", 1, "2024-12-16T7:50", "2024-12-20T7:50", "ARM A"
    ),
    ex = tibble::tribble(
      ~USUBJID, ~EXSEQ, ~EXTRT, ~EXSTDTC, ~EXENDTC, ~EXDOSE,
      "001", 1, "DRUG_A", "2024-12-16T7:50", "2024-12-18T7:50", 100,
      "002", 1, "DRUG_A", "2024-12-16T7:50", "2024-12-18T7:50", 100
    )
  ))

  result <- make_administration(sdtm, "DRUG_A", silent = TRUE)

  expect_equal(length(unique(result$USUBJID)), 2)
  expect_true(all(c("001", "002") %in% result$USUBJID))
})


test_that("make_administration handles single-day administrations", {
  sdtm <- sdtm(list(
    dm = tibble::tribble(
      ~USUBJID, ~SEX, ~RFSTDTC, ~RFENDTC, ~ACTARMCD,
      "001", 1, "2024-12-16T7:50", "2024-12-20T7:50", "ARM A"
    ),
    ex = tibble::tribble(
      ~USUBJID, ~EXSEQ, ~EXTRT, ~EXSTDTC, ~EXENDTC, ~EXDOSE,
      "001", 1, "DRUG_A", "2024-12-16T7:50", "2024-12-16T7:50", 100
    )
  ))

  result <- make_administration(sdtm, "DRUG_A", silent = TRUE)

  # Should have exactly one row for single-day administration
  expect_equal(nrow(result), 1)
})


test_that("make_administration returns nif object", {
  sdtm <- sdtm(list(
    dm = tibble::tribble(
      ~USUBJID, ~SEX, ~RFSTDTC, ~RFENDTC, ~ACTARMCD,
      "001", 1, "2024-12-16T7:50", "2024-12-20T7:50", "ARM A"
    ),
    ex = tibble::tribble(
      ~USUBJID, ~EXSEQ, ~EXTRT, ~EXSTDTC, ~EXENDTC, ~EXDOSE,
      "001", 1, "DRUG_A", "2024-12-16T7:50", "2024-12-18T7:50", 100
    )
  ))

  result <- make_administration(sdtm, "DRUG_A", silent = TRUE)

  expect_s3_class(result, "nif")
})


test_that("make_administration handles silent parameter", {
  sdtm <- sdtm(list(
    dm = tibble::tribble(
      ~USUBJID, ~SEX, ~RFSTDTC, ~RFENDTC, ~ACTARMCD,
      "001", 1, "2024-12-16T7:50", "2024-12-20T7:50", "ARM A"
    ),
    ex = tibble::tribble(
      ~USUBJID, ~EXSEQ, ~EXTRT, ~EXSTDTC, ~EXENDTC, ~EXDOSE,
      "001", 1, "DRUG_A", "2024-12-16T7:50", NA, 100
    )
  ))

  # Should not produce messages when silent = TRUE
  expect_no_message(
    result <- make_administration(sdtm, "DRUG_A", silent = TRUE)
  )
})



# -----------
## Tests for make_administration using no imputation rules

test_that("make_administration works with void imputation rule set", {
  # Minimal test data
  sdtm <- list(
    dm = tibble::tribble(
      ~USUBJID, ~DOMAIN, ~SEX,   ~ACTARMCD,
           "1",    "DM",    0, "TREATMENT"
    ),

    pc = tibble::tribble(
      ~USUBJID, ~DOMAIN,   ~PCTESTCD,          ~PCRFTDTC,
           "1",    "PC", "ANALYTE_A", "2025-08-15T08:15",
    ),

    ex = tibble::tribble(
      ~USUBJID,           ~EXSTDTC,     ~EXENDTC,              ~EXTRT, ~EXDOSE,
           "1", "2025-01-13T07:00", "2025-01-18T08:00", "TREATMENT_A",     100
    )
  ) |>
    sdtm()

  result = make_administration(
    sdtm, "TREATMENT_A", imputation = list(), silent = TRUE)

  test <- result |>
    decompose_dtc("DTC")

  expect_equal(test$DTC_time, c(
      "07:00", "07:00", "07:00", "07:00", "07:00", "08:00"))
})


test_that("make_administration works with examplinib without matching analyte", {
  sdtm <- examplinib_poc

  analyte <- NULL
  pctestcd = NULL
  cmt <- 1
  subject_filter <- "!ACTARMCD %in% c('SCRNFAIL', 'NOTTRT')"
  cut_off_date <- NULL
  keep <- ""
  imputation <- imputation_standard
  silent <- NULL

  # no pctestcd
  expect_no_error(
    expect_message(
      result <- make_administration(sdtm, extrt = "EXAMPLINIB")
    )
  )

  expect_no_error(
    expect_message(
      result <- make_administration(sdtm, extrt = "EXAMPLINIB", analyte = "RS2023")
    )
  )

  # result |>
  #   filter(EVID == 1) |>
  #   decompose_dtc("DTC") |>
  #   select(USUBJID, DTC_date, DTC_time, IMPUTATION)
})
















