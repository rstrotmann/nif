## Real-life administration time imputation with incomplete / missing DTC sources
##
## Case matrix (one subject per scenario):
##
##   S01  Complete EXSTDTC/EXENDTC times, no PC
##        → EX start/end times + carry-forward on middle day
##   S02  Date-only EX, no PC
##        → no clock time (midnight), empty IMPUTATION
##   S03  Date-only EX + complete PCRFTDTC
##        → PCRFTDTC
##   S04  EX with time + date-only PCRFTDTC, no NTIME source
##        → EX time kept
##   S05  EX with time + hour-only PCRFTDTC, no NTIME source
##        → EX time kept
##   S06  EX with time + trailing-space PCRFTDTC
##        → trimmed PCRFTDTC
##   S07  EX with time + blank/empty PCRFTDTC, no NTIME source
##        → EX time kept
##   S08  Date-only EX + NTIME only (PCRFTDTC NA)
##        → PCELTM/PCTPT estimate
##   S09  EX time + NTIME + complete PCRFTDTC
##        → PCRFTDTC wins over NTIME
##   S10  Date-only EX + unparseable PCDTC, no PCRFTDTC
##        → no clock time
##   S11  Multi-day EX (time on start only) + PCRFTDTC on day 1 only
##        → PCRFTDTC day 1, carry-forward later days
##   S12  Multiple PCRFTDTC on same day
##        → earlier PCRFTDTC selected
##   S13  EX with time + date-only PCRFTDTC + usable PCTPT
##        → falls through to NTIME (PCRFTDTC ignored)
##   S14  EX with time + hour-only PCRFTDTC + usable PCTPT
##        → falls through to NTIME
##   S15  Date-only EX + date-only PCRFTDTC, no NTIME
##        → still no clock time


test_that("admin time imputation handles all incomplete DTC and missing time sources", {
  dm <- tibble::tribble(
     ~USUBJID, ~SEX,           ~RFSTDTC,           ~RFENDTC, ~ACTARMCD,
        "S01",    0, "2025-03-01T08:00", "2025-03-03T18:00",   "TREAT",
        "S02",    1, "2025-03-01T08:00", "2025-03-03T18:00",   "TREAT",
        "S03",    0, "2025-03-01T08:00", "2025-03-01T18:00",   "TREAT",
        "S04",    1, "2025-03-01T08:00", "2025-03-01T18:00",   "TREAT",
        "S05",    0, "2025-03-01T08:00", "2025-03-01T18:00",   "TREAT",
        "S06",    1, "2025-03-01T08:00", "2025-03-01T18:00",   "TREAT",
        "S07",    0, "2025-03-01T08:00", "2025-03-01T18:00",   "TREAT",
        "S08",    1, "2025-03-01T08:00", "2025-03-01T18:00",   "TREAT",
        "S09",    0, "2025-03-01T08:00", "2025-03-01T18:00",   "TREAT",
        "S10",    1, "2025-03-01T08:00", "2025-03-01T18:00",   "TREAT",
        "S11",    0, "2025-03-01T08:00", "2025-03-05T18:00",   "TREAT",
        "S12",    1, "2025-03-01T08:00", "2025-03-01T18:00",   "TREAT",
        "S13",    0, "2025-03-01T08:00", "2025-03-01T18:00",   "TREAT",
        "S14",    1, "2025-03-01T08:00", "2025-03-01T18:00",   "TREAT",
        "S15",    0, "2025-03-01T08:00", "2025-03-01T18:00",   "TREAT"
     )


  ex <-tibble::tribble(
     ~USUBJID, ~EXSEQ, ~EXTRT,           ~EXSTDTC,           ~EXENDTC, ~EXDOSE,
        "S01",      1, "DRUG", "2025-03-01T08:00", "2025-03-03T09:30",     100,
        "S02",      1, "DRUG",       "2025-03-01",       "2025-03-03",     100,
        "S03",      1, "DRUG",       "2025-03-01",       "2025-03-01",     100,
        "S04",      1, "DRUG", "2025-03-01T07:45", "2025-03-01T07:45",     100,
        "S05",      1, "DRUG", "2025-03-01T07:50", "2025-03-01T07:50",     100,
        "S06",      1, "DRUG", "2025-03-01T07:00", "2025-03-01T07:00",     100,
        "S07",      1, "DRUG", "2025-03-01T07:15", "2025-03-01T07:15",     100,
        "S08",      1, "DRUG",       "2025-03-01",       "2025-03-01",     100,
        "S09",      1, "DRUG", "2025-03-01T07:00", "2025-03-01T07:00",     100,
        "S10",      1, "DRUG",       "2025-03-01",       "2025-03-01",     100,
        "S11",      1, "DRUG", "2025-03-01T08:00",       "2025-03-05",     100,
        "S12",      1, "DRUG", "2025-03-01T07:00", "2025-03-01T07:00",     100,
        "S13",      1, "DRUG", "2025-03-01T07:45", "2025-03-01T07:45",     100,
        "S14",      1, "DRUG", "2025-03-01T07:50", "2025-03-01T07:50",     100,
        "S15",      1, "DRUG",       "2025-03-01",       "2025-03-01",     100
     )


  ## PCTPT left NA when the case must not fall through to NTIME after PCRFTDTC fails
  pc <- tibble::tribble(
     ~USUBJID, ~DOMAIN, ~PCTESTCD,              ~PCDTC,           ~PCRFTDTC,         ~PCTPT,
        "S03",    "PC",    "DRUG",  "2025-03-01T09:00",  "2025-03-01T08:20", "1 H POSTDOSE",
        "S04",    "PC",    "DRUG",  "2025-03-01T09:00",        "2025-03-01",             NA,
        "S05",    "PC",    "DRUG",  "2025-03-01T09:00",     "2025-03-01T08",             NA,
        "S06",    "PC",    "DRUG",  "2025-03-01T09:00", "2025-03-01T08:45 ", "1 H POSTDOSE",
        "S07",    "PC",    "DRUG",  "2025-03-01T09:00",                 " ",             NA,
        "S07",    "PC",    "DRUG",  "2025-03-01T10:00",                  NA,             NA,
        "S08",    "PC",    "DRUG",  "2025-03-01T09:00",                  NA, "1 H POSTDOSE",
        "S08",    "PC",    "DRUG",  "2025-03-01T10:00",                  NA,      "2H POST",
        "S09",    "PC",    "DRUG",  "2025-03-01T09:00",  "2025-03-01T08:10", "1 H POSTDOSE",
        "S09",    "PC",    "DRUG",  "2025-03-01T10:00",  "2025-03-01T08:10",      "2H POST",
        "S10",    "PC",    "DRUG",     "2025-03-01T09",                  NA, "1 H POSTDOSE",
        "S10",    "PC",    "DRUG", "2025-03-01T10:00Z",                  NA,      "2H POST",
        "S11",    "PC",    "DRUG",  "2025-03-01T09:00",  "2025-03-01T08:05", "1 H POSTDOSE",
        "S12",    "PC",    "DRUG",  "2025-03-01T09:00",  "2025-03-01T08:30", "1 H POSTDOSE",
        "S12",    "PC",    "DRUG",  "2025-03-01T10:00",  "2025-03-01T08:00",      "2H POST",
        "S13",    "PC",    "DRUG",  "2025-03-01T09:00",        "2025-03-01", "1 H POSTDOSE",
        "S14",    "PC",    "DRUG",  "2025-03-01T09:00",     "2025-03-01T08", "1 H POSTDOSE",
        "S15",    "PC",    "DRUG",  "2025-03-01T09:00",        "2025-03-01",             NA
     )


  sdtm <- sdtm(list(dm = dm, ex = ex, pc = pc))

  expect_no_warning(
    result <- add_administration(
      nif(),
      sdtm,
      extrt      = "DRUG",
      pctestcd   = "DRUG",
      imputation = imputation_rules_standard,
      silent     = TRUE
    )
  )

  res <- as.data.frame(result) |>
    dplyr::mutate(
      date = as.Date(.data$DTC),
      time = format(.data$DTC, "%H:%M")
    )

  ## ---- S01: EX times + carry-forward ---------------------------------------
  s01 <- dplyr::filter(res, .data$USUBJID == "S01") |>
    dplyr::arrange(.data$date)
  expect_equal(nrow(s01), 3L)
  expect_equal(s01$time, c("08:00", "08:00", "09:30"))
  expect_match(s01$IMPUTATION[1], "EXSTDTC")
  expect_match(s01$IMPUTATION[2], "carried forward")
  expect_match(s01$IMPUTATION[3], "EXENDTC")

  ## ---- S02: date-only EX, no PC → no clock time ----------------------------
  s02 <- dplyr::filter(res, .data$USUBJID == "S02")
  expect_equal(nrow(s02), 3L)
  expect_true(all(format(s02$DTC, "%H:%M:%S") == "00:00:00"))
  expect_true(all(s02$IMPUTATION == "" | is.na(s02$IMPUTATION)))

  ## ---- S03: complete PCRFTDTC ----------------------------------------------
  s03 <- dplyr::filter(res, .data$USUBJID == "S03")
  expect_equal(nrow(s03), 1L)
  expect_equal(s03$time, "08:20")
  expect_match(s03$IMPUTATION, "PCRFTDTC")

  ## ---- S04: date-only PCRFTDTC, no NTIME → keep EX time --------------------
  s04 <- dplyr::filter(res, .data$USUBJID == "S04")
  expect_equal(s04$time, "07:45")
  expect_false(grepl("PCRFTDTC|PCELTM|PCTPT", s04$IMPUTATION))
  expect_match(s04$IMPUTATION, "EXSTDTC")

  ## ---- S05: hour-only PCRFTDTC, no NTIME → keep EX time --------------------
  s05 <- dplyr::filter(res, .data$USUBJID == "S05")
  expect_equal(s05$time, "07:50")
  expect_false(grepl("PCRFTDTC|PCELTM|PCTPT", s05$IMPUTATION))
  expect_match(s05$IMPUTATION, "EXSTDTC")

  ## ---- S06: trailing-space PCRFTDTC → trimmed ------------------------------
  s06 <- dplyr::filter(res, .data$USUBJID == "S06")
  expect_equal(s06$time, "08:45")
  expect_match(s06$IMPUTATION, "PCRFTDTC")

  ## ---- S07: blank/empty PCRFTDTC, no NTIME → keep EX time ------------------
  s07 <- dplyr::filter(res, .data$USUBJID == "S07")
  expect_equal(s07$time, "07:15")
  expect_false(grepl("PCRFTDTC|PCELTM|PCTPT", s07$IMPUTATION))
  expect_match(s07$IMPUTATION, "EXSTDTC")

  ## ---- S08: NTIME-only estimate --------------------------------------------
  s08 <- dplyr::filter(res, .data$USUBJID == "S08")
  expect_equal(nrow(s08), 1L)
  expect_equal(s08$time, "08:00")
  expect_match(s08$IMPUTATION, "PCELTM/PCTPT")

  ## ---- S09: PCRFTDTC overrides NTIME ---------------------------------------
  s09 <- dplyr::filter(res, .data$USUBJID == "S09")
  expect_equal(s09$time, "08:10")
  expect_match(s09$IMPUTATION, "PCRFTDTC")
  expect_false(grepl("PCELTM/PCTPT", s09$IMPUTATION))

  ## ---- S10: unparseable PCDTC, no PCRFTDTC → no clock time -----------------
  s10 <- dplyr::filter(res, .data$USUBJID == "S10")
  expect_equal(nrow(s10), 1L)
  expect_equal(format(s10$DTC, "%H:%M:%S"), "00:00:00")
  expect_false(any(grepl("PCRFTDTC|PCELTM|PCTPT", s10$IMPUTATION)))

  ## ---- S11: PCRFTDTC day 1, carry-forward days 2–5 -------------------------
  s11 <- dplyr::filter(res, .data$USUBJID == "S11") |>
    dplyr::arrange(.data$date)
  expect_equal(nrow(s11), 5L)
  expect_equal(s11$time[1], "08:05")
  expect_match(s11$IMPUTATION[1], "PCRFTDTC")
  expect_true(all(s11$time[2:5] == "08:05"))
  expect_true(all(grepl("carried forward", s11$IMPUTATION[2:5])))

  ## ---- S12: multiple PCRFTDTC → earlier time -------------------------------
  s12 <- dplyr::filter(res, .data$USUBJID == "S12")
  expect_equal(s12$time, "08:00")
  expect_match(s12$IMPUTATION, "PCRFTDTC")

  ## ---- S13: date-only PCRFTDTC + PCTPT → NTIME fallthrough -----------------
  s13 <- dplyr::filter(res, .data$USUBJID == "S13")
  expect_equal(s13$time, "08:00")
  expect_match(s13$IMPUTATION, "PCELTM/PCTPT")
  expect_false(grepl("PCRFTDTC", s13$IMPUTATION))

  ## ---- S14: hour-only PCRFTDTC + PCTPT → NTIME fallthrough -----------------
  s14 <- dplyr::filter(res, .data$USUBJID == "S14")
  expect_equal(s14$time, "08:00")
  expect_match(s14$IMPUTATION, "PCELTM/PCTPT")
  expect_false(grepl("PCRFTDTC", s14$IMPUTATION))

  ## ---- S15: date-only EX + date-only PCRFTDTC, no NTIME -------------------
  s15 <- dplyr::filter(res, .data$USUBJID == "S15")
  expect_equal(format(s15$DTC, "%H:%M:%S"), "00:00:00")
  expect_false(grepl("PCRFTDTC|PCELTM|PCTPT|EXSTDTC|EXENDTC", s15$IMPUTATION))
})


test_that("admin time imputation surfaces incomplete PCRFTDTC messages when not silent", {
  dm <- tibble::tribble(
     ~USUBJID, ~SEX,           ~RFSTDTC,     ~RFENDTC, ~ACTARMCD,
        "S04",    0, "2025-03-01T08:00", "2025-03-01",   "TREAT",
        "S05",    1, "2025-03-01T08:00", "2025-03-01",   "TREAT"
     )

  ex <- tibble::tribble(
     ~USUBJID, ~EXSEQ, ~EXTRT,           ~EXSTDTC,           ~EXENDTC, ~EXDOSE,
        "S04",      1, "DRUG", "2025-03-01T07:45", "2025-03-01T07:45",     100,
        "S05",      1, "DRUG", "2025-03-01T07:50", "2025-03-01T07:50",     100
     )

  pc <- tibble::tribble(
     ~USUBJID, ~DOMAIN, ~PCTESTCD,             ~PCDTC,       ~PCRFTDTC, ~PCTPT,
        "S04",    "PC",    "DRUG", "2025-03-01T09:00",    "2025-03-01",     NA,
        "S05",    "PC",    "DRUG", "2025-03-01T09:00", "2025-03-01T08",     NA
     )

  sdtm <- sdtm(list(dm = dm, ex = ex, pc = pc))

  msgs <- capture_messages(
    add_administration(
      nif(),
      sdtm,
      extrt      = "DRUG",
      pctestcd   = "DRUG",
      imputation = imputation_rules_standard,
      silent     = FALSE
    )
  )
  expect_true(any(grepl("no time component", msgs)))
  expect_true(any(grepl("unparseable PCRFTDTC", msgs)))
})
