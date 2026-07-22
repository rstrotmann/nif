## Oral QD (3 days) plus day-3 IV infusion - 8 subjects, BSA-scaled IV dose


test_that("add_administration handles oral plus IV on day 3 for 8 subjects", {
  infusion_min <- c(40, 45, 50, 52, 55, 60, 65, 70)

  dm <- tibble::tribble(
    ~USUBJID, ~SEX,          ~RFSTDTC,          ~RFENDTC, ~ACTARMCD,
        "S01",    0, "2025-06-01T08:00", "2025-06-03T18:00",    "TREAT",
        "S02",    1, "2025-06-01T08:00", "2025-06-03T18:00",    "TREAT",
        "S03",    0, "2025-06-01T08:00", "2025-06-03T18:00",    "TREAT",
        "S04",    1, "2025-06-01T08:00", "2025-06-03T18:00",    "TREAT",
        "S05",    0, "2025-06-01T08:00", "2025-06-03T18:00",    "TREAT",
        "S06",    1, "2025-06-01T08:00", "2025-06-03T18:00",    "TREAT",
        "S07",    0, "2025-06-01T08:00", "2025-06-03T18:00",    "TREAT",
        "S08",    1, "2025-06-01T08:00", "2025-06-03T18:00",    "TREAT"
  )

  vs <- tibble::tribble(
    ~USUBJID, ~VSTESTCD, ~VSSTRESN,            ~VSDTC,
        "S01",  "HEIGHT",        170, "2025-05-30T08:00",
        "S01",  "WEIGHT",         65, "2025-05-30T08:00",
        "S02",  "HEIGHT",        175, "2025-05-30T08:00",
        "S02",  "WEIGHT",         70, "2025-05-30T08:00",
        "S03",  "HEIGHT",        180, "2025-05-30T08:00",
        "S03",  "WEIGHT",         75, "2025-05-30T08:00",
        "S04",  "HEIGHT",        165, "2025-05-30T08:00",
        "S04",  "WEIGHT",         58, "2025-05-30T08:00",
        "S05",  "HEIGHT",        172, "2025-05-30T08:00",
        "S05",  "WEIGHT",         68, "2025-05-30T08:00",
        "S06",  "HEIGHT",        178, "2025-05-30T08:00",
        "S06",  "WEIGHT",         82, "2025-05-30T08:00",
        "S07",  "HEIGHT",        168, "2025-05-30T08:00",
        "S07",  "WEIGHT",         62, "2025-05-30T08:00",
        "S08",  "HEIGHT",        183, "2025-05-30T08:00",
        "S08",  "WEIGHT",         88, "2025-05-30T08:00"
  )

  anthro <- vs |>
    tidyr::pivot_wider(names_from = VSTESTCD, values_from = VSSTRESN) |>
    dplyr::mutate(
      BSA     = bsa_mosteller(.data$WEIGHT, .data$HEIGHT),
      IV_DOSE = round(.data$BSA * 20, 2),
      EXDUR   = paste0("PT", infusion_min, "M"),
      IV_DUR  = infusion_min / 60,
      IV_END  = format(
        as.POSIXct("2025-06-03T14:00", tz = "UTC") + infusion_min * 60,
        "%Y-%m-%dT%H:%M"
      )
    )

  oral_ex <- anthro |>
    dplyr::transmute(
      USUBJID,
      EXSEQ   = 1L,
      EXTRT   = "EXAMPLINIB",
      EXSTDTC = "2025-06-01T08:00",
      EXENDTC = "2025-06-03T08:00",
      EXDOSE  = 100,
      EXROUTE = "ORAL",
      EXDUR   = NA_character_
    )

  iv_ex <- anthro |>
    dplyr::transmute(
      USUBJID,
      EXSEQ   = 2L,
      EXTRT   = "EXAMPLINIB",
      EXSTDTC = "2025-06-03T14:00",
      EXENDTC = .data$IV_END,
      EXDOSE  = .data$IV_DOSE,
      EXROUTE = "IV",
      EXDUR   = .data$EXDUR
    )

  ex <- dplyr::bind_rows(oral_ex, iv_ex) |>
    dplyr::arrange(.data$USUBJID, .data$EXSEQ)

  sdtm <- sdtm(list(dm = dm, vs = vs, ex = ex))

  expected_iv <- anthro |>
    dplyr::select(USUBJID, expected_iv_amt = IV_DOSE, expected_iv_dur = IV_DUR)

  result <- nif() |>
    add_administration(
      sdtm,
      extrt      = "EXAMPLINIB",
      keep       = "SRC_SEQ",
      imputation = imputation_rules_void,
      silent     = TRUE
    )

  res <- as.data.frame(result)

  expect_s3_class(result, "nif")
  expect_equal(nrow(res), 32L)
  expect_true(all(res$EVID == 1))

  per_subject <- res |>
    dplyr::reframe(
      n_rows     = dplyr::n(),
      n_trtdy    = dplyr::n_distinct(.data$TRTDY),
      n_day3     = sum(.data$TRTDY == 3),
      .by = "USUBJID"
    )

  expect_equal(per_subject$n_rows, rep(4L, 8))
  expect_equal(per_subject$n_trtdy, rep(3L, 8))
  expect_equal(per_subject$n_day3, rep(2L, 8))
  expect_setequal(unique(res$TRTDY), c(1, 2, 3))

  oral <- res[res$SRC_SEQ == 1, ]
  expect_equal(oral$AMT, rep(100, nrow(oral)))
  expect_equal(oral$DOSE, rep(100, nrow(oral)))
  expect_true(all(is.na(oral$DUR)))

  iv <- res |>
    dplyr::filter(.data$SRC_SEQ == 2) |>
    dplyr::left_join(expected_iv, by = "USUBJID")

  expect_equal(iv$AMT, iv$expected_iv_amt, tolerance = 1e-6)
  expect_equal(iv$DOSE, iv$expected_iv_amt, tolerance = 1e-6)
  expect_equal(iv$DUR, iv$expected_iv_dur, tolerance = 1e-6)
  expect_equal(as.Date(iv$DTC), rep(as.Date("2025-06-03"), nrow(iv)))
  expect_equal(format(iv$DTC, "%H:%M"), rep("14:00", nrow(iv)))

  expect_false(any(is.na(res$TIME)))
  time_order <- res |>
    dplyr::reframe(
      non_decreasing = all(diff(.data$TIME) >= 0),
      .by = "ID"
    )
  expect_true(all(time_order$non_decreasing))

  first_admin <- res |>
    dplyr::slice_min(.data$TIME, n = 1, by = "ID")
  expect_equal(first_admin$TIME, rep(0, nrow(first_admin)))
})


test_that("add_administration oral-IV complex detects IV route", {
  infusion_min <- 50

  dm <- tibble::tribble(
    ~USUBJID, ~SEX,          ~RFSTDTC,          ~RFENDTC, ~ACTARMCD,
        "S01",    0, "2025-06-01T08:00", "2025-06-03T18:00",    "TREAT"
  )

  ex <- tibble::tribble(
    ~USUBJID, ~EXSEQ,        ~EXTRT,          ~EXSTDTC,          ~EXENDTC, ~EXDOSE, ~EXROUTE, ~EXDUR,
        "S01",      1, "EXAMPLINIB", "2025-06-01T08:00", "2025-06-03T08:00",     100,    "ORAL",      NA,
        "S01",      2, "EXAMPLINIB", "2025-06-03T14:00", "2025-06-03T14:50",   35.42,      "IV", "PT50M"
  )

  sdtm <- sdtm(list(dm = dm, ex = ex))

  expect_message(
      expect_message(
      expect_message(
        add_administration(
          nif(),
          sdtm,
          extrt      = "EXAMPLINIB",
          imputation = imputation_rules_void,
          silent     = FALSE
        ),
        "IV administration"
      )
    )
  )
})
