# Helpers -----------------------------------------------------------------

make_subjects_dm <- function() {
  tibble::tribble(
    ~USUBJID,   ~SEX, ~ACTARMCD,  ~BRTHDTC,    ~RFICDTC,    ~RFSTDTC,    ~RFXSTDTC,          ~RACE, ~ETHNIC,                 ~COUNTRY,
    "SUBJ-001", "M",  "TRT",      "1970-01-01", "2020-01-01", "2020-01-15", "2020-01-15T08:00", "WHITE", "NOT HISPANIC OR LATINO", "USA",
    "SUBJ-002", "F",  "TRT",      "1980-02-15", "2020-01-02", "2020-01-16", "2020-01-16T09:00", "ASIAN", "HISPANIC OR LATINO",     "CAN",
    "SUBJ-003", "M",  "NOTTRT",   "1990-06-30", "2020-01-03", "2020-01-17", "2020-01-17T10:00", "BLACK OR AFRICAN AMERICAN", "NOT HISPANIC OR LATINO", "GBR",
    "SUBJ-004", "F",  "SCRNFAIL", "2000-12-25", "2020-01-04", "2020-01-18", "2020-01-18T11:00", "OTHER", "HISPANIC OR LATINO",     "AUS"
  )
}


make_subjects_vs_blfl <- function() {
  tibble::tribble(
    ~USUBJID,   ~VSTESTCD, ~VSSTRESN, ~VSBLFL, ~VSDTC,
    "SUBJ-001", "WEIGHT",  70,        "Y",     "2020-01-05",
    "SUBJ-001", "HEIGHT",  175,       "Y",     "2020-01-05",
    "SUBJ-001", "WEIGHT",  71,        "N",     "2020-01-20",
    "SUBJ-001", "HEIGHT",  175,       "N",     "2020-01-20",
    "SUBJ-002", "WEIGHT",  65,        "Y",     "2020-01-06",
    "SUBJ-002", "HEIGHT",  160,       "Y",     "2020-01-06",
    "SUBJ-002", "WEIGHT",  64,        "N",     "2020-01-21",
    "SUBJ-002", "HEIGHT",  160,       "N",     "2020-01-21",
    "SUBJ-003", "WEIGHT",  80,        "Y",     "2020-01-07",
    "SUBJ-003", "HEIGHT",  180,       "Y",     "2020-01-07",
    "SUBJ-004", "WEIGHT",  55,        "Y",     "2020-01-08",
    "SUBJ-004", "HEIGHT",  155,       "Y",     "2020-01-08"
  )
}


make_subjects_vs_dates <- function() {
  tibble::tribble(
    ~USUBJID,   ~VSTESTCD, ~VSSTRESN, ~VSDTC,
    "SUBJ-001", "WEIGHT",  70,        "2020-01-05",
    "SUBJ-001", "HEIGHT",  175,       "2020-01-05",
    "SUBJ-001", "WEIGHT",  99,        "2020-01-15",
    "SUBJ-001", "HEIGHT",  199,       "2020-01-15",
    "SUBJ-002", "WEIGHT",  65,        "2020-01-06",
    "SUBJ-002", "HEIGHT",  160,       "2020-01-06",
    "SUBJ-002", "WEIGHT",  90,        "2020-01-20",
    "SUBJ-002", "HEIGHT",  161,       "2020-01-20"
  )
}


# Validation --------------------------------------------------------------

test_that("make_subjects validates dm input", {
  expect_error(make_subjects("not a dataframe"), "dm must be a data.frame")
  expect_error(make_subjects(list(a = 1)), "dm must be a data.frame")
  expect_error(make_subjects(NULL), "dm must not be NULL")

  incomplete <- tibble::tribble(
    ~USUBJID, ~ACTARMCD,
    "001",    "TRT"
  )
  expect_error(make_subjects(incomplete), "Missing columns in dm: SEX")
})


test_that("make_subjects validates vs input", {
  dm <- tibble::tribble(
    ~USUBJID, ~SEX, ~ACTARMCD,
    "001",    "M",  "TRT"
  )

  expect_error(
    make_subjects(dm, vs = "not a dataframe"),
    "vs must be a data.frame"
  )
  expect_error(
    make_subjects(dm, vs = list(a = 1)),
    "vs must be a data.frame"
  )

  incomplete_vs <- tibble::tribble(
    ~USUBJID, ~VSSTRESN,
    "001",    170
  )
  expect_error(
    make_subjects(dm, incomplete_vs),
    "Missing columns in vs: VSTESTCD"
  )
})


test_that("make_subjects validates subject_filter and keep", {
  dm <- make_subjects_dm()

  expect_error(
    make_subjects(dm, subject_filter = 1),
    "subject_filter must be a character value"
  )
  expect_error(
    make_subjects(dm, subject_filter = NULL),
    "subject_filter must not be NULL"
  )
  expect_error(
    make_subjects(dm, keep = 1),
    "keep must be a character value"
  )
})


test_that("make_subjects requires VSDTC when VSBLFL is absent", {
  dm <- tibble::tribble(
    ~USUBJID, ~SEX, ~ACTARMCD, ~RFSTDTC,
    "001",    "M",  "TRT",     "2020-01-15"
  )
  vs <- tibble::tribble(
    ~USUBJID, ~VSTESTCD, ~VSSTRESN,
    "001",    "HEIGHT",  170
  )

  expect_error(
    make_subjects(dm, vs),
    "When 'VSBLFL' is not available in vs, 'VSDTC' must be present for baseline determination",
    fixed = TRUE
  )
})


test_that("make_subjects requires RFSTDTC when deriving baseline from VSDTC", {
  dm <- tibble::tribble(
    ~USUBJID, ~SEX, ~ACTARMCD,
    "001",    "M",  "TRT"
  )
  vs <- tibble::tribble(
    ~USUBJID, ~VSTESTCD, ~VSSTRESN, ~VSDTC,
    "001",    "HEIGHT",  170,       "2020-01-01"
  )

  expect_error(
    make_subjects(dm, vs),
    "Baseline covariates cannot be determined",
    fixed = TRUE
  )
})


# Basic structure and filtering -------------------------------------------

test_that("make_subjects returns expected structure without vs", {
  dm <- make_subjects_dm()
  result <- make_subjects(dm)

  expect_s3_class(result, "data.frame")
  expect_equal(names(result)[1], "ID")
  expect_true(all(c(
    "ID", "USUBJID", "SEX", "RACE", "ETHNIC", "COUNTRY", "AGE",
    "ACTARMCD", "RFXSTDTC", "RFSTDTC"
  ) %in% names(result)))
  expect_false("HEIGHT" %in% names(result))
  expect_false("WEIGHT" %in% names(result))
  expect_false("BMI" %in% names(result))
})


test_that("make_subjects applies default ACTARMCD filter", {
  dm <- make_subjects_dm()
  result <- make_subjects(dm)

  expect_equal(nrow(result), 2)
  expect_equal(result$USUBJID, c("SUBJ-001", "SUBJ-002"))
  expect_false(any(result$ACTARMCD %in% c("SCRNFAIL", "NOTTRT")))
})


test_that("make_subjects assigns sequential IDs after filtering", {
  dm <- make_subjects_dm()
  result <- make_subjects(dm)

  expect_equal(result$ID, c(1, 2))
  expect_equal(names(result)[1], "ID")

  result_all <- make_subjects(dm, subject_filter = "TRUE")
  expect_equal(result_all$ID, seq_len(nrow(result_all)))
})


test_that("make_subjects respects custom subject_filter", {
  dm <- make_subjects_dm()

  result_all <- make_subjects(dm, subject_filter = "TRUE")
  expect_equal(nrow(result_all), 4)

  result_f <- make_subjects(dm, subject_filter = "SEX == 'F'")
  expect_equal(result_f$USUBJID, c("SUBJ-002", "SUBJ-004"))
  expect_equal(result_f$SEX, c(1, 1))

  result_trt <- make_subjects(dm, subject_filter = "ACTARMCD == 'TRT'")
  expect_equal(result_trt$USUBJID, c("SUBJ-001", "SUBJ-002"))
})


test_that("make_subjects warns when subject_filter returns no rows", {
  dm <- make_subjects_dm()

  expect_warning(
    result <- make_subjects(dm, subject_filter = "USUBJID == 'NONE'"),
    "The subject_filter 'USUBJID == 'NONE'' returned no entries.",
    fixed = TRUE
  )
  expect_equal(nrow(result), 0)
  expect_true(all(c("ID", "USUBJID", "SEX") %in% names(result)))

  expect_no_warning(make_subjects(dm, subject_filter = "TRUE"))
})


# Demographics and age ----------------------------------------------------

test_that("make_subjects calculates AGE from BRTHDTC and RFICDTC", {
  dm <- make_subjects_dm()
  result <- make_subjects(dm)

  expect_equal(result$AGE, c(50, 40))
})


test_that("make_subjects preserves non-missing AGE and fills NA", {
  dm <- make_subjects_dm() %>%
    mutate(AGE = c(99, NA_real_, 10, NA_real_))

  result <- make_subjects(dm, subject_filter = "TRUE")
  expect_equal(result$AGE[result$USUBJID == "SUBJ-001"], 99)
  expect_equal(result$AGE[result$USUBJID == "SUBJ-002"], 40)
})


test_that("make_subjects recodes SEX to 0/1", {
  dm <- make_subjects_dm()
  result <- make_subjects(dm, subject_filter = "TRUE")

  expect_equal(result$SEX, c(0, 1, 0, 1))
})


test_that("make_subjects warns on invalid SEX values", {
  dm <- make_subjects_dm() %>%
    mutate(SEX = c("M", "F", "X", "F"))

  expect_warning(
    result <- make_subjects(dm, subject_filter = "TRUE"),
    "Invalid sex values converted to NA: X"
  )
  expect_true(is.na(result$SEX[result$USUBJID == "SUBJ-003"]))
})


# keep parameter ----------------------------------------------------------

test_that("make_subjects keeps additional dm columns", {
  dm <- make_subjects_dm() %>%
    mutate(
      CUSTOM1 = c("A", "B", "C", "D"),
      CUSTOM2 = c("W", "X", "Y", "Z")
    )

  result <- make_subjects(dm, keep = "CUSTOM1")
  expect_true("CUSTOM1" %in% names(result))
  expect_equal(result$CUSTOM1, c("A", "B"))
  expect_false("CUSTOM2" %in% names(result))

  result_multi <- make_subjects(dm, keep = c("CUSTOM1", "CUSTOM2"))
  expect_true(all(c("CUSTOM1", "CUSTOM2") %in% names(result_multi)))
  expect_equal(result_multi$CUSTOM2, c("W", "X"))
})


test_that("make_subjects silently drops unknown keep columns", {
  dm <- make_subjects_dm()
  result <- make_subjects(dm, keep = "NOT_A_COLUMN")

  expect_false("NOT_A_COLUMN" %in% names(result))
  expect_equal(nrow(result), 2)
})


test_that("make_subjects keep = NULL is the default", {
  dm <- make_subjects_dm()
  result_null <- make_subjects(dm, keep = NULL)
  result_default <- make_subjects(dm)

  expect_equal(names(result_null), names(result_default))
})


# VS baselines ------------------------------------------------------------

test_that("make_subjects derives HEIGHT, WEIGHT, BMI from VSBLFL", {
  dm <- make_subjects_dm()
  vs <- make_subjects_vs_blfl()
  result <- make_subjects(dm, vs)

  expect_true(all(c("HEIGHT", "WEIGHT", "BMI") %in% names(result)))
  expect_equal(result$WEIGHT[result$USUBJID == "SUBJ-001"], 70)
  expect_equal(result$HEIGHT[result$USUBJID == "SUBJ-001"], 175)
  expect_equal(
    result$BMI[result$USUBJID == "SUBJ-001"],
    70 / (175 / 100)^2,
    tolerance = 1e-6
  )

  # Post-baseline flagged N rows are ignored
  expect_false(any(result$WEIGHT == 71, na.rm = TRUE))
})


test_that("make_subjects derives baseline from VSDTC before RFSTDTC", {
  dm <- make_subjects_dm()
  vs <- make_subjects_vs_dates()
  result <- make_subjects(dm, vs)

  expect_equal(result$WEIGHT[result$USUBJID == "SUBJ-001"], 70)
  expect_equal(result$HEIGHT[result$USUBJID == "SUBJ-001"], 175)
  expect_equal(result$WEIGHT[result$USUBJID == "SUBJ-002"], 65)

  # Same-day and post-RFSTDTC values are excluded (strict <)
  expect_false(any(result$WEIGHT %in% c(99, 90), na.rm = TRUE))
})


test_that("make_subjects averages multiple baseline VS rows", {
  dm <- tibble::tribble(
    ~USUBJID, ~SEX, ~ACTARMCD, ~RFSTDTC,
    "S1",     "M",  "TRT",     "2020-01-15"
  )
  vs <- tibble::tribble(
    ~USUBJID, ~VSTESTCD, ~VSSTRESN, ~VSBLFL,
    "S1",     "WEIGHT",  70,        "Y",
    "S1",     "WEIGHT",  80,        "Y",
    "S1",     "HEIGHT",  170,       "Y",
    "S1",     "HEIGHT",  180,       "Y"
  )

  result <- make_subjects(dm, vs)
  expect_equal(result$WEIGHT, 75)
  expect_equal(result$HEIGHT, 175)
  expect_equal(result$BMI, 75 / (175 / 100)^2, tolerance = 1e-6)
})


test_that("make_subjects handles missing HEIGHT or WEIGHT without BMI", {
  dm <- tibble::tribble(
    ~USUBJID, ~SEX, ~ACTARMCD, ~RFSTDTC,
    "S1",     "M",  "TRT",     "2020-01-15",
    "S2",     "F",  "TRT",     "2020-01-15"
  )
  vs <- tibble::tribble(
    ~USUBJID, ~VSTESTCD, ~VSSTRESN, ~VSBLFL,
    "S1",     "HEIGHT",  170,       "Y",
    "S2",     "WEIGHT",  60,        "Y"
  )

  result <- make_subjects(dm, vs)
  expect_equal(result$HEIGHT[result$USUBJID == "S1"], 170)
  expect_true(is.na(result$WEIGHT[result$USUBJID == "S1"]))
  expect_true(is.na(result$BMI[result$USUBJID == "S1"]))

  expect_equal(result$WEIGHT[result$USUBJID == "S2"], 60)
  expect_true(is.na(result$HEIGHT[result$USUBJID == "S2"]))
  expect_true(is.na(result$BMI[result$USUBJID == "S2"]))
})


test_that("make_subjects leaves HEIGHT/WEIGHT NA when subject has no VS", {
  dm <- tibble::tribble(
    ~USUBJID, ~SEX, ~ACTARMCD, ~RFSTDTC,
    "S1",     "M",  "TRT",     "2020-01-15",
    "S2",     "F",  "TRT",     "2020-01-15"
  )
  vs <- tibble::tribble(
    ~USUBJID, ~VSTESTCD, ~VSSTRESN, ~VSBLFL,
    "S1",     "HEIGHT",  170,       "Y",
    "S1",     "WEIGHT",  70,        "Y"
  )

  result <- make_subjects(dm, vs)
  expect_equal(result$HEIGHT[result$USUBJID == "S1"], 170)
  expect_true(is.na(result$HEIGHT[result$USUBJID == "S2"]))
  expect_true(is.na(result$WEIGHT[result$USUBJID == "S2"]))
  expect_true(is.na(result$BMI[result$USUBJID == "S2"]))
})


test_that("make_subjects ignores non-HEIGHT/WEIGHT VSTESTCD", {
  dm <- tibble::tribble(
    ~USUBJID, ~SEX, ~ACTARMCD, ~RFSTDTC,
    "S1",     "M",  "TRT",     "2020-01-15"
  )
  vs <- tibble::tribble(
    ~USUBJID, ~VSTESTCD, ~VSSTRESN, ~VSBLFL,
    "S1",     "HEIGHT",  170,       "Y",
    "S1",     "WEIGHT",  70,        "Y",
    "S1",     "SYSBP",   120,       "Y"
  )

  result <- make_subjects(dm, vs)
  expect_false("SYSBP" %in% names(result))
  expect_equal(result$HEIGHT, 170)
  expect_equal(result$WEIGHT, 70)
})


test_that("make_subjects prefers VSBLFL over VSDTC when both present", {
  dm <- tibble::tribble(
    ~USUBJID, ~SEX, ~ACTARMCD, ~RFSTDTC,
    "S1",     "M",  "TRT",     "2020-01-15"
  )
  vs <- tibble::tribble(
    ~USUBJID, ~VSTESTCD, ~VSSTRESN, ~VSBLFL, ~VSDTC,
    "S1",     "WEIGHT",  70,        "Y",     "2020-01-20",
    "S1",     "HEIGHT",  175,       "Y",     "2020-01-20",
    "S1",     "WEIGHT",  60,        "N",     "2020-01-01",
    "S1",     "HEIGHT",  160,       "N",     "2020-01-01"
  )

  # VSBLFL == Y wins even if VSDTC is after RFSTDTC
  result <- make_subjects(dm, vs)
  expect_equal(result$WEIGHT, 70)
  expect_equal(result$HEIGHT, 175)
})
