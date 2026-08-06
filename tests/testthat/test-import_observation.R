# Helper function to create a minimal nif object for testing
create_test_nif <- function() {
  tibble::tribble(
    ~USUBJID,    ~ID, ~ANALYTE, ~CMT, ~AMT, ~DOSE, ~DTC,                          ~NTIME, ~TIME, ~PARENT, ~METABOLITE, ~EVID, ~MDV, ~DV, ~IMPUTATION,
    "SUBJ-001",  1,   "DRUG",   1,   100,  100,   ymd_hms("2023-01-01 08:00:00"), 0,      0,     "DRUG",  FALSE,       1,     1,    NA,  "",
    "SUBJ-002",  2,   "DRUG",   1,   150,  150,   ymd_hms("2023-01-01 08:00:00"), 0,      0,     "DRUG",  FALSE,       1,     1,    NA,  ""
  ) %>%
    nif()
}


# Helper function to create test raw observation data
create_test_raw <- function(with_dtc = TRUE, with_ntime = TRUE) {
  df <- tibble::tribble(
    ~USUBJID,    ~DV_VALUE,
    "SUBJ-001",  10.5,
    "SUBJ-001",  5.2,
    "SUBJ-002",  8.7
  )

  if (with_dtc) {
    df$OBS_DTC <- ymd_hms(c(
      "2023-01-01 10:00:00",
      "2023-01-01 14:00:00",
      "2023-01-01 10:00:00"
    ))
  }

  if (with_ntime) {
    df$OBS_NTIME <- c(2, 6, 2)
  }

  return(df)
}


test_that("import_observation validates input parameters correctly", {
  expect_error(
    import_observation(
      data.frame(),
      create_test_raw(),
      analyte = "TEST",
      dv_field = "DV_VALUE"
    ),
    "Input must be a nif object"
  )

  test_nif <- create_test_nif()

  expect_error(
    import_observation(
      test_nif,
      create_test_raw(),
      analyte = "TEST"
    )
  )

  expect_error(
    import_observation(
      test_nif,
      create_test_raw(with_dtc = FALSE, with_ntime = FALSE),
      analyte = "TEST",
      dv_field = "DV_VALUE"
    ),
    "ERROR: One of the time fields .* must be present"
  )

  expect_error(
    import_observation(
      test_nif,
      create_test_raw(),
      analyte = "TEST",
      parent = "DRUG",
      cmt = 2,
      dv_field = "DV_VALUE",
      dtc_field = "OBS_DTC",
      observation_filter = "USUBJID == 'NON-EXISTENT'"
    ),
    "The observation_filter .* returned no entries"
  )
})


test_that("import_observation rejects missing DV or USUBJID fields", {
  test_nif <- create_test_nif()
  test_raw <- create_test_raw(with_dtc = TRUE, with_ntime = FALSE)

  expect_error(
    import_observation(
      test_nif,
      test_raw,
      analyte = "TEST",
      parent = "DRUG",
      cmt = 2,
      dv_field = "MISSING_DV",
      dtc_field = "OBS_DTC"
    ),
    "DV field .* and USUBJID field .* must both be present"
  )

  expect_error(
    import_observation(
      test_nif,
      test_raw,
      analyte = "TEST",
      parent = "DRUG",
      cmt = 2,
      dv_field = "DV_VALUE",
      usubjid_field = "MISSING_ID",
      dtc_field = "OBS_DTC"
    ),
    "DV field .* and USUBJID field .* must both be present"
  )
})


test_that("import_observation rejects invalid argument types", {
  test_nif <- create_test_nif()
  test_raw <- create_test_raw()

  expect_error(
    import_observation(
      test_nif,
      test_raw,
      analyte = 1,
      dv_field = "DV_VALUE",
      dtc_field = "OBS_DTC"
    )
  )

  expect_error(
    import_observation(
      test_nif,
      test_raw,
      analyte = "TEST",
      cmt = "two",
      dv_field = "DV_VALUE",
      dtc_field = "OBS_DTC"
    )
  )

  expect_error(
    import_observation(
      test_nif,
      test_raw,
      analyte = "TEST",
      parent = "DRUG",
      cmt = 2,
      dv_field = "DV_VALUE",
      dtc_field = "OBS_DTC",
      debug = "yes"
    )
  )
})


test_that("import_observation correctly handles DTC field", {
  test_nif <- create_test_nif()
  test_raw <- create_test_raw(with_dtc = TRUE, with_ntime = FALSE)

  result <- import_observation(
    test_nif,
    test_raw,
    analyte = "TEST",
    parent = "DRUG",
    cmt = 2,
    dv_field = "DV_VALUE",
    dtc_field = "OBS_DTC",
    ntime_field = NULL
  )

  expect_s3_class(result, "nif")
  expect_equal(nrow(result), nrow(test_nif) + nrow(test_raw))

  obs_records <- result %>% filter(EVID == 0)
  expect_equal(nrow(obs_records), nrow(test_raw))
  expect_equal(unique(obs_records$ANALYTE), "TEST")
  expect_equal(unique(obs_records$PARENT), "DRUG")
  expect_equal(unique(obs_records$CMT), 2)

  # Order-safe: match on subject and DV rather than positional DTC vectors
  compared <- obs_records %>%
    select(USUBJID, DV, DTC) %>%
    inner_join(
      test_raw %>% transmute(USUBJID, DV = DV_VALUE, OBS_DTC),
      by = c("USUBJID", "DV")
    )

  expect_equal(nrow(compared), nrow(test_raw))
  expect_equal(compared$DTC, compared$OBS_DTC)
})


test_that("import_observation preserves DTC regardless of raw row order", {
  test_nif <- create_test_nif()
  test_raw <- tibble::tribble(
    ~USUBJID,    ~DV_VALUE, ~OBS_DTC,
    "SUBJ-002",  8.7,       ymd_hms("2023-01-01 10:00:00"),
    "SUBJ-001",  5.2,       ymd_hms("2023-01-01 14:00:00"),
    "SUBJ-001",  10.5,      ymd_hms("2023-01-01 10:00:00")
  )

  result <- import_observation(
    test_nif,
    test_raw,
    analyte = "TEST",
    parent = "DRUG",
    cmt = 2,
    dv_field = "DV_VALUE",
    dtc_field = "OBS_DTC",
    ntime_field = NULL
  )

  compared <- result %>%
    filter(EVID == 0) %>%
    select(USUBJID, DV, DTC) %>%
    inner_join(
      test_raw %>% transmute(USUBJID, DV = DV_VALUE, OBS_DTC),
      by = c("USUBJID", "DV")
    )

  expect_equal(nrow(compared), nrow(test_raw))
  expect_equal(compared$DTC, compared$OBS_DTC)
})


test_that("import_observation lubrifies character DTC strings", {
  test_nif <- create_test_nif()
  test_raw <- tibble::tribble(
    ~USUBJID,    ~DV_VALUE, ~OBS_DTC,
    "SUBJ-001",  10.5,      "2023-01-01T10:00",
    "SUBJ-001",  5.2,       "2023-01-01T14:00",
    "SUBJ-002",  8.7,       "2023-01-01T10:00"
  )

  result <- import_observation(
    test_nif,
    test_raw,
    analyte = "TEST",
    parent = "DRUG",
    cmt = 2,
    dv_field = "DV_VALUE",
    dtc_field = "OBS_DTC",
    ntime_field = NULL
  )

  obs_records <- result %>% filter(EVID == 0)
  expect_s3_class(obs_records$DTC, "POSIXct")
  expect_false(any(is.na(obs_records$DTC)))
})


test_that("import_observation correctly handles NTIME field", {
  test_nif <- create_test_nif()
  test_raw <- create_test_raw(with_dtc = FALSE, with_ntime = TRUE)

  result <- import_observation(
    test_nif,
    test_raw,
    analyte = "TEST",
    parent = "DRUG",
    cmt = 2,
    dv_field = "DV_VALUE",
    dtc_field = NULL,
    ntime_field = "OBS_NTIME"
  )

  obs_records <- result %>% filter(EVID == 0)
  expect_equal(nrow(obs_records), nrow(test_raw))

  compared_ntime <- obs_records %>%
    select(USUBJID, DV, NTIME) %>%
    inner_join(
      test_raw %>% transmute(USUBJID, DV = DV_VALUE, OBS_NTIME),
      by = c("USUBJID", "DV")
    )
  expect_equal(compared_ntime$NTIME, compared_ntime$OBS_NTIME)

  expect_false(any(is.na(obs_records$DTC)))
  expect_true(all(obs_records$IMPUTATION == "DTC derived from OBS_NTIME"))

  expected <- test_raw %>%
    left_join(
      test_nif %>%
        filter(EVID == 1) %>%
        select(USUBJID, ADMIN_DTC = DTC),
      by = "USUBJID"
    ) %>%
    mutate(EXPECTED_DTC = ADMIN_DTC + duration(hours = OBS_NTIME)) %>%
    select(USUBJID, DV = DV_VALUE, EXPECTED_DTC)

  compared_dtc <- obs_records %>%
    select(USUBJID, DV, DTC) %>%
    inner_join(expected, by = c("USUBJID", "DV"))

  expect_equal(compared_dtc$DTC, compared_dtc$EXPECTED_DTC)
})


test_that("import_observation prefers DTC when both dtc_field and ntime_field are set", {
  test_nif <- create_test_nif()
  test_raw <- create_test_raw(with_dtc = TRUE, with_ntime = TRUE)

  result <- import_observation(
    test_nif,
    test_raw,
    analyte = "TEST",
    parent = "DRUG",
    cmt = 2,
    dv_field = "DV_VALUE",
    dtc_field = "OBS_DTC",
    ntime_field = "OBS_NTIME"
  )

  obs_records <- result %>% filter(EVID == 0)

  compared <- obs_records %>%
    select(USUBJID, DV, DTC, NTIME) %>%
    inner_join(
      test_raw %>% transmute(USUBJID, DV = DV_VALUE, OBS_DTC, OBS_NTIME),
      by = c("USUBJID", "DV")
    )

  expect_equal(compared$DTC, compared$OBS_DTC)
  expect_equal(compared$NTIME, compared$OBS_NTIME)
  expect_false(any(grepl("DTC derived from", obs_records$IMPUTATION)))
})


test_that("import_observation automatically assigns compartment when cmt is NULL", {
  test_nif <- create_test_nif()
  test_raw <- create_test_raw()

  expect_message(
    result <- import_observation(
      test_nif,
      test_raw,
      analyte = "TEST",
      parent = "DRUG",
      cmt = NULL,
      dv_field = "DV_VALUE",
      dtc_field = "OBS_DTC",
      ntime_field = "OBS_NTIME",
      silent = FALSE
    ),
    "Compartment for TEST set to"
  )

  obs_records <- result %>% filter(EVID == 0)
  expect_equal(unique(obs_records$CMT), max(test_nif$CMT) + 1)
})


test_that("import_observation silent suppresses compartment message", {
  test_nif <- create_test_nif()
  test_raw <- create_test_raw()

  expect_no_message(
    import_observation(
      test_nif,
      test_raw,
      analyte = "TEST",
      parent = "DRUG",
      cmt = NULL,
      dv_field = "DV_VALUE",
      dtc_field = "OBS_DTC",
      silent = TRUE
    )
  )
})


test_that("import_observation automatically determines parent when parent is NULL", {
  test_nif <- create_test_nif()
  test_raw <- create_test_raw()

  # Case 1: analyte matches an existing ANALYTE in nif
  expect_message(
    result1 <- import_observation(
      test_nif,
      test_raw,
      analyte = "DRUG",
      parent = NULL,
      cmt = 2,
      dv_field = "DV_VALUE",
      dtc_field = "OBS_DTC",
      ntime_field = "OBS_NTIME",
      silent = FALSE
    ),
    NA
  )

  obs_records1 <- result1 %>% filter(EVID == 0)
  expect_equal(unique(obs_records1$PARENT), "DRUG")

  # Case 2: analyte doesn't match existing ANALYTE — parent guessed
  result2 <- import_observation(
    test_nif,
    test_raw,
    analyte = "METABOLITE",
    parent = NULL,
    cmt = 3,
    dv_field = "DV_VALUE",
    dtc_field = "OBS_DTC",
    ntime_field = "OBS_NTIME",
    silent = TRUE
  )

  obs_records2 <- result2 %>% filter(EVID == 0)
  expect_equal(unique(obs_records2$PARENT), "DRUG")
})


test_that("import_observation correctly joins subject data", {
  test_nif <- create_test_nif() %>%
    mutate(
      BL_WT = c(70, 85),
      BL_HT = c(175, 180)
    )

  test_raw <- create_test_raw()

  result <- import_observation(
    test_nif,
    test_raw,
    analyte = "TEST",
    parent = "DRUG",
    cmt = 2,
    dv_field = "DV_VALUE",
    dtc_field = "OBS_DTC",
    ntime_field = "OBS_NTIME",
    keep = c("BL_WT", "BL_HT")
  )

  obs_records <- result %>% filter(EVID == 0)

  for (id in unique(test_raw$USUBJID)) {
    obs_subject <- obs_records %>% filter(USUBJID == id)
    nif_subject <- test_nif %>% filter(USUBJID == id)

    expect_equal(unique(obs_subject$BL_WT), unique(nif_subject$BL_WT))
    expect_equal(unique(obs_subject$BL_HT), unique(nif_subject$BL_HT))
  }
})


test_that("import_observation requires keep fields to exist on nif", {
  test_nif <- create_test_nif()
  test_raw <- create_test_raw(with_dtc = TRUE, with_ntime = FALSE) %>%
    mutate(VISIT = c("V1", "V2", "V1"))

  expect_error(
    import_observation(
      test_nif,
      test_raw,
      analyte = "TEST",
      parent = "DRUG",
      cmt = 2,
      dv_field = "DV_VALUE",
      dtc_field = "OBS_DTC",
      keep = "VISIT"
    ),
    "Missing required fields: VISIT"
  )
})


test_that("import_observation preserves keep fields from nif through import", {
  test_nif <- create_test_nif() %>%
    mutate(COHORT = c("A", "B"))

  test_raw <- create_test_raw(with_dtc = TRUE, with_ntime = FALSE)

  result <- import_observation(
    test_nif,
    test_raw,
    analyte = "TEST",
    parent = "DRUG",
    cmt = 2,
    dv_field = "DV_VALUE",
    dtc_field = "OBS_DTC",
    keep = "COHORT"
  )

  expect_true("COHORT" %in% names(result))

  obs_records <- result %>% filter(EVID == 0)
  expect_equal(
    obs_records %>%
      distinct(USUBJID, COHORT) %>%
      arrange(USUBJID),
    test_nif %>%
      distinct(USUBJID, COHORT) %>%
      arrange(USUBJID)
  )
})


test_that("import_observation correctly sets debug fields", {
  test_nif <- create_test_nif()
  test_raw <- create_test_raw()

  result1 <- import_observation(
    test_nif,
    test_raw,
    analyte = "TEST",
    parent = "DRUG",
    cmt = 2,
    dv_field = "DV_VALUE",
    dtc_field = "OBS_DTC",
    ntime_field = "OBS_NTIME",
    debug = FALSE
  )

  result2 <- import_observation(
    test_nif,
    test_raw,
    analyte = "TEST",
    parent = "DRUG",
    cmt = 2,
    dv_field = "DV_VALUE",
    dtc_field = "OBS_DTC",
    ntime_field = "OBS_NTIME",
    debug = TRUE
  )

  expect_false(all(c("SRC_DOMAIN", "SRC_SEQ") %in% names(result1)))
  expect_true(all(c("SRC_DOMAIN", "SRC_SEQ") %in% names(result2)))

  obs_records <- result2 %>% filter(EVID == 0)
  expect_equal(unique(obs_records$SRC_DOMAIN), "IMPORT")
  expect_true(all(is.na(obs_records$SRC_SEQ)))
})


test_that("import_observation applies observation_filter", {
  test_nif <- create_test_nif()
  test_raw <- create_test_raw(with_dtc = TRUE, with_ntime = FALSE)

  result <- import_observation(
    test_nif,
    test_raw,
    analyte = "TEST",
    parent = "DRUG",
    cmt = 2,
    dv_field = "DV_VALUE",
    dtc_field = "OBS_DTC",
    observation_filter = "USUBJID == 'SUBJ-001'"
  )

  obs_records <- result %>% filter(EVID == 0)
  expect_equal(nrow(obs_records), 2)
  expect_true(all(obs_records$USUBJID == "SUBJ-001"))
  expect_equal(nrow(result), nrow(test_nif) + 2)
})


test_that("import_observation supports custom usubjid_field", {
  test_nif <- create_test_nif()
  test_raw <- create_test_raw(with_dtc = TRUE, with_ntime = FALSE) %>%
    rename(SUBJECT = USUBJID)

  result <- import_observation(
    test_nif,
    test_raw,
    analyte = "TEST",
    parent = "DRUG",
    cmt = 2,
    dv_field = "DV_VALUE",
    dtc_field = "OBS_DTC",
    usubjid_field = "SUBJECT"
  )

  obs_records <- result %>% filter(EVID == 0)
  expect_equal(nrow(obs_records), nrow(test_raw))
  expect_true(all(obs_records$USUBJID %in% c("SUBJ-001", "SUBJ-002")))
})


test_that("import_observation sets observation defaults and MDV", {
  test_nif <- create_test_nif()
  test_raw <- tibble::tribble(
    ~USUBJID,    ~DV_VALUE, ~OBS_DTC,
    "SUBJ-001",  10.5,      ymd_hms("2023-01-01 10:00:00"),
    "SUBJ-001",  NA,        ymd_hms("2023-01-01 14:00:00"),
    "SUBJ-002",  8.7,       ymd_hms("2023-01-01 10:00:00")
  )

  result <- import_observation(
    test_nif,
    test_raw,
    analyte = "TEST",
    parent = "DRUG",
    cmt = 2,
    dv_field = "DV_VALUE",
    dtc_field = "OBS_DTC"
  )

  obs_records <- result %>% filter(EVID == 0)

  expect_true(all(obs_records$EVID == 0))
  expect_true(all(obs_records$AMT == 0))
  expect_true(all(obs_records$METABOLITE == FALSE))
  expect_equal(obs_records$MDV, as.numeric(is.na(obs_records$DV)))
  expect_equal(sum(obs_records$MDV), 1)
})


test_that("import_observation drops raw subjects not in nif via inner join", {
  test_nif <- create_test_nif()
  test_raw <- tibble::tribble(
    ~USUBJID,    ~DV_VALUE, ~OBS_DTC,
    "SUBJ-001",  10.5,      ymd_hms("2023-01-01 10:00:00"),
    "SUBJ-999",  99.0,      ymd_hms("2023-01-01 11:00:00")
  )

  result <- import_observation(
    test_nif,
    test_raw,
    analyte = "TEST",
    parent = "DRUG",
    cmt = 2,
    dv_field = "DV_VALUE",
    dtc_field = "OBS_DTC"
  )

  obs_records <- result %>% filter(EVID == 0)
  expect_equal(nrow(obs_records), 1)
  expect_equal(obs_records$USUBJID, "SUBJ-001")
  expect_false("SUBJ-999" %in% obs_records$USUBJID)
})


test_that("import_observation derives TIME after import", {
  test_nif <- create_test_nif()
  test_raw <- create_test_raw(with_dtc = TRUE, with_ntime = FALSE)

  result <- import_observation(
    test_nif,
    test_raw,
    analyte = "TEST",
    parent = "DRUG",
    cmt = 2,
    dv_field = "DV_VALUE",
    dtc_field = "OBS_DTC"
  )

  obs_records <- result %>% filter(EVID == 0)
  expect_true("TIME" %in% names(result))
  expect_false(any(is.na(obs_records$TIME)))
  expect_true(all(obs_records$TIME >= 0))
})
