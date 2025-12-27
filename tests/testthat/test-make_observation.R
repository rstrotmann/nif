make_test_sdtm1 <- function() {
  temp <- list(
    dm = tibble::tribble(
      ~USUBJID, ~DOMAIN, ~SEX, ~ACTARMCD, ~RFXSTDTC, ~RFSTDTC, ~ACTARM, ~STUDYID,
      "1", "DM", "M", "A", "2024-01-01T08:00:00", "2024-01-01T08:00:00", "Arm A", "Study 1",
      "2", "DM", "M", "A", "2024-01-01T08:00:00", "2024-01-01T08:00:00", "Arm A", "Study 1",
      "3", "DM", "M", "A", "2024-01-01T08:00:00", "2024-01-01T08:00:00", "Arm A", "Study 1",
      "4", "DM", "M", "A", "2024-01-01T08:00:00", "2024-01-01T08:00:00", "Arm A", "Study 1"
    ) %>%
      mutate(RFENDTC = "2024-01-02T08:00:00"),
    vs = tibble::tribble(
      ~USUBJID, ~DOMAIN, ~VSTESTCD, ~VSSTRESN, ~VSDTC,
      "1", "VS", "HEIGHT", 100, "2024-01-01T08:00:00",
      "2", "VS", "HEIGHT", 100, "2024-01-01T08:00:00",
      "3", "VS", "HEIGHT", 100, "2024-01-01T08:00:00",
      "4", "VS", "HEIGHT", 100, "2024-01-01T08:00:00"
    ),
    lb = tibble::tribble(
      ~USUBJID, ~DOMAIN, ~LBSPEC, ~LBTESTCD, ~LBSTRESN, ~LBDTC,
      "1", "LB", "SERUM", "CREAT", 100, "2024-01-01T08:00:00",
      "2", "LB", "SERUM", "CREAT", 100, "2024-01-01T08:00:00",
      "3", "LB", "SERUM", "CREAT", 100, "2024-01-01T08:00:00",
      "4", "LB", "SERUM", "CREAT", 100, "2024-01-01T08:00:00"
    ),
    ex = tibble::tribble(
      ~USUBJID, ~DOMAIN, ~EXDOSE, ~EXTRT, ~EXSTDTC, ~EXENDTC, ~EXSEQ,
      "1", "EX", 1, "A", "2024-01-01T08:00:00", "2024-01-01T08:00:00", 1L,
      "2", "EX", 1, "A", "2024-01-01T08:00:00", "2024-01-01T08:00:00", 2L,
      "3", "EX", 1, "A", "2024-01-01T08:00:00", "2024-01-01T08:00:00", 3L,
      "4", "EX", 1, "A", "2024-01-01T08:00:00", "2024-01-01T08:00:00", 4L
    ),
    pc = tibble::tribble(
      ~USUBJID, ~DOMAIN, ~PCTESTCD, ~PCDTC, ~PCSTRESN, ~PCSPEC, ~PCTEST, ~PCELTM,
      "1", "PC", "A", "2024-01-01T08:00:00", 100, "Spec", "Analyte A", "PT0H",
      "2", "PC", "A", "2024-01-01T08:00:00", 100, "Spec", "Analyte A", "PT0H",
      "3", "PC", "A", "2024-01-01T08:00:00", 100, "Spec", "Analyte A", "PT0H",
      "4", "PC", "A", "2024-01-01T08:00:00", 100, "Spec", "Analyte A", "PT0H"
    )
  )

  return(sdtm(temp))
}


test_that("make_observation works", {
  sdtm <- make_test_sdtm1()
  expect_no_error(
    make_observation(sdtm, "pc", "A", ntime_method = "ELTM", silent = TRUE)
  )
})


test_that("make_observation issues warning if observation filter returns no observations", {
  sdtm <- make_test_sdtm1()
  # Should issue warning when silent is FALSE or NULL
  expect_error(
    make_observation(sdtm, "pc", "A",
      observation_filter = "FALSE",
      ntime_method = "ELTM", silent = FALSE
    ),
    "No entries found after applying observation filter!"
  )
})


test_that("make_observation works with coding table", {
  sdtm <- examplinib_poc
  suppressMessages(expect_no_error(
    make_observation(
      sdtm, "pp", "LAMZNPT",
      dtc_field = "PPRFTDTC",
      observation_filter = "PPSEQ == 7",
      ntime_method = "ELTM",
      coding_table = tibble::tribble(
        ~PPSTRESN, ~DV,
        3, 33,
        4, 44,
        5, 55,
        6, 66
      )
    )
  ))
})


# Additional tests for make_observation function
test_that("make_observation handles different domains correctly", {
  sdtm <- make_test_sdtm1()

  # Test with PC domain
  pc_obs <- make_observation(sdtm, "pc", "A",
    ntime_method = "ELTM",
    silent = TRUE
  )
  expect_equal(unique(pc_obs$ANALYTE), "A")

  # Test with LB domain
  lb_obs <- make_observation(sdtm, "lb", "CREAT",
    ntime_method = "ELTM",
    silent = TRUE
  )
  expect_equal(unique(lb_obs$ANALYTE), "CREAT")
})


test_that("make_observation applies DV factor correctly", {
  sdtm <- make_test_sdtm1()

  # Default factor (1)
  default_obs <- make_observation(sdtm, "pc", "A",
    ntime_method = "ELTM",
    silent = TRUE
  )
  expect_equal(unique(default_obs$DV), 100)

  # Custom factor (2)
  factor_obs <- make_observation(sdtm, "pc", "A",
    factor = 2,
    ntime_method = "ELTM", silent = TRUE
  )
  expect_equal(unique(factor_obs$DV), 200)
})


test_that("make_observation handles custom DV_field correctly", {
  sdtm <- make_test_sdtm1()

  # Create test data with a custom field
  sdtm$domains$pc$PCCUSTOM <- sdtm$domains$pc$PCSTRESN * 2

  custom_obs <- make_observation(sdtm, "pc", "A",
    dv_field = "PCCUSTOM",
    ntime_method = "ELTM", silent = TRUE
  )
  expect_equal(unique(custom_obs$DV), 200)
})


test_that("make_observation handles custom TESTCD_field correctly", {
  sdtm <- make_test_sdtm1()

  # Create test data with a custom TESTCD field
  sdtm$domains$pc$PCCUSTOMCD <- "CUSTOM"
  sdtm$domains$pc$PCCUSTOMCD[2] <- "A"

  # Should only select rows where PCCUSTOMCD equals "A"
  custom_obs <- make_observation(
    sdtm, "pc", "A",
    testcd_field = "PCCUSTOMCD",
    observation_filter = "USUBJID == 2",
    ntime_method = "ELTM",
    silent = TRUE
  )

  expect_equal(nrow(custom_obs), 1)
  expect_equal(custom_obs$USUBJID, "2")
})


test_that("make_observation handles custom DTC_field correctly", {
  sdtm <- make_test_sdtm1()

  # Create test data with a custom date field
  custom_date <- "2024-01-02 12:00:00"
  sdtm$domains$pc$PCCUSTOMDTC <- custom_date

  custom_obs <- make_observation(
    sdtm, "pc", "A",
    dtc_field = "PCCUSTOMDTC",
    ntime_method = "ELTM", silent = TRUE
  )
  expect_equal(unique(as.character(custom_obs$DTC)), custom_date)
})


test_that("make_observation creates proper output fields", {
  sdtm <- make_test_sdtm1()

  result <- make_observation(
    sdtm, "pc", "A",
    cmt = 5, parent = "PARENT", metabolite = TRUE,
    ntime_method = "ELTM", silent = TRUE
  )

  # Check required fields
  expect_true("ANALYTE" %in% names(result))
  expect_true("DV" %in% names(result))
  expect_true("TIME" %in% names(result))
  expect_true("NTIME" %in% names(result))
  expect_true("CMT" %in% names(result))
  expect_true("EVID" %in% names(result))
  expect_true("MDV" %in% names(result))

  # Check values of specific fields
  expect_equal(unique(result$ANALYTE), "A")
  expect_equal(unique(result$CMT), 5)
  expect_equal(unique(result$PARENT), "PARENT")
  expect_equal(unique(result$METABOLITE), TRUE)
  expect_equal(unique(result$EVID), 0) # Observations have EVID=0
  expect_equal(unique(result$MDV), 0) # DV exists so MDV=0
})


test_that("make_observation handles NTIME lookup correctly", {
  sdtm <- make_test_sdtm1()

  # Create a custom NTIME lookup
  ntl <- data.frame(
    PCELTM = c("PT0H", "PT1H", "PT2H"),
    NTIME = c(0, 1, 2)
  )

  result <- make_observation(sdtm, "pc", "A",
    ntime_lookup = ntl,
    ntime_method = "ELTM", silent = TRUE
  )
  expect_equal(unique(result$NTIME), 0) # Should match the PT0H value
})


test_that("make_observation validates inputs correctly", {
  sdtm <- make_test_sdtm1()

  # Test with invalid domain
  expect_error(
    make_observation(sdtm, "invalid_domain", "A",
      ntime_method = "ELTM", silent = TRUE
    ),
    "Expected domain missing in sdtm object: invalid_domain"
  )

  # Test with non-sdtm object
  not_sdtm <- list(domains = list(pc = data.frame()))
  expect_error(
    make_observation(not_sdtm, "pc", "A",
      ntime_method = "ELTM", silent = TRUE
    ),
    "Input must be a sdtm object"
  )
})


test_that("make_observation handles missing DV field with coding table", {
  sdtm <- make_test_sdtm1()

  # Create a version without PCSTRESN
  modified_pc <- sdtm$domains$pc
  modified_pc$PCSTRESN <- NULL
  sdtm$domains$pc <- modified_pc

  # Should error when no coding table is provided
  expect_error(
    make_observation(sdtm, "pc", "A",
      ntime_method = "ELTM", silent = TRUE
    ),
    "DV field 'PCSTRESN' not found in domain and no coding table provided"
  )

  # Should work with a coding table
  coding_table <- tibble::tribble(
    ~PCTESTCD, ~DV,
    "A", 50
  )

  expect_no_error(
    result <- make_observation(
      sdtm, "pc", "A",
      coding_table = coding_table,
      ntime_method = "ELTM", silent = TRUE
    )
  )

  # Check the coded values
  expect_equal(unique(result$DV), 50)
})


test_that("make_observation sets MDV correctly for missing values", {
  sdtm <- make_test_sdtm1()

  # Create test data with some missing values
  sdtm$domains$pc$PCSTRESN[1] <- NA

  result <- make_observation(
    sdtm, "pc", "A",
    ntime_method = "ELTM", silent = TRUE
  )

  # MDV should be 1 for the row with NA
  expect_equal(result$MDV[result$USUBJID == "1"], 1)

  # MDV should be 0 for rows with values
  expect_equal(result$MDV[result$USUBJID == "2"], 0)
})


# Tests for add_observation function
test_that("add_observation basic functionality works", {
  # Create a base nif object with administration data
  base_nif <- nif() %>%
    add_administration(
      examplinib_sad, "EXAMPLINIB",
      analyte = "RS2023", silent = TRUE
    )

  # Test basic observation adding
  expect_no_error({
    nif_with_obs <- base_nif %>%
      add_observation(
        examplinib_sad, "pc", "RS2023",
        ntime_method = "ELTM", silent = TRUE
      )
  })

  # Verify the observation was added correctly
  expect_true(any(nif_with_obs$EVID == 0), "Observations should have EVID = 0")
  expect_equal(unique(nif_with_obs$ANALYTE), "RS2023")
})


test_that("make_observation works with ntime_method = 'TPT'", {
  # Create test data
  pc_data <- tribble(
    ~USUBJID, ~DOMAIN, ~PCTESTCD, ~PCTPT, ~PCDTC, ~PCSTRESN,
    "SUBJ1", "PC", "TEST1", "PRE-DOSE", "2023-01-01T08:00:00", 0,
    "SUBJ1", "PC", "TEST1", "1H POST-DOSE", "2023-01-01T09:00:00", 10,
    "SUBJ1", "PC", "TEST1", "2.5 HOURS POST DOSE", "2023-01-01T10:30:00", 20,
    "SUBJ1", "PC", "TEST1", "30 MIN POST DOSE", "2023-01-01T08:30:00", 5,
    "SUBJ1", "PC", "TEST1", "0.5H TO 2H POST-DOSE", "2023-01-01T10:00:00", 15,
    "SUBJ1", "PC", "TEST1", "DAY1 - 2 HOURS POST ADMINISTRATION", "2023-01-02T10:00:00", 25
  )

  dm_data <- tribble(
    ~USUBJID, ~RFSTDTC,     ~ACTARMCD, ~SEX,
    "SUBJ1",  "2023-01-01", "ACTIVE",  "M"
  )

  vs_data <- tribble(
    ~USUBJID, ~VSTESTCD, ~VSSTRESN, ~VSDTC,
    "SUBJ1", "WEIGHT", 70, "2023-01-01T08:00:00"
  )

  # Create SDTM object
  sdtm <- sdtm(list(
    pc = pc_data,
    dm = dm_data,
    vs = vs_data
  ))

  # Test make_observation
  result <- make_observation(
    sdtm = sdtm,
    domain = "PC",
    testcd = "TEST1",
    ntime_method = "TPT"
  )

  # Verify results
  expect_equal(nrow(result), 6)
  expect_equal(
    result$NTIME,
    c(0, 1, 2.5, 0.5, 2, 2) # Expected NTIME values based on PCTPT
  )
  expect_equal(result$DV, c(0, 10, 20, 5, 15, 25))
  expect_equal(result$ANALYTE, rep("TEST1", 6))
  expect_equal(result$CMT, rep(NA, 6))
  expect_equal(result$EVID, rep(0, 6))
  expect_equal(result$MDV, rep(0, 6))
})


test_that("make_observation handles missing NTIME values", {
  # Create test data with some invalid time points
  pc_data <- tribble(
    ~USUBJID, ~DOMAIN, ~PCTESTCD, ~PCTPT, ~PCDTC, ~PCSTRESN,
    "SUBJ1", "PC", "TEST1", "PRE-DOSE", "2023-01-01T08:00:00", 0,
    "SUBJ1", "PC", "TEST1", "INVALID TIME", "2023-01-01T09:00:00", 10,
    "SUBJ1", "PC", "TEST1", "1H POST-DOSE", "2023-01-01T10:00:00", 20
  )

  dm_data <- tribble(
    ~USUBJID, ~RFSTDTC, ~ACTARMCD, ~SEX,
    "SUBJ1", "2023-01-01", "ACTIVE", "M"
  )

  vs_data <- tribble(
    ~USUBJID, ~VSTESTCD, ~VSSTRESN, ~VSDTC,
    "SUBJ1", "WEIGHT", 70, "2023-01-01"
  )

  # Create SDTM object
  sdtm <- sdtm(list(
    pc = pc_data,
    dm = dm_data,
    vs = vs_data
  ))

  # Test make_observation
  result <- make_observation(
    sdtm = sdtm,
    domain = "PC",
    testcd = "TEST1",
    ntime_method = "TPT"
  )

  # Verify results
  expect_equal(nrow(result), 3)
  expect_equal(
    result$NTIME,
    c(0, NA, 1) # Middle value should be NA due to invalid time point
  )
})


test_that("make_observation handles different time point formats", {
  # Create test data with various time point formats
  pc_data <- tribble(
    ~USUBJID, ~DOMAIN, ~PCTESTCD, ~PCTPT, ~PCDTC, ~PCSTRESN,
    "SUBJ1", "PC", "TEST1", "PREDOSE", "2023-01-01T08:00:00", 0,
    "SUBJ1", "PC", "TEST1", "1 HR POST-DOSE", "2023-01-01T09:00:00", 10,
    "SUBJ1", "PC", "TEST1", "45 MINS POST DOSE", "2023-01-01T08:45:00", 15,
    "SUBJ1", "PC", "TEST1", "1.5 HOURS POST ADMINISTRATION", "2023-01-01T09:30:00", 20
  )

  dm_data <- tribble(
    ~USUBJID, ~RFSTDTC, ~ACTARMCD, ~SEX,
    "SUBJ1", "2023-01-01", "ACTIVE", "M"
  )

  vs_data <- tribble(
    ~USUBJID, ~VSTESTCD, ~VSSTRESN, ~VSDTC,
    "SUBJ1", "WEIGHT", 70, "2023-01-01"
  )

  # Create SDTM object
  sdtm <- sdtm(list(
    pc = pc_data,
    dm = dm_data,
    vs = vs_data
  ))

  # Test make_observation
  result <- make_observation(
    sdtm = sdtm,
    domain = "PC",
    testcd = "TEST1",
    ntime_method = "TPT"
  )

  # Verify results
  expect_equal(nrow(result), 4)
  expect_equal(
    result$NTIME,
    c(0, 1, 0.75, 1.5) # Expected NTIME values based on PCTPT
  )
})


# Tests for cat and scat parameters
test_that("make_observation filters by cat parameter correctly", {
  # Create test data with PCCAT field
  pc_data <- tribble(
    ~USUBJID, ~DOMAIN, ~PCTESTCD, ~PCCAT, ~PCSTRESN, ~PCDTC, ~PCELTM,
    "SUBJ1", "PC", "TEST1", "PK", 100, "2023-01-01T08:00:00", "PT0H",
    "SUBJ1", "PC", "TEST1", "PK", 200, "2023-01-01T09:00:00", "PT1H",
    "SUBJ1", "PC", "TEST1", "PD", 300, "2023-01-01T10:00:00", "PT2H",
    "SUBJ1", "PC", "TEST1", "PD", 400, "2023-01-01T11:00:00", "PT3H"
  )

  dm_data <- tribble(
    ~USUBJID, ~RFSTDTC,     ~ACTARMCD, ~SEX,
    "SUBJ1",  "2023-01-01", "ACTIVE",  "M"
  )

  vs_data <- tribble(
    ~USUBJID, ~VSTESTCD, ~VSSTRESN, ~VSDTC,
    "SUBJ1", "WEIGHT", 70, "2023-01-01"
  )

  sdtm <- sdtm(list(
    pc = pc_data,
    dm = dm_data,
    vs = vs_data
  ))

  # Test filtering by cat = "PK"
  result_pk <- make_observation(
    sdtm = sdtm,
    domain = "PC",
    testcd = "TEST1",
    cat = "PK",
    ntime_method = "ELTM",
    silent = TRUE
  )

  expect_equal(nrow(result_pk), 2)
  expect_equal(unique(result_pk$DV), c(100, 200))
  expect_equal(unique(result_pk$NTIME), c(0, 1))

  # Test filtering by cat = "PD"
  result_pd <- make_observation(
    sdtm = sdtm,
    domain = "PC",
    testcd = "TEST1",
    cat = "PD",
    ntime_method = "ELTM",
    silent = TRUE
  )

  expect_equal(nrow(result_pd), 2)
  expect_equal(unique(result_pd$DV), c(300, 400))
  expect_equal(unique(result_pd$NTIME), c(2, 3))
})


test_that("make_observation filters by scat parameter correctly", {
  # Create test data with PCSCAT field
  pc_data <- tribble(
    ~USUBJID, ~DOMAIN, ~PCTESTCD, ~PCSCAT, ~PCSTRESN, ~PCDTC, ~PCELTM,
    "SUBJ1", "PC", "TEST1", "PLASMA", 100, "2023-01-01T08:00:00", "PT0H",
    "SUBJ1", "PC", "TEST1", "PLASMA", 200, "2023-01-01T09:00:00", "PT1H",
    "SUBJ1", "PC", "TEST1", "URINE", 300, "2023-01-01T10:00:00", "PT2H",
    "SUBJ1", "PC", "TEST1", "URINE", 400, "2023-01-01T11:00:00", "PT3H"
  )

  dm_data <- tribble(
    ~USUBJID, ~RFSTDTC,     ~ACTARMCD, ~SEX,
    "SUBJ1",  "2023-01-01", "ACTIVE",  "M"
  )

  vs_data <- tribble(
    ~USUBJID, ~VSTESTCD, ~VSSTRESN, ~VSDTC,
    "SUBJ1", "WEIGHT", 70, "2023-01-01"
  )

  sdtm <- sdtm(list(
    pc = pc_data,
    dm = dm_data,
    vs = vs_data
  ))

  # Test filtering by scat = "PLASMA"
  result_plasma <- make_observation(
    sdtm = sdtm,
    domain = "PC",
    testcd = "TEST1",
    scat = "PLASMA",
    ntime_method = "ELTM",
    silent = TRUE
  )

  expect_equal(nrow(result_plasma), 2)
  expect_equal(unique(result_plasma$DV), c(100, 200))
  expect_equal(unique(result_plasma$NTIME), c(0, 1))

  # Test filtering by scat = "URINE"
  result_urine <- make_observation(
    sdtm = sdtm,
    domain = "PC",
    testcd = "TEST1",
    scat = "URINE",
    ntime_method = "ELTM",
    silent = TRUE
  )

  expect_equal(nrow(result_urine), 2)
  expect_equal(unique(result_urine$DV), c(300, 400))
  expect_equal(unique(result_urine$NTIME), c(2, 3))
})


test_that("make_observation filters by both cat and scat parameters correctly", {
  # Create test data with both PCCAT and PCSCAT fields
  pc_data <- tribble(
    ~USUBJID, ~DOMAIN, ~PCTESTCD, ~PCCAT, ~PCSCAT, ~PCSTRESN, ~PCDTC, ~PCELTM,
    "SUBJ1", "PC", "TEST1", "PK", "PLASMA", 100, "2023-01-01T08:00:00", "PT0H",
    "SUBJ1", "PC", "TEST1", "PK", "URINE", 200, "2023-01-01T09:00:00", "PT1H",
    "SUBJ1", "PC", "TEST1", "PD", "PLASMA", 300, "2023-01-01T10:00:00", "PT2H",
    "SUBJ1", "PC", "TEST1", "PD", "URINE", 400, "2023-01-01T11:00:00", "PT3H"
  )

  dm_data <- tribble(
    ~USUBJID, ~RFSTDTC,     ~ACTARMCD, ~SEX,
    "SUBJ1",  "2023-01-01", "ACTIVE",  "M"
  )

  vs_data <- tribble(
    ~USUBJID, ~VSTESTCD, ~VSSTRESN, ~VSDTC,
    "SUBJ1", "WEIGHT", 70, "2023-01-01"
  )

  sdtm <- sdtm(list(
    pc = pc_data,
    dm = dm_data,
    vs = vs_data
  ))

  # Test filtering by both cat = "PK" and scat = "PLASMA"
  result_pk_plasma <- make_observation(
    sdtm = sdtm,
    domain = "PC",
    testcd = "TEST1",
    cat = "PK",
    scat = "PLASMA",
    ntime_method = "ELTM",
    silent = TRUE
  )

  expect_equal(nrow(result_pk_plasma), 1)
  expect_equal(result_pk_plasma$DV, 100)
  expect_equal(result_pk_plasma$NTIME, 0)

  # Test filtering by both cat = "PD" and scat = "URINE"
  result_pd_urine <- make_observation(
    sdtm = sdtm,
    domain = "PC",
    testcd = "TEST1",
    cat = "PD",
    scat = "URINE",
    ntime_method = "ELTM",
    silent = TRUE
  )

  expect_equal(nrow(result_pd_urine), 1)
  expect_equal(result_pd_urine$DV, 400)
  expect_equal(result_pd_urine$NTIME, 3)
})


test_that("make_observation handles custom cat and scat field names", {
  # Create test data with custom field names
  pc_data <- tribble(
    ~USUBJID, ~DOMAIN, ~PCTESTCD, ~PCCAT, ~PCSCAT, ~PCSTRESN, ~PCDTC, ~PCELTM,
    "SUBJ1", "PC", "TEST1", "CUSTOM1", "SPEC1", 100, "2023-01-01T08:00:00", "PT0H",
    "SUBJ1", "PC", "TEST1", "CUSTOM1", "SPEC2", 200, "2023-01-01T09:00:00", "PT1H",
    "SUBJ1", "PC", "TEST1", "CUSTOM2", "SPEC1", 300, "2023-01-01T10:00:00", "PT2H",
    "SUBJ1", "PC", "TEST1", "CUSTOM2", "SPEC2", 400, "2023-01-01T11:00:00", "PT3H"
  )

  dm_data <- tribble(
    ~USUBJID, ~RFSTDTC,     ~ACTARMCD, ~SEX,
    "SUBJ1",  "2023-01-01", "ACTIVE",  "M"
  )

  vs_data <- tribble(
    ~USUBJID, ~VSTESTCD, ~VSSTRESN, ~VSDTC,
    "SUBJ1", "WEIGHT", 70, "2023-01-01"
  )

  sdtm <- sdtm(list(
    pc = pc_data,
    dm = dm_data,
    vs = vs_data
  ))

  # Test with custom cat value
  result_custom_cat <- make_observation(
    sdtm = sdtm,
    domain = "PC",
    testcd = "TEST1",
    cat = "CUSTOM1",
    ntime_method = "ELTM",
    silent = TRUE
  )

  expect_equal(nrow(result_custom_cat), 2)
  expect_equal(unique(result_custom_cat$DV), c(100, 200))

  # Test with custom scat value
  result_custom_scat <- make_observation(
    sdtm = sdtm,
    domain = "PC",
    testcd = "TEST1",
    scat = "SPEC1",
    ntime_method = "ELTM",
    silent = TRUE
  )

  expect_equal(nrow(result_custom_scat), 2)
  expect_equal(unique(result_custom_scat$DV), c(100, 300))
})

test_that("make_observation handles cat and scat with different domains", {
  # Test with LB domain
  lb_data <- tribble(
    ~USUBJID, ~DOMAIN, ~LBTESTCD, ~LBCAT, ~LBSCAT, ~LBSTRESN, ~LBDTC, ~LBELTM,
    "SUBJ1", "LB", "CREAT", "CHEM", "SERUM", 1.2, "2023-01-01T08:00:00", "LT0H",
    "SUBJ1", "LB", "CREAT", "CHEM", "URINE", 2.1, "2023-01-01T09:00:00", "LT1H",
    "SUBJ1", "LB", "CREAT", "HEMA", "SERUM", 1.5, "2023-01-01T10:00:00", "LT2H"
  )

  dm_data <- tribble(
    ~USUBJID, ~RFSTDTC,     ~ACTARMCD, ~SEX,
    "SUBJ1",  "2023-01-01", "ACTIVE",  "M"
  )

  vs_data <- tribble(
    ~USUBJID, ~VSTESTCD, ~VSSTRESN, ~VSDTC,
    "SUBJ1", "WEIGHT", 70, "2023-01-01"
  )

  sdtm <- sdtm(list(
    lb = lb_data,
    dm = dm_data,
    vs = vs_data
  ))

  # Test filtering by LBCAT
  result_lb_cat <- make_observation(
    sdtm = sdtm,
    domain = "LB",
    testcd = "CREAT",
    cat = "CHEM",
    ntime_method = "ELTM",
    silent = TRUE
  )

  expect_equal(nrow(result_lb_cat), 2)
  expect_equal(unique(result_lb_cat$DV), c(1.2, 2.1))

  # Test filtering by LBSCAT
  result_lb_scat <- make_observation(
    sdtm = sdtm,
    domain = "LB",
    testcd = "CREAT",
    scat = "SERUM",
    ntime_method = "ELTM",
    silent = TRUE
  )

  expect_equal(nrow(result_lb_scat), 2)
  expect_equal(unique(result_lb_scat$DV), c(1.2, 1.5))
})


test_that("make_observation handles cat and scat with no matching values", {
  # Create test data
  sdtm <- sdtm(list(
    pc = tribble(
      ~USUBJID, ~DOMAIN, ~PCTESTCD, ~PCCAT, ~PCSCAT, ~PCSTRESN, ~PCDTC, ~PCELTM,
      "SUBJ1", "PC", "TEST1", "PK", "PLASMA", 100, "2023-01-01T08:00:00", "PT0H",
      "SUBJ1", "PC", "TEST1", "PK", "PLASMA", 200, "2023-01-01T09:00:00", "PT1H"
    ),

    dm = tribble(
      ~USUBJID, ~RFSTDTC,     ~ACTARMCD, ~SEX,
      "SUBJ1",  "2023-01-01", "ACTIVE",  "M"
    ),

    vs = tribble(
      ~USUBJID, ~VSTESTCD, ~VSSTRESN, ~VSDTC,
      "SUBJ1", "WEIGHT", 70, "2023-01-01"
    )
  ))

  # sdtm <- sdtm(list(
  #   pc = pc_data,
  #   dm = dm_data,
  #   vs = vs_data
  # ))

  # Test with cat value that doesn't exist
  expect_error(
    make_observation(
      sdtm = sdtm,
      domain = "PC",
      testcd = "TEST1",
      cat = "NONEXISTENT",
      ntime_method = "ELTM",
      silent = TRUE
    ),
    "NONEXISTENT not found in the PCCAT field!"
  )

  # Test with scat value that doesn't exist
  expect_error(
    make_observation(
      sdtm = sdtm,
      domain = "PC",
      testcd = "TEST1",
      scat = "NONEXISTENT",
      ntime_method = "ELTM",
      silent = TRUE
    ),
    "NONEXISTENT not found in the PCSCAT field!"
  )
})


test_that("make_observation handles NULL cat and scat parameters", {
  # Create test data
  pc_data <- tribble(
    ~USUBJID, ~DOMAIN, ~PCTESTCD, ~PCCAT, ~PCSCAT, ~PCSTRESN, ~PCDTC, ~PCELTM,
    "SUBJ1", "PC", "TEST1", "PK", "PLASMA", 100, "2023-01-01T08:00:00", "PT0H",
    "SUBJ1", "PC", "TEST1", "PD", "URINE", 200, "2023-01-01T09:00:00", "PT1H"
  )

  dm_data <- tribble(
    ~USUBJID, ~RFSTDTC,     ~ACTARMCD, ~SEX,
    "SUBJ1",  "2023-01-01", "ACTIVE",  "M"
  )

  vs_data <- tribble(
    ~USUBJID, ~VSTESTCD, ~VSSTRESN, ~VSDTC,
    "SUBJ1", "WEIGHT", 70, "2023-01-01"
  )

  sdtm <- sdtm(list(
    pc = pc_data,
    dm = dm_data,
    vs = vs_data
  ))

  # Test with NULL cat and scat (should return all rows)
  result_null_filters <- make_observation(
    sdtm = sdtm,
    domain = "PC",
    testcd = "TEST1",
    cat = NULL,
    scat = NULL,
    ntime_method = "ELTM",
    silent = TRUE
  )

  expect_equal(nrow(result_null_filters), 2)
  expect_equal(unique(result_null_filters$DV), c(100, 200))
})

test_that("make_observation handles omit_not_done parameter correctly", {
  # Use existing helper function and add STAT field
  sdtm <- make_test_sdtm1()

  # Add PCSTAT field to the PC domain
  sdtm$domains$pc$PCSTAT <- ""
  sdtm$domains$pc$PCSTAT[c(1, 3)] <- "NOT DONE" # Set 2 rows to "NOT DONE"

  # Test with omit_not_done = TRUE (default)
  result_omit_true <- make_observation(
    sdtm, "pc", "A",
    ntime_method = "ELTM",
    omit_not_done = TRUE,
    silent = TRUE
  )

  # Should exclude 2 rows with PCSTAT = "NOT DONE"
  expect_equal(nrow(result_omit_true), 2)
  expect_false(any(result_omit_true$PCSTAT == "NOT DONE", na.rm = TRUE))

  # Test with omit_not_done = FALSE
  result_omit_false <- make_observation(
    sdtm, "pc", "A",
    ntime_method = "ELTM",
    omit_not_done = FALSE,
    silent = TRUE
  )

  # Should include all 4 rows including "NOT DONE"
  expect_equal(nrow(result_omit_false), 4)
  expect_equal(sum(result_omit_false$PCSTAT == "NOT DONE", na.rm = TRUE), 2)
})

test_that("make_observation handles omit_not_done with different domains", {
  # Use existing helper function and add STAT field to LB domain
  sdtm <- make_test_sdtm1()

  # Add LBSTAT field to the LB domain
  sdtm$domains$lb$LBSTAT <- ""
  sdtm$domains$lb$LBSTAT[2] <- "NOT DONE" # Set 1 row to "NOT DONE"

  # Test with omit_not_done = TRUE for LB domain
  result_lb_omit_true <- make_observation(
    sdtm, "lb", "CREAT",
    ntime_method = "ELTM",
    omit_not_done = TRUE,
    silent = TRUE
  )

  # Should exclude 1 row with LBSTAT = "NOT DONE"
  expect_equal(nrow(result_lb_omit_true), 3)
  expect_false(any(result_lb_omit_true$LBSTAT == "NOT DONE", na.rm = TRUE))

  # Test with omit_not_done = FALSE for LB domain
  result_lb_omit_false <- make_observation(
    sdtm, "lb", "CREAT",
    ntime_method = "ELTM",
    omit_not_done = FALSE,
    silent = TRUE
  )

  # Should include all 4 rows including "NOT DONE"
  expect_equal(nrow(result_lb_omit_false), 4)
  expect_equal(sum(result_lb_omit_false$LBSTAT == "NOT DONE", na.rm = TRUE), 1)
})

test_that("make_observation handles omit_not_done when STAT field is missing", {
  # Create test SDTM without STAT field
  test_sdtm_no_stat <- list(
    dm = tibble::tribble(
      ~USUBJID, ~DOMAIN, ~SEX, ~ACTARMCD, ~RFXSTDTC, ~RFSTDTC, ~ACTARM, ~STUDYID,
      "1", "DM", "M", "A", "2024-01-01T08:00:00", "2024-01-01T08:00:00", "Arm A", "Study 1",
      "2", "DM", "M", "A", "2024-01-01T08:00:00", "2024-01-01T08:00:00", "Arm A", "Study 1"
    ) %>%
      mutate(RFENDTC = "2024-01-02T08:00:00"),
    vs = tibble::tribble(
      ~USUBJID, ~DOMAIN, ~VSTESTCD, ~VSSTRESN, ~VSDTC,
      "1", "VS", "HEIGHT", 100, "2024-01-01T08:00:00",
      "2", "VS", "HEIGHT", 100, "2024-01-01T08:00:00"
    ),
    pc = tibble::tribble(
      ~USUBJID, ~DOMAIN, ~PCTESTCD, ~PCDTC, ~PCSTRESN, ~PCSPEC, ~PCTEST, ~PCELTM,
      "1", "PC", "A", "2024-01-01T08:00:00", 100, "Spec", "Analyte A", "PT0H",
      "1", "PC", "A", "2024-01-01T09:00:00", 150, "Spec", "Analyte A", "PT1H",
      "2", "PC", "A", "2024-01-01T08:00:00", 110, "Spec", "Analyte A", "PT0H",
      "2", "PC", "A", "2024-01-01T09:00:00", 160, "Spec", "Analyte A", "PT1H"
    )
  ) %>%
    sdtm()

  # Test with omit_not_done = TRUE when no STAT field exists
  result_no_stat_omit_true <- make_observation(
    test_sdtm_no_stat, "pc", "A",
    ntime_method = "ELTM",
    omit_not_done = TRUE,
    silent = TRUE
  )

  # Should include all rows since no STAT field to filter on
  expect_equal(nrow(result_no_stat_omit_true), 4)

  # Test with omit_not_done = FALSE when no STAT field exists
  result_no_stat_omit_false <- make_observation(
    test_sdtm_no_stat, "pc", "A",
    ntime_method = "ELTM",
    omit_not_done = FALSE,
    silent = TRUE
  )

  # Should include all rows since no STAT field to filter on
  expect_equal(nrow(result_no_stat_omit_false), 4)
})

test_that("make_observation shows correct message when omit_not_done filters observations", {
  # Create test SDTM with STAT field and "NOT DONE" values
  test_sdtm_message <- list(
    dm = tibble::tribble(
      ~USUBJID, ~DOMAIN, ~SEX, ~ACTARMCD, ~RFXSTDTC, ~RFSTDTC, ~ACTARM, ~STUDYID,
      "1", "DM", "M", "A", "2024-01-01T08:00:00", "2024-01-01T08:00:00", "Arm A", "Study 1"
    ) %>%
      mutate(RFENDTC = "2024-01-02T08:00:00"),
    vs = tibble::tribble(
      ~USUBJID, ~DOMAIN, ~VSTESTCD, ~VSSTRESN, ~VSDTC,
      "1", "VS", "HEIGHT", 100, "2024-01-01T08:00:00"
    ),
    pc = tibble::tribble(
      ~USUBJID, ~DOMAIN, ~PCTESTCD, ~PCDTC, ~PCSTRESN, ~PCSPEC, ~PCTEST, ~PCELTM, ~PCSTAT,
      "1", "PC", "A", "2024-01-01T08:00:00", 100, "Spec", "Analyte A", "PT0H", "",
      "1", "PC", "A", "2024-01-01T09:00:00", 150, "Spec", "Analyte A", "PT1H", "NOT DONE",
      "1", "PC", "A", "2024-01-01T10:00:00", 200, "Spec", "Analyte A", "PT2H", "NOT DONE"
    )
  ) %>%
    sdtm()

  # Test that message is shown when silent = FALSE
  expect_message(
    make_observation(
      test_sdtm_message, "pc", "A",
      ntime_method = "ELTM",
      omit_not_done = TRUE,
      silent = FALSE
    ),
    "2 observations for A with PCSTAT of 'NOT DONE' omitted"
  )

  # Test that no message is shown when silent = TRUE
  expect_no_message(
    make_observation(
      test_sdtm_message, "pc", "A",
      ntime_method = "ELTM",
      omit_not_done = TRUE,
      silent = TRUE
    )
  )
})

test_that("make_observation validates omit_not_done parameter", {
  sdtm <- make_test_sdtm1()

  # Test with invalid omit_not_done parameter
  expect_error(
    make_observation(sdtm, "pc", "A", omit_not_done = "invalid"),
    "omit_not_done must be a logical value"
  )

  expect_error(
    make_observation(sdtm, "pc", "A", omit_not_done = 123),
    "omit_not_done must be a logical value"
  )

  expect_error(
    make_observation(sdtm, "pc", "A", omit_not_done = NULL),
    "omit_not_done must not be NULL"
  )

  # Test with valid omit_not_done parameters
  expect_no_error(
    make_observation(sdtm, "pc", "A", omit_not_done = TRUE, silent = TRUE)
  )

  expect_no_error(
    make_observation(sdtm, "pc", "A", omit_not_done = FALSE, silent = TRUE)
  )
})
