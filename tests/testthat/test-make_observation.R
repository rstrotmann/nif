make_test_sdtm1 <- function() {
  temp = list(
    dm = tibble::tribble(
      ~USUBJID, ~DOMAIN, ~SEX, ~ACTARMCD,             ~RFXSTDTC,              ~RFSTDTC, ~ACTARM,  ~STUDYID,
      "1",    "DM",  "M",       "A", "2024-01-01T08:00:00", "2024-01-01T08:00:00", "Arm A", "Study 1",
      "2",    "DM",  "M",       "A", "2024-01-01T08:00:00", "2024-01-01T08:00:00", "Arm A", "Study 1",
      "3",    "DM",  "M",       "A", "2024-01-01T08:00:00", "2024-01-01T08:00:00", "Arm A", "Study 1",
      "4",    "DM",  "M",       "A", "2024-01-01T08:00:00", "2024-01-01T08:00:00", "Arm A", "Study 1"
    ) %>%
      mutate(RFENDTC = "2024-01-02T08:00:00"),

    vs = tibble::tribble(
      ~USUBJID, ~DOMAIN, ~VSTESTCD, ~VSSTRESN,                ~VSDTC,
      "1",    "VS",  "HEIGHT",       100, "2024-01-01T08:00:00",
      "2",    "VS",  "HEIGHT",       100, "2024-01-01T08:00:00",
      "3",    "VS",  "HEIGHT",       100, "2024-01-01T08:00:00",
      "4",    "VS",  "HEIGHT",       100, "2024-01-01T08:00:00"
    ),

    lb = tibble::tribble(
      ~USUBJID, ~DOMAIN, ~LBSPEC, ~LBTESTCD, ~LBSTRESN,                ~LBDTC,
      "1",    "LB", "SERUM",   "CREAT",       100, "2024-01-01T08:00:00",
      "2",    "LB", "SERUM",   "CREAT",       100, "2024-01-01T08:00:00",
      "3",    "LB", "SERUM",   "CREAT",       100, "2024-01-01T08:00:00",
      "4",    "LB", "SERUM",   "CREAT",       100, "2024-01-01T08:00:00"
    ),

    # ex = tibble::tribble(
    #   ~USUBJID, ~DOMAIN, ~EXDOSE, ~EXTRT,              ~EXSTDTC,              ~EXENDTC,
    #   "1",    "EX",       1,    "A", "2024-01-01T08:00:00", "2024-01-01T08:00:00",
    #   "2",    "EX",       1,    "A", "2024-01-01T08:00:00", "2024-01-01T08:00:00",
    #   "3",    "EX",       1,    "A", "2024-01-01T08:00:00", "2024-01-01T08:00:00",
    #   "4",    "EX",       1,    "A", "2024-01-01T08:00:00", "2024-01-01T08:00:00"
    # ) %>% mutate(EXSEQ = row_number()),

    ex = tibble::tribble(
      ~USUBJID, ~DOMAIN, ~EXDOSE, ~EXTRT,              ~EXSTDTC,              ~EXENDTC, ~EXSEQ,
      "1",    "EX",       1,    "A", "2024-01-01T08:00:00", "2024-01-01T08:00:00",     1L,
      "2",    "EX",       1,    "A", "2024-01-01T08:00:00", "2024-01-01T08:00:00",     2L,
      "3",    "EX",       1,    "A", "2024-01-01T08:00:00", "2024-01-01T08:00:00",     3L,
      "4",    "EX",       1,    "A", "2024-01-01T08:00:00", "2024-01-01T08:00:00",     4L
    ),

    pc = tibble::tribble(
      ~USUBJID, ~DOMAIN, ~PCTESTCD,                ~PCDTC, ~PCSTRESN, ~PCSPEC,     ~PCTEST, ~PCELTM,
      "1",    "PC",       "A", "2024-01-01T08:00:00",       100,  "Spec", "Analyte A",  "PT0H",
      "2",    "PC",       "A", "2024-01-01T08:00:00",       100,  "Spec", "Analyte A",  "PT0H",
      "3",    "PC",       "A", "2024-01-01T08:00:00",       100,  "Spec", "Analyte A",  "PT0H",
      "4",    "PC",       "A", "2024-01-01T08:00:00",       100,  "Spec", "Analyte A",  "PT0H"
    ))

  return(new_sdtm(temp))
}


test_that("make_observation works", {
  sdtm <- make_test_sdtm1()
  expect_no_error(
    make_observation(sdtm, "pc", "A", ntime_method = "ELTM",
                     silent = TRUE))
})


test_that("make_observation issues warning if observation filter returns no observations", {
  sdtm <- make_test_sdtm1()
  # Should issue warning when silent is FALSE or NULL
  expect_error(
    make_observation(sdtm, "pc", "A", observation_filter = "FALSE",
                     ntime_method = "ELTM", silent = FALSE),
    "The observation_filter 'FALSE' returned no entries."
  )
})


test_that("make_observation works with coding table", {
  sdtm <- examplinib_poc
  suppressMessages(expect_no_error(
    make_observation(
      sdtm, "pp", "LAMZNPT", DTC_field = "PPRFTDTC",
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
  )
  )
})


# Additional tests for make_observation function
test_that("make_observation handles different domains correctly", {
  sdtm <- make_test_sdtm1()

  # Test with PC domain
  pc_obs <- make_observation(sdtm, "pc", "A", ntime_method = "ELTM",
                             silent = TRUE)
  expect_equal(unique(pc_obs$ANALYTE), "A")

  # Test with LB domain
  lb_obs <- make_observation(sdtm, "lb", "CREAT", ntime_method = "ELTM",
                             silent = TRUE)
  expect_equal(unique(lb_obs$ANALYTE), "CREAT")
})


test_that("make_observation applies DV factor correctly", {
  sdtm <- make_test_sdtm1()

  # Default factor (1)
  default_obs <- make_observation(sdtm, "pc", "A", ntime_method = "ELTM",
                                  silent = TRUE)
  expect_equal(unique(default_obs$DV), 100)

  # Custom factor (2)
  factor_obs <- make_observation(sdtm, "pc", "A", factor = 2,
                                 ntime_method = "ELTM", silent = TRUE)
  expect_equal(unique(factor_obs$DV), 200)
})


test_that("make_observation handles custom DV_field correctly", {
  sdtm <- make_test_sdtm1()

  # Create test data with a custom field
  sdtm$domains$pc$PCCUSTOM <- sdtm$domains$pc$PCSTRESN * 2

  custom_obs <- make_observation(sdtm, "pc", "A", DV_field = "PCCUSTOM",
                                 ntime_method = "ELTM", silent = TRUE)
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
    TESTCD_field = "PCCUSTOMCD",
    observation_filter = "USUBJID == 2",
    ntime_method = "ELTM",
    silent = TRUE)

  expect_equal(nrow(custom_obs), 1)
  expect_equal(custom_obs$USUBJID, "2")
})


test_that("make_observation handles custom DTC_field correctly", {
  sdtm <- make_test_sdtm1()

  # Create test data with a custom date field
  custom_date <- "2024-01-02 12:00:00"
  sdtm$domains$pc$PCCUSTOMDTC <- custom_date

  custom_obs <- make_observation(
    sdtm, "pc", "A", DTC_field = "PCCUSTOMDTC",
    ntime_method = "ELTM", silent = TRUE)
  expect_equal(unique(as.character(custom_obs$DTC)), custom_date)
})


test_that("make_observation creates proper output fields", {
  sdtm <- make_test_sdtm1()

  result <- make_observation(
    sdtm, "pc", "A", cmt = 5, parent = "PARENT", metabolite = TRUE,
    ntime_method = "ELTM", silent = TRUE)

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
  ntime_lookup <- data.frame(
    PCELTM = c("PT0H", "PT1H", "PT2H"),
    NTIME = c(0, 1, 2)
  )

  result <- make_observation(sdtm, "pc", "A", NTIME_lookup = ntime_lookup,
                             ntime_method = "ELTM", silent = TRUE)
  expect_equal(unique(result$NTIME), 0) # Should match the PT0H value
})


test_that("make_observation validates inputs correctly", {
  sdtm <- make_test_sdtm1()

  # Test with invalid domain
  expect_error(
    make_observation(sdtm, "invalid_domain", "A",
                     ntime_method = "ELTM", silent = TRUE),
    "Domain 'invalid_domain' not found in sdtm object"
  )

  # Test with non-sdtm object
  not_sdtm <- list(domains = list(pc = data.frame()))
  expect_error(
    make_observation(not_sdtm, "pc", "A",
                     ntime_method = "ELTM", silent = TRUE),
    "sdtm must be an sdtm object"
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
                     ntime_method = "ELTM", silent = TRUE),
    "DV field 'PCSTRESN' not found in domain and no coding table provided"
  )

  # Should work with a coding table
  coding_table <- tibble::tribble(
    ~PCTESTCD, ~DV,
    "A", 50
  )

  expect_no_error(
    result <- make_observation(
      sdtm, "pc", "A", coding_table = coding_table,
      ntime_method = "ELTM", silent = TRUE)
  )

  # Check the coded values
  expect_equal(unique(result$DV), 50)
})


test_that("make_observation sets MDV correctly for missing values", {
  sdtm <- make_test_sdtm1()

  # Create test data with some missing values
  sdtm$domains$pc$PCSTRESN[1] <- NA

  result <- make_observation(
    sdtm, "pc", "A", ntime_method = "ELTM", silent = TRUE)

  # MDV should be 1 for the row with NA
  expect_equal(result$MDV[result$USUBJID == "1"], 1)

  # MDV should be 0 for rows with values
  expect_equal(result$MDV[result$USUBJID == "2"], 0)
})


# Tests for add_observation function
test_that("add_observation basic functionality works", {
  # Create a base nif object with administration data
  base_nif <- new_nif() %>%
    add_administration(examplinib_sad, "EXAMPLINIB", analyte = "RS2023")

  # Test basic observation adding
  expect_no_error({
    nif_with_obs <- base_nif %>%
      add_observation(examplinib_sad, "pc", "RS2023",
                      ntime_method = "ELTM", silent = TRUE)
  })

  # Verify the observation was added correctly
  expect_true(any(nif_with_obs$EVID == 0), "Observations should have EVID = 0")
  expect_equal(unique(nif_with_obs$ANALYTE), "RS2023")
})


# test_that("add_observation requires administration first", {
#   # Try to add observation to empty nif
#   expect_error(
#     new_nif() %>% add_observation(examplinib_sad, "pc", "RS2023"),
#     "Please add at least one administration first!"
#   )
# })
