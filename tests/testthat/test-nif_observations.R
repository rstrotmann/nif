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
    make_observation(sdtm, "pc", "A", silent = TRUE))
})


test_that("make_observation issues warning if observation filter returns no observations", {
  sdtm <- make_test_sdtm1()
  # Should issue warning when silent is FALSE or NULL
  expect_error(
    make_observation(sdtm, "pc", "A", observation_filter = "FALSE", silent = FALSE),
    "The observation_filter 'FALSE' returned no entries."
  )
})


test_that("make_observation works with coding table", {
  sdtm <- examplinib_poc
  suppressMessages(expect_no_error(
    make_observation(
      sdtm, "pp", "LAMZNPT", DTC_field = "PPRFTDTC",
      observation_filter = "PPSEQ == 7",
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
  pc_obs <- make_observation(sdtm, "pc", "A", silent = TRUE)
  expect_equal(unique(pc_obs$ANALYTE), "A")

  # Test with LB domain
  lb_obs <- make_observation(sdtm, "lb", "CREAT", silent = TRUE)
  expect_equal(unique(lb_obs$ANALYTE), "CREAT")
})


test_that("make_observation applies DV factor correctly", {
  sdtm <- make_test_sdtm1()

  # Default factor (1)
  default_obs <- make_observation(sdtm, "pc", "A", silent = TRUE)
  expect_equal(unique(default_obs$DV), 100)

  # Custom factor (2)
  factor_obs <- make_observation(sdtm, "pc", "A", factor = 2, silent = TRUE)
  expect_equal(unique(factor_obs$DV), 200)
})


test_that("make_observation handles custom DV_field correctly", {
  sdtm <- make_test_sdtm1()

  # Create test data with a custom field
  sdtm$domains$pc$PCCUSTOM <- sdtm$domains$pc$PCSTRESN * 2

  custom_obs <- make_observation(sdtm, "pc", "A", DV_field = "PCCUSTOM", silent = TRUE)
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
    sdtm, "pc", "A", DTC_field = "PCCUSTOMDTC", silent = TRUE)
  expect_equal(unique(as.character(custom_obs$DTC)), custom_date)
})


test_that("make_observation creates proper output fields", {
  sdtm <- make_test_sdtm1()

  result <- make_observation(
    sdtm, "pc", "A", cmt = 5, parent = "PARENT", metabolite = TRUE,
    silent = TRUE)

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

  result <- make_observation(sdtm, "pc", "A", NTIME_lookup = ntime_lookup, silent = TRUE)
  expect_equal(unique(result$NTIME), 0) # Should match the PT0H value
})


test_that("make_observation validates inputs correctly", {
  sdtm <- make_test_sdtm1()

  # Test with invalid domain
  expect_error(
    make_observation(sdtm, "invalid_domain", "A", silent = TRUE),
    "Domain 'invalid_domain' not found in sdtm object"
  )

  # Test with non-sdtm object
  not_sdtm <- list(domains = list(pc = data.frame()))
  expect_error(
    make_observation(not_sdtm, "pc", "A", silent = TRUE),
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
    make_observation(sdtm, "pc", "A", silent = TRUE),
    "DV field 'PCSTRESN' not found in domain and no coding table provided"
  )

  # Should work with a coding table
  coding_table <- tibble::tribble(
    ~PCTESTCD, ~DV,
           "A", 50
  )

  expect_no_error(
    result <- make_observation(
      sdtm, "pc", "A", coding_table = coding_table, silent = TRUE)
  )

  # Check the coded values
  expect_equal(unique(result$DV), 50)
})


test_that("make_observation sets MDV correctly for missing values", {
  sdtm <- make_test_sdtm1()

  # Create test data with some missing values
  sdtm$domains$pc$PCSTRESN[1] <- NA

  result <- make_observation(
    sdtm, "pc", "A", silent = TRUE)

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
      add_observation(examplinib_sad, "pc", "RS2023", silent = TRUE)
  })

  # Verify the observation was added correctly
  expect_true(any(nif_with_obs$EVID == 0), "Observations should have EVID = 0")
  expect_equal(unique(nif_with_obs$ANALYTE), "RS2023")
})


test_that("add_observation requires administration first", {
  # Try to add observation to empty nif
  expect_error(
    new_nif() %>% add_observation(examplinib_sad, "pc", "RS2023"),
    "Please add at least one administration first!"
  )
})


test_that("add_observation warns about duplicate compartment", {
  # Create a base nif object with administration data where CMT=1
  base_nif <- new_nif() %>%
    add_administration(examplinib_sad, "EXAMPLINIB", analyte = "RS2023")

  # Try to add observation with the same compartment
  expect_warning(
    base_nif %>%
      add_observation(examplinib_sad, "pc", "RS2023", cmt = 1),
    "Compartment 1 is already assigned!"
  )
})


test_that("add_observation auto-assigns compartment if not specified", {
  # Create a base nif with administration data
  base_nif <- new_nif() %>%
    add_administration(examplinib_sad, "EXAMPLINIB", analyte = "RS2023")

  # Add observation without specifying CMT, capture messages
  expect_message(
    nif_with_obs <- base_nif %>%
      add_observation(examplinib_sad, "pc", "RS2023"),
    "Compartment for RS2023 was not specified and has been set to 2"
  )

  # Verify the compartment was auto-assigned
  expect_true(2 %in% unique(nif_with_obs$CMT))
})


test_that("add_observation auto-assigns parent if not specified", {
  # Create a base nif with administration data
  base_nif <- new_nif() %>%
    add_administration(examplinib_sad, "EXAMPLINIB", analyte = "RS2023")

  # Different observation analyte without specifying parent
  expect_message(
    nif_with_obs <- base_nif %>%
      add_observation(
        examplinib_sad, "pc", "RS2023", analyte = "DIFFERENT", cmt = 2),
    "Parent for DIFFERENT was set to RS2023!"
  )
})


test_that("add_observation properly uses observation_filter", {
  # Create a base nif with administration data
  base_nif <- new_nif() %>%
    add_administration(examplinib_sad, "EXAMPLINIB", analyte = "RS2023")

  # Add observation with filter that returns nothing
  expect_error(
    nif_with_filtered_obs <- base_nif %>% add_observation(
      examplinib_sad, "pc", "RS2023", cmt = 2,
      observation_filter = "PCTESTCD == 'NON_EXISTENT'"
    )
  )
})


test_that("add_observation works with factor parameter", {
  # Create a base nif with administration data
  base_nif <- new_nif() %>%
    add_administration(examplinib_sad, "EXAMPLINIB", analyte = "RS2023")

  # Add observation with normal factor
  nif_normal <- base_nif %>%
    add_observation(examplinib_sad, "pc", "RS2023", factor = 1, silent = TRUE)

  # Add observation with double factor
  nif_doubled <- base_nif %>%
    add_observation(examplinib_sad, "pc", "RS2023", factor = 2, silent = TRUE)

  # Get DV values for both
  dv_normal <- nif_normal %>%
    filter(EVID == 0) %>%
    pull(DV)

  dv_doubled <- nif_doubled %>%
    filter(EVID == 0) %>%
    pull(DV)

  # Verify the doubled values are twice the normal values
  if (length(dv_normal) > 0 && !all(is.na(dv_normal))) {
    expect_equal(dv_doubled, dv_normal * 2)
  }
})


test_that("add_observation handles metabolites correctly", {
  # Create a base nif with administration data
  base_nif <- new_nif() %>%
    add_administration(examplinib_sad, "EXAMPLINIB", analyte = "RS2023")

  # Add metabolite observation
  nif_with_metabolite <- base_nif %>%
    add_observation(
      examplinib_sad, "pc", "RS2023487A",
      parent = "RS2023",
      metabolite = TRUE,
      silent = TRUE
    )

  # Check if metabolite flag is set correctly
  metabolite_rows <- nif_with_metabolite %>%
    filter(ANALYTE == "RS2023487A") %>%
    pull(METABOLITE)

  expect_true(all(metabolite_rows))
})


test_that("add_observation works with custom NTIME_lookup", {
  # Create a base nif with administration data
  base_nif <- new_nif() %>%
    add_administration(examplinib_sad, "EXAMPLINIB", analyte = "RS2023")

  # Create custom NTIME lookup table
  custom_ntime <- data.frame(
    PCELTM = c("PT0H", "PT1H", "PT2H"),
    NTIME = c(0, 1, 2)
  )

  # Add observation with custom NTIME lookup
  expect_no_error({
    nif_with_custom_ntime <- base_nif %>%
      add_observation(
        examplinib_sad, "pc", "RS2023",
        NTIME_lookup = custom_ntime,
        silent = TRUE
      )
  })
})


test_that("add_observation handles debug mode correctly", {
  # Create a base nif with administration data
  base_nif <- new_nif() %>%
    add_administration(examplinib_sad, "EXAMPLINIB", analyte = "RS2023",
                       debug = TRUE)

  # Add observation in debug mode
  nif_debug <- base_nif %>%
    add_observation(examplinib_sad, "pc", "RS2023", debug = TRUE, silent = TRUE)

  # Check if debug columns are present
  expect_true("SRC_DOMAIN" %in% names(nif_debug))
  expect_true("SRC_SEQ" %in% names(nif_debug))
})


test_that("add_observation updates columns correctly", {
  # Create a base nif with administration data
  base_nif <- new_nif() %>%
    add_administration(examplinib_sad, "EXAMPLINIB", analyte = "RS2023")

  # Check columns before adding observation
  before_cols <- names(base_nif)

  # Add observation with additional columns to keep
  nif_with_extra <- base_nif %>%
    add_observation(examplinib_sad, "pc", "RS2023", keep = c("PCSPEC"), silent = TRUE)

  # Check if the extra column was kept
  after_cols <- names(nif_with_extra)

  # The difference should contain the kept column
  expect_true("PCSPEC" %in% after_cols)
})


test_that("add_observation handles include_day_in_ntime parameter", {
  # Create a base nif with administration data
  base_nif <- new_nif() %>%
    add_administration(examplinib_sad, "EXAMPLINIB", analyte = "RS2023")

  custom_sdtm <- examplinib_sad
  custom_sdtm$domains$pc$PCDY = 2

  # Add observation with include_day_in_ntime = TRUE
  nif_with_day <- base_nif %>%
    add_observation(
      custom_sdtm, "pc", "RS2023",
      include_day_in_ntime = TRUE,
      silent = TRUE
    )

  # Add observation with include_day_in_ntime = FALSE
  nif_without_day <- base_nif %>%
    add_observation(
      examplinib_sad, "pc", "RS2023",
      include_day_in_ntime = FALSE,
      silent = TRUE
    )

  expect_equal(
    nif_with_day[nif_with_day$EVID == 0, "NTIME"],
    nif_without_day[nif_with_day$EVID == 0, "NTIME"] + 24
    )
})


test_that("add_observation handles missing NTIME gracefully", {
  # Create a test SDTM object without ELTM field
  sdtm_test <- make_test_sdtm1()

  # Remove PCELTM field
  if ("PCELTM" %in% names(sdtm_test$domains$pc)) {
    sdtm_test$domains$pc$PCELTM <- NULL
  }

  # Create a base nif with administration data
  base_nif <- new_nif() %>%
    add_administration(sdtm_test, "A", analyte = "A")

  # Should run without error but show a message about NTIME
  expect_message(
      expect_message(
        nif_without_ntime <- base_nif %>%
          add_observation(sdtm_test, "pc", "A", cmt = 2),
      "ELTM is not defined"
    ),
  "No NTIME_lookup could be created")

  # NTIME should be NA in the resulting object
  obs_rows <- nif_without_ntime %>%
    filter(EVID == 0)

  if (nrow(obs_rows) > 0) {
    expect_true(all(is.na(obs_rows$NTIME)))
  }
})


test_that("add_observation handles DV field properly", {
  # Create a test SDTM object
  sdtm_test <- make_test_sdtm1()

  # Create a base nif with administration data
  base_nif <- new_nif() %>%
    add_administration(sdtm_test, "A", analyte = "A")

  # Custom DV field
  expect_no_error({
    nif_custom_dv <- base_nif %>%
      add_observation(sdtm_test, "pc", "A", DV_field = "PCSTRESN", silent = TRUE)
  })

  # Check if values match the source data
  dv_values <- nif_custom_dv %>%
    filter(EVID == 0) %>%
    pull(DV)

  source_values <- sdtm_test$domains$pc %>%
    filter(PCTESTCD == "A") %>%
    pull(PCSTRESN)

  # The values should match if the custom DV field is used correctly
  expect_equal(dv_values, source_values)
})


test_that("add_observation handles subject filtering", {
  # Create a test SDTM object
  sdtm_test <- make_test_sdtm1()

  # Create a base nif with administration data
  base_nif <- new_nif() %>%
    add_administration(sdtm_test, "A", analyte = "A")

  # Filter to include only subject "1"
  nif_filtered <- base_nif %>%
    add_observation(
      sdtm_test, "pc", "A",
      subject_filter = "USUBJID == '1'",
      silent = TRUE
    )

  # Check that only subject "1" observations are included
  obs_subjects <- nif_filtered %>%
    filter(EVID == 0) %>%
    pull(USUBJID) %>%
    unique()

  # Should only contain subject "1"
    expect_equal(obs_subjects, "1")
})


test_that("add_observation can handle non-existent domain gracefully", {
  # Create a base nif with administration data
  base_nif <- new_nif() %>%
    add_administration(examplinib_sad, "EXAMPLINIB", analyte = "RS2023")

  # Try adding observations from non-existent domain
  expect_error(
    base_nif %>%
      add_observation(
        examplinib_sad, "NON_EXISTENT_DOMAIN", "RS2023",
        silent = TRUE
    )
  )
})


test_that("add_observation handles observations without matching administrations", {
  # Create a base nif with administration data for a different analyte
  base_nif <- new_nif() %>%
    add_administration(examplinib_sad, "EXAMPLINIB", analyte = "RS2023")

  # Add observation with different parent
  nif_with_different_parent <- base_nif %>%
    add_observation(
      examplinib_sad, "pc", "RS2023",
      parent = "DIFFERENT_PARENT",
      silent = TRUE
    )

  # Should give warning about missing administrations
  expect_message(
    nif_with_no_admin <- base_nif %>%
      add_observation(
        examplinib_sad, "pc", "RS2023", cmt = 2,
        parent = "DIFFERENT_PARENT"
    ),
    "Missing administration information"
  )

  # Observations should be filtered out
  expect_equal(
    sum(nif_with_no_admin$PARENT == "DIFFERENT_PARENT" & nif_with_no_admin$EVID == 0),
    0
  )
})


test_that("add_observation properly handles custom testcd field", {
  # Create a test SDTM object
  sdtm_test <- make_test_sdtm1()

  # Create a base nif with administration data
  base_nif <- new_nif() %>%
    add_administration(sdtm_test, "A", analyte = "A")

  # Use custom TESTCD field
  expect_no_error({
    nif_custom_testcd <- base_nif %>%
      add_observation(
        sdtm_test, "pc", "Analyte A",
        TESTCD_field = "PCTEST",
        silent = TRUE
      )
  })
})

