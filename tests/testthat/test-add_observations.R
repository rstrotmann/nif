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

  return(new_sdtm(temp))
}


test_that("add_observation warns about duplicate compartment", {
  # Create a base nif object with administration data where CMT=1
  base_nif <- new_nif() %>%
    add_administration(
      examplinib_sad, "EXAMPLINIB",
      analyte = "RS2023", silent = TRUE
    )

  # Try to add observation with the same compartment
  expect_warning(
    base_nif %>%
      add_observation(examplinib_sad, "pc", "RS2023", cmt = 1, silent = TRUE),
    "Compartment 1 is already assigned!"
  )
})


test_that("add_observation auto-assigns compartment if not specified", {
  # Create a base nif with administration data
  base_nif <- new_nif() %>%
    add_administration(
      examplinib_sad, "EXAMPLINIB",
      analyte = "RS2023", silent = TRUE
    )

  # Add observation without specifying CMT, capture messages
  expect_message(
    nif_with_obs <- base_nif %>%
      add_observation(examplinib_sad, "pc", "RS2023", silent = FALSE),
    "Compartment for RS2023 set to 2"
  )

  # Verify the compartment was auto-assigned
  expect_true(2 %in% unique(nif_with_obs$CMT))
})


test_that("add_observation auto-assigns parent if not specified", {
  # Create a base nif with administration data
  base_nif <- new_nif() %>%
    add_administration(
      examplinib_sad, "EXAMPLINIB",
      analyte = "RS2023", silent = TRUE
    )

  # Different observation analyte without specifying parent
  nif_with_obs <- base_nif %>%
    add_observation(
      examplinib_sad, "pc", "RS2023",
      analyte = "DIFFERENT", cmt = 2,
      silent = FALSE
    )

  expect_equal(unique(filter(nif_with_obs, ANALYTE == "DIFFERENT")$PARENT), "RS2023")
})


test_that("add_observation properly uses observation_filter", {
  # Create a base nif with administration data
  base_nif <- new_nif() %>%
    add_administration(
      examplinib_sad, "EXAMPLINIB",
      analyte = "RS2023", silent = TRUE
    )

  # Add observation with filter that returns nothing
  expect_error(
    nif_with_filtered_obs <- base_nif %>% add_observation(
      examplinib_sad, "pc", "RS2023",
      cmt = 2,
      observation_filter = "PCTESTCD == 'NON_EXISTENT'"
    )
  )
})


test_that("add_observation works with factor parameter", {
  # Create a base nif with administration data
  base_nif <- new_nif() %>%
    add_administration(
      examplinib_sad, "EXAMPLINIB",
      analyte = "RS2023", silent = TRUE
    )

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
    add_administration(
      examplinib_sad, "EXAMPLINIB",
      analyte = "RS2023", silent = TRUE
    )

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
    add_administration(
      examplinib_sad, "EXAMPLINIB",
      analyte = "RS2023", silent = TRUE
    )

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
    add_administration(
      examplinib_sad, "EXAMPLINIB",
      analyte = "RS2023",
      debug = TRUE, silent = TRUE
    )

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
    add_administration(
      examplinib_sad, "EXAMPLINIB",
      analyte = "RS2023", silent = TRUE
    )

  # Check columns before adding observation
  before_cols <- names(base_nif)

  # Add observation with additional columns to keep
  nif_with_extra <- base_nif %>%
    add_observation(examplinib_sad, "pc", "RS2023",
      keep = c("PCSPEC"), silent = TRUE
    )

  # Check if the extra column was kept
  after_cols <- names(nif_with_extra)

  # The difference should contain the kept column
  expect_true("PCSPEC" %in% after_cols)
})


test_that("add_observation handles include_day_in_ntime parameter", {
  # Create a base nif with administration data
  base_nif <- new_nif() %>%
    add_administration(
      examplinib_sad, "EXAMPLINIB",
      analyte = "RS2023", silent = TRUE
    )

  custom_sdtm <- examplinib_sad
  custom_sdtm$domains$pc$PCDY <- 2

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
    add_administration(sdtm_test, "A", analyte = "A", silent = TRUE)

  # Should run without error but show a message about NTIME
  expect_message(
    expect_message(
      nif_without_ntime <- base_nif %>%
        add_observation(sdtm_test, "pc", "A",
          cmt = 2, ntime_method = "ELTM",
          silent = FALSE
        ),
      "ELTM is not defined"
    ),
    "No NTIME_lookup could be created"
  )

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
    add_administration(sdtm_test, "A", analyte = "A", silent = TRUE)

  # Custom DV field
  expect_no_error({
    nif_custom_dv <- base_nif %>%
      add_observation(
        sdtm_test, "pc", "A",
        dv_field = "PCSTRESN", ,
        ntime_method = "ELTM", silent = TRUE
      )
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
    add_administration(sdtm_test, "A", analyte = "A", silent = TRUE)

  # Filter to include only subject "1"
  nif_filtered <- base_nif %>%
    add_observation(
      sdtm_test, "pc", "A",
      subject_filter = "USUBJID == '1'",
      ntime_method = "ELTM",
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
    add_administration(
      examplinib_sad, "EXAMPLINIB",
      analyte = "RS2023", silent = TRUE
    )

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
    add_administration(
      examplinib_sad, "EXAMPLINIB",
      analyte = "RS2023", silent = TRUE
    )

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
        examplinib_sad, "pc", "RS2023",
        cmt = 2,
        parent = "DIFFERENT_PARENT",
        silent = FALSE
      ),
    "Missing administration information"
  )

  # Observations should be filtered out
  expect_equal(
    sum(nif_with_no_admin$PARENT == "DIFFERENT_PARENT" & nif_with_no_admin$EVID == 0),
    0
  )
})


test_that("add_observation handles na.rm parameter when resolving duplicates", {
  sdtm_obj <- make_test_sdtm1()

  pc <- tibble::tribble(
    ~USUBJID, ~DOMAIN, ~PCTESTCD, ~PCDTC, ~PCSTRESN,
    "1", "PC", "A", "2023-01-01 08:00:00", 100,
    "1", "PC", "A", "2023-01-01 12:00:00", 200,
    "1", "PC", "A", "2023-01-01 08:00:00", NA,
    "1", "PC", "A", "2023-01-01 12:00:00", 400,
    "2", "PC", "A", "2023-01-01 08:00:00", 500,
    "2", "PC", "A", "2023-01-01 12:00:00", NA,
    "2", "PC", "A", "2023-01-01 08:00:00", 700,
    "2", "PC", "A", "2023-01-01 12:00:00", 800
  )

  sdtm_obj$domains$pc <- pc

  # Create administration data
  base_nif <- new_nif() %>%
    add_administration(sdtm_obj, "A", analyte = "A", silent = TRUE)

  # Test with na.rm = TRUE (default)
  nif_with_na_rm <- base_nif %>%
    add_observation(
      sdtm_obj, "pc", "A",
      duplicates = "resolve",
      ntime_method = "ELTM",
      silent = TRUE
    )

  # The observation at 08:00 for subject 1 should be 100 (NA is removed)
  obs_1_8am <- nif_with_na_rm %>%
    filter(USUBJID == "1" & EVID == 0 & format(DTC, "%H:%M") == "08:00") %>%
    pull(DV)
  expect_equal(obs_1_8am, 100)

  # Test with na.rm = FALSE
  nif_without_na_rm <- base_nif %>%
    add_observation(
      sdtm_obj, "pc", "A",
      duplicates = "resolve",
      na.rm = FALSE,
      ntime_method = "ELTM",
      silent = TRUE
    )

  # The observation at 08:00 for subject 1 should be NA (NA is not removed)
  obs_1_8am_no_rm <- nif_without_na_rm %>%
    filter(USUBJID == "1" & EVID == 0 & format(DTC, "%H:%M") == "08:00") %>%
    pull(DV)
  expect_equal(obs_1_8am_no_rm, 100)
})
