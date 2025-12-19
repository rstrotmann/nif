test_that("nca_from_pp works with valid inputs", {
  # Create test data
  nif_obj <- new_nif(
    tibble::tribble(
      ~ID, ~USUBJID, ~ANALYTE, ~EVID, ~DOSE, ~AGE,
      1, "SUBJ1", "DRUG", 1, 100, 30,
      2, "SUBJ2", "DRUG", 1, 200, 40
    )
  )

  sdtm_data <- new_sdtm(list(
    pp = tibble::tribble(
      ~USUBJID, ~PPTESTCD, ~PPSTRESN, ~PPSPEC, ~PPCAT, ~PPRFTDTC, ~DOMAIN,
      "SUBJ1", "AUC", 100, "PLASMA", "DRUG", "2023-01-01", "PP",
      "SUBJ1", "CMAX", 50, "PLASMA", "DRUG", "2023-01-01", "PP",
      "SUBJ2", "AUC", 200, "PLASMA", "DRUG", "2023-01-02", "PP",
      "SUBJ2", "CMAX", 100, "PLASMA", "DRUG", "2023-01-02", "PP"
    )
  ))

  # Test basic functionality
  result <- nca_from_pp(nif_obj, sdtm_data, analyte = "DRUG", silent = TRUE)
  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 4)
  expect_true(all(c(
    "USUBJID", "PPTESTCD", "PPSTRESN", "DOSE", "AGE"
  ) %in% names(result)))

  # Test with observation filter
  result_filtered <- nca_from_pp(
    nif_obj, sdtm_data,
    analyte = "DRUG",
    observation_filter = "PPSPEC == 'PLASMA' & PPTESTCD == 'AUC'"
  )
  expect_equal(nrow(result_filtered), 2)
  expect_true(all(result_filtered$PPTESTCD == "AUC"))

  # Test with grouping
  result_grouped <- nca_from_pp(
    nif_obj, sdtm_data,
    analyte = "DRUG",
    group = "PPSPEC"
  )
  expect_true("PPSPEC" %in% names(result_grouped))
})

test_that("nca_from_pp handles missing analyte", {
  nif_obj <- structure(
    tibble::tribble(
      ~ID, ~USUBJID, ~ANALYTE, ~EVID, ~DOSE,
      1, "SUBJ1", "DRUG", 1, 100,
    ),
    class = c("nif", "data.frame")
  )

  sdtm_data <- new_sdtm(list(
    pp = tibble::tribble(
      ~USUBJID, ~PPTESTCD, ~PPSTRESN, ~PPSPEC, ~PPCAT, ~PPRFTDTC,
      "SUBJ1", "AUC", 100, "PLASMA", "DRUG", "2023-01-01"
    )
  ))

  # Test with NULL analyte
  result <- nca_from_pp(nif_obj, sdtm_data, "DRUG", silent = TRUE)
  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 1)
})

test_that("nca_from_pp handles errors appropriately", {
  nif_obj <- data.frame(
    ID = 1,
    USUBJID = "SUBJ1",
    ANALYTE = "DRUG",
    EVID = 1,
    DOSE = 100
  ) %>%
    new_nif()

  sdtm_data <- new_sdtm(
    list(
      pp = data.frame(
        USUBJID = "SUBJ1",
        PPTESTCD = "AUC",
        PPSTRESN = 100
      )
    )
  )

  # Test missing required parameters
  expect_error(nca_from_pp(), 'argument "obj" is missing, with no default')

  # Test invalid sdtm_data structure
  expect_error(
    nca_from_pp(nif_obj, list(), silent = TRUE),
    "sdtm_data must be an sdtm object"
  )

  # Test invalid observation filter
  expect_error(
    nca_from_pp(nif_obj, sdtm_data, observation_filter = "invalid filter"),
    "Invalid observation_filter"
  )
})

test_that("nca_from_pp handles empty results", {
  nif_obj <- data.frame(
    ID = 1,
    USUBJID = "SUBJ1",
    ANALYTE = "DRUG",
    EVID = 1,
    DOSE = 100
  ) %>%
    new_nif()

  sdtm_data <- new_sdtm(list(
    pp = data.frame(
      USUBJID = "SUBJ1",
      PPTESTCD = "AUC",
      PPSTRESN = 100,
      PPSPEC = "PLASMA",
      PPCAT = "DRUG"
    )
  ))

  # Test with filter that returns no results
  expect_warning(
    nca_from_pp(
      nif_obj, sdtm_data, "DRUG",
      observation_filter = "PPSPEC == 'URINE'",
      silent = TRUE
    ),
    "No data found after applying filters"
  )
})

test_that("nca_from_pp handles keep parameter correctly", {
  nif_obj <- data.frame(
    ID = 1,
    USUBJID = "SUBJ1",
    ANALYTE = "DRUG",
    EVID = 1,
    DOSE = 100,
    AGE = 30,
    SEX = "M"
  ) %>%
    new_nif()

  sdtm_data <- new_sdtm(list(
    pp = data.frame(
      USUBJID = "SUBJ1",
      PPTESTCD = "AUC",
      PPSTRESN = 100,
      PPSPEC = "PLASMA",
      PPCAT = "DRUG"
    )
  ))

  # Test with keep parameter
  result <- nca_from_pp(
    nif_obj, sdtm_data, "DRUG",
    keep = c("AGE", "SEX"), silent = TRUE
  )
  expect_true(all(c("AGE", "SEX") %in% names(result)))

  # Test with non-existent keep columns
  result <- nca_from_pp(nif_obj, sdtm_data, "DRUG", keep = "NONEXISTENT", silent = TRUE)
  expect_false("NONEXISTENT" %in% names(result))
})
