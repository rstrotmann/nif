test_that("nif_auto works with basic input", {
  # Create minimal SDTM object
  dm <- tibble::tribble(
    ~DOMAIN, ~USUBJID, ~SEX, ~RACE, ~AGE, ~ACTARMCD, ~RFSTDTC, ~RFENDTC,
    "DM", "001", "M", "WHITE", 30, "TRT", "2023-01-01", "2023-01-15",
    "DM", "002", "F", "BLACK", 25, "TRT", "2023-01-01", "2023-01-15",
    "DM", "003", "M", "ASIAN", 35, "SCRNFAIL", "2023-01-01", "2023-01-15"
  )

  vs <- tibble::tribble(
    ~DOMAIN, ~USUBJID, ~VSTESTCD, ~VSSTRESN, ~VISIT, ~VSDTC, ~VSBLFL,
    "VS", "001", "WEIGHT", 70, "SCREENING", "2022-12-30", "Y",
    "VS", "002", "WEIGHT", 65, "SCREENING", "2022-12-30", "Y",
    "VS", "003", "WEIGHT", 80, "SCREENING", "2022-12-30", "Y"
  )

  ex <- tibble::tribble(
    ~DOMAIN, ~USUBJID, ~EXTRT, ~EXDOSE, ~EXSTDTC, ~EXENDTC, ~EXSEQ,
    "EX", "001", "DRUG1", 100, "2023-01-01", "2023-01-01", 1,
    "EX", "002", "DRUG1", 100, "2023-01-01", "2023-01-01", 1
  )

  pc <- tibble::tribble(
    ~DOMAIN, ~USUBJID, ~PCTESTCD, ~PCSTRESN, ~PCDTC, ~VISIT,
    "PC", "001", "DRUG1", 10, "2023-01-01 08:00", "DAY 1",
    "PC", "001", "DRUG1", 8, "2023-01-01 12:00", "DAY 1",
    "PC", "002", "DRUG1", 12, "2023-01-01 08:00", "DAY 1",
    "PC", "002", "DRUG1", 9, "2023-01-01 12:00", "DAY 1"
  )

  sdtm <- list(domains = list(dm = dm, vs = vs, ex = ex, pc = pc))
  class(sdtm) <- "sdtm"

  # Test basic functionality
  result <- nif_auto(sdtm, DRUG1 ~ DRUG1, silent = TRUE)
  expect_s3_class(result, "nif")
  expect_true("DRUG1" %in% result$ANALYTE)
})

test_that("nif_auto handles metabolites correctly", {
  # Create SDTM object with parent and metabolite
  dm <- tibble::tribble(
    ~DOMAIN, ~USUBJID, ~SEX, ~RACE, ~AGE, ~ACTARMCD, ~RFSTDTC, ~RFENDTC,
    "DM", "001", "M", "WHITE", 30, "TRT", "2023-01-01", "2023-01-15"
  )

  vs <- tibble::tribble(
    ~DOMAIN, ~USUBJID, ~VSTESTCD, ~VSSTRESN, ~VISIT, ~VSDTC, ~VSBLFL,
    "VS", "001", "WEIGHT", 70, "SCREENING", "2022-12-30", "Y"
  )

  ex <- tibble::tribble(
    ~DOMAIN, ~USUBJID, ~EXTRT, ~EXDOSE, ~EXSTDTC, ~EXENDTC, ~EXSEQ,
    "EX", "001", "DRUG1", 100, "2023-01-01", "2023-01-01", 1
  )

  pc <- tibble::tribble(
    ~DOMAIN, ~USUBJID, ~PCTESTCD, ~PCSTRESN, ~PCDTC, ~VISIT,
    "PC", "001", "DRUG1", 10, "2023-01-01 08:00", "DAY 1",
    "PC", "001", "M1", 5, "2023-01-01 08:00", "DAY 1"
  )

  sdtm <- list(domains = list(dm = dm, vs = vs, ex = ex, pc = pc))
  class(sdtm) <- "sdtm"

  result <- nif_auto(sdtm, DRUG1 + M1 ~ DRUG1, silent = TRUE)
  expect_true(all(c("DRUG1", "M1") %in% result$ANALYTE))
  expect_true(any(result$METABOLITE))
})

test_that("nif_auto adds baseline covariates when available", {
  # Create SDTM object with LB data
  dm <- tibble::tribble(
    ~DOMAIN, ~USUBJID, ~SEX, ~RACE, ~AGE, ~ACTARMCD, ~RFSTDTC, ~RFENDTC,
    "DM", "001", "M", "WHITE", 30, "TRT", "2023-01-01", "2023-01-15"
  )

  vs <- tibble::tribble(
    ~DOMAIN, ~USUBJID, ~VSTESTCD, ~VSSTRESN, ~VISIT, ~VSDTC, ~VSBLFL,
    "VS", "001", "WEIGHT", 70, "SCREENING", "2022-12-30", "Y"
  )

  ex <- tibble::tribble(
    ~DOMAIN, ~USUBJID, ~EXTRT, ~EXDOSE, ~EXSTDTC, ~EXENDTC, ~EXSEQ,
    "EX", "001", "DRUG1", 100, "2023-01-01", "2023-01-01", 1
  )

  pc <- tibble::tribble(
    ~DOMAIN, ~USUBJID, ~PCTESTCD, ~PCSTRESN, ~PCDTC, ~VISIT,
    "PC", "001", "DRUG1", 10, "2023-01-01 08:00", "DAY 1"
  )

  lb <- tibble::tribble(
    ~DOMAIN, ~USUBJID, ~LBTESTCD, ~LBSTRESN, ~VISIT, ~LBBLFL, ~LBSPEC, ~LBSTNRHI,
    "LB", "001", "CREAT", 0.8, "SCREENING", "Y", "SERUM", 1.2,
    "LB", "001", "BILI", 0.5, "SCREENING", "Y", "SERUM", 1.2,
    "LB", "001", "AST", 20, "SCREENING", "Y", "SERUM", 40
  )

  sdtm <- list(domains = list(dm = dm, vs = vs, ex = ex, pc = pc, lb = lb))
  class(sdtm) <- "sdtm"

  result <- nif_auto(sdtm, DRUG1 ~ DRUG1, silent = TRUE)
  expect_true(all(c("BL_CREAT", "BL_CRCL", "BL_RENAL", "BL_ODWG") %in% names(result)))
})

test_that("nif_auto handles subject filtering correctly", {
  # Create SDTM object with multiple subjects
  dm <- tibble::tribble(
    ~DOMAIN, ~USUBJID, ~SEX, ~RACE, ~AGE, ~ACTARMCD, ~RFSTDTC, ~RFENDTC,
    "DM", "001", "M", "WHITE", 30, "TRT", "2023-01-01", "2023-01-15",
    "DM", "002", "F", "BLACK", 25, "SCRNFAIL", "2023-01-01", "2023-01-15",
    "DM", "003", "M", "ASIAN", 35, "NOTTRT", "2023-01-01", "2023-01-15"
  )

  vs <- tibble::tribble(
    ~DOMAIN, ~USUBJID, ~VSTESTCD, ~VSSTRESN, ~VISIT, ~VSDTC, ~VSBLFL,
    "VS", "001", "WEIGHT", 70, "SCREENING", "2022-12-30", "Y",
    "VS", "002", "WEIGHT", 65, "SCREENING", "2022-12-30", "Y",
    "VS", "003", "WEIGHT", 80, "SCREENING", "2022-12-30", "Y"
  )

  ex <- tibble::tribble(
    ~DOMAIN, ~USUBJID, ~EXTRT, ~EXDOSE, ~EXSTDTC, ~EXENDTC, ~EXSEQ,
    "EX", "001", "DRUG1", 100, "2023-01-01", "2023-01-01", 1,
    "EX", "002", "DRUG1", 100, "2023-01-01", "2023-01-01", 1,
    "EX", "003", "DRUG1", 100, "2023-01-01", "2023-01-01", 1
  )

  pc <- tibble::tribble(
    ~DOMAIN, ~USUBJID, ~PCTESTCD, ~PCSTRESN, ~PCDTC, ~VISIT,
    "PC", "001", "DRUG1", 10, "2023-01-01 08:00", "DAY 1",
    "PC", "002", "DRUG1", 12, "2023-01-01 08:00", "DAY 1",
    "PC", "003", "DRUG1", 15, "2023-01-01 08:00", "DAY 1"
  )

  sdtm <- list(domains = list(dm = dm, vs = vs, ex = ex, pc = pc))
  class(sdtm) <- "sdtm"

  # Test with default filter
  result_default <- nif_auto(sdtm, DRUG1 ~ DRUG1, silent = TRUE)
  expect_equal(length(unique(result_default$USUBJID)), 1)

  # Test with custom filter
  result_custom <- nif_auto(
    sdtm, DRUG1 ~ DRUG1,
    subject_filter = "ACTARMCD == 'TRT'",
    silent = TRUE
  )
  expect_equal(length(unique(result_custom$USUBJID)), 1)
})

test_that("nif_auto handles duplicates correctly", {
  # Create SDTM object with duplicate observations
  dm <- tibble::tribble(
    ~DOMAIN, ~USUBJID, ~SEX, ~RACE, ~AGE, ~ACTARMCD, ~RFSTDTC, ~RFENDTC,
    "DM", "001", "M", "WHITE", 30, "TRT", "2023-01-01", "2023-01-15"
  )

  vs <- tibble::tribble(
    ~DOMAIN, ~USUBJID, ~VSTESTCD, ~VSSTRESN, ~VISIT, ~VSDTC, ~VSBLFL,
    "VS", "001", "WEIGHT", 70, "SCREENING", "2022-12-30", "Y"
  )

  ex <- tibble::tribble(
    ~DOMAIN, ~USUBJID, ~EXTRT, ~EXDOSE, ~EXSTDTC, ~EXENDTC, ~EXSEQ,
    "EX", "001", "DRUG1", 100, "2023-01-01", "2023-01-01", 1
  )

  pc <- tibble::tribble(
    ~DOMAIN, ~USUBJID, ~PCTESTCD, ~PCSTRESN, ~PCDTC, ~VISIT,
    "PC", "001", "DRUG1", 10, "2023-01-01 08:00", "DAY 1",
    "PC", "001", "DRUG1", 12, "2023-01-01 08:00", "DAY 1"
  )

  sdtm <- list(domains = list(dm = dm, vs = vs, ex = ex, pc = pc))
  class(sdtm) <- "sdtm"

  # Test with mean function
  result_mean <- nif_auto(
    sdtm, DRUG1 ~ DRUG1,
    duplicates = "resolve", duplicate_function = mean,
    silent = TRUE
  )
  expect_equal(nrow(filter(result_mean, EVID == 0)), 1)
  expect_equal(filter(result_mean, EVID == 0)$DV, 11)

  # Test with max function
  result_max <- nif_auto(
    sdtm, DRUG1 ~ DRUG1,
    duplicates = "resolve", duplicate_function = max,
    silent = TRUE
  )
  expect_equal(nrow(filter(result_max, EVID == 0)), 1)
  expect_equal(filter(result_max, EVID == 0)$DV, 12)
})
