test_that("add_baseline adds baseline covariate correctly", {
  # Create a minimal test dataset
  nif <- tibble::tribble(
      ~USUBJID,         ~DTC, ~DOSE,
    "SUBJ-001", "2023-01-01",   100,
    "SUBJ-001", "2023-01-02",   100,
    "SUBJ-002", "2023-01-01",   200
  ) %>%
    new_nif()

  sdtm_data <- list(
    dm = tibble::tribble(
      ~USUBJID,   ~ACTARMCD,
      "SUBJ-001", "TREATMENT",
      "SUBJ-002", "TREATMENT"
    ),

    vs = tibble::tribble(
        ~USUBJID,       ~VSDTC, ~VSTESTCD, ~VSSTRESN, ~VSBLFL,
      "SUBJ-001", "2023-01-01",  "WEIGHT",        70,     "Y",
      "SUBJ-001", "2023-01-02",  "WEIGHT",        71,     "N",
      "SUBJ-002", "2023-01-01",  "WEIGHT",        80,     "Y",
      "SUBJ-002", "2023-01-02",  "HEIGHT",       180,     "Y"
    )
  )

  sdtm <- new_sdtm(sdtm_data)

  # Test basic functionality
  result <- add_baseline(nif, sdtm, "vs", "WEIGHT", silent = TRUE)

  expect_true("BL_WEIGHT" %in% names(result))
  expect_equal(result$BL_WEIGHT[result$USUBJID == "SUBJ-001"], rep(70, 2))
  expect_equal(result$BL_WEIGHT[result$USUBJID == "SUBJ-002"], 80)
  expect_s3_class(result, "nif")
})


test_that("add_baseline handles custom baseline filter", {
  test_nif <- tibble::tribble(
    ~USUBJID,         ~DTC,
    "SUBJ-001", "2023-01-01",
    "SUBJ-002", "2023-01-01"
  )
  class(test_nif) <- c("nif", "data.frame")

  test_vs <- tibble::tribble(
    ~USUBJID,       ~VSDTC, ~VSTESTCD, ~VSSTRESN, ~VSBLFL,
    "SUBJ-001", "2023-01-01",  "WEIGHT",        70,     "",
    "SUBJ-001", "2023-01-02",  "WEIGHT",        72,     "Y",
    "SUBJ-002", "2023-01-01",  "WEIGHT",        80,     "",
    "SUBJ-002", "2023-01-02",  "WEIGHT",        82,     "Y"
  )

  test_dm <- tibble::tribble(
    ~USUBJID,   ~ACTARMCD,
    "SUBJ-001", "TREATMENT",
    "SUBJ-002", "TREATMENT"
  )

  test_sdtm <- new_sdtm(list(vs = test_vs, dm = test_dm))

  # Test custom baseline filter (use day 2 as baseline)
  result <- add_baseline(test_nif, test_sdtm, "vs", "WEIGHT",
                        baseline_filter = "VSBLFL == 'Y'")

  expect_equal(result$BL_WEIGHT[result$USUBJID == "SUBJ-001"], 72)
  expect_equal(result$BL_WEIGHT[result$USUBJID == "SUBJ-002"], 82)
})


test_that("add_baseline handles coding table correctly", {
  test_nif <- data.frame(
    ID = c(1, 2),
    USUBJID = c("SUBJ-001", "SUBJ-002"),
    DTC = c("2023-01-01", "2023-01-01")
  )
  class(test_nif) <- c("nif", "data.frame")

  test_vs <- data.frame(
    USUBJID = c("SUBJ-001", "SUBJ-002"),
    VSDTC = c("2023-01-01", "2023-01-01"),
    VSTESTCD = c("SEX", "SEX"),
    VSORRES = c("MALE", "FEMALE"),
    VSBLFL = c("Y", "Y")
  )

  test_dm <- data.frame(
    USUBJID = c("SUBJ-001", "SUBJ-002"),
    ACTARMCD = c("TREATMENT", "TREATMENT")
  )

  test_coding <- data.frame(
    VSORRES = c("MALE", "FEMALE"),
    DV = c(1, 0)
  )

  test_sdtm <- new_sdtm(list(vs = test_vs, dm = test_dm))

  # Test with coding table
  result <- add_baseline(
    test_nif, test_sdtm, "vs", "SEX", DV_field = "VSORRES",
    coding_table = test_coding, silent = TRUE)

  expect_equal(result$BL_SEX[result$USUBJID == "SUBJ-001"], 1)
  expect_equal(result$BL_SEX[result$USUBJID == "SUBJ-002"], 0)
})


test_that("add_baseline validates inputs correctly", {
  test_nif <- data.frame(
    USUBJID = c("SUBJ-001"),
    DTC = c("2023-01-01")
  )
  class(test_nif) <- c("nif", "data.frame")

  test_vs <- data.frame(
    USUBJID = c("SUBJ-001"),
    VSDTC = c("2023-01-01"),
    VSTESTCD = c("WEIGHT"),
    VSSTRESN = c(70),
    VSBLFL = c("Y")
  )

  test_dm <- data.frame(
    USUBJID = c("SUBJ-001"),
    ACTARMCD = c("TREATMENT")
  )

  test_sdtm <- new_sdtm(list(vs = test_vs, dm = test_dm))

  # Test missing nif object
  fake_nif <- data.frame(USUBJID = "SUBJ-001")
  expect_error(
    add_baseline(fake_nif, test_sdtm, "vs", "WEIGHT"),
    "First argument must be a nif object"
  )

  # Test missing SDTM object
  expect_error(
    add_baseline(test_nif, domain = "vs", testcd = "WEIGHT"),
    "SDTM object is required"
  )

  # Test invalid domain
  expect_error(
    add_baseline(test_nif, test_sdtm, "invalid_domain", "WEIGHT"),
    "Domain 'invalid_domain' not found in SDTM object"
  )

  # Test invalid testcd
  expect_error(
    add_baseline(test_nif, test_sdtm, "vs", "INVALID_TEST"),
    "Test code 'INVALID_TEST' not found"
  )
})


test_that("add_baseline handles multiple baseline values correctly", {
  test_nif <- tibble::tribble(
      ~USUBJID,         ~DTC,
    "SUBJ-001", "2023-01-01"
  )
  class(test_nif) <- c("nif", "data.frame")

  test_vs <- tibble::tribble(
      ~USUBJID,       ~VSDTC, ~VSTESTCD, ~VSSTRESN, ~VSBLFL,
    "SUBJ-001", "2023-01-01",  "WEIGHT",        70,     "Y",
    "SUBJ-001", "2023-01-01",  "WEIGHT",        72,     "Y"
  )

  test_dm <- tibble::tribble(
      ~USUBJID,   ~ACTARMCD,
    "SUBJ-001", "TREATMENT"
  )

  test_sdtm <- new_sdtm(list(vs = test_vs, dm = test_dm))

  # Two baseline values should be averaged
  result <- add_baseline(test_nif, test_sdtm, "vs", "WEIGHT", silent = TRUE)
  expect_equal(result$BL_WEIGHT, 71)

  # Test with a different summary function
  result_max <- add_baseline(
    test_nif, test_sdtm, "vs", "WEIGHT", summary_function = max,
    , silent = TRUE)
  expect_equal(result_max$BL_WEIGHT, 72)
})


test_that("add_baseline handles empty result after filtering", {
  test_nif <- data.frame(
    USUBJID = c("SUBJ-001"),
    DTC = c("2023-01-01")
  )
  class(test_nif) <- c("nif", "data.frame")

  test_vs <- tibble::tribble(
      ~USUBJID,       ~VSDTC, ~VSTESTCD, ~VSSTRESN, ~VSBLFL,
    "SUBJ-001", "2023-01-01",  "WEIGHT",        70,     "N"
  )

  test_dm <- data.frame(
    USUBJID = c("SUBJ-001"),
    ACTARMCD = c("TREATMENT")
  )

  test_sdtm <- new_sdtm(list(vs = test_vs, dm = test_dm))

  # Test when filtering results in no baseline values
  expect_error(
    result <- add_baseline(test_nif, test_sdtm, "vs", "WEIGHT", silent = TRUE)
  )
})


test_that("add baseline hepatic function class works", {
  dm <- tribble(
    ~USUBJID, ~DOMAIN, ~SEX,   ~ACTARMCD,          ~RFXSTDTC,
    "1", "DM", "M", "TREATMENT", "2001-01-01T10:29",
    "2", "DM", "M", "TREATMENT", "2001-01-02T09:09",
    "3", "DM", "M", "TREATMENT", "2000-12-29T09:07",
    "4", "DM", "F", "TREATMENT", "2001-01-06T11:18",
    "5", "DM", "F", "TREATMENT", "2001-01-07T11:18"
  ) %>%
    mutate(RFSTDTC = RFXSTDTC)

  vs <- tribble(
    ~USUBJID, ~DOMAIN, ~VSTESTCD, ~VSBLFL, ~VSSTRESN,
    "1", "VS", "HEIGHT",     "Y",     190.8,
    "1", "VS", "WEIGHT",     "Y",      79.3,
    "2", "VS", "HEIGHT",     "Y",     199.5,
    "2", "VS", "WEIGHT",     "Y",      81.6,
    "3", "VS", "HEIGHT",     "Y",     185.4,
    "3", "VS", "WEIGHT",     "Y",      92.8,
    "4", "VS", "HEIGHT",     "Y",     177.8,
    "4", "VS", "WEIGHT",     "Y",      83.3,
    "5", "VS", "HEIGHT",     "Y",     177.9,
    "5", "VS", "WEIGHT",     "Y",      83.4
  )
  lb <- tribble(
    ~USUBJID, ~DOMAIN, ~LBSPEC, ~LBDTC,             ~LBBLFL, ~LBTESTCD, ~LBSTRESN, ~LBSTNRHI,
    "1",      "LB",    "SERUM", "2001-01-01T10:29", "Y",     "BILI",    1,          1, # normal
    "1",      "LB",    "SERUM", "2001-01-01T10:29", "Y",     "AST",     1,          1,
    "2",      "LB",    "SERUM", "2001-01-02T09:09", "Y",     "BILI",    1.5,        1, # mild
    "2",      "LB",    "SERUM", "2001-01-02T09:09", "Y",     "AST",     1,          1,
    "3",      "LB",    "SERUM", "2000-12-29T09:07", "Y",     "BILI",    1,          1, # mild
    "3",      "LB",    "SERUM", "2000-12-29T09:07", "Y",     "AST",     1.2,        1,
    "4",      "LB",    "SERUM", "2001-01-06T11:18", "Y",     "BILI",    3,          1, # moderate
    "4",      "LB",    "SERUM", "2001-01-06T11:18", "Y",     "AST",     1,          1,
    "5",      "LB",    "SERUM", "2001-01-07T11:18", "Y",     "BILI",    3.1,        1, # severe
    "5",      "LB",    "SERUM", "2001-01-07T11:18", "Y",     "AST",     1,          1
  )
  sdtm <- new_sdtm(list(dm = dm, vs = vs, lb = lb))

  test_nif <- data.frame(USUBJID = as.character(c(1, 2, 3, 4, 5)))
  class(test_nif) <- c("nif", "data.frame")

  expect_no_error(
    temp <- test_nif %>%
      add_bl_odwg(sdtm)
  )

  expect_equal(
    as.character(temp$BL_ODWG),
    c("normal", "mild", "mild", "moderate", "severe"))
})


test_that("add_baseline handles all NA baseline values correctly", {
  test_nif <- data.frame(
    USUBJID = c("SUBJ-001", "SUBJ-002"),
    DTC = c("2023-01-01", "2023-01-01")
  )
  class(test_nif) <- c("nif", "data.frame")

  test_vs <- tibble::tribble(
      ~USUBJID,       ~VSDTC, ~VSTESTCD, ~VSSTRESN, ~VSBLFL,
    "SUBJ-001", "2023-01-01",  "WEIGHT",        NA,     "Y",
    "SUBJ-002", "2023-01-01",  "WEIGHT",        NA,     "Y"
  )

  test_dm <- data.frame(
    USUBJID = c("SUBJ-001", "SUBJ-002"),
    ACTARMCD = c("TREATMENT", "TREATMENT")
  )

  test_sdtm <- new_sdtm(list(vs = test_vs, dm = test_dm))

  # Test when all baseline values are NA
  expect_error(
    add_baseline(test_nif, test_sdtm, "vs", "WEIGHT", silent = TRUE),
    "No valid baseline values found for test code 'WEIGHT'. Data was found but all values are NA after processing."
  )
})


test_that("add_baseline warns when some baseline values are NA", {
  test_nif <- data.frame(
    USUBJID = c("SUBJ-001", "SUBJ-002", "SUBJ-003"),
    DTC = c("2023-01-01", "2023-01-01", "2023-01-01")
  )
  class(test_nif) <- c("nif", "data.frame")

  test_vs <- tibble::tribble(
      ~USUBJID,       ~VSDTC, ~VSTESTCD, ~VSSTRESN, ~VSBLFL,
    "SUBJ-001", "2023-01-01",  "WEIGHT",        70,     "Y",
    "SUBJ-002", "2023-01-01",  "WEIGHT",        NA,     "Y",
    "SUBJ-003", "2023-01-01",  "WEIGHT",        80,     "Y"
  )

  test_dm <- data.frame(
    USUBJID = c("SUBJ-001", "SUBJ-002", "SUBJ-003"),
    ACTARMCD = c("TREATMENT", "TREATMENT", "TREATMENT")
  )

  test_sdtm <- new_sdtm(list(vs = test_vs, dm = test_dm))

  # Test when some baseline values are NA - should give warning but complete
  expect_message(
    expect_message(
      result <- add_baseline(test_nif, test_sdtm, "vs", "WEIGHT", silent = FALSE),
    "Some subjects have missing baseline values for test code 'WEIGHT'. These will be NA in the output."),
  "baseline_filter for BL_WEIGHT set to VSBLFL == 'Y'")

  # Check that function completed and returned correct values
  expect_equal(result$BL_WEIGHT[result$USUBJID == "SUBJ-001"], 70)
  expect_true(is.na(result$BL_WEIGHT[result$USUBJID == "SUBJ-002"]))
  expect_equal(result$BL_WEIGHT[result$USUBJID == "SUBJ-003"], 80)
})


test_that("add_baseline validates required fields correctly", {
  test_nif <- data.frame(
    USUBJID = c("SUBJ-001"),
    DTC = c("2023-01-01")
  )
  class(test_nif) <- c("nif", "data.frame")

  # Create a domain with missing field
  test_vs_missing_field <- tibble::tribble(
    ~USUBJID, ~VSDTC, ~VSTESTCD, ~VSBLFL,  # Missing VSSTRESN
    "SUBJ-001", "2023-01-01", "WEIGHT", "Y"
  )

  test_dm <- data.frame(
    USUBJID = c("SUBJ-001"),
    ACTARMCD = c("TREATMENT")
  )

  test_sdtm_missing_field <- new_sdtm(list(vs = test_vs_missing_field, dm = test_dm))

  # Test missing required field (VSSTRESN)
  expect_error(
    add_baseline(test_nif, test_sdtm_missing_field, "vs", "WEIGHT"),
    "Required fields missing in domain data: VSSTRESN"
  )

  # Test with non-existent test code
  test_vs_valid <- tibble::tribble(
    ~USUBJID, ~VSDTC, ~VSTESTCD, ~VSSTRESN, ~VSBLFL,
    "SUBJ-001", "2023-01-01", "WEIGHT", 70, "Y"
  )

  test_sdtm_valid <- new_sdtm(list(vs = test_vs_valid, dm = test_dm))

  expect_error(
    add_baseline(test_nif, test_sdtm_valid, "vs", "HEIGHT"),
    "Test code 'HEIGHT' not found in domain 'vs'"
  )
})


test_that("add_baseline name parameter works correctly", {
  # Test default name (NULL)
  result_default <- add_baseline(
    examplinib_sad_nif,
    examplinib_sad,
    "vs",
    "WEIGHT",
    silent = TRUE
  )
  expect_true("BL_WEIGHT" %in% names(result_default))

  # Test custom name
  result_custom <- add_baseline(
    examplinib_sad_nif,
    examplinib_sad,
    "vs",
    "WEIGHT",
    name = "baseline_weight",
    silent = TRUE
  )
  expect_true("baseline_weight" %in% names(result_custom))
  expect_false("BL_WEIGHT" %in% names(result_custom))

  # Verify both results have the same values, just different column names
  expect_equal(
    result_default$BL_WEIGHT,
    result_custom$baseline_weight
  )
})

