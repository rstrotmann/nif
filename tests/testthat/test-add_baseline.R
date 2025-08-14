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
      ~USUBJID,   ~ACTARMCD, ~DOMAIN,
      "SUBJ-001", "TREATMENT", "DM",
      "SUBJ-002", "TREATMENT", "DM"
    ),

    vs = tibble::tribble(
        ~USUBJID,       ~VSDTC, ~VSTESTCD, ~VSSTRESN, ~VSBLFL, ~DOMAIN,
      "SUBJ-001", "2023-01-01",  "WEIGHT",        70,     "Y", "VS",
      "SUBJ-001", "2023-01-02",  "WEIGHT",        71,     "N", "VS",
      "SUBJ-002", "2023-01-01",  "WEIGHT",        80,     "Y", "VS",
      "SUBJ-002", "2023-01-02",  "HEIGHT",       180,     "Y", "VS"
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
    ~USUBJID,       ~VSDTC, ~VSTESTCD, ~VSSTRESN, ~VSBLFL,  ~DOMAIN,
    "SUBJ-001", "2023-01-01",  "WEIGHT",        70,     "",  "VS",
    "SUBJ-001", "2023-01-02",  "WEIGHT",        72,     "Y", "VS",
    "SUBJ-002", "2023-01-01",  "WEIGHT",        80,     "",  "VS",
    "SUBJ-002", "2023-01-02",  "WEIGHT",        82,     "Y", "VS"
  )

  test_dm <- tibble::tribble(
    ~USUBJID,   ~ACTARMCD,  ~DOMAIN,
    "SUBJ-001", "TREATMENT", "DM",
    "SUBJ-002", "TREATMENT",  "DM"
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
    VSBLFL = c("Y", "Y"),
    DOMAIN = c("VS", "VS")
  )

  test_dm <- data.frame(
    USUBJID = c("SUBJ-001", "SUBJ-002"),
    ACTARMCD = c("TREATMENT", "TREATMENT"),
    DOMAIN = c("DM", "DM")
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
    VSBLFL = c("Y"),
    DOMAIN = c("VS")
  )

  test_dm <- data.frame(
    USUBJID = c("SUBJ-001"),
    ACTARMCD = c("TREATMENT"),
    DOMAIN = c("DM")
  )

  test_sdtm <- new_sdtm(list(vs = test_vs, dm = test_dm))

  # Test missing nif object
  fake_nif <- data.frame(USUBJID = "SUBJ-001")
  expect_error(
    add_baseline(fake_nif, test_sdtm, "vs", "WEIGHT"),
    "Input must be a nif object"
  )

  # Test missing SDTM object
  expect_error(
    add_baseline(test_nif, domain = "vs", testcd = "WEIGHT"),
    'argument "sdtm" is missing, with no default'
  )

  # Test invalid domain
  expect_error(
    add_baseline(test_nif, test_sdtm, "invalid_domain", "WEIGHT"),
    "Domain invalid_domain not found in sdtm object!"
  )

  # Test invalid testcd
  expect_error(
    add_baseline(test_nif, test_sdtm, "vs", "INVALID_TEST"),
    "Testcd INVALID_TEST not found in domain VS!"
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
  ) %>% mutate(DOMAIN = "VS")

  test_dm <- tibble::tribble(
      ~USUBJID,   ~ACTARMCD,
    "SUBJ-001", "TREATMENT"
  ) %>% mutate(DOMAIN = "DM")

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
  ) %>% mutate(DOMAIN = "VS")

  test_dm <- data.frame(
    USUBJID = c("SUBJ-001", "SUBJ-002"),
    ACTARMCD = c("TREATMENT", "TREATMENT")
  ) %>% mutate(DOMAIN = "DM")

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
  ) %>% mutate(DOMAIN = "VS")

  test_dm <- data.frame(
    USUBJID = c("SUBJ-001", "SUBJ-002", "SUBJ-003"),
    ACTARMCD = c("TREATMENT", "TREATMENT", "TREATMENT")
  ) %>% mutate(DOMAIN = "DM")

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
  ) %>% mutate(DOMAIN = "VS")

  test_dm <- data.frame(
    USUBJID = c("SUBJ-001"),
    ACTARMCD = c("TREATMENT")
  ) %>% mutate(DOMAIN = "DM")

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
    "Testcd HEIGHT not found in domain VS!"
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


test_that("add_baseline coding table validation works correctly", {
  test_nif <- tibble::tribble(
    ~USUBJID,         ~DTC,
    "SUBJ-001", "2023-01-01",
    "SUBJ-002", "2023-01-01"
  ) %>%
    new_nif()

  test_vs <- tibble::tribble(
    ~USUBJID,       ~VSDTC,    ~VSTESTCD,   ~VSORRES, ~VSBLFL,
    "SUBJ-001", "2023-01-01",      "SEX",     "MALE",     "Y",
    "SUBJ-002", "2023-01-01",   "FEMALE",   "FEMALE",     "Y"
  ) %>% mutate(DOMAIN = "VS")

  test_dm <- tibble::tribble(
    ~USUBJID,   ~ACTARMCD,
    "SUBJ-001", "TREATMENT",
    "SUBJ-002", "TREATMENT"
  ) %>% mutate(DOMAIN = "DM")

  test_sdtm <- new_sdtm(list(vs = test_vs, dm = test_dm))

  # Test invalid coding table (not a data frame)
  expect_error(
    add_baseline(test_nif, test_sdtm, "vs", "SEX",
                 DV_field = "VSORRES", coding_table = "not_a_df"),
    "coding table must be a data frame!"
  )

  # Test coding table without DV field
  invalid_coding <- tibble::tribble(
    ~VSORRES,
    "MALE",
    "FEMALE"
  )

  expect_error(
    add_baseline(test_nif, test_sdtm, "vs", "SEX",
                 DV_field = "VSORRES", coding_table = invalid_coding),
    "Coding table must include a numeric 'DV' field!"
  )

  # Test coding table with non-numeric DV field
  invalid_coding_numeric <- tibble::tribble(
    ~VSORRES, ~DV,
    "MALE",   "1",
    "FEMALE", "0"
  )

  expect_error(
    add_baseline(test_nif, test_sdtm, "vs", "SEX",
                 DV_field = "VSORRES", coding_table = invalid_coding_numeric),
    "DV field in coding table must be numeric!"
  )

  # Test coding table with no matching join fields
  no_match_coding <- tibble::tribble(
    ~NONEXISTENT, ~DV,
    "MALE",        1,
    "FEMALE",      0
  )

  expect_error(
    add_baseline(test_nif, test_sdtm, "vs", "SEX", DV_field = "VSORRES",
                 coding_table = no_match_coding, silent = TRUE),
    "Coding table cannot be applied - no valid data column!"
  )
})


test_that("add_baseline coding table with multiple join fields works", {
  test_nif <- tibble::tribble(
    ~USUBJID,           ~DTC,
    "SUBJ-001", "2023-01-01",
    "SUBJ-002", "2023-01-01"
  ) %>%
    new_nif()

  test_vs <- tibble::tribble(
    ~USUBJID,       ~VSDTC,   ~VSTESTCD, ~VSORRES, ~VSCAT, ~VSBLFL,
    "SUBJ-001", "2023-01-01",     "SEX",   "MALE", "DEMO",     "Y",
    "SUBJ-002", "2023-01-01",     "SEX", "FEMALE", "DEMO",     "Y"
  ) %>% mutate(DOMAIN = "VS")

  test_dm <- tibble::tribble(
    ~USUBJID,   ~ACTARMCD,
    "SUBJ-001", "TREATMENT",
    "SUBJ-002", "TREATMENT"
  ) %>% mutate(DOMAIN = "DM")

  test_sdtm <- new_sdtm(list(vs = test_vs, dm = test_dm))

  # Coding table with multiple join fields
  multi_field_coding <- tibble::tribble(
    ~VSORRES, ~VSCAT, ~DV,
    "MALE",   "DEMO",   1,
    "FEMALE", "DEMO",   0
  )

  result <- add_baseline(test_nif, test_sdtm, "vs", "SEX",
                        DV_field = "VSORRES", coding_table = multi_field_coding,
                        silent = TRUE)

  expect_equal(result$BL_SEX[result$USUBJID == "SUBJ-001"], 1)
  expect_equal(result$BL_SEX[result$USUBJID == "SUBJ-002"], 0)
})


test_that("add_baseline coding table with partial matches works", {
  test_nif <- tibble::tribble(
      ~USUBJID,         ~DTC,
    "SUBJ-001", "2023-01-01",
    "SUBJ-002", "2023-01-01"
  ) %>%
    new_nif()

  test_vs <- tibble::tribble(
    ~USUBJID,       ~VSDTC,   ~VSTESTCD, ~VSORRES, ~VSCAT, ~VSBLFL,
    "SUBJ-001", "2023-01-01",     "SEX",   "MALE", "DEMO",     "Y",
    "SUBJ-002", "2023-01-01",     "SEX", "FEMALE", "DEMO",     "Y"
  ) %>% mutate(DOMAIN = "VS")

  test_dm <- tibble::tribble(
    ~USUBJID,   ~ACTARMCD,
    "SUBJ-001", "TREATMENT",
    "SUBJ-002", "TREATMENT"
  ) %>% mutate(DOMAIN = "DM")

  test_sdtm <- new_sdtm(list(vs = test_vs, dm = test_dm))

  # Coding table with only one join field (should work)
  single_field_coding <- tibble::tribble(
    ~VSORRES, ~DV,
    "MALE",     1,
    "FEMALE",   0
  )

  result <- add_baseline(test_nif, test_sdtm, "vs", "SEX",
                        DV_field = "VSORRES", coding_table = single_field_coding,
                        silent = TRUE)

  expect_equal(result$BL_SEX[result$USUBJID == "SUBJ-001"], 1)
  expect_equal(result$BL_SEX[result$USUBJID == "SUBJ-002"], 0)
})


test_that("add_baseline coding table with missing values handles correctly", {
  test_nif <- tibble::tribble(
    ~USUBJID,         ~DTC,
    "SUBJ-001", "2023-01-01",
    "SUBJ-002", "2023-01-01",
    "SUBJ-003", "2023-01-01"
  ) %>%
    new_nif()

  test_vs <- tibble::tribble(
    ~USUBJID,       ~VSDTC, ~VSTESTCD, ~VSORRES, ~VSBLFL,
    "SUBJ-001", "2023-01-01",     "SEX",   "MALE",     "Y",
    "SUBJ-002", "2023-01-01",     "SEX", "FEMALE",     "Y",
    "SUBJ-003", "2023-01-01",     "SEX", "OTHER",     "Y"
  ) %>% mutate(DOMAIN = "VS")

  test_dm <- tibble::tribble(
    ~USUBJID,   ~ACTARMCD,
    "SUBJ-001", "TREATMENT",
    "SUBJ-002", "TREATMENT",
    "SUBJ-003", "TREATMENT"
  ) %>% mutate(DOMAIN = "DM")

  test_sdtm <- new_sdtm(list(vs = test_vs, dm = test_dm))

  # Coding table missing one value
  incomplete_coding <- tibble::tribble(
    ~VSORRES, ~DV,
    "MALE",     1,
    "FEMALE",   0
  )

  result <- add_baseline(test_nif, test_sdtm, "vs", "SEX",
                        DV_field = "VSORRES", coding_table = incomplete_coding,
                        silent = TRUE)

  expect_equal(result$BL_SEX[result$USUBJID == "SUBJ-001"], 1)
  expect_equal(result$BL_SEX[result$USUBJID == "SUBJ-002"], 0)
  expect_true(is.na(result$BL_SEX[result$USUBJID == "SUBJ-003"]))
})


test_that("add_baseline handles different summary functions with coding tables", {
  test_nif <- tibble::tribble(
    ~USUBJID,         ~DTC,
    "SUBJ-001", "2023-01-01"
  ) %>%
    new_nif()

  # Need to use different test codes to avoid pivot_wider issues
  test_vs <- tibble::tribble(
    ~USUBJID,       ~VSDTC, ~VSTESTCD, ~VSORRES, ~VSBLFL,
    "SUBJ-001", "2023-01-01",     "SEX",   "MALE",     "Y"
  ) %>% mutate(DOMAIN = "VS")

  test_dm <- tibble::tribble(
    ~USUBJID,   ~ACTARMCD,
    "SUBJ-001", "TREATMENT"
  ) %>% mutate(DOMAIN = "DM")

  test_sdtm <- new_sdtm(list(vs = test_vs, dm = test_dm))

  coding_table <- tibble::tribble(
    ~VSORRES, ~DV,
    "MALE",     1,
    "FEMALE",   0
  )

  # Test with mean (default)
  result_mean <- add_baseline(test_nif, test_sdtm, "vs", "SEX",
                             DV_field = "VSORRES", coding_table = coding_table,
                             summary_function = mean, silent = TRUE)
  expect_equal(result_mean$BL_SEX, 1)

  # Test with max
  result_max <- add_baseline(test_nif, test_sdtm, "vs", "SEX",
                            DV_field = "VSORRES", coding_table = coding_table,
                            summary_function = max, silent = TRUE)
  expect_equal(result_max$BL_SEX, 1)

  # Test with min
  result_min <- add_baseline(test_nif, test_sdtm, "vs", "SEX",
                            DV_field = "VSORRES", coding_table = coding_table,
                            summary_function = min, silent = TRUE)
  expect_equal(result_min$BL_SEX, 1)
})


test_that("add_baseline handles complex coding scenarios", {
  test_nif <- tibble::tribble(
    ~USUBJID,         ~DTC,
    "SUBJ-001", "2023-01-01",
    "SUBJ-002", "2023-01-01",
    "SUBJ-003", "2023-01-01"
  ) %>%
    new_nif()

  test_vs <- tibble::tribble(
    ~USUBJID,       ~VSDTC, ~VSTESTCD, ~VSORRES, ~VSBLFL,
    "SUBJ-001", "2023-01-01",   "RACE", "WHITE",     "Y",
    "SUBJ-002", "2023-01-01",   "RACE", "BLACK",     "Y",
    "SUBJ-003", "2023-01-01",   "RACE", "ASIAN",     "Y"
  ) %>% mutate(DOMAIN = "VS")

  test_dm <- tibble::tribble(
    ~USUBJID,   ~ACTARMCD,
    "SUBJ-001", "TREATMENT",
    "SUBJ-002", "TREATMENT",
    "SUBJ-003", "TREATMENT"
  ) %>% mutate(DOMAIN = "DM")

  test_sdtm <- new_sdtm(list(vs = test_vs, dm = test_dm))

  # Complex coding table with multiple categories
  race_coding <- tibble::tribble(
    ~VSORRES, ~DV,
    "WHITE",    1,
    "BLACK",    2,
    "ASIAN",    3,
    "OTHER",    4
  )

  result <- add_baseline(test_nif, test_sdtm, "vs", "RACE",
                        DV_field = "VSORRES", coding_table = race_coding,
                        silent = TRUE)

  expect_equal(result$BL_RACE[result$USUBJID == "SUBJ-001"], 1)
  expect_equal(result$BL_RACE[result$USUBJID == "SUBJ-002"], 2)
  expect_equal(result$BL_RACE[result$USUBJID == "SUBJ-003"], 3)
})


test_that("add_baseline handles baseline filter with LOBXFL", {
  test_nif <- tibble::tribble(
    ~USUBJID,         ~DTC,
    "SUBJ-001", "2023-01-01",
    "SUBJ-002", "2023-01-01"
  ) %>%
    new_nif()

  test_lb <- tibble::tribble(
    ~USUBJID,       ~LBDTC, ~LBTESTCD, ~LBSTRESN, ~LBLOBXFL,
    "SUBJ-001", "2023-01-01",    "CREA",       1.2,     "Y",
    "SUBJ-001", "2023-01-02",    "CREA",       1.3,     "N",
    "SUBJ-002", "2023-01-01",    "CREA",       1.1,     "Y",
    "SUBJ-002", "2023-01-02",    "CREA",       1.4,     "N"
  ) %>% mutate(DOMAIN = "LB")

  test_dm <- tibble::tribble(
    ~USUBJID,   ~ACTARMCD,
    "SUBJ-001", "TREATMENT",
    "SUBJ-002", "TREATMENT"
  ) %>% mutate(DOMAIN = "DM")

  test_sdtm <- new_sdtm(list(lb = test_lb, dm = test_dm))

  # Should automatically detect LOBXFL and use it
  result <- add_baseline(test_nif, test_sdtm, "lb", "CREA", silent = TRUE)

  expect_equal(result$BL_CREA[result$USUBJID == "SUBJ-001"], 1.2)
  expect_equal(result$BL_CREA[result$USUBJID == "SUBJ-002"], 1.1)
})


test_that("add_baseline handles custom observation filter with coding tables", {
  test_nif <- tibble::tribble(
      ~USUBJID,         ~DTC,
    "SUBJ-001", "2023-01-01",
    "SUBJ-002", "2023-01-01"
  ) %>%
    new_nif()

  test_vs <- tibble::tribble(
      ~USUBJID,       ~VSDTC, ~VSTESTCD, ~VSORRES, ~VSCAT, ~VSBLFL,
    "SUBJ-001", "2023-01-01",     "SEX",   "MALE", "DEMO",     "Y",
    "SUBJ-001", "2023-01-02",     "SEX",   "MALE", "DEMO",     "Y",
    "SUBJ-002", "2023-01-01",     "SEX", "FEMALE", "DEMO",     "Y",
    "SUBJ-002", "2023-01-02",     "SEX", "FEMALE", "DEMO",     "Y"
  ) %>% mutate(DOMAIN = "VS")

  test_dm <- tibble::tribble(
    ~USUBJID,   ~ACTARMCD,
    "SUBJ-001", "TREATMENT",
    "SUBJ-002", "TREATMENT"
  ) %>% mutate(DOMAIN = "DM")

  test_sdtm <- new_sdtm(list(vs = test_vs, dm = test_dm))

  coding_table <- tibble::tribble(
    ~VSORRES, ~DV,
    "MALE",     1,
    "FEMALE",   0
  )

  # Filter to only include first day observations
  result <- add_baseline(
    test_nif, test_sdtm, "vs", "SEX", DV_field = "VSORRES",
    coding_table = coding_table,
    observation_filter = "VSDTC == lubridate::as_datetime('2023-01-01')",
    silent = TRUE)

  expect_equal(result$BL_SEX[result$USUBJID == "SUBJ-001"], 1)
  expect_equal(result$BL_SEX[result$USUBJID == "SUBJ-002"], 0)
})


test_that("add_baseline handles edge cases with coding tables", {
  test_nif <- tibble::tribble(
    ~USUBJID,         ~DTC,
    "SUBJ-001", "2023-01-01"
  ) %>%
    new_nif()

  test_vs <- tibble::tribble(
    ~USUBJID,       ~VSDTC, ~VSTESTCD, ~VSORRES, ~VSBLFL,
    "SUBJ-001", "2023-01-01",     "SEX",   "MALE",     "Y"
  ) %>% mutate(DOMAIN = "VS")

  test_dm <- tibble::tribble(
    ~USUBJID,   ~ACTARMCD,
    "SUBJ-001", "TREATMENT"
  ) %>% mutate(DOMAIN = "DM")

  test_sdtm <- new_sdtm(list(vs = test_vs, dm = test_dm))

  # Test with empty coding table - should fail because no join fields
  empty_coding <- tibble::tribble(
    ~VSORRES, ~DV
  )

  expect_error(
    add_baseline(test_nif, test_sdtm, "vs", "SEX",
                 DV_field = "VSORRES", coding_table = empty_coding),
    "DV field in coding table must be numeric!"
  )

  # Test with coding table that has extra columns
  extra_col_coding <- tibble::tribble(
    ~VSORRES, ~DV, ~EXTRA_COL,
    "MALE",     1,        "X",
    "FEMALE",   0,        "Y"
  )

  result <- add_baseline(test_nif, test_sdtm, "vs", "SEX",
                        DV_field = "VSORRES", coding_table = extra_col_coding,
                        silent = TRUE)

  expect_equal(result$BL_SEX, 1)
})


test_that("add_baseline handles numeric coding with different data types", {
  test_nif <- tibble::tribble(
    ~USUBJID,         ~DTC,
    "SUBJ-001", "2023-01-01",
    "SUBJ-002", "2023-01-01"
  ) %>%
    new_nif()

  test_vs <- tibble::tribble(
    ~USUBJID,       ~VSDTC, ~VSTESTCD, ~VSORRES, ~VSBLFL,
    "SUBJ-001", "2023-01-01",     "SEX",   "MALE",     "Y",
    "SUBJ-002", "2023-01-01",     "SEX", "FEMALE",     "Y"
  ) %>% mutate(DOMAIN = "VS")

  test_dm <- tibble::tribble(
    ~USUBJID,   ~ACTARMCD,
    "SUBJ-001", "TREATMENT",
    "SUBJ-002", "TREATMENT"
  ) %>% mutate(DOMAIN = "DM")

  test_sdtm <- new_sdtm(list(vs = test_vs, dm = test_dm))

  # Test with integer DV values
  int_coding <- tibble::tribble(
    ~VSORRES, ~DV,
    "MALE",     1L,
    "FEMALE",   0L
  )

  result_int <- add_baseline(test_nif, test_sdtm, "vs", "SEX",
                            DV_field = "VSORRES", coding_table = int_coding,
                            silent = TRUE)

  expect_equal(result_int$BL_SEX[result_int$USUBJID == "SUBJ-001"], 1)
  expect_equal(result_int$BL_SEX[result_int$USUBJID == "SUBJ-002"], 0)

  # Test with double DV values
  double_coding <- tibble::tribble(
    ~VSORRES, ~DV,
    "MALE",   1.0,
    "FEMALE", 0.0
  )

  result_double <- add_baseline(test_nif, test_sdtm, "vs", "SEX",
                               DV_field = "VSORRES", coding_table = double_coding,
                               silent = TRUE)

  expect_equal(result_double$BL_SEX[result_double$USUBJID == "SUBJ-001"], 1.0)
  expect_equal(result_double$BL_SEX[result_double$USUBJID == "SUBJ-002"], 0.0)
})


test_that("add_baseline handles multiple test codes with coding tables", {
  test_nif <- tibble::tribble(
    ~USUBJID,         ~DTC,
    "SUBJ-001", "2023-01-01",
    "SUBJ-002", "2023-01-01"
  ) %>%
    new_nif()

  test_vs <- tibble::tribble(
    ~USUBJID,       ~VSDTC, ~VSTESTCD, ~VSORRES, ~VSBLFL,
    "SUBJ-001", "2023-01-01",     "SEX",   "MALE",     "Y",
    "SUBJ-001", "2023-01-01",   "RACE", "WHITE",     "Y",
    "SUBJ-002", "2023-01-01",     "SEX", "FEMALE",     "Y",
    "SUBJ-002", "2023-01-01",   "RACE", "BLACK",     "Y"
  ) %>% mutate(DOMAIN = "VS")

  test_dm <- tibble::tribble(
    ~USUBJID,   ~ACTARMCD,
    "SUBJ-001", "TREATMENT",
    "SUBJ-002", "TREATMENT"
  ) %>% mutate(DOMAIN = "DM")

  test_sdtm <- new_sdtm(list(vs = test_vs, dm = test_dm))

  coding_table <- tibble::tribble(
    ~VSORRES, ~DV,
    "MALE",     1,
    "FEMALE",   0,
    "WHITE",    1,
    "BLACK",    2
  )

  # Test SEX
  result_sex <- add_baseline(test_nif, test_sdtm, "vs", "SEX",
                            DV_field = "VSORRES", coding_table = coding_table,
                            silent = TRUE)

  expect_equal(result_sex$BL_SEX[result_sex$USUBJID == "SUBJ-001"], 1)
  expect_equal(result_sex$BL_SEX[result_sex$USUBJID == "SUBJ-002"], 0)

  # Test RACE
  result_race <- add_baseline(test_nif, test_sdtm, "vs", "RACE",
                             DV_field = "VSORRES", coding_table = coding_table,
                             silent = TRUE)

  expect_equal(result_race$BL_RACE[result_race$USUBJID == "SUBJ-001"], 1)
  expect_equal(result_race$BL_RACE[result_race$USUBJID == "SUBJ-002"], 2)
})

