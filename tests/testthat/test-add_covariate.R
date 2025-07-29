# Create test fixtures
create_test_nif <- function() {
  nif_df <- tibble::tribble(
    ~ID, ~USUBJID, ~DTC,          ~EVID, ~AMT, ~DV, ~CMT,
    1,  "SUBJ-001", "2020-01-15", 1,     100,  NA,  1,
    1,  "SUBJ-001", "2020-01-16", 0,     0,    10,  2,
    1,  "SUBJ-001", "2020-01-17", 0,     0,    20,  2,
    1,  "SUBJ-001", "2020-01-20", 0,     0,    30,  2,
    2,  "SUBJ-002", "2020-01-15", 1,     200,  NA,  1,
    2,  "SUBJ-002", "2020-01-16", 0,     0,    15,  2,
    2,  "SUBJ-002", "2020-01-17", 0,     0,    25,  2,
    2,  "SUBJ-002", "2020-01-20", 0,     0,    35,  2
  ) %>%
    mutate(DTC = lubridate::as_datetime(DTC)) %>%
    new_nif()

  # class(nif_df) <- c("nif", "data.frame")
  return(nif_df)
}

create_test_sdtm <- function() {
  # Create DM domain
  dm <- tibble::tribble(
    ~USUBJID,   ~DOMAIN, ~SEX, ~RFSTDTC,    ~RFENDTC,
    "SUBJ-001", "DM",    "M",  "2020-01-15", "2020-02-15",
    "SUBJ-002", "DM",    "F",  "2020-01-15", "2020-02-15",
    "SUBJ-003", "DM",    "M",  "2020-01-15", "2020-02-15"
  )

  # Create VS domain
  vs <- tibble::tribble(
    ~USUBJID,   ~DOMAIN, ~VSTESTCD, ~VSSTRESN, ~VSDTC,
    "SUBJ-001", "VS",    "WEIGHT",  70,        "2020-01-15",
    "SUBJ-001", "VS",    "HEIGHT",  175,       "2020-01-15",
    "SUBJ-001", "VS",    "WEIGHT",  71,        "2020-01-17",
    "SUBJ-001", "VS",    "WEIGHT",  72,        "2020-01-19",
    "SUBJ-002", "VS",    "WEIGHT",  65,        "2020-01-15",
    "SUBJ-002", "VS",    "HEIGHT",  160,       "2020-01-15",
    "SUBJ-002", "VS",    "WEIGHT",  66,        "2020-01-17",
    "SUBJ-002", "VS",    "WEIGHT",  67,        "2020-01-19",
    "SUBJ-003", "VS",    "WEIGHT",  80,        "2020-01-15",
    "SUBJ-003", "VS",    "HEIGHT",  180,       "2020-01-15"
  )

  # Create LB domain
  lb <- tibble::tribble(
    ~USUBJID,   ~DOMAIN, ~LBTESTCD, ~LBSTRESN, ~LBDTC,
    "SUBJ-001", "LB",    "CREAT",   0.8,       "2020-01-15",
    "SUBJ-001", "LB",    "CREAT",   0.9,       "2020-01-18",
    "SUBJ-002", "LB",    "CREAT",   0.7,       "2020-01-15",
    "SUBJ-002", "LB",    "CREAT",   0.7,       "2020-01-18",
    "SUBJ-003", "LB",    "CREAT",   1.0,       "2020-01-15"
  )

  # Create sdtm object
  sdtm <- list(
    domains = list(
      dm = dm,
      vs = vs,
      lb = lb
    ),
    dm = dm,
    vs = vs,
    lb = lb
  )

  class(sdtm) <- c("sdtm", "list")
  return(sdtm)
}


# Test for basic functionality
test_that("add_covariate works with valid inputs", {
  nif <- create_test_nif()
  sdtm <- create_test_sdtm()

  # Add WEIGHT covariate
  result <- add_covariate(nif, sdtm, "vs", "WEIGHT", covariate = "wt")

  # Check that the covariate was added
  expect_true("wt" %in% names(result))

  # Check that values were properly carried forward for subject 1
  subj1 <- result %>% filter(USUBJID == "SUBJ-001")
  expect_equal(subj1$wt[1], 70)  # From 2020-01-15
  expect_equal(subj1$wt[2], 70)  # Carried forward
  expect_equal(subj1$wt[3], 71)  # From 2020-01-17
  expect_equal(subj1$wt[4], 72)  # From 2020-01-19 (carried forward to 2020-01-20)

  # Check that values were properly carried forward for subject 2
  subj2 <- result %>% filter(USUBJID == "SUBJ-002")
  expect_equal(subj2$wt[1], 65)  # From 2020-01-15
  expect_equal(subj2$wt[2], 65)  # Carried forward
  expect_equal(subj2$wt[3], 66)  # From 2020-01-17
  expect_equal(subj2$wt[4], 67)  # From 2020-01-19 (carried forward to 2020-01-20)
})


# Test for validation of nif object
test_that("add_covariate validates nif object", {
  sdtm <- create_test_sdtm()
  nif <- data.frame(ID = 1, USUBJID = "SUBJ-001")  # Not a nif object

  expect_error(
    add_covariate(nif, sdtm, "vs", "WEIGHT", covariate = "wt"),
    "Input must be a nif object"
  )
})


# Test for validation of missing sdtm
test_that("add_covariate validates sdtm is provided", {
  nif <- create_test_nif()

  expect_error(
    add_covariate(nif = nif, domain = "vs", testcd = "WEIGHT", covariate = "wt"),
    'argument "sdtm" is missing, with no default'
  )
})


# Test for validation of domain existence
test_that("add_covariate validates domain exists", {
  nif <- create_test_nif()
  sdtm <- create_test_sdtm()

  expect_error(
    add_covariate(nif, sdtm, "xx", "WEIGHT", covariate = "wt"),
    "Domain 'xx' not found in sdtm object"
  )
})


# Test for validation of required fields
test_that("add_covariate validates required fields exist", {
  nif <- create_test_nif()
  sdtm <- create_test_sdtm()

  # Create a broken VS domain without VSSTRESN
  # broken_vs <- sdtm$domains$vs %>% select(-VSSTRESN)
  broken_vs <- domain(sdtm, "vs") %>% select(-VSSTRESN)
  sdtm$domains$vs <- broken_vs
  sdtm$vs <- broken_vs

  expect_error(
    add_covariate(nif, sdtm, "vs", "WEIGHT", covariate = "wt"),
    "Required fields missing in domain data: VSSTRESN"
  )
})


# Test for validation of testcd existence
test_that("add_covariate validates testcd exists", {
  nif <- create_test_nif()
  sdtm <- create_test_sdtm()

  expect_error(
    add_covariate(nif, sdtm, "vs", "NONEXISTENT", covariate = "wt"),
    "Test code 'NONEXISTENT' not found in domain 'vs'"
  )
})


# Test for validation of matching subjects
test_that("add_covariate validates matching subjects exist", {
  nif <- create_test_nif()
  sdtm <- create_test_sdtm()

  # Modify USUBJID in nif to create mismatch
  nif$USUBJID <- paste0(nif$USUBJID, "-MISMATCH")

  expect_error(
    add_covariate(nif, sdtm, "vs", "WEIGHT", covariate = "wt"),
    "No matching subjects found between SDTM domain and NIF object"
  )
})


# Test for empty result after filtering
test_that("add_covariate casts error if no data after filtering", {
  nif <- create_test_nif()
  sdtm <- create_test_sdtm()

  # Use a filter that eliminates all rows
  expect_error(
    add_covariate(nif, sdtm, "vs", "WEIGHT",
                  observation_filter = "VSSTRESN > 1000",
                  covariate = "wt")
  )
})


# Test with custom field names
test_that("add_covariate works with custom field names", {
  nif <- create_test_nif()
  sdtm <- create_test_sdtm()

  # Rename fields in VS domain
  vs_custom <- sdtm$domains$vs %>%
    rename(CUSTOM_DTC = VSDTC,
           CUSTOM_TESTCD = VSTESTCD,
           CUSTOM_STRESN = VSSTRESN)

  sdtm$domains$vs <- vs_custom
  # sdtm$vs <- vs_custom

  # Add WEIGHT covariate with custom field names
  result <- add_covariate(nif, sdtm, "vs", "WEIGHT",
                         DTC_field = "CUSTOM_DTC",
                         TESTCD_field = "CUSTOM_TESTCD",
                         DV_field = "CUSTOM_STRESN",
                         covariate = "wt")

  # Check that the covariate was added
  expect_true("wt" %in% names(result))
})


# Test with duplicated observations on same date
test_that("add_covariate handles duplicated observations correctly", {
  nif <- create_test_nif()
  sdtm <- create_test_sdtm()

  # Add duplicate observation on same date with different values
  dup_row <- sdtm$domains$vs[1, ]
  dup_row$VSSTRESN <- 75  # Different value
  sdtm$domains$vs <- bind_rows(sdtm$domains$vs, dup_row)
  # sdtm$vs <- sdtm$domains$vs

  # Should take the last value due to distinct() in the function
  result <- add_covariate(nif, sdtm, "vs", "WEIGHT", covariate = "wt")

  # Check that only one value was used (should be 75, the last one)
  subj1 <- result %>% filter(USUBJID == "SUBJ-001")
  expect_equal(subj1$wt[1], 72.5)
})



# Test with default covariate name
test_that("add_covariate uses default covariate name if not specified", {
  nif <- create_test_nif()
  sdtm <- create_test_sdtm()

  # Add covariate without specifying name
  result <- add_covariate(nif, sdtm, "vs", "WEIGHT")

  # Check that the default name (testcd) was used
  expect_true("WEIGHT" %in% names(result))
})

