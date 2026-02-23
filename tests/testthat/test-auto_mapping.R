test_that("auto_mapping creates correct mapping from NULL input", {
  # Create test SDTM object with domains
 test_sdtm <-  sdtm(list(
    dm = tibble::tribble(
      ~DOMAIN, ~STUDYID,   ~USUBJID,
         "DM", "STUDY1", "SUBJ-001"
      ),
    ex = tibble::tribble(
      ~DOMAIN, ~STUDYID,   ~USUBJID,  ~EXTRT,
         "EX", "STUDY1", "SUBJ-001", "DRUG1",
         "EX", "STUDY1", "SUBJ-001", "DRUG2"
      ),
    pc = tibble::tribble(
      ~DOMAIN, ~STUDYID,   ~USUBJID, ~PCTESTCD,
         "PC", "STUDY1", "SUBJ-001",   "DRUG1",
         "PC", "STUDY1", "SUBJ-001",   "DRUG2"
      )
  ))

  result <- auto_mapping(test_sdtm, silent = TRUE)

  expect_equal(nrow(result), 2)
  expect_equal(result$EXTRT, c("DRUG1", "DRUG2"))
  expect_equal(result$TESTCD, c("DRUG1", "DRUG2"))
  expect_equal(result$ANALYTE, c("DRUG1", "DRUG2"))
})


test_that("auto_mapping creates correct mapping from single formula", {
  # Create test SDTM object with domains
  test_sdtm <- sdtm(list(
    dm = tibble::tribble(
      ~DOMAIN, ~STUDYID,   ~USUBJID,
         "DM", "STUDY1", "SUBJ-001"
      ),
    ex = tibble::tribble(
      ~DOMAIN, ~STUDYID,   ~USUBJID,  ~EXTRT,
         "EX", "STUDY1", "SUBJ-001", "DRUG1",
         "EX", "STUDY1", "SUBJ-001", "DRUG2"
      ),
    pc = tibble::tribble(
      ~DOMAIN, ~STUDYID,   ~USUBJID, ~PCTESTCD,
         "PC", "STUDY1", "SUBJ-001",     "PK1",
         "PC", "STUDY1", "SUBJ-001",     "PK2"
      )
  ))

  result <- auto_mapping(test_sdtm, PK1 ~ DRUG1)

  expect_equal(nrow(result), 1)
  expect_equal(result$EXTRT, "DRUG1")
  expect_equal(result$TESTCD, "PK1")
  expect_equal(result$ANALYTE, "PK1")
})


test_that("auto_mapping creates correct mapping from list of formulae", {
  # Create test SDTM object with domains
  dm_data <- tibble::tribble(
    ~DOMAIN, ~STUDYID, ~USUBJID,
    "DM", "STUDY1", "SUBJ-001"
  )

  ex_data <- tibble::tribble(
    ~DOMAIN, ~STUDYID, ~USUBJID, ~EXTRT,
    "EX", "STUDY1", "SUBJ-001", "DRUG1",
    "EX", "STUDY1", "SUBJ-001", "DRUG2"
  )

  pc_data <- tibble::tribble(
    ~DOMAIN, ~STUDYID, ~USUBJID, ~PCTESTCD,
    "PC", "STUDY1", "SUBJ-001", "PK1",
    "PC", "STUDY1", "SUBJ-001", "PK2"
  )

  test_sdtm <- sdtm(list(dm = dm_data, ex = ex_data, pc = pc_data))

  result <- auto_mapping(test_sdtm, PK1 ~ DRUG1, PK2 ~ DRUG2)

  expect_equal(nrow(result), 2)
  expect_equal(result$EXTRT, c("DRUG1", "DRUG2"))
  expect_equal(result$TESTCD, c("PK1", "PK2"))
  expect_equal(result$ANALYTE, c("PK1", "PK2"))
})


test_that("auto_mapping handles multiple analytes from same treatment", {
  # Create test SDTM object with domains
  dm_data <- tibble::tribble(
    ~DOMAIN, ~STUDYID, ~USUBJID,
    "DM", "STUDY1", "SUBJ-001"
  )

  ex_data <- tibble::tribble(
    ~DOMAIN, ~STUDYID, ~USUBJID, ~EXTRT,
    "EX", "STUDY1", "SUBJ-001", "DRUG1"
  )

  pc_data <- tibble::tribble(
    ~DOMAIN, ~STUDYID, ~USUBJID, ~PCTESTCD,
    "PC", "STUDY1", "SUBJ-001", "PK1",
    "PC", "STUDY1", "SUBJ-001", "PK2"
  )

  test_sdtm <- sdtm(list(dm = dm_data, ex = ex_data, pc = pc_data))

  result <- auto_mapping(test_sdtm, PK1 + PK2 ~ DRUG1)

  expect_equal(nrow(result), 2)
  expect_equal(result$EXTRT, c("DRUG1", "DRUG1"))
  expect_equal(result$TESTCD, c("PK1", "PK2"))
  expect_equal(result$ANALYTE, c("PK1", "PK2"))
})


test_that("auto_mapping detects duplicate mappings", {
  # Create test SDTM object with domains
  dm_data <- tibble::tribble(
    ~DOMAIN, ~STUDYID, ~USUBJID,
    "DM", "STUDY1", "SUBJ-001"
  )

  ex_data <- tibble::tribble(
    ~DOMAIN, ~STUDYID, ~USUBJID, ~EXTRT,
    "EX", "STUDY1", "SUBJ-001", "DRUG1"
  )

  pc_data <- tibble::tribble(
    ~DOMAIN, ~STUDYID, ~USUBJID, ~PCTESTCD,
    "PC", "STUDY1", "SUBJ-001", "PK1"
  )

  test_sdtm <- sdtm(list(dm = dm_data, ex = ex_data, pc = pc_data))

  expect_error(
    auto_mapping(test_sdtm, PK1 ~ DRUG1, PK1 ~ DRUG1),
    "Duplicate mappings found"
  )
})


test_that("auto_mapping validates input types", {
  # Create test SDTM object with domains
  dm_data <- tibble::tribble(
    ~DOMAIN, ~STUDYID, ~USUBJID,
    "DM", "STUDY1", "SUBJ-001"
  )

  ex_data <- tibble::tribble(
    ~DOMAIN, ~STUDYID, ~USUBJID, ~EXTRT,
    "EX", "STUDY1", "SUBJ-001", "DRUG1"
  )

  pc_data <- tibble::tribble(
    ~DOMAIN, ~STUDYID, ~USUBJID, ~PCTESTCD,
    "PC", "STUDY1", "SUBJ-001", "PK1"
  )

  test_sdtm <- sdtm(list(dm = dm_data, ex = ex_data, pc = pc_data))

  # Test invalid mapping type
  expect_error(
    auto_mapping(test_sdtm, "not a formula"),
    "mappings must be formulae!"
  )

  # Test invalid list element
  expect_error(
    auto_mapping(test_sdtm, PK1 ~ DRUG1, "not a formula"),
    "mappings must be formulae!"
  )
})


test_that("auto_mapping sets correct METABOLITE flags", {
  test_sdtm <- sdtm(list(
    dm = tibble::tribble(
      ~DOMAIN, ~STUDYID,   ~USUBJID,
         "DM", "STUDY1", "SUBJ-001"
      ),
    ex = tibble::tribble(
      ~DOMAIN, ~STUDYID,   ~USUBJID,  ~EXTRT,
         "EX", "STUDY1", "SUBJ-001", "DRUG1"
      ),
    pc = tibble::tribble(
      ~DOMAIN, ~STUDYID,   ~USUBJID, ~PCTESTCD,
         "PC", "STUDY1", "SUBJ-001",   "DRUG1",
         "PC", "STUDY1", "SUBJ-001",      "M1",
         "PC", "STUDY1", "SUBJ-001",      "M2"
      )
  ))

  result <- auto_mapping(test_sdtm, DRUG1 + M1 + M2 ~ DRUG1)

  expect_equal(nrow(result), 3)
  expect_equal(result$METABOLITE, c(FALSE, TRUE, TRUE))
})


test_that("auto_mapping generates automatic mapping for multiple analytes", {
  # Create test SDTM object with domains
  test_sdtm <- sdtm(list(
    dm = tibble::tribble(
      ~DOMAIN, ~STUDYID, ~USUBJID,
      "DM", "STUDY1", "SUBJ-001",
      "DM", "STUDY1", "SUBJ-002"
    ),
    ex = tibble::tribble(
      ~DOMAIN, ~STUDYID, ~USUBJID, ~EXTRT,
      "EX", "STUDY1", "SUBJ-001", "DRUG1",
      "EX", "STUDY1", "SUBJ-001", "DRUG2",
      "EX", "STUDY1", "SUBJ-002", "DRUG1"
    ),
    pc = tibble::tribble(
      ~DOMAIN, ~STUDYID, ~USUBJID, ~PCTESTCD,
      "PC", "STUDY1", "SUBJ-001", "DRUG1",
      "PC", "STUDY1", "SUBJ-001", "DRUG2",
      "PC", "STUDY1", "SUBJ-002", "DRUG1"
    )
  ))

  result <- auto_mapping(test_sdtm, silent = TRUE)

  expect_equal(nrow(result), 2)
  expect_equal(result$ANALYTE, c("DRUG1", "DRUG2"))
})

