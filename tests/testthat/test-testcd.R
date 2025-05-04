test_that("testcd() returns empty data frame for empty sdtm object", {
  # Create empty sdtm object
  empty_sdtm <- new_sdtm(list())

  # Test
  expect_equal(nrow(testcd(empty_sdtm)), 0)
})


test_that("testcd() handles domains without TESTCD columns", {
  # Create sdtm object with domains but no TESTCD columns
  sdtm_data <- list(
    dm = tribble(
      ~DOMAIN, ~USUBJID,
      "DM", "SUBJ001"
    ),
    vs = tribble(
      ~DOMAIN, ~USUBJID,
      "VS", "SUBJ001"
    )
  )
  sdtm_obj <- new_sdtm(sdtm_data)

  # Test
  expect_equal(nrow(testcd(sdtm_obj)), 0)
})


test_that("testcd() extracts TESTCD values correctly", {
  # Create sdtm object with domains containing TESTCD columns
  sdtm_data <- list(
    dm = tribble(
      ~DOMAIN, ~USUBJID, ~DMTESTCD,
          "DM", "SUBJ001", "AGE",
          "DM", "SUBJ002", "AGE"
    ),
    vs = tribble(
      ~DOMAIN, ~USUBJID, ~VSTESTCD,
        "VS", "SUBJ001", "SYSBP",
        "VS", "SUBJ001", "DIABP",
        "VS", "SUBJ002", "SYSBP"
    ),
    pc = tribble(
      ~DOMAIN, ~USUBJID, ~PCTESTCD,
        "PC", "SUBJ001", "CONC",
        "PC", "SUBJ002", "CONC"
    )
  )
  sdtm_obj <- new_sdtm(sdtm_data)

  # Expected output
  expected <- tribble(
    ~DOMAIN, ~TESTCD,
    "DM", "AGE",
    "VS", "SYSBP",
    "VS", "DIABP",
    "PC", "CONC"
  )

  # Test
  expect_equal(testcd(sdtm_obj), as.data.frame(expected))
})


test_that("testcd() handles domains with missing DOMAIN column", {
  # Create sdtm object with a domain missing DOMAIN column
  sdtm_data <- list(
    dm = tribble(
      ~USUBJID, ~DMTESTCD,
      "SUBJ001", "AGE"
    ),
    vs = tribble(
      ~DOMAIN, ~USUBJID, ~VSTESTCD,
      "VS", "SUBJ001", "SYSBP"
    )
  )
  sdtm_obj <- new_sdtm(sdtm_data)

  # Expected output (only VS domain should be included)
  expected <- tribble(
    ~DOMAIN, ~TESTCD,
    "VS", "SYSBP"
  )

  # Test with warning
  expect_warning(
    result <- testcd(sdtm_obj),
    "Domain data frame missing DOMAIN column"
  )
  expect_equal(result, as.data.frame(expected))
})


test_that("testcd() handles empty DOMAIN column", {
  # Create sdtm object with empty DOMAIN column
  sdtm_data <- list(
    dm = tribble(
      ~DOMAIN, ~USUBJID, ~DMTESTCD,
      "", "SUBJ001", "AGE"
    ),
    vs = tribble(
      ~DOMAIN, ~USUBJID, ~VSTESTCD,
      "VS", "SUBJ001", "SYSBP"
    )
  )
  sdtm_obj <- new_sdtm(sdtm_data)

  # Expected output (only VS domain should be included)
  expected <- tribble(
    ~DOMAIN, ~TESTCD,
       "VS", "SYSBP"
  )

  result <- testcd(sdtm_obj)
  expect_equal(result, as.data.frame(expected))
})


test_that("testcd() handles mixed case domain names", {
  # Create sdtm object with mixed case domain names
  sdtm_data <- list(
    dm = tribble(
      ~DOMAIN, ~USUBJID, ~DMTESTCD,
      "dm", "SUBJ001", "AGE",
      "DM", "SUBJ002", "AGE"
    ),
    vs = tribble(
      ~DOMAIN, ~USUBJID, ~VSTESTCD,
      "vs", "SUBJ001", "SYSBP",
      "VS", "SUBJ002", "SYSBP"
    )
  )
  sdtm_obj <- new_sdtm(sdtm_data)

  # Expected output (domains should be uppercase)
  expected <- tribble(
    ~DOMAIN, ~TESTCD,
    "DM", "AGE",
    "VS", "SYSBP"
  )

  # Test
  expect_equal(testcd(sdtm_obj), as.data.frame(expected))
})

