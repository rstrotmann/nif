test_that("testcd() returns empty data frame for empty sdtm object", {
  # Create empty sdtm object
  empty_sdtm <- sdtm(list())

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
  sdtm_obj <- sdtm(sdtm_data)

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
  sdtm_obj <- sdtm(sdtm_data)

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

#
# test_that("testcd() handles domains with missing DOMAIN column", {
#   # Create sdtm object with a domain missing DOMAIN column
#   sdtm_data <- list(
#     dm = tribble(
#       ~USUBJID, ~DMTESTCD,
#       "SUBJ001", "AGE"
#     ),
#     vs = tribble(
#       ~DOMAIN, ~USUBJID, ~VSTESTCD,
#       "VS", "SUBJ001", "SYSBP"
#     )
#   )
#   sdtm_obj <- sdtm(sdtm_data)
#
#   # Expected output (only VS domain should be included)
#   expected <- tribble(
#     ~DOMAIN, ~TESTCD,
#     "VS", "SYSBP"
#   )
#
#   # # Test with warning
#   # expect_warning(
#   #   result <- testcd(sdtm_obj),
#   #   "Domain data frame missing DOMAIN column"
#   # )
#   # expect_equal(result, as.data.frame(expected))
# })


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
  sdtm_obj <- sdtm(sdtm_data)

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
  sdtm_obj <- sdtm(sdtm_data)

  # Expected output (domains should be uppercase)
  expected <- tribble(
    ~DOMAIN, ~TESTCD,
    "DM", "AGE",
    "VS", "SYSBP"
  )

  # Test
  expect_equal(testcd(sdtm_obj), as.data.frame(expected))
})


# Additional tests to identify potential problems

test_that("testcd() handles NA values in TESTCD columns", {
  # Create sdtm object with NA values in TESTCD columns
  sdtm_data <- list(
    dm = tribble(
      ~DOMAIN, ~USUBJID, ~DMTESTCD,
      "DM", "SUBJ001", "AGE",
      "DM", "SUBJ002", NA_character_
    ),
    vs = tribble(
      ~DOMAIN, ~USUBJID, ~VSTESTCD,
      "VS", "SUBJ001", "SYSBP",
      "VS", "SUBJ002", NA_character_
    )
  )
  sdtm_obj <- sdtm(sdtm_data)

  # Expected output (NA values should be included)
  expected <- tribble(
    ~DOMAIN, ~TESTCD,
    "DM", "AGE",
    "DM", NA_character_,
    "VS", "SYSBP",
    "VS", NA_character_
  )

  # Test
  result <- testcd(sdtm_obj)
  expect_equal(result, as.data.frame(expected))
})


test_that("testcd() handles domain parameter correctly", {
  # Create sdtm object with multiple domains
  sdtm_data <- list(
    dm = tribble(
      ~DOMAIN, ~USUBJID, ~DMTESTCD,
      "DM", "SUBJ001", "AGE"
    ),
    vs = tribble(
      ~DOMAIN, ~USUBJID, ~VSTESTCD,
      "VS", "SUBJ001", "SYSBP"
    ),
    pc = tribble(
      ~DOMAIN, ~USUBJID, ~PCTESTCD,
      "PC", "SUBJ001", "CONC"
    )
  )
  sdtm_obj <- sdtm(sdtm_data)

  # Test with specific domain
  result_dm <- testcd(sdtm_obj, domain = "dm")
  expect_equal(nrow(result_dm), 1)
  expect_equal(result_dm$DOMAIN, "DM")
  expect_equal(result_dm$TESTCD, "AGE")

  # Test with multiple domains
  result_multi <- testcd(sdtm_obj, domain = c("dm", "vs"))
  expect_equal(nrow(result_multi), 2)
  expect_equal(sort(result_multi$DOMAIN), c("DM", "VS"))

  # Test with non-existent domain
  result_nonexistent <- testcd(sdtm_obj, domain = "NONEXISTENT")
  expect_equal(nrow(result_nonexistent), 0)
})


test_that("testcd() handles case sensitivity in domain parameter", {
  # Create sdtm object
  sdtm_data <- list(
    vs = tribble(
      ~DOMAIN, ~USUBJID, ~VSTESTCD,
      "VS", "SUBJ001", "HR"
    )
  )
  sdtm_obj <- sdtm(sdtm_data)

  # Test with lowercase domain name
  result_lower <- testcd(sdtm_obj, domain = "vs")
  expect_equal(nrow(result_lower), 1)
  expect_equal(result_lower$DOMAIN, "VS")

  # Test with uppercase domain name
  result_upper <- testcd(sdtm_obj, domain = "VS")
  expect_equal(nrow(result_upper), 1)
  expect_equal(result_upper$DOMAIN, "VS")
})


test_that("testcd() handles empty data frames within domains", {
  # Create sdtm object with empty data frames
  sdtm_data <- list(
    dm = data.frame(DOMAIN = character(), USUBJID = character(), DMTESTCD = character()),
    vs = tribble(
      ~DOMAIN, ~USUBJID, ~VSTESTCD,
      "VS", "SUBJ001", "SYSBP"
    )
  )
  sdtm_obj <- sdtm(sdtm_data)

  # Expected output (only VS domain should be included)
  expected <- tribble(
    ~DOMAIN, ~TESTCD,
    "VS", "SYSBP"
  )

  expect_warning(
    result <- testcd(sdtm_obj),
    "DOMAIN column is empty"
  )
  expect_equal(result, as.data.frame(expected))
})


test_that("testcd() handles duplicate TESTCD values correctly", {
  # Create sdtm object with duplicate TESTCD values
  sdtm_data <- list(
    vs = tribble(
      ~DOMAIN, ~USUBJID, ~VSTESTCD,
      "VS", "SUBJ001", "SYSBP",
      "VS", "SUBJ002", "SYSBP",
      "VS", "SUBJ003", "SYSBP"
    )
  )
  sdtm_obj <- sdtm(sdtm_data)

  # Expected output (duplicates should be removed)
  expected <- tribble(
    ~DOMAIN, ~TESTCD,
    "VS", "SYSBP"
  )

  result <- testcd(sdtm_obj)
  expect_equal(result, as.data.frame(expected))
})


test_that("testcd() handles invalid input types", {
  # Test with non-sdtm object
  expect_error(
    testcd("not an sdtm object"),
    "Input must be a sdtm object"
  )

  # Test with NULL input
  expect_error(
    testcd(NULL),
    "Input must be a sdtm object"
  )

  # Test with numeric input
  expect_error(
    testcd(123),
    "Input must be a sdtm object"
  )
})


test_that("testcd() handles domain parameter validation", {
  # Create sdtm object
  sdtm_data <- list(
    dm = tribble(
      ~DOMAIN, ~USUBJID, ~DMTESTCD,
      "DM", "SUBJ001", "AGE"
    )
  )
  sdtm_obj <- sdtm(sdtm_data)

  # Test with invalid domain parameter types
  expect_error(
    testcd(sdtm_obj, domain = 123),
    "domain.*string"
  )

  expect_error(
    testcd(sdtm_obj, domain = list("dm")),
    "domain.*string"
  )
})

test_that("testcd() handles domains with all NA TESTCD values", {
  # Create sdtm object with all NA TESTCD values
  sdtm_data <- list(
    dm = tribble(
      ~DOMAIN, ~USUBJID, ~DMTESTCD,
      "DM", "SUBJ001", NA_character_,
      "DM", "SUBJ002", NA_character_
    )
  )
  sdtm_obj <- sdtm(sdtm_data)

  # Expected output (should include NA values)
  expected <- tribble(
    ~DOMAIN, ~TESTCD,
    "DM", NA_character_
  )

  result <- testcd(sdtm_obj)
  expect_equal(result, as.data.frame(expected))
})


test_that("testcd() handles domains with empty TESTCD values", {
  # Create sdtm object with empty TESTCD values
  sdtm_data <- list(
    dm = tribble(
      ~DOMAIN, ~USUBJID, ~DMTESTCD,
      "DM", "SUBJ001", "",
      "DM", "SUBJ002", "AGE"
    )
  )
  sdtm_obj <- sdtm(sdtm_data)

  # Expected output (should include empty string)
  expected <- tribble(
    ~DOMAIN, ~TESTCD,
    "DM", "",
    "DM", "AGE"
  )

  result <- testcd(sdtm_obj)
  expect_equal(result, as.data.frame(expected))
})


test_that("testcd() handles domains with whitespace in TESTCD values", {
  # Create sdtm object with whitespace in TESTCD values
  sdtm_data <- list(
    dm = tribble(
      ~DOMAIN, ~USUBJID, ~DMTESTCD,
      "DM", "SUBJ001", " AGE ",
      "DM", "SUBJ002", "AGE"
    )
  )
  sdtm_obj <- sdtm(sdtm_data)

  # Expected output (whitespace should be preserved)
  expected <- tribble(
    ~DOMAIN, ~TESTCD,
    "DM", " AGE ",
    "DM", "AGE"
  )

  result <- testcd(sdtm_obj)
  expect_equal(result, as.data.frame(expected))
})


test_that("testcd() handles domains with special characters in TESTCD values", {
  # Create sdtm object with special characters in TESTCD values
  sdtm_data <- list(
    dm = tribble(
      ~DOMAIN, ~USUBJID, ~DMTESTCD,
      "DM", "SUBJ001", "AGE_123",
      "DM", "SUBJ002", "AGE-456",
      "DM", "SUBJ003", "AGE.789"
    )
  )
  sdtm_obj <- sdtm(sdtm_data)

  # Expected output (special characters should be preserved)
  expected <- tribble(
    ~DOMAIN, ~TESTCD,
    "DM", "AGE_123",
    "DM", "AGE-456",
    "DM", "AGE.789"
  )

  result <- testcd(sdtm_obj)
  expect_equal(result, as.data.frame(expected))
})


test_that("testcd() handles domains with numeric TESTCD values", {
  # Create sdtm object with numeric TESTCD values (converted to character)
  sdtm_data <- list(
    dm = tribble(
      ~DOMAIN, ~USUBJID, ~DMTESTCD,
      "DM", "SUBJ001", "123",
      "DM", "SUBJ002", "456"
    )
  )
  sdtm_obj <- sdtm(sdtm_data)

  # Expected output (numeric strings should be preserved)
  expected <- tribble(
    ~DOMAIN, ~TESTCD,
    "DM", "123",
    "DM", "456"
  )

  result <- testcd(sdtm_obj)
  expect_equal(result, as.data.frame(expected))
})


test_that("testcd() handles domains with very long TESTCD values", {
  # Create sdtm object with very long TESTCD values
  long_testcd <- paste(rep("A", 1000), collapse = "")
  sdtm_data <- list(
    dm = tribble(
      ~DOMAIN, ~USUBJID, ~DMTESTCD,
      "DM", "SUBJ001", long_testcd
    )
  )
  sdtm_obj <- sdtm(sdtm_data)

  # Expected output (long strings should be preserved)
  expected <- tribble(
    ~DOMAIN, ~TESTCD,
    "DM", long_testcd
  )

  result <- testcd(sdtm_obj)
  expect_equal(result, as.data.frame(expected))
})


test_that("testcd() handles domains with unicode characters in TESTCD values", {
  # Create sdtm object with unicode characters in TESTCD values
  sdtm_data <- list(
    dm = tribble(
      ~DOMAIN, ~USUBJID, ~DMTESTCD,
      "DM", "SUBJ001", "AGE_αβγ",
      "DM", "SUBJ002", "AGE_中文",
      "DM", "SUBJ003", "AGE_🎉"
    )
  )
  sdtm_obj <- sdtm(sdtm_data)

  # Expected output (unicode characters should be preserved)
  expected <- tribble(
    ~DOMAIN, ~TESTCD,
    "DM", "AGE_αβγ",
    "DM", "AGE_中文",
    "DM", "AGE_🎉"
  )

  result <- testcd(sdtm_obj)
  expect_equal(result, as.data.frame(expected))
})


test_that("testcd() handles domains with all missing TESTCD columns", {
  # Create sdtm object with domains that have no TESTCD columns
  sdtm_data <- list(
    dm = tribble(
      ~DOMAIN, ~USUBJID, ~OTHERCOL,
      "DM", "SUBJ001", "VALUE1"
    ),
    vs = tribble(
      ~DOMAIN, ~USUBJID, ~OTHERCOL,
      "VS", "SUBJ001", "VALUE2"
    )
  )
  sdtm_obj <- sdtm(sdtm_data)

  # Expected output (empty data frame)
  expected <- data.frame(DOMAIN = character(), TESTCD = character())

  result <- testcd(sdtm_obj)
  expect_equal(result, expected)
})

test_that("testcd() handles domains with factor TESTCD values", {
  # Create sdtm object with factor TESTCD values
  sdtm_data <- list(
    dm = tribble(
      ~DOMAIN, ~USUBJID, ~DMTESTCD,
      "DM", "SUBJ001", "AGE",
      "DM", "SUBJ002", "SEX"
    )
  )
  sdtm_obj <- sdtm(sdtm_data)

  # Convert TESTCD to factor
  sdtm_obj$domains$dm$DMTESTCD <- factor(sdtm_obj$domains$dm$DMTESTCD)

  # Expected output (factors should be converted to character)
  expected <- tribble(
    ~DOMAIN, ~TESTCD,
    "DM", "AGE",
    "DM", "SEX"
  )

  result <- testcd(sdtm_obj)
  expect_equal(result, as.data.frame(expected))
})


test_that("testcd() handles domains with numeric TESTCD values (actual numeric)", {
  # Create sdtm object with actual numeric TESTCD values
  sdtm_data <- list(
    dm = tribble(
      ~DOMAIN, ~USUBJID, ~DMTESTCD,
      "DM", "SUBJ001", 123,
      "DM", "SUBJ002", 456
    )
  )
  sdtm_obj <- sdtm(sdtm_data)

  # Expected output (numeric values should be converted to character)
  expected <- tribble(
    ~DOMAIN, ~TESTCD,
    "DM", "123",
    "DM", "456"
  )

  result <- testcd(sdtm_obj)
  expect_equal(result, as.data.frame(expected))
})


test_that("testcd() handles domains with complex TESTCD values", {
  # Create sdtm object with complex TESTCD values
  sdtm_data <- list(
    dm = tribble(
      ~DOMAIN, ~USUBJID, ~DMTESTCD,
      "DM", "SUBJ001", "AGE",
      "DM", "SUBJ002", "AGE_123",
      "DM", "SUBJ003", "AGE-456",
      "DM", "SUBJ004", "AGE.789",
      "DM", "SUBJ005", "AGE_αβγ",
      "DM", "SUBJ006", "AGE_中文",
      "DM", "SUBJ007", "AGE_🎉",
      "DM", "SUBJ008", "",
      "DM", "SUBJ009", NA_character_
    )
  )
  sdtm_obj <- sdtm(sdtm_data)

  # Expected output (all values should be preserved)
  expected <- tribble(
    ~DOMAIN, ~TESTCD,
    "DM", "AGE",
    "DM", "AGE_123",
    "DM", "AGE-456",
    "DM", "AGE.789",
    "DM", "AGE_αβγ",
    "DM", "AGE_中文",
    "DM", "AGE_🎉",
    "DM", "",
    "DM", NA_character_
  )

  result <- testcd(sdtm_obj)
  expect_equal(result, as.data.frame(expected))
})
