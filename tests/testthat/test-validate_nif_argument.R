test_that("validate_nif_argument passes for a minimal valid nif", {
  test_nif <- as_nif_test(tibble::tribble(
      ~ID, ~TIME, ~AMT, ~CMT, ~EVID, ~DV,
      1,   0,     100,  1,    1,     0,
      1,   1,     0,    2,    0,     10
    ))

  expect_invisible(validate_nif_argument(test_nif))
  expect_null(validate_nif_argument(test_nif))
})


test_that("validate_nif_argument rejects non-nif input", {
  expect_error(
    validate_nif_argument(data.frame(
      ID = 1, TIME = 0, AMT = 0, CMT = 1, EVID = 0, DV = 10
    )),
    "Input must be a nif object"
  )
  expect_error(
    validate_nif_argument(list(ID = 1)),
    "Input must be a nif object"
  )
  expect_error(
    validate_nif_argument(NULL),
    "Input must be a nif object"
  )
})


test_that("validate_nif_argument rejects missing essential fields", {
  incomplete <- as_nif_test(tibble::tribble(
      ~ID,
      1
    ))

  expect_error(
    validate_nif_argument(incomplete),
    "Missing essential fields in nif object: TIME, AMT, CMT, EVID and DV"
  )
})


test_that("validate_nif_argument reports a single missing essential field", {
  missing_dv <- as_nif_test(tibble::tribble(
      ~ID, ~TIME, ~AMT, ~CMT, ~EVID,
      1,   0,     100,  1,    1
    ))

  expect_error(
    validate_nif_argument(missing_dv),
    "Missing essential fields in nif object: DV"
  )
})


test_that("validate_nif_argument accepts empty nif with required columns", {
  empty <- as_nif_test(tibble::tribble(
      ~ID, ~TIME, ~AMT, ~CMT, ~EVID, ~DV
    ))

  expect_invisible(validate_nif_argument(empty))
})


test_that("validate_nif_argument accepts extra columns beyond the minimum", {
  test_nif <- as_nif_test(tibble::tribble(
      ~ID, ~TIME, ~AMT, ~CMT, ~EVID, ~DV,  ~ANALYTE, ~WEIGHT,
      1,   0,     100,  1,    1,     0,    "DRUG",   70,
      1,   1,     0,    2,    0,     10,   "DRUG",   70
    ))

  expect_invisible(validate_nif_argument(test_nif))
})


test_that("validate_nif_argument fields=NULL is a no-op", {
  test_nif <- as_nif_test(tibble::tribble(
      ~ID, ~TIME, ~AMT, ~CMT, ~EVID, ~DV,
      1,   0,     100,  1,    1,     0
    ))

  expect_invisible(validate_nif_argument(test_nif, fields = NULL))
})


test_that("validate_nif_argument fields=character(0) is a no-op", {
  test_nif <- as_nif_test(tibble::tribble(
      ~ID, ~TIME, ~AMT, ~CMT, ~EVID, ~DV,
      1,   0,     100,  1,    1,     0
    ))

  expect_invisible(validate_nif_argument(test_nif, fields = character(0)))
})


test_that("validate_nif_argument accepts present additional fields", {
  test_nif <- as_nif_test(tibble::tribble(
      ~ID, ~TIME, ~AMT, ~CMT, ~EVID, ~DV,  ~ANALYTE, ~WEIGHT,
      1,   0,     100,  1,    1,     0,    "DRUG",   70,
      1,   1,     0,    2,    0,     10,   "DRUG",   70
    ))

  expect_invisible(validate_nif_argument(test_nif, fields = "ANALYTE"))
  expect_invisible(validate_nif_argument(test_nif, fields = c("ANALYTE", "WEIGHT")))
})


test_that("validate_nif_argument errors when additional fields are missing", {
  test_nif <- as_nif_test(tibble::tribble(
      ~ID, ~TIME, ~AMT, ~CMT, ~EVID, ~DV,  ~ANALYTE,
      1,   0,     100,  1,    1,     0,    "DRUG"
    ))

  expect_error(
    validate_nif_argument(test_nif, fields = "WEIGHT"),
    "Missing required fields: WEIGHT"
  )
  expect_error(
    validate_nif_argument(test_nif, fields = c("WEIGHT", "AGE")),
    "Missing required fields: WEIGHT and AGE"
  )
})


test_that("validate_nif_argument additional fields can overlap minimal fields", {
  test_nif <- as_nif_test(tibble::tribble(
      ~ID, ~TIME, ~AMT, ~CMT, ~EVID, ~DV,
      1,   0,     100,  1,    1,     0
    ))

  expect_invisible(validate_nif_argument(test_nif, fields = "TIME"))
  expect_invisible(validate_nif_argument(test_nif, fields = c("ID", "DV")))
})


test_that("validate_nif_argument reports only missing additional fields when minimal are present", {
  test_nif <- as_nif_test(tibble::tribble(
      ~ID, ~TIME, ~AMT, ~CMT, ~EVID, ~DV,  ~ANALYTE,
      1,   0,     100,  1,    1,     0,    "DRUG"
    ))

  err <- tryCatch(
    validate_nif_argument(test_nif, fields = c("ANALYTE", "WEIGHT")),
    error = function(e) e$message
  )
  expect_match(err, "Missing required fields: WEIGHT")
  expect_false(grepl("ANALYTE", err))
  expect_false(grepl("essential", err))
})


test_that("validate_nif_argument validates fields argument type", {
  test_nif <- as_nif_test(tibble::tribble(
      ~ID, ~TIME, ~AMT, ~CMT, ~EVID, ~DV,
      1,   0,     100,  1,    1,     0
    ))

  expect_error(
    validate_nif_argument(test_nif, fields = 1),
    "fields must be a character value"
  )
  expect_error(
    validate_nif_argument(test_nif, fields = TRUE),
    "fields must be a character value"
  )
  expect_error(
    validate_nif_argument(test_nif, fields = NA_character_),
    "fields must not contain NA"
  )
  expect_error(
    validate_nif_argument(test_nif, fields = ""),
    "fields must be a non-empty string"
  )
})


test_that("validate_nif_argument checks class before missing fields", {
  # Plain data.frame missing columns should still fail on class, not fields
  expect_error(
    validate_nif_argument(data.frame(ID = 1)),
    "Input must be a nif object"
  )
})


test_that("validate_nif_argument works with tibble class underneath", {
  test_nif <- tibble::tribble(
    ~ID, ~TIME, ~AMT, ~CMT, ~EVID, ~DV,
    1,   0,     100,  1,    1,     0
  )
  test_nif <- as_nif_test(test_nif)
  expect_invisible(validate_nif_argument(test_nif))
  expect_invisible(validate_nif_argument(test_nif, fields = "TIME"))
})

