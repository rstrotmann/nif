test_that("validate_nif passes for a minimal valid nif", {
  test_nif <- structure(
    tibble::tribble(
      ~ID, ~TIME, ~AMT, ~CMT, ~EVID, ~DV,
      1,   0,     100,  1,    1,     0,
      1,   1,     0,    2,    0,     10
    ),
    class = c("nif", "data.frame")
  )

  expect_invisible(validate_nif(test_nif))
  expect_null(validate_nif(test_nif))
})


test_that("validate_nif rejects non-nif input", {
  expect_error(
    validate_nif(data.frame(
      ID = 1, TIME = 0, AMT = 0, CMT = 1, EVID = 0, DV = 10
    )),
    "Input must be a nif object"
  )
  expect_error(
    validate_nif(list(ID = 1)),
    "Input must be a nif object"
  )
  expect_error(
    validate_nif(NULL),
    "Input must be a nif object"
  )
})


test_that("validate_nif rejects missing essential fields", {
  incomplete <- structure(
    tibble::tribble(
      ~ID,
      1
    ),
    class = c("nif", "data.frame")
  )

  expect_error(
    validate_nif(incomplete),
    "Missing essential fields in nif object: TIME, AMT, CMT, EVID and DV"
  )
})


test_that("validate_nif reports a single missing essential field", {
  missing_dv <- structure(
    tibble::tribble(
      ~ID, ~TIME, ~AMT, ~CMT, ~EVID,
      1,   0,     100,  1,    1
    ),
    class = c("nif", "data.frame")
  )

  expect_error(
    validate_nif(missing_dv),
    "Missing essential fields in nif object: DV"
  )
})


test_that("validate_nif accepts empty nif with required columns", {
  empty <- structure(
    tibble::tribble(
      ~ID, ~TIME, ~AMT, ~CMT, ~EVID, ~DV
    ),
    class = c("nif", "data.frame")
  )

  expect_invisible(validate_nif(empty))
})


test_that("validate_nif accepts extra columns beyond the minimum", {
  test_nif <- structure(
    tibble::tribble(
      ~ID, ~TIME, ~AMT, ~CMT, ~EVID, ~DV,  ~ANALYTE, ~WEIGHT,
      1,   0,     100,  1,    1,     0,    "DRUG",   70,
      1,   1,     0,    2,    0,     10,   "DRUG",   70
    ),
    class = c("nif", "data.frame")
  )

  expect_invisible(validate_nif(test_nif))
})


test_that("validate_nif fields=NULL is a no-op", {
  test_nif <- structure(
    tibble::tribble(
      ~ID, ~TIME, ~AMT, ~CMT, ~EVID, ~DV,
      1,   0,     100,  1,    1,     0
    ),
    class = c("nif", "data.frame")
  )

  expect_invisible(validate_nif(test_nif, fields = NULL))
})


test_that("validate_nif fields=character(0) is a no-op", {
  test_nif <- structure(
    tibble::tribble(
      ~ID, ~TIME, ~AMT, ~CMT, ~EVID, ~DV,
      1,   0,     100,  1,    1,     0
    ),
    class = c("nif", "data.frame")
  )

  expect_invisible(validate_nif(test_nif, fields = character(0)))
})


test_that("validate_nif accepts present additional fields", {
  test_nif <- structure(
    tibble::tribble(
      ~ID, ~TIME, ~AMT, ~CMT, ~EVID, ~DV,  ~ANALYTE, ~WEIGHT,
      1,   0,     100,  1,    1,     0,    "DRUG",   70,
      1,   1,     0,    2,    0,     10,   "DRUG",   70
    ),
    class = c("nif", "data.frame")
  )

  expect_invisible(validate_nif(test_nif, fields = "ANALYTE"))
  expect_invisible(validate_nif(test_nif, fields = c("ANALYTE", "WEIGHT")))
})


test_that("validate_nif errors when additional fields are missing", {
  test_nif <- structure(
    tibble::tribble(
      ~ID, ~TIME, ~AMT, ~CMT, ~EVID, ~DV,  ~ANALYTE,
      1,   0,     100,  1,    1,     0,    "DRUG"
    ),
    class = c("nif", "data.frame")
  )

  expect_error(
    validate_nif(test_nif, fields = "WEIGHT"),
    "Missing required fields: WEIGHT"
  )
  expect_error(
    validate_nif(test_nif, fields = c("WEIGHT", "AGE")),
    "Missing required fields: WEIGHT and AGE"
  )
})


test_that("validate_nif additional fields can overlap minimal fields", {
  test_nif <- structure(
    tibble::tribble(
      ~ID, ~TIME, ~AMT, ~CMT, ~EVID, ~DV,
      1,   0,     100,  1,    1,     0
    ),
    class = c("nif", "data.frame")
  )

  expect_invisible(validate_nif(test_nif, fields = "TIME"))
  expect_invisible(validate_nif(test_nif, fields = c("ID", "DV")))
})


test_that("validate_nif reports only missing additional fields when minimal are present", {
  test_nif <- structure(
    tibble::tribble(
      ~ID, ~TIME, ~AMT, ~CMT, ~EVID, ~DV,  ~ANALYTE,
      1,   0,     100,  1,    1,     0,    "DRUG"
    ),
    class = c("nif", "data.frame")
  )

  err <- tryCatch(
    validate_nif(test_nif, fields = c("ANALYTE", "WEIGHT")),
    error = function(e) e$message
  )
  expect_match(err, "Missing required fields: WEIGHT")
  expect_false(grepl("ANALYTE", err))
  expect_false(grepl("essential", err))
})


test_that("validate_nif validates fields argument type", {
  test_nif <- structure(
    tibble::tribble(
      ~ID, ~TIME, ~AMT, ~CMT, ~EVID, ~DV,
      1,   0,     100,  1,    1,     0
    ),
    class = c("nif", "data.frame")
  )

  expect_error(
    validate_nif(test_nif, fields = 1),
    "fields must be a character value"
  )
  expect_error(
    validate_nif(test_nif, fields = TRUE),
    "fields must be a character value"
  )
  expect_error(
    validate_nif(test_nif, fields = NA_character_),
    "fields must not contain NA"
  )
  expect_error(
    validate_nif(test_nif, fields = ""),
    "fields must be a non-empty string"
  )
})


test_that("validate_nif checks class before missing fields", {
  # Plain data.frame missing columns should still fail on class, not fields
  expect_error(
    validate_nif(data.frame(ID = 1)),
    "Input must be a nif object"
  )
})


test_that("validate_nif works with tibble class underneath", {
  test_nif <- tibble::tribble(
    ~ID, ~TIME, ~AMT, ~CMT, ~EVID, ~DV,
    1,   0,     100,  1,    1,     0
  )
  class(test_nif) <- c("nif", class(test_nif))

  expect_invisible(validate_nif(test_nif))
  expect_invisible(validate_nif(test_nif, fields = "TIME"))
})

