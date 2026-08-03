unique_per_subject_nif <- function(...) {
  tibble::tribble(...) |>
    nif(silent = TRUE)
}


test_that("ensure_unique_per_subject passes for a unique field", {
  test_nif <- unique_per_subject_nif(
    ~ID, ~TIME, ~DV, ~EVID, ~AMT, ~CMT, ~WEIGHT,
    1,   0,     0,   1,     100,  1,    70,
    1,   1,     10,  0,     0,    2,    70,
    2,   0,     0,   1,     100,  1,    75,
    2,   1,     12,  0,     0,    2,    75
  )

  expect_invisible(ensure_unique_per_subject(test_nif, "WEIGHT"))
  expect_null(ensure_unique_per_subject(test_nif, "WEIGHT"))
})


test_that("ensure_unique_per_subject passes for multiple unique fields", {
  test_nif <- unique_per_subject_nif(
    ~ID, ~TIME, ~DV, ~EVID, ~AMT, ~CMT, ~WEIGHT, ~AGE,
    1,   0,     0,   1,     100,  1,    70,      30,
    1,   1,     10,  0,     0,    2,    70,      30,
    2,   0,     0,   1,     100,  1,    75,      35,
    2,   1,     12,  0,     0,    2,    75,      35
  )

  expect_invisible(
    ensure_unique_per_subject(test_nif, c("WEIGHT", "AGE"))
  )
})


test_that("ensure_unique_per_subject errors for a non-unique field", {
  test_nif <- unique_per_subject_nif(
    ~ID, ~TIME, ~DV, ~EVID, ~AMT, ~CMT, ~WEIGHT,
    1,   0,     0,   1,     100,  1,    70,
    1,   1,     10,  0,     0,    2,    70,
    2,   0,     0,   1,     100,  1,    75,
    2,   1,     12,  0,     0,    2,    80
  )

  expect_error(
    ensure_unique_per_subject(test_nif, "WEIGHT"),
    "Non-unique values for WEIGHT"
  )
})


test_that("ensure_unique_per_subject only reports fields that vary", {
  # WEIGHT is unique for all subjects; AGE varies for ID 3 only
  test_nif <- unique_per_subject_nif(
    ~ID, ~TIME, ~DV, ~EVID, ~AMT, ~CMT, ~WEIGHT, ~AGE,
    1,   0,     0,   1,     100,  1,    70,      30,
    1,   1,     10,  0,     0,    2,    70,      30,
    3,   0,     0,   1,     100,  1,    60,      40,
    3,   1,     8,   0,     0,    2,    60,      41
  )

  err <- tryCatch(
    ensure_unique_per_subject(test_nif, c("WEIGHT", "AGE")),
    error = function(e) e$message
  )
  expect_match(err, "Non-unique values for AGE")
  expect_false(grepl("WEIGHT", err))
  expect_invisible(ensure_unique_per_subject(test_nif, "WEIGHT"))
})


test_that("ensure_unique_per_subject reports multiple non-unique fields", {
  test_nif <- unique_per_subject_nif(
    ~ID, ~TIME, ~DV, ~EVID, ~AMT, ~CMT, ~WEIGHT, ~AGE,
    2,   0,     0,   1,     100,  1,    75,      35,
    2,   1,     12,  0,     0,    2,    80,      36
  )

  expect_error(
    ensure_unique_per_subject(test_nif, c("WEIGHT", "AGE")),
    "Non-unique values for WEIGHT or AGE"
  )
})


test_that("ensure_unique_per_subject treats mixed NA and value as non-unique", {
  test_nif <- unique_per_subject_nif(
    ~ID, ~TIME, ~DV, ~EVID, ~AMT, ~CMT, ~WEIGHT,
    1,   0,     0,   1,     100,  1,    70,
    1,   1,     10,  0,     0,    2,    NA_real_
  )

  expect_error(
    ensure_unique_per_subject(test_nif, "WEIGHT"),
    "Non-unique values for WEIGHT"
  )
})


test_that("ensure_unique_per_subject accepts all-NA values for a subject", {
  test_nif <- unique_per_subject_nif(
    ~ID, ~TIME, ~DV, ~EVID, ~AMT, ~CMT, ~WEIGHT,
    1,   0,     0,   1,     100,  1,    NA_real_,
    1,   1,     10,  0,     0,    2,    NA_real_,
    2,   0,     0,   1,     100,  1,    75,
    2,   1,     12,  0,     0,    2,    75
  )

  expect_invisible(ensure_unique_per_subject(test_nif, "WEIGHT"))
})


test_that("ensure_unique_per_subject works with character covariates", {
  test_nif <- unique_per_subject_nif(
    ~ID, ~TIME, ~DV, ~EVID, ~AMT, ~CMT, ~SEX,
    1,   0,     0,   1,     100,  1,    "M",
    1,   1,     10,  0,     0,    2,    "M",
    2,   0,     0,   1,     100,  1,    "F",
    2,   1,     12,  0,     0,    2,    "F"
  )

  expect_invisible(ensure_unique_per_subject(test_nif, "SEX"))

  bad_nif <- unique_per_subject_nif(
    ~ID, ~TIME, ~DV, ~EVID, ~AMT, ~CMT, ~SEX,
    1,   0,     0,   1,     100,  1,    "M",
    1,   1,     10,  0,     0,    2,    "F"
  )

  expect_error(
    ensure_unique_per_subject(bad_nif, "SEX"),
    "Non-unique values for SEX"
  )
})


test_that("ensure_unique_per_subject ignores ID when mixed with other fields", {
  test_nif <- unique_per_subject_nif(
    ~ID, ~TIME, ~DV, ~EVID, ~AMT, ~CMT, ~WEIGHT,
    1,   0,     0,   1,     100,  1,    70,
    1,   1,     10,  0,     0,    2,    70
  )

  expect_invisible(
    ensure_unique_per_subject(test_nif, c("ID", "WEIGHT"))
  )
})


test_that("ensure_unique_per_subject no-ops when field is only ID", {
  test_nif <- unique_per_subject_nif(
    ~ID, ~TIME, ~DV, ~EVID, ~AMT, ~CMT, ~WEIGHT,
    1,   0,     0,   1,     100,  1,    70,
    1,   1,     10,  0,     0,    2,    70
  )

  expect_no_error(
    ensure_unique_per_subject(test_nif, "ID")
  )
})


test_that("ensure_unique_per_subject no-ops for empty field", {
  test_nif <- unique_per_subject_nif(
    ~ID, ~TIME, ~DV, ~EVID, ~AMT, ~CMT, ~WEIGHT,
    1,   0,     0,   1,     100,  1,    70,
    1,   1,     10,  0,     0,    2,    70
  )

  expect_no_error(
    ensure_unique_per_subject(test_nif, character(0))
  )
})


test_that("ensure_unique_per_subject validates field argument", {
  test_nif <- unique_per_subject_nif(
    ~ID, ~TIME, ~DV, ~EVID, ~AMT, ~CMT, ~WEIGHT,
    1,   0,     0,   1,     100,  1,    70,
    1,   1,     10,  0,     0,    2,    70
  )

  expect_no_error(
    ensure_unique_per_subject(test_nif, NULL)
  )

  expect_error(
    ensure_unique_per_subject(test_nif, "BMI"),
    "field must be"
  )

  expect_error(
    ensure_unique_per_subject(test_nif, 1),
    "field must be a character value"
  )
})


test_that("ensure_unique_per_subject validates nif input", {
  expect_error(
    ensure_unique_per_subject(data.frame(ID = 1, WEIGHT = 70), "WEIGHT"),
    "Input must be a nif object"
  )

  incomplete <- structure(
    tibble::tribble(
      ~ID, ~WEIGHT,
      1,   70
    ),
    class = c("nif", "data.frame")
  )
  expect_error(
    ensure_unique_per_subject(incomplete, "WEIGHT"),
    "Missing essential fields in nif object"
  )
})


test_that("ensure_unique_per_subject passes with one row per subject", {
  test_nif <- unique_per_subject_nif(
    ~ID, ~TIME, ~DV, ~EVID, ~AMT, ~CMT, ~WEIGHT, ~AGE,
    1,   0,     10,  0,     0,    2,    70,      30,
    2,   0,     12,  0,     0,    2,    75,      35
  )

  expect_invisible(
    ensure_unique_per_subject(test_nif, c("WEIGHT", "AGE"))
  )
})


test_that("ensure_unique_per_subject error includes subject ID and param", {
  test_nif <- unique_per_subject_nif(
    ~ID, ~TIME, ~DV, ~EVID, ~AMT, ~CMT, ~WEIGHT,
    2,   0,     0,   1,     100,  1,    75,
    2,   1,     12,  0,     0,    2,    80
  )

  err <- tryCatch(
    ensure_unique_per_subject(test_nif, "WEIGHT"),
    error = function(e) e$message
  )
  expect_match(err, "WEIGHT")
  expect_match(err, "2")
  expect_match(err, "param")
})

