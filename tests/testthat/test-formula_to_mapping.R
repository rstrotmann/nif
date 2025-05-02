# Create test sdtm object
test_sdtm_formula_to_mapping <- function() {
  new_sdtm(list(
    pc = tribble(
      ~USUBJID, ~PCTESTCD, ~PCSTRESN,
        "001", "GLUC", 5.5,
        "001", "LACT", 2.1,
        "002", "GLUC", 6.2,
        "002", "LACT", 1.8
    ),
    ex = tribble(
      ~USUBJID, ~EXTRT,
        "001", "GLUC",
        "001", "LACT",
        "002", "GLUC",
        "002", "LACT"
    ))
  )
}

test_that("formula_to_mapping works with single analyte", {
  f <- GLUC ~ GLUC

  expected <- tribble(
    ~PCTESTCD, ~EXTRT, ~ANALYTE,
       "GLUC", "GLUC", "GLUC"
  )

  result <- formula_to_mapping(test_sdtm_formula_to_mapping(), f)
  expect_equal(as_tibble(result), expected)
})


test_that("formula_to_mapping works with multiple analytes", {
  f <- GLUC + LACT ~ GLUC

  expected <- tribble(
    ~PCTESTCD, ~EXTRT, ~ANALYTE,
      "GLUC", "GLUC", "GLUC",
      "LACT", "GLUC", "LACT"
  )

  result <- formula_to_mapping(test_sdtm_formula_to_mapping(), f)
  expect_equal(as_tibble(result), expected)
})


test_that("formula_to_mapping handles non-matching treatments", {
  f <- GLUC ~ NONEXISTENT

  expect_error(
    result <- formula_to_mapping(test_sdtm_formula_to_mapping(), f),
    "extrt NONEXISTENT not found in EX!"
  )
})


test_that("formula_to_mapping handles non-matching analytes", {
  f <- NONEXISTENT ~ GLUC

  expected <- tribble(
    ~PCTESTCD, ~EXTRT, ~ANALYTE
  )

  expect_error(
    result <- formula_to_mapping(test_sdtm_formula_to_mapping(), f),
    "pctestcd NONEXISTENT not found in PC!"
  )
})


test_that("formula_to_mapping throws error for non-sdtm input", {
  f <- GLUC ~ GLUC
  expect_error(
    formula_to_mapping("not_sdtm", f),
    "sdtm must be an sdtm object"
  )
})


test_that("formula_to_mapping throws error for non-formula input", {
  expect_error(
    formula_to_mapping(test_sdtm_formula_to_mapping(), "not_formula"),
    "argument not_formula is not a formula!"
  )
})

