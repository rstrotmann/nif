# Create test sdtm object
test_sdtm_formula_to_mapping <- function() {
  new_sdtm(list(
    pc = tibble::tribble(
      ~USUBJID, ~PCTESTCD, ~PCSTRESN, ~DOMAIN,
      "001",    "ANALYTE1",       1,    "PC",
      "001",    "ANALYTE2",       2,    "PC",
      "002",    "ANALYTE1",       3,    "PC",
      "002",    "ANALYTE2",       4,    "PC"
    ),
    lb = tibble::tribble(
      ~USUBJID, ~LBTESTCD, ~LBSTRESN, ~DOMAIN,
      "001",    "GLUC",       5.5,    "LB",
      "001",    "LACT",       2.1,    "LB",
      "002",    "GLUC",       6.2,    "LB",
      "002",    "LACT",       1.8,    "LB"
    ) ,
    ex = tibble::tribble(
      ~USUBJID, ~EXTRT, ~DOMAIN,
      "001", "DRUG1",    "EX",
      "001", "DRUG2",    "EX",
      "002", "DRUG1",    "EX",
      "002", "DRUG2",    "EX"
    ))
  )
}

test_that("formula_to_mapping works with single analyte", {
  f <- ANALYTE1 ~ DRUG1

  expected <- tibble::tribble(
    ~DOMAIN,    ~TESTCD,     ~PARAM,  ~EXTRT,   ~ANALYTE,
       "PC", "ANALYTE1", "PCTESTCD", "DRUG1", "ANALYTE1"
  )

  result <- formula_to_mapping(test_sdtm_formula_to_mapping(), f)
  expect_equal(as_tibble(result), expected)
})


test_that("formula_to_mapping works with multiple analytes", {
  f <- ANALYTE1 + LACT ~ DRUG1

  expected <- tibble::tribble(
    ~DOMAIN,    ~TESTCD,     ~PARAM,  ~EXTRT,   ~ANALYTE,
      "PC", "ANALYTE1", "PCTESTCD", "DRUG1", "ANALYTE1",
      "LB",     "LACT", "LBTESTCD", "DRUG1",     "LACT"
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
  f <- NONEXISTENT ~ DRUG1

  expect_message(
    result <- formula_to_mapping(test_sdtm_formula_to_mapping(), f),
    "analyte NONEXISTENT not found in any domain!"
  )
  expect_equal(nrow(result), 0)
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

