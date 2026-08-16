## Tests for guess_parent
##
## Contract: among EVID == 1 rows, return the most frequent PARENT
## (ties: alphabetical). If there are no administrations, return NULL.


test_that("guess_parent returns the parent with the most administrations", {
  obj <- tibble::tribble(
    ~ID, ~TIME, ~EVID, ~AMT, ~DV, ~CMT, ~MDV, ~ANALYTE, ~PARENT, ~METABOLITE,
      1,     0,     1,  100,  NA,    1,    1,  "DRUG1",  "DRUG1",      FALSE,
      1,    24,     1,  100,  NA,    1,    1,  "DRUG1",  "DRUG1",      FALSE,
      2,     0,     1,  100,  NA,    1,    1,  "DRUG2",  "DRUG2",      FALSE
  ) |>
    nif()

  expect_equal(guess_parent(obj), "DRUG1")
})


test_that("guess_parent counts by PARENT, not ANALYTE", {
  obj <- tibble::tribble(
    ~ID, ~TIME, ~EVID, ~AMT, ~DV, ~CMT, ~MDV, ~ANALYTE, ~PARENT, ~METABOLITE,
      1,     0,     1,  100,  NA,    1,    1,      "A",      "A",      FALSE,
      1,    24,     1,  100,  NA,    1,    1,      "A",      "A",      FALSE,
      1,     0,     1,   50,  NA,    2,    1,      "B",      "B",      FALSE,
      1,     1,     0,    0, 0.5,    3,    0,      "M",      "A",       TRUE
  ) |>
    nif()

  expect_equal(guess_parent(obj), "A")
})


test_that("guess_parent ignores observation counts when ranking parents", {
  obj <- tibble::tribble(
    ~ID, ~TIME, ~EVID, ~AMT, ~DV, ~CMT, ~MDV, ~ANALYTE, ~PARENT, ~METABOLITE,
      1,     0,     1,  100,  NA,    1,    1,  "DRUG1",  "DRUG1",      FALSE,
      2,     0,     1,  100,  NA,    1,    1,  "DRUG2",  "DRUG2",      FALSE,
      2,    24,     1,  100,  NA,    1,    1,  "DRUG2",  "DRUG2",      FALSE,
      1,     1,     0,    0,  10,    2,    0,  "DRUG1",  "DRUG1",      FALSE,
      1,     2,     0,    0,   8,    2,    0,  "DRUG1",  "DRUG1",      FALSE,
      1,     3,     0,    0,   6,    2,    0,  "DRUG1",  "DRUG1",      FALSE
  ) |>
    nif()

  expect_equal(guess_parent(obj), "DRUG2")
})


test_that("guess_parent breaks administration ties alphabetically", {
  obj <- tibble::tribble(
    ~ID, ~TIME, ~EVID, ~AMT, ~DV, ~CMT, ~MDV, ~ANALYTE, ~PARENT, ~METABOLITE,
      1,     0,     1,  100,  NA,    1,    1,  "DRUG1",  "DRUG1",      FALSE,
      2,     0,     1,  100,  NA,    1,    1,  "DRUG2",  "DRUG2",      FALSE
  ) |>
    nif()

  expect_equal(guess_parent(obj), "DRUG1")
})


test_that("guess_parent breaks three-way administration ties alphabetically", {
  obj <- tibble::tribble(
    ~ID, ~TIME, ~EVID, ~AMT, ~DV, ~CMT, ~MDV, ~ANALYTE, ~PARENT, ~METABOLITE,
      1,     0,     1,  100,  NA,    1,    1,     "C",     "C",      FALSE,
      2,     0,     1,  100,  NA,    1,    1,     "A",     "A",      FALSE,
      3,     0,     1,  100,  NA,    1,    1,     "B",     "B",      FALSE
  ) |>
    nif()

  expect_equal(guess_parent(obj), "A")
})


test_that("guess_parent returns a length-1 character for a single parent", {
  obj <- tibble::tribble(
    ~ID, ~TIME, ~EVID, ~AMT, ~DV, ~CMT, ~MDV, ~ANALYTE, ~PARENT, ~METABOLITE,
      1,     0,     1,  100,  NA,    1,    1,  "DRUG1",  "DRUG1",      FALSE,
      1,     1,     0,    0,  10,    2,    0,  "DRUG1",  "DRUG1",      FALSE
  ) |>
    nif()

  result <- guess_parent(obj)

  expect_type(result, "character")
  expect_length(result, 1L)
  expect_equal(result, "DRUG1")
})


test_that("guess_parent returns NULL for an empty nif", {
  empty_nif <- tibble::tribble(
    ~ID, ~TIME, ~EVID, ~AMT, ~DV, ~CMT, ~MDV, ~ANALYTE, ~PARENT, ~METABOLITE
  ) |>
    nif()

  expect_null(guess_parent(empty_nif))
  expect_null(guess_parent(nif()))
})


test_that("guess_parent returns NULL when there are only observations", {
  obj <- tibble::tribble(
    ~ID, ~TIME, ~EVID, ~AMT, ~DV, ~CMT, ~MDV, ~ANALYTE, ~PARENT, ~METABOLITE,
      1,     1,     0,    0,  10,    2,    0,  "DRUG1",  "DRUG1",      FALSE,
      1,     2,     0,    0,   8,    2,    0,  "DRUG1",  "DRUG1",      FALSE,
      1,     3,     0,    0,   6,    2,    0,  "DRUG1",  "DRUG1",      FALSE,
      2,     1,     0,    0,  15,    2,    0,  "DRUG2",  "DRUG2",      FALSE,
      2,     2,     0,    0,  12,    2,    0,  "DRUG2",  "DRUG2",      FALSE
  ) |>
    nif()

  expect_null(guess_parent(obj))
})


test_that("guess_parent returns NULL for metabolite-only observations", {
  obj <- tibble::tribble(
    ~ID, ~TIME, ~EVID, ~AMT, ~DV, ~CMT, ~MDV, ~ANALYTE, ~PARENT, ~METABOLITE,
      1,     1,     0,    0,   2,    3,    0,  "META1",  "DRUG1",       TRUE,
      1,     2,     0,    0,   3,    3,    0,  "META1",  "DRUG1",       TRUE
  ) |>
    nif()

  expect_null(guess_parent(obj))
})


test_that("guess_parent derives PARENT from ANALYTE when PARENT is missing", {
  obj <- tibble::tribble(
    ~ID, ~TIME, ~EVID, ~AMT, ~DV, ~CMT, ~MDV, ~ANALYTE,
      1,     0,     1,  100,  NA,    1,    1,  "DRUG1",
      1,    24,     1,  100,  NA,    1,    1,  "DRUG1",
      2,     0,     1,  100,  NA,    1,    1,  "DRUG2"
  ) |>
    nif()

  expect_equal(guess_parent(obj), "DRUG1")
})


test_that("guess_parent derives parent from CMT when ANALYTE and PARENT are missing", {
  obj <- tibble::tribble(
    ~ID, ~TIME, ~EVID, ~AMT, ~DV, ~CMT, ~MDV,
      1,     0,     1,  100,  NA,    1,    1,
      2,     0,     1,  100,  NA,    1,    1
  ) |>
    nif()

  expect_equal(guess_parent(obj), "CMT1")
})


test_that("guess_parent uses existing PARENT when ANALYTE is missing", {
  obj <- tibble::tribble(
    ~ID, ~TIME, ~EVID, ~AMT, ~DV, ~CMT, ~MDV, ~PARENT,
      1,     0,     1,  100,  NA,    1,    1,     "A",
      1,     1,     0,    0,   1,    2,    0,     "A",
      1,    24,     1,  100,  NA,    1,    1,     "A"
  ) |>
    nif()

  expect_equal(guess_parent(obj), "A")
})


test_that("guess_parent still ranks by PARENT when some ANALYTE values are NA", {
  obj <- tibble::tribble(
    ~ID, ~TIME, ~EVID, ~AMT, ~DV, ~CMT, ~MDV, ~ANALYTE, ~PARENT, ~METABOLITE,
      1,     0,     1,  100,  NA,    1,    1,       NA,  "DRUG1",      FALSE,
      1,    24,     1,  100,  NA,    1,    1,  "DRUG1",  "DRUG1",      FALSE,
      2,     0,     1,  100,  NA,    1,    1,  "DRUG2",  "DRUG2",      FALSE
  ) |>
    nif()

  expect_equal(guess_parent(obj), "DRUG1")
})


test_that("guess_parent picks the more frequently administered parent in a DDI-like schedule", {
  obj <- tibble::tribble(
    ~ID, ~TIME, ~EVID, ~AMT, ~DV, ~CMT, ~MDV,   ~ANALYTE,     ~PARENT, ~METABOLITE,
      1,     0,     1,  100,  NA,    1,    1,     "test",       "test",      FALSE,
      1,     1,     0,    0,   1,    2,    0,     "test",       "test",      FALSE,
      1,   264,     1,  100,  NA,    1,    1,     "test",       "test",      FALSE,
      1,   168,     1,  200,  NA,    3,    1, "itraconazole", "itraconazole",      FALSE,
      1,   192,     1,  200,  NA,    3,    1, "itraconazole", "itraconazole",      FALSE,
      1,   216,     1,  200,  NA,    3,    1, "itraconazole", "itraconazole",      FALSE
  ) |>
    nif()

  expect_equal(guess_parent(obj), "itraconazole")
})


test_that("guess_parent works with examplinib example data", {
  expect_equal(guess_parent(examplinib_poc_nif), "RS2023")
  expect_equal(guess_parent(examplinib_fe_nif), "RS2023")
})


test_that("guess_parent validates that input is a nif object", {
  expect_error(
    guess_parent(data.frame(ID = 1, TIME = 0, EVID = 1)),
    "nif object"
  )
})
