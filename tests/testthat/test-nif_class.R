
test_that("ensure_parent works as intended" , {
  nif <- tribble(
    ~ID, ~TIME, ~AMT, ~EVID, ~DV, ~CMT,
    1,   0,     100,  1,     NA,  1,
    1,   1,     0,    0,     1,   2,
    1,   2,     0,    0,     2,   2,
    1,   3,     0,    0,     3,   2
  )

  expect_no_error(
    nif %>%
    new_nif() %>%
    ensure_analyte() %>%
    ensure_parent() %>%
    as.data.frame()
  )
})


test_that("print works with minimal NIF" , {
  expect_no_error(print(examplinib_poc_min_nif))
  expect_no_error(print(examplinib_sad_min_nif))
})


test_that("subjects works with minimal NIF" , {
  expect_no_error(subjects(examplinib_poc_nif))
  expect_no_error(subjects(examplinib_poc_min_nif))
  expect_equal(ncol(subjects(examplinib_poc_nif)), 2)
  expect_equal(ncol(subjects(examplinib_poc_min_nif)), 2)
})


test_that("doses works with minimal NIF", {
  expect_no_error(doses(examplinib_poc_min_nif))
})


test_that("dose_levels works with minimal NIF", {
  expect_no_error(dose_levels(examplinib_poc_min_nif))
})

test_that("analytes works with minimal NIF and rich NIF", {
  expect_no_error(analytes(examplinib_poc_min_nif))
  expect_no_error(analytes(examplinib_poc_nif))
})

test_that("index_dosing_interval works with single parent" , {
  nif <- tribble(
    ~ID, ~TIME, ~AMT, ~RATE, ~EVID, ~DV, ~CMT, ~MDV, ~PARENT,
    1,   0.00,  0,    0,     0,     0,   2,    0,    "A",
    1,   0,     100,  0,     1,     NA,  1,    0,    "A",
    1,   1.00,  0,    0,     0,     1,   2,    0,    "A",
    1,   2.00,  0,    0,     0,     2,   2,    0,    "A",
    1,   4.00,  0,    0,     0,     3,   2,    0,    "A",
    1,   24,    100,  0,     1,     NA,  1,    0,    "A",
    1,   48,    100,  0,     1,     NA,  1,    0,    "A",
    1,   72,    100,  0,     1,     NA,  1,    0,    "A",
    1,   73.00, 0,    0,     0,     1,   2,    0,    "A",
    1,   74.00, 0,    0,     0,     2,   2,    0,    "A",
    1,   76.00, 0,    0,     0,     3,   2,    0,    "A"
  ) %>%
    new_nif()

  temp <- nif %>%
    index_dosing_interval() %>%
    as.data.frame()

  expect_equal(max(temp$DI), 4)
})


test_that("index_dosing_interval works with multiple parents" , {
  nif <- tribble(
    ~ID, ~TIME,  ~AMT, ~RATE, ~EVID, ~DV, ~CMT, ~MDV, ~PARENT,
    1,   0.00,   0,    0,     0,     0,   2,    0,    "A",
    1,   0,      100,  0,     1,     NA,  1,    0,    "A",
    1,   1.00,   0,    0,     0,     1,   2,    0,    "A",
    1,   2.00,   0,    0,     0,     2,   2,    0,    "A",
    1,   4.00,   0,    0,     0,     3,   2,    0,    "A",
    1,   24,     100,  0,     1,     NA,  1,    0,    "A",
    1,   48,     100,  0,     1,     NA,  1,    0,    "A",
    1,   72,     100,  0,     1,     NA,  1,    0,    "A",
    1,   73.00,  0,    0,     0,     1,   2,    0,    "A",
    1,   74.00,  0,    0,     0,     2,   2,    0,    "A",
    1,   76.00,  0,    0,     0,     3,   2,    0,    "A",
    1,   0.00,   0,    0,     0,     0,   2,    0,    "B",
    1,   0,      100,  0,     1,     NA,  1,    0,    "B",
    1,   1.00,   0,    0,     0,     1,   2,    0,    "B",
    1,   2.00,   0,    0,     0,     2,   2,    0,    "B",
    1,   4.00,   0,    0,     0,     3,   2,    0,    "B",
    1,   48,     100,  0,     1,     NA,  1,    0,    "B",
    1,   96,     100,  0,     1,     NA,  1,    0,    "B",
    1,   97.00,  0,    0,     0,     1,   2,    0,    "B",
    1,   98.00,  0,    0,     0,     2,   2,    0,    "B",
    1,   100.00, 0,    0,     0,     3,   2,    0,    "B"
  ) %>%
    new_nif() %>%
    index_nif() %>%
    index_dosing_interval()

  temp <- nif %>%
    group_by(PARENT) %>%
    summarize(max_di = max(DI))

  expect_equal(temp$max_di, c(4, 3))

  nif %>%
    filter(PARENT == "A", DI == 4, EVID == 0) %>%
    # as.data.frame() %>%
    nrow() %>%
    expect_equal(3)

  nif %>%
    filter(PARENT == "B", DI == 3, EVID == 0) %>%
    as.data.frame() %>%
    nrow() %>%
    expect_equal(3)
})




