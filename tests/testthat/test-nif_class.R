
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


# test_that("print works with minimal NIF" , {
#   expect_no_error(print(examplinib_poc_min_nif))
#   expect_no_error(print(examplinib_sad_min_nif))
# })


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


test_that("add baseline hepatic function class", {
  dm <- tribble(
    ~USUBJID, ~DOMAIN, ~SEX,   ~ACTARMCD,          ~RFXSTDTC,
    "1", "DM", "M", "TREATMENT", "2001-01-01T10:29",
    "2", "DM", "M", "TREATMENT", "2001-01-02T09:09",
    "3", "DM", "M", "TREATMENT", "2000-12-29T09:07",
    "4", "DM", "F", "TREATMENT", "2001-01-06T11:18",
    "5", "DM", "F", "TREATMENT", "2001-01-07T11:18"
  ) %>%
    mutate(RFSTDTC = RFXSTDTC)

  vs <- tribble(
    ~USUBJID, ~DOMAIN, ~VSTESTCD, ~VSBLFL, ~VSSTRESN,
    "1", "VS", "HEIGHT",     "Y",     190.8,
    "1", "VS", "WEIGHT",     "Y",      79.3,
    "2", "VS", "HEIGHT",     "Y",     199.5,
    "2", "VS", "WEIGHT",     "Y",      81.6,
    "3", "VS", "HEIGHT",     "Y",     185.4,
    "3", "VS", "WEIGHT",     "Y",      92.8,
    "4", "VS", "HEIGHT",     "Y",     177.8,
    "4", "VS", "WEIGHT",     "Y",      83.3,
    "5", "VS", "HEIGHT",     "Y",     177.9,
    "5", "VS", "WEIGHT",     "Y",      83.4
  )
  lb <- tribble(
    ~USUBJID, ~DOMAIN, ~LBSPEC, ~LBDTC,             ~LBBLFL, ~LBTESTCD, ~LBSTRESN, ~LBSTNRHI,
    "1",      "LB",    "SERUM", "2001-01-01T10:29", "Y",     "BILI",    1,          1, # normal
    "1",      "LB",    "SERUM", "2001-01-01T10:29", "Y",     "AST",     1,          1,
    "2",      "LB",    "SERUM", "2001-01-02T09:09", "Y",     "BILI",    1.5,        1, # mild
    "2",      "LB",    "SERUM", "2001-01-02T09:09", "Y",     "AST",     1,          1,
    "3",      "LB",    "SERUM", "2000-12-29T09:07", "Y",     "BILI",    1,          1, # mild
    "3",      "LB",    "SERUM", "2000-12-29T09:07", "Y",     "AST",     1.2,        1,
    "4",      "LB",    "SERUM", "2001-01-06T11:18", "Y",     "BILI",    3,          1, # moderate
    "4",      "LB",    "SERUM", "2001-01-06T11:18", "Y",     "AST",     1,          1,
    "5",      "LB",    "SERUM", "2001-01-07T11:18", "Y",     "BILI",    3.1,        1, # severe
    "5",      "LB",    "SERUM", "2001-01-07T11:18", "Y",     "AST",     1,          1
  )
  sdtm <- list(domains = list(dm = dm, vs = vs, lb = lb))
  suppressMessages(
    expect_no_error(
      temp <- data.frame(USUBJID = as.character(c(1, 2, 3, 4, 5))) %>%
      add_bl_odwg(sdtm)
    )
  )

  expect_equal(as.character(temp$BL_ODWG), c("normal", "mild", "mild", "moderate", "severe"))
})



test_that("cfb works", {
  obj <- tribble(
    ~ID, ~ANALYTE, ~EVID, ~TIME, ~DV,
    1,   "A",      0,     0,     NA,
    1,   "A",      0,     1,     1,
    1,   "A",      0,     2,     2,
    1,   "A",      0,     3,     3,
    1,   "A",      0,     4,     4,
    1,   "A",      0,     5,     5
  )

  expect_no_error(
    test <- obj %>%
      add_cfb(baseline_filter = "TIME < 4", summary_function = last) %>%
      as.data.frame() %>%
      pull(DVBL)
  )

  expect_equal(unique(test), 3)
})












