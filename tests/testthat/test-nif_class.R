test_that("new_nif works", {
  suppressMessages(expect_equal(dim(new_nif()), c(0,7)))
  suppressMessages(expect_no_error(new_nif(examplinib_sad)))
})


test_that("subject_info works", {
  expect_no_error(invisible(capture.output(subject_info(examplinib_sad_nif, 1))))
  expect_no_error(invisible(capture.output(
    print(subject_info(examplinib_sad_nif, 1)))))
})


test_that("subjects, usubjid works", {
  expect_true(nrow(subjects(examplinib_sad_nif)) > 0)
  expect_true(length(usubjid(examplinib_sad_nif)) > 0)
})


test_that("subjects works with minimal NIF" , {
  expect_no_error(subjects(examplinib_poc_nif))
  expect_no_error(subjects(examplinib_poc_min_nif))
  expect_equal(ncol(subjects(examplinib_poc_nif)), 2)
  expect_equal(ncol(subjects(examplinib_poc_min_nif)), 2)
})


test_that("parents works", {
  expect_true(length(parents(examplinib_sad_nif)) > 0)
})


test_that("dose_red_sbs works", {
  expect_true(length(dose_red_sbs(examplinib_poc_nif)) > 0)
})


test_that("rich_sampling_sbs works", {
  expect_equal(rich_sampling_sbs(examplinib_fe_nif),
               subjects(examplinib_fe_nif)$ID)
})


test_that("studies works", {
  expect_equal(length(studies(examplinib_fe_nif)), 1)
})


test_that("ensure works", {
  expect_contains(names(ensure_analyte(examplinib_sad_min_nif)), "ANALYTE")
  expect_contains(names(ensure_dose(examplinib_sad_min_nif)), "DOSE")
  expect_contains(names(ensure_parent(examplinib_sad_min_nif)), "PARENT")
  expect_contains(names(ensure_metabolite(examplinib_sad_min_nif)), "METABOLITE")
  expect_contains(names(ensure_tad(examplinib_sad_min_nif)), "TAD")
  expect_contains(names(ensure_tafd(examplinib_sad_min_nif)), "TAFD")
  expect_contains(names(ensure_time(examplinib_sad_min_nif)), "TIME")
  expect_contains(names(ensure_cfb(examplinib_sad_min_nif)), "DVCFB")
})


test_that("doses works", {
  expect_gt(length(doses(examplinib_sad_nif)), 0)
})


test_that("doses works with minimal NIF", {
  expect_no_error(doses(examplinib_poc_min_nif))
})

test_that("dose_levels works", {
  expect_gt(length(dose_levels(examplinib_sad_nif)), 0)
})


test_that("treatments works", {
  expect_gt(length(treatments(examplinib_sad_nif)), 0)
  expect_gt(length(treatments(examplinib_poc_min_nif)), 0)
})


test_that("dose_levels works with minimal NIF", {
  expect_no_error(dose_levels(examplinib_poc_min_nif))
})


test_that("analytes works", {
  expect_gt(length(analytes(examplinib_sad_nif)), 0)
})


test_that("analytes works with minimal NIF and rich NIF", {
  expect_no_error(analytes(examplinib_poc_min_nif))
  expect_no_error(analytes(examplinib_poc_nif))
})


test_that("analyte_overview works", {
  expect_gt(nrow(analyte_overview(examplinib_poc_nif)), 0)
  expect_gt(nrow(analyte_overview(examplinib_poc_min_nif)), 0)
})


test_that("cmt_mapping works", {
  expect_gt(nrow(cmt_mapping(examplinib_poc_nif)), 0)
  expect_gt(nrow(cmt_mapping(examplinib_poc_min_nif)), 0)
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
    nrow() %>%
    expect_equal(3)

  nif %>%
    filter(PARENT == "B", DI == 3, EVID == 0) %>%
    as.data.frame() %>%
    nrow() %>%
    expect_equal(3)
})


test_that("n_administrations, max_admin_time works, max_observation_time", {
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

  expect_equal(n_administrations(nif)$N, c(4, 3))
  expect_equal(max_admin_time(nif), 96)
  expect_equal(max_observation_time(nif), 100)
  expect_equal(max_time(nif), 100)
})


test_that("guess analyte, guess_parent works", {
  expect_equal(guess_analyte(examplinib_poc_nif), "RS2023")
  expect_equal(guess_parent(examplinib_poc_nif), "RS2023")
})


test_that("add_dose_level works", {
  nif <- tibble::tribble(
    ~ID, ~TIME, ~AMT, ~CMT, ~EVID,
    1,     0,  100,    1,     1,
    1,    24,   90,    1,     1,
    1,    48,   80,    1,     1,
    2,     0,  100,    1,     1,
    3,     0,  100,    1,     1,
    3,    24,   NA,    1,     1,
    3,    48,  150,    1,     1
  )

  temp <- add_dose_level(nif) %>%
    distinct(ID, DL)
  expect_equal(nrow(temp), 3)
  expect_equal(unique(temp$DL), 100)
})


test_that("add_tad, add_tafd works", {
  nif <- tibble::tribble(
    ~ID, ~TIME, ~AMT, ~CMT, ~EVID,
    1,     0,  100,    1,     1,
    1,     0,   NA,    1,     0,
    1,     1,   NA,    2,     0,
    1,     2,   NA,    2,     0,
    1,    24,   90,    1,     1,
    1,    48,   80,    1,     1,
    1,    50,   NA,    2,     0,
    2,    10,  100,    1,     1,
    2,    11,   NA,    2,     0,
    2,    12,   NA,    2,     0,
    2,    34,   90,    1,     1,
    2,    58,   80,    1,     1,
    2,    60,   NA,    2,     0
  )
  expect_equal(
    add_tad(nif)$TAD,
    c(0, 0, 1, 2, 0, 0, 2, 0, 1, 2, 0, 0, 2))
  expect_equal(
    add_tafd(nif)$TAFD,
    c(0, 0, 1, 2, 24, 48, 50, 0, 1, 2, 24, 48, 50))
})


test_that("add_trtdy works", {
  nif <- tibble::tribble(
    ~ID, ~TIME, ~AMT, ~CMT, ~EVID,                        ~DTC,
    1,     0,  100,    1,     1, "2024-12-06 07:31:35.14839",
    1,     0,   NA,    1,     0, "2024-12-06 07:31:35.14839",
    1,     1,   NA,    2,     0, "2024-12-06 08:31:35.14839",
    1,     2,   NA,    2,     0, "2024-12-06 09:31:35.14839",
    1,    24,   90,    1,     1, "2024-12-07 07:31:35.14839",
    1,    48,   80,    1,     1, "2024-12-08 07:31:35.14839",
    1,    50,   NA,    2,     0, "2024-12-08 09:31:35.14839",
    2,    10,  100,    1,     1, "2024-12-06 17:31:35.14839",
    2,    11,   NA,    2,     0, "2024-12-06 18:31:35.14839",
    2,    12,   NA,    2,     0, "2024-12-06 19:31:35.14839",
    2,    34,   90,    1,     1, "2024-12-07 17:31:35.14839",
    2,    58,   80,    1,     1, "2024-12-08 17:31:35.14839",
    2,    60,   NA,    2,     0, "2024-12-08 19:31:35.14839"
  ) %>%
    mutate(DTC = as.POSIXct(DTC))
  expect_equal(add_trtdy(nif)$TRTDY,
               c(1, 1, 1, 1, 2, 3, 3, 1, 1, 1, 2, 3, 3))
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


test_that("index_rich_sampling_intervals works", {
  nif <- tibble::tribble(
    ~ID, ~TIME, ~EVID, ~CMT,
    1,     0,     1,    1,
    1,     0,     0,    2,
    1,     2,     0,    2,
    1,     4,     0,    2,
    1,     6,     0,    2,
    1,     8,     0,    2,
    1,    24,     1,    1,
    1,    48,     1,    1,
    1,    72,     1,    1,
    1,    72,     0,    2,
    1,    72,     1,    1,
    1,    96,     1,    1,
    1,    96,     0,    2,
    1,    97,     0,    2,
    1,    98,     0,    2,
    1,   100,     0,    2,
    1,   102,     0,    2,
    1,   104,     0,    2
  )

  temp <- as.data.frame(index_rich_sampling_intervals(nif))
  expect_equal(unique(temp$RICH_N), c(NA, 1, 2))
  temp1 <- distinct(temp, OPDI, RICH_N)
  expect_equal(temp1[which(temp1$OPDI == 5), "RICH_N"], 1)
  expect_equal(temp1[which(temp1$OPDI == 6), "RICH_N"], 2)
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


test_that("analyte_overview", {
  expect_no_error(
    temp <- analyte_overview(examplinib_poc_nif)
  )
  expect_equal(dim(temp), c(2, 2))
})


test_that("write_nif works", {
  expect_no_error(invisible(capture.output(write_nif(examplinib_sad_nif))))
  expect_no_error(invisible(capture.output(write_monolix(examplinib_sad_nif))))
})


test_that("print.nif works", {
  expect_no_error(invisible(capture.output(print(examplinib_sad_nif))))
})


test_that("add_rtb works", {
  nif <- tibble::tribble(
    ~ID, ~TIME, ~EVID, ~CMT, ~DV,
    1,     0,     1,    1, NA,
    1,     0,     0,    2, 100,
    1,     2,     0,    2, 200,
    1,     4,     0,    2, 300,
    1,    24,     1,    1, NA,
    1,    48,     1,    1, NA,
    1,    72,     1,    1, NA,
    1,    72,     0,    2, 400,
    2,     0,     1,    1, NA,
    2,     0,     0,    2, 10,
    2,     2,     0,    2, 20,
    2,     4,     0,    2, 30,
    2,    72,     0,    2, 40,
  )
  temp <- as.data.frame(add_rtb(nif))
  expect_equal(temp$DVRTB,
               c(NA, 1, 2, 3, NA, NA, NA, 4, NA, 1, 2, 3, 4))
})




