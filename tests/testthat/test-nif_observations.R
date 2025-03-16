make_test_sdtm1 <- function() {
  temp = list(
    dm = tibble::tribble(
      ~USUBJID, ~DOMAIN, ~SEX, ~ACTARMCD,             ~RFXSTDTC,              ~RFSTDTC, ~ACTARM,  ~STUDYID,
            1L,    "DM",  "M",       "A", "2024-01-01T08:00:00", "2024-01-01T08:00:00", "Arm A", "Study 1",
            2L,    "DM",  "M",       "A", "2024-01-01T08:00:00", "2024-01-01T08:00:00", "Arm A", "Study 1",
            3L,    "DM",  "M",       "A", "2024-01-01T08:00:00", "2024-01-01T08:00:00", "Arm A", "Study 1",
            4L,    "DM",  "M",       "A", "2024-01-01T08:00:00", "2024-01-01T08:00:00", "Arm A", "Study 1"
    ),
    vs = tibble::tribble(
      ~USUBJID, ~DOMAIN, ~VSTESTCD, ~VSSTRESN,                ~VSDTC,
            1L,    "VS",  "HEIGHT",       100, "2024-01-01T08:00:00",
            2L,    "VS",  "HEIGHT",       100, "2024-01-01T08:00:00",
            3L,    "VS",  "HEIGHT",       100, "2024-01-01T08:00:00",
            4L,    "VS",  "HEIGHT",       100, "2024-01-01T08:00:00"
    ),
    lb = tibble::tribble(
      ~USUBJID, ~DOMAIN, ~LBSPEC, ~LBTESTCD, ~LBSTRESN,
            1L,    "LB", "SERUM",   "CREAT",       100,
            2L,    "LB", "SERUM",   "CREAT",       100,
            3L,    "LB", "SERUM",   "CREAT",       100,
            4L,    "LB", "SERUM",   "CREAT",       100
    ),
    ex = tibble::tribble(
      ~USUBJID, ~DOMAIN,                ~EXDTC, ~EXDOSE, ~EXTRT,
            1L,    "EX", "2024-01-01T08:00:00",       1,    "A",
            2L,    "EX", "2024-01-01T08:00:00",       1,    "A",
            3L,    "EX", "2024-01-01T08:00:00",       1,    "A",
            4L,    "EX", "2024-01-01T08:00:00",       1,    "A"
    ),
    pc = tibble::tribble(
      ~USUBJID, ~DOMAIN, ~PCTESTCD,                ~PCDTC, ~PCSTRESN, ~PCSPEC,     ~PCTEST,
            1L,    "PC",       "A", "2024-01-01T08:00:00",       100,  "Spec", "Analyte A",
            2L,    "PC",       "A", "2024-01-01T08:00:00",       100,  "Spec", "Analyte A",
            3L,    "PC",       "A", "2024-01-01T08:00:00",       100,  "Spec", "Analyte A",
            4L,    "PC",       "A", "2024-01-01T08:00:00",       100,  "Spec", "Analyte A"
    ))

  return(new_sdtm(temp))
}


test_that("make_observation works", {
  sdtm <- make_test_sdtm1()
  expect_no_error(
    make_observation(sdtm, "pc", "A", silent = TRUE))
})


test_that("make_observation issues warning if observation filter returns no observations", {
  sdtm <- make_test_sdtm1()
  # Should issue warning when silent is FALSE or NULL
  expect_warning(
    make_observation(sdtm, "pc", "A", observation_filter = "FALSE", silent = FALSE),
    "The observation_filter 'FALSE' returned no entries."
  )
  
  expect_warning(
    make_observation(sdtm, "pc", "A", observation_filter = "FALSE", silent = NULL),
    "The observation_filter 'FALSE' returned no entries."
  )
  
  # Should NOT issue warning when silent is TRUE
  expect_no_warning(
    make_observation(sdtm, "pc", "A", observation_filter = "FALSE", silent = TRUE)
  )
})


test_that("make_observation works with coding table", {
  sdtm <- examplinib_poc
  suppressMessages(expect_no_error(
    make_observation(
      sdtm, "pp", "LAMZNPT", DTC_field = "PPRFTDTC",
      observation_filter = "PPSEQ == 7",
      coding_table = tibble::tribble(
        ~PPSTRESN, ~DV,
                3, 33,
                4, 44,
                5, 55,
                6, 66
      )
    )
  )
  )
})
