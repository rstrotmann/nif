test_that("TAFD is correctly calculated for different treatments", {
  temp <- list(
    dm = tibble::tribble(
      ~USUBJID, ~ACTARMCD, ~RFXSTDTC, ~RFSTDTC,
      "1", "A", "2025-11-27T08:00:00", "2025-11-27T08:00:00",
      "2", "B", "2025-11-27T08:00:00", "2025-11-27T08:00:00"
    ) %>%
      mutate(
        DOMAIN = "DM",
        RFENDTC = "2025-12-27T08:00:00",
        STUDYID = "Study 1",
        DOMAIN = "DM",
        SEX = "M"
      ),
    vs = tibble::tribble(
      ~USUBJID, ~VSTESTCD, ~VSSTRESN, ~VSDTC,
      "1", "HEIGHT", 100, "2025-11-26T08:00:00",
      "2", "HEIGHT", 100, "2025-11-26T08:00:00",
    ) %>%
      mutate(
        DOMAIN = "VS"
      ),
    ex = tibble::tribble(
      ~USUBJID, ~EXDOSE, ~EXTRT,    ~EXSTDTC,              ~EXENDTC,
      "1",      1,       "ACTIVE",  "2025-11-27T08:00:00", "2025-11-30T08:00:00",
      "2",      0,       "PLACEBO", "2025-11-27T08:00:00", "2025-11-30T08:00:00"
    ) %>%
      mutate(
        DOMAIN = "EX",
        EXSEQ = row_number()
      ),
    lb = tibble::tribble(
      ~USUBJID, ~LBTESTCD, ~LBSPEC, ~LBDTC,                ~LBSTRESN,
      "1",      "AST",     "SERUM", "2025-11-25T08:00:00", 1,
      "1",      "AST",     "SERUM", "2025-11-26T08:00:00", 2,
      "1",      "AST",     "SERUM", "2025-11-27T08:00:00", 3,
      "1",      "AST",     "SERUM", "2025-11-28T08:00:00", 4,
      "1",      "AST",     "SERUM", "2025-11-29T08:00:00", 5,
      "2",      "AST",     "SERUM", "2025-11-25T08:00:00", 1,
      "2",      "AST",     "SERUM", "2025-11-26T08:00:00", 2,
      "2",      "AST",     "SERUM", "2025-11-27T08:00:00", 3,
      "2",      "AST",     "SERUM", "2025-11-28T08:00:00", 4,
      "2",      "AST",     "SERUM", "2025-11-29T08:00:00", 5
    ) %>%
      mutate(DOMAIN = "LB") %>%
      mutate(LBSEQ = row_number())
  )

  sdtm <- sdtm(temp)

  nif <- nif() %>%
    add_administration(sdtm, "ACTIVE", silent = TRUE) %>%
    add_administration(sdtm, "PLACEBO", silent = TRUE) %>%
    add_observation(sdtm, "lb", "AST", silent = TRUE)

  expect_equal(unique(filter(nif, ID == 1, EVID == 0)$PARENT), "ACTIVE")
  expect_equal(unique(filter(nif, ID == 2, EVID == 0)$PARENT), "PLACEBO")
})
