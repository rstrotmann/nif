# Helper function to create mock SDTM data
create_edish_sdtm <- function() {
  out <- list(
    lb = tibble::tribble(
      ~USUBJID, ~LBTESTCD, ~LBSTRESN, ~LBSTNRHI, ~LBSPEC,       ~LBDTC, ~DOMAIN,
      "SUBJ001",     "ALT",        30,        40, "BLOOD", "2024-01-01",    "LB",
      "SUBJ001",     "ALT",        60,        40, "BLOOD", "2024-01-02",    "LB",
      "SUBJ001",    "BILI",       1.2,         1, "BLOOD", "2024-01-01",    "LB",
      "SUBJ001",    "BILI",       2.4,         1, "BLOOD", "2024-01-02",    "LB"
    ),
    dm = tibble::tribble(
      ~USUBJID, ~SEX, ~ACTARMCD,     ~RFSTDTC, ~DOMAIN,
      "SUBJ001",    1,    "TEST", "2024-01-01",    "DM"
    ),
    vs = tibble::tribble(
      ~USUBJID,       ~VSDTC, ~VSTESTCD, ~VSSTRESN, ~VSBLFL, ~DOMAIN,
      "SUBJ-001", "2023-01-01",  "WEIGHT",        70,     "Y",    "VS"
    )
  )

  # class(out) <- c("sdtm", "list")
  new_sdtm(out)
}


# Helper function to create mock NIF data
create_edish_nif <- function() {
  out <- tibble::tribble(
    ~ID, ~TIME, ~NTIME, ~DV, ~ANALYTE, ~EVID, ~CMT, ~PARENT, ~DTC,
      1,     0,      0,   0,      "A",     1,    1,     "A", "2024-01-01",
      1,     0,      0,   1,      "A",     0,    2,     "A", "2024-01-01",
      1,    24,     24,   2,      "A",     0,    2,     "A", "2024-01-02"
  ) %>% mutate(USUBJID = "SUBJ001") %>% lubrify_dates()
  # class(out) <- c("nif", "data.frame")
  new_nif(out)
}


test_that("edish_plot handles valid input correctly", {
  result <- edish_plot(create_edish_nif(), create_edish_sdtm(),
                       ntime_method = "ELTM")
  expect_s3_class(result, "ggplot")
  expect_equal(result$labels$x, "ALT/ULN")
  expect_equal(result$labels$y, "BILI/ULN")
})


test_that("edish_plot validates enzyme parameter", {
  expect_error(
    edish_plot(create_edish_nif(), create_edish_sdtm(), enzyme = "INVALID"),
    "enzyme must be either 'ALT' or 'AST'"
  )
})



test_that("edish_plot handles missing required lab tests", {
  sdtm <- create_edish_sdtm()
  invalid_lb <- domain(sdtm, "lb") %>%
    filter(LBTESTCD != "BILI")
  sdtm$domains$lb <- invalid_lb

  expect_error(
    edish_plot(create_edish_nif(), sdtm),
    "missing lab tests for BILI"
  )
})


test_that("edish_plot handles zero ULN values", {
  sdtm <- create_edish_sdtm()
  invalid_lb <- domain(sdtm, "lb")
  invalid_lb$LBSTNRHI[1] <- 0
  sdtm$domains$lb <- invalid_lb

  invisible(capture.output(
    expect_error(
    edish_plot(create_edish_nif(), sdtm),
    "Data validation failed"
  )
  ))
})

