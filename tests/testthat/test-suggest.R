make_suggest_sdtm <- function() {
  domains = list(
    dm = tibble::tribble(
      ~USUBJID, ~DOMAIN,    ~STUDYID, ~ACTARMCD,           ~ACTARM,
    "SUBJ-001",    "DM", "STUDY-001",    "ARM1", "Treatment Arm 1",
    "SUBJ-002",    "DM", "STUDY-001",    "ARM2", "Treatment Arm 2"
    ),
    ex = tibble::tribble(
      ~USUBJID, ~DOMAIN,   ~EXTRT, ~EXDOSE,
    "SUBJ-001",    "EX", "Drug A",     100,
    "SUBJ-002",    "EX", "Drug A",     200
    ),
    pc = tibble::tribble(
      ~USUBJID, ~DOMAIN,  ~PCSPEC,  ~PCTEST, ~PCTESTCD, ~PCTPT, ~PCTPTNUM,   ~PCCAT,
    "SUBJ-001",    "PC", "PLASMA", "Drug A",   "DRUGA",  "PRE",         0, "PLASMA",
    "SUBJ-001",    "PC", "PLASMA", "Drug A",   "DRUGA",  "1HR",         1,  "SERUM",
    "SUBJ-002",    "PC", "PLASMA", "Drug A",   "DRUGA",  "2HR",         2, "PLASMA",
    "SUBJ-001",    "PC",  "URINE", "Drug A",   "DRUGA",  "PRE",         2,  "URINE",
    )
  )
  sdtm(domains)
}

test_that("Suggest works with a basic example", {
  sdtm <- make_suggest_sdtm()
  result <- capture_messages(
    suggest(sdtm)
  )

  expect_match(result, r"(There are 1 treatments \(EXTRT\) in EX: Drug A)", all = FALSE)
  expect_match(result, r"(There is one pharmacokinetic analyte:)", all = FALSE)
  expect_match(result, r"(The PC domain contains multiple fields that the nominal sampling time)", all = FALSE)
  expect_match(result, r"(There are 2 study arms defined in DM)", all = FALSE)
  expect_match(result, r"(Note that there are data from 2 different PK sample specimen types)", all = FALSE)
  expect_match(result, r"(Note that there are different analytical categories)", all = FALSE)
})
