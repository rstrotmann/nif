sdtm <- examplinib_poc

ex <- domain(sdtm, "ex")

replace1 <- tibble::tribble(
  ~DOMAIN,     ~STUDYID,            ~USUBJID,           ~EXSTDTC,           ~EXENDTC, ~DOSE,       ~EXTRT, ~EXDOSE,      ~EPOCH, ~EXROUTE, ~EXDOSFRM, ~EXSEQ,
    # "EX", "2023000022", "20230000221010001", "2001-01-13T10:36", "2001-04-06T10:36",   500, "EXAMPLINIB",     500, "TREATMENT",   "ORAL",  "TABLET",     1L,

    "EX", "2023000022", "20230000221010001", "2001-01-13T10:36", "2001-01-15",         500, "EXAMPLINIB",     500, "TREATMENT",   "ORAL",  "TABLET",     NA, # missing time in EXENDTC
    "EX", "2023000022", "20230000221010001", "2001-01-20T08:00", "2001-01-20",         500, "EXAMPLINIB",     500, "TREATMENT",   "ORAL",  "TABLET",     NA, # missing time in EXENDTC
    "EX", "2023000022", "20230000221010001", "2001-01-22T08:00", "2001-01-21",         500, "EXAMPLINIB",     500, "TREATMENT",   "ORAL",  "TABLET",     NA, # EXENDTC before EXSTDTC
    "EX", "2023000022", "20230000221010001", "2001-01-22T09:00",  NA,                  500, "EXAMPLINIB",     500, "TREATMENT",   "ORAL",  "TABLET",     NA, # missing EXENDTC
    "EX", "2023000022", "20230000221010001", "2001-01-28T10:36", "2001-03-01",         500, "EXAMPLINIB",     500, "TREATMENT",   "ORAL",  "TABLET",     NA,
    "EX", "2023000022", "20230000221010001", "2001-03-02T08:31",  NA,                  500, "EXAMPLINIB",     500, "TREATMENT",   "ORAL",  "TABLET",     NA, # missing last EXENDTC

    "EX", "2023000022", "20230000221010001", "2001-04-07T10:36", "2001-04-13T10:36",   250, "EXAMPLINIB",     250, "TREATMENT",   "ORAL",  "TABLET",     NA
)

temp <- bind_rows(
    ex[3:nrow(ex),],
    replace1
  ) %>%
  arrange(USUBJID, EXSTDTC) %>%
  group_by(.data$USUBJID) %>%
  mutate(EXSEQ = row_number()) %>%
  ungroup()

temp[15, "EXENDTC"] <- NA

sdtm$domains[["ex"]] <- temp

examplinib_poc_messy <- sdtm
usethis::use_data(examplinib_poc_messy, overwrite = T)
