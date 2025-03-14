make_test_sdtm1 <- function() {
  temp = list(
    dm = data.frame(
      USUBJID = seq(1, 4),
      DOMAIN = "DM",
      SEX = "M",
      ACTARMCD = "A",
      RFXSTDTC = "2024-01-01T08:00:00",
      RFSTDTC = "2024-01-01T08:00:00"
    ),
    vs = data.frame(
      USUBJID = seq(1, 4),
      DOMAIN = "VS",
      VSTESTCD = "HEIGHT",
      VSSTRESN = 100
    ),
    lb = data.frame(
      USUBJID = seq(1, 4),
      DOMAIN = "LB",
      LBSPEC = "SERUM",
      LBTESTCD = "CREAT",
      LBSTRESN = 100
    ),
    pc = data.frame(
      USUBJID = seq(1, 4),
      DOMAIN = "PC",
      PCTESTCD = "A",
      PCDTC = "2024-01-01T08:00:00",
      PCSTRESN = 100
    )
  )
  out <- list(domains = temp)
  class(out) <- c("sdtm", "list")
  return(out)
}
