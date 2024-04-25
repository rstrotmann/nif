


compare_ex_ec <- function(sdtm) {
  if(!all(c("ex", "ec") %in% names(sdtm$domains)))
    stop("EX and EC not both found in the sdtm object!")
  ec <- domain(sdtm, "ec") %>%
    assertr::verify(has_all_names("USUBJID", "ECTRT", "ECSTDTC", "ECENDTC")) %>%
    filter(ECMOOD == "PERFORMED", !is.na(ECDOSE)) %>%
    select(USUBJID, TRT = ECTRT, STDTC = ECSTDTC, ENDY = ECENDTC, DOSE = ECDOSE)
  ex <- domain(sdtm, "ex") %>%
    assertr::verify(has_all_names("USUBJID", "EXTRT", "EXSTDTC", "EXENDTC")) %>%
    select(USUBJID, TRT = EXTRT, STDTC = EXSTDTC, ENDY = EXENDTC, DOSE = EXDOSE)

}


# sdtm <- sdtm.001
#
# sdtm <- sdtm.101
#
# d1 <- ex %>% filter(USUBJID == "20192400011011003")
#
# d2 <- ec %>% filter(USUBJID == "20192400011011003")
#
# all.equal(d1, d2)
#
# all.equal(ex, ec)




#' EC with ECCAT == "DISCRETE"
#' compare with PCRFDTC, should be the same!
