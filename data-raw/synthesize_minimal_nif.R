#' Create minmimal NIF file for demonstration and testing purposes
#'
#' The NIF file is derived from examplinib_poc_nif
examplinib_poc_min_nif <- examplinib_poc_nif %>%
  select(ID, TIME, AMT, RATE, EVID, DV, CMT, MDV) %>%
  as.data.frame()

usethis::use_data(examplinib_poc_min_nif, overwrite = T)
