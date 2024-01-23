#' Create minmimal NIF file from examplinib_poc_nic for demonstration and
#' testing purposes
#'
#' The NIF file is derived from examplinib_poc_nif
examplinib_poc_min_nif <- examplinib_poc_nif %>%
  select(ID, TIME, AMT, RATE, EVID, DV, CMT, MDV)

usethis::use_data(examplinib_poc_min_nif, overwrite = T)


#' Create minmimal NIF file from examplinib_sad_nif for demonstration and
#' testing purposes
#'
#' The NIF file is derived from examplinib_sad_nif
examplinib_sad_min_nif <- examplinib_sad_nif %>%
  select(ID, TIME, AMT, RATE, EVID, DV, CMT, MDV)

usethis::use_data(examplinib_sad_min_nif, overwrite = T)
