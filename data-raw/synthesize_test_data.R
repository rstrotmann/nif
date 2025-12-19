#' This script can be executed to re-create the package data for the sample study
#' data. The data include SDTM and NIF objects for the following fictional
#' studies with the imaginary drug `examplinib`:
#'
#' * A single ascending dose (SAD) study
#' * A single-arm multiple-dose proof-of-concept study
#' * A cross-over food effect (FE) study
#'
#' All SDTM data are entirely synthetic and have no relation whatsoever with real
#' clinical study data. This also applies to the pharmacokinetic data in the
#' SDTM/PC domain. These data are created using a population PK model that is
#' parametrized with arbitrary parameters.

set.seed(1234)

# synthesize SDTM package data
examplinib_sad <- synthesize_sdtm_sad_study()
examplinib_poc <- synthesize_sdtm_poc_study()
examplinib_fe <- synthesize_sdtm_food_effect_study()

examplinib_sad_nif <- new_nif() %>%
  add_administration(examplinib_sad, "EXAMPLINIB", analyte = "RS2023") %>%
  add_observation(examplinib_sad, "pc", "RS2023", cmt = 2) %>%
  add_baseline(examplinib_sad, "lb", "CREAT") %>%
  add_bl_crcl()

examplinib_sad$domains[["pp"]] <- examplinib_sad_nif %>%
  index_rich_sampling_intervals() %>%
  # as.data.frame() %>%
  filter(!is.na("RICH_N")) %>%
  synthesize_pp()

examplinib_poc_nif <- new_nif() %>%
  add_administration(examplinib_poc, "EXAMPLINIB", analyte = "RS2023") %>%
  add_observation(examplinib_poc, "pc", "RS2023", cmt = 2) %>%
  add_observation(examplinib_poc, "pc", "RS2023487A",
    parent = "RS2023",
    cmt = 3
  ) %>%
  add_baseline(examplinib_poc, "lb", "CREAT") %>%
  add_bl_crcl()

examplinib_poc$domains[["pp"]] <- examplinib_poc_nif %>%
  index_rich_sampling_intervals() %>%
  # as.data.frame() %>%
  filter(!is.na("RICH_N")) %>%
  synthesize_pp()

examplinib_fe_nif <- new_nif() %>%
  add_administration(examplinib_fe, "EXAMPLINIB", analyte = "RS2023", keep = "EPOCH") %>%
  add_observation(examplinib_fe, "pc", "RS2023", cmt = 2, keep = "EPOCH") %>%
  mutate(PERIOD = as.numeric(str_sub(EPOCH, -1, -1))) %>%
  mutate(TREATMENT = str_sub(ACTARMCD, PERIOD, PERIOD)) %>%
  mutate(FASTED = case_when(TREATMENT == "A" ~ 1, .default = 0))

examplinib_fe$domains[["pp"]] <- examplinib_fe_nif %>%
  index_rich_sampling_intervals() %>%
  # as.data.frame() %>%
  filter(!is.na("RICH_N")) %>%
  synthesize_pp()

usethis::use_data(examplinib_sad, overwrite = T)
usethis::use_data(examplinib_poc, overwrite = T)
usethis::use_data(examplinib_fe, overwrite = T)

usethis::use_data(examplinib_sad_nif, overwrite = T)
usethis::use_data(examplinib_poc_nif, overwrite = T)
usethis::use_data(examplinib_fe_nif, overwrite = T)
