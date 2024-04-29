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

usethis::use_data(examplinib_sad, overwrite = T)
usethis::use_data(examplinib_poc, overwrite = T)
usethis::use_data(examplinib_fe, overwrite = T)

# make NIF package data
# examplinib_sad_nif <- examplinib_sad %>%
#   make_nif(spec = "PLASMA", silent = TRUE) %>%
#   add_bl_lab(examplinib_sad$domains[["lb"]], "CREAT", "SERUM") %>%
#   add_bl_crcl() %>%
#   compress()

examplinib_sad_nif <- new_nif() %>%
  add_administration(examplinib_sad, "EXAMPLINIB", analyte = "RS2023") %>%
  add_observation(examplinib_sad, "pc", "RS2023", cmt = 2) %>%
  add_baseline(examplinib_sad, "lb", "CREAT") %>%
  add_bl_crcl()

# examplinib_poc_nif <- examplinib_poc %>%
#   make_nif(spec = "PLASMA", silent = TRUE) %>%
#   add_bl_lab(examplinib_poc$domains[["lb"]], "CREAT", "SERUM") %>%
#   mutate(BL_CRCL = egfr_mdrd(BL_CREAT, AGE, SEX, RACE, molar = T)) %>%
#   compress()

examplinib_poc_nif <- new_nif() %>%
  add_administration(examplinib_poc, "EXAMPLINIB", analyte = "RS2023") %>%
  add_observation(examplinib_poc, "pc", "RS2023", cmt = 2) %>%
  add_observation(examplinib_poc, "pc", "RS2023487A", parent = "RS2023",
                  cmt = 3) %>%
  add_baseline(examplinib_poc, "lb", "CREAT") %>%
  add_bl_crcl()

# examplinib_fe_nif <- make_nif(examplinib_fe, spec = "PLASMA", silent = TRUE) %>%
#   mutate(PERIOD = str_sub(EPOCH, -1, -1)) %>%
#   mutate(TREATMENT = str_sub(ACTARMCD, PERIOD, PERIOD)) %>%
#   mutate(FASTED = case_when(TREATMENT == "A" ~ 1, .default = 0)) %>%
#   compress()

examplinib_fe_nif <- new_nif() %>%
  add_administration(examplinib_fe, "EXAMPLINIB", analyte = "RS2023", keep = "EPOCH") %>%
  add_observation(examplinib_fe, "pc", "RS2023", cmt = 2, keep = "EPOCH") %>%
  mutate(PERIOD = as.numeric(str_sub(EPOCH, -1, -1))) %>%
  mutate(TREATMENT = str_sub(ACTARMCD, PERIOD, PERIOD)) %>%
  mutate(FASTED = case_when(TREATMENT == "A" ~ 1, .default = 0))

usethis::use_data(examplinib_sad_nif, overwrite = T)
usethis::use_data(examplinib_poc_nif, overwrite = T)
usethis::use_data(examplinib_fe_nif, overwrite = T)
