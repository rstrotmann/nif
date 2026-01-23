cdiscpilot01_sdtm <- sdtm(
  list(
    dm = pharmaversesdtm::dm,
    ex = pharmaversesdtm::ex,
    vs = pharmaversesdtm::vs,
    pc = pharmaversesdtm::pc,
    pp = pharmaversesdtm::pp,
    lb = pharmaversesdtm::lb,
    ts = pharmaversesdtm::ts,
    eg = pharmaversesdtm::eg,
    ae  = pharmaversesdtm::ae
  )
)

cdiscpilot01_nif <- nif() |>
  add_administration(sdtm, 'XANOMELINE', analyte = "XAN") |>
  add_administration(sdtm, 'PLACEBO') |>
  add_observation(sdtm, 'pc', 'XAN', observation_filter = "PCSPEC == 'PLASMA'") |>
  add_observation(sdtm, 'lb', 'ALT', ntime_method = "DY") |>
  add_observation(sdtm, 'lb', 'HGB', ntime_method = "DY") |>
  add_baseline(sdtm, "lb", "CREAT") |>
  add_bl_renal(molar = TRUE) |>
  derive_cfb_analyte("HGB") |>
  add_dose_level()


usethis::use_data(cdiscpilot01_sdtm, overwrite = T)
usethis::use_data(cdiscpilot01_nif, overwrite = T)
