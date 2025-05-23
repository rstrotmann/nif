#' @keywords internal
"_PACKAGE"

## usethis namespace: start
#' @importFrom lifecycle deprecated
## usethis namespace: end
NULL

utils::globalVariables(c(
  "ANALYTE", "CMT", "EVID", "TIME", "NTIME",
  "DV", "AMT", "ID", "DOSE", "AGE",
  "SEX", "RACE", "ACTARMCD", "HEIGHT",
  "WEIGHT", "BMI", "PCDTC",

  "dtc", "PCRFTDTC", "PCTESTCD",
  "EXTRT", "admin.time", "REF",
  "EXENDTC", "EXSEQ", "EXSTDTC",
  "EXDOSE", "EXSTDY", "EXENDY", "EPOCH",
  "SUBJID",
  "runif", "ACTARM", "VSTESTCD", "VSSTRESN", "ut", "RFICDTC", "BRTHDTC",

  "brthyr", "refdtc",

  "ETHNIC", "COUNTRY", "ARM", "PCSPEC", "PCSTAT", "PCELTM",
  "PCSTRESN", "PERIOD", "RFSTDTC",
  "VSORRES", "VSORRESU",

  # "c_centr", "c_metab", "EXDY", "rnorm",
  # "centr", "v_centr", "centr.err", "peri", "v_peri",
  # "metab", "v_metab", "t.ke", "eta.ke", "t.ka", "eta.ka", "FOOD", "t.ka1",
  # "t.d1", "eta.d1", "t.fm", "eta.fm", "t.cl", "eta.cl", "t.kem", "eta.kem",
  # "eta.fpar", "t.fpar1", "t.q", "eta.q", "/<-", "dt",

  "dur<-", "renal",
  "metab_excr", "sd", "GROUP", "first_admin_time", "regimen", "PCTEST",
  "no.analyte", "bl_wt", "rc", "maxwt", "DOMAIN", "no_admin", "PARENT",
  "PCTESTCD_metab", "PCTESTCD_parent", "METABOLITE", "GROUPING", "PPORRES",
  "PPTESTCD", "next_start", ":=", "DI", "N", "DAY", "VAL", "PCTPTNUM",
  "RICHINT_TEMP", "LEAD", "RICHINT", "FLAG", "RICH_N", "initial_dose",
  "RATE", "LNDV", "use_data", "first_dtc", "rich", "ref_time", "delta",
  "time1", "delta_time", "cohort", "dose", "DL",

  # "block", "block_id",
  # "dose_red_start", "dose_restart", "i", "prev_dose", "ref", "trtdur",
  # "end_rich", "n_obs",

  "FIRSTADMINDTC", "FIRSTTRTDTC", "DVBL", "DVCFB",
  "OPDI", "BL_CRCL", "age_lo", "age_hi", "Mean", "EGFR", "CREA", "LBORRES",
  "BL_CREAT", "LAST_ADMIN", "RFENDTC", "TRTDY", "BL_RENAL", "EXENDTC_has_time",
  "EXENDTC1", "VISIT", "VSBLFL", "EXENDTC_new", "impute_exendtc_time",
  "CLASS", "center", "dispersion", "value", "admin_time", "TAD", "COLOR",
  "ANALYTE1", "LBBLFL", "PCREFID", "RS2023", "RS2023487A", "VSDTC", "admin_REF",
  "evid", "last_obs", "lb", ".", "cmt_name", "LBSTAT", "VSSTAT", "MDV",
  "rich_start", "admin_id",

  # "auc",
  # "auc_metab",
  # "eta_cl_met", "eta_cl_par", "eta_f", "eta_f_f", "eta_ka",
  # "eta_ke", "eta_q", "obs_id", "t_cl_met", "t_cl_par", "t_d1", "t_d1_f", "t_f",
  # "t_f_f", "t_ka", "t_ka_f", "t_ke", "t_q", "t_v_centr",
  "domain_model"
))
