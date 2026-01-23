# Package index

## All functions

- [`GeomAdmin`](GeomAdmin.md) : GeomAdmin ggproto object

- [`adam()`](adam.md) : adam object class constructor

- [`add_administration()`](add_administration.md) : Add administration
  events

- [`add_ae_observation()`](add_ae_observation.md) **\[experimental\]** :
  Add AE observation

- [`add_baseline()`](add_baseline.md) : Add a baseline covariate

- [`add_bintime()`](add_bintime.md) **\[experimental\]** : Make BINTIME
  field

- [`add_bl_crcl()`](add_bl_crcl.md) : Add baseline creatinine clearance
  field.

- [`add_bl_creat()`](add_bl_creat.md) : Add baseline creatinine

- [`add_bl_lbm()`](add_bl_lbm.md) : Add baseline lean body mass (LBM)

- [`add_bl_odwg()`](add_bl_odwg.md) : Add baseline hepatic function
  class

- [`add_bl_renal()`](add_bl_renal.md) : Add baseline renal function
  class

- [`add_cfb()`](add_cfb.md) **\[deprecated\]** : Add baseline and change
  from baseline fields

- [`add_covariate()`](add_covariate.md) : Add time-varying covariate

- [`add_dd()`](add_dd.md) : Add Field definition to Data Definition
  Table

- [`add_dose_level()`](add_dose_level.md) :

  Add dose level (`DL`) column

- [`add_ntile()`](add_ntile.md) : Add quantiles for a subject-level
  covariate

- [`add_obs_per_dosing_interval()`](add_obs_per_dosing_interval.md) :
  Add the number of observations per dosing interval

- [`add_observation()`](add_observation.md) : Add observation events to
  nif

- [`add_time_deviation()`](add_time_deviation.md) **\[experimental\]** :
  Calculate time deviations for observations

- [`add_time_window_flag()`](add_time_window_flag.md)
  **\[experimental\]** : Flag time window violations

- [`add_trtdy()`](add_trtdy.md) : Add treatment day ('TRTDY') column

- [`administration_summary()`](administration_summary.md) :
  Administration summary

- [`ae_summary()`](ae_summary.md) : AE summary

- [`analyte_overview()`](analyte_overview.md) : Analyte overview

- [`analytes()`](analytes.md) : Analytes within a NIF or SDTM object

- [`bintime_plot()`](bintime_plot.md) **\[experimental\]** : BINTIME
  plot

- [`cdiscpilot01_nif`](cdiscpilot01_nif.md) : Pharmaverse sample nif
  object

- [`cdiscpilot01_sdtm`](cdiscpilot01_sdtm.md) : Pharmaverse sample SDTM
  data

- [`cmt_mapping()`](cmt_mapping.md) **\[deprecated\]** :
  Analyte-to-compartment mapping

- [`compartments()`](compartments.md) : Compartments used in a nif
  object

- [`correlate_obs()`](correlate_obs.md) **\[experimental\]** : Correlate
  observations

- [`covariate_barplot()`](covariate_barplot.md) : Generic covariate
  barplot

- [`covariate_hist()`](covariate_hist.md) : Generic covariate
  distribution histogram

- [`crea_mdrd()`](crea_mdrd.md) : Serum creatinine estimation from eGFR
  (MDRD)

- [`crea_raynaud()`](crea_raynaud.md) : Serum creatinine estimation from
  eGFR (Raynaud method)

- [`dataset()`](dataset.md) : Retrieve dataset from adam object

- [`ddt()`](ddt.md) : Data definition table for nif object

- [`derive_baseline()`](derive_baseline.md) : Extract the individual
  baseline value for an analyte

- [`derive_cfb()`](derive_cfb.md) : Calculate change from baseline

- [`derive_cfb_analyte()`](derive_cfb_analyte.md) : Derive a new analyte
  with change from baseline from an existing analyte

- [`derive_rtb()`](derive_rtb.md) : Calculate ratio to baseline

- [`derive_sld()`](derive_sld.md) **\[experimental\]** : Calculate SLD
  for SDTM.TR domain

- [`disposition_summary()`](disposition_summary.md) : Subject
  disposition overview

- [`domain()`](domain.md) : Return a specific domain from a sdtm object

- [`domain_model`](domain_model.md) : SDTM Domain Model variables

- [`dose_levels()`](dose_levels.md) : Dose levels within a NIF object

- [`dose_lin()`](dose_lin.md) **\[experimental\]** : Test for dose
  linearity

- [`dose_red_sbs()`](dose_red_sbs.md) : Subjects with dose reduction

- [`doses()`](doses.md) : Doses in a nif or sdtm object

- [`edish_plot()`](edish_plot.md) : Drug-induced serious hepatotoxicity
  (eDISH) plot

- [`egfr_cg()`](egfr_cg.md) : Glomerular filtration rate estimation from
  serum creatinine (Cockcroft-Gault)

- [`egfr_mdrd()`](egfr_mdrd.md) : Glomerular filtration rate estimation
  from serum creatinine (MDRD)

- [`egfr_raynaud()`](egfr_raynaud.md) : Glomerular filtration rate
  estimation from serum creatinine (Raynaud method)

- [`examplinib_fe`](examplinib_fe.md) : Synthetic SDTM data from a
  fictional clinical food effect study for 'examplinib'.

- [`examplinib_fe_nif`](examplinib_fe_nif.md) : Synthetic NIF data from
  a fictional clinical food effect study for 'examplinib'.

- [`examplinib_poc`](examplinib_poc.md) : Synthetic SDTM data from a
  fictional clinical single-arm study with rich and sparse pk sampling.

- [`examplinib_poc_min_nif`](examplinib_poc_min_nif.md) : Synthetic NIF
  data from a fictional clinical single-arm study with rich and sparse
  pk sampling. Reduced to the minimally required fields.

- [`examplinib_poc_nif`](examplinib_poc_nif.md) : Synthetic NIF data
  from a fictional clinical single-arm study with rich and sparse pk
  sampling.

- [`examplinib_sad`](examplinib_sad.md) : Synthetic SDTM data from a
  fictional clinical SAD FIH study with rich PK sampling

- [`examplinib_sad_min_nif`](examplinib_sad_min_nif.md) : Synthetic NIF
  data from a fictional clinical SAD FIH study with rich PK sampling.
  Reduced to the minimally required fields.

- [`examplinib_sad_nif`](examplinib_sad_nif.md) : Synthetic NIF data
  from a fictional clinical SAD FIH study with rich PK sampling

- [`filter_subject()`](filter_subject.md) : Keep only selected USUBJID
  in the data set

- [`geom_admin()`](geom_admin.md) : Administration geom layer for ggplot

- [`import_from_connection()`](import_from_connection.md) : Import nif
  object from connection

- [`import_nif()`](import_nif.md) : Import nif file

- [`import_observation()`](import_observation.md) **\[experimental\]** :
  Add observation from non-SDTM-formatted data table

- [`index_dosing_interval()`](index_dosing_interval.md) : Index dosing
  intervals

- [`index_rich_sampling_intervals()`](index_rich_sampling_intervals.md)
  : Identify and index rich PK sampling intervals

- [`last_dtc()`](last_dtc.md) : Last recorded date-time

- [`lbm_boer()`](lbm_boer.md) : Lean body mass (Boer formula)

- [`lbm_hume()`](lbm_hume.md) : Lean body mass (Hume formula)

- [`lbm_peters()`](lbm_peters.md) : Lean body mass (Peters formula)

- [`limit()`](limit.md) : Subset nif to rows with DTC before the last
  individual or global observation

- [`max_admin_time()`](max_admin_time.md) : Maximal administration time

- [`max_observation_time()`](max_observation_time.md) : Maximal
  observation time

- [`max_time()`](max_time.md) : Maximal time in nif object

- [`n_administrations()`](n_administrations.md) : Number of
  administrations per subject

- [`nca()`](nca.md) **\[experimental\]** : Non-compartmental analysis of
  NIF data

- [`nca_from_pp()`](nca_from_pp.md) : Generate NCA table from the
  SDTM.PP domain

- [`nca_power_model()`](nca_power_model.md) **\[experimental\]** : Power
  fit for PK parameters

- [`nca_summary()`](nca_summary.md) : PK parameter summary statistics by
  dose

- [`nca_summary_table()`](nca_summary_table.md) : PK parameter summary
  statistics table by dose

- [`new_nif()`](new_nif.md) **\[deprecated\]** : nif class constructor

- [`new_sdtm()`](new_sdtm.md) **\[deprecated\]** : SDTM class
  constructor, creating a sdtm object from a set of SDTM domains

- [`nif()`](nif.md) : nif class constructor

- [`nif_auto()`](nif_auto.md) **\[experimental\]** : Auto-generate nif
  from sdtm object

- [`nif_disclaimer()`](nif_disclaimer.md) : Disclaimer statement

- [`nif_option()`](nif_option.md) : Set or get global options

- [`nif_pinboard()`](nif_pinboard.md) : Get or set pinboard path

- [`nif_viewer()`](nif_viewer.md) : NIF viewer

- [`obs_per_dose_level()`](obs_per_dose_level.md) : Observations per
  dose level

- [`pb_list_nif()`](pb_list_nif.md) **\[experimental\]** : List nif
  objects in pinboard

- [`pb_list_sdtm()`](pb_list_sdtm.md) **\[experimental\]** : List sdtm
  objects in pinboard

- [`pb_read_nif()`](pb_read_nif.md) **\[experimental\]** : Read nif
  object from pinboard

- [`pb_read_sdtm()`](pb_read_sdtm.md) **\[experimental\]** : Read sdtm
  object from pinboard

- [`pb_write()`](pb_write.md) **\[experimental\]** : Generic pin_write
  function

- [`plot(`*`<domain>`*`)`](plot.domain.md) : Plot domain object

- [`plot(`*`<nif>`*`)`](plot.nif.md) : Plot NIF object.

- [`plot(`*`<sdtm>`*`)`](plot.sdtm.md) : Plot SDTM object

- [`qtime_plot()`](qtime_plot.md) **\[experimental\]** : Plot analyte
  over time by discretized time after first dose (QTIME)

- [`race_coding`](race_coding.md) : Race coding table

- [`read_sdtm()`](read_sdtm.md) : Read SDTM data

- [`recode_race()`](recode_race.md) : Recode RACE columns in nif object

- [`rich_sampling_sbs()`](rich_sampling_sbs.md) : Identify subjects with
  rich sampling

- [`sdtm()`](sdtm.md) : SDTM class constructor

- [`sdtm_missing_times()`](sdtm_missing_times.md) : Number of DTC
  entries with missing time information

- [`standardize_date_format()`](standardize_date_format.md) : Convert
  date fields to POSIX format

- [`stat_admin()`](stat_admin.md) : ggplot stat for treatment
  administrations

- [`studies()`](studies.md) : Studies within a nif object

- [`subject_info()`](subject_info.md) : Baseline details for specific
  subjects

- [`subjects()`](subjects.md) : Unique subjects within a data set

- [`subs_per_dose_level()`](subs_per_dose_level.md) : Subjects per dose
  level

- [`suggest()`](suggest.md) : Suggest data programming steps to generate
  a nif object from an sdtm object

- [`summary(`*`<adam>`*`)`](summary.adam.md) : Summary method for adam
  objects

- [`testcd()`](testcd.md) : Extract TESTCD fields by domain from a sdtm
  object

- [`treatments()`](treatments.md) : Treatments in a nif or sdtm object

- [`trial_dco()`](trial_dco.md) : Cut-off date

- [`trial_title()`](trial_title.md) : Trial title

- [`usubjid()`](usubjid.md) : Get the USUBJID of subject

- [`watermark()`](watermark.md) : Add a watermark annotation layer for a
  ggplot2 object

- [`write_monolix()`](write_monolix.md) : Write as comma-separated file,
  complying with the format used by Monolix

- [`write_nonmem()`](write_nonmem.md) : Write NONMEM input formatted nif
  object to file
