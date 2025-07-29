Tests and Coverage
================
29 July, 2025 08:17:11

- [Coverage](#coverage)
- [Unit Tests](#unit-tests)

This output is created by
[covrpage](https://github.com/yonicd/covrpage).

## Coverage

Coverage summary is created using the
[covr](https://github.com/r-lib/covr) package.

    ## ⚠️ Not All Tests Passed
    ##   Coverage statistics are approximations of the non-failing tests.
    ##   Use with caution
    ## 
    ##  For further investigation check in testthat summary tables.

| Object | Coverage (%) |
|:---|:--:|
| nif | 84.21 |
| [R/nif_nca_new.R](../R/nif_nca_new.R) | 0.00 |
| [R/nif_nca.R](../R/nif_nca.R) | 35.80 |
| [R/nif_viewer.R](../R/nif_viewer.R) | 38.82 |
| [R/physiological_calculations.R](../R/physiological_calculations.R) | 67.44 |
| [R/ggplot_extension.R](../R/ggplot_extension.R) | 76.24 |
| [R/nif_ae.R](../R/nif_ae.R) | 78.85 |
| [R/nif_auto.R](../R/nif_auto.R) | 87.12 |
| [R/nif_observations.R](../R/nif_observations.R) | 87.68 |
| [R/utilities.R](../R/utilities.R) | 89.51 |
| [R/nif_ntile.R](../R/nif_ntile.R) | 89.66 |
| [R/nif_class.R](../R/nif_class.R) | 90.58 |
| [R/nif_exploration.R](../R/nif_exploration.R) | 91.11 |
| [R/sdtm_class.R](../R/sdtm_class.R) | 91.44 |
| [R/validation.R](../R/validation.R) | 91.82 |
| [R/make_nif.R](../R/make_nif.R) | 92.41 |
| [R/nif_load.R](../R/nif_load.R) | 92.48 |
| [R/sdtm_exploration.R](../R/sdtm_exploration.R) | 92.64 |
| [R/nif_write.R](../R/nif_write.R) | 92.86 |
| [R/nif_ensure.R](../R/nif_ensure.R) | 93.16 |
| [R/sdtm_mappings.R](../R/sdtm_mappings.R) | 95.07 |
| [R/nif_covariates.R](../R/nif_covariates.R) | 95.77 |
| [R/nif_plot.R](../R/nif_plot.R) | 96.30 |
| [R/nif_administrations.R](../R/nif_administrations.R) | 96.45 |
| [R/nif_options.R](../R/nif_options.R) | 96.55 |
| [R/nif_imputations.R](../R/nif_imputations.R) | 97.88 |
| [R/nif_subjects.R](../R/nif_subjects.R) | 98.55 |
| [R/sdtm_analytics.R](../R/sdtm_analytics.R) | 100.00 |
| [R/sdtm_load.R](../R/sdtm_load.R) | 100.00 |

<br>

## Unit Tests

Unit Test summary is created using the
[testthat](https://github.com/r-lib/testthat) package.

| file | n | time | error | failed | skipped | warning |
|:---|---:|---:|---:|---:|---:|---:|
| [test-add_ae_observation.R](testthat/test-add_ae_observation.R) | 18 | 0.806 | 0 | 0 | 0 | 0 |
| [test-add_analyte_mapping.R](testthat/test-add_analyte_mapping.R) | 41 | 0.088 | 0 | 0 | 0 | 0 |
| [test-add_baseline.R](testthat/test-add_baseline.R) | 25 | 0.148 | 1 | 0 | 0 | 0 |
| [test-add_cfb.R](testthat/test-add_cfb.R) | 53 | 0.124 | 0 | 0 | 0 | 0 |
| [test-add_covariate.R](testthat/test-add_covariate.R) | 16 | 0.178 | 3 | 0 | 0 | 0 |
| [test-add_metabolite_mapping.R](testthat/test-add_metabolite_mapping.R) | 27 | 0.069 | 0 | 0 | 0 | 0 |
| [test-add_ntile.R](testthat/test-add_ntile.R) | 51 | 0.147 | 0 | 0 | 0 | 0 |
| [test-add_observations.R](testthat/test-add_observations.R) | 24 | 2.896 | 0 | 0 | 0 | 0 |
| [test-add_parent_mapping.R](testthat/test-add_parent_mapping.R) | 26 | 0.065 | 0 | 0 | 0 | 0 |
| [test-add_tad.R](testthat/test-add_tad.R) | 12 | 0.056 | 0 | 0 | 0 | 0 |
| [test-add_tafd.R](testthat/test-add_tafd.R) | 27 | 0.134 | 0 | 0 | 0 | 0 |
| [test-ae_summary.R](testthat/test-ae_summary.R) | 24 | 0.075 | 0 | 0 | 0 | 0 |
| [test-auto_mapping.R](testthat/test-auto_mapping.R) | 23 | 0.091 | 0 | 0 | 0 | 0 |
| [test-calculate_bmi.R](testthat/test-calculate_bmi.R) | 15 | 0.026 | 0 | 0 | 0 | 0 |
| [test-decompose_dtc.R](testthat/test-decompose_dtc.R) | 25 | 0.052 | 1 | 0 | 0 | 0 |
| [test-df_to_string.R](testthat/test-df_to_string.R) | 13 | 0.024 | 0 | 0 | 0 | 0 |
| [test-domain.R](testthat/test-domain.R) | 15 | 0.036 | 0 | 0 | 0 | 0 |
| [test-edish_plot.R](testthat/test-edish_plot.R) | 6 | 0.115 | 0 | 0 | 0 | 0 |
| [test-ensure_analyte.R](testthat/test-ensure_analyte.R) | 17 | 0.034 | 0 | 0 | 0 | 0 |
| [test-ensure_dose.R](testthat/test-ensure_dose.R) | 12 | 0.050 | 0 | 0 | 0 | 0 |
| [test-ensure_parent.R](testthat/test-ensure_parent.R) | 11 | 0.027 | 0 | 0 | 0 | 0 |
| [test-ensure_tad.R](testthat/test-ensure_tad.R) | 15 | 0.075 | 0 | 0 | 0 | 0 |
| [test-ensure_tafd.R](testthat/test-ensure_tafd.R) | 18 | 0.089 | 0 | 0 | 0 | 0 |
| [test-ensure_time.R](testthat/test-ensure_time.R) | 22 | 0.102 | 0 | 0 | 0 | 0 |
| [test-find_duplicates.R](testthat/test-find_duplicates.R) | 18 | 0.043 | 1 | 0 | 0 | 0 |
| [test-formula_to_mapping.R](testthat/test-formula_to_mapping.R) | 7 | 0.028 | 0 | 0 | 0 | 0 |
| [test-geom_admin.R](testthat/test-geom_admin.R) | 18 | 0.048 | 0 | 0 | 0 | 0 |
| [test-ggplot_extension.R](testthat/test-ggplot_extension.R) | 1 | 0.002 | 0 | 0 | 0 | 0 |
| [test-guess_ntime.R](testthat/test-guess_ntime.R) | 14 | 0.029 | 0 | 0 | 0 | 0 |
| [test-guess_parent.R](testthat/test-guess_parent.R) | 11 | 0.059 | 0 | 0 | 0 | 0 |
| [test-guess_pcspec.R](testthat/test-guess_pcspec.R) | 15 | 0.026 | 0 | 0 | 0 | 0 |
| [test-has_domain.R](testthat/test-has_domain.R) | 25 | 0.042 | 0 | 0 | 0 | 0 |
| [test-import_from_connection.R](testthat/test-import_from_connection.R) | 35 | 0.079 | 0 | 0 | 0 | 0 |
| [test-import_nif.R](testthat/test-import_nif.R) | 30 | 0.055 | 0 | 0 | 0 | 0 |
| [test-import_observation.R](testthat/test-import_observation.R) | 31 | 0.253 | 0 | 0 | 0 | 0 |
| [test-imputations.R](testthat/test-imputations.R) | 5 | 0.054 | 0 | 0 | 0 | 0 |
| [test-impute_exendtc_to_cutoff.R](testthat/test-impute_exendtc_to_cutoff.R) | 4 | 0.044 | 0 | 0 | 0 | 0 |
| [test-impute_exendtc_to_rfendtc.R](testthat/test-impute_exendtc_to_rfendtc.R) | 19 | 0.074 | 0 | 0 | 0 | 0 |
| [test-impute_missing_exendtc.R](testthat/test-impute_missing_exendtc.R) | 16 | 0.072 | 0 | 0 | 0 | 0 |
| [test-is_valid_filter.R](testthat/test-is_valid_filter.R) | 28 | 0.053 | 0 | 0 | 0 | 0 |
| [test-lbm_boer.R](testthat/test-lbm_boer.R) | 26 | 0.034 | 0 | 0 | 0 | 0 |
| [test-lbm_hume.R](testthat/test-lbm_hume.R) | 19 | 0.024 | 0 | 0 | 0 | 0 |
| [test-lbm_peters.R](testthat/test-lbm_peters.R) | 17 | 0.017 | 0 | 0 | 0 | 0 |
| [test-lubrify_dates.R](testthat/test-lubrify_dates.R) | 37 | 0.055 | 4 | 0 | 0 | 0 |
| [test-make_administration.R](testthat/test-make_administration.R) | 6 | 0.322 | 0 | 0 | 0 | 0 |
| [test-make_ae.R](testthat/test-make_ae.R) | 19 | 0.160 | 0 | 0 | 0 | 0 |
| [test-make_nif.R](testthat/test-make_nif.R) | 25 | 0.901 | 0 | 0 | 0 | 0 |
| [test-make_ntime_from_tpt.R](testthat/test-make_ntime_from_tpt.R) | 4 | 0.010 | 0 | 0 | 0 | 0 |
| [test-make_ntime.R](testthat/test-make_ntime.R) | 16 | 0.049 | 0 | 0 | 0 | 0 |
| [test-make_observation.R](testthat/test-make_observation.R) | 46 | 0.547 | 0 | 0 | 0 | 0 |
| [test-make_plot_data_set.R](testthat/test-make_plot_data_set.R) | 23 | 0.238 | 0 | 0 | 0 | 0 |
| [test-make_subjects_sdtm.R](testthat/test-make_subjects_sdtm.R) | 14 | 0.046 | 0 | 0 | 0 | 0 |
| [test-nca_from_pp.R](testthat/test-nca_from_pp.R) | 14 | 0.044 | 0 | 0 | 0 | 0 |
| [test-nca.R](testthat/test-nca.R) | 15 | 0.334 | 0 | 0 | 0 | 0 |
| [test-nif_add_baseline.R](testthat/test-nif_add_baseline.R) | 1 | 0.002 | 0 | 0 | 0 | 0 |
| [test-nif_administrations.R](testthat/test-nif_administrations.R) | 16 | 0.077 | 0 | 0 | 0 | 0 |
| [test-nif_ae.R](testthat/test-nif_ae.R) | 1 | 0.002 | 0 | 0 | 0 | 0 |
| [test-nif_auto.R](testthat/test-nif_auto.R) | 11 | 0.790 | 0 | 0 | 0 | 0 |
| [test-nif_class.R](testthat/test-nif_class.R) | 113 | 0.714 | 0 | 0 | 0 | 0 |
| [test-nif_exploration.R](testthat/test-nif_exploration.R) | 24 | 0.445 | 0 | 0 | 0 | 0 |
| [test-nif_load.R](testthat/test-nif_load.R) | 13 | 0.021 | 0 | 0 | 0 | 0 |
| [test-nif_nca.R](testthat/test-nif_nca.R) | 2 | 0.005 | 0 | 0 | 0 | 0 |
| [test-nif_options.R](testthat/test-nif_options.R) | 9 | 0.015 | 0 | 0 | 0 | 0 |
| [test-nif_plot.R](testthat/test-nif_plot.R) | 15 | 0.477 | 0 | 0 | 0 | 0 |
| [test-nif_subjects.R](testthat/test-nif_subjects.R) | 60 | 0.255 | 0 | 0 | 0 | 0 |
| [test-nif_viewer.R](testthat/test-nif_viewer.R) | 5 | 0.025 | 0 | 0 | 0 | 0 |
| [test-physiological_calculations.R](testthat/test-physiological_calculations.R) | 51 | 0.082 | 0 | 0 | 0 | 0 |
| [test-resolve-duplicates.R](testthat/test-resolve-duplicates.R) | 26 | 0.046 | 0 | 0 | 0 | 0 |
| [test-sdtm_analytics.R](testthat/test-sdtm_analytics.R) | 1 | 0.004 | 0 | 0 | 0 | 0 |
| [test-sdtm_class.R](testthat/test-sdtm_class.R) | 28 | 0.089 | 0 | 0 | 0 | 0 |
| [test-sdtm_exploration.R](testthat/test-sdtm_exploration.R) | 38 | 0.631 | 0 | 0 | 0 | 0 |
| [test-sdtm_load.R](testthat/test-sdtm_load.R) | 21 | 0.067 | 0 | 0 | 0 | 0 |
| [test-StatAdmin.R](testthat/test-StatAdmin.R) | 17 | 0.027 | 0 | 0 | 0 | 0 |
| [test-summary-nif.R](testthat/test-summary-nif.R) | 44 | 0.677 | 0 | 0 | 0 | 0 |
| [test-summary-sdtm.R](testthat/test-summary-sdtm.R) | 39 | 0.066 | 0 | 0 | 0 | 0 |
| [test-testcd.R](testthat/test-testcd.R) | 37 | 0.077 | 0 | 0 | 0 | 0 |
| [test-utilities.R](testthat/test-utilities.R) | 99 | 0.157 | 0 | 0 | 0 | 0 |
| [test-validate_domain.R](testthat/test-validate_domain.R) | 16 | 0.060 | 0 | 0 | 0 | 0 |
| [test-validate_sdtm.R](testthat/test-validate_sdtm.R) | 4 | 0.015 | 0 | 0 | 0 | 0 |
| [test-validation.R](testthat/test-validation.R) | 87 | 0.189 | 0 | 0 | 0 | 0 |
| [test-viewer.R](testthat/test-viewer.R) | 1 | 0.002 | 0 | 0 | 0 | 0 |
| [test-watermark.R](testthat/test-watermark.R) | 24 | 0.052 | 0 | 0 | 0 | 0 |
| [test-write_nif.R](testthat/test-write_nif.R) | 11 | 0.037 | 0 | 0 | 0 | 0 |

<details open>

<summary>

Show Detailed Test Results
</summary>

| file | context | test | status | n | time |
|:---|:---|:---|:---|---:|---:|
| [test-add_ae_observation.R](testthat/test-add_ae_observation.R#L52) | add_ae_observation | add_ae_observation handles basic case correctly | PASS | 6 | 0.132 |
| [test-add_ae_observation.R](testthat/test-add_ae_observation.R#L103) | add_ae_observation | add_ae_observation handles different ae_fields correctly | PASS | 2 | 0.197 |
| [test-add_ae_observation.R](testthat/test-add_ae_observation.R#L155) | add_ae_observation | add_ae_observation handles filters correctly | PASS | 3 | 0.147 |
| [test-add_ae_observation.R](testthat/test-add_ae_observation.R#L206) | add_ae_observation | add_ae_observation handles debug mode correctly | PASS | 3 | 0.111 |
| [test-add_ae_observation.R](testthat/test-add_ae_observation.R#L245) | add_ae_observation | add_ae_observation handles keep parameter correctly | PASS | 2 | 0.110 |
| [test-add_ae_observation.R](testthat/test-add_ae_observation.R#L285) | add_ae_observation | add_ae_observation handles automatic parent and cmt assignment | PASS | 2 | 0.109 |
| [test-add_analyte_mapping.R](testthat/test-add_analyte_mapping.R#L14) | add_analyte_mapping | add_analyte_mapping adds correct mapping | PASS | 5 | 0.008 |
| [test-add_analyte_mapping.R](testthat/test-add_analyte_mapping.R#L40) | add_analyte_mapping | add_analyte_mapping uses custom analyte name when provided | PASS | 1 | 0.003 |
| [test-add_analyte_mapping.R](testthat/test-add_analyte_mapping.R#L57) | add_analyte_mapping | add_analyte_mapping enforces unique EXTRT | PASS | 2 | 0.006 |
| [test-add_analyte_mapping.R](testthat/test-add_analyte_mapping.R#L82) | add_analyte_mapping | add_analyte_mapping can add multiple mappings with different EXTRT | PASS | 8 | 0.010 |
| [test-add_analyte_mapping.R](testthat/test-add_analyte_mapping.R#L106) | add_analyte_mapping | add_analyte_mapping prevents updating existing EXTRT mappings | PASS | 2 | 0.005 |
| [test-add_analyte_mapping.R](testthat/test-add_analyte_mapping.R#L117_L120) | add_analyte_mapping | add_analyte_mapping validates input correctly | PASS | 5 | 0.015 |
| [test-add_analyte_mapping.R](testthat/test-add_analyte_mapping.R#L155_L158) | add_analyte_mapping | add_analyte_mapping handles vector inputs with warnings | PASS | 6 | 0.014 |
| [test-add_analyte_mapping.R](testthat/test-add_analyte_mapping.R#L181_L184) | add_analyte_mapping | add_analyte_mapping rejects NA values | PASS | 3 | 0.010 |
| [test-add_analyte_mapping.R](testthat/test-add_analyte_mapping.R#L204_L207) | add_analyte_mapping | add_analyte_mapping validates analyte parameter | PASS | 2 | 0.007 |
| [test-add_analyte_mapping.R](testthat/test-add_analyte_mapping.R#L233) | add_analyte_mapping | add_analyte_mapping initializes analyte_mapping when NULL | PASS | 4 | 0.005 |
| [test-add_analyte_mapping.R](testthat/test-add_analyte_mapping.R#L247) | add_analyte_mapping | add_analyte_mapping properly trims whitespace | PASS | 3 | 0.005 |
| [test-add_baseline.R](testthat/test-add_baseline.R#L32) | add_baseline | add_baseline adds baseline covariate correctly | PASS | 4 | 0.013 |
| [test-add_baseline.R](testthat/test-add_baseline.R#L67) | add_baseline | add_baseline handles custom baseline filter | PASS | 2 | 0.010 |
| [test-add_baseline.R](testthat/test-add_baseline.R#L105) | add_baseline | add_baseline handles coding table correctly | PASS | 2 | 0.021 |
| [test-add_baseline.R](testthat/test-add_baseline.R#L134_L137) | add_baseline | add_baseline validates inputs correctly | ERROR | 0 | 0.004 |
| [test-add_baseline.R](testthat/test-add_baseline.R#L181) | add_baseline | add_baseline handles multiple baseline values correctly | PASS | 2 | 0.016 |
| [test-add_baseline.R](testthat/test-add_baseline.R#L211_L213) | add_baseline | add_baseline handles empty result after filtering | PASS | 1 | 0.006 |
| [test-add_baseline.R](testthat/test-add_baseline.R#L259_L262) | add_baseline | add baseline hepatic function class works | PASS | 2 | 0.018 |
| [test-add_baseline.R](testthat/test-add_baseline.R#L291_L294) | add_baseline | add_baseline handles all NA baseline values correctly | PASS | 1 | 0.010 |
| [test-add_baseline.R](testthat/test-add_baseline.R#L320_L324) | add_baseline | add_baseline warns when some baseline values are NA | PASS | 5 | 0.018 |
| [test-add_baseline.R](testthat/test-add_baseline.R#L354_L357) | add_baseline | add_baseline validates required fields correctly | PASS | 2 | 0.007 |
| [test-add_baseline.R](testthat/test-add_baseline.R#L383) | add_baseline | add_baseline name parameter works correctly | PASS | 4 | 0.025 |
| [test-add_cfb.R](testthat/test-add_cfb.R#L19) | add_cfb | add_cfb works with valid input | PASS | 5 | 0.010 |
| [test-add_cfb.R](testthat/test-add_cfb.R#L49) | add_cfb | add_cfb correctly handles baseline calculation with time ≤ 0 | PASS | 10 | 0.024 |
| [test-add_cfb.R](testthat/test-add_cfb.R#L80_L84) | add_cfb | add_cfb handles NA values in grouping columns | PASS | 3 | 0.012 |
| [test-add_cfb.R](testthat/test-add_cfb.R#L102) | add_cfb | add_cfb works with different summary functions | PASS | 3 | 0.010 |
| [test-add_cfb.R](testthat/test-add_cfb.R#L126) | add_cfb | add_cfb works with custom baseline filter | PASS | 1 | 0.004 |
| [test-add_cfb.R](testthat/test-add_cfb.R#L138) | add_cfb | add_cfb handles missing required columns | PASS | 2 | 0.007 |
| [test-add_cfb.R](testthat/test-add_cfb.R#L160) | add_cfb | add_cfb handles non-numeric columns | PASS | 2 | 0.007 |
| [test-add_cfb.R](testthat/test-add_cfb.R#L194) | add_cfb | add_cfb correctly handles complex baseline filters | PASS | 10 | 0.014 |
| [test-add_cfb.R](testthat/test-add_cfb.R#L227) | add_cfb | add_cfb correctly handles empty baseline sets | PASS | 2 | 0.006 |
| [test-add_cfb.R](testthat/test-add_cfb.R#L251) | add_cfb | add_cfb correctly handles baseline filter with missing values | PASS | 8 | 0.012 |
| [test-add_cfb.R](testthat/test-add_cfb.R#L284) | add_cfb | add_cfb correctly handles baseline filter with character columns | PASS | 7 | 0.018 |
| [test-add_covariate.R](testthat/test-add_covariate.R#L81) | add_covariate | add_covariate works with valid inputs | PASS | 9 | 0.077 |
| [test-add_covariate.R](testthat/test-add_covariate.R#L104_L107) | add_covariate | add_covariate validates nif object | ERROR | 0 | 0.010 |
| [test-add_covariate.R](testthat/test-add_covariate.R#L115_L118) | add_covariate | add_covariate validates sdtm is provided | ERROR | 0 | 0.007 |
| [test-add_covariate.R](testthat/test-add_covariate.R#L127_L130) | add_covariate | add_covariate validates domain exists | ERROR | 0 | 0.007 |
| [test-add_covariate.R](testthat/test-add_covariate.R#L145_L148) | add_covariate | add_covariate validates required fields exist | PASS | 1 | 0.007 |
| [test-add_covariate.R](testthat/test-add_covariate.R#L157_L160) | add_covariate | add_covariate validates testcd exists | PASS | 1 | 0.006 |
| [test-add_covariate.R](testthat/test-add_covariate.R#L172_L175) | add_covariate | add_covariate validates matching subjects exist | PASS | 1 | 0.006 |
| [test-add_covariate.R](testthat/test-add_covariate.R#L185_L189) | add_covariate | add_covariate casts error if no data after filtering | PASS | 1 | 0.008 |
| [test-add_covariate.R](testthat/test-add_covariate.R#L215) | add_covariate | add_covariate works with custom field names | PASS | 1 | 0.019 |
| [test-add_covariate.R](testthat/test-add_covariate.R#L235) | add_covariate | add_covariate handles duplicated observations correctly | PASS | 1 | 0.016 |
| [test-add_covariate.R](testthat/test-add_covariate.R#L249) | add_covariate | add_covariate uses default covariate name if not specified | PASS | 1 | 0.015 |
| [test-add_metabolite_mapping.R](testthat/test-add_metabolite_mapping.R#L14) | add_metabolite_mapping | add_metabolite_mapping adds correct mapping | PASS | 4 | 0.006 |
| [test-add_metabolite_mapping.R](testthat/test-add_metabolite_mapping.R#L42) | add_metabolite_mapping | add_metabolite_mapping can add multiple mappings | PASS | 6 | 0.018 |
| [test-add_metabolite_mapping.R](testthat/test-add_metabolite_mapping.R#L66_L69) | add_metabolite_mapping | add_metabolite_mapping prevents duplicate mappings | PASS | 1 | 0.005 |
| [test-add_metabolite_mapping.R](testthat/test-add_metabolite_mapping.R#L74_L77) | add_metabolite_mapping | add_metabolite_mapping validates input correctly | PASS | 5 | 0.014 |
| [test-add_metabolite_mapping.R](testthat/test-add_metabolite_mapping.R#L112_L115) | add_metabolite_mapping | add_metabolite_mapping handles vector inputs with warnings | PASS | 4 | 0.009 |
| [test-add_metabolite_mapping.R](testthat/test-add_metabolite_mapping.R#L131_L134) | add_metabolite_mapping | add_metabolite_mapping rejects NA values | PASS | 2 | 0.008 |
| [test-add_metabolite_mapping.R](testthat/test-add_metabolite_mapping.R#L160) | add_metabolite_mapping | add_metabolite_mapping initializes metabolite_mapping when NULL | PASS | 3 | 0.005 |
| [test-add_metabolite_mapping.R](testthat/test-add_metabolite_mapping.R#L173) | add_metabolite_mapping | add_metabolite_mapping properly trims whitespace | PASS | 2 | 0.004 |
| [test-add_ntile.R](testthat/test-add_ntile.R#L24) | add_ntile | add_ntile works with basic input | PASS | 6 | 0.012 |
| [test-add_ntile.R](testthat/test-add_ntile.R#L56) | add_ntile | add_ntile works with custom n value | PASS | 7 | 0.013 |
| [test-add_ntile.R](testthat/test-add_ntile.R#L86) | add_ntile | add_ntile works with custom column name | PASS | 6 | 0.011 |
| [test-add_ntile.R](testthat/test-add_ntile.R#L144) | add_ntile | add_ntile handles NA values in input column | PASS | 4 | 0.009 |
| [test-add_ntile.R](testthat/test-add_ntile.R#L163_L166) | add_ntile | add_ntile handles non-numeric input column | PASS | 1 | 0.005 |
| [test-add_ntile.R](testthat/test-add_ntile.R#L180_L183) | add_ntile | add_ntile handles missing input column | PASS | 1 | 0.004 |
| [test-add_ntile.R](testthat/test-add_ntile.R#L198_L201) | add_ntile | add_ntile handles invalid n value | PASS | 5 | 0.025 |
| [test-add_ntile.R](testthat/test-add_ntile.R#L235_L239) | add_ntile | add_ntile handles empty data frame | PASS | 1 | 0.003 |
| [test-add_ntile.R](testthat/test-add_ntile.R#L322) | add_ntile | add_ntile preserves original data | PASS | 2 | 0.007 |
| [test-add_ntile.R](testthat/test-add_ntile.R#L343) | add_ntile | add_ntile returns a nif object | PASS | 1 | 0.005 |
| [test-add_ntile.R](testthat/test-add_ntile.R#L363) | add_ntile | add_ntile works with different input columns | PASS | 3 | 0.017 |
| [test-add_ntile.R](testthat/test-add_ntile.R#L410) | add_ntile | add_ntile handles all subjects having the same value | PASS | 3 | 0.007 |
| [test-add_ntile.R](testthat/test-add_ntile.R#L438) | add_ntile | add_ntile handles subjects with multiple distinct values | PASS | 5 | 0.010 |
| [test-add_ntile.R](testthat/test-add_ntile.R#L457_L460) | add_ntile | add_ntile handles invalid input object | PASS | 1 | 0.004 |
| [test-add_ntile.R](testthat/test-add_ntile.R#L479_L482) | add_ntile | add_ntile handles invalid input_col parameter | PASS | 3 | 0.009 |
| [test-add_ntile.R](testthat/test-add_ntile.R#L513_L516) | add_ntile | add_ntile handles invalid ntile_name parameter | PASS | 2 | 0.006 |
| [test-add_observations.R](testthat/test-add_observations.R#L54_L58) | add_observations | add_observation warns about duplicate compartment | PASS | 1 | 0.187 |
| [test-add_observations.R](testthat/test-add_observations.R#L68_L72) | add_observations | add_observation auto-assigns compartment if not specified | PASS | 2 | 0.176 |
| [test-add_observations.R](testthat/test-add_observations.R#L85_L91) | add_observations | add_observation auto-assigns parent if not specified | PASS | 1 | 0.181 |
| [test-add_observations.R](testthat/test-add_observations.R#L101_L106) | add_observations | add_observation properly uses observation_filter | PASS | 1 | 0.134 |
| [test-add_observations.R](testthat/test-add_observations.R#L134) | add_observations | add_observation works with factor parameter | PASS | 1 | 0.262 |
| [test-add_observations.R](testthat/test-add_observations.R#L158) | add_observations | add_observation handles metabolites correctly | PASS | 1 | 0.182 |
| [test-add_observations.R](testthat/test-add_observations.R#L174_L181) | add_observations | add_observation works with custom NTIME_lookup | PASS | 1 | 0.168 |
| [test-add_observations.R](testthat/test-add_observations.R#L196) | add_observations | add_observation handles debug mode correctly | PASS | 2 | 0.188 |
| [test-add_observations.R](testthat/test-add_observations.R#L218) | add_observations | add_observation updates columns correctly | PASS | 1 | 0.176 |
| [test-add_observations.R](testthat/test-add_observations.R#L246_L249) | add_observations | add_observation handles include_day_in_ntime parameter | PASS | 1 | 0.235 |
| [test-add_observations.R](testthat/test-add_observations.R#L267_L274) | add_observations | add_observation handles missing NTIME gracefully | PASS | 3 | 0.119 |
| [test-add_observations.R](testthat/test-add_observations.R#L295_L300) | add_observations | add_observation handles DV field properly | PASS | 2 | 0.118 |
| [test-add_observations.R](testthat/test-add_observations.R#L340) | add_observations | add_observation handles subject filtering | PASS | 1 | 0.119 |
| [test-add_observations.R](testthat/test-add_observations.R#L350_L356) | add_observations | add_observation can handle non-existent domain gracefully | PASS | 1 | 0.124 |
| [test-add_observations.R](testthat/test-add_observations.R#L374_L382) | add_observations | add_observation handles observations without matching administrations | PASS | 2 | 0.253 |
| [test-add_observations.R](testthat/test-add_observations.R#L401_L409) | add_observations | add_observation properly handles custom testcd field | PASS | 1 | 0.119 |
| [test-add_observations.R](testthat/test-add_observations.R#L447) | add_observations | add_observation handles na.rm parameter when resolving duplicates | PASS | 2 | 0.155 |
| [test-add_parent_mapping.R](testthat/test-add_parent_mapping.R#L14) | add_parent_mapping | add_parent_mapping adds correct parent mapping | PASS | 4 | 0.006 |
| [test-add_parent_mapping.R](testthat/test-add_parent_mapping.R#L42) | add_parent_mapping | add_parent_mapping can add multiple mappings | PASS | 3 | 0.005 |
| [test-add_parent_mapping.R](testthat/test-add_parent_mapping.R#L71) | add_parent_mapping | add_parent_mapping preserves existing mappings | PASS | 3 | 0.004 |
| [test-add_parent_mapping.R](testthat/test-add_parent_mapping.R#L78_L81) | add_parent_mapping | add_parent_mapping validates input correctly | PASS | 5 | 0.026 |
| [test-add_parent_mapping.R](testthat/test-add_parent_mapping.R#L116_L119) | add_parent_mapping | add_parent_mapping handles vector inputs with warnings | PASS | 4 | 0.009 |
| [test-add_parent_mapping.R](testthat/test-add_parent_mapping.R#L135_L138) | add_parent_mapping | add_parent_mapping rejects NA values | PASS | 2 | 0.007 |
| [test-add_parent_mapping.R](testthat/test-add_parent_mapping.R#L164) | add_parent_mapping | add_parent_mapping initializes parent_mapping when NULL | PASS | 3 | 0.005 |
| [test-add_parent_mapping.R](testthat/test-add_parent_mapping.R#L177) | add_parent_mapping | add_parent_mapping properly trims whitespace | PASS | 2 | 0.003 |
| [test-add_tad.R](testthat/test-add_tad.R#L18) | add_tad | add_tad works with basic input | PASS | 2 | 0.008 |
| [test-add_tad.R](testthat/test-add_tad.R#L38) | add_tad | add_tad handles multiple administrations | PASS | 1 | 0.007 |
| [test-add_tad.R](testthat/test-add_tad.R#L54) | add_tad | add_tad handles observations before first dose | PASS | 1 | 0.007 |
| [test-add_tad.R](testthat/test-add_tad.R#L72) | add_tad | add_tad handles multiple parent compounds | PASS | 1 | 0.007 |
| [test-add_tad.R](testthat/test-add_tad.R#L83) | add_tad | add_tad handles empty data frame | PASS | 2 | 0.003 |
| [test-add_tad.R](testthat/test-add_tad.R#L95) | add_tad | add_tad handles missing required columns | PASS | 1 | 0.004 |
| [test-add_tad.R](testthat/test-add_tad.R#L110) | add_tad | add_tad preserves original data | PASS | 2 | 0.008 |
| [test-add_tad.R](testthat/test-add_tad.R#L126) | add_tad | add_tad handles NA values in TIME | PASS | 1 | 0.006 |
| [test-add_tad.R](testthat/test-add_tad.R#L139) | add_tad | add_tad returns a nif object | PASS | 1 | 0.006 |
| [test-add_tafd.R](testthat/test-add_tafd.R#L17) | add_tafd | add_tafd works with basic input | PASS | 2 | 0.008 |
| [test-add_tafd.R](testthat/test-add_tafd.R#L37) | add_tafd | add_tafd handles observations before first dose | PASS | 1 | 0.007 |
| [test-add_tafd.R](testthat/test-add_tafd.R#L54) | add_tafd | add_tafd handles multiple administrations | PASS | 1 | 0.006 |
| [test-add_tafd.R](testthat/test-add_tafd.R#L72) | add_tafd | add_tafd handles multiple parent compounds | PASS | 1 | 0.007 |
| [test-add_tafd.R](testthat/test-add_tafd.R#L90) | add_tafd | add_tafd handles different first dose times | PASS | 1 | 0.018 |
| [test-add_tafd.R](testthat/test-add_tafd.R#L101) | add_tafd | add_tafd handles empty data frame | PASS | 2 | 0.004 |
| [test-add_tafd.R](testthat/test-add_tafd.R#L113) | add_tafd | add_tafd handles missing required columns | PASS | 1 | 0.004 |
| [test-add_tafd.R](testthat/test-add_tafd.R#L123) | add_tafd | add_tafd validates input is a nif object | PASS | 1 | 0.004 |
| [test-add_tafd.R](testthat/test-add_tafd.R#L138) | add_tafd | add_tafd preserves original data | PASS | 2 | 0.008 |
| [test-add_tafd.R](testthat/test-add_tafd.R#L151) | add_tafd | add_tafd validates numeric data types | PASS | 3 | 0.010 |
| [test-add_tafd.R](testthat/test-add_tafd.R#L184_L186) | add_tafd | add_tafd handles data with no dosing events | PASS | 1 | 0.005 |
| [test-add_tafd.R](testthat/test-add_tafd.R#L199) | add_tafd | add_tafd returns a nif object | PASS | 1 | 0.007 |
| [test-add_tafd.R](testthat/test-add_tafd.R#L217) | add_tafd | add_tafd handles missing PARENT column by creating it | PASS | 2 | 0.011 |
| [test-add_tafd.R](testthat/test-add_tafd.R#L234) | add_tafd | add_tafd properly ungroups the result | PASS | 2 | 0.008 |
| [test-add_tafd.R](testthat/test-add_tafd.R#L252) | add_tafd | add_tafd handles NA values in TIME correctly | PASS | 2 | 0.008 |
| [test-add_tafd.R](testthat/test-add_tafd.R#L266_L268) | add_tafd | add_tafd correctly handles NA values in ID column | PASS | 1 | 0.004 |
| [test-add_tafd.R](testthat/test-add_tafd.R#L284) | add_tafd | add_tafd respects parent grouping with mixed dosing times | PASS | 1 | 0.006 |
| [test-add_tafd.R](testthat/test-add_tafd.R#L301) | add_tafd | add_tafd works with CMT column but no PARENT column | PASS | 2 | 0.009 |
| [test-ae_summary.R](testthat/test-ae_summary.R#L19) | ae_summary | ae_summary handles basic case correctly | PASS | 5 | 0.009 |
| [test-ae_summary.R](testthat/test-ae_summary.R#L42) | ae_summary | ae_summary works with different levels | PASS | 3 | 0.009 |
| [test-ae_summary.R](testthat/test-ae_summary.R#L63) | ae_summary | ae_summary handles show_cd parameter | PASS | 2 | 0.014 |
| [test-ae_summary.R](testthat/test-ae_summary.R#L81) | ae_summary | ae_summary handles grouping | PASS | 2 | 0.007 |
| [test-ae_summary.R](testthat/test-ae_summary.R#L100) | ae_summary | ae_summary handles ordering | PASS | 1 | 0.004 |
| [test-ae_summary.R](testthat/test-ae_summary.R#L117) | ae_summary | ae_summary handles filtering | PASS | 2 | 0.005 |
| [test-ae_summary.R](testthat/test-ae_summary.R#L132_L133) | ae_summary | ae_summary handles invalid inputs | PASS | 2 | 0.007 |
| [test-ae_summary.R](testthat/test-ae_summary.R#L151) | ae_summary | ae_summary handles empty data | PASS | 2 | 0.005 |
| [test-ae_summary.R](testthat/test-ae_summary.R#L159_L160) | ae_summary | ae_summary validates SDTM object structure | PASS | 5 | 0.015 |
| [test-auto_mapping.R](testthat/test-auto_mapping.R#L26) | auto_mapping | auto_mapping creates correct mapping from NULL input | PASS | 4 | 0.014 |
| [test-auto_mapping.R](testthat/test-auto_mapping.R#L56) | auto_mapping | auto_mapping creates correct mapping from single formula | PASS | 4 | 0.010 |
| [test-auto_mapping.R](testthat/test-auto_mapping.R#L86) | auto_mapping | auto_mapping creates correct mapping from list of formulae | PASS | 4 | 0.012 |
| [test-auto_mapping.R](testthat/test-auto_mapping.R#L115) | auto_mapping | auto_mapping handles multiple analytes from same treatment | PASS | 4 | 0.010 |
| [test-auto_mapping.R](testthat/test-auto_mapping.R#L141_L144) | auto_mapping | auto_mapping detects duplicate mappings | PASS | 1 | 0.010 |
| [test-auto_mapping.R](testthat/test-auto_mapping.R#L168_L171) | auto_mapping | auto_mapping validates input types | PASS | 2 | 0.006 |
| [test-auto_mapping.R](testthat/test-auto_mapping.R#L204) | auto_mapping | auto_mapping sets correct METABOLITE flags | PASS | 2 | 0.008 |
| [test-auto_mapping.R](testthat/test-auto_mapping.R#L236) | auto_mapping | auto_mapping generates automatic mapping for multiple analytes | PASS | 2 | 0.021 |
| [test-calculate_bmi.R](testthat/test-calculate_bmi.R#L3) | calculate_bmi | calculate_bmi works correctly for valid inputs | PASS | 3 | 0.004 |
| [test-calculate_bmi.R](testthat/test-calculate_bmi.R#L16) | calculate_bmi | calculate_bmi handles NA values correctly | PASS | 5 | 0.006 |
| [test-calculate_bmi.R](testthat/test-calculate_bmi.R#L31) | calculate_bmi | calculate_bmi handles invalid inputs correctly | PASS | 4 | 0.005 |
| [test-calculate_bmi.R](testthat/test-calculate_bmi.R#L40) | calculate_bmi | calculate_bmi handles type errors correctly | PASS | 2 | 0.007 |
| [test-calculate_bmi.R](testthat/test-calculate_bmi.R#L47_L50) | calculate_bmi | calculate_bmi handles length mismatch correctly | PASS | 1 | 0.004 |
| [test-decompose_dtc.R](testthat/test-decompose_dtc.R#L16) | decompose_dtc | decompose_dtc works comprehensively | PASS | 10 | 0.020 |
| [test-decompose_dtc.R](testthat/test-decompose_dtc.R#L56) | decompose_dtc | decompose_dtc handles edge cases | PASS | 7 | 0.013 |
| [test-decompose_dtc.R](testthat/test-decompose_dtc.R#L96) | decompose_dtc | decompose_dtc preserves original data | PASS | 6 | 0.009 |
| [test-decompose_dtc.R](testthat/test-decompose_dtc.R#L121_L122) | decompose_dtc | decompose_dtc handles different time formats correctly | PASS | 2 | 0.005 |
| [test-decompose_dtc.R](testthat/test-decompose_dtc.R#L138) | decompose_dtc | decompose_dtc validates input parameters | ERROR | 0 | 0.005 |
| [test-df_to_string.R](testthat/test-df_to_string.R#L11) | df_to_string | df_to_string basic functionality works | PASS | 6 | 0.009 |
| [test-df_to_string.R](testthat/test-df_to_string.R#L31) | df_to_string | df_to_string handles empty data frames | PASS | 2 | 0.003 |
| [test-df_to_string.R](testthat/test-df_to_string.R#L48) | df_to_string | df_to_string respects n parameter | PASS | 1 | 0.002 |
| [test-df_to_string.R](testthat/test-df_to_string.R#L61) | df_to_string | df_to_string handles color formatting | PASS | 2 | 0.004 |
| [test-df_to_string.R](testthat/test-df_to_string.R#L74) | df_to_string | df_to_string handles NA values | PASS | 1 | 0.003 |
| [test-df_to_string.R](testthat/test-df_to_string.R#L90) | df_to_string | df_to_string maintains column alignment | PASS | 1 | 0.003 |
| [test-domain.R](testthat/test-domain.R#L26) | domain | domain() returns correct data frames for existing domains | PASS | 2 | 0.003 |
| [test-domain.R](testthat/test-domain.R#L41) | domain | domain() errors for non-existent domains | PASS | 3 | 0.009 |
| [test-domain.R](testthat/test-domain.R#L59) | domain | domain() is case-insensitive | PASS | 4 | 0.005 |
| [test-domain.R](testthat/test-domain.R#L77_L79) | domain | domain() handles input validation correctly | PASS | 5 | 0.015 |
| [test-domain.R](testthat/test-domain.R#L110_L112) | domain | domain() rejects vectors with multiple names | PASS | 1 | 0.004 |
| [test-edish_plot.R](testthat/test-edish_plot.R#L41) | edish_plot | edish_plot handles valid input correctly | PASS | 3 | 0.096 |
| [test-edish_plot.R](testthat/test-edish_plot.R#L48_L51) | edish_plot | edish_plot validates enzyme parameter | PASS | 1 | 0.006 |
| [test-edish_plot.R](testthat/test-edish_plot.R#L62_L65) | edish_plot | edish_plot handles missing required lab tests | PASS | 1 | 0.006 |
| [test-edish_plot.R](testthat/test-edish_plot.R#L75_L80) | edish_plot | edish_plot handles zero ULN values | PASS | 1 | 0.007 |
| [test-ensure_analyte.R](testthat/test-ensure_analyte.R#L17) | ensure_analyte | ensure_analyte creates ANALYTE from CMT when missing | PASS | 2 | 0.004 |
| [test-ensure_analyte.R](testthat/test-ensure_analyte.R#L36) | ensure_analyte | ensure_analyte preserves existing ANALYTE values | PASS | 1 | 0.003 |
| [test-ensure_analyte.R](testthat/test-ensure_analyte.R#L51) | ensure_analyte | ensure_analyte handles NA values in CMT | PASS | 3 | 0.005 |
| [test-ensure_analyte.R](testthat/test-ensure_analyte.R#L69) | ensure_analyte | ensure_analyte handles non-numeric CMT values | PASS | 1 | 0.003 |
| [test-ensure_analyte.R](testthat/test-ensure_analyte.R#L83) | ensure_analyte | ensure_analyte returns NIF object | PASS | 2 | 0.003 |
| [test-ensure_analyte.R](testthat/test-ensure_analyte.R#L96) | ensure_analyte | ensure_analyte errors on non-NIF input | PASS | 1 | 0.003 |
| [test-ensure_analyte.R](testthat/test-ensure_analyte.R#L108) | ensure_analyte | ensure_analyte errors when CMT is missing | PASS | 1 | 0.004 |
| [test-ensure_analyte.R](testthat/test-ensure_analyte.R#L118) | ensure_analyte | ensure_analyte handles empty data frame | PASS | 3 | 0.005 |
| [test-ensure_analyte.R](testthat/test-ensure_analyte.R#L136) | ensure_analyte | ensure_analyte preserves other columns | PASS | 3 | 0.004 |
| [test-ensure_dose.R](testthat/test-ensure_dose.R#L17) | ensure_dose | ensure_dose creates DOSE field correctly | PASS | 3 | 0.008 |
| [test-ensure_dose.R](testthat/test-ensure_dose.R#L34) | ensure_dose | ensure_dose handles existing DOSE field | PASS | 1 | 0.002 |
| [test-ensure_dose.R](testthat/test-ensure_dose.R#L51) | ensure_dose | ensure_dose handles multiple doses per subject | PASS | 1 | 0.006 |
| [test-ensure_dose.R](testthat/test-ensure_dose.R#L67) | ensure_dose | ensure_dose handles NA values correctly | PASS | 1 | 0.006 |
| [test-ensure_dose.R](testthat/test-ensure_dose.R#L78) | ensure_dose | ensure_dose handles empty data frame | PASS | 2 | 0.003 |
| [test-ensure_dose.R](testthat/test-ensure_dose.R#L91_L94) | ensure_dose | ensure_dose handles missing required columns | PASS | 1 | 0.004 |
| [test-ensure_dose.R](testthat/test-ensure_dose.R#L106_L109) | ensure_dose | ensure_dose handles non-NIF input | PASS | 1 | 0.003 |
| [test-ensure_dose.R](testthat/test-ensure_dose.R#L128) | ensure_dose | ensure_dose handles unsorted data correctly | PASS | 1 | 0.012 |
| [test-ensure_dose.R](testthat/test-ensure_dose.R#L144) | ensure_dose | ensure_dose handles zero doses correctly | PASS | 1 | 0.006 |
| [test-ensure_parent.R](testthat/test-ensure_parent.R#L18) | ensure_parent | ensure_parent() works correctly | PASS | 11 | 0.027 |
| [test-ensure_tad.R](testthat/test-ensure_tad.R#L17) | ensure_tad | ensure_tad works with basic input | PASS | 2 | 0.010 |
| [test-ensure_tad.R](testthat/test-ensure_tad.R#L37) | ensure_tad | ensure_tad handles multiple administrations | PASS | 1 | 0.008 |
| [test-ensure_tad.R](testthat/test-ensure_tad.R#L53) | ensure_tad | ensure_tad handles observations before first dose | PASS | 1 | 0.009 |
| [test-ensure_tad.R](testthat/test-ensure_tad.R#L71) | ensure_tad | ensure_tad handles multiple parent compounds | PASS | 1 | 0.008 |
| [test-ensure_tad.R](testthat/test-ensure_tad.R#L80_L83) | ensure_tad | ensure_tad handles empty data frame | PASS | 3 | 0.007 |
| [test-ensure_tad.R](testthat/test-ensure_tad.R#L97) | ensure_tad | ensure_tad handles missing required columns | PASS | 1 | 0.004 |
| [test-ensure_tad.R](testthat/test-ensure_tad.R#L112) | ensure_tad | ensure_tad preserves original data | PASS | 2 | 0.009 |
| [test-ensure_tad.R](testthat/test-ensure_tad.R#L128) | ensure_tad | ensure_tad handles NA values in TIME | PASS | 1 | 0.007 |
| [test-ensure_tad.R](testthat/test-ensure_tad.R#L141) | ensure_tad | ensure_tad returns a nif object | PASS | 1 | 0.007 |
| [test-ensure_tad.R](testthat/test-ensure_tad.R#L154) | ensure_tad | ensure_tad handles non-nif input | PASS | 1 | 0.004 |
| [test-ensure_tad.R](testthat/test-ensure_tad.R#L168) | ensure_tad | ensure_tad handles existing TAD column | PASS | 1 | 0.002 |
| [test-ensure_tafd.R](testthat/test-ensure_tafd.R#L17) | ensure_tafd | ensure_tafd works with basic input | PASS | 2 | 0.009 |
| [test-ensure_tafd.R](testthat/test-ensure_tafd.R#L37) | ensure_tafd | ensure_tafd handles multiple administrations | PASS | 1 | 0.013 |
| [test-ensure_tafd.R](testthat/test-ensure_tafd.R#L53) | ensure_tafd | ensure_tafd handles observations before first dose | PASS | 1 | 0.007 |
| [test-ensure_tafd.R](testthat/test-ensure_tafd.R#L71) | ensure_tafd | ensure_tafd handles multiple parent compounds | PASS | 1 | 0.006 |
| [test-ensure_tafd.R](testthat/test-ensure_tafd.R#L82) | ensure_tafd | ensure_tafd handles missing required columns | PASS | 1 | 0.004 |
| [test-ensure_tafd.R](testthat/test-ensure_tafd.R#L97) | ensure_tafd | ensure_tafd preserves original data | PASS | 2 | 0.008 |
| [test-ensure_tafd.R](testthat/test-ensure_tafd.R#L110) | ensure_tafd | ensure_tafd handles NA values in TIME | PASS | 2 | 0.007 |
| [test-ensure_tafd.R](testthat/test-ensure_tafd.R#L124) | ensure_tafd | ensure_tafd returns a nif object | PASS | 1 | 0.005 |
| [test-ensure_tafd.R](testthat/test-ensure_tafd.R#L137) | ensure_tafd | ensure_tafd handles non-nif input | PASS | 1 | 0.004 |
| [test-ensure_tafd.R](testthat/test-ensure_tafd.R#L151) | ensure_tafd | ensure_tafd handles existing TAFD column | PASS | 1 | 0.002 |
| [test-ensure_tafd.R](testthat/test-ensure_tafd.R#L162) | ensure_tafd | ensure_tafd handles non-numeric ID values | PASS | 1 | 0.004 |
| [test-ensure_tafd.R](testthat/test-ensure_tafd.R#L173) | ensure_tafd | ensure_tafd handles non-numeric EVID values | PASS | 1 | 0.004 |
| [test-ensure_tafd.R](testthat/test-ensure_tafd.R#L184) | ensure_tafd | ensure_tafd handles NA values in ID | PASS | 1 | 0.004 |
| [test-ensure_tafd.R](testthat/test-ensure_tafd.R#L196_L199) | ensure_tafd | ensure_tafd handles no dosing events | PASS | 1 | 0.005 |
| [test-ensure_tafd.R](testthat/test-ensure_tafd.R#L217) | ensure_tafd | ensure_tafd handles different first dose times | PASS | 1 | 0.007 |
| [test-ensure_time.R](testthat/test-ensure_time.R#L16) | ensure_time | ensure_time works with TIME, TAD, and TAFD already present | PASS | 1 | 0.002 |
| [test-ensure_time.R](testthat/test-ensure_time.R#L34) | ensure_time | ensure_time calculates TIME, TAD, and TAFD from DTC | PASS | 4 | 0.013 |
| [test-ensure_time.R](testthat/test-ensure_time.R#L61) | ensure_time | ensure_time calculates TIME, TAD, and TAFD from TIME | PASS | 3 | 0.013 |
| [test-ensure_time.R](testthat/test-ensure_time.R#L85) | ensure_time | ensure_time handles multiple dosing events | PASS | 3 | 0.014 |
| [test-ensure_time.R](testthat/test-ensure_time.R#L110) | ensure_time | ensure_time handles multiple parent compounds | PASS | 3 | 0.018 |
| [test-ensure_time.R](testthat/test-ensure_time.R#L134) | ensure_time | ensure_time handles observations before first dose | PASS | 3 | 0.014 |
| [test-ensure_time.R](testthat/test-ensure_time.R#L153_L155) | ensure_time | ensure_time handles missing required columns | PASS | 1 | 0.004 |
| [test-ensure_time.R](testthat/test-ensure_time.R#L171) | ensure_time | ensure_time preserves original data | PASS | 3 | 0.013 |
| [test-ensure_time.R](testthat/test-ensure_time.R#L186) | ensure_time | ensure_time returns a nif object | PASS | 1 | 0.011 |
| [test-find_duplicates.R](testthat/test-find_duplicates.R#L16) | find_duplicates | find_duplicates works with default fields | PASS | 5 | 0.009 |
| [test-find_duplicates.R](testthat/test-find_duplicates.R#L40) | find_duplicates | find_duplicates works with custom fields | PASS | 3 | 0.006 |
| [test-find_duplicates.R](testthat/test-find_duplicates.R#L58) | find_duplicates | find_duplicates returns count only when requested | PASS | 1 | 0.003 |
| [test-find_duplicates.R](testthat/test-find_duplicates.R#L72) | find_duplicates | find_duplicates returns NULL when no duplicates exist | PASS | 1 | 0.003 |
| [test-find_duplicates.R](testthat/test-find_duplicates.R#L90) | find_duplicates | find_duplicates works with return_all_cols = FALSE | PASS | 4 | 0.008 |
| [test-find_duplicates.R](testthat/test-find_duplicates.R#L108_L111) | find_duplicates | find_duplicates handles missing fields | ERROR | 0 | 0.004 |
| [test-find_duplicates.R](testthat/test-find_duplicates.R#L129) | find_duplicates | find_duplicates handles NA values | PASS | 3 | 0.007 |
| [test-find_duplicates.R](testthat/test-find_duplicates.R#L142) | find_duplicates | find_duplicates handles empty data frame | PASS | 1 | 0.003 |
| [test-formula_to_mapping.R](testthat/test-formula_to_mapping.R#L37) | formula_to_mapping | formula_to_mapping works with single analyte | PASS | 1 | 0.004 |
| [test-formula_to_mapping.R](testthat/test-formula_to_mapping.R#L51) | formula_to_mapping | formula_to_mapping works with multiple analytes | PASS | 1 | 0.004 |
| [test-formula_to_mapping.R](testthat/test-formula_to_mapping.R#L58_L61) | formula_to_mapping | formula_to_mapping handles non-matching treatments | PASS | 1 | 0.004 |
| [test-formula_to_mapping.R](testthat/test-formula_to_mapping.R#L68_L72) | formula_to_mapping | formula_to_mapping handles non-matching analytes | PASS | 2 | 0.008 |
| [test-formula_to_mapping.R](testthat/test-formula_to_mapping.R#L79_L82) | formula_to_mapping | formula_to_mapping throws error for non-sdtm input | PASS | 1 | 0.004 |
| [test-formula_to_mapping.R](testthat/test-formula_to_mapping.R#L87_L90) | formula_to_mapping | formula_to_mapping throws error for non-formula input | PASS | 1 | 0.004 |
| [test-geom_admin.R](testthat/test-geom_admin.R#L15) | geom_admin | geom_admin works with valid inputs | PASS | 2 | 0.011 |
| [test-geom_admin.R](testthat/test-geom_admin.R#L39_L42) | geom_admin | geom_admin validates parameters correctly | PASS | 8 | 0.023 |
| [test-geom_admin.R](testthat/test-geom_admin.R#L119) | geom_admin | geom_admin handles different data types | PASS | 2 | 0.004 |
| [test-geom_admin.R](testthat/test-geom_admin.R#L144) | geom_admin | geom_admin handles NA values | PASS | 1 | 0.002 |
| [test-geom_admin.R](testthat/test-geom_admin.R#L161) | geom_admin | geom_admin works with different linetype values | PASS | 2 | 0.003 |
| [test-geom_admin.R](testthat/test-geom_admin.R#L186) | geom_admin | geom_admin works with different color values | PASS | 2 | 0.003 |
| [test-geom_admin.R](testthat/test-geom_admin.R#L205) | geom_admin | geom_admin works with empty data | PASS | 1 | 0.002 |
| [test-ggplot_extension.R](testthat/test-ggplot_extension.R#L2) | ggplot_extension | multiplication works | PASS | 1 | 0.002 |
| [test-guess_ntime.R](testthat/test-guess_ntime.R#L26) | guess_ntime | guess_ntime correctly parses various time formats | PASS | 3 | 0.006 |
| [test-guess_ntime.R](testthat/test-guess_ntime.R#L50_L53) | guess_ntime | guess_ntime handles ISO 8601 dates with a warning | PASS | 5 | 0.009 |
| [test-guess_ntime.R](testthat/test-guess_ntime.R#L71_L74) | guess_ntime | guess_ntime errors on missing PC domain | PASS | 1 | 0.004 |
| [test-guess_ntime.R](testthat/test-guess_ntime.R#L92_L95) | guess_ntime | guess_ntime errors on missing PCTPT column | PASS | 1 | 0.004 |
| [test-guess_ntime.R](testthat/test-guess_ntime.R#L120) | guess_ntime | guess_ntime handles additional predose variations | PASS | 4 | 0.006 |
| [test-guess_parent.R](testthat/test-guess_parent.R#L17) | guess_parent | guess_parent identifies analyte with most administrations | PASS | 1 | 0.004 |
| [test-guess_parent.R](testthat/test-guess_parent.R#L37) | guess_parent | guess_parent falls back to observations when no administrations exist | PASS | 1 | 0.006 |
| [test-guess_parent.R](testthat/test-guess_parent.R#L58) | guess_parent | guess_parent ignores metabolite observations | PASS | 1 | 0.005 |
| [test-guess_parent.R](testthat/test-guess_parent.R#L81) | guess_parent | guess_parent prioritizes administrations over observations | PASS | 1 | 0.004 |
| [test-guess_parent.R](testthat/test-guess_parent.R#L93) | guess_parent | guess_parent returns NULL for empty dataset | PASS | 1 | 0.006 |
| [test-guess_parent.R](testthat/test-guess_parent.R#L108) | guess_parent | guess_parent works with minimal dataset | PASS | 1 | 0.004 |
| [test-guess_parent.R](testthat/test-guess_parent.R#L122) | guess_parent | guess_parent handles tied administration counts | PASS | 1 | 0.004 |
| [test-guess_parent.R](testthat/test-guess_parent.R#L139) | guess_parent | guess_parent works with ensure_analyte | PASS | 1 | 0.005 |
| [test-guess_parent.R](testthat/test-guess_parent.R#L153) | guess_parent | guess_parent returns NULL for dataset with only metabolite observations | PASS | 1 | 0.005 |
| [test-guess_parent.R](testthat/test-guess_parent.R#L169) | guess_parent | guess_parent handles NA values in key columns | PASS | 1 | 0.010 |
| [test-guess_parent.R](testthat/test-guess_parent.R#L186) | guess_parent | guess_parent correctly counts tied observations when no administrations exist | PASS | 1 | 0.006 |
| [test-guess_pcspec.R](testthat/test-guess_pcspec.R#L4) | guess_pcspec | guess_pcspec works correctly | PASS | 8 | 0.011 |
| [test-guess_pcspec.R](testthat/test-guess_pcspec.R#L33_L34) | guess_pcspec | guess_pcspec handles errors correctly | PASS | 5 | 0.013 |
| [test-guess_pcspec.R](testthat/test-guess_pcspec.R#L59) | guess_pcspec | guess_pcspec maintains data frame attributes | PASS | 2 | 0.002 |
| [test-has_domain.R](testthat/test-has_domain.R#L15) | has_domain | has_domain correctly identifies existing domains | PASS | 6 | 0.007 |
| [test-has_domain.R](testthat/test-has_domain.R#L36) | has_domain | has_domain is case-insensitive | PASS | 4 | 0.005 |
| [test-has_domain.R](testthat/test-has_domain.R#L54) | has_domain | has_domain handles input validation correctly | PASS | 5 | 0.015 |
| [test-has_domain.R](testthat/test-has_domain.R#L78) | has_domain | has_domain handles multiple domain names correctly | PASS | 5 | 0.006 |
| [test-has_domain.R](testthat/test-has_domain.R#L99_L101) | has_domain | domain() rejects vectors with multiple names | PASS | 1 | 0.003 |
| [test-has_domain.R](testthat/test-has_domain.R#L112) | has_domain | has_domain works with example data | PASS | 1 | 0.004 |
| [test-has_domain.R](testthat/test-has_domain.R#L128) | has_domain | domain function behaviors | PASS | 3 | 0.002 |
| [test-import_from_connection.R](testthat/test-import_from_connection.R#L21) | import_from_connection | import_from_connection handles CSV data correctly | PASS | 6 | 0.010 |
| [test-import_from_connection.R](testthat/test-import_from_connection.R#L50) | import_from_connection | import_from_connection handles fixed-width data correctly | PASS | 6 | 0.009 |
| [test-import_from_connection.R](testthat/test-import_from_connection.R#L76) | import_from_connection | import_from_connection auto-detects CSV format | PASS | 2 | 0.005 |
| [test-import_from_connection.R](testthat/test-import_from_connection.R#L98) | import_from_connection | import_from_connection auto-detects fixed-width format | PASS | 2 | 0.004 |
| [test-import_from_connection.R](testthat/test-import_from_connection.R#L126) | import_from_connection | import_from_connection ignores comments and empty lines | PASS | 2 | 0.004 |
| [test-import_from_connection.R](testthat/test-import_from_connection.R#L149) | import_from_connection | import_from_connection handles datetime conversion | PASS | 3 | 0.006 |
| [test-import_from_connection.R](testthat/test-import_from_connection.R#L173) | import_from_connection | import_from_connection handles custom delimiters | PASS | 3 | 0.006 |
| [test-import_from_connection.R](testthat/test-import_from_connection.R#L196) | import_from_connection | import_from_connection adds missing fields | PASS | 3 | 0.005 |
| [test-import_from_connection.R](testthat/test-import_from_connection.R#L221) | import_from_connection | import_from_connection respects no_numeric parameter | PASS | 3 | 0.012 |
| [test-import_from_connection.R](testthat/test-import_from_connection.R#L229) | import_from_connection | import_from_connection handles errors correctly | PASS | 3 | 0.010 |
| [test-import_from_connection.R](testthat/test-import_from_connection.R#L255_L258) | import_from_connection | import_from_connection detects invalid fixed-width format | PASS | 1 | 0.004 |
| [test-import_from_connection.R](testthat/test-import_from_connection.R#L276_L279) | import_from_connection | import_from_connection detects inconsistent CSV format | PASS | 1 | 0.004 |
| [test-import_nif.R](testthat/test-import_nif.R#L10) | import_nif | import_nif loads CSV files correctly | PASS | 6 | 0.009 |
| [test-import_nif.R](testthat/test-import_nif.R#L28) | import_nif | import_nif loads fixed-width files correctly | PASS | 6 | 0.009 |
| [test-import_nif.R](testthat/test-import_nif.R#L46) | import_nif | import_nif automatically detects CSV format | PASS | 2 | 0.005 |
| [test-import_nif.R](testthat/test-import_nif.R#L60) | import_nif | import_nif automatically detects fixed-width format | PASS | 2 | 0.004 |
| [test-import_nif.R](testthat/test-import_nif.R#L74) | import_nif | import_nif handles custom delimiter | PASS | 2 | 0.004 |
| [test-import_nif.R](testthat/test-import_nif.R#L88) | import_nif | import_nif handles no_numeric parameter correctly | PASS | 2 | 0.005 |
| [test-import_nif.R](testthat/test-import_nif.R#L105) | import_nif | import_nif handles date/time conversion | PASS | 2 | 0.004 |
| [test-import_nif.R](testthat/test-import_nif.R#L119) | import_nif | import_nif handles comments and empty lines | PASS | 2 | 0.004 |
| [test-import_nif.R](testthat/test-import_nif.R#L125_L128) | import_nif | import_nif errors on file not found | PASS | 1 | 0.003 |
| [test-import_nif.R](testthat/test-import_nif.R#L148) | import_nif | import_nif renames columns correctly | PASS | 5 | 0.008 |
| [test-import_observation.R](testthat/test-import_observation.R#L48_L56) | import_observation | import_observation validates input parameters correctly | PASS | 4 | 0.022 |
| [test-import_observation.R](testthat/test-import_observation.R#L131) | import_observation | import_observation correctly handles DTC field | PASS | 7 | 0.038 |
| [test-import_observation.R](testthat/test-import_observation.R#L165) | import_observation | import_observation correctly handles NTIME field | PASS | 7 | 0.033 |
| [test-import_observation.R](testthat/test-import_observation.R#L201_L214) | import_observation | import_observation automatically assigns compartment when cmt is NULL | PASS | 2 | 0.028 |
| [test-import_observation.R](testthat/test-import_observation.R#L228_L240) | import_observation | import_observation automatically determines parent when parent is NULL | PASS | 4 | 0.052 |
| [test-import_observation.R](testthat/test-import_observation.R#L297) | import_observation | import_observation correctly joins subject data | PASS | 4 | 0.034 |
| [test-import_observation.R](testthat/test-import_observation.R#L334) | import_observation | import_observation correctly sets debug fields | PASS | 3 | 0.046 |
| [test-imputations.R](testthat/test-imputations.R#L33) | imputations | impute_admin_times_from_pcrftdtc works | PASS | 1 | 0.011 |
| [test-imputations.R](testthat/test-imputations.R#L60) | imputations | impute_admin_times_from_pcrftdtc handles missing data | PASS | 1 | 0.010 |
| [test-imputations.R](testthat/test-imputations.R#L86) | imputations | impute_admin_times_from_pcrftdtc handles date mismatches | PASS | 1 | 0.011 |
| [test-imputations.R](testthat/test-imputations.R#L114_L117) | imputations | impute_admin_times_from_pcrftdtc preserves existing times | PASS | 2 | 0.022 |
| [test-impute_exendtc_to_cutoff.R](testthat/test-impute_exendtc_to_cutoff.R#L42) | impute_exendtc_to_cutoff | impute_exendtc_to_cutoff works correctly | PASS | 4 | 0.044 |
| [test-impute_exendtc_to_rfendtc.R](testthat/test-impute_exendtc_to_rfendtc.R#L12_L15) | impute_exendtc_to_rfendtc | impute_exendtc_to_rfendtc validates input correctly | PASS | 2 | 0.009 |
| [test-impute_exendtc_to_rfendtc.R](testthat/test-impute_exendtc_to_rfendtc.R#L53) | impute_exendtc_to_rfendtc | impute_exendtc_to_rfendtc adds IMPUTATION column if not present | PASS | 1 | 0.008 |
| [test-impute_exendtc_to_rfendtc.R](testthat/test-impute_exendtc_to_rfendtc.R#L79) | impute_exendtc_to_rfendtc | impute_exendtc_to_rfendtc performs imputations correctly | PASS | 4 | 0.013 |
| [test-impute_exendtc_to_rfendtc.R](testthat/test-impute_exendtc_to_rfendtc.R#L106) | impute_exendtc_to_rfendtc | impute_exendtc_to_rfendtc does not impute non-last administrations | PASS | 4 | 0.012 |
| [test-impute_exendtc_to_rfendtc.R](testthat/test-impute_exendtc_to_rfendtc.R#L134) | impute_exendtc_to_rfendtc | impute_exendtc_to_rfendtc handles multiple treatments per subject correctly | PASS | 4 | 0.013 |
| [test-impute_exendtc_to_rfendtc.R](testthat/test-impute_exendtc_to_rfendtc.R#L162) | impute_exendtc_to_rfendtc | impute_exendtc_to_rfendtc returns unmodified data when no imputations needed | PASS | 2 | 0.010 |
| [test-impute_exendtc_to_rfendtc.R](testthat/test-impute_exendtc_to_rfendtc.R#L184) | impute_exendtc_to_rfendtc | impute_exendtc_to_rfendtc handles case with no RFENDTC in DM | PASS | 2 | 0.009 |
| [test-impute_missing_exendtc.R](testthat/test-impute_missing_exendtc.R#L8_L11) | impute_missing_exendtc | impute_missing_exendtc validates input correctly | PASS | 1 | 0.010 |
| [test-impute_missing_exendtc.R](testthat/test-impute_missing_exendtc.R#L32) | impute_missing_exendtc | impute_missing_exendtc handles basic imputation correctly | PASS | 2 | 0.011 |
| [test-impute_missing_exendtc.R](testthat/test-impute_missing_exendtc.R#L49) | impute_missing_exendtc | impute_missing_exendtc does not modify data when no imputations needed | PASS | 1 | 0.007 |
| [test-impute_missing_exendtc.R](testthat/test-impute_missing_exendtc.R#L68) | impute_missing_exendtc | impute_missing_exendtc creates IMPUTATION column if missing | PASS | 2 | 0.010 |
| [test-impute_missing_exendtc.R](testthat/test-impute_missing_exendtc.R#L92) | impute_missing_exendtc | impute_missing_exendtc does not impute last administrations | PASS | 3 | 0.011 |
| [test-impute_missing_exendtc.R](testthat/test-impute_missing_exendtc.R#L123) | impute_missing_exendtc | impute_missing_exendtc handles multiple subjects and treatments | PASS | 3 | 0.011 |
| [test-impute_missing_exendtc.R](testthat/test-impute_missing_exendtc.R#L150) | impute_missing_exendtc | impute_missing_exendtc preserves existing data and columns | PASS | 4 | 0.012 |
| [test-is_valid_filter.R](testthat/test-is_valid_filter.R#L12) | is_valid_filter | is_valid_filter works with basic filters | PASS | 8 | 0.010 |
| [test-is_valid_filter.R](testthat/test-is_valid_filter.R#L36) | is_valid_filter | is_valid_filter works with NA values | PASS | 5 | 0.014 |
| [test-is_valid_filter.R](testthat/test-is_valid_filter.R#L60) | is_valid_filter | is_valid_filter works with date columns | PASS | 4 | 0.013 |
| [test-is_valid_filter.R](testthat/test-is_valid_filter.R#L80) | is_valid_filter | is_valid_filter works with complex expressions | PASS | 6 | 0.008 |
| [test-is_valid_filter.R](testthat/test-is_valid_filter.R#L98) | is_valid_filter | is_valid_filter handles empty data frames | PASS | 5 | 0.008 |
| [test-lbm_boer.R](testthat/test-lbm_boer.R#L3) | lbm_boer | lbm_boer calculates correct values for valid inputs | PASS | 6 | 0.008 |
| [test-lbm_boer.R](testthat/test-lbm_boer.R#L16) | lbm_boer | lbm_boer handles NA inputs correctly | PASS | 7 | 0.007 |
| [test-lbm_boer.R](testthat/test-lbm_boer.R#L27) | lbm_boer | lbm_boer handles invalid numeric inputs correctly | PASS | 6 | 0.010 |
| [test-lbm_boer.R](testthat/test-lbm_boer.R#L39) | lbm_boer | lbm_boer handles invalid sex inputs correctly | PASS | 4 | 0.005 |
| [test-lbm_boer.R](testthat/test-lbm_boer.R#L50) | lbm_boer | lbm_boer produces consistent results for same inputs | PASS | 3 | 0.004 |
| [test-lbm_hume.R](testthat/test-lbm_hume.R#L5_L9) | lbm_hume | lbm_hume calculates correct values for valid inputs | PASS | 4 | 0.006 |
| [test-lbm_hume.R](testthat/test-lbm_hume.R#L32) | lbm_hume | lbm_hume handles edge cases correctly | PASS | 7 | 0.008 |
| [test-lbm_hume.R](testthat/test-lbm_hume.R#L46_L49) | lbm_hume | lbm_hume handles different sex input formats | PASS | 6 | 0.007 |
| [test-lbm_hume.R](testthat/test-lbm_hume.R#L82) | lbm_hume | lbm_hume maintains consistency with other LBM formulas | PASS | 2 | 0.003 |
| [test-lbm_peters.R](testthat/test-lbm_peters.R#L3_L7) | lbm_peters | lbm_peters calculates lean body mass correctly | PASS | 4 | 0.004 |
| [test-lbm_peters.R](testthat/test-lbm_peters.R#L34) | lbm_peters | lbm_peters handles edge cases | PASS | 9 | 0.009 |
| [test-lbm_peters.R](testthat/test-lbm_peters.R#L62_L66) | lbm_peters | lbm_peters handles vectorized inputs correctly | PASS | 4 | 0.004 |
| [test-lubrify_dates.R](testthat/test-lubrify_dates.R#L17) | lubrify_dates | lubrify_dates converts DTC columns to POSIXct when col is NULL | PASS | 6 | 0.006 |
| [test-lubrify_dates.R](testthat/test-lubrify_dates.R#L36) | lubrify_dates | lubrify_dates converts specified columns when col parameter is provided | ERROR | 0 | 0.003 |
| [test-lubrify_dates.R](testthat/test-lubrify_dates.R#L52) | lubrify_dates | lubrify_dates handles empty data frame | PASS | 3 | 0.004 |
| [test-lubrify_dates.R](testthat/test-lubrify_dates.R#L67) | lubrify_dates | lubrify_dates handles data frame with no DTC columns | PASS | 4 | 0.004 |
| [test-lubrify_dates.R](testthat/test-lubrify_dates.R#L83) | lubrify_dates | lubrify_dates handles NA values in DTC columns | PASS | 4 | 0.005 |
| [test-lubrify_dates.R](testthat/test-lubrify_dates.R#L101) | lubrify_dates | lubrify_dates handles all supported DTC formats | PASS | 4 | 0.004 |
| [test-lubrify_dates.R](testthat/test-lubrify_dates.R#L108) | lubrify_dates | lubrify_dates throws error for non-data frame input | ERROR | 0 | 0.004 |
| [test-lubrify_dates.R](testthat/test-lubrify_dates.R#L120_L123) | lubrify_dates | lubrify_dates throws error for missing columns when col is specified | ERROR | 0 | 0.003 |
| [test-lubrify_dates.R](testthat/test-lubrify_dates.R#L137) | lubrify_dates | lubrify_dates handles single column specification | ERROR | 0 | 0.003 |
| [test-lubrify_dates.R](testthat/test-lubrify_dates.R#L153) | lubrify_dates | lubrify_dates handles mixed column types correctly | PASS | 5 | 0.004 |
| [test-lubrify_dates.R](testthat/test-lubrify_dates.R#L172) | lubrify_dates | lubrify_dates preserves data frame structure | PASS | 3 | 0.005 |
| [test-lubrify_dates.R](testthat/test-lubrify_dates.R#L189) | lubrify_dates | lubrify_dates handles edge case with single row | PASS | 3 | 0.004 |
| [test-lubrify_dates.R](testthat/test-lubrify_dates.R#L203) | lubrify_dates | lubrify_dates handles edge case with single column | PASS | 2 | 0.003 |
| [test-lubrify_dates.R](testthat/test-lubrify_dates.R#L216) | lubrify_dates | lubrify_dates works with tibble input | PASS | 3 | 0.003 |
| [test-make_administration.R](testthat/test-make_administration.R#L2_L4) | make_administration | make_administration works for examplinib_poc | PASS | 1 | 0.228 |
| [test-make_administration.R](testthat/test-make_administration.R#L77_L79) | make_administration | make_administration works without pc | PASS | 3 | 0.045 |
| [test-make_administration.R](testthat/test-make_administration.R#L124_L129) | make_administration | make_administration imputes missing last EXENDTC | PASS | 2 | 0.049 |
| [test-make_ae.R](testthat/test-make_ae.R#L35) | make_ae | make_ae handles basic case correctly | PASS | 5 | 0.026 |
| [test-make_ae.R](testthat/test-make_ae.R#L71) | make_ae | make_ae handles different ae_fields correctly | PASS | 4 | 0.031 |
| [test-make_ae.R](testthat/test-make_ae.R#L106) | make_ae | make_ae handles filters correctly | PASS | 3 | 0.030 |
| [test-make_ae.R](testthat/test-make_ae.R#L140) | make_ae | make_ae handles missing data correctly | PASS | 1 | 0.015 |
| [test-make_ae.R](testthat/test-make_ae.R#L149_L150) | make_ae | make_ae handles errors appropriately | PASS | 2 | 0.019 |
| [test-make_ae.R](testthat/test-make_ae.R#L196) | make_ae | make_ae handles compartment and parent parameters correctly | PASS | 2 | 0.023 |
| [test-make_ae.R](testthat/test-make_ae.R#L221) | make_ae | make_ae preserves specified columns with keep parameter | PASS | 2 | 0.016 |
| [test-make_nif.R](testthat/test-make_nif.R#L52) | make_nif | date conversion works correctly | PASS | 1 | 0.003 |
| [test-make_nif.R](testthat/test-make_nif.R#L90_L97) | make_nif | impute_exendtc_to_rfendtc works as intended | PASS | 1 | 0.010 |
| [test-make_nif.R](testthat/test-make_nif.R#L114_L118) | make_nif | impute_exendtc_to_rfendtc works correctly | PASS | 3 | 0.014 |
| [test-make_nif.R](testthat/test-make_nif.R#L173) | make_nif | impute_missing_exendtc | PASS | 1 | 0.011 |
| [test-make_nif.R](testthat/test-make_nif.R#L196_L202) | make_nif | impute_exendtc_to_cutoff works | PASS | 2 | 0.011 |
| [test-make_nif.R](testthat/test-make_nif.R#L222) | make_nif | filter_EXSTDTC_after_EXENDTC works | PASS | 2 | 0.008 |
| [test-make_nif.R](testthat/test-make_nif.R#L248_L249) | make_nif | make_nif | PASS | 1 | 0.189 |
| [test-make_nif.R](testthat/test-make_nif.R#L271) | make_nif | make_time | PASS | 1 | 0.011 |
| [test-make_nif.R](testthat/test-make_nif.R#L389_L395) | make_nif | add_administration, add_observation | PASS | 2 | 0.158 |
| [test-make_nif.R](testthat/test-make_nif.R#L413_L429) | make_nif | import_observation | PASS | 1 | 0.031 |
| [test-make_nif.R](testthat/test-make_nif.R#L434_L446) | make_nif | make_nif integration works | PASS | 1 | 0.433 |
| [test-make_nif.R](testthat/test-make_nif.R#L451_L452) | make_nif | guess_pcspec works | PASS | 3 | 0.004 |
| [test-make_nif.R](testthat/test-make_nif.R#L461_L462) | make_nif | guess_lbspec works | PASS | 2 | 0.002 |
| [test-make_nif.R](testthat/test-make_nif.R#L477) | make_nif | add_time works | PASS | 1 | 0.004 |
| [test-make_nif.R](testthat/test-make_nif.R#L506_L510) | make_nif | limit works | PASS | 3 | 0.012 |
| [test-make_ntime_from_tpt.R](testthat/test-make_ntime_from_tpt.R#L18_L21) | make_ntime_from_tpt | make_ntime_from_tpt works with generic PCTPT | PASS | 1 | 0.003 |
| [test-make_ntime_from_tpt.R](testthat/test-make_ntime_from_tpt.R#L47_L50) | make_ntime_from_tpt | make_ntime_from_tpt works with irregular hour unit | PASS | 1 | 0.003 |
| [test-make_ntime_from_tpt.R](testthat/test-make_ntime_from_tpt.R#L69_L72) | make_ntime_from_tpt | make_ntime_from_tpt works with minutes | PASS | 1 | 0.002 |
| [test-make_ntime_from_tpt.R](testthat/test-make_ntime_from_tpt.R#L92_L95) | make_ntime_from_tpt | make_ntime_from_tpt works with day information | PASS | 1 | 0.002 |
| [test-make_ntime.R](testthat/test-make_ntime.R#L16) | make_ntime | make_ntime returns lookup table for valid input | PASS | 4 | 0.008 |
| [test-make_ntime.R](testthat/test-make_ntime.R#L36) | make_ntime | make_ntime handles include_day parameter correctly | PASS | 2 | 0.009 |
| [test-make_ntime.R](testthat/test-make_ntime.R#L55) | make_ntime | make_ntime returns NULL when no ELTM field is present | PASS | 3 | 0.008 |
| [test-make_ntime.R](testthat/test-make_ntime.R#L75) | make_ntime | make_ntime handles missing DY column correctly | PASS | 2 | 0.009 |
| [test-make_ntime.R](testthat/test-make_ntime.R#L94) | make_ntime | make_ntime handles NA values correctly | PASS | 4 | 0.011 |
| [test-make_ntime.R](testthat/test-make_ntime.R#L116) | make_ntime | make_ntime correctly converts ISO 8601 formatted durations | PASS | 1 | 0.004 |
| [test-make_observation.R](testthat/test-make_observation.R#L58_L60) | make_observation | make_observation works | PASS | 1 | 0.019 |
| [test-make_observation.R](testthat/test-make_observation.R#L67_L71) | make_observation | make_observation issues warning if observation filter returns no observations | PASS | 1 | 0.024 |
| [test-make_observation.R](testthat/test-make_observation.R#L77_L91) | make_observation | make_observation works with coding table | PASS | 1 | 0.021 |
| [test-make_observation.R](testthat/test-make_observation.R#L102) | make_observation | make_observation handles different domains correctly | PASS | 2 | 0.035 |
| [test-make_observation.R](testthat/test-make_observation.R#L117) | make_observation | make_observation applies DV factor correctly | PASS | 2 | 0.040 |
| [test-make_observation.R](testthat/test-make_observation.R#L134) | make_observation | make_observation handles custom DV_field correctly | PASS | 1 | 0.020 |
| [test-make_observation.R](testthat/test-make_observation.R#L153) | make_observation | make_observation handles custom TESTCD_field correctly | PASS | 2 | 0.026 |
| [test-make_observation.R](testthat/test-make_observation.R#L168) | make_observation | make_observation handles custom DTC_field correctly | PASS | 1 | 0.019 |
| [test-make_observation.R](testthat/test-make_observation.R#L180) | make_observation | make_observation creates proper output fields | PASS | 13 | 0.033 |
| [test-make_observation.R](testthat/test-make_observation.R#L209) | make_observation | make_observation handles NTIME lookup correctly | PASS | 1 | 0.019 |
| [test-make_observation.R](testthat/test-make_observation.R#L217_L221) | make_observation | make_observation validates inputs correctly | PASS | 2 | 0.007 |
| [test-make_observation.R](testthat/test-make_observation.R#L242_L246) | make_observation | make_observation handles missing DV field with coding table | PASS | 3 | 0.034 |
| [test-make_observation.R](testthat/test-make_observation.R#L275) | make_observation | make_observation sets MDV correctly for missing values | PASS | 2 | 0.027 |
| [test-make_observation.R](testthat/test-make_observation.R#L289_L293) | make_observation | add_observation basic functionality works | PASS | 3 | 0.165 |
| [test-make_observation.R](testthat/test-make_observation.R#L347) | make_observation | make_observation works with ntime_method = ‘TPT’ | PASS | 7 | 0.022 |
| [test-make_observation.R](testthat/test-make_observation.R#L395) | make_observation | make_observation handles missing NTIME values | PASS | 2 | 0.018 |
| [test-make_observation.R](testthat/test-make_observation.R#L439) | make_observation | make_observation handles different time point formats | PASS | 2 | 0.018 |
| [test-make_plot_data_set.R](testthat/test-make_plot_data_set.R#L15) | make_plot_data_set | make_plot_data_set handles basic functionality | PASS | 5 | 0.028 |
| [test-make_plot_data_set.R](testthat/test-make_plot_data_set.R#L36) | make_plot_data_set | make_plot_data_set handles time parameters correctly | PASS | 4 | 0.068 |
| [test-make_plot_data_set.R](testthat/test-make_plot_data_set.R#L55) | make_plot_data_set | make_plot_data_set handles color and facet parameters | PASS | 6 | 0.061 |
| [test-make_plot_data_set.R](testthat/test-make_plot_data_set.R#L79) | make_plot_data_set | make_plot_data_set handles cfb and dose_norm parameters | PASS | 2 | 0.034 |
| [test-make_plot_data_set.R](testthat/test-make_plot_data_set.R#L97) | make_plot_data_set | make_plot_data_set handles min_time and max_time parameters | PASS | 2 | 0.020 |
| [test-make_plot_data_set.R](testthat/test-make_plot_data_set.R#L112) | make_plot_data_set | make_plot_data_set handles multiple analytes | PASS | 2 | 0.019 |
| [test-make_plot_data_set.R](testthat/test-make_plot_data_set.R#L124) | make_plot_data_set | make_plot_data_set handles invalid inputs | PASS | 2 | 0.008 |
| [test-make_subjects_sdtm.R](testthat/test-make_subjects_sdtm.R#L30) | make_subjects_sdtm | make_subjects_sdtm creates a proper subject data frame | PASS | 7 | 0.017 |
| [test-make_subjects_sdtm.R](testthat/test-make_subjects_sdtm.R#L71) | make_subjects_sdtm | make_subjects_sdtm works with missing height/weight | PASS | 4 | 0.019 |
| [test-make_subjects_sdtm.R](testthat/test-make_subjects_sdtm.R#L97) | make_subjects_sdtm | make_subjects_sdtm works with example data | PASS | 3 | 0.010 |
| [test-nca_from_pp.R](testthat/test-nca_from_pp.R#L23) | nca_from_pp | nca_from_pp works with valid inputs | PASS | 6 | 0.015 |
| [test-nca_from_pp.R](testthat/test-nca_from_pp.R#L65) | nca_from_pp | nca_from_pp handles missing analyte | PASS | 2 | 0.005 |
| [test-nca_from_pp.R](testthat/test-nca_from_pp.R#L90) | nca_from_pp | nca_from_pp handles errors appropriately | PASS | 3 | 0.010 |
| [test-nca_from_pp.R](testthat/test-nca_from_pp.R#L126_L131) | nca_from_pp | nca_from_pp handles empty results | PASS | 1 | 0.006 |
| [test-nca_from_pp.R](testthat/test-nca_from_pp.R#L159) | nca_from_pp | nca_from_pp handles keep parameter correctly | PASS | 2 | 0.008 |
| [test-nca.R](testthat/test-nca.R#L3) | nca | nca() input validation | PASS | 2 | 0.007 |
| [test-nca.R](testthat/test-nca.R#L39_L41) | nca | nca() analyte handling | PASS | 2 | 0.056 |
| [test-nca.R](testthat/test-nca.R#L75_L78) | nca | nca() grouping functionality | PASS | 6 | 0.081 |
| [test-nca.R](testthat/test-nca.R#L107_L111) | nca | nca() time handling | PASS | 2 | 0.054 |
| [test-nca.R](testthat/test-nca.R#L147_L149) | nca | nca() duplicate handling | PASS | 2 | 0.053 |
| [test-nca.R](testthat/test-nca.R#L223) | nca | nca works with the whale data set | PASS | 1 | 0.083 |
| [test-nif_add_baseline.R](testthat/test-nif_add_baseline.R#L2) | nif_add_baseline | multiplication works | PASS | 1 | 0.002 |
| [test-nif_administrations.R](testthat/test-nif_administrations.R#L3) | nif_administrations | date_list works | PASS | 8 | 0.010 |
| [test-nif_administrations.R](testthat/test-nif_administrations.R#L26) | nif_administrations | expand_ex works in general | PASS | 2 | 0.013 |
| [test-nif_administrations.R](testthat/test-nif_administrations.R#L51) | nif_administrations | expand_ex works with TRTDY | PASS | 2 | 0.013 |
| [test-nif_administrations.R](testthat/test-nif_administrations.R#L69) | nif_administrations | expand_ex works with missing EXENDTC | PASS | 2 | 0.012 |
| [test-nif_administrations.R](testthat/test-nif_administrations.R#L90) | nif_administrations | expand_ex errs when end date before start date | PASS | 1 | 0.014 |
| [test-nif_administrations.R](testthat/test-nif_administrations.R#L100) | nif_administrations | expand_ex errs when end day before start day | PASS | 1 | 0.015 |
| [test-nif_ae.R](testthat/test-nif_ae.R#L2) | nif_ae | multiplication works | PASS | 1 | 0.002 |
| [test-nif_auto.R](testthat/test-nif_auto.R#L37) | nif_auto | nif_auto works with basic input | PASS | 2 | 0.107 |
| [test-nif_auto.R](testthat/test-nif_auto.R#L68) | nif_auto | nif_auto handles metabolites correctly | PASS | 2 | 0.137 |
| [test-nif_auto.R](testthat/test-nif_auto.R#L105) | nif_auto | nif_auto adds baseline covariates when available | PASS | 1 | 0.125 |
| [test-nif_auto.R](testthat/test-nif_auto.R#L143) | nif_auto | nif_auto handles subject filtering correctly | PASS | 2 | 0.209 |
| [test-nif_auto.R](testthat/test-nif_auto.R#L184) | nif_auto | nif_auto handles duplicates correctly | PASS | 4 | 0.212 |
| [test-nif_class.R](testthat/test-nif_class.R#L2) | nif_class | new_nif works | PASS | 2 | 0.186 |
| [test-nif_class.R](testthat/test-nif_class.R#L8) | nif_class | subject_info works | PASS | 2 | 0.019 |
| [test-nif_class.R](testthat/test-nif_class.R#L15) | nif_class | parents works | PASS | 1 | 0.003 |
| [test-nif_class.R](testthat/test-nif_class.R#L20) | nif_class | dose_red_sbs works | PASS | 1 | 0.015 |
| [test-nif_class.R](testthat/test-nif_class.R#L25_L26) | nif_class | rich_sampling_sbs works | PASS | 1 | 0.011 |
| [test-nif_class.R](testthat/test-nif_class.R#L31) | nif_class | studies works | PASS | 1 | 0.002 |
| [test-nif_class.R](testthat/test-nif_class.R#L36) | nif_class | ensure works | PASS | 8 | 0.050 |
| [test-nif_class.R](testthat/test-nif_class.R#L48) | nif_class | doses works | PASS | 1 | 0.002 |
| [test-nif_class.R](testthat/test-nif_class.R#L53) | nif_class | doses works with minimal NIF | PASS | 1 | 0.002 |
| [test-nif_class.R](testthat/test-nif_class.R#L57) | nif_class | dose_levels works | PASS | 1 | 0.009 |
| [test-nif_class.R](testthat/test-nif_class.R#L62) | nif_class | treatments works | PASS | 2 | 0.006 |
| [test-nif_class.R](testthat/test-nif_class.R#L68) | nif_class | dose_levels works with minimal NIF | PASS | 1 | 0.011 |
| [test-nif_class.R](testthat/test-nif_class.R#L73) | nif_class | analytes works | PASS | 1 | 0.002 |
| [test-nif_class.R](testthat/test-nif_class.R#L78) | nif_class | analytes works with minimal NIF and rich NIF | PASS | 2 | 0.004 |
| [test-nif_class.R](testthat/test-nif_class.R#L84) | nif_class | analyte_overview works | PASS | 2 | 0.008 |
| [test-nif_class.R](testthat/test-nif_class.R#L90) | nif_class | cmt_mapping works | PASS | 2 | 0.006 |
| [test-nif_class.R](testthat/test-nif_class.R#L116) | nif_class | index_dosing_interval works with single parent | PASS | 1 | 0.020 |
| [test-nif_class.R](testthat/test-nif_class.R#L153) | nif_class | index_dosing_interval works with multiple parents | PASS | 3 | 0.016 |
| [test-nif_class.R](testthat/test-nif_class.R#L197) | nif_class | n_administrations, max_admin_time works, max_observation_time | PASS | 4 | 0.029 |
| [test-nif_class.R](testthat/test-nif_class.R#L205) | nif_class | guess analyte, guess_parent works | PASS | 2 | 0.008 |
| [test-nif_class.R](testthat/test-nif_class.R#L224) | nif_class | add_dose_level works | PASS | 2 | 0.018 |
| [test-nif_class.R](testthat/test-nif_class.R#L275_L276) | nif_class | add_trtdy works | PASS | 1 | 0.007 |
| [test-nif_class.R](testthat/test-nif_class.R#L305) | nif_class | index_rich_sampling_intervals works | PASS | 3 | 0.047 |
| [test-nif_class.R](testthat/test-nif_class.R#L325_L330) | nif_class | cfb works | PASS | 2 | 0.006 |
| [test-nif_class.R](testthat/test-nif_class.R#L337_L339) | nif_class | analyte_overview | PASS | 2 | 0.004 |
| [test-nif_class.R](testthat/test-nif_class.R#L345) | nif_class | write_nif works | PASS | 2 | 0.106 |
| [test-nif_class.R](testthat/test-nif_class.R#L351) | nif_class | print.nif works | PASS | 1 | 0.008 |
| [test-nif_class.R](testthat/test-nif_class.R#L375_L376) | nif_class | add_rtb works | PASS | 1 | 0.006 |
| [test-nif_class.R](testthat/test-nif_class.R#L384) | nif_class | subjects.nif works with standard NIF object | PASS | 6 | 0.006 |
| [test-nif_class.R](testthat/test-nif_class.R#L415) | nif_class | subjects.nif works with minimal NIF object | PASS | 5 | 0.006 |
| [test-nif_class.R](testthat/test-nif_class.R#L432) | nif_class | subjects.nif works with empty NIF object | PASS | 4 | 0.006 |
| [test-nif_class.R](testthat/test-nif_class.R#L452) | nif_class | subjects.nif handles NA values in ID correctly | PASS | 3 | 0.005 |
| [test-nif_class.R](testthat/test-nif_class.R#L471) | nif_class | subjects.nif works with only ID column | PASS | 4 | 0.006 |
| [test-nif_class.R](testthat/test-nif_class.R#L488_L491) | nif_class | subjects.nif works with only USUBJID column | PASS | 1 | 0.004 |
| [test-nif_class.R](testthat/test-nif_class.R#L504_L507) | nif_class | subjects.nif works with neither ID nor USUBJID columns | PASS | 1 | 0.003 |
| [test-nif_class.R](testthat/test-nif_class.R#L515) | nif_class | subjects.nif preserves data types | PASS | 2 | 0.002 |
| [test-nif_class.R](testthat/test-nif_class.R#L525) | nif_class | usubjid works with valid single ID | PASS | 3 | 0.004 |
| [test-nif_class.R](testthat/test-nif_class.R#L538) | nif_class | usubjid works with multiple IDs | PASS | 3 | 0.004 |
| [test-nif_class.R](testthat/test-nif_class.R#L560_L563) | nif_class | usubjid works with minimal NIF | PASS | 1 | 0.004 |
| [test-nif_class.R](testthat/test-nif_class.R#L569_L572) | nif_class | usubjid handles missing IDs gracefully | PASS | 3 | 0.006 |
| [test-nif_class.R](testthat/test-nif_class.R#L583_L586) | nif_class | usubjid handles mixed valid and invalid IDs | PASS | 4 | 0.008 |
| [test-nif_class.R](testthat/test-nif_class.R#L598_L601) | nif_class | usubjid validates input parameters | PASS | 2 | 0.006 |
| [test-nif_class.R](testthat/test-nif_class.R#L615_L618) | nif_class | usubjid works with empty NIF | PASS | 1 | 0.004 |
| [test-nif_class.R](testthat/test-nif_class.R#L634) | nif_class | usubjid works with NIF containing NA USUBJID | PASS | 4 | 0.004 |
| [test-nif_class.R](testthat/test-nif_class.R#L647) | nif_class | usubjid works with silent parameter | PASS | 3 | 0.006 |
| [test-nif_class.R](testthat/test-nif_class.R#L660_L663) | nif_class | usubjid handles edge cases | PASS | 9 | 0.015 |
| [test-nif_class.R](testthat/test-nif_class.R#L688_L691) | nif_class | usubjid validates NIF object | PASS | 1 | 0.004 |
| [test-nif_exploration.R](testthat/test-nif_exploration.R#L5) | nif_exploration | nif summary works | PASS | 2 | 0.082 |
| [test-nif_exploration.R](testthat/test-nif_exploration.R#L11_L15) | nif_exploration | nif_summary_plot works | PASS | 1 | 0.104 |
| [test-nif_exploration.R](testthat/test-nif_exploration.R#L20_L22) | nif_exploration | nif_plot_id | PASS | 5 | 0.110 |
| [test-nif_exploration.R](testthat/test-nif_exploration.R#L43) | nif_exploration | dose_plot_id works | PASS | 2 | 0.037 |
| [test-nif_exploration.R](testthat/test-nif_exploration.R#L49_L50) | nif_exploration | covariate_hist works | PASS | 2 | 0.012 |
| [test-nif_exploration.R](testthat/test-nif_exploration.R#L57_L58) | nif_exploration | covariate_barplot works | PASS | 2 | 0.014 |
| [test-nif_exploration.R](testthat/test-nif_exploration.R#L65) | nif_exploration | x_by_y works | PASS | 5 | 0.021 |
| [test-nif_exploration.R](testthat/test-nif_exploration.R#L74) | nif_exploration | time_by_ntime works | PASS | 1 | 0.013 |
| [test-nif_exploration.R](testthat/test-nif_exploration.R#L79_L82) | nif_exploration | administration_summary works | PASS | 1 | 0.018 |
| [test-nif_exploration.R](testthat/test-nif_exploration.R#L87_L89) | nif_exploration | mean_dose_plot works | PASS | 1 | 0.010 |
| [test-nif_exploration.R](testthat/test-nif_exploration.R#L94_L97) | nif_exploration | subs_per_dose_level works | PASS | 1 | 0.012 |
| [test-nif_exploration.R](testthat/test-nif_exploration.R#L102_L105) | nif_exploration | obs_per_dose_level works | PASS | 1 | 0.012 |
| [test-nif_load.R](testthat/test-nif_load.R#L2_L4) | nif_load | is_char_datetime works correctly | PASS | 5 | 0.006 |
| [test-nif_load.R](testthat/test-nif_load.R#L23) | nif_load | is_likely_datetime works as intended | PASS | 5 | 0.006 |
| [test-nif_load.R](testthat/test-nif_load.R#L45) | nif_load | convert_char_datetime works correctly | PASS | 2 | 0.005 |
| [test-nif_load.R](testthat/test-nif_load.R#L63) | nif_load | import from connection works | PASS | 1 | 0.004 |
| [test-nif_nca.R](testthat/test-nif_nca.R#L5_L8) | nif_nca | nca_from_pp | PASS | 2 | 0.005 |
| [test-nif_options.R](testthat/test-nif_options.R#L2) | nif_options | nif_disclaimer works | PASS | 1 | 0.001 |
| [test-nif_options.R](testthat/test-nif_options.R#L7) | nif_options | nif_disclaimer works with custom text | PASS | 1 | 0.001 |
| [test-nif_options.R](testthat/test-nif_options.R#L12) | nif_options | nif_option works | PASS | 5 | 0.010 |
| [test-nif_options.R](testthat/test-nif_options.R#L24) | nif_options | nif_option_value works | PASS | 1 | 0.002 |
| [test-nif_options.R](testthat/test-nif_options.R#L29) | nif_options | nif_option with empty arguments returns list | PASS | 1 | 0.001 |
| [test-nif_plot.R](testthat/test-nif_plot.R#L2_L7) | nif_plot | make_plot_data_set | PASS | 1 | 0.027 |
| [test-nif_plot.R](testthat/test-nif_plot.R#L12_L13) | nif_plot | plot.nif | PASS | 14 | 0.450 |
| [test-nif_subjects.R](testthat/test-nif_subjects.R#L12) | nif_subjects | calculate_age calculates age correctly | PASS | 1 | 0.005 |
| [test-nif_subjects.R](testthat/test-nif_subjects.R#L27) | nif_subjects | calculate_age preserves existing AGE values when not NA | PASS | 1 | 0.006 |
| [test-nif_subjects.R](testthat/test-nif_subjects.R#L42) | nif_subjects | calculate_age overwrites existing AGE values when preserve_age = FALSE | PASS | 1 | 0.007 |
| [test-nif_subjects.R](testthat/test-nif_subjects.R#L56) | nif_subjects | calculate_age uses custom reference date column | PASS | 1 | 0.005 |
| [test-nif_subjects.R](testthat/test-nif_subjects.R#L76) | nif_subjects | calculate_age returns dataframe unchanged when required columns missing | PASS | 2 | 0.003 |
| [test-nif_subjects.R](testthat/test-nif_subjects.R#L82) | nif_subjects | calculate_age handles non-dataframe input | PASS | 2 | 0.008 |
| [test-nif_subjects.R](testthat/test-nif_subjects.R#L102) | nif_subjects | calculate_age rounds age correctly | PASS | 1 | 0.005 |
| [test-nif_subjects.R](testthat/test-nif_subjects.R#L144_L146) | nif_subjects | make subjects | PASS | 2 | 0.011 |
| [test-nif_subjects.R](testthat/test-nif_subjects.R#L167) | nif_subjects | make_subject works with different age definitions | PASS | 2 | 0.017 |
| [test-nif_subjects.R](testthat/test-nif_subjects.R#L181_L185) | nif_subjects | make_subjects validates inputs correctly | PASS | 5 | 0.016 |
| [test-nif_subjects.R](testthat/test-nif_subjects.R#L283) | nif_subjects | BMI calculation handles edge cases correctly | PASS | 7 | 0.017 |
| [test-nif_subjects.R](testthat/test-nif_subjects.R#L345) | nif_subjects | make_subjects handles basic case correctly | PASS | 7 | 0.015 |
| [test-nif_subjects.R](testthat/test-nif_subjects.R#L366) | nif_subjects | make_subjects handles VS data correctly | PASS | 6 | 0.021 |
| [test-nif_subjects.R](testthat/test-nif_subjects.R#L386) | nif_subjects | make_subjects respects VSBLFL flag | PASS | 2 | 0.016 |
| [test-nif_subjects.R](testthat/test-nif_subjects.R#L396) | nif_subjects | make_subjects respects custom subject filter | PASS | 4 | 0.017 |
| [test-nif_subjects.R](testthat/test-nif_subjects.R#L414) | nif_subjects | make_subjects keeps specified columns | PASS | 2 | 0.010 |
| [test-nif_subjects.R](testthat/test-nif_subjects.R#L424) | nif_subjects | make_subjects errors on invalid inputs | PASS | 4 | 0.023 |
| [test-nif_subjects.R](testthat/test-nif_subjects.R#L447) | nif_subjects | make_subjects handles missing VS data gracefully | PASS | 3 | 0.016 |
| [test-nif_subjects.R](testthat/test-nif_subjects.R#L460) | nif_subjects | make_subjects handles empty data frames | PASS | 4 | 0.012 |
| [test-nif_subjects.R](testthat/test-nif_subjects.R#L479) | nif_subjects | SEX is properly recoded | PASS | 1 | 0.009 |
| [test-nif_subjects.R](testthat/test-nif_subjects.R#L487_L491) | nif_subjects | make_subjects issues warning for empty subject filter results | PASS | 2 | 0.016 |
| [test-nif_viewer.R](testthat/test-nif_viewer.R#L3) | nif_viewer | nif_viewer handles invalid inputs | PASS | 2 | 0.008 |
| [test-nif_viewer.R](testthat/test-nif_viewer.R#L24_L26) | nif_viewer | nif_viewer handles missing required columns | PASS | 1 | 0.004 |
| [test-nif_viewer.R](testthat/test-nif_viewer.R#L43_L45) | nif_viewer | nif_viewer handles invalid data types | PASS | 1 | 0.004 |
| [test-nif_viewer.R](testthat/test-nif_viewer.R#L63_L64) | nif_viewer | nif_viewer handles missing values | PASS | 1 | 0.009 |
| [test-physiological_calculations.R](testthat/test-physiological_calculations.R#L42) | physiological_calculations | BMI calculation handles edge cases correctly | PASS | 7 | 0.016 |
| [test-physiological_calculations.R](testthat/test-physiological_calculations.R#L58) | physiological_calculations | validate_lbw_parameters validates inputs correctly | PASS | 6 | 0.007 |
| [test-physiological_calculations.R](testthat/test-physiological_calculations.R#L68) | physiological_calculations | validate_lbw_parameters handles NA values correctly | PASS | 7 | 0.008 |
| [test-physiological_calculations.R](testthat/test-physiological_calculations.R#L79) | physiological_calculations | validate_lbw_parameters handles invalid numeric inputs correctly | PASS | 4 | 0.005 |
| [test-physiological_calculations.R](testthat/test-physiological_calculations.R#L87) | physiological_calculations | validate_lbw_parameters handles invalid sex values correctly | PASS | 4 | 0.011 |
| [test-physiological_calculations.R](testthat/test-physiological_calculations.R#L100) | physiological_calculations | validate_lbw_parameters handles vectorized inputs correctly | PASS | 4 | 0.006 |
| [test-physiological_calculations.R](testthat/test-physiological_calculations.R#L115_L118) | physiological_calculations | validate_lbw_parameters handles length mismatches correctly | PASS | 3 | 0.009 |
| [test-physiological_calculations.R](testthat/test-physiological_calculations.R#L133_L136) | physiological_calculations | validate_lbw_parameters handles non-numeric inputs correctly | PASS | 2 | 0.006 |
| [test-physiological_calculations.R](testthat/test-physiological_calculations.R#L146) | physiological_calculations | is_male correctly identifies male sex values | PASS | 4 | 0.004 |
| [test-physiological_calculations.R](testthat/test-physiological_calculations.R#L158) | physiological_calculations | is_male correctly identifies non-male sex values | PASS | 4 | 0.004 |
| [test-physiological_calculations.R](testthat/test-physiological_calculations.R#L170) | physiological_calculations | is_male handles NA values correctly | PASS | 1 | 0.001 |
| [test-physiological_calculations.R](testthat/test-physiological_calculations.R#L175) | physiological_calculations | is_male handles invalid sex values correctly | PASS | 5 | 0.005 |
| [test-resolve-duplicates.R](testthat/test-resolve-duplicates.R#L18) | resolve-duplicates | resolve_duplicates works with default fields and mean function | PASS | 3 | 0.005 |
| [test-resolve-duplicates.R](testthat/test-resolve-duplicates.R#L46) | resolve-duplicates | resolve_duplicates works with custom fields and sum function | PASS | 2 | 0.004 |
| [test-resolve-duplicates.R](testthat/test-resolve-duplicates.R#L63) | resolve-duplicates | resolve_duplicates works with custom function to keep first value | PASS | 1 | 0.002 |
| [test-resolve-duplicates.R](testthat/test-resolve-duplicates.R#L78) | resolve-duplicates | resolve_duplicates works with custom function to keep last value | PASS | 1 | 0.002 |
| [test-resolve-duplicates.R](testthat/test-resolve-duplicates.R#L92) | resolve-duplicates | resolve_duplicates works with custom function to keep max value | PASS | 1 | 0.002 |
| [test-resolve-duplicates.R](testthat/test-resolve-duplicates.R#L103_L106) | resolve-duplicates | resolve_duplicates handles non-existent fields | PASS | 1 | 0.004 |
| [test-resolve-duplicates.R](testthat/test-resolve-duplicates.R#L115) | resolve-duplicates | resolve_duplicates handles empty data frame | PASS | 2 | 0.004 |
| [test-resolve-duplicates.R](testthat/test-resolve-duplicates.R#L128) | resolve-duplicates | resolve_duplicates handles data frame with no duplicates | PASS | 2 | 0.004 |
| [test-resolve-duplicates.R](testthat/test-resolve-duplicates.R#L142) | resolve-duplicates | resolve_duplicates handles NA values | PASS | 2 | 0.004 |
| [test-resolve-duplicates.R](testthat/test-resolve-duplicates.R#L156) | resolve-duplicates | resolve_duplicates works with different data types | PASS | 2 | 0.003 |
| [test-resolve-duplicates.R](testthat/test-resolve-duplicates.R#L173) | resolve-duplicates | resolve_duplicates correctly handles NA values with na.rm=TRUE | PASS | 9 | 0.012 |
| [test-sdtm_analytics.R](testthat/test-sdtm_analytics.R#L3) | sdtm_analytics | sdtm_missing_times works | PASS | 1 | 0.004 |
| [test-sdtm_class.R](testthat/test-sdtm_class.R#L25) | sdtm_class | guess_ntime works | PASS | 2 | 0.007 |
| [test-sdtm_class.R](testthat/test-sdtm_class.R#L35_L43) | sdtm_class | new_sdtm | PASS | 1 | 0.001 |
| [test-sdtm_class.R](testthat/test-sdtm_class.R#L48) | sdtm_class | sdtm summary | PASS | 3 | 0.012 |
| [test-sdtm_class.R](testthat/test-sdtm_class.R#L77) | sdtm_class | sdtm_summary works with metabolite mapping | PASS | 1 | 0.003 |
| [test-sdtm_class.R](testthat/test-sdtm_class.R#L82_L86) | sdtm_class | suggest works with consider_nif_auto | PASS | 1 | 0.019 |
| [test-sdtm_class.R](testthat/test-sdtm_class.R#L91_L93) | sdtm_class | suggest_sdtm works | PASS | 1 | 0.007 |
| [test-sdtm_class.R](testthat/test-sdtm_class.R#L118_L121) | sdtm_class | suggest throws error when required domains are missing | PASS | 2 | 0.006 |
| [test-sdtm_class.R](testthat/test-sdtm_class.R#L140_L142) | sdtm_class | subject_info works | PASS | 1 | 0.002 |
| [test-sdtm_class.R](testthat/test-sdtm_class.R#L147) | sdtm_class | subjects, analytes, treatments, doses works for sdtm | PASS | 4 | 0.003 |
| [test-sdtm_class.R](testthat/test-sdtm_class.R#L156_L158) | sdtm_class | filter_subject works | PASS | 2 | 0.005 |
| [test-sdtm_class.R](testthat/test-sdtm_class.R#L175_L178) | sdtm_class | derive_sld works | PASS | 1 | 0.004 |
| [test-sdtm_class.R](testthat/test-sdtm_class.R#L191_L194) | sdtm_class | derive_sld works with TR containing TRTEST | PASS | 1 | 0.004 |
| [test-sdtm_class.R](testthat/test-sdtm_class.R#L210_L213) | sdtm_class | derive_sld works with multiple diagnostic methods | PASS | 1 | 0.004 |
| [test-sdtm_class.R](testthat/test-sdtm_class.R#L232_L235) | sdtm_class | guess_ntime warns about ISO 8601 date formats | PASS | 7 | 0.012 |
| [test-sdtm_exploration.R](testthat/test-sdtm_exploration.R#L11_L13) | sdtm_exploration | filter_correct_date_format works | PASS | 1 | 0.004 |
| [test-sdtm_exploration.R](testthat/test-sdtm_exploration.R#L18) | sdtm_exploration | check_date_format, check_date_time_format works | PASS | 3 | 0.269 |
| [test-sdtm_exploration.R](testthat/test-sdtm_exploration.R#L25) | sdtm_exploration | check_last_exendtc works | PASS | 1 | 0.016 |
| [test-sdtm_exploration.R](testthat/test-sdtm_exploration.R#L30) | sdtm_exploration | check_sdtm works | PASS | 1 | 0.195 |
| [test-sdtm_exploration.R](testthat/test-sdtm_exploration.R#L35) | sdtm_exploration | plot.sdtm works | PASS | 6 | 0.049 |
| [test-sdtm_exploration.R](testthat/test-sdtm_exploration.R#L51_L67) | sdtm_exploration | disposition_summary works | PASS | 1 | 0.005 |
| [test-sdtm_exploration.R](testthat/test-sdtm_exploration.R#L86) | sdtm_exploration | filter_correct_date_format handles valid ISO 8601 dates correctly | PASS | 2 | 0.004 |
| [test-sdtm_exploration.R](testthat/test-sdtm_exploration.R#L102_L107) | sdtm_exploration | filter_correct_date_format filters out invalid date formats | PASS | 3 | 0.014 |
| [test-sdtm_exploration.R](testthat/test-sdtm_exploration.R#L124_L126) | sdtm_exploration | filter_correct_date_format handles empty strings and NA values | PASS | 3 | 0.005 |
| [test-sdtm_exploration.R](testthat/test-sdtm_exploration.R#L141_L144) | sdtm_exploration | filter_correct_date_format provides correct verbose output | PASS | 1 | 0.006 |
| [test-sdtm_exploration.R](testthat/test-sdtm_exploration.R#L156_L158) | sdtm_exploration | filter_correct_date_format handles silent parameter correctly | PASS | 2 | 0.008 |
| [test-sdtm_exploration.R](testthat/test-sdtm_exploration.R#L170_L173) | sdtm_exploration | filter_correct_date_format validates input correctly | PASS | 2 | 0.009 |
| [test-sdtm_exploration.R](testthat/test-sdtm_exploration.R#L197_L200) | sdtm_exploration | filter_correct_date_format handles multiple DTC columns correctly | PASS | 3 | 0.008 |
| [test-sdtm_exploration.R](testthat/test-sdtm_exploration.R#L216_L219) | sdtm_exploration | check_missing_time handles valid inputs correctly | PASS | 2 | 0.008 |
| [test-sdtm_exploration.R](testthat/test-sdtm_exploration.R#L230_L233) | sdtm_exploration | check_missing_time handles invalid inputs correctly | PASS | 2 | 0.006 |
| [test-sdtm_exploration.R](testthat/test-sdtm_exploration.R#L256_L259) | sdtm_exploration | check_missing_time handles empty dataframes correctly | PASS | 1 | 0.004 |
| [test-sdtm_exploration.R](testthat/test-sdtm_exploration.R#L277_L280) | sdtm_exploration | check_missing_time handles different date formats correctly | PASS | 1 | 0.007 |
| [test-sdtm_exploration.R](testthat/test-sdtm_exploration.R#L293) | sdtm_exploration | check_missing_time preserves input data | PASS | 1 | 0.004 |
| [test-sdtm_exploration.R](testthat/test-sdtm_exploration.R#L306_L309) | sdtm_exploration | check_missing_time handles multiple DTC columns correctly | PASS | 1 | 0.005 |
| [test-sdtm_exploration.R](testthat/test-sdtm_exploration.R#L320_L323) | sdtm_exploration | check_missing_time handles missing DOMAIN column | PASS | 1 | 0.005 |
| [test-sdtm_load.R](testthat/test-sdtm_load.R#L4_L7) | sdtm_load | read_sdtm validates inputs correctly | PASS | 4 | 0.012 |
| [test-sdtm_load.R](testthat/test-sdtm_load.R#L34_L37) | sdtm_load | read_sdtm handles missing files correctly | PASS | 1 | 0.004 |
| [test-sdtm_load.R](testthat/test-sdtm_load.R#L58) | sdtm_load | read_sdtm reads different formats correctly | PASS | 8 | 0.019 |
| [test-sdtm_load.R](testthat/test-sdtm_load.R#L99) | sdtm_load | read_sdtm handles multiple domains correctly | PASS | 4 | 0.024 |
| [test-sdtm_load.R](testthat/test-sdtm_load.R#L123) | sdtm_load | read_sdtm handles custom delimiters for CSV | PASS | 2 | 0.004 |
| [test-sdtm_load.R](testthat/test-sdtm_load.R#L146) | sdtm_load | read_sdtm passes additional parameters to read functions | PASS | 2 | 0.004 |
| [test-StatAdmin.R](testthat/test-StatAdmin.R#L14) | StatAdmin | StatAdmin works with valid inputs | PASS | 4 | 0.004 |
| [test-StatAdmin.R](testthat/test-StatAdmin.R#L33) | StatAdmin | StatAdmin handles empty data correctly | PASS | 3 | 0.003 |
| [test-StatAdmin.R](testthat/test-StatAdmin.R#L51_L54) | StatAdmin | StatAdmin handles NA values correctly | PASS | 2 | 0.004 |
| [test-StatAdmin.R](testthat/test-StatAdmin.R#L70) | StatAdmin | StatAdmin handles no administration points | PASS | 3 | 0.003 |
| [test-StatAdmin.R](testthat/test-StatAdmin.R#L78_L81) | StatAdmin | StatAdmin throws appropriate errors | PASS | 4 | 0.011 |
| [test-StatAdmin.R](testthat/test-StatAdmin.R#L126) | StatAdmin | StatAdmin works with different numeric admin values | PASS | 1 | 0.002 |
| [test-summary-nif.R](testthat/test-summary-nif.R#L7) | summary-nif | summary.nif returns the correct class and structure | PASS | 5 | 0.051 |
| [test-summary-nif.R](testthat/test-summary-nif.R#L33_L35) | summary-nif | summary.nif errors appropriately with missing required columns | PASS | 1 | 0.004 |
| [test-summary-nif.R](testthat/test-summary-nif.R#L48) | summary-nif | summary.nif calculates sex distribution correctly | PASS | 4 | 0.056 |
| [test-summary-nif.R](testthat/test-summary-nif.R#L67) | summary-nif | summary.nif handles missing sex data correctly | PASS | 5 | 0.049 |
| [test-summary-nif.R](testthat/test-summary-nif.R#L93) | summary-nif | summary.nif handles renal function classification correctly | PASS | 4 | 0.067 |
| [test-summary-nif.R](testthat/test-summary-nif.R#L121) | summary-nif | summary.nif handles missing renal function data correctly | PASS | 2 | 0.041 |
| [test-summary-nif.R](testthat/test-summary-nif.R#L144) | summary-nif | summary.nif handles hepatic function classification correctly | PASS | 4 | 0.051 |
| [test-summary-nif.R](testthat/test-summary-nif.R#L166) | summary-nif | summary.nif correctly counts subjects per dose level | PASS | 2 | 0.062 |
| [test-summary-nif.R](testthat/test-summary-nif.R#L179) | summary-nif | summary.nif correctly calculates administration duration | PASS | 3 | 0.064 |
| [test-summary-nif.R](testthat/test-summary-nif.R#L199) | summary-nif | plot.summary_nif produces plots | PASS | 12 | 0.151 |
| [test-summary-nif.R](testthat/test-summary-nif.R#L224) | summary-nif | plot.summary_nif handles weight correctly | PASS | 2 | 0.081 |
| [test-summary-sdtm.R](testthat/test-summary-sdtm.R#L38) | summary-sdtm | summary.sdtm handles valid SDTM objects correctly | PASS | 10 | 0.016 |
| [test-summary-sdtm.R](testthat/test-summary-sdtm.R#L71) | summary-sdtm | summary.sdtm handles missing domains gracefully | PASS | 5 | 0.007 |
| [test-summary-sdtm.R](testthat/test-summary-sdtm.R#L110) | summary-sdtm | summary.sdtm handles missing fields in domains | PASS | 6 | 0.008 |
| [test-summary-sdtm.R](testthat/test-summary-sdtm.R#L137_L139) | summary-sdtm | summary.sdtm correctly processes pc_timepoints | PASS | 5 | 0.006 |
| [test-summary-sdtm.R](testthat/test-summary-sdtm.R#L163_L165) | summary-sdtm | summary.sdtm handles empty data frames | PASS | 6 | 0.008 |
| [test-summary-sdtm.R](testthat/test-summary-sdtm.R#L197_L199) | summary-sdtm | summary.sdtm handles NA values in fields | PASS | 1 | 0.002 |
| [test-summary-sdtm.R](testthat/test-summary-sdtm.R#L245) | summary-sdtm | summary.sdtm handles multiple unique values appropriately | PASS | 6 | 0.019 |
| [test-testcd.R](testthat/test-testcd.R#L6) | testcd | testcd() returns empty data frame for empty sdtm object | PASS | 1 | 0.002 |
| [test-testcd.R](testthat/test-testcd.R#L25) | testcd | testcd() handles domains without TESTCD columns | PASS | 1 | 0.002 |
| [test-testcd.R](testthat/test-testcd.R#L61) | testcd | testcd() extracts TESTCD values correctly | PASS | 1 | 0.003 |
| [test-testcd.R](testthat/test-testcd.R#L86_L89) | testcd | testcd() handles domains with missing DOMAIN column | PASS | 2 | 0.005 |
| [test-testcd.R](testthat/test-testcd.R#L115) | testcd | testcd() handles empty DOMAIN column | PASS | 1 | 0.002 |
| [test-testcd.R](testthat/test-testcd.R#L143) | testcd | testcd() handles mixed case domain names | PASS | 1 | 0.002 |
| [test-testcd.R](testthat/test-testcd.R#L176) | testcd | testcd() handles NA values in TESTCD columns | PASS | 1 | 0.002 |
| [test-testcd.R](testthat/test-testcd.R#L200) | testcd | testcd() handles domain parameter correctly | PASS | 6 | 0.008 |
| [test-testcd.R](testthat/test-testcd.R#L227) | testcd | testcd() handles case sensitivity in domain parameter | PASS | 4 | 0.005 |
| [test-testcd.R](testthat/test-testcd.R#L254_L256) | testcd | testcd() handles empty data frames within domains | PASS | 2 | 0.005 |
| [test-testcd.R](testthat/test-testcd.R#L280) | testcd | testcd() handles duplicate TESTCD values correctly | PASS | 1 | 0.002 |
| [test-testcd.R](testthat/test-testcd.R#L286_L289) | testcd | testcd() handles invalid input types | PASS | 3 | 0.010 |
| [test-testcd.R](testthat/test-testcd.R#L316_L319) | testcd | testcd() handles domain parameter validation | PASS | 2 | 0.007 |
| [test-testcd.R](testthat/test-testcd.R#L345) | testcd | testcd() handles domains with all NA TESTCD values | PASS | 1 | 0.002 |
| [test-testcd.R](testthat/test-testcd.R#L368) | testcd | testcd() handles domains with empty TESTCD values | PASS | 1 | 0.002 |
| [test-testcd.R](testthat/test-testcd.R#L391) | testcd | testcd() handles domains with whitespace in TESTCD values | PASS | 1 | 0.002 |
| [test-testcd.R](testthat/test-testcd.R#L416) | testcd | testcd() handles domains with special characters in TESTCD values | PASS | 1 | 0.002 |
| [test-testcd.R](testthat/test-testcd.R#L439) | testcd | testcd() handles domains with numeric TESTCD values | PASS | 1 | 0.002 |
| [test-testcd.R](testthat/test-testcd.R#L461) | testcd | testcd() handles domains with very long TESTCD values | PASS | 1 | 0.002 |
| [test-testcd.R](testthat/test-testcd.R#L486) | testcd | testcd() handles domains with unicode characters in TESTCD values | PASS | 1 | 0.002 |
| [test-testcd.R](testthat/test-testcd.R#L508) | testcd | testcd() handles domains with all missing TESTCD columns | PASS | 1 | 0.002 |
| [test-testcd.R](testthat/test-testcd.R#L533) | testcd | testcd() handles domains with factor TESTCD values | PASS | 1 | 0.002 |
| [test-testcd.R](testthat/test-testcd.R#L556) | testcd | testcd() handles domains with numeric TESTCD values (actual numeric) | PASS | 1 | 0.002 |
| [test-testcd.R](testthat/test-testcd.R#L593) | testcd | testcd() handles domains with complex TESTCD values | PASS | 1 | 0.002 |
| [test-utilities.R](testthat/test-utilities.R#L6) | utilities | conditional message works | PASS | 2 | 0.006 |
| [test-utilities.R](testthat/test-utilities.R#L27) | utilities | recode sex works | PASS | 1 | 0.003 |
| [test-utilities.R](testthat/test-utilities.R#L32) | utilities | positive_or_zero works | PASS | 3 | 0.004 |
| [test-utilities.R](testthat/test-utilities.R#L39) | utilities | indent_string works | PASS | 3 | 0.004 |
| [test-utilities.R](testthat/test-utilities.R#L57) | utilities | standardize_date_format works | PASS | 2 | 0.004 |
| [test-utilities.R](testthat/test-utilities.R#L77_L85) | utilities | isofy_date_format works | PASS | 1 | 0.004 |
| [test-utilities.R](testthat/test-utilities.R#L101) | utilities | lubrify_dates works | PASS | 2 | 0.003 |
| [test-utilities.R](testthat/test-utilities.R#L121) | utilities | isofy_dates works | PASS | 2 | 0.011 |
| [test-utilities.R](testthat/test-utilities.R#L137_L138) | utilities | is_iso_datetime works | PASS | 1 | 0.003 |
| [test-utilities.R](testthat/test-utilities.R#L153_L154) | utilities | is_iso_date works | PASS | 1 | 0.001 |
| [test-utilities.R](testthat/test-utilities.R#L170) | utilities | pt_to_hours works | PASS | 1 | 0.004 |
| [test-utilities.R](testthat/test-utilities.R#L175) | utilities | compose_dtc works | PASS | 1 | 0.003 |
| [test-utilities.R](testthat/test-utilities.R#L196_L199) | utilities | extract_date works | PASS | 1 | 0.003 |
| [test-utilities.R](testthat/test-utilities.R#L218_L220) | utilities | extract_time works | PASS | 1 | 0.003 |
| [test-utilities.R](testthat/test-utilities.R#L230) | utilities | has time works | PASS | 4 | 0.006 |
| [test-utilities.R](testthat/test-utilities.R#L254) | utilities | nice enumeration works | PASS | 3 | 0.003 |
| [test-utilities.R](testthat/test-utilities.R#L262) | utilities | plural works | PASS | 4 | 0.005 |
| [test-utilities.R](testthat/test-utilities.R#L270) | utilities | safe_mean works | PASS | 3 | 0.004 |
| [test-utilities.R](testthat/test-utilities.R#L277) | utilities | safe_sd works | PASS | 3 | 0.004 |
| [test-utilities.R](testthat/test-utilities.R#L284) | utilities | safe min works | PASS | 4 | 0.005 |
| [test-utilities.R](testthat/test-utilities.R#L292) | utilities | pos_diff works | PASS | 2 | 0.004 |
| [test-utilities.R](testthat/test-utilities.R#L301) | utilities | trialday_to_day works | PASS | 5 | 0.008 |
| [test-utilities.R](testthat/test-utilities.R#L313) | utilities | is_iso8601_datetime correctly identifies ISO 8601 date-time formats | PASS | 25 | 0.032 |
| [test-utilities.R](testthat/test-utilities.R#L366) | utilities | is_iso8601_datetime works with vectors | PASS | 1 | 0.002 |
| [test-utilities.R](testthat/test-utilities.R#L372) | utilities | is_iso8601_date correctly identifies ISO 8601 date formats | PASS | 21 | 0.025 |
| [test-utilities.R](testthat/test-utilities.R#L425) | utilities | is_iso8601_date works with vectors | PASS | 2 | 0.003 |
| [test-validate_domain.R](testthat/test-validate_domain.R#L11) | validate_domain | validate_domain accepts valid domain | PASS | 1 | 0.004 |
| [test-validate_domain.R](testthat/test-validate_domain.R#L17_L20) | validate_domain | validate_domain rejects non-data frame input | PASS | 2 | 0.006 |
| [test-validate_domain.R](testthat/test-validate_domain.R#L37_L40) | validate_domain | validate_domain requires DOMAIN column | PASS | 1 | 0.012 |
| [test-validate_domain.R](testthat/test-validate_domain.R#L50_L53) | validate_domain | validate_domain rejects empty DOMAIN column | PASS | 1 | 0.004 |
| [test-validate_domain.R](testthat/test-validate_domain.R#L66) | validate_domain | validate_domain handles multiple DOMAIN values | PASS | 2 | 0.007 |
| [test-validate_domain.R](testthat/test-validate_domain.R#L88_L94) | validate_domain | validate_domain warns about missing expected columns | PASS | 3 | 0.009 |
| [test-validate_domain.R](testthat/test-validate_domain.R#L112_L118) | validate_domain | validate_domain warns about missing permitted columns | PASS | 3 | 0.009 |
| [test-validate_domain.R](testthat/test-validate_domain.R#L135_L138) | validate_domain | validate_domain handles unknown domains gracefully | PASS | 1 | 0.003 |
| [test-validate_domain.R](testthat/test-validate_domain.R#L150) | validate_domain | validate_domain handles correctly case sensitivity | PASS | 1 | 0.003 |
| [test-validate_domain.R](testthat/test-validate_domain.R#L164) | validate_domain | validate_domain works with example data | PASS | 1 | 0.003 |
| [test-validate_sdtm.R](testthat/test-validate_sdtm.R#L25) | validate_sdtm | validate_sdtm_domains validates all domains in a valid SDTM object | PASS | 2 | 0.005 |
| [test-validate_sdtm.R](testthat/test-validate_sdtm.R#L54_L56) | validate_sdtm | validate_sdtm_domains shows/suppresses messages based on silent parameter | PASS | 1 | 0.004 |
| [test-validate_sdtm.R](testthat/test-validate_sdtm.R#L82_L85) | validate_sdtm | validate_sdtm_domains handles mixed valid and unknown domains | PASS | 1 | 0.006 |
| [test-validation.R](testthat/test-validation.R#L3) | validation | validate_param works correctly for string parameters | PASS | 15 | 0.029 |
| [test-validation.R](testthat/test-validation.R#L30) | validation | validate_param works correctly for logical parameters | PASS | 11 | 0.027 |
| [test-validation.R](testthat/test-validation.R#L53) | validation | validate_param works correctly for numeric parameters | PASS | 11 | 0.022 |
| [test-validation.R](testthat/test-validation.R#L76) | validation | validate_param validates type parameter | PASS | 4 | 0.005 |
| [test-validation.R](testthat/test-validation.R#L86) | validation | validate_char_param works correctly | PASS | 10 | 0.020 |
| [test-validation.R](testthat/test-validation.R#L108) | validation | validate_logical_param works correctly | PASS | 9 | 0.018 |
| [test-validation.R](testthat/test-validation.R#L127) | validation | validate_numeric_param works correctly | PASS | 9 | 0.017 |
| [test-validation.R](testthat/test-validation.R#L146) | validation | validate_param handles edge cases correctly | PASS | 11 | 0.022 |
| [test-validation.R](testthat/test-validation.R#L167) | validation | validate_param provides informative error messages | PASS | 7 | 0.029 |
| [test-viewer.R](testthat/test-viewer.R#L2) | viewer | multiplication works | PASS | 1 | 0.002 |
| [test-watermark.R](testthat/test-watermark.R#L10) | watermark | watermark creates correct grob with default parameters | PASS | 2 | 0.004 |
| [test-watermark.R](testthat/test-watermark.R#L17) | watermark | watermark handles empty or NA input correctly | PASS | 3 | 0.003 |
| [test-watermark.R](testthat/test-watermark.R#L29) | watermark | watermark validates numeric parameters | PASS | 8 | 0.023 |
| [test-watermark.R](testthat/test-watermark.R#L48) | watermark | watermark validates fontface parameter | PASS | 5 | 0.008 |
| [test-watermark.R](testthat/test-watermark.R#L69) | watermark | watermark adjusts text size for long text | PASS | 2 | 0.004 |
| [test-watermark.R](testthat/test-watermark.R#L79) | watermark | watermark handles custom parameters correctly | PASS | 4 | 0.010 |
| [test-write_nif.R](testthat/test-write_nif.R#L15) | write_nif | write_nif basic functionality works | PASS | 6 | 0.017 |
| [test-write_nif.R](testthat/test-write_nif.R#L52) | write_nif | write_nif handles fixed-width format | PASS | 2 | 0.007 |
| [test-write_nif.R](testthat/test-write_nif.R#L73) | write_nif | write_nif handles empty dataframe | PASS | 2 | 0.008 |
| [test-write_nif.R](testthat/test-write_nif.R#L99) | write_nif | write_nif preserves column order | PASS | 1 | 0.005 |

</details>

<details>

<summary>

Session Info
</summary>

| Field    | Value                        |
|:---------|:-----------------------------|
| Version  | R version 4.5.1 (2025-06-13) |
| Platform | aarch64-apple-darwin20       |
| Running  | macOS Sequoia 15.5           |
| Language | en_US                        |
| Timezone | Europe/Berlin                |

| Package  | Version |
|:---------|:--------|
| testthat | 3.2.3   |
| covr     | 3.6.4   |
| covrpage | 0.2     |

</details>

<!--- Final Status : error/failed --->
