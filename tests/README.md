Tests and Coverage
================
06 April, 2025 15:25:30

- [Coverage](#coverage)
- [Unit Tests](#unit-tests)

This output is created by
[covrpage](https://github.com/yonicd/covrpage).

## Coverage

Coverage summary is created using the
[covr](https://github.com/r-lib/covr) package.

| Object | Coverage (%) |
|:---|:--:|
| nif | 71.70 |
| [R/nif_debugger.R](../R/nif_debugger.R) | 0.00 |
| [R/nif_viewer.R](../R/nif_viewer.R) | 0.00 |
| [R/nif_nca.R](../R/nif_nca.R) | 5.91 |
| [R/sdtm_synthesis.R](../R/sdtm_synthesis.R) | 17.51 |
| [R/ggplot_extension.R](../R/ggplot_extension.R) | 43.24 |
| [R/physiological_calculations.R](../R/physiological_calculations.R) | 87.21 |
| [R/nif_exploration.R](../R/nif_exploration.R) | 88.10 |
| [R/sdtm_class.R](../R/sdtm_class.R) | 88.34 |
| [R/nif_class.R](../R/nif_class.R) | 92.49 |
| [R/sdtm_exploration.R](../R/sdtm_exploration.R) | 92.58 |
| [R/make_nif.R](../R/make_nif.R) | 93.18 |
| [R/nif_plot.R](../R/nif_plot.R) | 94.44 |
| [R/nif_observations.R](../R/nif_observations.R) | 94.90 |
| [R/nif_load.R](../R/nif_load.R) | 95.28 |
| [R/utilities.R](../R/utilities.R) | 95.74 |
| [R/nif_administrations.R](../R/nif_administrations.R) | 96.12 |
| [R/nif_options.R](../R/nif_options.R) | 96.55 |
| [R/nif_ae.R](../R/nif_ae.R) | 96.72 |
| [R/nif_subjects.R](../R/nif_subjects.R) | 98.55 |
| [R/nif_imputations.R](../R/nif_imputations.R) | 98.63 |
| [R/nif_covariates.R](../R/nif_covariates.R) | 99.12 |
| [R/sdtm_analytics.R](../R/sdtm_analytics.R) | 100.00 |
| [R/sdtm_load.R](../R/sdtm_load.R) | 100.00 |

<br>

## Unit Tests

Unit Test summary is created using the
[testthat](https://github.com/r-lib/testthat) package.

| file | n | time | error | failed | skipped | warning |
|:---|---:|---:|---:|---:|---:|---:|
| [test-add_ae_observation.R](testthat/test-add_ae_observation.R) | 18 | 0.893 | 0 | 0 | 0 | 0 |
| [test-add_baseline.R](testthat/test-add_baseline.R) | 28 | 0.201 | 0 | 0 | 0 | 0 |
| [test-add_cfb.R](testthat/test-add_cfb.R) | 53 | 0.111 | 0 | 0 | 0 | 0 |
| [test-add_covariate.R](testthat/test-add_covariate.R) | 19 | 0.127 | 0 | 0 | 0 | 0 |
| [test-add_parent_mapping.R](testthat/test-add_parent_mapping.R) | 10 | 0.012 | 0 | 0 | 0 | 0 |
| [test-ae_summary.R](testthat/test-ae_summary.R) | 24 | 0.072 | 0 | 0 | 0 | 0 |
| [test-calculate_bmi.R](testthat/test-calculate_bmi.R) | 15 | 0.026 | 0 | 0 | 0 | 0 |
| [test-df_to_string.R](testthat/test-df_to_string.R) | 13 | 0.032 | 0 | 0 | 0 | 0 |
| [test-domain.R](testthat/test-domain.R) | 16 | 0.045 | 0 | 0 | 0 | 0 |
| [test-edish_plot.R](testthat/test-edish_plot.R) | 6 | 0.165 | 0 | 0 | 0 | 0 |
| [test-ggplot_extension.R](testthat/test-ggplot_extension.R) | 1 | 0.002 | 0 | 0 | 0 | 0 |
| [test-guess_ntime.R](testthat/test-guess_ntime.R) | 14 | 0.035 | 0 | 0 | 0 | 0 |
| [test-guess_parent.R](testthat/test-guess_parent.R) | 11 | 0.051 | 0 | 0 | 0 | 0 |
| [test-guess_pcspec.R](testthat/test-guess_pcspec.R) | 15 | 0.028 | 0 | 0 | 0 | 0 |
| [test-has_domain.R](testthat/test-has_domain.R) | 25 | 0.036 | 0 | 0 | 0 | 0 |
| [test-import_from_connection.R](testthat/test-import_from_connection.R) | 35 | 0.108 | 0 | 0 | 0 | 0 |
| [test-import_nif.R](testthat/test-import_nif.R) | 25 | 0.055 | 0 | 0 | 0 | 0 |
| [test-import_observation.R](testthat/test-import_observation.R) | 32 | 0.279 | 0 | 0 | 0 | 0 |
| [test-lbm_boer.R](testthat/test-lbm_boer.R) | 26 | 0.038 | 0 | 0 | 0 | 0 |
| [test-lbm_hume.R](testthat/test-lbm_hume.R) | 19 | 0.024 | 0 | 0 | 0 | 0 |
| [test-lbm_peters.R](testthat/test-lbm_peters.R) | 17 | 0.022 | 0 | 0 | 0 | 0 |
| [test-make_ae.R](testthat/test-make_ae.R) | 19 | 0.135 | 0 | 0 | 0 | 0 |
| [test-make_nif.R](testthat/test-make_nif.R) | 39 | 1.375 | 0 | 0 | 0 | 0 |
| [test-make_subjects_sdtm.R](testthat/test-make_subjects_sdtm.R) | 14 | 0.040 | 0 | 0 | 0 | 0 |
| [test-nif_add_baseline.R](testthat/test-nif_add_baseline.R) | 1 | 0.001 | 0 | 0 | 0 | 0 |
| [test-nif_administrations.R](testthat/test-nif_administrations.R) | 16 | 0.073 | 0 | 0 | 0 | 0 |
| [test-nif_class.R](testthat/test-nif_class.R) | 61 | 0.566 | 0 | 0 | 0 | 0 |
| [test-nif_exploration.R](testthat/test-nif_exploration.R) | 24 | 0.483 | 0 | 0 | 0 | 0 |
| [test-nif_load.R](testthat/test-nif_load.R) | 13 | 0.020 | 0 | 0 | 0 | 0 |
| [test-nif_nca.R](testthat/test-nif_nca.R) | 2 | 0.004 | 0 | 0 | 0 | 0 |
| [test-nif_observations.R](testthat/test-nif_observations.R) | 58 | 2.885 | 0 | 0 | 0 | 0 |
| [test-nif_options.R](testthat/test-nif_options.R) | 9 | 0.017 | 0 | 0 | 0 | 0 |
| [test-nif_plot.R](testthat/test-nif_plot.R) | 15 | 0.450 | 0 | 0 | 0 | 0 |
| [test-nif_subjects.R](testthat/test-nif_subjects.R) | 60 | 0.248 | 0 | 0 | 0 | 0 |
| [test-physiological_calculations.R](testthat/test-physiological_calculations.R) | 51 | 0.086 | 0 | 0 | 0 | 0 |
| [test-sdtm_analytics.R](testthat/test-sdtm_analytics.R) | 1 | 0.003 | 0 | 0 | 0 | 0 |
| [test-sdtm_class.R](testthat/test-sdtm_class.R) | 28 | 0.120 | 0 | 0 | 0 | 0 |
| [test-sdtm_exploration.R](testthat/test-sdtm_exploration.R) | 38 | 0.643 | 0 | 0 | 0 | 0 |
| [test-sdtm_load.R](testthat/test-sdtm_load.R) | 21 | 0.127 | 0 | 0 | 0 | 0 |
| [test-sdtm_synthesis.R](testthat/test-sdtm_synthesis.R) | 2 | 0.495 | 0 | 0 | 0 | 0 |
| [test-summary-sdtm.R](testthat/test-summary-sdtm.R) | 46 | 0.087 | 0 | 0 | 0 | 0 |
| [test-utilities.R](testthat/test-utilities.R) | 100 | 0.156 | 0 | 0 | 0 | 0 |
| [test-validate_domain.R](testthat/test-validate_domain.R) | 16 | 0.063 | 0 | 0 | 0 | 0 |
| [test-validate_sdtm.R](testthat/test-validate_sdtm.R) | 4 | 0.015 | 0 | 0 | 0 | 0 |
| [test-viewer.R](testthat/test-viewer.R) | 1 | 0.001 | 0 | 0 | 0 | 0 |
| [test-write_nif.R](testthat/test-write_nif.R) | 11 | 0.035 | 0 | 0 | 0 | 0 |

<details closed>
<summary>
Show Detailed Test Results
</summary>

| file | context | test | status | n | time |
|:---|:---|:---|:---|---:|---:|
| [test-add_ae_observation.R](testthat/test-add_ae_observation.R#L52) | add_ae_observation | add_ae_observation handles basic case correctly | PASS | 6 | 0.188 |
| [test-add_ae_observation.R](testthat/test-add_ae_observation.R#L103) | add_ae_observation | add_ae_observation handles different ae_fields correctly | PASS | 2 | 0.305 |
| [test-add_ae_observation.R](testthat/test-add_ae_observation.R#L155) | add_ae_observation | add_ae_observation handles filters correctly | PASS | 3 | 0.129 |
| [test-add_ae_observation.R](testthat/test-add_ae_observation.R#L206) | add_ae_observation | add_ae_observation handles debug mode correctly | PASS | 3 | 0.094 |
| [test-add_ae_observation.R](testthat/test-add_ae_observation.R#L245) | add_ae_observation | add_ae_observation handles keep parameter correctly | PASS | 2 | 0.089 |
| [test-add_ae_observation.R](testthat/test-add_ae_observation.R#L285) | add_ae_observation | add_ae_observation handles automatic parent and cmt assignment | PASS | 2 | 0.088 |
| [test-add_baseline.R](testthat/test-add_baseline.R#L32) | add_baseline | add_baseline adds baseline covariate correctly | PASS | 4 | 0.011 |
| [test-add_baseline.R](testthat/test-add_baseline.R#L65) | add_baseline | add_baseline handles custom baseline filter | PASS | 2 | 0.069 |
| [test-add_baseline.R](testthat/test-add_baseline.R#L102) | add_baseline | add_baseline handles coding table correctly | PASS | 2 | 0.009 |
| [test-add_baseline.R](testthat/test-add_baseline.R#L131_L134) | add_baseline | add_baseline validates inputs correctly | PASS | 4 | 0.015 |
| [test-add_baseline.R](testthat/test-add_baseline.R#L178) | add_baseline | add_baseline handles multiple baseline values correctly | PASS | 2 | 0.022 |
| [test-add_baseline.R](testthat/test-add_baseline.R#L206_L208) | add_baseline | add_baseline handles empty result after filtering | PASS | 1 | 0.007 |
| [test-add_baseline.R](testthat/test-add_baseline.R#L254_L257) | add_baseline | add baseline hepatic function class works | PASS | 2 | 0.021 |
| [test-add_baseline.R](testthat/test-add_baseline.R#L286_L289) | add_baseline | add_baseline handles all NA baseline values correctly | PASS | 1 | 0.010 |
| [test-add_baseline.R](testthat/test-add_baseline.R#L315_L318) | add_baseline | add_baseline warns when some baseline values are NA | PASS | 4 | 0.013 |
| [test-add_baseline.R](testthat/test-add_baseline.R#L348_L351) | add_baseline | add_baseline validates required fields correctly | PASS | 2 | 0.007 |
| [test-add_baseline.R](testthat/test-add_baseline.R#L376) | add_baseline | add_baseline name parameter works correctly | PASS | 4 | 0.017 |
| [test-add_cfb.R](testthat/test-add_cfb.R#L19) | add_cfb | add_cfb works with valid input | PASS | 5 | 0.007 |
| [test-add_cfb.R](testthat/test-add_cfb.R#L49) | add_cfb | add_cfb correctly handles baseline calculation with time ≤ 0 | PASS | 10 | 0.024 |
| [test-add_cfb.R](testthat/test-add_cfb.R#L80_L84) | add_cfb | add_cfb handles NA values in grouping columns | PASS | 3 | 0.010 |
| [test-add_cfb.R](testthat/test-add_cfb.R#L102) | add_cfb | add_cfb works with different summary functions | PASS | 3 | 0.009 |
| [test-add_cfb.R](testthat/test-add_cfb.R#L126) | add_cfb | add_cfb works with custom baseline filter | PASS | 1 | 0.003 |
| [test-add_cfb.R](testthat/test-add_cfb.R#L138) | add_cfb | add_cfb handles missing required columns | PASS | 2 | 0.007 |
| [test-add_cfb.R](testthat/test-add_cfb.R#L160) | add_cfb | add_cfb handles non-numeric columns | PASS | 2 | 0.007 |
| [test-add_cfb.R](testthat/test-add_cfb.R#L194) | add_cfb | add_cfb correctly handles complex baseline filters | PASS | 10 | 0.013 |
| [test-add_cfb.R](testthat/test-add_cfb.R#L227) | add_cfb | add_cfb correctly handles empty baseline sets | PASS | 2 | 0.005 |
| [test-add_cfb.R](testthat/test-add_cfb.R#L251) | add_cfb | add_cfb correctly handles baseline filter with missing values | PASS | 8 | 0.010 |
| [test-add_cfb.R](testthat/test-add_cfb.R#L284) | add_cfb | add_cfb correctly handles baseline filter with character columns | PASS | 7 | 0.016 |
| [test-add_covariate.R](testthat/test-add_covariate.R#L81) | add_covariate | add_covariate works with valid inputs | PASS | 9 | 0.028 |
| [test-add_covariate.R](testthat/test-add_covariate.R#L104_L107) | add_covariate | add_covariate validates nif object | PASS | 1 | 0.019 |
| [test-add_covariate.R](testthat/test-add_covariate.R#L115_L118) | add_covariate | add_covariate validates sdtm is provided | PASS | 1 | 0.007 |
| [test-add_covariate.R](testthat/test-add_covariate.R#L127_L130) | add_covariate | add_covariate validates domain exists | PASS | 1 | 0.006 |
| [test-add_covariate.R](testthat/test-add_covariate.R#L145_L148) | add_covariate | add_covariate validates required fields exist | PASS | 1 | 0.006 |
| [test-add_covariate.R](testthat/test-add_covariate.R#L157_L160) | add_covariate | add_covariate validates testcd exists | PASS | 1 | 0.005 |
| [test-add_covariate.R](testthat/test-add_covariate.R#L172_L175) | add_covariate | add_covariate validates matching subjects exist | PASS | 1 | 0.005 |
| [test-add_covariate.R](testthat/test-add_covariate.R#L185_L189) | add_covariate | add_covariate casts error if no data after filtering | PASS | 1 | 0.006 |
| [test-add_covariate.R](testthat/test-add_covariate.R#L215) | add_covariate | add_covariate works with custom field names | PASS | 1 | 0.014 |
| [test-add_covariate.R](testthat/test-add_covariate.R#L235) | add_covariate | add_covariate handles duplicated observations correctly | PASS | 1 | 0.018 |
| [test-add_covariate.R](testthat/test-add_covariate.R#L249) | add_covariate | add_covariate uses default covariate name if not specified | PASS | 1 | 0.013 |
| [test-add_parent_mapping.R](testthat/test-add_parent_mapping.R#L14) | add_parent_mapping | add_parent_mapping adds correct parent mapping | PASS | 4 | 0.005 |
| [test-add_parent_mapping.R](testthat/test-add_parent_mapping.R#L42) | add_parent_mapping | add_parent_mapping can add multiple mappings | PASS | 3 | 0.004 |
| [test-add_parent_mapping.R](testthat/test-add_parent_mapping.R#L71) | add_parent_mapping | add_parent_mapping preserves existing mappings | PASS | 3 | 0.003 |
| [test-ae_summary.R](testthat/test-ae_summary.R#L19) | ae_summary | ae_summary handles basic case correctly | PASS | 5 | 0.007 |
| [test-ae_summary.R](testthat/test-ae_summary.R#L42) | ae_summary | ae_summary works with different levels | PASS | 3 | 0.016 |
| [test-ae_summary.R](testthat/test-ae_summary.R#L63) | ae_summary | ae_summary handles show_cd parameter | PASS | 2 | 0.005 |
| [test-ae_summary.R](testthat/test-ae_summary.R#L81) | ae_summary | ae_summary handles grouping | PASS | 2 | 0.005 |
| [test-ae_summary.R](testthat/test-ae_summary.R#L100) | ae_summary | ae_summary handles ordering | PASS | 1 | 0.004 |
| [test-ae_summary.R](testthat/test-ae_summary.R#L117) | ae_summary | ae_summary handles filtering | PASS | 2 | 0.004 |
| [test-ae_summary.R](testthat/test-ae_summary.R#L132_L133) | ae_summary | ae_summary handles invalid inputs | PASS | 2 | 0.007 |
| [test-ae_summary.R](testthat/test-ae_summary.R#L151) | ae_summary | ae_summary handles empty data | PASS | 2 | 0.004 |
| [test-ae_summary.R](testthat/test-ae_summary.R#L159_L160) | ae_summary | ae_summary validates SDTM object structure | PASS | 5 | 0.020 |
| [test-calculate_bmi.R](testthat/test-calculate_bmi.R#L3) | calculate_bmi | calculate_bmi works correctly for valid inputs | PASS | 3 | 0.005 |
| [test-calculate_bmi.R](testthat/test-calculate_bmi.R#L16) | calculate_bmi | calculate_bmi handles NA values correctly | PASS | 5 | 0.006 |
| [test-calculate_bmi.R](testthat/test-calculate_bmi.R#L31) | calculate_bmi | calculate_bmi handles invalid inputs correctly | PASS | 4 | 0.004 |
| [test-calculate_bmi.R](testthat/test-calculate_bmi.R#L40) | calculate_bmi | calculate_bmi handles type errors correctly | PASS | 2 | 0.007 |
| [test-calculate_bmi.R](testthat/test-calculate_bmi.R#L47_L50) | calculate_bmi | calculate_bmi handles length mismatch correctly | PASS | 1 | 0.004 |
| [test-df_to_string.R](testthat/test-df_to_string.R#L11) | df_to_string | df_to_string basic functionality works | PASS | 6 | 0.017 |
| [test-df_to_string.R](testthat/test-df_to_string.R#L31) | df_to_string | df_to_string handles empty data frames | PASS | 2 | 0.003 |
| [test-df_to_string.R](testthat/test-df_to_string.R#L48) | df_to_string | df_to_string respects n parameter | PASS | 1 | 0.003 |
| [test-df_to_string.R](testthat/test-df_to_string.R#L61) | df_to_string | df_to_string handles color formatting | PASS | 2 | 0.004 |
| [test-df_to_string.R](testthat/test-df_to_string.R#L74) | df_to_string | df_to_string handles NA values | PASS | 1 | 0.002 |
| [test-df_to_string.R](testthat/test-df_to_string.R#L90) | df_to_string | df_to_string maintains column alignment | PASS | 1 | 0.003 |
| [test-domain.R](testthat/test-domain.R#L26) | domain | domain() returns correct data frames for existing domains | PASS | 2 | 0.003 |
| [test-domain.R](testthat/test-domain.R#L41) | domain | domain() errors for non-existent domains | PASS | 3 | 0.010 |
| [test-domain.R](testthat/test-domain.R#L59) | domain | domain() is case-insensitive | PASS | 4 | 0.005 |
| [test-domain.R](testthat/test-domain.R#L77) | domain | domain() handles input validation correctly | PASS | 5 | 0.021 |
| [test-domain.R](testthat/test-domain.R#L100_L101) | domain | domain() rejects vectors with multiple names | PASS | 2 | 0.006 |
| [test-edish_plot.R](testthat/test-edish_plot.R#L41) | edish_plot | edish_plot handles valid input correctly | PASS | 3 | 0.125 |
| [test-edish_plot.R](testthat/test-edish_plot.R#L48_L51) | edish_plot | edish_plot validates enzyme parameter | PASS | 1 | 0.027 |
| [test-edish_plot.R](testthat/test-edish_plot.R#L62_L65) | edish_plot | edish_plot handles missing required lab tests | PASS | 1 | 0.006 |
| [test-edish_plot.R](testthat/test-edish_plot.R#L75_L80) | edish_plot | edish_plot handles zero ULN values | PASS | 1 | 0.007 |
| [test-ggplot_extension.R](testthat/test-ggplot_extension.R#L2) | ggplot_extension | multiplication works | PASS | 1 | 0.002 |
| [test-guess_ntime.R](testthat/test-guess_ntime.R#L26) | guess_ntime | guess_ntime correctly parses various time formats | PASS | 3 | 0.005 |
| [test-guess_ntime.R](testthat/test-guess_ntime.R#L50_L53) | guess_ntime | guess_ntime handles ISO 8601 dates with a warning | PASS | 5 | 0.017 |
| [test-guess_ntime.R](testthat/test-guess_ntime.R#L71_L74) | guess_ntime | guess_ntime errors on missing PC domain | PASS | 1 | 0.004 |
| [test-guess_ntime.R](testthat/test-guess_ntime.R#L92_L95) | guess_ntime | guess_ntime errors on missing PCTPT column | PASS | 1 | 0.003 |
| [test-guess_ntime.R](testthat/test-guess_ntime.R#L120) | guess_ntime | guess_ntime handles additional predose variations | PASS | 4 | 0.006 |
| [test-guess_parent.R](testthat/test-guess_parent.R#L17) | guess_parent | guess_parent identifies analyte with most administrations | PASS | 1 | 0.004 |
| [test-guess_parent.R](testthat/test-guess_parent.R#L37) | guess_parent | guess_parent falls back to observations when no administrations exist | PASS | 1 | 0.004 |
| [test-guess_parent.R](testthat/test-guess_parent.R#L58) | guess_parent | guess_parent ignores metabolite observations | PASS | 1 | 0.005 |
| [test-guess_parent.R](testthat/test-guess_parent.R#L81) | guess_parent | guess_parent prioritizes administrations over observations | PASS | 1 | 0.004 |
| [test-guess_parent.R](testthat/test-guess_parent.R#L93) | guess_parent | guess_parent returns NULL for empty dataset | PASS | 1 | 0.005 |
| [test-guess_parent.R](testthat/test-guess_parent.R#L108) | guess_parent | guess_parent works with minimal dataset | PASS | 1 | 0.004 |
| [test-guess_parent.R](testthat/test-guess_parent.R#L122) | guess_parent | guess_parent handles tied administration counts | PASS | 1 | 0.003 |
| [test-guess_parent.R](testthat/test-guess_parent.R#L139) | guess_parent | guess_parent works with ensure_analyte | PASS | 1 | 0.003 |
| [test-guess_parent.R](testthat/test-guess_parent.R#L153) | guess_parent | guess_parent returns NULL for dataset with only metabolite observations | PASS | 1 | 0.005 |
| [test-guess_parent.R](testthat/test-guess_parent.R#L169) | guess_parent | guess_parent handles NA values in key columns | PASS | 1 | 0.009 |
| [test-guess_parent.R](testthat/test-guess_parent.R#L186) | guess_parent | guess_parent correctly counts tied observations when no administrations exist | PASS | 1 | 0.005 |
| [test-guess_pcspec.R](testthat/test-guess_pcspec.R#L4) | guess_pcspec | guess_pcspec works correctly | PASS | 8 | 0.011 |
| [test-guess_pcspec.R](testthat/test-guess_pcspec.R#L33_L34) | guess_pcspec | guess_pcspec handles errors correctly | PASS | 5 | 0.015 |
| [test-guess_pcspec.R](testthat/test-guess_pcspec.R#L59) | guess_pcspec | guess_pcspec maintains data frame attributes | PASS | 2 | 0.002 |
| [test-has_domain.R](testthat/test-has_domain.R#L15) | has_domain | has_domain correctly identifies existing domains | PASS | 6 | 0.006 |
| [test-has_domain.R](testthat/test-has_domain.R#L36) | has_domain | has_domain is case-insensitive | PASS | 4 | 0.004 |
| [test-has_domain.R](testthat/test-has_domain.R#L54) | has_domain | has_domain handles input validation correctly | PASS | 5 | 0.015 |
| [test-has_domain.R](testthat/test-has_domain.R#L78) | has_domain | has_domain handles multiple domain names correctly | PASS | 5 | 0.005 |
| [test-has_domain.R](testthat/test-has_domain.R#L99_L100) | has_domain | domain() rejects vectors with multiple names | PASS | 1 | 0.003 |
| [test-has_domain.R](testthat/test-has_domain.R#L111) | has_domain | has_domain works with example data | PASS | 1 | 0.001 |
| [test-has_domain.R](testthat/test-has_domain.R#L127) | has_domain | domain function behaviors | PASS | 3 | 0.002 |
| [test-import_from_connection.R](testthat/test-import_from_connection.R#L21) | import_from_connection | import_from_connection handles CSV data correctly | PASS | 6 | 0.008 |
| [test-import_from_connection.R](testthat/test-import_from_connection.R#L50) | import_from_connection | import_from_connection handles fixed-width data correctly | PASS | 6 | 0.013 |
| [test-import_from_connection.R](testthat/test-import_from_connection.R#L76) | import_from_connection | import_from_connection auto-detects CSV format | PASS | 2 | 0.031 |
| [test-import_from_connection.R](testthat/test-import_from_connection.R#L98) | import_from_connection | import_from_connection auto-detects fixed-width format | PASS | 2 | 0.007 |
| [test-import_from_connection.R](testthat/test-import_from_connection.R#L126) | import_from_connection | import_from_connection ignores comments and empty lines | PASS | 2 | 0.005 |
| [test-import_from_connection.R](testthat/test-import_from_connection.R#L149) | import_from_connection | import_from_connection handles datetime conversion | PASS | 3 | 0.005 |
| [test-import_from_connection.R](testthat/test-import_from_connection.R#L173) | import_from_connection | import_from_connection handles custom delimiters | PASS | 3 | 0.006 |
| [test-import_from_connection.R](testthat/test-import_from_connection.R#L196) | import_from_connection | import_from_connection adds missing fields | PASS | 3 | 0.006 |
| [test-import_from_connection.R](testthat/test-import_from_connection.R#L221) | import_from_connection | import_from_connection respects no_numeric parameter | PASS | 3 | 0.004 |
| [test-import_from_connection.R](testthat/test-import_from_connection.R#L229) | import_from_connection | import_from_connection handles errors correctly | PASS | 3 | 0.013 |
| [test-import_from_connection.R](testthat/test-import_from_connection.R#L255_L258) | import_from_connection | import_from_connection detects invalid fixed-width format | PASS | 1 | 0.005 |
| [test-import_from_connection.R](testthat/test-import_from_connection.R#L276_L279) | import_from_connection | import_from_connection detects inconsistent CSV format | PASS | 1 | 0.005 |
| [test-import_nif.R](testthat/test-import_nif.R#L10) | import_nif | import_nif loads CSV files correctly | PASS | 6 | 0.010 |
| [test-import_nif.R](testthat/test-import_nif.R#L28) | import_nif | import_nif loads fixed-width files correctly | PASS | 6 | 0.009 |
| [test-import_nif.R](testthat/test-import_nif.R#L46) | import_nif | import_nif automatically detects CSV format | PASS | 2 | 0.004 |
| [test-import_nif.R](testthat/test-import_nif.R#L60) | import_nif | import_nif automatically detects fixed-width format | PASS | 2 | 0.004 |
| [test-import_nif.R](testthat/test-import_nif.R#L74) | import_nif | import_nif handles custom delimiter | PASS | 2 | 0.004 |
| [test-import_nif.R](testthat/test-import_nif.R#L88) | import_nif | import_nif handles no_numeric parameter correctly | PASS | 2 | 0.005 |
| [test-import_nif.R](testthat/test-import_nif.R#L105) | import_nif | import_nif handles date/time conversion | PASS | 2 | 0.004 |
| [test-import_nif.R](testthat/test-import_nif.R#L119) | import_nif | import_nif handles comments and empty lines | PASS | 2 | 0.011 |
| [test-import_nif.R](testthat/test-import_nif.R#L125_L128) | import_nif | import_nif errors on file not found | PASS | 1 | 0.004 |
| [test-import_observation.R](testthat/test-import_observation.R#L47_L55) | import_observation | import_observation validates input parameters correctly | PASS | 5 | 0.046 |
| [test-import_observation.R](testthat/test-import_observation.R#L130) | import_observation | import_observation correctly handles DTC field | PASS | 7 | 0.032 |
| [test-import_observation.R](testthat/test-import_observation.R#L164) | import_observation | import_observation correctly handles NTIME field | PASS | 7 | 0.038 |
| [test-import_observation.R](testthat/test-import_observation.R#L200_L212) | import_observation | import_observation automatically assigns compartment when cmt is NULL | PASS | 2 | 0.028 |
| [test-import_observation.R](testthat/test-import_observation.R#L226_L238) | import_observation | import_observation automatically determines parent when parent is NULL | PASS | 4 | 0.056 |
| [test-import_observation.R](testthat/test-import_observation.R#L294) | import_observation | import_observation correctly joins subject data | PASS | 4 | 0.029 |
| [test-import_observation.R](testthat/test-import_observation.R#L331) | import_observation | import_observation correctly sets debug fields | PASS | 3 | 0.050 |
| [test-lbm_boer.R](testthat/test-lbm_boer.R#L3) | lbm_boer | lbm_boer calculates correct values for valid inputs | PASS | 6 | 0.014 |
| [test-lbm_boer.R](testthat/test-lbm_boer.R#L16) | lbm_boer | lbm_boer handles NA inputs correctly | PASS | 7 | 0.007 |
| [test-lbm_boer.R](testthat/test-lbm_boer.R#L27) | lbm_boer | lbm_boer handles invalid numeric inputs correctly | PASS | 6 | 0.010 |
| [test-lbm_boer.R](testthat/test-lbm_boer.R#L39) | lbm_boer | lbm_boer handles invalid sex inputs correctly | PASS | 4 | 0.004 |
| [test-lbm_boer.R](testthat/test-lbm_boer.R#L50) | lbm_boer | lbm_boer produces consistent results for same inputs | PASS | 3 | 0.003 |
| [test-lbm_hume.R](testthat/test-lbm_hume.R#L5_L9) | lbm_hume | lbm_hume calculates correct values for valid inputs | PASS | 4 | 0.008 |
| [test-lbm_hume.R](testthat/test-lbm_hume.R#L32) | lbm_hume | lbm_hume handles edge cases correctly | PASS | 7 | 0.007 |
| [test-lbm_hume.R](testthat/test-lbm_hume.R#L46_L49) | lbm_hume | lbm_hume handles different sex input formats | PASS | 6 | 0.007 |
| [test-lbm_hume.R](testthat/test-lbm_hume.R#L82) | lbm_hume | lbm_hume maintains consistency with other LBM formulas | PASS | 2 | 0.002 |
| [test-lbm_peters.R](testthat/test-lbm_peters.R#L3_L7) | lbm_peters | lbm_peters calculates lean body mass correctly | PASS | 4 | 0.004 |
| [test-lbm_peters.R](testthat/test-lbm_peters.R#L34) | lbm_peters | lbm_peters handles edge cases | PASS | 9 | 0.014 |
| [test-lbm_peters.R](testthat/test-lbm_peters.R#L62_L66) | lbm_peters | lbm_peters handles vectorized inputs correctly | PASS | 4 | 0.004 |
| [test-make_ae.R](testthat/test-make_ae.R#L35) | make_ae | make_ae handles basic case correctly | PASS | 5 | 0.017 |
| [test-make_ae.R](testthat/test-make_ae.R#L71) | make_ae | make_ae handles different ae_fields correctly | PASS | 4 | 0.028 |
| [test-make_ae.R](testthat/test-make_ae.R#L106) | make_ae | make_ae handles filters correctly | PASS | 3 | 0.027 |
| [test-make_ae.R](testthat/test-make_ae.R#L140) | make_ae | make_ae handles missing data correctly | PASS | 1 | 0.018 |
| [test-make_ae.R](testthat/test-make_ae.R#L149_L150) | make_ae | make_ae handles errors appropriately | PASS | 2 | 0.016 |
| [test-make_ae.R](testthat/test-make_ae.R#L196) | make_ae | make_ae handles compartment and parent parameters correctly | PASS | 2 | 0.015 |
| [test-make_ae.R](testthat/test-make_ae.R#L221) | make_ae | make_ae preserves specified columns with keep parameter | PASS | 2 | 0.014 |
| [test-make_nif.R](testthat/test-make_nif.R#L52) | make_nif | date conversion works correctly | PASS | 1 | 0.003 |
| [test-make_nif.R](testthat/test-make_nif.R#L90_L97) | make_nif | impute_exendtc_to_rfendtc works as intended | PASS | 1 | 0.011 |
| [test-make_nif.R](testthat/test-make_nif.R#L114_L118) | make_nif | impute_exendtc_to_rfendtc works correctly | PASS | 3 | 0.014 |
| [test-make_nif.R](testthat/test-make_nif.R#L173) | make_nif | impute_missing_exendtc | PASS | 1 | 0.015 |
| [test-make_nif.R](testthat/test-make_nif.R#L196_L202) | make_nif | impute_exendtc_to_cutoff works | PASS | 2 | 0.011 |
| [test-make_nif.R](testthat/test-make_nif.R#L222) | make_nif | filter_EXSTDTC_after_EXENDTC works | PASS | 2 | 0.008 |
| [test-make_nif.R](testthat/test-make_nif.R#L246_L248) | make_nif | make_administration works for examplinib_poc | PASS | 1 | 0.214 |
| [test-make_nif.R](testthat/test-make_nif.R#L299_L301) | make_nif | make_administration uses correct time imputations | PASS | 3 | 0.056 |
| [test-make_nif.R](testthat/test-make_nif.R#L345_L347) | make_nif | make_administration works without pc | PASS | 3 | 0.041 |
| [test-make_nif.R](testthat/test-make_nif.R#L392_L396) | make_nif | make_administration imputes missing last EXENDTC | PASS | 2 | 0.049 |
| [test-make_nif.R](testthat/test-make_nif.R#L406_L407) | make_nif | make_nif | PASS | 1 | 0.299 |
| [test-make_nif.R](testthat/test-make_nif.R#L431) | make_nif | make_time | PASS | 1 | 0.011 |
| [test-make_nif.R](testthat/test-make_nif.R#L500_L502) | make_nif | multiple imputations | PASS | 1 | 0.019 |
| [test-make_nif.R](testthat/test-make_nif.R#L549_L555) | make_nif | add_administration, add_observation | PASS | 2 | 0.154 |
| [test-make_nif.R](testthat/test-make_nif.R#L573_L589) | make_nif | import_observation | PASS | 1 | 0.034 |
| [test-make_nif.R](testthat/test-make_nif.R#L605) | make_nif | make_ntime works | PASS | 4 | 0.010 |
| [test-make_nif.R](testthat/test-make_nif.R#L614_L626) | make_nif | make_nif integration works | PASS | 1 | 0.395 |
| [test-make_nif.R](testthat/test-make_nif.R#L631_L632) | make_nif | guess_pcspec works | PASS | 3 | 0.004 |
| [test-make_nif.R](testthat/test-make_nif.R#L641_L642) | make_nif | guess_lbspec works | PASS | 2 | 0.003 |
| [test-make_nif.R](testthat/test-make_nif.R#L657) | make_nif | add_time works | PASS | 1 | 0.005 |
| [test-make_nif.R](testthat/test-make_nif.R#L686_L690) | make_nif | limit works | PASS | 3 | 0.019 |
| [test-make_subjects_sdtm.R](testthat/test-make_subjects_sdtm.R#L30) | make_subjects_sdtm | make_subjects_sdtm creates a proper subject data frame | PASS | 7 | 0.014 |
| [test-make_subjects_sdtm.R](testthat/test-make_subjects_sdtm.R#L71) | make_subjects_sdtm | make_subjects_sdtm works with missing height/weight | PASS | 4 | 0.011 |
| [test-make_subjects_sdtm.R](testthat/test-make_subjects_sdtm.R#L97) | make_subjects_sdtm | make_subjects_sdtm works with example data | PASS | 3 | 0.015 |
| [test-nif_add_baseline.R](testthat/test-nif_add_baseline.R#L2) | nif_add_baseline | multiplication works | PASS | 1 | 0.001 |
| [test-nif_administrations.R](testthat/test-nif_administrations.R#L3) | nif_administrations | date_list works | PASS | 8 | 0.010 |
| [test-nif_administrations.R](testthat/test-nif_administrations.R#L26) | nif_administrations | expand_ex works in general | PASS | 2 | 0.012 |
| [test-nif_administrations.R](testthat/test-nif_administrations.R#L51) | nif_administrations | expand_ex works with TRTDY | PASS | 2 | 0.013 |
| [test-nif_administrations.R](testthat/test-nif_administrations.R#L69) | nif_administrations | expand_ex works with missing EXENDTC | PASS | 2 | 0.012 |
| [test-nif_administrations.R](testthat/test-nif_administrations.R#L90) | nif_administrations | expand_ex errs when end date before start date | PASS | 1 | 0.013 |
| [test-nif_administrations.R](testthat/test-nif_administrations.R#L100) | nif_administrations | expand_ex errs when end day before start day | PASS | 1 | 0.013 |
| [test-nif_class.R](testthat/test-nif_class.R#L2) | nif_class | new_nif works | PASS | 2 | 0.172 |
| [test-nif_class.R](testthat/test-nif_class.R#L8) | nif_class | subject_info works | PASS | 2 | 0.034 |
| [test-nif_class.R](testthat/test-nif_class.R#L15) | nif_class | subjects, usubjid works | PASS | 2 | 0.003 |
| [test-nif_class.R](testthat/test-nif_class.R#L21) | nif_class | subjects works with minimal NIF | PASS | 4 | 0.005 |
| [test-nif_class.R](testthat/test-nif_class.R#L29) | nif_class | parents works | PASS | 1 | 0.002 |
| [test-nif_class.R](testthat/test-nif_class.R#L34) | nif_class | dose_red_sbs works | PASS | 1 | 0.008 |
| [test-nif_class.R](testthat/test-nif_class.R#L39_L40) | nif_class | rich_sampling_sbs works | PASS | 1 | 0.009 |
| [test-nif_class.R](testthat/test-nif_class.R#L45) | nif_class | studies works | PASS | 1 | 0.002 |
| [test-nif_class.R](testthat/test-nif_class.R#L50) | nif_class | ensure works | PASS | 8 | 0.039 |
| [test-nif_class.R](testthat/test-nif_class.R#L62) | nif_class | doses works | PASS | 1 | 0.007 |
| [test-nif_class.R](testthat/test-nif_class.R#L67) | nif_class | doses works with minimal NIF | PASS | 1 | 0.002 |
| [test-nif_class.R](testthat/test-nif_class.R#L71) | nif_class | dose_levels works | PASS | 1 | 0.008 |
| [test-nif_class.R](testthat/test-nif_class.R#L76) | nif_class | treatments works | PASS | 2 | 0.003 |
| [test-nif_class.R](testthat/test-nif_class.R#L82) | nif_class | dose_levels works with minimal NIF | PASS | 1 | 0.009 |
| [test-nif_class.R](testthat/test-nif_class.R#L87) | nif_class | analytes works | PASS | 1 | 0.002 |
| [test-nif_class.R](testthat/test-nif_class.R#L92) | nif_class | analytes works with minimal NIF and rich NIF | PASS | 2 | 0.003 |
| [test-nif_class.R](testthat/test-nif_class.R#L98) | nif_class | analyte_overview works | PASS | 2 | 0.004 |
| [test-nif_class.R](testthat/test-nif_class.R#L104) | nif_class | cmt_mapping works | PASS | 2 | 0.004 |
| [test-nif_class.R](testthat/test-nif_class.R#L130) | nif_class | index_dosing_interval works with single parent | PASS | 1 | 0.010 |
| [test-nif_class.R](testthat/test-nif_class.R#L167) | nif_class | index_dosing_interval works with multiple parents | PASS | 3 | 0.020 |
| [test-nif_class.R](testthat/test-nif_class.R#L211) | nif_class | n_administrations, max_admin_time works, max_observation_time | PASS | 4 | 0.029 |
| [test-nif_class.R](testthat/test-nif_class.R#L219) | nif_class | guess analyte, guess_parent works | PASS | 2 | 0.012 |
| [test-nif_class.R](testthat/test-nif_class.R#L238) | nif_class | add_dose_level works | PASS | 2 | 0.012 |
| [test-nif_class.R](testthat/test-nif_class.R#L260_L262) | nif_class | add_tad, add_tafd works | PASS | 2 | 0.017 |
| [test-nif_class.R](testthat/test-nif_class.R#L287_L288) | nif_class | add_trtdy works | PASS | 1 | 0.005 |
| [test-nif_class.R](testthat/test-nif_class.R#L316) | nif_class | index_rich_sampling_intervals works | PASS | 3 | 0.031 |
| [test-nif_class.R](testthat/test-nif_class.R#L335_L340) | nif_class | cfb works | PASS | 2 | 0.004 |
| [test-nif_class.R](testthat/test-nif_class.R#L347_L349) | nif_class | analyte_overview | PASS | 2 | 0.003 |
| [test-nif_class.R](testthat/test-nif_class.R#L355) | nif_class | write_nif works | PASS | 2 | 0.097 |
| [test-nif_class.R](testthat/test-nif_class.R#L361) | nif_class | print.nif works | PASS | 1 | 0.006 |
| [test-nif_class.R](testthat/test-nif_class.R#L383_L384) | nif_class | add_rtb works | PASS | 1 | 0.004 |
| [test-nif_exploration.R](testthat/test-nif_exploration.R#L5) | nif_exploration | nif summary works | PASS | 2 | 0.114 |
| [test-nif_exploration.R](testthat/test-nif_exploration.R#L11_L15) | nif_exploration | nif_summary_plot works | PASS | 1 | 0.100 |
| [test-nif_exploration.R](testthat/test-nif_exploration.R#L20_L22) | nif_exploration | nif_plot_id | PASS | 5 | 0.110 |
| [test-nif_exploration.R](testthat/test-nif_exploration.R#L43) | nif_exploration | dose_plot_id works | PASS | 2 | 0.047 |
| [test-nif_exploration.R](testthat/test-nif_exploration.R#L49_L50) | nif_exploration | covariate_hist works | PASS | 2 | 0.010 |
| [test-nif_exploration.R](testthat/test-nif_exploration.R#L57_L58) | nif_exploration | covariate_barplot works | PASS | 2 | 0.022 |
| [test-nif_exploration.R](testthat/test-nif_exploration.R#L65) | nif_exploration | x_by_y works | PASS | 5 | 0.021 |
| [test-nif_exploration.R](testthat/test-nif_exploration.R#L74) | nif_exploration | time_by_ntime works | PASS | 1 | 0.010 |
| [test-nif_exploration.R](testthat/test-nif_exploration.R#L79_L82) | nif_exploration | administration_summary works | PASS | 1 | 0.018 |
| [test-nif_exploration.R](testthat/test-nif_exploration.R#L87_L89) | nif_exploration | mean_dose_plot works | PASS | 1 | 0.010 |
| [test-nif_exploration.R](testthat/test-nif_exploration.R#L94_L97) | nif_exploration | subs_per_dose_level works | PASS | 1 | 0.011 |
| [test-nif_exploration.R](testthat/test-nif_exploration.R#L102_L105) | nif_exploration | obs_per_dose_level works | PASS | 1 | 0.010 |
| [test-nif_load.R](testthat/test-nif_load.R#L2_L4) | nif_load | is_char_datetime works correctly | PASS | 5 | 0.005 |
| [test-nif_load.R](testthat/test-nif_load.R#L23) | nif_load | is_likely_datetime works as intended | PASS | 5 | 0.005 |
| [test-nif_load.R](testthat/test-nif_load.R#L45) | nif_load | convert_char_datetime works correctly | PASS | 2 | 0.006 |
| [test-nif_load.R](testthat/test-nif_load.R#L63) | nif_load | import from connection works | PASS | 1 | 0.004 |
| [test-nif_nca.R](testthat/test-nif_nca.R#L5_L8) | nif_nca | nca_from_pp | PASS | 2 | 0.004 |
| [test-nif_observations.R](testthat/test-nif_observations.R#L58_L59) | nif_observations | make_observation works | PASS | 1 | 0.034 |
| [test-nif_observations.R](testthat/test-nif_observations.R#L66_L69) | nif_observations | make_observation issues warning if observation filter returns no observations | PASS | 1 | 0.019 |
| [test-nif_observations.R](testthat/test-nif_observations.R#L75_L88) | nif_observations | make_observation works with coding table | PASS | 1 | 0.018 |
| [test-nif_observations.R](testthat/test-nif_observations.R#L98) | nif_observations | make_observation handles different domains correctly | PASS | 2 | 0.031 |
| [test-nif_observations.R](testthat/test-nif_observations.R#L111) | nif_observations | make_observation applies DV factor correctly | PASS | 2 | 0.061 |
| [test-nif_observations.R](testthat/test-nif_observations.R#L126) | nif_observations | make_observation handles custom DV_field correctly | PASS | 1 | 0.017 |
| [test-nif_observations.R](testthat/test-nif_observations.R#L144) | nif_observations | make_observation handles custom TESTCD_field correctly | PASS | 2 | 0.018 |
| [test-nif_observations.R](testthat/test-nif_observations.R#L158) | nif_observations | make_observation handles custom DTC_field correctly | PASS | 1 | 0.028 |
| [test-nif_observations.R](testthat/test-nif_observations.R#L170) | nif_observations | make_observation creates proper output fields | PASS | 13 | 0.033 |
| [test-nif_observations.R](testthat/test-nif_observations.R#L198) | nif_observations | make_observation handles NTIME lookup correctly | PASS | 1 | 0.016 |
| [test-nif_observations.R](testthat/test-nif_observations.R#L206_L209) | nif_observations | make_observation validates inputs correctly | PASS | 2 | 0.008 |
| [test-nif_observations.R](testthat/test-nif_observations.R#L229_L232) | nif_observations | make_observation handles missing DV field with coding table | PASS | 3 | 0.031 |
| [test-nif_observations.R](testthat/test-nif_observations.R#L260) | nif_observations | make_observation sets MDV correctly for missing values | PASS | 2 | 0.017 |
| [test-nif_observations.R](testthat/test-nif_observations.R#L274_L277) | nif_observations | add_observation basic functionality works | PASS | 3 | 0.156 |
| [test-nif_observations.R](testthat/test-nif_observations.R#L287_L290) | nif_observations | add_observation requires administration first | PASS | 1 | 0.005 |
| [test-nif_observations.R](testthat/test-nif_observations.R#L300_L304) | nif_observations | add_observation warns about duplicate compartment | PASS | 1 | 0.156 |
| [test-nif_observations.R](testthat/test-nif_observations.R#L314_L318) | nif_observations | add_observation auto-assigns compartment if not specified | PASS | 2 | 0.158 |
| [test-nif_observations.R](testthat/test-nif_observations.R#L331_L336) | nif_observations | add_observation auto-assigns parent if not specified | PASS | 1 | 0.161 |
| [test-nif_observations.R](testthat/test-nif_observations.R#L346_L351) | nif_observations | add_observation properly uses observation_filter | PASS | 1 | 0.120 |
| [test-nif_observations.R](testthat/test-nif_observations.R#L379) | nif_observations | add_observation works with factor parameter | PASS | 1 | 0.218 |
| [test-nif_observations.R](testthat/test-nif_observations.R#L403) | nif_observations | add_observation handles metabolites correctly | PASS | 1 | 0.157 |
| [test-nif_observations.R](testthat/test-nif_observations.R#L419_L426) | nif_observations | add_observation works with custom NTIME_lookup | PASS | 1 | 0.150 |
| [test-nif_observations.R](testthat/test-nif_observations.R#L441) | nif_observations | add_observation handles debug mode correctly | PASS | 2 | 0.156 |
| [test-nif_observations.R](testthat/test-nif_observations.R#L462) | nif_observations | add_observation updates columns correctly | PASS | 1 | 0.154 |
| [test-nif_observations.R](testthat/test-nif_observations.R#L490_L493) | nif_observations | add_observation handles include_day_in_ntime parameter | PASS | 1 | 0.211 |
| [test-nif_observations.R](testthat/test-nif_observations.R#L511_L517) | nif_observations | add_observation handles missing NTIME gracefully | PASS | 3 | 0.108 |
| [test-nif_observations.R](testthat/test-nif_observations.R#L538_L541) | nif_observations | add_observation handles DV field properly | PASS | 2 | 0.104 |
| [test-nif_observations.R](testthat/test-nif_observations.R#L580) | nif_observations | add_observation handles subject filtering | PASS | 1 | 0.100 |
| [test-nif_observations.R](testthat/test-nif_observations.R#L590_L596) | nif_observations | add_observation can handle non-existent domain gracefully | PASS | 1 | 0.110 |
| [test-nif_observations.R](testthat/test-nif_observations.R#L614_L621) | nif_observations | add_observation handles observations without matching administrations | PASS | 2 | 0.222 |
| [test-nif_observations.R](testthat/test-nif_observations.R#L640_L647) | nif_observations | add_observation properly handles custom testcd field | PASS | 1 | 0.108 |
| [test-nif_options.R](testthat/test-nif_options.R#L2) | nif_options | nif_disclaimer works | PASS | 1 | 0.001 |
| [test-nif_options.R](testthat/test-nif_options.R#L7) | nif_options | nif_disclaimer works with custom text | PASS | 1 | 0.002 |
| [test-nif_options.R](testthat/test-nif_options.R#L12) | nif_options | nif_option works | PASS | 5 | 0.011 |
| [test-nif_options.R](testthat/test-nif_options.R#L24) | nif_options | nif_option_value works | PASS | 1 | 0.002 |
| [test-nif_options.R](testthat/test-nif_options.R#L29) | nif_options | nif_option with empty arguments returns list | PASS | 1 | 0.001 |
| [test-nif_plot.R](testthat/test-nif_plot.R#L2_L7) | nif_plot | make_plot_data_set | PASS | 1 | 0.031 |
| [test-nif_plot.R](testthat/test-nif_plot.R#L12_L13) | nif_plot | plot.nif | PASS | 14 | 0.419 |
| [test-nif_subjects.R](testthat/test-nif_subjects.R#L12) | nif_subjects | calculate_age calculates age correctly | PASS | 1 | 0.004 |
| [test-nif_subjects.R](testthat/test-nif_subjects.R#L27) | nif_subjects | calculate_age preserves existing AGE values when not NA | PASS | 1 | 0.004 |
| [test-nif_subjects.R](testthat/test-nif_subjects.R#L42) | nif_subjects | calculate_age overwrites existing AGE values when preserve_age = FALSE | PASS | 1 | 0.004 |
| [test-nif_subjects.R](testthat/test-nif_subjects.R#L56) | nif_subjects | calculate_age uses custom reference date column | PASS | 1 | 0.004 |
| [test-nif_subjects.R](testthat/test-nif_subjects.R#L76) | nif_subjects | calculate_age returns dataframe unchanged when required columns missing | PASS | 2 | 0.003 |
| [test-nif_subjects.R](testthat/test-nif_subjects.R#L82) | nif_subjects | calculate_age handles non-dataframe input | PASS | 2 | 0.007 |
| [test-nif_subjects.R](testthat/test-nif_subjects.R#L102) | nif_subjects | calculate_age rounds age correctly | PASS | 1 | 0.004 |
| [test-nif_subjects.R](testthat/test-nif_subjects.R#L144_L146) | nif_subjects | make subjects | PASS | 2 | 0.017 |
| [test-nif_subjects.R](testthat/test-nif_subjects.R#L167) | nif_subjects | make_subject works with different age definitions | PASS | 2 | 0.015 |
| [test-nif_subjects.R](testthat/test-nif_subjects.R#L181_L185) | nif_subjects | make_subjects validates inputs correctly | PASS | 5 | 0.017 |
| [test-nif_subjects.R](testthat/test-nif_subjects.R#L283) | nif_subjects | BMI calculation handles edge cases correctly | PASS | 7 | 0.015 |
| [test-nif_subjects.R](testthat/test-nif_subjects.R#L345) | nif_subjects | make_subjects handles basic case correctly | PASS | 7 | 0.014 |
| [test-nif_subjects.R](testthat/test-nif_subjects.R#L366) | nif_subjects | make_subjects handles VS data correctly | PASS | 6 | 0.025 |
| [test-nif_subjects.R](testthat/test-nif_subjects.R#L386) | nif_subjects | make_subjects respects VSBLFL flag | PASS | 2 | 0.016 |
| [test-nif_subjects.R](testthat/test-nif_subjects.R#L396) | nif_subjects | make_subjects respects custom subject filter | PASS | 4 | 0.016 |
| [test-nif_subjects.R](testthat/test-nif_subjects.R#L414) | nif_subjects | make_subjects keeps specified columns | PASS | 2 | 0.009 |
| [test-nif_subjects.R](testthat/test-nif_subjects.R#L424) | nif_subjects | make_subjects errors on invalid inputs | PASS | 4 | 0.017 |
| [test-nif_subjects.R](testthat/test-nif_subjects.R#L447) | nif_subjects | make_subjects handles missing VS data gracefully | PASS | 3 | 0.015 |
| [test-nif_subjects.R](testthat/test-nif_subjects.R#L460) | nif_subjects | make_subjects handles empty data frames | PASS | 4 | 0.016 |
| [test-nif_subjects.R](testthat/test-nif_subjects.R#L479) | nif_subjects | SEX is properly recoded | PASS | 1 | 0.009 |
| [test-nif_subjects.R](testthat/test-nif_subjects.R#L487_L491) | nif_subjects | make_subjects issues warning for empty subject filter results | PASS | 2 | 0.017 |
| [test-physiological_calculations.R](testthat/test-physiological_calculations.R#L42) | physiological_calculations | BMI calculation handles edge cases correctly | PASS | 7 | 0.015 |
| [test-physiological_calculations.R](testthat/test-physiological_calculations.R#L58) | physiological_calculations | validate_lbw_parameters validates inputs correctly | PASS | 6 | 0.006 |
| [test-physiological_calculations.R](testthat/test-physiological_calculations.R#L68) | physiological_calculations | validate_lbw_parameters handles NA values correctly | PASS | 7 | 0.007 |
| [test-physiological_calculations.R](testthat/test-physiological_calculations.R#L79) | physiological_calculations | validate_lbw_parameters handles invalid numeric inputs correctly | PASS | 4 | 0.004 |
| [test-physiological_calculations.R](testthat/test-physiological_calculations.R#L87) | physiological_calculations | validate_lbw_parameters handles invalid sex values correctly | PASS | 4 | 0.004 |
| [test-physiological_calculations.R](testthat/test-physiological_calculations.R#L100) | physiological_calculations | validate_lbw_parameters handles vectorized inputs correctly | PASS | 4 | 0.004 |
| [test-physiological_calculations.R](testthat/test-physiological_calculations.R#L115_L118) | physiological_calculations | validate_lbw_parameters handles length mismatches correctly | PASS | 3 | 0.010 |
| [test-physiological_calculations.R](testthat/test-physiological_calculations.R#L133_L136) | physiological_calculations | validate_lbw_parameters handles non-numeric inputs correctly | PASS | 2 | 0.021 |
| [test-physiological_calculations.R](testthat/test-physiological_calculations.R#L146) | physiological_calculations | is_male correctly identifies male sex values | PASS | 4 | 0.005 |
| [test-physiological_calculations.R](testthat/test-physiological_calculations.R#L158) | physiological_calculations | is_male correctly identifies non-male sex values | PASS | 4 | 0.004 |
| [test-physiological_calculations.R](testthat/test-physiological_calculations.R#L170) | physiological_calculations | is_male handles NA values correctly | PASS | 1 | 0.001 |
| [test-physiological_calculations.R](testthat/test-physiological_calculations.R#L175) | physiological_calculations | is_male handles invalid sex values correctly | PASS | 5 | 0.005 |
| [test-sdtm_analytics.R](testthat/test-sdtm_analytics.R#L3) | sdtm_analytics | sdtm_missing_times works | PASS | 1 | 0.003 |
| [test-sdtm_class.R](testthat/test-sdtm_class.R#L25) | sdtm_class | guess_ntime works | PASS | 2 | 0.007 |
| [test-sdtm_class.R](testthat/test-sdtm_class.R#L35_L43) | sdtm_class | new_sdtm | PASS | 1 | 0.001 |
| [test-sdtm_class.R](testthat/test-sdtm_class.R#L48) | sdtm_class | sdtm summary | PASS | 3 | 0.022 |
| [test-sdtm_class.R](testthat/test-sdtm_class.R#L77) | sdtm_class | sdtm_summary works with metabolite mapping | PASS | 1 | 0.003 |
| [test-sdtm_class.R](testthat/test-sdtm_class.R#L82_L86) | sdtm_class | suggest works with consider_nif_auto | PASS | 1 | 0.009 |
| [test-sdtm_class.R](testthat/test-sdtm_class.R#L91_L93) | sdtm_class | suggest_sdtm works | PASS | 1 | 0.032 |
| [test-sdtm_class.R](testthat/test-sdtm_class.R#L118_L121) | sdtm_class | suggest throws error when required domains are missing | PASS | 2 | 0.007 |
| [test-sdtm_class.R](testthat/test-sdtm_class.R#L140_L142) | sdtm_class | subject_info works | PASS | 1 | 0.001 |
| [test-sdtm_class.R](testthat/test-sdtm_class.R#L147) | sdtm_class | subjects, analytes, treatments, doses works for sdtm | PASS | 4 | 0.002 |
| [test-sdtm_class.R](testthat/test-sdtm_class.R#L156_L158) | sdtm_class | filter_subject works | PASS | 2 | 0.005 |
| [test-sdtm_class.R](testthat/test-sdtm_class.R#L175_L178) | sdtm_class | derive_sld works | PASS | 1 | 0.004 |
| [test-sdtm_class.R](testthat/test-sdtm_class.R#L191_L194) | sdtm_class | derive_sld works with TR containing TRTEST | PASS | 1 | 0.012 |
| [test-sdtm_class.R](testthat/test-sdtm_class.R#L210_L213) | sdtm_class | derive_sld works with multiple diagnostic methods | PASS | 1 | 0.004 |
| [test-sdtm_class.R](testthat/test-sdtm_class.R#L232_L235) | sdtm_class | guess_ntime warns about ISO 8601 date formats | PASS | 7 | 0.011 |
| [test-sdtm_exploration.R](testthat/test-sdtm_exploration.R#L11_L13) | sdtm_exploration | filter_correct_date_format works | PASS | 1 | 0.003 |
| [test-sdtm_exploration.R](testthat/test-sdtm_exploration.R#L18) | sdtm_exploration | check_date_format, check_date_time_format works | PASS | 3 | 0.245 |
| [test-sdtm_exploration.R](testthat/test-sdtm_exploration.R#L25) | sdtm_exploration | check_last_exendtc works | PASS | 1 | 0.007 |
| [test-sdtm_exploration.R](testthat/test-sdtm_exploration.R#L30) | sdtm_exploration | check_sdtm works | PASS | 1 | 0.199 |
| [test-sdtm_exploration.R](testthat/test-sdtm_exploration.R#L35) | sdtm_exploration | plot.sdtm works | PASS | 6 | 0.086 |
| [test-sdtm_exploration.R](testthat/test-sdtm_exploration.R#L51_L67) | sdtm_exploration | disposition_summary works | PASS | 1 | 0.004 |
| [test-sdtm_exploration.R](testthat/test-sdtm_exploration.R#L86) | sdtm_exploration | filter_correct_date_format handles valid ISO 8601 dates correctly | PASS | 2 | 0.008 |
| [test-sdtm_exploration.R](testthat/test-sdtm_exploration.R#L102_L107) | sdtm_exploration | filter_correct_date_format filters out invalid date formats | PASS | 3 | 0.007 |
| [test-sdtm_exploration.R](testthat/test-sdtm_exploration.R#L124_L126) | sdtm_exploration | filter_correct_date_format handles empty strings and NA values | PASS | 3 | 0.004 |
| [test-sdtm_exploration.R](testthat/test-sdtm_exploration.R#L141_L144) | sdtm_exploration | filter_correct_date_format provides correct verbose output | PASS | 1 | 0.006 |
| [test-sdtm_exploration.R](testthat/test-sdtm_exploration.R#L156_L158) | sdtm_exploration | filter_correct_date_format handles silent parameter correctly | PASS | 2 | 0.009 |
| [test-sdtm_exploration.R](testthat/test-sdtm_exploration.R#L170_L173) | sdtm_exploration | filter_correct_date_format validates input correctly | PASS | 2 | 0.010 |
| [test-sdtm_exploration.R](testthat/test-sdtm_exploration.R#L197_L200) | sdtm_exploration | filter_correct_date_format handles multiple DTC columns correctly | PASS | 3 | 0.008 |
| [test-sdtm_exploration.R](testthat/test-sdtm_exploration.R#L216_L219) | sdtm_exploration | check_missing_time handles valid inputs correctly | PASS | 2 | 0.008 |
| [test-sdtm_exploration.R](testthat/test-sdtm_exploration.R#L230_L233) | sdtm_exploration | check_missing_time handles invalid inputs correctly | PASS | 2 | 0.007 |
| [test-sdtm_exploration.R](testthat/test-sdtm_exploration.R#L256_L259) | sdtm_exploration | check_missing_time handles empty dataframes correctly | PASS | 1 | 0.010 |
| [test-sdtm_exploration.R](testthat/test-sdtm_exploration.R#L277_L280) | sdtm_exploration | check_missing_time handles different date formats correctly | PASS | 1 | 0.006 |
| [test-sdtm_exploration.R](testthat/test-sdtm_exploration.R#L293) | sdtm_exploration | check_missing_time preserves input data | PASS | 1 | 0.004 |
| [test-sdtm_exploration.R](testthat/test-sdtm_exploration.R#L306_L309) | sdtm_exploration | check_missing_time handles multiple DTC columns correctly | PASS | 1 | 0.006 |
| [test-sdtm_exploration.R](testthat/test-sdtm_exploration.R#L320_L323) | sdtm_exploration | check_missing_time handles missing DOMAIN column | PASS | 1 | 0.006 |
| [test-sdtm_load.R](testthat/test-sdtm_load.R#L3_L6) | sdtm_load | read_sdtm validates inputs correctly | PASS | 4 | 0.021 |
| [test-sdtm_load.R](testthat/test-sdtm_load.R#L33_L36) | sdtm_load | read_sdtm handles missing files correctly | PASS | 1 | 0.004 |
| [test-sdtm_load.R](testthat/test-sdtm_load.R#L57) | sdtm_load | read_sdtm reads different formats correctly | PASS | 8 | 0.081 |
| [test-sdtm_load.R](testthat/test-sdtm_load.R#L98) | sdtm_load | read_sdtm handles multiple domains correctly | PASS | 4 | 0.015 |
| [test-sdtm_load.R](testthat/test-sdtm_load.R#L122) | sdtm_load | read_sdtm handles custom delimiters for CSV | PASS | 2 | 0.003 |
| [test-sdtm_load.R](testthat/test-sdtm_load.R#L145) | sdtm_load | read_sdtm passes additional parameters to read functions | PASS | 2 | 0.003 |
| [test-sdtm_synthesis.R](testthat/test-sdtm_synthesis.R#L21) | sdtm_synthesis | synthesize_crea works | PASS | 1 | 0.234 |
| [test-sdtm_synthesis.R](testthat/test-sdtm_synthesis.R#L76) | sdtm_synthesis | EGFR is age-dependent | PASS | 1 | 0.261 |
| [test-summary-sdtm.R](testthat/test-summary-sdtm.R#L38) | summary-sdtm | summary.sdtm handles valid SDTM objects correctly | PASS | 10 | 0.030 |
| [test-summary-sdtm.R](testthat/test-summary-sdtm.R#L71) | summary-sdtm | summary.sdtm handles missing domains gracefully | PASS | 5 | 0.006 |
| [test-summary-sdtm.R](testthat/test-summary-sdtm.R#L110) | summary-sdtm | summary.sdtm handles missing fields in domains | PASS | 6 | 0.008 |
| [test-summary-sdtm.R](testthat/test-summary-sdtm.R#L137_L139) | summary-sdtm | summary.sdtm correctly processes pc_timepoints | PASS | 5 | 0.006 |
| [test-summary-sdtm.R](testthat/test-summary-sdtm.R#L163_L165) | summary-sdtm | summary.sdtm handles empty data frames | PASS | 6 | 0.008 |
| [test-summary-sdtm.R](testthat/test-summary-sdtm.R#L197_L199) | summary-sdtm | summary.sdtm handles NA values in fields | PASS | 1 | 0.003 |
| [test-summary-sdtm.R](testthat/test-summary-sdtm.R#L245) | summary-sdtm | summary.sdtm handles multiple unique values appropriately | PASS | 6 | 0.010 |
| [test-summary-sdtm.R](testthat/test-summary-sdtm.R#L282) | summary-sdtm | summary.sdtm handles objects with complete mapping data | PASS | 7 | 0.016 |
| [test-utilities.R](testthat/test-utilities.R#L6) | utilities | conditional message works | PASS | 2 | 0.005 |
| [test-utilities.R](testthat/test-utilities.R#L27) | utilities | recode sex works | PASS | 1 | 0.002 |
| [test-utilities.R](testthat/test-utilities.R#L32) | utilities | positive_or_zero works | PASS | 3 | 0.009 |
| [test-utilities.R](testthat/test-utilities.R#L39) | utilities | indent_string works | PASS | 3 | 0.004 |
| [test-utilities.R](testthat/test-utilities.R#L57) | utilities | standardize_date_format works | PASS | 2 | 0.004 |
| [test-utilities.R](testthat/test-utilities.R#L77_L85) | utilities | isofy_date_format works | PASS | 1 | 0.004 |
| [test-utilities.R](testthat/test-utilities.R#L101) | utilities | lubrify_dates works | PASS | 2 | 0.003 |
| [test-utilities.R](testthat/test-utilities.R#L121) | utilities | isofy_dates works | PASS | 2 | 0.003 |
| [test-utilities.R](testthat/test-utilities.R#L137_L138) | utilities | is_iso_datetime works | PASS | 1 | 0.002 |
| [test-utilities.R](testthat/test-utilities.R#L153_L154) | utilities | is_iso_date works | PASS | 1 | 0.001 |
| [test-utilities.R](testthat/test-utilities.R#L170) | utilities | pt_to_hours works | PASS | 1 | 0.003 |
| [test-utilities.R](testthat/test-utilities.R#L175) | utilities | compose_dtc works | PASS | 1 | 0.002 |
| [test-utilities.R](testthat/test-utilities.R#L190) | utilities | decompose_dtc works | PASS | 1 | 0.004 |
| [test-utilities.R](testthat/test-utilities.R#L208_L211) | utilities | extract_date works | PASS | 1 | 0.002 |
| [test-utilities.R](testthat/test-utilities.R#L230_L232) | utilities | extract_time works | PASS | 1 | 0.003 |
| [test-utilities.R](testthat/test-utilities.R#L242) | utilities | has time works | PASS | 4 | 0.007 |
| [test-utilities.R](testthat/test-utilities.R#L266) | utilities | nice enumeration works | PASS | 3 | 0.004 |
| [test-utilities.R](testthat/test-utilities.R#L274) | utilities | plural works | PASS | 4 | 0.005 |
| [test-utilities.R](testthat/test-utilities.R#L282) | utilities | safe_mean works | PASS | 3 | 0.004 |
| [test-utilities.R](testthat/test-utilities.R#L289) | utilities | safe_sd works | PASS | 3 | 0.004 |
| [test-utilities.R](testthat/test-utilities.R#L296) | utilities | safe min works | PASS | 4 | 0.004 |
| [test-utilities.R](testthat/test-utilities.R#L304) | utilities | pos_diff works | PASS | 2 | 0.003 |
| [test-utilities.R](testthat/test-utilities.R#L313) | utilities | trialday_to_day works | PASS | 5 | 0.008 |
| [test-utilities.R](testthat/test-utilities.R#L325) | utilities | is_iso8601_datetime correctly identifies ISO 8601 date-time formats | PASS | 25 | 0.037 |
| [test-utilities.R](testthat/test-utilities.R#L378) | utilities | is_iso8601_datetime works with vectors | PASS | 1 | 0.002 |
| [test-utilities.R](testthat/test-utilities.R#L384) | utilities | is_iso8601_date correctly identifies ISO 8601 date formats | PASS | 21 | 0.025 |
| [test-utilities.R](testthat/test-utilities.R#L437) | utilities | is_iso8601_date works with vectors | PASS | 2 | 0.002 |
| [test-validate_domain.R](testthat/test-validate_domain.R#L11) | validate_domain | validate_domain accepts valid domain | PASS | 1 | 0.003 |
| [test-validate_domain.R](testthat/test-validate_domain.R#L17_L20) | validate_domain | validate_domain rejects non-data frame input | PASS | 2 | 0.008 |
| [test-validate_domain.R](testthat/test-validate_domain.R#L37_L40) | validate_domain | validate_domain requires DOMAIN column | PASS | 1 | 0.004 |
| [test-validate_domain.R](testthat/test-validate_domain.R#L50_L53) | validate_domain | validate_domain rejects empty DOMAIN column | PASS | 1 | 0.004 |
| [test-validate_domain.R](testthat/test-validate_domain.R#L66) | validate_domain | validate_domain handles multiple DOMAIN values | PASS | 2 | 0.007 |
| [test-validate_domain.R](testthat/test-validate_domain.R#L88_L94) | validate_domain | validate_domain warns about missing expected columns | PASS | 3 | 0.010 |
| [test-validate_domain.R](testthat/test-validate_domain.R#L112_L118) | validate_domain | validate_domain warns about missing permitted columns | PASS | 3 | 0.017 |
| [test-validate_domain.R](testthat/test-validate_domain.R#L135_L138) | validate_domain | validate_domain handles unknown domains gracefully | PASS | 1 | 0.004 |
| [test-validate_domain.R](testthat/test-validate_domain.R#L150) | validate_domain | validate_domain handles correctly case sensitivity | PASS | 1 | 0.003 |
| [test-validate_domain.R](testthat/test-validate_domain.R#L164) | validate_domain | validate_domain works with example data | PASS | 1 | 0.003 |
| [test-validate_sdtm.R](testthat/test-validate_sdtm.R#L25) | validate_sdtm | validate_sdtm validates all domains in a valid SDTM object | PASS | 2 | 0.005 |
| [test-validate_sdtm.R](testthat/test-validate_sdtm.R#L54_L56) | validate_sdtm | validate_sdtm shows/suppresses messages based on silent parameter | PASS | 1 | 0.004 |
| [test-validate_sdtm.R](testthat/test-validate_sdtm.R#L82_L85) | validate_sdtm | validate_sdtm handles mixed valid and unknown domains | PASS | 1 | 0.006 |
| [test-viewer.R](testthat/test-viewer.R#L2) | viewer | multiplication works | PASS | 1 | 0.001 |
| [test-write_nif.R](testthat/test-write_nif.R#L15) | write_nif | write_nif basic functionality works | PASS | 6 | 0.016 |
| [test-write_nif.R](testthat/test-write_nif.R#L52) | write_nif | write_nif handles fixed-width format | PASS | 2 | 0.007 |
| [test-write_nif.R](testthat/test-write_nif.R#L73) | write_nif | write_nif handles empty dataframe | PASS | 2 | 0.007 |
| [test-write_nif.R](testthat/test-write_nif.R#L99) | write_nif | write_nif preserves column order | PASS | 1 | 0.005 |

</details>
<details>
<summary>
Session Info
</summary>

| Field    | Value                        |
|:---------|:-----------------------------|
| Version  | R version 4.4.3 (2025-02-28) |
| Platform | aarch64-apple-darwin20       |
| Running  | macOS Sequoia 15.4           |
| Language | en_US                        |
| Timezone | Europe/Berlin                |

| Package  | Version |
|:---------|:--------|
| testthat | 3.2.1.1 |
| covr     | 3.6.4   |
| covrpage | 0.2     |

</details>
<!--- Final Status : pass --->
