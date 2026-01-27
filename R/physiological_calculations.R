#' Calculate Body Mass Index (BMI)
#'
#' This function calculates BMI from height and weight values.
#' BMI is calculated as weight (kg) / (height (m))^2.
#'
#' @param height Height in centimeters. Can be a single value or a vector.
#' @param weight Weight in kilograms. Can be a single value or a vector.
#'
#' @return A numeric vector containing the calculated BMI values. NA values are
#'   returned for:
#'   - Missing (NA) height or weight values
#'   - Height or weight less than or equal to zero
#'   - Length mismatch between height and weight vectors
#'
#' @keywords internal
#' @noRd
calculate_bmi <- function(height, weight) {
  # Input validation
  if (!(is.numeric(height) || is.na(height)) ||
        !(is.numeric(weight) || is.na(weight))) {
    stop("Height and weight must be numeric values")
  }

  if (length(height) != length(weight)) {
    stop("Height and weight vectors must have the same length")
  }

  # Convert height to meters and calculate BMI
  # Use vectorized operations for better performance
  height_m <- height / 100
  valid_inputs <- !is.na(height) &
    !is.na(weight) &
    height > 0 &
    weight > 0

  result <- rep(NA_real_, length(height))
  result[valid_inputs] <- weight[valid_inputs] / (height_m[valid_inputs])^2
  result
}


#' Glomerular filtration rate estimation from serum creatinine (Raynaud method)
#'
#' Source: Raynaud M, et al., Race-free estimated glomerular
#' filtration rate equation in kidney transplant recipients: development and
#' validation study. BMJ. 2023 May 31;381:e073654,
#' <doi:10.1136/bmj-2022-073654>.
#'
#' \deqn{
#'  eGFR = e^{4.43 - 0.82 * ln(crea) - 0.012 * crea^2 - 0.006 * age + 0.18 *
#'  male}
#' }
#'
#' @param crea Serum creatinine in mg/dl or umol/l (if molar=TRUE).
#' @param age Age in years.
#' @param sex Sex encoded as number (male is 0) or character (male is "M").
#' @param race Race. Dummy variable for compatibility, race is not used by this
#'   method.
#' @param weight Body weight. Not used in this formula but included for
#' compatibility.
#' @param molar Switch to select whether the creatinine value is in mg/dl
#' (default) or umol/l units.
#' @seealso [nif::egfr_mdrd()]
#' @seealso [nif::egfr_cg()]
#'
#' @return Estimated GFR in ml/min/1.73 m^2.
#' @export
egfr_raynaud <- function(crea, age, sex, race = "", weight = NA,
                         molar = FALSE) {
  if (molar)
    crea <- crea / 88.4

  male <- ifelse((sex == 0) | (sex == "M"), 1, 0)
  egfr <- exp(4.4275492 - 0.8230475 * log(crea) - 0.0124264 * crea**2 -
                0.0055068 * age + 0.1806494 * male)
  base::attr(egfr, "unit") <- "ml/min/1.73 m^2"

  egfr
}


#' Serum creatinine estimation from eGFR (Raynaud method)
#'
#' Inverse of the function published in <doi:10.1136/bmj-2022-073654>.
#'
#' To convert crea from mg/dl to umol/l, multiply by 88.4.
#'
#' @param egfr EGFR in ml/min/1.73 m^2.
#' @param age Age in years.
#' @param sex Sex encoded as number (male is 0) or character (male is "M").
#' @param race Race. Dummy variable for compatibility, race is not used by this
#'   method.
#'
#' @importFrom pracma lambertWn
#' @importFrom pracma lambertWp
#' @seealso [nif::crea_mdrd()]
#'
#' @return Serum creatinine in mg/dl.
#' @export
crea_raynaud <- function(egfr, age, sex, race = "") {
  male <- ifelse((sex == 0) | (sex == "M"), 1, 0)
  a <- 4.4275492 - 0.0055068 * age + 0.1806494 * male
  b <- 0.8230475
  c <- 0.0124264

  z <- 2 * exp(2 * a / b) * c * egfr^(-2 / b) / b
  w <- pracma::lambertWp(z)
  crea <- 0.707107 * sqrt(b) * sqrt(w) / sqrt(c)
  base::attr(crea, "unit") <- "mg/dl"
  crea
}


#' Glomerular filtration rate estimation from serum creatinine (MDRD)
#'
#' Source: <doi:10.7326/0003-4819-130-6-199903160-00002>.
#'
#' \deqn{
#'  eGFR = 175 * crea^{1.154} * age^{0.203} * 0.742 (female) * 1.212 (black)
#' }
#'
#' @param crea Serum creatinine in mg/dl.
#' @param age Age in years.
#' @param sex Sex encoded as number (female is 1) or character (female is "F").
#' @param race Race as per CDISC nomenclature. Black race is identified as the
#'   occurrence of 'black' in the value.
#' @param molar Switch to select whether the creatinine value is in mg/dl
#' (default) or umol/l units.
#' @param weight Body weight. Not used in this formula but included for
#' compatibility.
#' @seealso [nif::egfr_raynaud()]
#' @seealso [nif::egfr_cg()]
#' @return Estimated GFR in ml/min/1.73 m^2.
#' @export
egfr_mdrd <- function(crea, age, sex, race = "", weight = NA, molar = FALSE) {
  if (molar) {
    crea <- crea / 88.4
  }
  female_factor <- ifelse((sex == 1) | (sex == "F"), .742, 1)
  race_factor <- ifelse(grepl("BLACK", toupper(race)), 1.212, 1)

  egfr <- 175 * crea^-1.154 * age^-0.203 * female_factor * race_factor
  base::attr(egfr, "unit") <- "ml/min/1.73 m^2"
  egfr
}


#' Serum creatinine estimation from eGFR (MDRD)
#'
#' Inverse of the function published in
#' <doi:10.7326/0003-4819-130-6-199903160-00002>.
#'
#' To convert crea from mg/dl to umol/l, multiply by 88.4.
#'
#' @param egfr EGFR in ml/min/1.73 m^2.
#' @param age Age in years.
#' @param sex Sex encoded as number (female is 1) or character (female is "F").
#' @param race Race as per CDISC nomenclature. Black race is identified as the
#'   occurrence of 'black' in the value.
#' @seealso [nif::crea_raynaud()]
#'
#' @return Serum creatinine in mg/dl.
#' @export
crea_mdrd <- function(egfr, age, sex, race = "") {
  female_factor <- ifelse((sex == 1) | (sex == "F"), .742, 1)
  race_factor <- ifelse(grepl("BLACK", toupper(race)), 1.212, 1)

  crea <- (egfr / (175 * age^-0.203 * female_factor * race_factor))^(1 / -1.154)
  base::attr(crea, "unit") <- "mg/dl"
  crea
}


#' Glomerular filtration rate estimation from serum creatinine (Cockcroft-Gault)
#'
#' Reference: Cockcroft and Gault, Nephron 1976, <doi:10.1159/000180580>.
#'
#' @param crea Serum creatinine in mg/dl.
#' @param age Age in years.
#' @param sex Sex encoded as number (female is 1) or character (female is "F").
#' @param race Race as per CDISC nomenclature. Black race is identified as the
#'   occurrence of 'black' in the value. For campatibility onla, not used in
#'   this formula.
#' @param weight Body weight in kg.
#' @param molar Switch to select whether the creatinine value is in mg/dl
#'   (default) or umol/l units.
#' @return Estimated GFR in ml/min (as body size is accounted for by the weight
#'   in the input).
#' @seealso [nif::egfr_raynaud]
#' @seealso [nif::egfr_mdrd()]
#' @export
egfr_cg <- function(crea, age, sex, race = "", weight = NA, molar = FALSE) {
  if (molar) {
    crea <- crea / 88.4
  }
  female_factor <- ifelse((sex == 1) | (sex == "F"), .85, 1)

  egfr <- (140 - age) * weight * female_factor / 72 / crea
  base::attr(egfr, "unit") <- "ml/min"
  egfr
}


#' Validate inputs to lbm functions
#'
#' @param weight weight.
#' @param height height.
#' @param sex sex.
#'
#' @return vector of valid inputs
#' @noRd
validate_lbw_parameters <- function(weight, height, sex) {
  if (!(is.numeric(height) || is.na(height)) ||
        !(is.numeric(weight) || is.na(weight))) {
    stop("Height and weight must be numeric values")
  }

  if (length(height) != length(weight) ||
        length(height) != length(sex)) {
    stop("Height and weight vectors must have the same length")
  }

  !is.na(height) &
    !is.na(weight) &
    !is.na(sex) &
    height > 0 &
    weight > 0 &
    sex %in% c(0, 1, "M", "F", "m", "f")
}


#' Test whether sex is male
#'
#' @param sex Vector of numbers or strings.
#'
#' @return logical.
#' @noRd
is_male <- function(sex) {
  ((sex == 0) | (toupper(sex) == "M")) & !is.na(sex)
}


#' Lean body mass (Boer formula)
#'
#' Source: Caruso D, et al., Lean Body Weight-Tailored Iodinated
#' Contrast Injection in Obese Patient: Boer versus James Formula. Biomed Res
#' Int. 2018 Aug 13;2018:8521893, <doi:10.1155/2018/8521893>.
#'
#' The Boer formula is one of the most commonly used formulas for estimating
#' lean body mass. It provides separate equations for males and females based on
#' height and weight measurements.
#'
#' @param weight Body weight in kg, as numeric.
#' @param height Body height in cm, as numeric.
#' @param sex Sex encoded as number (male is 0) or character (male is "M").
#' @seealso [nif::lbm_hume()]
#' @seealso [nif::lbm_peters()]
#'
#' @return Lean body mass in kg, as numeric. Returns NA for invalid inputs.
#' @export
lbm_boer <- function(weight, height, sex) {
  # Input validation
  valid_inputs <- validate_lbw_parameters(weight, height, sex)

  result <- rep(NA_real_, length(height))

  # Calculate LBM using Boer formula
  result[valid_inputs & is_male(sex)] <-
    (0.407 * weight[valid_inputs & is_male(sex)]) +
    (0.267 * height[valid_inputs & is_male(sex)]) - 19.2

  result[valid_inputs & !is_male(sex)] <-
    (0.252 * weight[valid_inputs & !is_male(sex)]) +
    (0.473 * height[valid_inputs & !is_male(sex)]) - 48.3

  result
}


#' Lean body mass (Hume formula)
#'
#' Source: Hume R. Prediction of lean body mass from height and weight. J Clin
#' Pathol. 1966 Jul;19(4):389-91, <doi:10.1136/jcp.19.4.389>.
#'
#' @param weight Body weight in kg, as numeric.
#' @param height Body height in cm, as numeric.
#' @param sex Sex encoded as number (male is 0) or character (male is "M").
#' @seealso [nif::lbm_boer()]
#' @seealso [nif::lbm_peters()]
#'
#' @return Lean body mass in kg, as numeric. Returns NA for invalid inputs.
#' @export
lbm_hume <- function(weight, height, sex) {
  # Input validation
  valid_inputs <- validate_lbw_parameters(weight, height, sex)

  result <- rep(NA_real_, length(height))

  result[valid_inputs & is_male(sex)] <-
    (0.32810 * weight[valid_inputs & is_male(sex)]) +
    (0.33929 * height[valid_inputs & is_male(sex)]) - 29.5336

  result[valid_inputs & !is_male(sex)] <-
    (0.29569 * weight[valid_inputs & !is_male(sex)]) +
    (0.41813 * height[valid_inputs & !is_male(sex)]) - 43.2933

  result
}


#' Lean body mass (Peters formula)
#'
#' Source: Peters AM, Snelling HL, Glass DM, Bird NJ. Estimation of lean body
#' mass in children. Br J Anaesth. 2011 May;106(5):719-23,
#' <doi:10.1093/bja/aer057>.
#' PMID: 21498495.
#'
#' @param weight Body weight in kg, as numeric.
#' @param height Body height in cm, as numeric.
#' @param sex Sex encoded as number (male is 0) or character (male is "M").
#' @seealso [nif::lbm_boer()]
#' @seealso [nif::lbm_hume()]
#'
#' @return Lean body mass in kg, as numeric. Returns NA for invalid inputs.
#' @export
lbm_peters <- function(weight, height, sex) {
  # Input validation
  valid_inputs <- validate_lbw_parameters(weight, height, sex)

  result <- rep(NA_real_, length(height))

  3.8 * (0.0215 * weight^0.6469 * height^0.7236)

  result[valid_inputs] <- 3.8 * (0.0215 * weight[valid_inputs]^0.6469 *
                                   height[valid_inputs]^0.7236)

  result
}
