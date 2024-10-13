#' Glomerular filtration rate estimation from serum creatinine (Raynaud method)
#'
#' Source: Raynaud M, et al., Race-free estimated glomerular
#' filtration rate equation in kidney transplant recipients: development and
#' validation study. BMJ. 2023 May 31;381:e073654.
#' [doi: 10.1136/bmj-2022-073654](https://doi.org/10.1136/bmj-2022-073654).
#' PMID: 37257905; PMCID: PMC10231444.
#'
#' \deqn{
#'  eGFR = e^{4.43 - 0.82 * ln(crea) - 0.012 * crea^2 - 0.006 * age + 0.18 * male}
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
#' @seealso [efgr_mdrd()]
#' @seealso [egfr_cg()]
#'
#' @return Estimated GFR in ml/min/1.73 m^2.
#' @export
egfr_raynaud <- function(crea, age, sex, race = "", weight = NA, molar = F) {
  if (molar) {
    crea <- crea / 88.4
  }
  male <- ifelse((sex == 0) | (sex == "M"), 1, 0)
  egfr <- exp(4.4275492 - 0.8230475 * log(crea) - 0.0124264 * crea**2 -
    0.0055068 * age + 0.1806494 * male)
  attr(egfr, "unit") <- "ml/min/1.73 m^2"
  return(egfr)
}


#' Serum creatinine estimation from eGFR (Raynaud method)
#'
#' Inverse of the function published in
#' [doi: 10.1136/bmj-2022-073654](https://doi.org/10.1136/bmj-2022-073654).
#'
#' To convert crea from mg/dl to umol/l, multiply by 88.4.
#'
#' @param egfr EGFR in ml/min/1.73 m^2.
#' @param age Age in years.
#' @param sex Sex encocded as number (male is 0) or character (male is "M").
#' @param race Race. Dummy variable for compatibility, race is not used by this
#'   method.
#'
#' @importFrom pracma lambertWn
#' @importFrom pracma lambertWp
#' @seealso [crea_mdrd()]
#'
#' @return Serum creatinine in mg/dl.
#' @export
crea_raynaud <- function(egfr, age, sex, race = "") {
  male <- ifelse((sex == 0) | (sex == "M"), 1, 0)
  A <- 4.4275492 - 0.0055068 * age + 0.1806494 * male
  B <- 0.8230475
  C <- 0.0124264

  z <- 2 * exp(2 * A / B) * C * egfr^(-2 / B) / B
  # if(z < -1/exp(1)) {
  #   stop("z is < -1/e")
  # }

  # if(z < 0) {
  #   W <- pracma::lambertWn(z)
  # } else {
  W <- pracma::lambertWp(z)
  # }
  crea <- 0.707107 * sqrt(B) * sqrt(W) / sqrt(C)
  attr(crea, "unit") <- "mg/dl"
  return(crea)
}


#' Glomerular filtration rate estimation from serum creatinine (MDRD)
#'
#' Source:
#' [National Kidney Foundation](https://www.kidney.org/content/mdrd-study-equation)
#'
#' \deqn{
#'  eGFR = 175 * crea^{1.154} * age^{0.203} * 0.742 (female) * 1.212 (black)
#' }
#'
#' @param crea Serum creatinine in mg/dl.
#' @param age Age in years.
#' @param sex Sex encocded as number (female is 1) or character (female is "F").
#' @param race Race as per CDISC nomenclature. Black race is identified as the
#'   occurrence of 'black' in the value.
#' @param molar Switch to select whether the creatinine value is in mg/dl
#' (default) or umol/l units.
#' @param weight Body weight. Not used in this formula but included for
#' compatibility.
#' @seealso [egfr_raynaud()]
#' @seealso [egfr_cg()]
#' @return Estimated GFR in ml/min/1.73 m^2.
#' @export
egfr_mdrd <- function(crea, age, sex, race = "", weight = NA, molar = F) {
  if (molar) {
    crea <- crea / 88.4
  }
  female_factor <- ifelse((sex <- 1) | (sex == "F"), .742, 1)
  race_factor <- ifelse(grepl("black", str_to_lower(race)), 1.212, 1)

  egfr <- 175 * crea^-1.154 * age^-0.203 * female_factor * race_factor
  attr(egfr, "unit") <- "ml/min/1.73 m^2"
  return(egfr)
}


#' Serum creatinine estimation from eGFR (MDRD)
#'
#' Inverse of the function published in
#' \href{https://www.kidney.org/content/mdrd-study-equation}{National Kidney Foundation}
#'
#' To convert crea from mg/dl to umol/l, multiply by 88.4.
#'
#' @param egfr EGFR in ml/min/1.73 m^2.
#' @param age Age in years.
#' @param sex Sex encocded as number (female is 1) or character (female is "F").
#' @param race Race as per CDISC nomenclature. Black race is identified as the
#'   occurrence of 'black' in the value.
#' @seealso [crea_raynaud()]
#'
#' @return Serum creatinine in mg/dl.
#' @export
crea_mdrd <- function(egfr, age, sex, race = "") {
  female_factor <- ifelse((sex <- 1) | (sex == "F"), .742, 1)
  race_factor <- ifelse(grepl("black", str_to_lower(race)), 1.212, 1)

  crea <- (egfr / (175 * age^-0.203 * female_factor * race_factor))^(1 / -1.154)
  attr(crea, "unit") <- "mg/dl"
  return(crea)
}


#' Glomerular filtration rate estimation from serum creatinine (Cockcroft-Gault)
#'
#' Reference:
#' \href{https://doi.org/10.1159/000180580}{Cockcroft and Gault, Nephron 1976}
#'
#' @param crea Serum creatinine in mg/dl.
#' @param age Age in years.
#' @param sex Sex encocded as number (female is 1) or character (female is "F").
#' @param race Race as per CDISC nomenclature. Black race is identified as the
#'   occurrence of 'black' in the value. For campatibility onla, not used in
#'   this formula.
#' @param weight Body weight in kg.
#' @param molar Switch to select whether the creatinine value is in mg/dl
#'   (default) or umol/l units.
#' @return Estimated GFR in ml/min (as body size is accounted for by the weight
#'   in the input).
#' @seealso [egfr_raynaud]
#' @seealso [egfr_mdrd()]
#' @export
egfr_cg <- function(crea, age, sex, race = "", weight = NA, molar = F) {
  if (molar) {
    crea <- crea / 88.4
  }
  female_factor <- ifelse((sex <- 1) | (sex == "F"), .85, 1)

  egfr <- (140 - age) * weight * female_factor / 72 / crea
  attr(egfr, "unit") <- "ml/min"
  return(egfr)
}


# egfr_ckd_epi <- function(crea, age, sex, race="", weight=NA, molar=F) {
#   black <- ifelse(grepl("black", str_to_lower(race)), T, F)
#   female <- ifelse((sex=1) | (sex=="F"), T, F)
#   age_factor <- 0.993^age
#
#   if(male & !black) {
#
#   }
# }


#' Lean body mass (Boer formula)
#'
#' Source: Caruso D, et al., Lean Body Weight-Tailored Iodinated
#' Contrast Injection in Obese Patient: Boer versus James Formula. Biomed Res
#' Int. 2018 Aug 13;2018:8521893.
#' [doi: 10.1155/2018/8521893](https://doi.org/10.1155/2018/8521893).
#' PMID: 30186869; PMCID: PMC6110034.
#'
#' @param weight Body weight in kg, as numeric.
#' @param height Body height in cm, as numeric.
#' @param sex Sex encoded as number (male is 0) or character (male is "M").
#' @seealso [lbm_hume()]
#' @seealso [lbm_peters()]
#'
#' @return Lean body mass in kg, as numeric.
#' @export
lbm_boer <- function(weight, height, sex) {
  ifelse((sex == "M") | (sex == 0),
    (0.407 * weight) + (0.267 * height) - 19.2,
    (0.252 * weight) + (0.473 * height) - 48.3
  )
}


#' Lean body mass (Hume formula)
#'
#' Source: Hume R. Prediction of lean body mass from height and weight. J Clin
#' Pathol. 1966 Jul;19(4):389-91.
#' [doi: 10.1136/jcp.19.4.389](https://doi.org/10.1136/jcp.19.4.389).
#' PMID: 5929341; PMCID: PMC473290.
#'
#' @param weight Body weight in kg, as numeric.
#' @param height Body height in cm, as numeric.
#' @param sex Sex encoded as number (male is 0) or character (male is "M").
#' @seealso [lbm_boer()]
#' @seealso [lbm_peters()]
#'
#' @return Lean body mass in kg, as numeric.
#' @export
lbm_hume <- function(weight, height, sex) {
  ifelse((sex == "M") | (sex == 0),
    (0.32810 * weight) + (0.33929 * height) - 29.5336,
    (0.29569 * weight) + (0.41813 * height) - 43.2933
  )
}


#' Lean body mass (Peters formula)
#'
#' Source: Peters AM, Snelling HL, Glass DM, Bird NJ. Estimation of lean body
#' mass in children. Br J Anaesth. 2011 May;106(5):719-23.
#' [doi: 10.1093/bja/aer057](https://doi.org/10.1093/bja/aer057).
#' PMID: 21498495.
#'
#' @param weight Body weight in kg, as numeric.
#' @param height Body height in cm, as numeric.
#' @param sex Sex encoded as number (male is 0) or character (male is "M").
#' @seealso [lbm_boer()]
#' @seealso [lbm_hume()]
#'
#' @return Lean body mass in kg, as numeric.
#' @export
lbm_peters <- function(weight, height, sex) {
  3.8 * (0.0215 * weight**0.6469 * height**0.7236)
}
