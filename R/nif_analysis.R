
#' Glomerular filtration rate estimation from serum creatinine (Raynaud method)
#'
#' Reference:
#' \href{https://doi.org/10.1136/bmj-2022-073654}{Raynaud, BMJ 2023;381:e073654}
#'
#' Internally, the function uses mg/dl units
#'
#' @param crea Serum creatinine in mg/dl or umol/l (if molar=TRUE).
#' @param age Age in years.
#' @param sex Sex encocded as number (male is 0) or character (male is "M").
#' @param race Race. Dummy variable for compatibility, race is not used by this
#'   method.
#' @param molar Switch to select whether the creatinine value is in mg/dl
#' (default) or umol/l units.
#'
#' @return Estimated GFR in ml/min/1.73 m^2.
#' @export
raynaud_egfr <- function(crea, age, sex, race="", molar=F) {
  if(molar) {
    crea <- crea / 88.4
  }
  male <- ifelse((sex==0) | (sex=="M"), 1, 0)
  egfr <- exp(4.4275492 - 0.8230475 * log(crea) - 0.0124264 * crea**2 -
                0.0055068 * age + 0.1806494 * male)
  attr(egfr, "unit") <- "ml/min/1.73 m^2"
  return(egfr)
}


#' Serum creatinine estimation from eGFR (Raynaud method)
#'
#' Inverse of the function published in
#' \href{https://doi.org/10.1136/bmj-2022-073654}{Raynaud, BMJ 2023;381:e073654}
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
#'
#' @return Serum creatinine in mg/dl.
#' @export
raynaud_crea <- function(egfr, age, sex, race="") {
  male <- ifelse((sex==0) | (sex=="M"), 1, 0)
  A = 4.4275492 - 0.0055068*age + 0.1806494*male
  B = 0.8230475
  C = 0.0124264

  z = 2*exp(2*A/B) * C*egfr^(-2/B) / B
  # if(z < -1/exp(1)) {
  #   stop("z is < -1/e")
  # }


  # if(z < 0) {
  #   W <- pracma::lambertWn(z)
  # } else {
  W <- pracma::lambertWp(z)
  # }
  crea <- 0.707107*sqrt(B)*sqrt(W)/sqrt(C)
  attr(crea, "unit") <- "mg/dl"
  return(crea)
}


#' Glomerular filtration rate estimation from serum creatinine (MDRD)
#'
#' Reference:
#' \href{https://www.kidney.org/content/mdrd-study-equation}{National Kidney Foundation}
#'
#' @param crea Serum creatinine in mg/dl.
#' @param age Age in years.
#' @param sex Sex encocded as number (female is 1) or character (female is "F").
#' @param race Race as per CDISC nomenclature. Black race is identified as the
#'   occurrence of 'black' in the value.
#' @param molar Switch to select whether the creatinine value is in mg/dl
#' (default) or umol/l units.
#' #'
#' @return Estimated GFR in ml/min/1.73 m^2.
#' @export
mdrd_egfr <- function(crea, age, sex, race="", molar=F) {
  if(molar) {
    crea <- crea / 88.4
  }
  female_factor <- ifelse((sex=1) | (sex=="F"), .742, 1)
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
#'
#' @return Serum creatinine in mg/dl.
#' @export
mdrd_crea <- function(egfr, age, sex, race="") {
  female_factor <- ifelse((sex=1) | (sex=="F"), .742, 1)
  race_factor <- ifelse(grepl("black", str_to_lower(race)), 1.212, 1)

  crea <- (egfr/(175 * age^-0.203 * female_factor * race_factor))^(1/-1.154)
  attr(crea, "unit") <- "mg/dl"
  return(crea)
}
