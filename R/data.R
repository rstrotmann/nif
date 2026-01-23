#' Synthetic SDTM data from a fictional clinical SAD FIH study with rich PK
#' sampling
#' @format A sdtm object
"examplinib_sad"

#' Synthetic SDTM data from a fictional clinical single-arm study with rich and
#' sparse pk sampling.
#' @format  A sdtm object
"examplinib_poc"

#' Synthetic SDTM data from a fictional clinical food effect study for
#' 'examplinib'.
#' @format A sdtm object
"examplinib_fe"

#' Synthetic NIF data from a fictional clinical SAD FIH study with rich PK
#' sampling
#' @format A NIF object
"examplinib_sad_nif"

#' Synthetic NIF data from a fictional clinical single-arm study with rich and
#' sparse pk sampling.
#' @format A NIF object
"examplinib_poc_nif"

#' Synthetic NIF data from a fictional clinical single-arm study with rich and
#' sparse pk sampling. Reduced to the minimally required fields.
#'
#' @format A NIF object
"examplinib_poc_min_nif"

#' Synthetic NIF data from a fictional clinical SAD FIH study with rich PK
#' sampling. Reduced to the minimally required fields.
#'
#' @format A NIF object
"examplinib_sad_min_nif"

#' Synthetic NIF data from a fictional clinical food effect study for
#' 'examplinib'.
#'
#' @format A NIF object
"examplinib_fe_nif"


#' SDTM Domain Model variables
#'
#' An overview on the variables included in the SDTM specification.
#'
#' @format
#' A data frame with 1969 rows and 6 columns.
#'
#' \describe{
#'  \item{DOMAIN}{Domain name}
#'  \item{VARNAM}{Variable name}
#'  \item{VARLABEL}{Long-form variable name}
#'  \item{DEFTYPE}{Variable type}
#'  \item{LENGTH}{Length of variable}
#'  \item{CORE}{Level of requiredness}
#' }
#'
#' @details
#' The first few lines of the data set for reference:
#'
#' \preformatted{
#' DOMAIN   VARNAM                            VARLABEL DEFTYPE LENGTH CORE
#'     TA  STUDYID                    Study Identifier    text     10  Req
#'     TA   DOMAIN                 Domain Abbreviation    text      2  Req
#'     TA    ARMCD                    Planned Arm Code    text     20  Req
#'     TA      ARM          Description of Planned Arm    text    100  Req
#'     TA  TAETORD Planned Order of Element within Arm integer      8  Req
#'     TA     ETCD                        Element Code    text      8  Req
#'     TA  ELEMENT              Description of Element    text    200 Perm
#'     TA TABRANCH                              Branch    text    200  Exp
#'     TA  TATRANS                     Transition Rule    text    200  Exp
#'     TA    EPOCH                               Epoch    text    100  Req
#'     ...
#' }
"domain_model"


#' Pharmaverse sample SDTM data
#'
#' An sdtm object based on the CDISCPILOT01 data set exported from the
#' 'pharmaversesdtm' package:
#'
#' Patil L, Bundfuss S, Dahnert K, Gautier F, Mancini E, Namai T, Nguyen V,
#' Shuliar V (2025).
#' _pharmaversesdtm: SDTM Test Data for the 'Pharmaverse'Family of Packages_.
#' R package version 1.3.1.9011, commit 55eba18f3d1d4b19e4fda886ff2da78582cae933,
#' <https://github.com/pharmaverse/pharmaversesdtm>.
#'
#' This is a fictional study with the title:
#' "Safety and Efficacy of the Xanomeline Transdermal Therapeutic System (TTS) in
#' Patients with Mild to Moderate Alzheimers Disease."
#'
#' @format ## `cdiscpilot01_sdtm`
#' A sdtm object with the following domains
#' \describe{
#' \item{dm}{Demographics}
#' \item{ex}{Exposure}
#' \item{vs}{Vital signs}
#' \item{pc}{Pharmacokinetic concentrations}
#' \item{pp}{Pharmacokinetic parameters}
#' \item{lb}{Laboratory data}
#' \item{ts}{Trial summary}
#' \item{eg}{ECG data}
#' \item{ae}{Adverse events}
#' }
#' @source <https://github.com/pharmaverse/pharmaversesdtma>
"cdiscpilot01_sdtm"
