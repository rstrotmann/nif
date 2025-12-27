#' Synthetic SDTM data from a fictional clinical SAD FIH study with rich PK
#' sampling
#' @format A sdtm object
#' @noRd
"examplinib_sad"

#' Synthetic SDTM data from a fictional clinical single-arm study with rich and
#' sparse pk sampling.
#' @format  A sdtm object
#' @noRd
"examplinib_poc"

#' Synthetic SDTM data from a fictional clinical food effect study for
#' 'examplinib'.
#' @format A sdtm object
#' @noRd
"examplinib_fe"

#' Synthetic NIF data from a fictional clinical SAD FIH study with rich PK
#' sampling
#' @format A NIF object
#' @noRd
"examplinib_sad_nif"

#' Synthetic NIF data from a fictional clinical single-arm study with rich and
#' sparse pk sampling.
#' @format A NIF object
#' @noRd
"examplinib_poc_nif"

#' Synthetic NIF data from a fictional clinical single-arm study with rich and
#' sparse pk sampling. Reduced to the minimally required fields.
#'
#' @format A NIF object
#' @noRd
"examplinib_poc_min_nif"

#' Synthetic NIF data from a fictional clinical SAD FIH study with rich PK
#' sampling. Reduced to the minimally required fields.
#'
#' @format A NIF object
#' @noRd
"examplinib_sad_min_nif"

#' Synthetic NIF data from a fictional clinical food effect study for
#' 'examplinib'.
#'
#' @format A NIF object
#' @noRd
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
