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
#' _pharmaversesdtm: SDTM Test Data for the 'Pharmaverse' Family of Packages_.
#' <https://github.com/pharmaverse/pharmaversesdtm>.
#'
#' @format
#' sdtm object with 9 domains:
#'
#' \preformatted{
#' -------- SDTM data set summary --------
#' Study CDISCPILOT01
#'
#' Safety and Efficacy of the Xanomeline Transdermal Therapeutic System (TTS) in
#' Patients with Mild to Moderate Alzheimers Disease.
#'
#' Data disposition
#'   DOMAIN   SUBJECTS   OBSERVATIONS
#'   dm       306        306
#'   ex       254        591
#'   vs       254        29643
#'   pc       254        4572
#'   pp       168        2688
#'   lb       254        59580
#'   ts       0          0
#'   eg       254        26717
#'   ae       225        1191
#'
#' Arms (DM):
#'   ACTARMCD   ACTARM
#'   Pbo        Placebo
#'   Scrnfail   Screen Failure
#'   Xan_Hi     Xanomeline High Dose
#'   Xan_Lo     Xanomeline Low Dose
#'
#' Treatments (EX):
#'   PLACEBO, XANOMELINE
#'
#' PK sample specimens (PC):
#'   PLASMA, URINE
#'
#' PK analytes (PC):
#'   PCTEST       PCTESTCD
#'   XANOMELINE   XAN
#' }
#'
#' @source <https://github.com/pharmaverse/pharmaversesdtm>
"cdiscpilot01_sdtm"



#' Pharmaverse sample nif object
#'
#' A nif object based on the CDISCPILOT01 data set exported from the
#' 'pharmaversesdtm' package including administration events for XAN and
#' PLACEBO, and observations for XAN, ALT and HGB.
#'
#' \preformatted{
#' ----- NONMEM Input Format (NIF) data -----
#' 8988 observations from 254 subjects across 1 study
#' Analytes: ALT, HGB, CFB_HGB, XAN and PLACEBO
#' 111 males (43.7%), 143 females (56.3%)
#'
#' Columns:
#'   REF, ID, STUDYID, USUBJID, AGE, SEX, RACE, WEIGHT, DTC, TIME, NTIME, TAFD,
#'   TAD, EVID, AMT, ANALYTE, CMT, PARENT, TRTDY, METABOLITE, DOSE, MDV, ACTARMCD,
#'   IMPUTATION, DV, BL_CREAT, BL_CRCL, BL_RENAL, DL
#'
#' Hash: f36650cc3aa5ecdad143a87e418b1439
#'
#' Data (selected columns):
#'   ID   NTIME   TIME      TAD       ANALYTE   EVID   CMT   AMT   DOSE   DV
#'   1    -192    0         -153.25   ALT       0      3     0     0      27
#'   1    -192    0         -153.25   HGB       0      4     0     0      8.875
#'   1    -192    0         -153.25   CFB_HGB   0      5     0     0      0
#'   1    NA      152.75    -0.5      XAN       0      2     0     0      0
#'   1    0       153.25    0         PLACEBO   1      1     0     0      NA
#'   1    0.083   153.333   0.083     XAN       0      2     0     0      NA
#'   1    0.5     153.75    0.5       XAN       0      2     0     0      NA
#'   1    NA      154.25    1         XAN       0      2     0     0      NA
#'   1    NA      154.75    1.5       XAN       0      2     0     0      NA
#'   1    NA      155.25    2         XAN       0      2     0     0      NA
#' 38465 more rows
#' }
#'
#' @format
#' nif object with 38475 rows and 29 columns:
#'
#' \describe{
#'  \item{REF}{row number}
#'  \item{ID}{subject ID}
#'  \item{STUDYID}{study ID}
#'  \item{USUBJID}{unique subject ID}
#'  \item{AGE}{age in years}
#'  \item{SEX}{sex (0: male, 1, female)}
#'  \item{RACE}{race}
#'  \item{WEIGHT}{baseline body weight}
#'  \item{DTC}{date time code for event}
#'  \item{TIME}{time in hours}
#'  \item{NTIME}{nominal time for event in hours}
#'  \item{TAFD}{time after individual first dose}
#'  \item{TAD}{time after last dose}
#'  \item{EVID}{event ID (0: observation, 1: administration)}
#'  \item{AMT}{amount in mg}
#'  \item{ANALYTE}{analyte name}
#'  \item{CMT}{analyte compartment}
#'  \item{PARENT}{parent analyte}
#'  \item{TRTDY}{treatment day}
#'  \item{METABOLITE}{metabolite (0: no, 1: yes)}
#'  \item{DOSE}{dose in mg}
#'  \item{MDV}{missing dependent variable}
#'  \item{ACTARMCD}{actual arm code}
#'  \item{IMPUTATION}{imputation note}
#'  \item{DV}{dependent variable}
#'  \item{BL_CREAT}{baseline creatinine in umol/l}
#'  \item{BL_CRCL}{baseline creatinine clearance in ml/min}
#'  \item{BL_RENAL}{baseline renal function category}
#'  \item{DL}{dose level in mg}
#'  }
#'
#' @source Created from <https://github.com/pharmaverse/pharmaversesdtm>
"cdiscpilot01_nif"
