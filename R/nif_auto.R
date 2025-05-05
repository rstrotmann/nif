#' Make analyte mapping table from formula object
#'
#' The formula object must be given as PCTESTCD ~ EXTRT or, if there are multiple
#' analytes from the same treatment: PCTESTCD1 + PCTESTCD2 ~ EXTRT, etc.
#'
#' The return object is a data frame with one line for each analyte. The ANALYTE
#' field is the PCTESTCD for the respective analyte.
#'
#' @param sdtm A sdtm object.
#' @param f A formula.
#' @param silent Suppress messages.
#' @importFrom rlang f_rhs
#' @importFrom rlang f_lhs
#' @importFrom rlang is_formula
#'
#' @returns A data frame.
formula_to_mapping <- function(sdtm, f, silent = NULL) {
  # input validation
  if (!inherits(sdtm, "sdtm")) {
    stop("sdtm must be an sdtm object")
  }

  if(!rlang::is_formula(f))
    stop(paste0(
      "argument ", as.character(f), " is not a formula!"))

  extrt <- intersect(str_split_1(deparse(f_rhs(f)), " "), treatments(sdtm))

  if(length(extrt) == 0)
    stop(paste0("extrt ", as.character(rlang::f_rhs(f)),
                " not found in EX!"))

  allowed_testcd <- testcd(sdtm)

  # identify analytes from left hand side of f
  temp <- str_split_1(deparse(f_lhs(f)), " ")
  temp <- temp[grepl("^[A-Z0-9_]*$", temp)]

  invalid_analytes <- setdiff(temp, allowed_testcd$TESTCD)
  n_invalid <- length(invalid_analytes)
  if(n_invalid > 0)
    conditional_message(
      plural("analyte", n_invalid > 1), " ",
      invalid_analytes, " not found in any domain!",
      silent = silent
    )

  testcd <- testcd(sdtm) %>%
    filter(TESTCD %in% temp)

  out <- testcd %>%
    mutate(PARAM = paste0(DOMAIN, "TESTCD")) %>%
    mutate(EXTRT = extrt) %>%
    mutate(ANALYTE = TESTCD)

  return(out)
}


#' Create analyte mapping table from mapping formulae
#'
#' Creates a mapping between treatments (EXTRT) and analytes (PCTESTCD). Each
#' combination of EXTRT and PCTESTCD must be unique. If duplicate mappings are
#' found, an error will be raised.
#'
#' @param sdtm A sdtm object.
#' @param ... Analyte mappings as formulae.
#'
#' @importFrom rlang is_formula
#' @importFrom dplyr bind_rows
#' @importFrom dplyr distinct
#'
#' @returns A data frame with unique combinations of EXTRT and PCTESTCD.
auto_mapping <- function(sdtm, ...) {
  # input validation
  if (!inherits(sdtm, "sdtm")) {
    stop("sdtm must be an sdtm object")
  }

  mapping = list(...)

  if(length(mapping) != 0) {
    temp <- sapply(mapping, is_formula)
    if(!all(temp) == TRUE)
      stop("mappings must be formulae!")
  }

  # Initialize empty data frame with correct structure
  ex_pc_mapping <- data.frame(
    DOMAIN = character(0),
    PARAM = character(0),
    TESTCD = character(0),
    EXTRT = character(0),
    ANALYTE = character(0),
    stringsAsFactors = FALSE
  )

  if(length(mapping) == 0) {
    # auto mapping: PC analytes
    common <- intersect(analytes(sdtm), treatments(sdtm))
    if(length(common) == 0)
      stop(paste0(
        "Cannot autogenerate mapping, no matching entries between analytes (",
        analytes(sdtm),
        ") and treatments (",
        treatments(sdtm),
        "). Provide mapping formula!\n"))
    ex_pc_mapping <- data.frame(
      DOMAIN = "PC",
      PARAM = "PCTESTCD",
      TESTCD = common,
      EXTRT = common,
      ANALYTE = common,
      stringsAsFactors = FALSE
    )
  } else {
    for(i in 1:length(mapping)) {
      f <- mapping[[i]]
      if(rlang::is_formula(f)) {
        ex_pc_mapping <- bind_rows(
          ex_pc_mapping, formula_to_mapping(sdtm, f)
        )
      }
    }
  }

  # Check for duplicate mappings
  duplicates <- ex_pc_mapping %>%
    group_by(EXTRT, TESTCD) %>%
    filter(n() > 1) %>%
    distinct(EXTRT, TESTCD)

  if (nrow(duplicates) > 0) {
    stop("Duplicate mappings found for the following EXTRT-TESTCD combinations:\n",
         paste(sprintf("%s ~ %s", duplicates$EXTRT, duplicates$TESTCD),
               collapse = "\n"))
  }

  out <- ex_pc_mapping %>%
    group_by(EXTRT) %>%
    mutate(PARENT = ANALYTE[row_number() == 1]) %>%
    mutate(METABOLITE = row_number() != 1) %>%
    ungroup()

  return(out)
}






#' Auto-generate nif from sdtm object
#'
#' If no mapping is provided, [nif_auto()] will try to find matching treatment
#' names in EX and PCTESTCD in PC to create a basic nif object for pharmacokinetic
#' observations. It will fail if no EXTRT and PCTESTCD with identical names are
#' found.
#'
#' If one or more mappings are provided as additional parameters, [nif_auto()]
#' will create respective observations. Mappings must be provided in the form:
#'
#' `TESTCD ~ EXTRT`
#'
#' where TESTCD can be any xxTESTCD from any domain xx included in the sdtm
#' object. See [testcd()] for an overview
#' on all analytes in all domains. Formulae can also specify multiple
#' observations to be associated with one treatment, e.g.,
#'
#' `RS2023 + RS2023487A ~ EXAMPLINIB`.
#'
#' Multiple mappings can be given, separated by commas. The analyte name in the
#' nif object is automatically set to the respective TESTCD.
#'
#' __IMPORTANT__: If multiple analytes are specified for a treatment, it is
#' implicitly assumed that the first analyte corresponds to the parent analyte
#' for the treatment, e.g., in the case above, 'RS2023' is assumed to be the
#' analyte for the 'EXAMPLINIB' treatment parent compound.
#'
#' Note that compartments (CMT) are automatically assigned. If more control over
#' compartments, analyte names and parent assignment is desired, stepwise
#' creation of a nif object using [add_administration()] and [add_observation()]
#' is recommended.
#'
#' @inheritParams add_observation
#' @param ... Formulae to define the relationships between PCTESTCD and EXTRT.
#'
#' @returns A nif object
#'
#' @seealso [testcd()]
#' @seealso [add_administration()]
#' @seealso [add_observation()]
#'
#' @description
#' `r lifecycle::badge("experimental")`
#'
#' @export
#'
#' @examples
#' nif_auto(examplinib_sad, RS2023 ~ EXAMPLINIB)
#' nif_auto(examplinib_sad, RS2023 + RS2023487A ~ EXAMPLINIB)
#' nif_auto(examplinib_sad, RS2023 + WEIGHT ~ EXAMPLINIB)
nif_auto <- function(
    sdtm, ...,
    subject_filter = "!ACTARMCD %in% c('SCRNFAIL', 'NOTTRT')",
    observation_filter = "TRUE",
    baseline_filter = "LBBLFL == 'Y'",
    duplicates = "resolve",
    duplicate_function = mean,
    keep = NULL,
    silent = NULL) {
  # input validation
  if (!inherits(sdtm, "sdtm")) {
    stop("sdtm must be an sdtm object")
  }

  expected_domains <- c("dm", "vs", "ex", "pc")
  missing_domains <- setdiff(expected_domains, names(sdtm$domains))
  if(length(missing_domains) > 0)
    stop(paste0("Missing domains: ", nice_enumeration(missing_domains)))

  analyte_mapping <- auto_mapping(sdtm, ...)
  if(nrow(analyte_mapping) == 0)
    stop("Analyte mapping could not be autogenerated!")

  out <- new_nif()

  # treatments
  # treatments <- distinct(analyte_mapping, EXTRT, ANALYTE)
  treatments <- analyte_mapping %>%
    filter(METABOLITE == FALSE) %>%
    distinct(EXTRT, ANALYTE)

  for(i in 1:nrow(treatments)) {
    t <- treatments[i,]
    out <- out %>%
      add_administration(
        sdtm, extrt = t$EXTRT, analyte = t$ANALYTE,
        subject_filter = subject_filter,
        silent = silent)
  }

  # observations
  for(i in 1:nrow(analyte_mapping)){
    o <- analyte_mapping[i,]
    out <- out %>%
      add_observation(
        sdtm, o$DOMAIN, o$TESTCD, analyte = o$ANALYTE,
        parent = o$PARENT, metabolite = o$METABOLITE,
        subject_filter = subject_filter,
        observation_filter = observation_filter,
        duplicates = duplicates,
        duplicate_function = duplicate_function,
        keep = keep,
        silent = silent)
  }

  # LB-related baseline covariates
  if("lb" %in% names(sdtm$domains)) {
    lb <- domain(sdtm, "lb")

      # baseline renal function
      if("CREAT" %in% unique(lb$LBTESTCD)) {
        conditional_message("Adding baseline CREAT", silent = silent)
        out <- add_baseline(
          out, sdtm, "lb", "CREAT",
          baseline_filter = baseline_filter)

        if(all(c("BL_CREAT", "AGE", "SEX", "RACE", "WEIGHT") %in% names(out))) {
          out <- add_bl_crcl(out)
          out <- add_bl_renal(out)
          conditional_message(
            "Adding baseline CRCL and renal function class",
            silent = silent)
        }
      }

      # baseline hepatic function
      if(all(c("BILI", "AST") %in% unique(lb$LBTESTCD))) {
        conditional_message("Adding baseline hepatic function", silent = silent)
        out <- add_bl_odwg(
          out, sdtm,
          baseline_filter = baseline_filter)
      }
    # }
  }

  return(out)
}













