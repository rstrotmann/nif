#' SDTM class constructor, creating a sdtm object from a set of SDTM domains
#'
#' @param sdtm_data, a list of SDTM domains as data.frames
#' @import dplyr
#' @returns A sdtm object
#' @export
new_sdtm <- function(sdtm_data) {
  domains <- sdtm_data
  vs <- domains[["vs"]]
  ex <- domains[["ex"]]
  pc <- domains[["pc"]]
  dm <- domains[["dm"]]

  temp <- list(
    study = dm[1, "STUDYID"],
    subjects = pc %>%
      dplyr::distinct(USUBJID),
    specimens = pc %>%
      dplyr::distinct(PCSPEC),
    analytes = pc %>%
      dplyr::distinct(PCTEST, PCTESTCD),
    treatments = ex %>%
      dplyr::distinct(EXTRT),
    doses = ex %>%
      dplyr::distinct(EXDOSE),
    arms = dm %>%
      dplyr::distinct(ACTARM, ACTARMCD),
    domains = domains,
    pc = pc,
    dm = dm,
    ex = ex,
    vs = vs,
    analyte_mapping = data.frame(),
    metabolite_mapping = data.frame(),
    time_mapping = data.frame()
  )

  class(temp) <- "sdtm"
  temp
}



#' SDTM summary
#'
#' @param object A SDTM object.
#' @param ... Further parameters.
#'
#' @return A sdtm_summary object.
#' @export
summary.sdtm <- function(object, ...) {
  subjects <- object$domains[["pc"]] %>%
    filter(!is.na(PCSTRESN)) %>%
    dplyr::distinct(USUBJID) %>%
    pull(USUBJID) %>%
    as.character()

  out <- list(
    study = object$domains[["dm"]] %>%
      distinct(STUDYID) %>%
      pull(STUDYID) %>%
      as.character(),
    subjects = subjects,
    n_subs = length(subjects),
    pc_timepoints = object$domains[["pc"]] %>%
      dplyr::distinct(across(any_of(c("PCTPT", "PCTPTNUM")))),
    domains = names(object$domains),
    treatments = object$domains[["ex"]] %>%
      dplyr::distinct(EXTRT),
    arms = object$domains[["dm"]] %>%
      dplyr::distinct(ACTARM, ACTARMCD),
    doses = object$domains[["ex"]] %>%
      distinct(EXTRT, EXDOSE),
    specimems = object$domains[["pc"]] %>%
      dplyr::distinct(PCSPEC) %>%
      pull(PCSPEC) %>%
      as.character(),
    analytes = object$domains[["pc"]] %>%
      dplyr::distinct(PCTEST, PCTESTCD),
    analyte_mapping = object$analyte_mapping,
    metabolite_mapping = object$metabolite_mapping,
    time_mapping = object$time_mapping
  )
  class(out) <- "summary_sdtm"
  return(out)
}


#' print SDTM summary
#'
#' @param x SDTM object
#' @param ... Further parameters
#'
#' @return none
#' @export
print.summary_sdtm <- function(x, ...) {
  cat("SDTM data set summary\n\n")
  cat(paste("Study", x$study))
  cat(paste(" with", x$n_subs, "subjects providing PC data.\n"))
  cat("SDTM domains: ")
  cat(paste(x$domains, collapse = ", "))

  cat("\n\nArms:\n")
  print(x$arms, right = FALSE, justify = FALSE)

  cat("\nTreatments:\n")
  print(x$treatments %>% as.data.frame(), right = FALSE)
  cat("\nSpecimens:\n")
  print(x$specimens, right = FALSE)
  cat("\nAnalytes:\n")
  print(x$analytes %>% as.data.frame(), right = FALSE)

  cat("\nTreatment-to-analyte mappings:\n")
  if (nrow(x$analyte_mapping) > 0) {
    print(x$analyte_mapping, right = FALSE)
  } else {
    cat("none\n")
  }

  cat("\nParent-to-metabolite mappings:\n")
  if (nrow(x$metabolite_mapping) > 0) {
    print(x$metabolite_mapping, right = FALSE)
  } else {
    cat("none\n")
  }

  cat("\nTime mappings:\n")
  if (nrow(x$time_mapping) > 0) {
    print(x$time_mapping, right = FALSE)
  } else {
    cat("none\n")
  }

  invisible(x)
}



#' Attach a treatment-analyte mapping to an SDTM object
#'
#' In some studies, multiple drugs are co-administered, and there may be plasma
#' concentration data from different parent drugs.
#' In order to appropriately correlate observations with administrations, the
#' [make_nif()] algorithm needs to know which analyte (PCTESTCD within PC)
#' belongs to which drug (EXTRT within EX). If the respective names differ,
#' add_treatment_mapping() can be used to attach this information to the SDTM
#' object. Multiple mappings may be needed.
#'
#' @param obj A SDTM object.
#' @param extrt The treatment as defined in EX.
#' @param pctestcd The analyte as defined in PC.
#'
#' @seealso [make_nif()]
#' @examples
#' sdtm_object <- add_analyte_mapping(examplinib, "EXAMPLINIB", "RS2023")
#'
#' @export
add_analyte_mapping <- function(obj, extrt = "", pctestcd = "") {
  obj$analyte_mapping <- rbind(
    obj$analyte_mapping,
    data.frame("EXTRT" = extrt, "PCTESTCD" = pctestcd)
  )
  return(obj)
}


#' Add mapping method for legacy compatibility
#'
#' @param obj A SDTM object.
#' @param extrt The treatment as defined in EX.
#' @param pctestcd The analyte as defined in PC.
#' @description `r lifecycle::badge("deprecated")`
#' @export
add_mapping <- function(obj, extrt = "", pctestcd = "") {
  lifecycle::deprecate_warn("0.19.0", "add_mapping()", "add_analyte_mapping()")
  UseMethod("add_analyte_mapping")
}


#' Attach a parent-metabolite mapping to a SDTM object.
#'
#' In case multiple analytes are measured for a specific administered drug,
#' some functions need that information to correlate plasma concentrations
#' with administrations. 'add_metabolite_mapping()' is used to attach this
#' information to a SDTM object.
#'
#' @param obj The SDTM object.
#' @param pctestcd_parent The PCTESTCD of the parent compound.
#' @param pctestcd_metabolite The PCTESTCD of the metabolite.
#'
#' @return The SDTM object.
#' @export
add_metabolite_mapping <- function(
    obj,
    pctestcd_parent = "",
    pctestcd_metabolite = "") {
  obj$metabolite_mapping <- rbind(
    obj$metabolite_mapping,
    data.frame(
      "PCTESTCD_parent" = pctestcd_parent,
      "PCTESTCD_metab" = pctestcd_metabolite
    )
  )
  return(obj)
}


#' Attach a time mapping to an sdtm object
#'
#' The nominal time of observations in PC (in the field PCTPT) is not required
#' to follow a strict format and is in most cases provided as a composite
#' string. 'add_time_mapping()' can be used to explicitly define the nominal
#' observation times (in hours) for the values of PCTPT used in the PC domain.
#'
#' @param obj The SDTM object.
#' @param ... Mappings in the form '"<PCTPT>"=<NTIME>' with multiple mappings
#'   separated by commas. <PCTPT> corresponds to the value in the PCTPT fiels,
#'   and NTIME corresponds to the nominal time in hours.
#' @return The SDTM object
#' @export
#' @seealso [suggest()]
#' @examples
#' sdtm_object <- add_time_mapping(examplinib,
#'   "PREDOSE" = 0,
#'   "HOUR 0.5" = 0.5,
#'   "HOUR 1" = 1,
#'   "HOUR 1.5" = 1.5,
#'   "HOUR 2" = 2,
#'   "HOUR 3" = 3,
#'   "HOUR 4" = 4,
#'   "HOUR 6" = 6,
#'   "HOUR 8" = 8,
#'   "HOUR 10" = 10,
#'   "HOUR 12" = 12,
#'   "HOUR 24" = 24,
#'   "HOUR 48" = 48,
#'   "HOUR 72" = 72,
#'   "HOUR 96" = 96,
#'   "HOUR 144" = 144,
#'   "HOUR 168" = 168
#' )
add_time_mapping <- function(obj, ...) {
  UseMethod("add_time_mapping")
}


#' Attach a time mapping to an sdtm object
#'
#' The nominal time of observations in PC (in the field PCTPT) is not required
#' to follow a strict format and is in most cases provided as a composite
#' string. 'add_time_mapping()' can be used to explicitly define the nominal
#' observation times (in hours) for the values of PCTPT used in the PC domain.
#'
#' @param obj The SDTM object.
#' @param ... Mappings in the form '"<PCTPT>"=<NTIME>' with multiple mappings
#'   separated by commas. <PCTPT> corresponds to the value in the PCTPT fiels,
#'   and NTIME corresponds to the nominal time in hours.
#' @return The SDTM object
#' @export
#' @seealso [suggest()]
#' @examples
#' sdtm_object <- add_time_mapping(examplinib,
#'   "PREDOSE" = 0,
#'   "HOUR 0.5" = 0.5,
#'   "HOUR 1" = 1,
#'   "HOUR 1.5" = 1.5,
#'   "HOUR 2" = 2,
#'   "HOUR 3" = 3,
#'   "HOUR 4" = 4,
#'   "HOUR 6" = 6,
#'   "HOUR 8" = 8,
#'   "HOUR 10" = 10,
#'   "HOUR 12" = 12,
#'   "HOUR 24" = 24,
#'   "HOUR 48" = 48,
#'   "HOUR 72" = 72,
#'   "HOUR 96" = 96,
#'   "HOUR 144" = 144,
#'   "HOUR 168" = 168
#' )
add_time_mapping.sdtm <- function(obj, ...) {
  temp <- unlist(c(as.list(environment())[-1], list(...)))
  mapping <- data.frame(PCTPT = names(temp), NTIME = as.numeric(temp))
  obj$time_mapping <- rbind(obj$time_mapping, mapping)
  return(obj)
}


#' print() implementation for SDTM
#'
#' @param x A SDTM object.
#' @param ... Further parameters
#'
#' @export
print.sdtm <- function(x, ...) {
  print(summary(x))
}


#' Get a specific domain from a SDTM object
#'
#' @param obj The sdtm object.
#' @param name The domain to return.
#'
#' @return The specified domain as data.frame
#' @export
#' @examples
#' domain(examplinib, "dm")
domain <- function(obj, name) {
  obj$domains[[name]]
}


#' Subject information generic method
#'
#' @param obj The object, either an SDTM or NIF object.
#' @param id The ID or USUBJID as numeric or character.
#'
#' @export
subject_info <- function(obj, id) {
  UseMethod("subject_info")
}


#' Subject demographic information
#'
#' @param obj A SDTM object.
#' @param id The USUBJID.
#' @export
subject_info.sdtm <- function(obj, id) {
  temp <- obj$domains[["dm"]] %>%
    dplyr::filter(USUBJID %in% id) %>%
    as.list()
  class(temp) <- c("subject_info", "data.frame")
  return(temp)
}


#' Suggest common manual data programming steps for a sdtm data set
#'
#' @param obj A sdtm object
#' @seealso [read_sdtm_sas()]
#' @import dplyr
#' @export
suggest <- function(obj) {
  n_suggestion <- 1

  arms <- obj$dm %>%
    dplyr::filter(ACTARMCD != "") %>%
    dplyr::distinct(ACTARM, ACTARMCD)

  if (nrow(arms) > 1) {
    message(paste0(
      n_suggestion, ". There are ", nrow(arms),
      " arms defined in DM (see below).\n",
      "   Consider defining a PART or ARM variable in the nif dataset, \n",
      "   filtering for a particular arm, or defining a covariate based\n",
      "   on ACTARMCD.\n\n",
      df_to_string(arms, indent = "       "), "\n"
    ))
    n_suggestion <- n_suggestion + 1
  }

  specimems <- obj$pc %>%
    dplyr::filter(PCSPEC != "") %>%
    dplyr::distinct(PCSPEC)

  if (nrow(specimems) > 1) {
    message(paste0(
      n_suggestion, ". There are data from ", nrow(specimems),
      " different sample specimem types in PC\n",
      "   (see below).\n",
      "   Consider filtering for a specific specimem, or defining CMT",
      "accordingly.\n\n",
      df_to_string(specimems, indent = "       "), "\n"
    ))
    n_suggestion <- n_suggestion + 1
  }

  treatments <- obj$ex %>%
    dplyr::distinct(EXTRT)

  if (nrow(treatments) > 1) {
    message(paste0(
      n_suggestion, ". There are ", nrow(treatments), " different treatments ",
      "in EX (see below).\n",
      "   Consider filtering for a specific treatment.\n\n",
      df_to_string(treatments, indent = "       "), "\n"
    ))
    n_suggestion <- n_suggestion + 1
  }

  analytes <- obj$pc %>%
    dplyr::distinct(PCTESTCD) %>%
    dplyr::pull(PCTESTCD)

  no_analyte_treatments <- treatments %>%
    dplyr::mutate(no.analyte = !(EXTRT %in% analytes)) %>%
    dplyr::filter(no.analyte == TRUE) %>%
    dplyr::select(EXTRT)

  if (nrow(no_analyte_treatments) > 0) {
    message(paste0(
      n_suggestion,
      ". There are treatments (EXTRT) without analytes (PCTESTCD) of\n",
      "   the same name (see below).\n",
      "   Consider adding a treatment-analyte mapping to the sdtm object\n",
      "   See '?add_analyte_mapping' for additional information.\n\n",
      df_to_string(no_analyte_treatments, indent = "       "), "\n\n",
      "   Available analytes:\n\n",
      df_to_string(obj$pc %>% dplyr::distinct(PCTESTCD), indent = "       "), "\n"
    ))
    n_suggestion <- n_suggestion + 1
  }

  if (!("PCELTM" %in% names(obj$pc))) {
    line <- function(x) {
      return(paste0("     \"", x, "\" = 0,"))
    }
    temp <- paste(sapply(unique(obj$pc[, "PCTPT"]), line), collapse = "\n")

    message(paste0(
      n_suggestion,
      ". By default, 'make_nif()' derives the nominal sampling time from the ",
      "permissible field\n",
      "   PCELTM in PC. However, in this data set, PCELTM is not defined, and ",
      "the nominal time\n",
      "   must be manually derived from PCTPT. Please provide a time mapping ",
      "using 'add_time_mapping()',\n",
      "   e.g., by adding the below code after creating the sdtm object. ",
      "Obviously, you need to\n",
      "   replace the zeros with the respective time after admininstation in ",
      "hours:\n\n",
      "   %>% add_time_mapping(\n",
      substr(temp, 1, nchar(temp) - 1), ")\n"
    ))
    n_suggestion <- n_suggestion + 1
  }
}
