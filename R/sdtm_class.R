
#' SDTM class constructor, creating a sdtm object from a set of SDTM domains
#'
#' @param sdtm.data, a list of SDTM domains as data.frames
#' @import dplyr
#' @returns A sdtm object
#' @export
new_sdtm <- function(sdtm.data){
  domains <- sdtm.data
  vs <- domains[["vs"]]
  ex <- domains[["ex"]]
  pc <- domains[["pc"]]
  dm <- domains[["dm"]]

  temp <- list(
    study=dm[1,"STUDYID"],
    subjects=pc %>%
      dplyr::distinct(USUBJID),
    specimens=pc %>%
      dplyr::distinct(PCSPEC),
    analytes=pc %>%
      dplyr::distinct(PCTEST, PCTESTCD),
    treatments=ex %>%
      dplyr::distinct(EXTRT),
    doses=ex %>%
      dplyr::distinct(EXDOSE),
    arms=dm %>%
      dplyr::distinct(ACTARM, ACTARMCD),
    domains=domains,
    pc=pc,
    dm=dm,
    ex=ex,
    vs=vs,
    analyte_mapping=data.frame(),
    metabolite_mapping=data.frame(),
    time_mapping=data.frame()
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
  subjects = object$domains[["pc"]] %>%
    dplyr::distinct(USUBJID) %>%
    pull(USUBJID) %>%
    as.character()

  out <- list(
    study = object$domains[["dm"]] %>%
      distinct(STUDYID) %>%
      pull(STUDYID) %>% as.character(),
    subjects = subjects,
    n_subs = length(subjects),
    domains=names(object$domains),
    arms = object$domains[["dm"]] %>%
      dplyr::distinct(ACTARM, ACTARMCD),
    doses = object$domains[["ex"]] %>%
      distinct(EXTRT, EXDOSE),
    specimems = object$domains[["pc"]] %>%
      dplyr::distinct(PCSPEC) %>%
      pull(PCSPEC) %>% as.character(),
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
  cat(paste(x$domains, collapse=", "))

  cat("\n\nArms:\n")
  print(x$arms, right=FALSE, justify=FALSE)

  cat("\nTreatments:\n")
  print(x$treatments %>% as.data.frame(), right=FALSE)
  cat("\nSpecimens:\n")
  print(x$specimens, right=FALSE)
  cat("\nAnalytes:\n")
  print(x$analytes, right=FALSE)

  cat("\nTreatment-to-analyte mappings:\n")
  if(nrow(x$analyte_mapping)>0){
    print(x$analyte_mapping, right=FALSE)
  } else {
    cat("none\n")
  }

  cat("\nParent-to-metabolite mappings:\n")
  if(nrow(x$metabolite_mapping)>0){
    print(x$metabolite_mapping, right=FALSE)
  } else {
    cat("none\n")
  }

  cat("\nTime mappings:\n")
  if(nrow(x$time_mapping)>0){
    print(x$time_mapping, right=FALSE)
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
#' [make_nif()] algorithm needs to know which analyte (PCTESTCD within PC) belongs
#' to which drug (EXTRT within EX). If the respective names differ,
#' add_treatment_mapping() can be used to attach this information to the SDTM
#' object. Multiple mappings may be needed.
#'
#' @param obj A SDTM object.
#' @param extrt The treatment as defined in EX.
#' @param pctestcd The analyte as defined in PC.
#' @seealso [make_nif()]
#' @examples
#' sdtm_object <- add_analyte_mapping(examplinib, "EXAMPLINIB", "RS2023")
#'
#' @export
add_analyte_mapping <- function(obj, extrt="", pctestcd="") {
  #UseMethod("add_analyte_mapping")
  obj$analyte_mapping <- rbind(
    obj$analyte_mapping,
    data.frame("EXTRT"=extrt, "PCTESTCD"=pctestcd))
  return(obj)
}

#' Add mapping method for legacy compatibility
#'
#' @param obj A SDTM object.
#' @param extrt The treatment as defined in EX.
#' @param pctestcd The analyte as defined in PC.
#' @description `r lifecycle::badge("deprecated")`
#' @export
add_mapping <- function(obj, extrt="", pctestcd="") {
  lifecycle::deprecate_warn("0.19.0", "add_mapping()", "add_analyte_mapping()")
  UseMethod("add_analyte_mapping")
}


#' Attach a parent-metabolite mapping to a SDTM object.
#'
#' @param obj The SDTM object.
#' @param pctestcd_parent The PCTESTCD of the parent compound.
#' @param pctestcd_metabolite The PCTESTCD of the metabolite.
#'
#' @return The SDTM object.
#' @export
add_metabolite_mapping <- function(
    obj,
    pctestcd_parent="",
    pctestcd_metabolite=""
    ) {
  obj$metabolite_mapping <- rbind(
    obj$metabolite_mapping,
    data.frame("PCTESTCD_parent"=pctestcd_parent, "PCTESTCD_metab"=pctestcd_metabolite))
  return(obj)
}


#' Attach a time mapping to an sdtm object
#'
#' @param obj The sdtm object.
#' @param ... Mappings in the form '"<PCTPT>"=<NTIME>' with multiple mappings
#'   separated by commas. <PCTPT> corresponds to the value in the PCTPT fiels,
#'   and NTIME corresponds to the nominal time in hours.
#' @return The modified sdtm object
#' @export
#' @examples
#' sdtm_object <- add_time_mapping(examplinib, "PREDOSE" = 0,
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
#'   "HOUR 168" = 168)
#'
#' @seealso [suggest()]
add_time_mapping <- function(obj, ...) {
  UseMethod("add_time_mapping")
}

#' Attach a time mapping to an sdtm object
#'
#' @param obj The sdtm object.
#' @param ... Mappings in the form '"<PCTPT>"=<NTIME>' with multiple mappings
#'   separated by commas. <PCTPT> corresponds to the value in the PCTPT fiels,
#'   and NTIME corresponds to the nominal time in hours.
#' @return The modified sdtm object
#' @export
#' @seealso [suggest()]
add_time_mapping.sdtm <- function(obj, ...) {
  temp <- unlist(c(as.list(environment())[-1], list(...)))
  mapping <- data.frame(PCTPT=names(temp), NTIME=as.numeric(temp))
  obj$time_mapping <- rbind(obj$time_mapping, mapping)
  return(obj)
}


#' print() implementation for SDTM
#'
#' @param x A SDTM object.
#' @param ... Further parameters
#'
#' @export
print.sdtm <- function(x, ...){
  # cat("SDTM data set\n")
  # cat(paste("Study", x$study))
  # cat(paste(" with", nrow(x$subjects), "subjects providing PC data.\n"))
  # cat("SDTM domains: ")
  # cat(paste(names(x$domains), collapse=", "))
  #
  # cat("\n\nArms:\n")
  # print(x$arms, right=FALSE, justify=FALSE)
  #
  # cat("\nTreatments:\n")
  # print(x$treatments %>% as.data.frame(), right=FALSE)
  # cat("\nSpecimens:\n")
  # print(x$specimens, right=FALSE)
  # cat("\nAnalytes:\n")
  # print(x$analytes, right=FALSE)
  #
  # cat("\nTreatment-to-analyte mappings:\n")
  # if(nrow(x$analyte_mapping)>0){
  #   print(x$analyte_mapping, right=FALSE)
  # } else {
  #   cat("none\n")
  # }
  #
  # cat("\nParent-to-metabolite mappings:\n")
  # if(nrow(x$metabolite_mapping)>0){
  #   print(x$metabolite_mapping, right=FALSE)
  # } else {
  #   cat("none\n")
  # }
  #
  # cat("\nTime mappings:\n")
  # if(nrow(x$time_mapping)>0){
  #   print(x$time_mapping, right=FALSE)
  # } else {
  #   cat("none\n")
  # }
  #
  #
  #
  # # if(!pctptnum.matches.pceltm(obj$pc)){
  # #   temp <- obj$pc %>%
  # #     dplyr::distinct(PCTPT, PCTPTNUM, PCELTM) %>%
  # #     as.data.frame()
  # #
  # #   message(paste0("\nCaution: In PC, PCTPTNUM and PCELTM do not match:\n",
  # #                 df.to.string(temp)))
  # # }
  print(summary(x))
}


#' #' Get a specific domain from the sdtm object
#' #'
#' #' @param obj The sdtm object.
#' #' @param dom The code of the domain to be returned.
#' #' @return The specified domain as data.frame
#' #' @export
#' #' @examples
#' #' domain(examplinib, "dm")
#' #'
#' domain <- function(obj, dom="") {
#'   UseMethod("domain")
#' }


#' @param dom The domain to return.
#' @param obj The sdtm object
#'
#' @export
domain <- function(obj, dom="dm") {
  obj$domains[[dom]]
}


#' #' Get the study names from an SDTM object
#' #'
#' #' @param obj The SDTM object.
#' #' @export
#' #' @export
#' #' @examples
#' #' studies(examplinib)
#' studies <- function(obj) {
#'   UseMethod(("studies"))
#' }
#'
#'
#' #' @export
#' studies.sdtm <- function(obj) {
#'   obj$dm %>%
#'     dplyr::distinct(STUDYID) %>%
#'     dplyr::pull(STUDYID) %>%
#'     as.character()
#' }


#' #' Get the USUBJIDs from an sdtm object
#' #'
#' #' @param obj The SDTM object.
#' #' @export
#' #' @examples
#' #' subjects(examplinib)
#' #'
#' subjects <- function(obj) {
#'   UseMethod(("subjects"))
#' }
#'
#' #' @export
#' subjects.sdtm <- function(obj) {
#'   obj$pc %>%
#'     dplyr::distinct(USUBJID) %>%
#'     dplyr::pull(USUBJID) %>%
#'     as.character()
#' }


#' #' Subject demographic information
#' #'
#' #' @param obj A SDTM object.
#' #' @param id The USUBJID.
#' #' @export
#' #' @examples
#' #' subject_info(examplinib, "20230004001010001")
#' #'
#' subject_info <- function(object, id="") {
#'   UseMethod("subject_info")
#' }


#' Subject demographic information
#'
#' @param obj A SDTM object.
#' @param id The USUBJID.
#' @export
subject_info <- function(obj, id="") {
  # if(!(id %in% (obj$subjects)$USUBJID)) {
  #   stop(paste("Subject", id, "is not in study"))
  # }
  temp <- obj$domains[["dm"]] %>%
    dplyr::filter(USUBJID %in% id) %>%
    t() %>%
    as.data.frame()
  colnames(temp) <- NULL
  print(temp, quote = FALSE, col.names=FALSE)
}


# pctptnum.matches.pceltm <- function(pc){
#   temp <- pc %>%
#     dplyr::mutate(x=stringr::str_extract(PCELTM, "PT([.0-9]+)H", group=1)) %>%
#     dplyr::filter(!is.na(x)) %>%
#     dplyr::mutate(test=(x==PCTPTNUM))
#   all(temp$test)
# }


#' #' Suggest common manual data programming steps for a sdtm data set
#' #'
#' #' @param obj A sdtm object
#' #' @seealso [read_sdtm_sas()]
#' #' @export
#' #' @examples
#' #' suggest(examplinib)
#' #'
#' suggest <- function(obj){
#'   UseMethod("suggest")
#' }


#' Suggest common manual data programming steps for a sdtm data set
#'
#' @param obj A sdtm object
#' @seealso [read_sdtm_sas()]
#' @import dplyr
#' @export
suggest <- function(obj) {
  n.suggestion <- 1

  arms <- obj$dm %>%
    dplyr::filter(ACTARMCD !="") %>%
    dplyr::distinct(ACTARM, ACTARMCD)

  if(nrow(arms)>1){
    message(paste0(
      n.suggestion, ". There are ", nrow(arms), " arms defined in DM (see below).\n",
      "   Consider defining a PART or ARM variable in the nif dataset, \n",
      "   filtering for a particular arm, or defining a covariate based\n",
      "   on ACTARMCD.\n\n",
      df.to.string(arms, indent="   "), "\n"
    ))
    n.suggestion <- n.suggestion + 1
  }

  specimems <- obj$pc %>%
    dplyr::filter(PCSPEC!="") %>%
    dplyr::distinct(PCSPEC)

  if(nrow(specimems)>1){
    message(paste0(
      n.suggestion, ". There are data from ", nrow(specimems),
      " different sample specimem types in PC\n",
      "   (see below).\n",
      "   Consider filtering for a specific specimem, or defining CMT accordingly.\n\n",
      df.to.string(specimems, indent="   "), "\n"
    ))
    n.suggestion <- n.suggestion + 1
  }

  treatments <- obj$ex %>%
    dplyr::distinct(EXTRT)

  if(nrow(treatments)>1){
    message(paste0(
      n.suggestion, ". There are ", nrow(treatments), " different treatments ",
      "in EX (see below).\n", "   Consider filtering for a specific treatment.\n\n",
      df.to.string(treatments, indent="   "), "\n"
    ))
    n.suggestion <- n.suggestion +1
  }

  analytes <- obj$pc %>%
    dplyr::distinct(PCTESTCD) %>%
    dplyr::pull(PCTESTCD)

  no.analyte.treatments <- treatments %>%
    dplyr::mutate(no.analyte=!(EXTRT %in% analytes)) %>%
    dplyr::filter(no.analyte==TRUE) %>%
    dplyr::select(EXTRT)

  if(nrow(no.analyte.treatments)>0){
    message(paste0(
      n.suggestion, ". There are treatments (EXTRT) without analytes of the ",
      "same name\n",
      "   (see below).\n",
      "   Consider adding a treatment-analyte mapping to the sdtm object\n",
      "   See '?add_treatment_mapping' for additional information.\n\n",
      df.to.string(no.analyte.treatments, indent="   "), "\n\n",
      "   Available analytes:\n\n",
      df.to.string(obj$pc %>% dplyr::distinct(PCTESTCD), indent="   "), "\n\n"
    ))
    n.suggestion <- n.suggestion + 1
  }

  if(!("PCELTM" %in% names(obj$pc))){
    line <- function(x) {
      return(paste0("     \"", x, "\" = 0,"))
    }
    temp <- paste(sapply(unique(obj$pc[,"PCTPT"]), line), collapse="\n")

    message(paste0(
      n.suggestion, ". By default, 'make_nif()' derives the nominal sampling time from the permissible field\n",
      "   PCELTM in PC. However, in this data set, PCELTM is not defined, and the nominal time\n",
      "   must be manually derived from PCTPT. Please provide a time mapping using 'add_time_mapping()',\n",
      "   e.g., by adding the below code after creating the sdtm object. Obviously, you need to\n",
      "   replace the zeros with the respective time after admininstation in hours:\n\n",
      "   %>% add_time_mapping(\n",
      substr(temp, 1, nchar(temp)-1), ")\n"
    ))
    n.suggestion <- n.suggestion + 1
  }
}
