
#' SDTM class constructor, creating a sdtm object from a set of SDTM domains
#'
#' @param sdtm.data, a list of SDTM domains as data.frames
#' @import dplyr
#' @returns A sdtm object
#' @export
sdtm <- function(sdtm.data){
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
    treatment.analyte.mappings=data.frame()
  )

  class(temp) <- "sdtm"
  temp
}

#' Attach a treatment-analyte mapping to an SDTM object
#'
#' In some studies, multiple drugs are co-administered, and there may be plasma
#' concentration data from different parent drugs.
#' In order to appropriately correlate observations with administrations, the
#' [make_nif()] algorithm needs to know which analyte (PCTESTCD within PC) belongs
#' to which drug (EXTRT within EX). If the respective names differ, add_mapping() can be
#' used to attach this information to the SDTM object.
#' Multiple mappings may be needed.
#'
#' @param obj A SDTM object.
#' @param extrt The treatment as defined in EX.
#' @param pctestcd The analyte as defined in PC.
#' @seealso [make_nif()]
#' @export
add_mapping <- function(obj, extrt="", pctestcd="") {
  UseMethod("add_mapping")
}

#' @export
add_mapping.sdtm <- function(obj, extrt="", pctestcd="") {
  obj$treatment.analyte.mappings <- rbind(obj$treatment.analyte.mappings,
    data.frame("EXTRT"=extrt, "PCTESTCD"=pctestcd))
  return(obj)
}


#' print() implementation for SDTM
#'
#' @param obj A SDTM object.
#' @export
print.sdtm <- function(obj){
  cat(paste("Study", obj$study))
  cat(paste(" with", nrow(obj$subjects), "subjects\n"))
  cat("SDTM domains: ")
  cat(paste(names(obj$domains), collapse=", "))

  cat("\n\nArms:\n")
  print(obj$arms, right=FALSE, justify=FALSE)

  cat("\nTreatments:\n")
  print(obj$treatments, right=FALSE)
  cat("\nSpecimens:\n")
  print(obj$specimens, right=FALSE)
  cat("\nAnalytes:\n")
  print(obj$analytes, right=FALSE)
  cat("\nTreatment-to-analyte mappings:\n")
  if(nrow(obj$treatment.analyte.mappings)>0){
    print(obj$treatment.analyte.mappings, right=FALSE)
  } else {
    cat("none")
  }

  # if(!pctptnum.matches.pceltm(obj$pc)){
  #   temp <- obj$pc %>%
  #     dplyr::distinct(PCTPT, PCTPTNUM, PCELTM) %>%
  #     as.data.frame()
  #
  #   message(paste0("\nCaution: In PC, PCTPTNUM and PCELTM do not match:\n",
  #                 df.to.string(temp)))
  # }
}


#' Get a specific domain from the sdtm object
#'
#' @param obj The sdtm object.
#' @param dom The code of the domain to be returned.
#' @return The specified domain as data.frame
#' @export
domain <- function(obj, dom="") {
  UseMethod("domain")
}


#' @export
domain.sdtm <- function(obj, dom="dm") {
  obj$domains[[dom]]
}


#' Get the study names from an SDTM object
#'
#' @param obj The SDTM object.
#' @export
#' @export
studies <- function(obj) {
  UseMethod(("studies"))
}

#' @export
studies.sdtm <- function(obj) {
  obj$dm %>%
    dplyr::distinct(STUDYID) %>%
    dplyr::pull(STUDYID) %>%
    as.character()
}


#' Get the USUBJIDs from an sdtm object
#'
#' @param obj The SDTM object.
#' @export
subjects <- function(obj) {
  UseMethod(("subjects"))
}

#' @export
subjects.sdtm <- function(obj) {
  obj$dm %>%
    dplyr::distinct(USUBJID) %>%
    dplyr::pull(USUBJID) %>%
    as.character()
}


#' Subject demographic information
#'
#' @param obj A SDTM object.
#' @param id The USUBJID.
#' @export
subject_info <- function(object, id="") {
  UseMethod("subject_info")
}

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


#' Suggest common manual data programming steps for a sdtm data set
#'
#' @param obj A sdtm object
#' @seealso [read_sdtm_sas()]
#' @export
suggest <- function(obj){
  UseMethod("suggest")
}


#' Suggest common manual data programming steps for a sdtm data set
#'
#' @param obj A sdtm object
#' @seealso [read_sdtm_sas()]
#' @import dplyr
#' @export
suggest.sdtm <- function(obj) {
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
      "   See '?add_mapping' for additional information.\n\n",
      df.to.string(no.analyte.treatments, indent="   "), "\n\n",
      "   Available analytes:\n\n",
      df.to.string(obj$pc %>% dplyr::distinct(PCTESTCD), indent="   "), "\n\n"
    ))
    n.suggestion <- n.suggestion + 1
  }
}
