#' SDTM class constructor, creating a sdtm object from a set of SDTM domains
#'
#' @param sdtm_data The SDTM domains as list of data frames.
#' @param analyte_mapping The analyte mapping as data frame.
#' @param metabolite_mapping The metabolite mapping as data frame.
#' @param parent_mapping The parent mapping as data frame.
#' @param time_mapping The time mapping as data frame.
#'
#' @import dplyr
#' @returns A sdtm object.
#' @export
new_sdtm <- function(sdtm_data,
                     analyte_mapping=data.frame(),
                     metabolite_mapping=data.frame(),
                     parent_mapping=data.frame(),
                     time_mapping=data.frame()) {
  domains <- sdtm_data
  vs <- domains[["vs"]]
  ex <- domains[["ex"]]
  pc <- domains[["pc"]]
  dm <- domains[["dm"]]

  temp <- list(
    study = dm[1, "STUDYID"],
    subjects = pc %>%
      dplyr::distinct(.data$USUBJID),
    specimens = pc %>%
      dplyr::distinct(.data$PCSPEC),
    analytes = pc %>%
      dplyr::distinct(.data$PCTEST, .data$PCTESTCD),
    treatments = ex %>%
      dplyr::distinct(.data$EXTRT),
    doses = ex %>%
      dplyr::distinct(.data$EXDOSE),
    arms = dm %>%
      dplyr::distinct(.data$ACTARM, .data$ACTARMCD),
    domains = domains,
    pc = pc,
    dm = dm,
    ex = ex,
    vs = vs,
    analyte_mapping = analyte_mapping,
    metabolite_mapping = metabolite_mapping,
    parent_mapping = parent_mapping,
    time_mapping = time_mapping
  )

  class(temp) <- c("sdtm", "list")
  temp
}


#' SDTM summary
#'
#' @param object A SDTM object.
#' @param ... Further parameters.
#' @return A sdtm_summary object.
#' @noRd
#' @export
#' @examples
#' summary(examplinib_poc)
summary.sdtm <- function(object, ...) {
  subjects <- object$domains[["pc"]] %>%
    filter(!is.na(.data$PCSTRESN)) %>%
    dplyr::distinct(.data$USUBJID) %>%
    pull(.data$USUBJID) %>%
    as.character()

  out <- list(
    study = object$domains[["dm"]] %>%
      distinct(.data$STUDYID) %>%
      pull(.data$STUDYID) %>%
      as.character(),
    subjects = subjects,
    n_subs = length(subjects),
    pc_timepoints = object$domains[["pc"]] %>%
      dplyr::distinct(across(any_of(c("PCTPT", "PCTPTNUM")))),
    # domains = names(object$domains),
    domains = data.frame(
      name = names(object$domains),
      N = as.numeric(lapply(object$domains, function(x) {length(unique(x$USUBJID))}))
    ),
    treatments = object$domains[["ex"]] %>%
      dplyr::distinct(.data$EXTRT),
    arms = object$domains[["dm"]] %>%
      dplyr::distinct(.data$ACTARM, .data$ACTARMCD),
    doses = object$domains[["ex"]] %>%
      distinct(.data$EXTRT, .data$EXDOSE),
    specimems = object$domains[["pc"]] %>%
      dplyr::distinct(.data$PCSPEC) %>%
      pull(.data$PCSPEC) %>%
      as.character(),
    analytes = object$domains[["pc"]] %>%
      dplyr::distinct(.data$PCTEST, .data$PCTESTCD),
    analyte_mapping = object$analyte_mapping,
    metabolite_mapping = object$metabolite_mapping,
    # parent_mapping = object$parent_mapping,
    time_mapping = object$time_mapping
  )
  class(out) <- "summary_sdtm"
  return(out)
}


#' print SDTM summary
#'
#' @param x SDTM object
#' @param ... Further parameters
#' @return none
#' @export
#' @noRd
print.summary_sdtm <- function(x, color = FALSE, ...) {
  hline <- paste0(rep("\U2500", 8), collapse="")
  indent = " "

  cat(paste(hline, "SDTM data set summary", hline, "\n"))
  cat(paste("Study", x$study))
  cat(paste(" with", x$n_subs, "subjects providing PC data.\n"))
  # cat("SDTM domains: ")
  # cat(paste(x$domains, collapse = ", "))

  cat("\nSubjects per domain:\n")
  temp <- x$domains
  # print(x$domains, right = FALSE)
  cat(paste0(df_to_string(x$domains, color = color, indent = indent, show_none = TRUE), "\n\n"))

  cat("Arms (DM):\n")
  # print(x$arms, right = FALSE, justify = FALSE)
  cat(paste0(df_to_string(x$arms, color = color, indent = indent, show_none = TRUE), "\n\n"))

  cat("Treatments (EX):\n")
  # print(x$treatments %>% as.data.frame(), right = FALSE)
  cat(df_to_string(x$treatments, color = color, indent = indent, show_none = TRUE), "\n\n")

  cat("Specimens (PC):\n")
  # print(x$specimens, right = FALSE)
  cat(df_to_string(x$specimens, color = color, indent = indent, show_none = TRUE), "\n\n")

  cat("Analytes (PC):\n")
  # print(x$analytes %>% as.data.frame(), right = FALSE)
  cat(df_to_string(x$analytes, color = color, indent = indent, show_none = TRUE), "\n\n")

  cat("Treatment-to-analyte mappings:\n")
  cat(df_to_string(x$analyte_mapping, color = color, indent = indent, show_none = TRUE), "\n\n")
  # if (nrow(x$analyte_mapping) > 0) {
  #   # print(x$analyte_mapping, right = FALSE)
  #   cat(df_to_string(x$analyte_mapping, color = color, indent = indent), "\n\n")
  # } else {
  #   cat("none\n")
  # }

  cat("Parent-to-metabolite mappings:\n")
  cat(df_to_string(x$metabolite_mapping, color = color, indent = indent, show_none = TRUE), "\n\n")
  # if (nrow(x$metabolite_mapping) > 0) {
  #   # print(x$metabolite_mapping, right = FALSE)
  #   cat(df_to_string(x$metabolite_mapping, color = color, indent = indent), "\n\n")
  # } else {
  #   cat("none\n")
  # }

  cat("Time mappings:\n")
  cat(df_to_string(x$time_mapping, color = color, indent = indent, show_none = TRUE), "\n\n")
  # if (nrow(x$time_mapping) > 0) {
  #   # print(x$time_mapping, right = FALSE)
  #   cat(df_to_string(x$time_mapping, color = color, indent = indent), "\n\n")
  # } else {
  #   cat("none\n")
  # }

  invisible(x)
}


#' Attach a treatment-analyte mapping to an SDTM object
#'
#' In some studies, multiple drugs are co-administered, and there may be analyte
#' data related to different parent drugs. In order to appropriately correlate
#' observations with administrations, the [make_nif()] algorithm needs to know
#' which analyte (PCTESTCD within PC) belongs to which drug (EXTRT within EX).
#' Multiple mappings may be needed.
#'
#' @param obj A SDTM object.
#' @param extrt The treatment as defined in EX.
#' @param pctestcd The analyte as character (as in 'PCTESTCD' for PK observations
#'   or, e.g., 'VSTESTCD' for vital sign observations, etc.).
#' @seealso [make_nif()]
#' @examples
#' sdtm_object <- add_analyte_mapping(examplinib_fe, "EXAMPLINIB", "RS2023")
#' @export
add_analyte_mapping <- function(obj, extrt = "", pctestcd = "") {
  obj$analyte_mapping <- rbind(
    obj$analyte_mapping,
    data.frame("EXTRT" = extrt, "PCTESTCD" = pctestcd)
  )
  return(obj)
}
# add_analyte_mapping <- function(obj, extrt = "", pctestcd = "") {
#   obj$analyte_mapping <- rbind(
#     obj$analyte_mapping,
#     data.frame(EXTRT = extrt, PCTESTCD = pctestcd)
#   )
#   return(obj)
# }


#' Add parent mapping
#'
#' @param obj A sdtm object.
#' @param analyte The analyte as character.
#' @param parent The parent as character.
#'
#' @return A sdtm object.
#' @export
add_parent_mapping <- function(obj, analyte, parent) {
  obj$parent_mapping <- rbind(
    obj$parent_mapping,
    data.frame(ANALYTE = analyte, PARENT = parent)
  )
  return(obj)
}


#' Attach a parent-metabolite mapping to a SDTM object.
#'
#' In case multiple analytes are measured for a specific administered drug, some
#' functions need that information to correlate plasma concentrations with
#' administrations. 'add_metabolite_mapping()' is used to attach this
#' information to a SDTM object.
#' @param obj The SDTM object.
#' @param pctestcd_parent The PCTESTCD of the parent compound.
#' @param pctestcd_metabolite The PCTESTCD of the metabolite.
#' @return The SDTM object.
#' @export
#' @examples
#' add_metabolite_mapping(examplinib_fe, "RS2023", "RS2023487A")
add_metabolite_mapping <- function(
    obj, pctestcd_parent = "", pctestcd_metabolite = "") {
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
#' @param obj The SDTM object.
#' @param ... Mappings in the form '"<PCTPT>"=<NTIME>' with multiple mappings
#'   separated by commas. <PCTPT> corresponds to the value in the PCTPT fiels,
#'   and NTIME corresponds to the nominal time in hours.
#' @return The SDTM object
#' @export
#' @seealso [suggest()]
#' @examples
#' sdtm_object <- add_time_mapping(examplinib_fe,
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
  temp <- unlist(c(as.list(environment())[-1], list(...)))
  mapping <- data.frame(PCTPT = names(temp), NTIME = as.numeric(temp))
  obj$time_mapping <- rbind(obj$time_mapping, mapping)
  return(obj)
}


#' print() implementation for a sdtm object
#'
#' @param x A SDTM object.
#' @param ... Further parameters
#' @export
#' @noRd
print.sdtm <- function(x, ...) {
  print(summary(x))
}


#' Return a specific domain from a sdtm object
#'
#' @param obj The sdtm object.
#' @param name The domain to return.
#' @return The specified domain as data.frame
#' @export
#' @examples
#' head(domain(examplinib_fe, "dm"))
domain <- function(obj, name) {
  obj$domains[[name]]
}


#' Details on selected subjects
#'
#' @param obj The object, either an SDTM or NIF object.
#' @param id The ID or USUBJID as numeric or character.
#' @export
#' @examples
#' subject_info(examplinib_fe, subjects(examplinib_fe)[1, "USUBJID"])
#' subject_info(examplinib_poc_nif, 1)
#' unclass(subject_info(examplinib_poc_nif, 1))
#' subject_info(examplinib_poc_nif, 1)$administrations
subject_info <- function(obj, id) {
  UseMethod("subject_info")
}


#' Subject demographic information
#'
#' @param obj A SDTM object.
#' @param id The USUBJID.
#' @export
#' @noRd
#' @examples
#' subject_info(examplinib_fe, subjects(examplinib_fe)[1, "USUBJID"])
subject_info.sdtm <- function(obj, id) {
  temp <- obj$domains[["dm"]] %>%
    dplyr::filter(.data$USUBJID %in% id) %>%
    as.list()
  class(temp) <- c("subject_info", "data.frame")
  return(temp)
}


#' Suggest manual data programming steps for a sdtm data set
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


#' Unique subjects within a sdtm object
#'
#' @param obj The sdtm object.
#'
#' @return A data frame.
#' @export
#' @noRd
#' @examples
#' subjects(examplinib_poc)
subjects.sdtm <- function(obj) {
  obj %>%
    domain("dm") %>%
    as.data.frame() %>%
    distinct(.data$USUBJID)
}


#' Keep only selected USUBJID in the data set
#'
#' @param obj The input, either a `nif` or `sdtm` object.
#' @param usubjid The USUBJID as character.
#' @return The filtered object.
#' @export
#' @examples
#' filter_subject(examplinib_poc, subjects(examplinib_poc)[1, "USUBJID"])
#' filter_subject(examplinib_poc_nif, subjects(examplinib_poc_nif)[1, "USUBJID"])
filter_subject <- function(obj, usubjid) {
  UseMethod("filter_subject")
}


#' Filter sdtm object for specific USUBJID
#'
#' @param obj The sdtm object.
#' @param usubjid The USUBJID as character
#' @return A sdtm object.
#' @export
#' @noRd
#' @examples
#' filter_subject(examplinib_poc, subjects(examplinib_poc)[1, "USUBJID"])
filter_subject.sdtm <- function(obj, usubjid) {
  temp <- lapply(obj$domains, function(x) filter(x, .data$USUBJID %in% usubjid))
  new_sdtm(sdtm_data = temp, analyte_mapping = obj$analyte_mapping,
           metabolite_mapping = obj$metabolite_mapping,
           time_mapping = obj$time_mapping)
}












