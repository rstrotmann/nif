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
                     analyte_mapping = data.frame(),
                     metabolite_mapping = data.frame(),
                     parent_mapping = data.frame(),
                     time_mapping = data.frame()) {
  domains <- sdtm_data
  vs <- domains[["vs"]]
  ex <- domains[["ex"]]
  pc <- domains[["pc"]]
  dm <- domains[["dm"]]

  temp <- list(
    # study = dm[1, "STUDYID"],
    # subjects = pc %>%
    #   dplyr::distinct(.data$USUBJID),
    # specimens = pc %>%
    #   dplyr::distinct(.data$PCSPEC),
    # analytes = pc %>%
    #   dplyr::distinct(.data$PCTEST, .data$PCTESTCD),
    # treatments = ex %>%
    #   dplyr::distinct(.data$EXTRT),
    # doses = ex %>%
    #   dplyr::distinct(.data$EXDOSE),
    # arms = dm %>%
    #   dplyr::distinct(.data$ACTARM, .data$ACTARMCD),
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
  out <- list(
    study = unique(domain(object, "dm")$STUDYID),
    subjects = unique(domain(object, "pc")$USUBJID),
    pc_timepoints = unique(domain(object, "pc")[c("PCTPT", "PCTPTNUM")]),
    domains = data.frame(
      DOMAIN = names(object$domains),
      N = as.numeric(lapply(
        object$domains, function(x) {length(unique(x$USUBJID))}))
    ),
    treatments = unique(domain(object, "ex")$EXTRT),
    arms = unique(domain(object, "dm")[c("ACTARMCD", "ACTARM")]),
    doses = unique(domain(object, "ex")[c("EXTRT", "EXDOSE")]),
    specimems = unique(domain(object, "pc")$PCSPEC),
    analytes = unique(domain(object, "pc")[c("PCTEST", "PCTESTCD")]),
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
#' @return none
#' @export
#' @noRd
print.summary_sdtm <- function(x, color = FALSE, ...) {
  hline <- paste0(rep("-", 8), collapse="")
  indent = " "

  n <- filter(x$domains, DOMAIN == "pc")$N
  if(length(n) == 0) n <- 0

  cat(paste(hline, "SDTM data set summary", hline, "\n"))
  cat(paste("Study", x$study))
  cat(paste(" with", n,
            "subjects providing PC data.\n"))

  cat("\nSubjects per domain:\n")
  temp <- x$domains
  cat(paste0(df_to_string(x$domains, color = color, indent = indent,
                          show_none = TRUE), "\n\n"))

  cat("Arms (DM):\n")
  cat(paste0(df_to_string(x$arms, color = color, indent = indent,
                          show_none = TRUE), "\n\n"))

  cat("Treatments (EX):\n")
  cat(df_to_string(x$treatments, color = color, indent = indent,
                   show_none = TRUE), "\n\n")

  cat("Specimens (PC):\n")
  cat(df_to_string(x$specimens, color = color, indent = indent,
                   show_none = TRUE), "\n\n")

  cat("Analytes (PC):\n")
  cat(df_to_string(x$analytes, color = color, indent = indent,
                   show_none = TRUE), "\n\n")

  if(nrow(x$analyte_mapping) != 0) {
    cat("Treatment-to-analyte mappings:\n")
    cat(df_to_string(x$analyte_mapping, color = color, indent = indent,
                     show_none = TRUE), "\n\n")}

  if(nrow(x$metabolite_mapping) != 0) {
    cat("Parent-to-metabolite mappings:\n")
    cat(df_to_string(x$metabolite_mapping, color = color, indent = indent,
                     show_none = TRUE), "\n\n")}

  if(nrow(x$time_mapping) != 0) {
    cat("Time mappings:\n")
    cat(df_to_string(x$time_mapping, color = color, indent = indent,
                     show_none = TRUE), "\n\n")}

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
#' <TO DO> Update documentation, remove references to make_nif().
#'
#' @param obj A SDTM object.
#' @param extrt The treatment as defined in EX.
#' @param pctestcd The analyte as character (as in 'PCTESTCD' for PK observations
#' @param analyte The analyte name to be used in the nif object, as character.
#' @seealso [nif_auto()]
#' @examples
#' sdtm_object <- add_analyte_mapping(examplinib_fe, "EXAMPLINIB", "RS2023")
#' @export
add_analyte_mapping <- function(obj, extrt, pctestcd, analyte = NULL) {
  if(is.null(analyte)) analyte <- pctestcd
  obj$analyte_mapping <- rbind(
    obj$analyte_mapping,
    data.frame("EXTRT" = extrt, "PCTESTCD" = pctestcd, "ANALYTE" = analyte)
  )
  return(obj)
}


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
#'
#' @param obj The SDTM object.
#' @param pctestcd_parent The PCTESTCD of the parent compound.
#' @param pctestcd_metabolite The PCTESTCD of the metabolite.
#' @return The SDTM object.
#' @export
#' @seealso [nif_auto()]
#' @examples
#' add_metabolite_mapping(examplinib_fe, "RS2023", "RS2023487A")
add_metabolite_mapping <- function(obj, pctestcd_parent, pctestcd_metabolite) {
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
  temp <- obj %>%
    domain("dm") %>%
    dplyr::filter(.data$USUBJID %in% id) %>%
    as.list()
  class(temp) <- c("subject_info", "data.frame")
  return(temp)
}


#' Suggest manual data programming steps for a sdtm data set
#'
#' @param obj A sdtm object
#' @import dplyr
#' @export
suggest <- function(obj) {
  suggest_out <- function(n, text, table = NULL) {
    out <- paste0(str_wrap(
          paste0(n, ". ", text), width = 80, indent = 0, exdent = 3), "\n")
    if(!is.null(table)){
      out <- paste0(out, "\n", df_to_string(table, indent = "       "), "\n")
    }
    message(out)
    return(n + 1)
  }

  col = 34
  n_suggestion <- 1

  treatments <- obj$ex %>%
    distinct(EXTRT)
  analytes <- obj$pc %>%
    distinct(PCTEST, PCTESTCD)
  n_suggestion <- suggest_out(n_suggestion, paste0(
    "Only needed if you want to generate nif objects automatically (using ",
    "'nif_auto()'): ",
    "There are ", nrow(treatments), " treatments (EXTRT) in 'EX', and ",
    nrow(analytes),
    " pharmacokinetic analytes (PCTESTCD) in 'PC':"))
  message(paste0(df_to_string(treatments, indent = "       "), "\n"))
  message(paste0(df_to_string(analytes, indent = "       "), "\n"))
  message(paste0(str_wrap(paste0(
      "To associate treatments with their respective parent analyte, consider ",
      "adding analyte mapping(s) to the sdtm object using the below code ",
      "snippet (replace 'x' with the corresponding PCTESTCD):"),
    width = 80, indent = 3, exdent = 3), "\n"))
  out <- sapply(treatments$EXTRT, function(x) {
    paste0("         add_analyte_mapping('", x, "', 'x')")
  })
  message(paste0(paste0("\033[0;", col, "m",
                        c("      ", out), collapse = " %>%\n")), "\033[0m", "\n")
  message(paste0(str_wrap(paste0(
      "For further information see the documentation to 'add_analyte_mapping ",
      "('?add_analyte_mapping')."),
    width = 80, indent = 3, exdent = 3), "\n"))


  n_trt <- length(unique(obj$ex$EXTRT))
  n_analytes <- length(unique(obj$pc$PCTESTCD))
  if (n_analytes > n_trt) {
    n_suggestion <- suggest_out(n_suggestion, paste0(
      "Only needed if you want to generate nif objects automatically (using ",
      "'nif_auto()'): ",
      "There are more pharmacokinetic analytes in 'PC' than treatments in ",
      "'EX'. If you want to include pharmacokinetic observations of ",
      "metabolites, you can add metabolite mapping(s) to the sdtm object ",
      "using 'add_metabolite_mapping' (see ?add_metabolite_mapping for ",
      "further information)."))
  }


  treatments <- obj$ex %>%
    dplyr::distinct(EXTRT)
  if (nrow(treatments) > 0) {
    n_suggestion <- suggest_out(n_suggestion, paste0(
      "There are ", nrow(treatments), " different treatments in 'EX' (see ",
      "below). Consider adding them to the nif object using ",
      "'add_administration()'.\n\n"), table = treatments)
  }

  obs <- obj$pc %>%
    distinct(PCTESTCD)
  if (nrow(obs) > 0) {
    n_suggestion <- suggest_out(n_suggestion, paste0(
      "There are ", nrow(obs), " different pharmacokinetic observations in ",
      "'PC' (see below). Consider adding them to the nif object using ",
      "'add_observation()'.\n\n"), table = obs)
  }


  specimems <- obj$pc %>%
    dplyr::filter(PCSPEC != "") %>%
    dplyr::distinct(PCSPEC)
  if (nrow(specimems) > 1) {
    n_suggestion <- suggest_out(n_suggestion, paste0(
      "There are data from ", nrow(specimems), " different sample specimem ",
      "types in 'PC'. ",
      "When calling 'add_observation()', consider filtering for a specific ",
      "specimem using the 'observation_filter' parameter."), table = specimems)
  }

  if (!("PCELTM" %in% names(obj$pc))) {
    temp <- guess_ntime(obj) %>%
      mutate(out = paste0('"', .data$PCTPT, '", ', .data$NTIME, ','))
    out <- paste0("        ", temp$out, collapse = "\n")
    n_suggestion <- suggest_out(n_suggestion, paste0(
      "By default, 'add_observation()' takes the nominal sampling time from ",
      "the (permissible) field PC.PCELTM. However, in this data set, PCELTM ",
      "is not defined, and the nominal time must be manually derived from, e.g., ",
      "PCTPT. Consider providing the NTIME_lookup parameter to ",
      "'add_observation()' as the below data frame (make sure to review the ",
      "suggested NTIME values):\n"))
    message(paste0(
      "\033[0;", col, "m",
      "      NTIME_lookup = tribble(\n",
      "        ~PCTPT, ~NTIME,\n",
      out, "\n",
      "      )\n", "\033[0m"
    ))
  }


  arms <- obj$dm %>%
    dplyr::filter(ACTARMCD != "") %>%
    dplyr::distinct(ACTARM, ACTARMCD)
  if (nrow(arms) > 1) {
    n_suggestion <- suggest_out(n_suggestion, paste0(
      "There are ", nrow(arms), " arms defined in DM (see ",
      "below). Consider defining a PART or ARM variable in the nif dataset, ",
      "filtering for a particular arm, or defining a covariate based on ",
      "ACTARMCD."), table = arms)
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


#' Analytes in a sdtm object
#'
#' @param obj The sdtm object.
#'
#' @return Character.
#' @export
#'
#' @examples
#' analytes(examplinib_sad)
analytes.sdtm <- function(obj) {
  unique(domain(obj, "pc")$PCTESTCD)
}


#' Doses in a sdtm object
#'
#' @param obj A sdtm object.
#'
#' @return Numeric.
#' @export
#'
#' @examples
#' doses(examplinib_poc)
doses.sdtm <- function(obj) {
  unique(domain(obj, "ex")$EXDOSE)
}


#' Treatments in a sdtm object
#'
#' @param obj A sdtm object.
#'
#' @return Character.
#' @export
#'
#' @examples
#' treatments(examplinib_poc)
treatments.sdtm <- function(obj) {
  unique(domain(obj, "ex")$EXTRT)
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


#' Guess NTIME from PCTPT
#'
#' @param sdtm A sdtm object.
#'
#' @return A data frame.
#' @export
#'
#' @examples
#' guess_ntime(examplinib_poc)
guess_ntime <- function(sdtm) {
  sdtm$pc %>%
    distinct(.data$PCTPT) %>%
    mutate(time = str_extract(tolower(.data$PCTPT),
                              "([0-9.]+)\\s*(h)", group = 1)) %>%
    mutate(pre = str_match(tolower(.data$PCTPT), "pre") == "pre") %>%
    mutate(NTIME = case_when(
      is.na(.data$time) & pre == TRUE ~ 0,
      !is.na(.data$time) & pre == TRUE ~ -as.numeric(.data$time),
      is.na(.data$time) & is.na(.data$pre) ~ NA,
      .default = as.numeric(.data$time))) %>%
    select(-c("time", "pre"))
}


#' Calculate SLD for SDTM.TR domain
#'
#' Calculate the sum of longest diameters (SLD) and number of lesions by subject
#' and time point, and add as rows. If a SDTM.TR domain is not included in the
#' input SDTM object, the SDTM object is returned as-is.
#'
#' @description
#' `r lifecycle::badge("experimental")`
#'
#' @param sdtm_obj A SDTM object.
#' @param observation_filter A filter term, as character.
#'
#' @return A SDTM object.
#' @export
derive_sld <- function(sdtm_obj, observation_filter = "TRGRPID == 'TARGET'") {
  if(!"tr" %in% names(sdtm_obj$domains)) {
    return(sdtm_obj)
  }

  tr <- domain(sdtm_obj, "tr") %>%
    assertr::verify(has_all_names("USUBJID", "TRTESTCD", "TRSTRESN", "TRDTC"))

  tr <- tr %>%
    add_row(tr %>%
      filter(eval(parse(text = observation_filter))) %>%
      filter(.data$TRTESTCD == "DIAMETER") %>%
      reframe(N_TARGET = n(), SLD = sum(.data$TRSTRESN),
              .by = any_of(c("STUDYID", "DOMAIN", "USUBJID", "SUBJID", "TRDTC",
                             "TRDY", "VISITNUM", "VISIT", "EPOCH", "TREVAL",
                             "TRMETHOD", "TRGRPID", "TRREFID"))) %>%
      distinct() %>%
      pivot_longer(cols = c("N_TARGET", "SLD"), names_to = "TRTESTCD",
                   values_to = "TRSTRESN") %>%
      mutate(TRSTRESU = case_match(.data$TRTESTCD, "SLD" ~ "mm", .default = "")) %>%
      mutate(TRTEST = case_match(.data$TRTESTCD, "SLD" ~ "Sum of longest diameters",
                                 "N_TARGET" ~ "Number of target lesions")) %>%
      mutate(DOMAIN = "TR") %>%
      as.data.frame()
    ) %>%
    arrange(.data$USUBJID, .data$TRDTC)

  temp = sdtm_obj
  temp$domains[["tr"]] <- tr
  return(temp)
}

