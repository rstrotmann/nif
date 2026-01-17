ddt_standard_fields <- tibble::tribble(
  ~name, ~definition, ~type, ~description, ~unit, ~source,
  "REF", "Consecutive record number", "integer", "Unique number for each row",
  NA, "Produced",
  "STUDYID", "Study", "character", "Study identification number", NA,
  "DM: STUDYID",
  "ID", "Subject identifier", "numeric", "Unique subject ID across all studies",
  NA, "Produced",
  "USUBJID", "USUBJID in source", "character", "Unique subject ID in study", NA,
  "DM: USUBJID",
  "AGE", "Age", "numeric", "Age of subjec at study start", "years",
  "DM: AGE or derived from DM: BRTHDTC",
  "SEX", "Sex", "0, 1", "0 = Male, 1 = Female", NA, "Derived from DM: SEX",
  "RACE", "Race", "character", "Race category", NA, "DM: RACE",
  "HEIGHT", "Body height", "numeric", "Baseline body height", "cm",
  "VS: VSTESTCD = HEIGHT",
  "WEIGHT", "Body weight", "numeric", "Baseline body weight", "kg",
  "VS: VSTESTCD = WEIGHT",
  "BMI", "Body Mass Index", "numeric", "Baseline BMI", "kg/m^2",
  "HEIGHT and WEIGHT",
  "DTC", "Datetime", "datetime", "Date and time of event", "Datetime",
  "SDTM domain",
  "TIME", "Time since start of treatment", "numeric",
  "Individual time since individual start of treatment", "hours", "DTC",
  "NTIME", "Nominal time", "numeric", "Nominal time of event", "hours",
  "SDTM domain",
  "TAFD", "Time after first dose", "numeric",
  "Actual time after individual first dose", "hours", "TIME",
  "TAD", "Time after last dose", "numeric", "Time after individual last dose",
  "hours", "TIME",
  "EVID", "Event ID", "0, 1", "0 = Observation, 1 = Administration", NA,
  "Produced",
  "AMT", "Amount", "numeric", "Dose administered", "mg", "EX: EXDOSE",
  "ANALYTE", "Analyte", "character", "Assigned name to observation substrate",
  NA, "EXTRT, xxTESTCD or assigned",
  "CMT", "Compartment", "numeric", "PK/PD compartment", NA,
  "Produced or assigned",
  "PARENT", "Parent analyte", "character",
  "Reference drug name for observations", NA,
  "Automatically or manually assigned",
  "METABOLITE", "Metabolite", "logical", "Metabolite flag", NA,
  "Automatically or manually assigned",
  "DOSE", "Dose", "numeric", "Last administerd dose", "mg", "EX: EXDOSE",
  "DV", "Dependent variable", "numeric",
  "Dependent variable, NA for administrations", NA, "SDTM domain",
  "MDV", "Missing DV", "numeric", "0 = non-missing DV, 1 = Missing DV", NA,
  "Produced",
  "IMPUTATION", "Imputation", "character", "time imputation applied to record",
  NA, "Produced",
  "ACTARMCD", "Actual arm code", "character", "ACTARMCD as in SDTM source", NA,
  "DM: ACTARMCD",
  "TRTDY", "Treatment day", "numeric", "Day after treatment start", NA,
  "SDTM domain",
  "BL_CREAT", "Baseline creatinine", "numeric",
  "Serum creatinine value at baseline", "umol/l", "LB",
  "BL_CRCL", "Baseline creatinnine clearance", "numeric",
  "Creatinine clearance based on baseline serum creatinine", "ml/min", "LB, DM",
  "BL_RENAL", "Baseline renal function class", "normal, mild, moderate, severe",
  "Renal function category at baseline, based on BL_CRCL", NA,
  "Derived from BL_CRCL",
  "DVBL", "Baseline value for dependent variable", "numeric",
  "DV value at baseline", NA, "SDTM domain",
  "DVCFB", "Change from baseline of dependent variable", "numeric",
  "DV difference to DVBL", NA, "Derived from DV, DVBL",
  "DL", "Dose level", "numeric", "Dose at treatment start", "mg",
  "Derived from DOSE"
)

#' Data definition table for nif object
#'
#' Generate a data definition table from a nif object. Known nif columns are
#' annotated automatically, for unknown fields, NA values are produced. Further
#' specifications can be added manually using `add_dd()`.
#'
#' @param obj A nif object.
#' @param silent Suppress messages, defaults to nif_option setting if NULL.
#'
#' @return A data table.
#' @export
#'
#' @examples
#' ddt(examplinib_sad_nif)
ddt <- function(obj, silent = NULL) {
  # validate inputs
  validate_nif(obj)

  out <- ddt_standard_fields

  # compartments
  temp <- obj |>
    ensure_analyte() |>
    as.data.frame() |>
    distinct(.data$ANALYTE, .data$CMT, .data$EVID) |>
    arrange(.data$CMT) |>
    mutate(TYPE = case_match(
      .data$EVID,
      0 ~ "observation",
      1 ~ "administration"
    )) |>
    mutate(DESC = paste(.data$CMT, "=", .data$ANALYTE, .data$TYPE))

  out[out$name == "CMT", "description"] <- paste(temp$DESC, collapse = ", ")
  out[out$name == "CMT", "type"] <- paste(temp$CMT, collapse = ", ")

  # race
  if ("RACE" %in% names(obj)) {
    if (inherits(obj$RACE, "numeric")) {
      temp <- obj |>
        as.data.frame() |>
        distinct(.data$RACE) |>
        arrange(.data$RACE) |>
        left_join(race_coding, by = c("RACE" = "RACEN")) |>
        mutate(DESC = paste(.data$RACE, "=", .data$RACE.y))
      out[out$name == "RACE", "type"] <- "numeric"
      out[out$name == "RACE", "description"] <- paste(temp$DESC,
                                                      collapse = ", ")
    }
  }

  # further fields
  further_name <- setdiff(names(obj), unique(out$name))
  further_type <- as.character(lapply(obj, class)[further_name])

  further <- data.frame(name = further_name, type = further_type)

  out <- bind_rows(out, further) |>
    filter(.data$name %in% names(obj)) |>
    as.data.frame()

  if (nrow(further) > 0) {
    conditional_cli({
      cli_alert_warning("Some data definition fields could not be generated:")
      cli_text(nice_enumeration(further$name))
    },
    silent = silent)
  }

  out
}


#' Add Field definition to Data Definition Table
#'
#' @param obj Data definition Table as data frame.
#' @param name Field name as character.
#' @param definition Field definition as character.
#' @param type Field type as character.
#' @param description Field description as character.
#' @param unit Field unit as character.
#' @param source Field source as character.
#'
#' @return A data frame.
#' @export
add_dd <- function(
  obj, name, definition, type, description, unit = NA_character_, source = ""
) {
  # validate input
  ddt_fields <- c("name", "definition", "type", "description", "unit", "source")
  if (!inherits(obj, "data.frame")) {
    stop("obj must be a data frame")
  }

  missing_fields <- setdiff(ddt_fields, names(obj))
  if (length(missing_fields) > 0) {
    stop(paste0(
      "missing fields in ddt: ", nice_enumeration(missing_fields)
    ))
  }

  validate_char_param(name, "name")
  validate_char_param(definition, "definition")
  validate_char_param(type, "type")
  validate_char_param(description, "description")
  if (!is.na(unit)) {
    validate_char_param(unit, "unit")
  }
  validate_char_param(source, "source")

  bind_rows(
    obj,
    c(
      name = name,
      definition = definition,
      type = type,
      description = description,
      unit = unit,
      source = source
    )
  )
}
