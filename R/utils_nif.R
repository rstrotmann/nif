#' Identify baseline columns in a data frame
#'
#' Identifies columns that are constant (baseline) for each ID value. A baseline
#' column is one where all rows with the same ID have the same value.
#'
#' @param df A data frame.
#' @param id_col Character string specifying the ID column name. Defaults to
#'   "ID".
#'
#' @return A character vector of column names that are baseline columns
#'   (constant per ID). Returns an empty character vector if no baseline columns
#'   are found.
#'
#' @import dplyr
#' @noRd
identify_baseline_columns <- function(df, id_col = "ID") {
  # Input validation
  if (!is.data.frame(df)) {
    stop("Input must be a data frame")
  }

  if (nrow(df) == 0) {
    return(character(0))
  }

  if (!is.character(id_col) || length(id_col) != 1) {
    stop("id_col must be a single character string")
  }

  if (!id_col %in% names(df)) {
    stop("ID column '", id_col, "' not found in data frame")
  }

  # Get all column names except the ID column
  all_cols <- names(df)
  cols_to_check <- setdiff(all_cols, id_col)

  if (length(cols_to_check) == 0) {
    return(character(0))
  }

  # A column is baseline if every ID has at most one distinct
  # non-NA value (all-NA per ID is still baseline).
  is_constant <- df |>
    summarise(
      across(
        all_of(cols_to_check),
        ~ n_distinct(.x, na.rm = TRUE) <= 1
      ),
      .by = all_of(id_col)
    ) |>
    summarise(across(-all_of(id_col), all))

  cols_to_check[unlist(is_constant, use.names = FALSE)]
}


#' Re-assign ID based on consistent criteria
#'
#' Subjects are ranked by `sum(DV)` then `sum(AMT)` (missing values as zero).
#' Ties keep first-appearance order. The result is sorted by the new ID, with
#' within-subject row order preserved.
#'
#' @param obj A nif object.
#'
#' @returns A nif object.
#' @noRd
#'
#' @examples
#' normalize_id(examplinib_sad_nif)
normalize_id <- function(obj) {
  # input validation
  validate_nif_argument(obj)

  # business logic
  id <- obj$ID
  if (length(id) == 0L) {
    out <- obj
    out$ID <- integer()
    return(restore_nif(out, obj))
  }

  uid <- unique(id)
  n_id <- length(uid)
  g <- match(id, uid)

  sums <- rowsum(
    cbind(obj$DV, obj$AMT, deparse.level = 0),
    group = g,
    reorder = FALSE,
    na.rm = TRUE
  )
  o <- order(sums[, 1L], sums[, 2L])
  new_for_uid <- integer(n_id)
  new_for_uid[o] <- seq_len(n_id)

  out <- obj
  out$ID <- new_for_uid[g]
  restore_nif(out[order(out$ID), ], obj)
}


#' XXH128 hash
#'
#' @param x An object.
#'
#' @returns XXH128 hash as string.
#' @export
hash <- function(x) {
  UseMethod("hash")
}


#' @export
hash.default <- function(x) {
  stop(
    "hash() has no method for class ",
    paste(class(x), collapse = "/"),
    ". Use a nif, sdtm, sdtm_domain or adam_dataset object.",
    call. = FALSE
  )
}


#' Ensure that fields are unique per subject
#'
#' @param nif A nif object.
#' @param field field(s) to check for uniqueness as character. "ID" is ignored.
#'
#' @returns Nothing or stop.
#' @noRd
ensure_unique_per_subject <- function(nif, field) {
  # input validation
  validate_nif_argument(nif)
  validate_argument(
    field, allow_multiple = TRUE, allow_null = TRUE, values = names(nif))

  # business logic
  field <- setdiff(field, "ID")
  if (length(field) != 0) {
    multiple_baseline <- nif |>
      select(all_of(c("ID", field))) |>
      mutate(across(all_of(c("ID", field)), as.character)) |>
      pivot_longer(cols = all_of(field), names_to = "param", values_to = "value") |>
      reframe(n = n_distinct(.data$value), .by = c("ID", "param")) |>
      filter(n > 1)

    n_multiple <- nrow(multiple_baseline)
    if (n_multiple > 0) {
      stop(paste0(
        "Non-unique values for ",
        nice_enumeration(unique(multiple_baseline$param), conjunction = "or"),
        ":\n",
        df_to_string(multiple_baseline, indent = 2)
      ))
    }
  }
  invisible(NULL)
}


#' Check consistency of ID, USUBJID and STUDYID assignments
#'
#' @section Cases
#'
#' **ID must not be re-assigned**
#'
#' ID values must be unique for USUBJID/STUDYID. If an ID is assigned to
#' different combinations of USUBJID and STUDYID, if present, an error is
#' issued.
#'
#' **The same USUBJID in different STUDYID**
#'
#' This may happen if the same participant was enrolled in different studies. In
#' these cases, they need to be assigned a different ID in the NIF. In such
#' cases, a warning is issued.
#'
#' If the STUDYID field is not present, the same USUBJID must not be assigned to
#' different ID. An error is issued.
#'
#' **The same USUBJID assigned to different ID but consistent STUDYID**
#'
#' This must not happen and results in an error.
#'
#' @param obj A data frame or nif object.
#'
#' @returns Nothing.
#' @noRd
nif_check_id_integrity <- function(obj) {
  if (!inherits(obj, "data.frame"))
    stop("input must be a data frame!")

  if (!"ID" %in% names(obj))
    stop("Input must contain at least the ID column!")

  if (any(is.na(obj$ID)))
    stop("ID must not be NA!")

  ids <- obj |>
    select(any_of(c("ID", "USUBJID", "STUDYID"))) |>
    distinct()

  # ID must be only assigned once
  id_reassignment <- ids |>
    group_by(.data$ID) |>
    filter(n() >1) |>
    ungroup()

  if (nrow(id_reassignment) > 0)
    stop(paste0(
      "Multiple assignment of the following ID:\n",
      df_to_string(id_reassignment, indent = 2)))

  if (all(c("STUDYID", "USUBJID") %in% names(ids))) {

    # Warn if an USUBJID is in multiple studies
    usubjid_reassignment <- ids |>
      select(c("USUBJID", "STUDYID")) |>
      distinct() |>
      group_by(.data$USUBJID) |>
      filter(n() > 1) |>
      ungroup()

    if (nrow(usubjid_reassignment) > 0)
      warning(paste0(
        "The following USUBJID was found in multiple studies:\n",
        df_to_string(usubjid_reassignment, indent = 2)))
  }
}


#' Re-code SEX field in a data frame
#'
#' @param obj The data.frame containing a SEX field
#' @return The output data frame with SEX coded as:
#'   - 0: "M", "0"
#'   - 1: "F", "1"
#'   - NA: Any other values (with warning)
#' @import dplyr
#' @keywords internal
#' @noRd
recode_sex <- function(obj) {
  # Input validation
  if (!is.data.frame(obj)) {
    stop("Input must be a data frame")
  }
  if (!"SEX" %in% names(obj)) {
    stop("Input data frame must contain 'SEX' column")
  }

  # Store original values for warning message
  orig_vals <- unique(obj$SEX[!is.na(obj$SEX)])

  result <- obj |>
    mutate(SEX = as.numeric(
      case_when(
        str_trim(toupper(as.character(.data$SEX))) %in% c("M", "0", "\u7537") ~ 0,
        str_trim(toupper(as.character(.data$SEX))) %in% c("F", "1", "\u5973") ~ 1,
        .default = NA
      )
    ))

  # Warn about invalid values that were converted to NA
  valid_vals <- c("m", "f", "M", "F", "0", "1", "\u7537", "\u5973")
  invalid_vals <- setdiff(orig_vals, valid_vals)
  if (length(invalid_vals) > 0) {
    warning(
      "Invalid sex values converted to NA: ",
      paste(invalid_vals, collapse = ", ")
    )
  }

  result
}


#' Race coding table
#'
#' Standard race coding table with numeric codes and labels
#'
#' @format A data frame with 8 rows and 3 columns:
#' \describe{
#'   \item{RACEN}{Numeric code}
#'   \item{RACE}{RACE CDISC submission value as per NCI code C74457}
#'   \item{LABEL}{Abbreviation for labeling purpose}
#' }
#' @export
race_coding <- tibble::tribble(
  ~RACEN, ~RACE, ~LABEL,
  0, "WHITE", "White",
  1, "ASIAN", "Asian",
  2, "BLACK OR AFRICAN AMERICAN", "Black",
  3, "AMERICAN INDIAN OR ALASKA NATIVE", "Native",
  4, "NATIVE HAWAIIAN OR OTHER PACIFIC ISLANDER", "Pacific",
  5, "NOT REPORTED", "NR",
  6, "UNKNOWN", "Unknown",
  7, "OTHER", "Other"
)


#' Recode RACE columns in nif object
#'
#' For some purposes, e.g., NONMEM-based modeling, numerical values are expected
#' in the RACE field. This function recodes RACE based on the following
#' associations:
#'
#' @param obj A nif object with RACE as character field.
#' @param coding_table A data frame with the columns RACE and RACEN. Uses
#' default coding, if NULL.
#' @param silent Suppress messages, defaults to nif_option setting, if NULL.
#'
#' @return A nif object with the original RACE replaced by the numerical race
#' code.
#' @export
#'
#' @examples
#' nif::race_coding
#' head(recode_race(examplinib_sad_nif))
recode_race <- function(
    obj,
    coding_table = NULL,
    silent = NULL
) {
  # validate inputs
  validate_nif_argument(obj, fields = "RACE")
  validate_df_argument(coding_table, allow_null = TRUE,
                       expected_fields = c("RACE", "RACEN"))
  validate_argument(silent, "logical", allow_null = TRUE)

  if (!"RACE" %in% names(obj)) {
    stop("RACE field not found")
  }

  if (is.null(coding_table)) {
    coding_table <- race_coding
  }

  # check coding table before joining
  unmatched <- setdiff(unique(obj$RACE), coding_table$RACE)
  if (length(unmatched) > 0) {
    conditional_message(
      "The following RACE values could not be matched and will become NA: ",
      nice_enumeration(unmatched),
      silent = silent
    )
  }

  obj |>
    left_join(select(coding_table, c("RACEN", "RACE")), by = "RACE") |>
    select(-c("RACE")) |>
    rename(RACE = "RACEN") |>
    order_nif_columns()
}


#' Apply CAT and SCAT filter
#'
#' @param obj A data frame.
#' @param param The parameter (e.g., the cat or scat value).
#' @param param_field The field in the data frame (e.g., PCCAT, PCSCAT).
#'
#' @returns A data frame.
#' @noRd
apply_cat_filter <- function(obj, param, param_field) {
  if (!is.null(param)) {
    if (!param_field %in% names(obj))
      stop(paste0("The input does not have a ", param_field, " field!"))
    if (!param %in% unique(obj[[param_field]]))
      stop(paste0(param, " not found in the ", param_field, " field!"))
    return(filter(obj, .data[[param_field]] == param))
  }
  obj
}
