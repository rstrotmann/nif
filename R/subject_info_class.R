#' Baseline details for specific subjects
#'
#' @param obj The object, either an SDTM or NIF object.
#' @param id The ID or USUBJID as numeric or character.
#' @param ... Further arguments.
#' @export
#' @examples
#' subject_info(examplinib_fe, subjects(examplinib_fe)[1, "USUBJID"])
#' subject_info(examplinib_poc_nif, 1)
subject_info <- function(obj, id, ...) {
  UseMethod("subject_info")
}


#' Print subject_info object
#'
#' @param x The subject_info object.
#' @param ... Further parameters.
#'
#' @returns Nothing.
#' @exportS3Method base::print
#' @noRd
print.subject_info <- function(x, ...) {
  cat(paste0(hline(), " Subject information ", hline(), "\n"))

  temp <- x |>
    mutate(across(everything(), as.character))

  rbind(colnames(temp), temp) |>
    as.matrix() |>
    t() |>
    data.frame() |>
    df_to_string(header = F) |>
    cat()

  invisible(x)
}


#' Baseline details for specific subjects
#'
#' @inheritParams subject_info
#' @export
#' @keywords internal
#' @examples
#' subject_info(examplinib_fe, subjects(examplinib_fe)[1, "USUBJID"])
#' subject_info(examplinib_fe, subjects(examplinib_fe)[1:3, "USUBJID"])
subject_info.sdtm <- function(obj, id, ...) {
  # input validation
  validate_sdtm_argument(obj, "dm")
  validate_argument(id, "character", allow_multiple = TRUE)

  out <- domain(obj, "dm") |>
    filter(.data$USUBJID %in% id) |>
    select(any_of(c(
      "SUBJID", "USUBJID", "ARM", "ARMCD", "ACTARM",
      "ACTARMCD", "SITEID", "COUNTRY", "RFSTDTC", "RFENDTC", "SEX", "AGE",
      "RACE", "ETHNIC"
    )))

  class(out) <- c("subject_info", "tbl_df", "tbl", "data.frame")
  out
}


#' Baseline details for specific subjects
#'
#' @param obj A NIF object.
#' @param id The USUBJID or ID.
#' @param digits Number of decimal places for numerical values.
#' @param ... Further arguments.
#' @export
#' @noRd
#' @examples
#' subject_info(examplinib_poc_nif, 1)
#' subject_info(examplinib_poc_nif, 1:3)
subject_info.nif <- function(obj, id, digits = 1, ...) {
  # input validation
  validate_nif_argument(obj)

  out <- obj |>
    filter(.data$ID %in% id | .data$USUBJID %in% id) |>
    select(any_of(c(
      "ID", "USUBJID", "ACTARMCD", "PART", "COHORT", "SEX", "AGE", "RACE",
      "WEIGHT", "HEIGHT", "BMI"
      )), starts_with("BL_")
    ) |>
    distinct_all() |>
    mutate(across(where(is.numeric), function(x) round(x, digits))) |>
    as_tibble()

  class(out) <- c("subject_info", "tbl_df", "tbl", "data.frame")
  out
}
