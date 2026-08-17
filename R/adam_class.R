#' adam object class constructor
#'
#' @param adam_data The ADaM datasets as data frames.
#'
#' @return An adam object.
#' @export
adam <- function(adam_data) {
  temp <- as.list(adam_data)
  class(temp) <- c("adam", "list")
  temp
}


#' Validate adam object
#'
#' @param obj An adam object.
#'
#' @returns Nothing.
#' @noRd
validate_adam <- function(obj) {
  if (!inherits(obj, "adam")) {
    stop("Input must be a adam object")
  }
}


#' Summary method for adam objects
#'
#' @param object An adam object.
#' @param ... Further parameters.
#'
#' @returns A summary_adam object.
#' @export
summary.adam <- function(object, ...) {
  out <- list(
    study = character(0),
    subjects = character(0),
    adam = object,
    n_observations = NULL
  )

  out$study <- purrr::map(
    object,
    function(x) {
      if ("STUDYID" %in% names(x)) {
        unique(x$STUDYID)
      } else {
        NULL
      }
    }
  ) |>
    unlist() |>
    as.character() |>
    unique()

  out$subjects <- purrr::map(
    object,
    function(x) {
      if ("USUBJID" %in% names(x)) {
        unique(x$USUBJID)
      } else {
        NULL
      }
    }
  ) |>
    unlist() |>
    as.character() |>
    unique()

  out$n_observations <- data.frame(
    DATASET = names(object),
    OBSERVATIONS = as.numeric(lapply(object, nrow))
  )

  out$datasets <- names(object)

  if ("adsl" %in% names(object)) {
    out$subj_disposition <- adsl_summary(dataset(object, "adsl"))
  }

  class(out) <- "summary_adam"
  out
}


#' Print function for summary_adam objects
#'
#' @param x The summary_adam object.
#' @param ... Further parameters.
#'
#' @return Nothing.
#' @export
#' @noRd
print.summary_adam <- function(x, ...) {
  hline <- paste0(rep("-", 8), collapse = "")

  cat(paste(hline, "ADaM data set summary", hline, "\n"))

  cat(paste0(
    "Data from ", length(x$subjects), " subjects across ",
    length(x$study), " ",
    plural("study", length(x$study) > 1), ":\n",
    nice_enumeration(x$study),
    "\n\n"
  ))

  cat(paste0(
    "DATASET OVERVIEW:\n",
    df_to_string(x$n_observations, indent = 2),
    "\n"
  ))

  if ("subj_disposition" %in% names(x)) {
    cat("SUBJECT DISPOSITION (non-enrollment failures):\n\n")

    temp <- x$subj_disposition
    cat(paste0(
      "Countries:\n", df_to_string(temp$country, indent = 2), "\n"
    ))

    cat(paste0(
      "Sex:\n", df_to_string(temp$sex, indent = 2), "\n"
    ))

    cat(paste0(
      "Race:\n", df_to_string(temp$race, indent = 2), "\n"
    ))

    cat(paste0(
      "Treatment arms:\n", df_to_string(temp$arm, indent = 2), "\n"
    ))

    cat(paste0(
      "Analysis populations:\n", df_to_string(temp$population, indent = 2), "\n"
    ))
  }
}


#' Print function for adam objects.
#'
#' @param x The adam object.
#' @param ... Further parameters.
#'
#' @returns Nothing.
#' @export
#' @noRd
print.adam <- function(x, ...) {
  print(summary(x))
}

