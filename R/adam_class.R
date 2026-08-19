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
  cat(paste(hline(), "ADaM data set summary", hline(), "\n"))

  out <- list(
    compose_message(
      paste("Data from", length(x$subjects), "subjects across",
        ifelse(
          length(x$study) == 1, "one study:",
          paste0(length(x$study), "studies:")
        ),
        nice_enumeration(x$study)
      )
    ),
    compose_message("Dataset overview:", x$n_observations)
  )

  if ("subj_disposition" %in% names(x)) {
    temp <- x$subj_disposition
    out <- append(
      out,
      list(
        compose_message(paste(
          hline(3), "Subject disposition (non-enrollment failures)")
        ),
        compose_message("Countries:", temp$country),
        compose_message("Sex:", temp$sex),
        compose_message("Race:", temp$race),
        compose_message("Treatment arms:",  temp$arm),
        compose_message("Analysis populations:", temp$population),
        compose_message("Completion status:", temp$eos)
      )
    )
  }

    cat_message(out)
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

