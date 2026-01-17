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
    "Data disposition:\n",
    df_to_string(x$n_observations, indent = 2)
  ))
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


#' Retrieve dataset from adam object
#'
#' @param adam An adam object.
#' @param name The dataset name as character.
#'
#' @return A data frame.
#' @export
dataset <- function(adam, name) {
  # input validation
  validate_adam(adam)
  validate_char_param(name, "name")

  if (!name %in% names(adam)) {
    stop(paste0(
      "Dataset ", name, " not found in adam object!"
    ))
  }

  adam[[name]]
}
