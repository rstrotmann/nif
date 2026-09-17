#' Constructor for adam objects
#'
#' @param adam_data The ADaM datasets as list of data frames.
#' @param source Source information as character.
#'
#' @returns
#' @noRd
new_adam <- function(adam_data, source = "") {
  names(adam_data) <- tolower(names(adam_data))

  structure(
    adam_data,
    class = c("adam", "list"),
    source = source
  )
}


#' Public constructor for adam objects
#'
#' @param adam_data The ADaM datasets as list of data frames.
#' @param source Source information as character.
#'
#' @return An adam object.
#' @export
adam <- function(adam_data, source = "") {
  # input validation
  validate_argument(source, "character", allow_empty = TRUE)

  if (!is.list(adam_data) || is.data.frame(adam_data)) {
    stop("Input must be a list of data frames!")
  }
  temp <- vapply(adam_data, is.data.frame, logical(1))
  if (any(!temp)) {
    stop(paste0(
      "Input is not a data frame: ",
      nice_enumeration(names(adam_data)[!temp])
    ))
  }

  # business logic
  new_adam(adam_data, source = source)
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
    n_observations = NULL,
    source = attr(object, "source")
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
  cat(paste(hline(), "ADaM data summary", hline(), "\n"))

  out <- list(
    compose_message(
      paste0("Source: ", x$source),
      condition = (!is.null(x$source) & nchar(x$source) > 0)
    ),

    compose_message(
      paste("Data from", length(x$subjects), "subjects across",
        ifelse(
          length(x$study) == 1, "one study:",
          paste0(length(x$study), "studies:")
        ),
        nice_enumeration(x$study)
      )
    ),
    compose_message("Datasets:", x$n_observations)
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

