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
  validate_argument(name, "character")

  if (!name %in% names(adam)) {
    stop(paste0(
      "Dataset ", name, " not found in adam object!"
    ))
  }

  adam[[name]] |>
    new_dataset()
}


#' adam class constructor
#'
#' @param data A data frame
#'
#' @returns An adam dotaset object
#' @noRd
new_dataset <- function(
    data
) {
  # input validation
  validate_df_argument(data)

  # business logic
  class(data) <- c("adam_dataset", "data.frame")
  data
}


#' Summarize adam_dataset
#'
#' @param obj An adam_daaset object.
#'
#' @returns A summary_dataset object.
#' @noRd
#' @export
summary.adam_dataset <- function(object, ...) {
  # input validation
  validate_dataset(object)

  # business logic
  subjects <- unique(object$USUBJID)
  study <- unique(object$STUDYID)

  if ("PARAM" %in% names(object)) {
    params <- object |>
      distinct(across(any_of(c("PARAMCD", "PARAM"))))
  } else {
    params <- NULL
  }

  flags <- names(object)[grepl("FL$", names(object))]

  out <- list(
    data = object,
    domain = ifelse("DOMAIN" %in% names(object), unique(object$DOMAIN), "NULL"),
    subjects = subjects,
    study = study,
    flags = flags,
    params = params
  )

  class(out) <- "summary_dataset"
  out
}


#' Print adam dataset summary objects
#'
#' @param x A summary_dataset object.
#' @param ... Further parameters.
#'
#' @returns Nothing.
#' @noRd
#' @export
print.summary_dataset <- function(x, ...) {
  indent <- 2
  hline <- paste0(rep("-", 8), collapse = "")

  cat(paste(hline, "ADaM dataset summary", hline, "\n"))

  if (!is.null(x$domain)) {
    cat(paste("Domain", x$domain, "\n"))
  }
  cat(paste("Study", x$study, "\n"))

  cat(paste(length(x$subjects), "subjects\n\n"))

  if (!is.null(x$params)) {
    cat(paste0(
      nrow(x$params), " parameters:\n",
      df_to_string(x$params, indent = indent, abbr_threshold = Inf),
      "\n"
    ))
  }

  cat(paste("Flags:", nice_enumeration(x$flags), "\n"))

}
