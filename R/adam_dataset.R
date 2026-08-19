#' Retrieve dataset from adam object
#'
#' Overview: [ADaMIG v1.3](https://www.cdisc.org/standards/foundational/adam/adamig-v1-3)
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


#' Summarize ADSL dataset
#'
#' [Reference](https://sastricks.com/cdisc/ADaMIG_v1.3.pdf)
#'
#' @param obj An adam dataset object.
#'
#' @returns A list.
#' @export
adsl_summary <- function(
    obj
) {
  # input validation
  validate_dataset(obj)

  # definitions
  population_flags <- tibble::tribble(
    ~flag,                ~population,
    "FASFL", "Full Analysis Set",
    "SAFFL",            "Safety",
    "ITTFL",   "Intent-To-Treat",
    "PPROTFL",      "Per-Protocol",
    "COMPLFL",        "Completers",
    "RANDFL",        "Randomized",
    "ENRLFL",          "Enrolled"
  )

  # one record per subject
  temp <- obj |>
    reframe(n = n(), .by = "USUBJID") |>
    filter(n > 1)
  if (nrow(temp) > 0) {
    stop(paste0(
      "data set contains more than one rows per subject!"
    ))
  }

  if ("TRT01P" %in% names(obj)) {
    treated <-  obj |>
      filter(!toupper(.data$TRT01P) %in% c("SCREEN FAILURE"))
  } else {
    treated <- obj
  }

  summary_by_field <- function(field) {
    matched_fields <- intersect(names(treated), field)
    if (length(matched_fields) == 0)
      return(NULL)
    reframe(treated, n = n(), .by = any_of(matched_fields)) |>
      add_percent()
  }

  disposition_by_flag <- function(flag) {
    if (flag %in% names(treated)) {
      treated |>
        reframe(n = n(), .by = all_of(flag)) |>
        add_percent() |>
        filter(.data[[flag]] == "Y") |>
        mutate(flag = flag) |>
        select(-all_of(flag)) |>
        relocate("flag")
    } else {
      NULL
    }
  }

  pop <- bind_rows(lapply(
    c("FASFL", "SAFFL", "ITTFL", "PPROTFL", "COMPLFL", "RANDFL", "ENRLFL"),
    disposition_by_flag
  ))

  if (is.null(pop) || nrow(pop) == 0) {
    pop <- NULL
  } else {
    pop <- pop |>
      left_join(population_flags, by = "flag") |>
      relocate("population")
  }

  out <- list(
    country = summary_by_field("COUNTRY"),
    site = unique(treated$SITEID),
    sex = summary_by_field("SEX"),
    race = summary_by_field("RACE"),
    arm = summary_by_field(c("TRT01P", "TRT01A")),
    eos = summary_by_field("EOSSTT"),
    population = pop
  )

  return(out)
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
  cat(paste(hline(), "ADaM dataset summary", hline(), "\n"))

  out <- list(
    compose_message(paste0(
      "Domain: ", x$domain, "\n",
      "Study: ", x$study, "\n",
      length(x$subjects), " subjects"
    )),

    # compose_message("Parameters:", x$params, condition = !is.null(x$params)),
    compose_message("Parameters:", x$params),
    compose_message("Flags:", paste0("  ", nice_enumeration(x$flags)))
  )

  cat_message(out)
}
