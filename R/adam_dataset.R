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
#' @returns Nothing.
#' @export
adsl_summary <- function(
    obj
) {
  # input validation
  validate_dataset(obj)

  # one record per subject
  temp <- obj |>
    reframe(n = n(), .by = "USUBJID") |>
    filter(n > 1)
  if (nrow(temp) > 0) {
    stop(paste0(
      "data set contains more than one rows per subject!"
    ))
  }

  add_percent <- function(df) {
    df |>
      mutate(percent = round(.data$n/sum(.data$n) * 100, 1))
  }

  if (any(c("ARMCD", "ARM") %in% names(obj))) {
    arm <- obj |>
      reframe(n = n(), .by = any_of(c("ARMCD", "ARM"))) |>
      add_percent()
  } else {
    arm <- NULL
  }

  if ("ARM" %in% names(obj)) {
    treated <-  obj |>
      filter(!toupper(.data$ARM) %in% c("SCREEN FAILURE"))
  } else {
    treated <- obj
  }

  country <- reframe(treated, n = n(), .by = "COUNTRY") |>
    add_percent()

  sex <- reframe(treated, n = n(), .by = "SEX") |>
    add_percent()

  race <- treated |>
    reframe(n = n(), .by = "RACE") |>
    add_percent()

  disposition_by_flag <- function(flag) {
    if (flag %in% names(treated)) {
      treated |>
        reframe(n = n(), .by = all_of(flag)) |>
        add_percent() |>
        filter(.data[[flag]] == "Y") |>
        mutate(population = flag) |>
        select(-c(flag)) |>
        relocate("population")
    } else {
      NULL
    }
  }

  pop <- bind_rows(lapply(
    c("FASFL", "SAFFL", "ITTFL", "PPROTFL", "COMPLFL", "RANDFL", "ENRLFL"),
    disposition_by_flag))

  out <- list(
    country = country,
    site = unique(obj$SITEID),
    sex = sex,
    race = race,
    arm = arm,
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

  if ("adsl" %in% names(object)) {
    subj_disposition <- adsl_summary(dataset(object, "adsl"))
  } else {
    subj_disposition <- NULL
  }

  out <- list(
    data = object,
    domain = ifelse("DOMAIN" %in% names(object), unique(object$DOMAIN), "NULL"),
    subjects = subjects,
    study = study,
    flags = flags,
    params = params,
    subj_disposition = subj_disposition
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
