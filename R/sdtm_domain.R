#' Return a specific domain from a sdtm object
#'
#' @param obj The sdtm object.
#' @param name The domain to return as a single character string.
#' @return The specified domain as data frame. Issues a warning if the domain
#'   does not exist and returns NULL.
#' @export
#' @examples
#' head(domain(examplinib_fe, "dm"), 3)
domain <- function(obj, name) {
  # validate input
  validate_sdtm(obj)
  validate_char_param(name, "name")

  # Normalize domain name to lowercase
  name <- tolower(name)

  # Check if domain exists
  if(!has_domain(obj, name)) {
    stop("Domain '", name, "' not found in SDTM object")
  }

  # Extract domain safely
  temp <- obj$domains[[name]]

  # Return the domain
  return(new_domain(temp))
}


#' Class constructor for domain objects
#'
#' @param domain_data A data.frame.
#'
#' @returns A domain object.
#' @keywords internal
new_domain <- function(
    domain_data
) {
  class(domain_data) <- c("domain", "data.frame")
  domain_data
}


#' Summary for domain object
#'
#' @param object Domain object.
#' @param ... Further parameters.
#' @param silent Suppress messages, defaults to nif_options, if NULL.
#'
#' @returns A summary object
#' @export
#' @keywords internal
summary.domain <- function(object, ..., silent = NULL) {
  # validate input
  if (!inherits(object, "data.frame")) {
    stop("Input must be a data frame")
  }

  # validate_domain(object, silent = silent)
  current_domain <- toupper(unique(object$DOMAIN))
  if(length(current_domain) > 1) {
    stop("Multiple domain values found")
  }

  testcd_field <- paste0(current_domain, "TESTCD")
  test_field <- paste0(current_domain, "TEST")
  tpt_field <- paste0(current_domain, "TPT")
  tptnum_field <- paste0(current_domain, "TPTNUM")
  eltm_field <- paste0(current_domain, "ELTM")
  cat_field <- paste0(current_domain, "CAT")
  scat_field <- paste0(current_domain, "SCAT")
  fast_field <- paste0(current_domain, "FAST")

  if(testcd_field %in% names(object)) {
    test <- distinct(select(
      object,
      any_of(c(test_field, testcd_field, cat_field, scat_field, fast_field, "PCSPEC"))))
    observations <- object %>%
      reframe(n = n(), .by = paste0(current_domain, "TESTCD"))
  } else {
    test <- NULL
    observations <- NULL
  }

  if(tpt_field %in% names(object)) {
    tpt <- distinct(select(
      object, any_of(c(tpt_field, tptnum_field, eltm_field))))
  } else {
    tpt <- NULL
  }

  if("EPOCH" %in% names(object)) {
    epoch <- distinct(object, EPOCH)
  } else {
    epoch <- NULL
  }

  if("VISIT" %in% names(object)) {
    visit <- distinct(select(object, any_of(c("VISIT"))))
  } else {
    visit <- NULL
  }

  category <- distinct(select(object, ends_with("CAT")))
  if(ncol(category) == 0) category <- NULL

  # output
  out <- list(
    data = object,
    domain = current_domain,
    study = unique(object$STUDYID),
    epoch = epoch,
    subjects = distinct(select(object, any_of(c("USUBJID", "SUBJID")))),
    test = test,
    category = category,
    observations = observations,
    tpt = tpt,
    n_obs = nrow(object),
    visit = visit,
    hash = rlang::hash(object),
    last = last_dtc(object)
  )

  class(out) <- "summary_domain"
  return(out)
}


#' Print function for summary_domain objects
#'
#' @param x The sdtm domain summary object.
#' @param ... Further parameters.
#'
#' @returns Nothing.
#' @export
#' @noRd
print.summary_domain <- function(x, ...) {
  indent <- 2
  hline <- paste0(rep("-", 8), collapse = "")
  spacer <- paste0(rep(" ", indent), collapse = "")

  cat(paste(hline, "SDTM domain summary", hline, "\n"))

  cat(paste("Study", x$study, "\n"))
  cat(paste("Domain", x$domain, "\n"))

  cat(paste(nrow(x$subjects), "subjects\n"))
  cat(paste(x$n_obs, "observations\n"))

  cat("\n")

  if(!is.null(x$category)) {
    cat("Categories\n")
    cat(df_to_string(
      x$category, indent = indent, show_none = TRUE
    ), "\n\n")
  }

  if(!is.null(x$test)) {
    cat("Testcodes\n")
    cat(df_to_string(
      x$test, indent = indent, show_none = TRUE
    ), "\n\n")
  }

  if(!is.null(x$tpt)) {
    cat("Observation time points\n")
    cat(df_to_string(
      x$tpt, indent = indent, show_none = TRUE
    ), "\n\n")
  }

  if(!is.null(x$epoch)) {
    cat("Epochs\n")
    cat(df_to_string(
      x$epoch, indent = indent, show_none = TRUE, header = FALSE
    ), "\n")
  }

  cat(paste0("Hash: ", x$hash, "\n"))
  cat(paste0("Last DTC: ", x$last))
}


#' Plot domain object
#'
#' @param x A domain object.
#' @param testcd Testcd field to filter for, defaults to all if NULL.
#' @param ... Further parameters.
#' @param points Plot points, as logical.
#' @param lines Plot lines, as logical.
#' @param legend Plot legend, as logical.
#'
#' @returns A ggplot2 object.
#' @export
#'
#' @examples
#' plot(domain(examplinib_sad, "lb"))
plot.domain <- function(
    x, testcd = NULL, points = TRUE, lines = FALSE,
    legend = TRUE, ...) {
  # input validation
  validate_domain_param(x)
  validate_char_param(testcd, "testcd", allow_null = TRUE)
  validate_logical_param(points, "points")
  validate_logical_param(lines, "lines")
  validate_logical_param(legend, "legend")

  # fields
  domain <- toupper(unique(x$DOMAIN))
  testcd_field <- paste0(domain, "TESTCD")
  dv_field <- paste0(domain, "STRESN")
  time_field <- paste0(domain, "DTC")
  dy_field <- paste0(domain, "DY")
  if(dy_field %in% names(x))
    time_field <- dy_field

  obj <- NULL

  x <- x %>%
    lubrify_dates()

  # specific plot for DM
  if (domain == "DM") {
    missing_fields <- setdiff(c("RFSTDTC", "RFENDTC", "USUBJID"), names(x))
    if(length(missing_fields) > 0)
      stop(paste0(
        "missing ", plural("field", length(missing_fields) > 1), ": ",
        nice_enumeration(missing_fields)
      ))

    obj <- x %>%
      filter(!is.na(.data$RFSTDTC)) %>%
      arrange(.data$RFSTDTC) %>%
      mutate(ID = as.numeric(factor(.data$USUBJID, unique(.data$USUBJID)))) %>%
      ggplot2::ggplot() +
      ggplot2::geom_segment(ggplot2::aes(
        x = .data$RFSTDTC,
        xend = .data$RFENDTC,
        y = .data$ID,
        yend = .data$ID)) +
      {if(points == TRUE) {ggplot2::geom_point(ggplot2::aes(
        x = .data$RFSTDTC, y = .data$ID))}} +
      ggplot2::scale_y_discrete(
        labels = NULL, breaks = NULL, name = "USUBJID") +
      ggplot2::scale_x_datetime(
        name = "RFSTDTC - RFENDTC", date_labels = "%Y-%m-%d")
  }

  # specific plot for EX
  if (domain == "EX") {
    missing_fields <- setdiff(c("EXSTDTC", "EXENDTC", "USUBJID"), names(x))
    if(length(missing_fields) > 0)
      stop(paste0(
        "missing ", plural("field", length(missing_fields) > 1), ": ",
        nice_enumeration(missing_fields)
      ))

    obj <- x %>%
      arrange(.data$EXSTDTC) %>%
      mutate(ID = as.numeric(factor(.data$USUBJID, unique(.data$USUBJID)))) %>%
      ggplot2::ggplot() +
      ggplot2::geom_segment(ggplot2::aes(
        x = .data$EXSTDTC,
        xend = .data$EXENDTC,
        y = .data$ID,
        yend = .data$ID)) +
      {if (points == TRUE) {ggplot2::geom_point(ggplot2::aes(
        x = .data$EXSTDTC, y = .data$ID))}} +
      ggplot2::scale_y_discrete(name = "USUBJID", labels = NULL) +
      ggplot2::scale_x_datetime(
        name = "EXSTDTC - EXENDTC", date_labels = "%Y-%m-%d")
  }

  # generic plot
  if(!domain %in% c("DM", "EX")){
    if(all(c(testcd_field, time_field, dv_field) %in% names(x))) {
      obj <- x %>%
        {if(!is.null(testcd)) filter(., .data[[testcd_field]] == testcd) else .} %>%
        ggplot(aes(x = !!sym(time_field),
                   y = !!sym(dv_field),
                   color = !!sym(testcd_field))) +
        {if(points == TRUE) geom_point()} +
        {if(lines == TRUE) geom_line()}
    }
  }

  obj <- obj +
    {if (legend == TRUE) {
        ggplot2::theme(legend.position = "bottom")
      } else {
        ggplot2::theme(legend.position = "none")
      }} +
    theme_bw() +
    ggtitle(paste0("Domain ", domain))
    watermark()

  return(obj)
}


#' Generate the XXH128 hash of a SDTM domain object
#'
#' @param obj A SDTM domain object.
#'
#' @returns The XXH128 hash of the nif object as character.
#' @export
#' @keywords internal
#' @importFrom rlang hash
hash.domain <- function(obj) {
  # validate input
  if (!inherits(obj, "domain")) {
    stop("Input must be a SDTM domain")
  }

  rlang::hash(obj)
}


#' Last date in SDTM domain object
#'
#' @param obj A nif object.
#'
#' @returns A POSIXct scalar.
#' @export
#' @keywords internal
#'
#' @examples
#' last_dtc(examplinib_sad_nif)
last_dtc.domain <- function(obj) {
  validate_domain(obj, silent = TRUE)
  # out <- last_dtc.data.frame(as.data.frame(obj))
  out <- last_dtc_data_frame(as.data.frame(obj))

  return(out)
}


