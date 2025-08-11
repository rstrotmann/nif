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
summary.domain <- function(object, ..., silent = NULL) {
  # validate input
  if (!inherits(object, "data.frame")) {
    stop("Input must be a data frame")
  }

  validate_domain(object, silent = silent)
  current_domain <- toupper(unique(object$DOMAIN))

  # if(length(current_domain) > 1)
  #   stop(paste0(
  #     "multiple domain codes found: ", nice_enumeration(current_domain)
  #   ))

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
    visit = distinct(select(object, any_of(c("VISIT"))))
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

  if(!is.null(x$test)) {
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
    ), "\n\n")
  }
}










