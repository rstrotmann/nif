#' Check whether fields in domain are compliant with SDTM
#'
#' @param domain The SDTM domain as data frame.
#' @param silent Suppress optional messages, as logical. Defaults to global
#'   nif_options if NULL.
#'
#' @return Invisibly returns TRUE if validation passes, or stops with an error
#'  if required columns are missing.
#' @noRd
#'
#' @examples
#' validate_domain(domain(examplinib_sad, "dm"))
validate_domain <- function(domain, silent = NULL) {
  # Input validation
  if (!is.data.frame(domain)) {
    stop("The 'domain' parameter must be a data frame")
  }

  if (!"DOMAIN" %in% colnames(domain)) {
    stop("The data frame must have a DOMAIN column")
  }

  # Check if domain$DOMAIN is empty
  if (length(domain$DOMAIN) == 0 || all(domain$DOMAIN == "")) {
    stop("The DOMAIN column is empty")
  }

  # Get unique domain names and handle multiple values
  domain_name <- unique(domain$DOMAIN)
  if (length(domain_name) > 1) {
    stop(paste0(
      "Multiple domain values found: ", nice_enumeration(domain_name)
    ))
  }

  if (!domain_name %in% unique(nif::domain_model$DOMAIN)) {
    conditional_message(
      "Unknown domain '", domain_name, "' cannot be validated!",
      silent = silent
    )
    invisible(TRUE)
  } else {
    temp <- nif::domain_model |>
      dplyr::filter(.data$DOMAIN == domain_name)

    required_names <- temp |>
      dplyr::filter(.data$CORE == "Req") |>
      dplyr::pull(.data$VARNAM)

    expected_names <- temp |>
      dplyr::filter(.data$CORE == "Exp") |>
      dplyr::pull(.data$VARNAM)

    permitted_names <- temp |>
      dplyr::filter(.data$CORE == "Perm") |>
      dplyr::pull(.data$VARNAM)

    missing_req <- setdiff(required_names, colnames(domain))
    missing_exp <- setdiff(expected_names, colnames(domain))
    missing_perm <- setdiff(permitted_names, colnames(domain))

    if (length(missing_req) > 0) {
      conditional_message(
        "The following required columns are missing in ",
        domain_name, ": ",
        paste(missing_req, collapse = ", "),
        silent = silent
      )
    }

    if (length(missing_exp) > 0) {
      conditional_message(
        "The following expected columns are missing in domain ",
        domain_name, ": ",
        paste(missing_exp, collapse = ", "),
        silent = silent
      )
    }

    if (length(missing_perm) > 0) {
      conditional_message(
        "The following permitted columns are missing in domain ",
        domain_name, ": ",
        paste(missing_perm, collapse = ", "),
        silent = silent
      )
    }

    invisible(TRUE)
  }
}


#' Check whether domains of sdtm object are compliant with SDTM standard
#'
#' @param sdtm SDTM object.
#' @param silent Suppress optional messages, as logical. Defaults to global
#'   nif_options if NULL.
#'
#' @return Invisibly returns TRUE if validation passes, or stops with an error
#'  if required columns are missing.
#' @noRd
validate_sdtm_domains <- function(sdtm, silent = NULL) {
  for (d in sdtm$domains) {
    validate_domain(d, silent = silent)
  }
}


#' Validate sdtm object
#'
#' @param obj A stdm object.
#' @param expected_domains Expected domains as character.
#'
#' @returns Nothing or stop.
#' @noRd
validate_sdtm <- function(
  obj,
  expected_domains = NULL
) {
  validate_char_param(expected_domains, "expected_domains",
    allow_null = TRUE,
    allow_multiple = TRUE
  )

  if (!inherits(obj, "sdtm")) {
    stop("Input must be a sdtm object")
  }

  if (!is.null(expected_domains)) {
    expected_domains <- tolower(expected_domains)
    missing_domains <- setdiff(expected_domains, names(obj$domains))
    if (length(missing_domains) > 0) {
      stop(paste0(
        "Expected ", plural("domain", length(missing_domains) > 1),
        " missing in sdtm object: ", nice_enumeration(missing_domains)
      ))
    }
  }
}


#' Generic function parameter validation
#'
#' @param type Parameter type (string, logical or numeric)
#' @param param The parameter to be tested.
#' @param param_name The parameter name as character.
#' @param allow_null Allow NULL values, as logical.
#' @param allow_empty Allow empty parameter, as logical.
#' @param allow_multiple Allow vector of the specified type, as logical.
#' @param allow_na Allow NA value, as logical.
#'
#' @returns Nothing or stop.
#' @noRd
validate_param <- function(
  type = c("character", "string", "logical", "numeric"),
  param,
  param_name,
  allow_null = FALSE,
  allow_empty = FALSE,
  allow_multiple = FALSE,
  allow_na = FALSE
) {
  # Validate type parameter
  type <- match.arg(type)

  # Check for NULL first
  if (is.null(param)) {
    if (allow_null) {
      return(invisible(NULL))
    } else {
      stop(paste0(param_name, " must not be NULL"))
    }
  }

  # Check for NA values
  if (!allow_na && any(is.na(param))) {
    stop(paste0(param_name, " must not contain NA"))
  }

  # Type checking
  if ((type == "string" && !is.character(param)) ||
        (type == "character" && !is.character(param)) ||
        (type == "logical" && !is.logical(param)) ||
        (type == "numeric" && !is.numeric(param))) {
    stop(paste0(param_name, " must be a ", type, " value"))
  }

  # Length checking
  if (length(param) != 1 && !allow_multiple) {
    stop(paste0(param_name, " must be a single value"))
  }

  # Empty string check (only for character types)
  if (
    (type == "string" || type == "character") &&
      !allow_empty && length(param) > 0 &&
      any(nchar(param) == 0)
  ) {
    stop(paste0(param_name, " must be a non-empty string"))
  }

  invisible(NULL)
}


#' validate function argument
#'
#' @param param The argument.
#' @param type The expected parameter type (one of 'character', 'logical',
#' 'numeric' or 'date').
#' @param allow_null Allow NULL values.
#' @param allow_empty Allow empty values.
#' @param allow_multiple Allow multiple values.
#' @param allow_na Allow NA values.
#'
#' @returns Nothing or stop.
#' @noRd
validate_argument <- function(
    param,
    type = c("character", "logical", "numeric", "date"),
    allow_null = FALSE,
    allow_empty = FALSE,
    allow_multiple = FALSE,
    allow_na = FALSE
) {
  # Validate type parameter
  type <- match.arg(type)

  param_name <- deparse(substitute(param))

  # Check for NULL first
  if (is.null(param)) {
    if (allow_null) {
      return(invisible(NULL))
    } else {
      stop(paste0(param_name, " must not be NULL"))
    }
  }

  # Check for NA values
  if (!allow_na && any(is.na(param))) {
    stop(paste0(param_name, " must not contain NA"))
  }

  # Type checking
  if ((type == "character" && !is.character(param)) ||
      (type == "logical" && !is.logical(param)) ||
      (type == "numeric" && !is.numeric(param)) ||
      (type == "date" && !is.Date(param))) {
    stop(paste0(param_name, " must be a ", type, " value"))
  }

  # Length checking
  if (length(param) != 1 && !allow_multiple) {
    stop(paste0(param_name, " must be a single value"))
  }

  # Empty string check (only for character types)
  if (
    type == "character" &&
    !allow_empty &&
    length(param) > 0 &&
    any(nchar(param) == 0)
  ) {
    stop(paste0(param_name, " must be a non-empty string"))
  }

  invisible(NULL)
}



#' Validate character parameter
#'
#' @param param The parameter to be tested.
#' @param param_name The parameter name as character.
#' @param allow_null Allow NULL values, as logical.
#' @param allow_empty Allow empty parameter, as logical.
#' @param allow_multiple Allow vector of the specified type, as logical.
#'
#' @returns Nothing or stop.
#' @noRd
validate_char_param <- function(
  param,
  param_name,
  allow_null = FALSE,
  allow_empty = FALSE,
  allow_multiple = FALSE
) {
  validate_param(
    "string",
    param,
    param_name,
    allow_null,
    allow_empty,
    allow_multiple
  )
}

#' Validate logical parameter
#'
#' @param param The parameter to be tested.
#' @param param_name The parameter name as character.
#' @param allow_null Allow NULL values, as logical.
#' @param allow_empty Allow empty parameter, as logical.
#' @param allow_multiple Allow vector of the specified type, as logical.
#'
#' @returns Nothing or stop.
#' @noRd
validate_logical_param <- function(
  param,
  param_name,
  allow_null = FALSE,
  allow_empty = FALSE,
  allow_multiple = FALSE
) {
  validate_param(
    "logical",
    param,
    param_name,
    allow_null,
    allow_empty,
    allow_multiple
  )
}

#' Validate numeric parameter
#'
#' @param param The parameter to be tested.
#' @param param_name The parameter name as character.
#' @param allow_null Allow NULL values, as logical.
#' @param allow_empty Allow empty parameter, as logical.
#' @param allow_multiple Allow vector of the specified type, as logical.
#' @param allow_na Allow NA values, as logical.
#'
#' @returns Nothing or stop.
#' @noRd
validate_numeric_param <- function(
  param,
  param_name,
  allow_null = FALSE,
  allow_empty = FALSE,
  allow_multiple = FALSE,
  allow_na = FALSE
) {
  validate_param(
    "numeric",
    param,
    param_name,
    allow_null,
    allow_empty,
    allow_multiple,
    allow_na
  )
}


#' Validate nif object parameter
#'
#' @param obj A nif object.
#'
#' @returns Nothing or stop.
#' @noRd
#'
validate_nif <- function(obj) {
  if (!inherits(obj, "nif")) {
    stop("Input must be a nif object")
  }

  missing_minimal_fields <- setdiff(minimal_nif_fields, names(obj))
  if (length(missing_minimal_fields) > 0) {
    stop(paste0(
      "Missing essential fields in nif object: ",
      nice_enumeration(missing_minimal_fields)
    ))
  }
}


#' Validate domain object parameter
#'
#' @param obj A domain object.
#'
#' @returns Nothing or stop.
#' @noRd
validate_domain_param <- function(obj) {
  if (!inherits(obj, "domain")) {
    stop("Input must be a domain object")
  }
}


#' Validate nif object with minimally required fields
#'
#' @param obj A nif object.
#' @param additional_fields Additional required fields as character.
#'
#' @returns Nothing or stop.
#' @noRd
#'
validate_min_nif <- function(obj, additional_fields = NULL) {
  validate_nif(obj)

  missing_fields <- setdiff(
    c(minimal_nif_fields, additional_fields),
    names(obj)
  )

  if (length(missing_fields) > 0) {
    stop(paste0(
      "missing required ", plural("field", length(missing_fields) > 1), ": ",
      nice_enumeration(missing_fields)
    ))
  }
}


#' Validate that one or multiple testcd are included in a sdtm object
#'
#' @param sdtm A sdtm object.
#' @param testcd Testcode to validate as character.
#' @param domain Domain as character
#' @noRd
#'
#' @returns Validated testcode(s) as character.
validate_testcd <- function(sdtm, testcd, domain = NULL) {
  # input validation
  validate_char_param(domain, "domain", allow_null = TRUE)
  validate_char_param(testcd, "testcd", allow_multiple = TRUE)

  if (!is.null(domain)) {
    domain <- tolower(domain)
    if (!domain %in% names(sdtm$domains)) {
      stop(paste0(
        "Domain ", domain, " not found in sdtm object!"
      ))
    }

    temp <- domain(sdtm, domain)
    testcd_field <- paste0(toupper(domain), "TESTCD")

    if (!testcd_field %in% names(temp)) {
      stop(paste0(
        toupper(domain), " has no ", testcd_field, " field!"
      ))
    }

    missing_testcd <- testcd[!testcd %in% unique(temp[[testcd_field]])]
    if (length(missing_testcd) > 0) {
      stop(paste0(
        "Testcd ", nice_enumeration(missing_testcd),
        " not found in domain ", toupper(domain), "!"
      ))
    }
  }

  missing_testcd <- testcd[!testcd %in% testcd(sdtm)$TESTCD]
  if (length(missing_testcd) > 0) {
    stop(paste0(
      "Testcd ", nice_enumeration(missing_testcd), " not found in sdtm!"
    ))
  }

  testcd
}


#' Ensure that analytes are present in nif object
#'
#' @param nif A nif object.
#' @param analyte Required analyte(s) as character.
#' @param allow_null Allow NULL values, as logical.
#' @param allow_empty Allow empty parameter, as logical.
#' @param allow_multiple Allow vector of the specified type, as logical.
#'
#' @returns Nothing or stop.
#' @noRd
validate_analyte <- function(
  nif,
  analyte,
  allow_multiple = TRUE,
  allow_null = FALSE,
  allow_empty = FALSE
) {
  # validate inputs
  validate_nif(nif)
  # validate_char_param(
  #   analyte, "analyte",
  #   allow_multiple = allow_multiple, allow_null = allow_null,
  #   allow_empty = allow_empty
  # )

  validate_argument(analyte, "character", allow_multiple = allow_multiple,
                    allow_null = allow_null, allow_empty = allow_empty)

  if (!is.null(analyte)) {
    missing_analytes <- setdiff(analyte, analytes(nif))
    if (length(missing_analytes) > 0) {
      stop(
        paste0(
          plural("Analyte", length(missing_analytes) > 1)
        ), " ",
        nice_enumeration(missing_analytes),
        " not found in nif object!"
      )
    }
  }
}


#' Confirm that fields are present in nif object
#'
#' @param obj A data frame.
#' @param fields Fields(s) to confirm presence of.
#'
#' @returns Nothing or stop.
#' @noRd
validate_fields <- function(obj, fields) {
  missing_fields <- setdiff(fields, names(obj))
  if (length(missing_fields) > 0) {
    stop(paste0(
      plural("Field", length(missing_fields) > 1),
      " not found in nif object: ", nice_enumeration(missing_fields)
    ))
  }
}


#' Confirm valid imputation rule set
#'
#' @param obj A list containing the required imputation functions.
#'
#' @returns Nothing or stop.
#' @noRd
validate_imputation_set <- function(obj) {
  if (!is.list(obj))
    stop("Imputation rule set must be a list!")

  expected_fields <- c("admin_pre_expansion", "admin_post_expansion",
                       "obs_raw", "obs_final")
  missing_fields <- setdiff(expected_fields, names(obj))
  if (length(missing_fields) > 0) {
    stop(paste0(
      "Missing ", plural("field", length(missing_fields) > 1),
      " in imputation rule set: ", nice_enumeration(missing_fields)))
  }
}


#' Test whether a filter term is valid
#'
#' @param data A data frame.
#' @param filter_string A filter term as character.
#' @param silent Suppress messages.
#'
#' @returns Logical.
#' @noRd
is_valid_filter <- function(data, filter_string, silent = TRUE) {
  # Input validation
  if (!is.data.frame(data)) {
    stop("data must be a data frame")
  }

  if (!is.character(filter_string) || length(filter_string) != 1) {
    stop("filter_string must be a single character string")
  }

  # Check for empty filter string
  if (nchar(trimws(filter_string)) == 0)
    return(FALSE)

  # Parse the filter expression
  filter_expr <- tryCatch(
    {
      rlang::parse_expr(filter_string)
    },
    error = function(e) FALSE
  )

  if (isFALSE(filter_expr))
    return(FALSE)

  # Extract column names from the expression
  col_names <- tryCatch(
    {
      all.vars(filter_expr)
    },
    error = function(e) FALSE
  )

  if (isFALSE(col_names))
    return(FALSE)

  # Check if all columns exist in the data frame
  if (!all(col_names %in% names(data)))
    return(FALSE)

  # Try to evaluate the filter expression
  result <- tryCatch(
    {
      # Try to evaluate the filter on the test row
      eval_result <- dplyr::filter(data, !!filter_expr)
      if (nrow(eval_result) == 0) {
        conditional_message(
          "filter '",
          filter_string,
          "' does not give any results!",
          silent = silent
        )
      }
      return(TRUE)
    },
    error = function(e) {
      FALSE
    }
  )

  result
}


#' Recursively validate an expression AST node
#'
#' Walks the parsed expression tree and ensures every node is an allowed
#' construct: comparison operators, logical connectors, `is.na()`, negation,
#' parentheses, symbols (column names), and literals. Rejects arbitrary
#' function calls, assignments, and other unsafe constructs.
#'
#' @param node A language object (parsed R expression node).
#' @param col_names Character vector of allowed column names, or NULL to skip
#'   column validation.
#'
#' @return Invisibly returns TRUE if the node is valid. Stops with an
#'   informative error otherwise.
#' @noRd
walk_expr <- function(node, col_names = NULL) {
  allowed_ops <- c("<=", "<", ">=", ">", "==", "!=", "&", "&&", "|", "||")

  if (is.symbol(node)) {
    name <- as.character(node)
    if (name %in% c("TRUE", "FALSE", "T", "F")) {
      return(invisible(TRUE))
    }
    if (!is.null(col_names) && !name %in% col_names) {
      stop(
        "Column '", name, "' not found in data. ",
        "Available columns: ", paste(col_names, collapse = ", ")
      )
    }
    return(invisible(TRUE))
  }

  if (is.numeric(node) || is.character(node) || is.logical(node)) {
    return(invisible(TRUE))
  }

  if (is.call(node)) {
    fn_node <- node[[1]]

    # handle namespace calls (e.g., pkg::fun) -- fn_node is itself a call
    if (is.call(fn_node)) {
      allowed_ns_funs <- c("lubridate::as_datetime")
      ns_name <- deparse(fn_node)
      if (!ns_name %in% allowed_ns_funs) {
        stop(
          "Disallowed construct in filter expression: '",
          ns_name, "'. ",
          "Only these namespaced functions are allowed: ",
          paste(allowed_ns_funs, collapse = ", ")
        )
      }
      for (i in seq_along(node)[-1]) {
        arg <- node[[i]]
        if (!(is.numeric(arg) || is.character(arg) || is.logical(arg))) {
          stop(
            ns_name, "() arguments must be literals, got: ",
            deparse(arg)
          )
        }
      }
      return(invisible(TRUE))
    }

    fn <- as.character(fn_node)

    # comparison and logical operators
    if (fn %in% allowed_ops) {
      walk_expr(node[[2]], col_names)
      walk_expr(node[[3]], col_names)
      return(invisible(TRUE))
    }

    # unary negation / not
    if (fn == "!") {
      walk_expr(node[[2]], col_names)
      return(invisible(TRUE))
    }

    # unary minus (negative numeric literals like -1)
    if (fn == "-" && length(node) == 2) {
      walk_expr(node[[2]], col_names)
      return(invisible(TRUE))
    }

    # parenthesized grouping
    if (fn == "(") {
      walk_expr(node[[2]], col_names)
      return(invisible(TRUE))
    }

    # is.na(COLUMN) -- argument must be a symbol
    if (fn == "is.na") {
      if (length(node) != 2 || !is.symbol(node[[2]])) {
        stop("is.na() must be called with a single column name")
      }
      walk_expr(node[[2]], col_names)
      return(invisible(TRUE))
    }

    # COL %in% c(...) -- LHS must be a symbol, RHS must be c() of literals
    if (fn == "%in%") {
      lhs <- node[[2]]
      rhs <- node[[3]]
      if (!is.symbol(lhs)) {
        stop("%in% left-hand side must be a column name")
      }
      walk_expr(lhs, col_names)
      if (!is.call(rhs) || !identical(as.character(rhs[[1]]), "c")) {
        stop("%in% right-hand side must be a c() call of literals")
      }
      for (i in seq_along(rhs)[-1]) {
        elem <- rhs[[i]]
        if (!(is.numeric(elem) || is.character(elem) || is.logical(elem))) {
          stop(
            "%in% c() arguments must be literals, got: ",
            deparse(elem)
          )
        }
      }
      return(invisible(TRUE))
    }

    stop(
      "Disallowed construct in filter expression: '", fn, "'. ",
      "Only column comparisons (<=, <, >=, >, ==, !=), ",
      "logical operators (&, |, !), is.na(), %in% c(), ",
      "and literals are allowed."
    )
  }

  stop(
    "Unexpected element in filter expression: ",
    deparse(node)
  )
}


#' Validate and parse a filter expression string
#'
#' Parses the filter string with [rlang::parse_expr()] and walks the resulting
#' AST to ensure it contains only safe constructs: column comparisons, logical
#' operators, `is.na()`, and literals. Rejects arbitrary function calls,
#' assignments, namespace operators, and other unsafe code.
#'
#' @param filter_string A filter expression as a single character string.
#' @param data Optional data frame. If provided, column names referenced in the
#'   expression are validated against `names(data)`.
#'
#' @return The parsed expression (a language object), invisibly. Stops with an
#'   error if the expression is invalid or contains disallowed constructs.
#' @noRd
validate_filter_ast <- function(filter_string, data = NULL) {
  if (!is.character(filter_string) || length(filter_string) != 1) {
    stop("filter_string must be a single character string")
  }

  if (nchar(trimws(filter_string)) == 0) {
    stop("filter_string must not be empty")
  }

  expr <- tryCatch(
    rlang::parse_expr(filter_string),
    error = function(e) {
      stop("Failed to parse filter expression: ", e$message)
    }
  )

  col_names <- if (!is.null(data)) names(data) else NULL
  walk_expr(expr, col_names)

  invisible(expr)
}
