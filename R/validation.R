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
validate_domain <- function(
    domain,
    silent = NULL,
    call = rlang::caller_env()
) {
  # Input validation
  if (!is.data.frame(domain)) {
    rlang::abort("The 'domain' parameter must be a data frame", call = call,
                 class = "sdtm_arg_error")
  }

  if (!"DOMAIN" %in% colnames(domain)) {
    rlang::abort("The data frame must have a DOMAIN column", call = call,
                 class = "sdtm_arg_error")
  }

  # Check if domain$DOMAIN is empty
  if (length(domain$DOMAIN) == 0 || all(domain$DOMAIN == "")) {
    rlang::abort("The DOMAIN column is empty", call = call,
                 class = "sdtm_arg_error")
  }

  # Get unique domain names and handle multiple values
  domain_name <- unique(domain$DOMAIN)
  if (length(domain_name) > 1) {
    rlang::abort(
      paste0("Multiple domain values found: ", nice_enumeration(domain_name)),
      call = call,
      class = "sdtm_arg_error"
    )
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


#' Check whether object is an adam_dataset
#'
#' @param obj Input.
#'
#' @returns Nothing or stop.
#' @noRd
validate_dataset <- function(obj, call = rlang::caller_env()) {
  if (!inherits(obj, "adam_dataset")) {
    rlang::abort("Input must be an adam_dataset object", call = call,
                 class = "adam_arg_error")
  }
}


#' Validate sdtm argument
#'
#' @param obj A stdm object.
#' @param expected_domains Expected domains as character.
#'
#' @returns Nothing or stop.
#' @noRd
validate_sdtm_argument <- function(
  obj,
  expected_domains = NULL,
  call = rlang::caller_env()
) {
  validate_argument(expected_domains, "character", allow_null = TRUE,
                    allow_multiple = TRUE, call = call)

  if (!inherits(obj, "sdtm")) {
    rlang::abort("Input must be a sdtm object", call = call,
                 class = "sdtm_arg_error")
  }

  if (!is.null(expected_domains)) {
    expected_domains <- tolower(expected_domains)
    missing_domains <- setdiff(expected_domains, names(obj))
    if (length(missing_domains) > 0) {
      rlang::abort(
        paste0(
          "Expected ", plural("domain", length(missing_domains) > 1),
          " missing in sdtm object: ", nice_enumeration(missing_domains)
        ),
        call = call,
        class = "sdtm_arg_error"
      )
    }
  }
}


#' validate function argument
#'
#' @param param The argument.
#' @param type The expected parameter type (one of 'character', 'logical',
#'   'numeric', 'date', or 'function').
#' @param allow_null Allow NULL values.
#' @param allow_empty Allow empty values.
#' @param allow_multiple Allow multiple values.
#' @param allow_na Allow NA values.
#' @param values Allowed values.
#'
#' @returns Nothing or stop.
#' @noRd
validate_argument <- function(
    param,
    type = c(
      "character", "logical", "numeric", "date", "function"
    ),
    allow_null = FALSE,
    allow_empty = FALSE,
    allow_multiple = FALSE,
    allow_na = FALSE,
    values = NULL,
    arg = rlang::caller_arg(param),
    call = rlang::caller_env()
) {
  # Validate type parameter
  type <- match.arg(type)

  # Check for NULL first
  if (is.null(param)) {
    if (allow_null) return(invisible(NULL))
    rlang::abort(
      paste0("`", arg, "` must not be NULL"),
      class = "nif_arg_error", call = call
    )
  }

  # Type checking. A bare NA (of any type) passes the type check for numeric
  # and is handled by the NA check below.
  type_ok <- switch(
    type,
    character = is.character(param),
    logical   = is.logical(param),
    numeric   = is.numeric(param) || (length(param) > 0 && all(is.na(param))),
    date      = lubridate::is.Date(param),
    `function` = is.function(param)
  )

  if (!type_ok) {
    rlang::abort(
      paste0(
        "`", arg, "` must be a ", type, " value",
        if (allow_null) " or NULL",
        ", not ", obj_type_friendly(param), "."
      ),
      class = "nif_arg_error", call = call
    )
  }

  # Check for NA values
  if (type != "function" && !allow_na && any(is.na(param))) {
    rlang::abort(
      paste0("`", arg, "` must not contain NA"),
      class = "nif_arg_error", call = call
    )
  }

  # Length checking
  if (length(param) != 1 && !allow_multiple) {
    rlang::abort(
      paste0("`", arg, "` must be a single value"),
      class = "nif_arg_error", call = call
    )
  }

  # Empty string check (only for character types; ignore NA when allow_na)
  if (
    type == "character" &&
    !allow_empty &&
    length(param) > 0 &&
    any(nchar(param[!is.na(param)]) == 0)
  ) {
    rlang::abort(
      paste0("`", arg, "` must be a non-empty string"),
      class = "nif_arg_error", call = call
    )
  }

  if (!is.null(values) && !all(param %in% values)) {
    rlang::abort(
      paste0(
        "`", arg, "` must be ",
        nice_enumeration(values, conjunction = "or"), "!"
      ),
      class = "nif_arg_error", call = call
    )
  }

  invisible(NULL)
}


#' Check that required columns are present in a data frame
#'
#' Shared core of `validate_df_argument()`, `validate_fields()` and
#' `validate_nif_argument()`. Assumes that `x` has already been confirmed to be
#' a data frame.
#'
#' @param x A data frame.
#' @param fields Required columns, as character, or NULL.
#' @param msg_prefix The beginning of the error message, up to and including
#'   the colon and trailing space. The missing columns are appended.
#' @param call The execution environment of the function to report in the error
#'   message, i.e., the user-facing function.
#'
#' @returns Nothing or abort.
#' @noRd
check_df_columns <- function(
    x,
    fields = NULL,
    msg_prefix = "Missing columns: ",
    call = rlang::caller_env()
) {
  missing_fields <- setdiff(fields, names(x))
  if (length(missing_fields) > 0) {
    rlang::abort(
      paste0(msg_prefix, nice_enumeration(missing_fields)),
      call = call,
      class = "nif_arg_error"
    )
  }
  invisible(NULL)
}


#' Validate function argument as data frame
#'
#' @param param The argument.
#' @param expected_fields Expected columns in the data frame.
#' @param allow_null Allow NULL.
#' @param arg The argument name to use in error messages. Defaults to the
#'   expression passed as `param`.
#' @param call The execution environment of the function to report in error
#'   messages. Defaults to the caller.
#'
#' @returns Nothing or stop.
#' @noRd
validate_df_argument <- function(
    param,
    expected_fields = NULL,
    allow_null = FALSE,
    arg = rlang::caller_arg(param),
    call = rlang::caller_env()
) {
  check_data_frame(param, allow_null = allow_null, arg = arg, call = call)

  if (!is.null(param)) {
    check_df_columns(
      param, expected_fields,
      msg_prefix = paste0("Missing columns in ", arg, ": "),
      call = call
    )
  }
  invisible(NULL)
}


#' Validate fields in a data frame
#'
#' @param obj A data frame.
#' @param fields Required fields as character.
#' @param arg The argument name to use in error messages. Defaults to the
#'   expression passed as `obj`.
#' @param call The execution environment of the function to report in error
#'   messages. Defaults to the caller.
#'
#' @returns Nothing or stop.
#' @noRd
validate_fields <- function(
    obj,
    fields = NULL,
    arg = rlang::caller_arg(obj),
    call = rlang::caller_env()
) {
  # input validation
  check_data_frame(obj, arg = arg, call = call)
  validate_argument(
    fields, "character", allow_null = TRUE, allow_multiple = TRUE,
    call = call)

  # business logic
  check_df_columns(
    obj, fields,
    msg_prefix = "Missing required fields: ",
    call = call
  )
}


#' Validate nif argument
#'
#' Checks that input is a nif object and has at least the minimally required
#' fields, ID, TIME, AMT, CMT, EVID and DV.
#' Further required fields can be specified with 'fields'.
#'
#' @param obj A nif object.
#' @param fields Custom required fields as character.
#'
#' @returns Nothing or stop.
#' @noRd
validate_nif_argument <- function(
    obj,
    fields = NULL,
    call = rlang::caller_env()
) {
  # input validation
  if (!inherits(obj, "nif")) {
    rlang::abort("Input must be a nif object", call = call,
                 class = "nif_arg_error")
  }

  validate_argument(
    fields, "character", allow_null = TRUE, allow_multiple = TRUE,
    call = call)

  # business logic
  check_df_columns(
    obj, minimal_nif_fields,
    msg_prefix = "Missing essential fields in nif object: ",
    call = call
  )

  validate_fields(obj, fields, call = call)

  invisible(NULL)
}


#' Validate domain object parameter
#'
#' @param obj A domain object.
#'
#' @returns Nothing or stop.
#' @noRd
validate_domain_param <- function(obj, call = rlang::caller_env()) {
  if (!inherits(obj, "sdtm_domain")) {
    rlang::abort("Input must be a sdtm_domain object", call = call,
                 class = "sdtm_arg_error")
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
validate_testcd <- function(
    sdtm,
    testcd,
    domain = NULL,
    call = rlang::caller_env()
) {
  # input validation
  validate_argument(domain, "character", allow_null = TRUE, call = call)
  validate_argument(testcd, "character", allow_multiple = TRUE, call = call)
  validate_sdtm_argument(sdtm, domain, call = call)

  if (!is.null(domain)) {
    domain <- tolower(domain)

    temp <- domain(sdtm, domain)
    testcd_field <- paste0(toupper(domain), "TESTCD")

    if (!testcd_field %in% names(temp)) {
      rlang::abort(
        paste0(toupper(domain), " has no ", testcd_field, " field!"),
        call = call,
        class = "sdtm_arg_error"
      )
    }

    missing_testcd <- testcd[!testcd %in% unique(temp[[testcd_field]])]
    if (length(missing_testcd) > 0) {
      rlang::abort(
        paste0(
          "Testcd ", nice_enumeration(missing_testcd),
          " not found in domain ", toupper(domain), "!"
        ),
        call = call,
        class = "sdtm_arg_error"
      )
    }
  }

  missing_testcd <- testcd[!testcd %in% testcd(sdtm)$TESTCD]
  if (length(missing_testcd) > 0) {
    rlang::abort(
      paste0(
        "Testcd ", nice_enumeration(missing_testcd), " not found in sdtm!"
      ),
      call = call,
      class = "sdtm_arg_error"
    )
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
  allow_empty = FALSE,
  call = rlang::caller_env()
) {
  # validate inputs
  validate_nif_argument(nif, fields = "ANALYTE", call = call)

  validate_argument(
    analyte, "character", allow_multiple = allow_multiple,
    allow_null = allow_null, allow_empty = allow_empty, call = call)

  if (!is.null(analyte)) {
    missing_analytes <- setdiff(analyte, analytes(nif))
    if (length(missing_analytes) > 0) {
      rlang::abort(
        paste0(
          plural("Analyte", length(missing_analytes) > 1), " ",
          nice_enumeration(missing_analytes),
          " not found in nif object!"
        ),
        call = call,
        class = "nif_arg_error"
      )
    }
  }
}


#' Confirm valid imputation rule set
#'
#' @param obj A list containing the required imputation functions.
#'
#' @returns Nothing or stop.
#' @noRd
validate_imputation_set <- function(obj, call = rlang::caller_env()) {
  if (!is.list(obj) || is.data.frame(obj))
    rlang::abort("Imputation rule set must be a list!", call = call,
                 class = "nif_imputation_error")

  if (length(obj) == 0)
    return(invisible(NULL))

  if (is.null(names(obj)) || any(names(obj) == ""))
    rlang::abort(
      "All elements of an imputation rule set must be named!", call = call,
      class = "nif_imputation_error"
    )

  # number of positional arguments
  n_positional <- c(
    admin_pre_expansion = 6,
    admin_post_expansion = 6,
    obs_raw = 1,
    obs_final = 1
  )

  unknown <- setdiff(names(obj), names(n_positional))
  if (length(unknown) > 0)
    rlang::abort(
      paste0(
        "Unknown ", plural("slot", length(unknown) > 1),
        " in imputation rule set: ", nice_enumeration(unknown),
        ". Allowed: ", nice_enumeration(names(n_positional))
      ),
      call = call,
      class = "nif_imputation_error"
    )

  slot_problem <- function(name) {
    f <- obj[[name]]
    if (!is.function(f))
      return(paste0("'", name, "' must be a function"))
    args <- names(formals(f))
    if ("..." %in% args)
      return(NA_character_)
    if (!"silent" %in% args ||
        length(setdiff(args, "silent")) < n_positional[[name]])
      return(paste0(
        "'", name, "' must accept ", n_positional[[name]],
        " positional ", plural("argument", n_positional[[name]] > 1),
        " and the 'silent' argument"
      ))
    NA_character_
  }

  problems <- vapply(names(obj), slot_problem, character(1))
  problems <- problems[!is.na(problems)]
  if (length(problems) > 0)
    rlang::abort(
      paste0(
        "Invalid imputation rule set: ",
        paste(problems, collapse = "; "), "!"
      ),
      call = call,
      class = "nif_imputation_error"
    )

  invisible(NULL)
}


#' Recursively validate an expression AST node
#'
#' Walks the parsed expression tree and ensures every node is an allowed
#' construct: comparison operators, logical connectors, `is.na()`, `grepl()`,
#' negation, parentheses, symbols (column names), and literals. Rejects arbitrary
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

    # grepl(pattern, COLUMN) -- pattern must be character literal, second arg column name
    if (fn == "grepl") {
      if (length(node) < 3) {
        stop("grepl() must be called with at least two arguments: pattern and x")
      }
      if (!is.character(node[[2]])) {
        stop("grepl() pattern (first argument) must be a character literal")
      }
      if (!is.symbol(node[[3]])) {
        stop("grepl() x (second argument) must be a column name")
      }
      walk_expr(node[[3]], col_names)
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
      "logical operators (&, |, !), is.na(), grepl(), %in% c(), ",
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
#' operators, `is.na()`, `grepl()`, and literals. Rejects arbitrary function calls,
#' assignments, namespace operators, and other unsafe code.
#'
#' @param filter_string A filter expression as a single character string.
#' @param data Optional data frame. If provided, column names referenced in the
#'   expression are validated against `names(data)`.
#'
#' @return The parsed expression (a language object), invisibly. Stops with an
#'   error if the expression is invalid or contains disallowed constructs.
#' @noRd
validate_filter <- function(filter_string, data = NULL) {
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
