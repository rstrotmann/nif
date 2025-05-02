#' Make analyte mapping table from formula object
#'
#' The formula object must be given as PCTESTCD ~ EXTRT or, if there are multiple
#' analytes from the same treatment: PCTESTCD1 + PCTESTCD2 ~ EXTRT, etc.
#'
#' The return object is a data frame with one line for each analyte. The ANALYTE
#' field is the PCTESTCD for the respective analyte.
#'
#' @param sdtm A sdtm object.
#' @param f A formula.
#' @importFrom rlang f_rhs
#' @importFrom rlang f_lhs
#' @importFrom rlang is_formula
#'
#' @returns A data frame.
formula_to_mapping <- function(sdtm, f) {
  # input validation
  if (!inherits(sdtm, "sdtm")) {
    stop("sdtm must be an sdtm object")
  }

  if(!rlang::is_formula(f))
    stop(paste0(
      "argument ", as.character(f), " is not a formula!"))

  extrt <- intersect(str_split_1(deparse(f_rhs(f)), " "), treatments(sdtm))
  pctestcd <- intersect(str_split_1(deparse(f_lhs(f)), " "), analytes(sdtm))

  if(length(extrt) == 0)
    stop(paste0("extrt ", as.character(rlang::f_rhs(f)),
                " not found in EX!"))
  if(length(pctestcd) == 0)
    stop(paste0("pctestcd ", as.character(rlang::f_lhs(f)),
                " not found in PC!"))

  out <- data.frame(
    PCTESTCD = pctestcd,
    EXTRT = extrt,
    ANALYTE = pctestcd
  )
  return(out)
}


#' Create analyte mapping table from mapping formula or list
#'
#' Creates a mapping between treatments (EXTRT) and analytes (PCTESTCD). Each
#' combination of EXTRT and PCTESTCD must be unique. If duplicate mappings are
#' found, an error will be raised.
#'
#' @param sdtm A sdtm object.
#' @param mapping A formula or list of formulae. Each formula should be in the
#'   format PCTESTCD ~ EXTRT or PCTESTCD1 + PCTESTCD2 ~ EXTRT for multiple
#'   analytes from the same treatment.
#'
#' @importFrom rlang is_formula
#' @importFrom dplyr bind_rows
#' @importFrom dplyr distinct
#'
#' @returns A data frame with unique combinations of EXTRT and PCTESTCD.
auto_mapping <- function(sdtm, mapping = NULL) {
  # input validation
  if (!inherits(sdtm, "sdtm")) {
    stop("sdtm must be an sdtm object")
  }

  # Validate mapping parameter
  if (!is.null(mapping)) {
    if (!rlang::is_formula(mapping) && !is.list(mapping)) {
      stop("mapping must be either NULL, a formula, or a list of formulae")
    }

    if (is.list(mapping)) {
      # Validate each element in the list
      invalid_elements <- !sapply(mapping, rlang::is_formula)
      if (any(invalid_elements)) {
        stop("All elements in mapping list must be formulae. Found invalid elements at positions: ",
             paste(which(invalid_elements), collapse = ", "))
      }
    }
  }

  # Initialize empty data frame with correct structure
  ex_pc_mapping <- data.frame(
    PCTESTCD = character(0),
    EXTRT = character(0),
    ANALYTE = character(0),
    stringsAsFactors = FALSE
  )

  if(is.null(mapping)) {
    common <- intersect(analytes(sdtm), treatments(sdtm))
    ex_pc_mapping <- data.frame(
      EXTRT = common,
      PCTESTCD = common,
      ANALYTE = common,
      stringsAsFactors = FALSE
    )
  } else {
    if(rlang::is_formula(mapping)) {
      ex_pc_mapping <- bind_rows(
        ex_pc_mapping, formula_to_mapping(sdtm, mapping)
      )
    } else {
      if(is.list(mapping)) {
        for(x in mapping) {
          ex_pc_mapping <- bind_rows(
            ex_pc_mapping, formula_to_mapping(sdtm, x)
          )
        }
      }
    }
  }

  # Check for duplicate mappings
  duplicates <- ex_pc_mapping %>%
    group_by(EXTRT, PCTESTCD) %>%
    filter(n() > 1) %>%
    distinct(EXTRT, PCTESTCD)

  if (nrow(duplicates) > 0) {
    stop("Duplicate mappings found for the following EXTRT-PCTESTCD combinations:\n",
         paste(sprintf("%s ~ %s", duplicates$EXTRT, duplicates$PCTESTCD),
               collapse = "\n"))
  }

  ex_pc_mapping <- ex_pc_mapping %>%
    mutate(PARENT = EXTRT) %>%
    mutate(METABOLITE = PCTESTCD != EXTRT) %>%
    group_by(EXTRT) %>%
    mutate(METABOLITE = row_number() != 1)

  ex_pc_mapping
}
