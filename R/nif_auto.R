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

  extrt <- intersect(as.character(rlang::f_rhs(f)), treatments(sdtm))
  pctestcd <- intersect(as.character(rlang::f_lhs(f)), analytes(sdtm))

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
#' @param sdtm A sdtm object.
#' @param mapping A formla or list of formulae.
#'
#' @importFrom rlang is_formula
#'
#' @returns A data frame.
auto_mapping <- function(sdtm, mapping = NULL) {
  # input validation
  if (!inherits(sdtm, "sdtm")) {
    stop("sdtm must be an sdtm object")
  }

  ex_pc_mapping <- NULL

  if(is.null(mapping)) {
    common <- intersect(analytes(sdtm), treatments(sdtm))
    ex_pc_mapping <- data.frame(
      EXTRT = common,
      PCTESTCD = common,
      ANALYTE = common
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

  ex_pc_mapping <- ex_pc_mapping %>%
    mutate(PARENT = EXTRT) %>%
    mutate(METABOLITE = PCTESTCD != EXTRT) %>%
    group_by(EXTRT) %>%
    mutate(METABOLITE = row_number() != 1)

  ex_pc_mapping
}
