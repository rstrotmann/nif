#' Load SDTM data from sas7bdat files
#'
#' This function loads relevant SDTM domain files required for the generation
#' of NIF files from a folder. It assumes that DM, EX, PC and VS are available
#' at this location and will throw an error if not.
#'
#' @param data_path Path to the required SDTM files in .sas7bdat format.
#' @param ... Further optional
#' parameters may specify the individual SDTM domains to be loaded (lowercase,
#' no file extensions). If no further parameters are given, the standard set of
#' dm, ex, pc and vs are loaded.
#' @return A named list of the relevant SDTM domains as data.frames
#' @export
read_sdtm_sas <- function(data_path, ...) {
  parameters <- c(as.list(environment()), list(...))
  fs <- as.character(parameters[-1])
  if (length(fs) == 0) {
    fs <- c("dm", "ex", "pc", "vs")
  }
  out <- list()
  for (domain in fs) {
    out[[domain]] <- as.data.frame(haven::read_sas(file.path(
      data_path, paste0(domain, ".sas7bdat")
    )))
  }
  new_sdtm(out)
}


#' Load SDTM data from xpt files
#'
#' This function loads relevant SDTM domain files required for the generation
#' of NIF files from a folder. It assumes that DM, EX, PC and VS are available
#' at this location and will throw an error if not.
#'
#' @param data_path Path to the required SDTM files in .xpt format.
#' @param ... Further optional
#' parameters may specify the individual SDTM domains to be loaded (lowercase,
#' no file extensions). If no further parameters are given, the standard set of
#' dm, ex, pc and vs are loaded.
#' @return A named list of the relevant SDTM domains as data.frames
#' @export
read_sdtm_xpt <- function(data_path, ...) {
  parameters <- c(as.list(environment()), list(...))
  fs <- as.character(parameters[-1])
  if (length(fs) == 0) {
    fs <- c("dm", "ex", "pc", "vs")
  }
  out <- list()
  for (domain in fs) {
    out[[domain]] <- as.data.frame(haven::read_xpt(file.path(
      data_path, paste0(domain, ".xpt")
    )))
  }
  new_sdtm(out)
}
