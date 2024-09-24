#' Load SDTM data from sas7bdat files
#'
#' This function loads relevant SDTM domain files required for the generation
#' of NIF files from a folder. It assumes that DM, EX, PC and VS are available
#' at this location and will throw an error if not.
#' @description
#' `r lifecycle::badge("deprecated")`
#'
#' `read_sdtm_sas()` has been deprecated in favor of [`read_sdtm()`].
#'
#' @param data_path Path to the required SDTM files in .sas7bdat format.
#' @param ... Further optional
#' parameters may specify the individual SDTM domains to be loaded (lowercase,
#' no file extensions). If no further parameters are given, the standard set of
#' dm, ex, pc and vs are loaded.
#' @return A named list of the relevant SDTM domains as data.frames
#' @seealso [read_sdtm()]
#' @keywords internal
read_sdtm_sas <- function(data_path, ...) {
  lifecycle::deprecate_warn("0.42.14", "read_sdtm_sas()", "read_sdtm()")
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


#' Read SDTM data from xpt files
#'
#' This function loads relevant SDTM domain files required for the generation of
#' NIF files from a folder. It assumes that DM, EX, PC and VS are available at
#' this location and will throw an error if not.
#' @description
#' `r lifecycle::badge("deprecated")`
#'
#' `read_sdtm_xpt()` has been deprecated in favor of [`read_sdtm()`].
#' @param data_path Path to the required SDTM files in .xpt format.
#' @param ... Further optional parameters may specify the individual SDTM
#'   domains to be loaded (lowercase, no file extensions). If no further
#'   parameters are given, the standard set of dm, ex, pc and vs are loaded.
#' @return A named list of the relevant SDTM domains as data.frames
#' @keywords internal
read_sdtm_xpt <- function(data_path, ...) {
  lifecycle::deprecate_warn("0.42.14", "read_sdtm_xpt()", "read_sdtm()")
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



#' Read SDTM data
#'
#' This function reads SDTM-formatted data as SAS or XPT files from a folder
#' location.
#'
#' @param data_path The file system path to the source folder as character.
#' @param domain The domain name(s) as character, defaults to
#'   `c("dm", "vs", "ex", "pc")`.
#' @param format The format of the source files as character, either 'sas'
#'   (default), 'xpt', 'xlsx', or 'csv'.
#' @param ... Further parameters, refer to readr::read_csv and
#'   readxl::read_xlsx.
#' @param delim Deliminator.
#' @return A `sdtm` object.
#' @export
read_sdtm <- function(data_path,
                      domain=c("dm", "vs", "ex", "pc"),
                      format="sas", delim = ",", ...) {
  out <- list()
  if(format == "sas"){
    for (x in domain) {
      out[[x]] <- as.data.frame(haven::read_sas(
        file.path(data_path, paste0(x, ".sas7bdat"))))
    }
  }
  if(format == "xpt") {
    for (x in domain) {
      out[[x]] <- as.data.frame(haven::read_xpt(
        file.path(data_path, paste0(x, ".xpt"))))
    }
  }
  if(format == "csv") {
    for (x in domain) {
      out[[x]] <- as.data.frame(
        readr::read_delim(
          file.path(data_path, paste0(x, ".csv"))), ...)
    }
  }
  if(format == "xlsx") {
    for (x in domain) {
      out[[x]] <- as.data.frame(
        readxl::read_xlsx(
          file.path(data_path, paste0(x, ".xlsx"))), ...)
    }
  }
  new_sdtm(out)
}
