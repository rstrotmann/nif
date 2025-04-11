
#' Write as space-delimited, fixed-width file as required by NONMEM or a
#' character-separated file
#'
#' All numeric fields are reduced to 4 significant places. For this, IEC 60559
#' is applied, i.e., rounding for a last digit of 5 is to the next even number.
#' All fields are converted to character, and NA-values are converted to '.'.
#' @param obj The NIF object.
#' @param fields The fields to export. If NULL (default), all fields will be
#' exported.
#' @param filename The filename as string. If not filename is specified, the
#' file is printed only.
#' @param sep The separating character, e.g. ',' or ';'. If NULL (default), the
#' output has a space-separated, fixed-width format.
#' @importFrom gdata write.fwf
#' @export
#' @examples
#' head(write_nif(examplinib_fe_nif), 5)
write_nif <- function(obj, filename = NULL, fields = NULL, sep = NULL) {
  temp <- obj %>%
    as.data.frame() %>%
    mutate_if(is.numeric, round, digits = 4) %>%
    mutate_all(as.character()) %>%
    mutate_all(function(x) {
      case_when(is.na(x) ~ ".", .default = as.character(x))
    })

  if (is.null(filename)) {
    print(temp, row.names = FALSE, col.names = FALSE)
  } else {
    if (is.null(sep)) {
      temp <- rbind(colnames(temp), temp)
      write.fwf(temp, file = filename, colnames = FALSE)
    } else {
      write.table(temp,
                  file = filename, row.names = FALSE,
                  sep = sep, dec = ".", quote = FALSE
      )
    }
  }
}


#' Write as comma-separated file, complying with the format used by Monolix
#'
#' @param obj The NIF object.
#' @param filename The filename as string. If not filename is specified, the
#' file is printed only.
#' @param fields The fields to export. If NULL (default), all fields will be
#' exported.
#' @return Nothing.
#' @export
#' @examples
#' write_monolix(examplinib_fe_nif)
write_monolix <- function(obj, filename = NULL, fields = NULL) {
  double_fields <- c(
    "NTIME", "TIME", "TAD", "AMT", "RATE", "DV", "LNDV", "DOSE",
    "AGE", "HEIGHT", "WEIGHT", "BMI"
  )
  bl_fields <- names(obj)[str_detect(names(obj), "^BL_")]
  int_fields <- c("REF", "ID", "MDV", "CMT", "EVID", "SEX", "TRTDY")
  num_fields <- c(double_fields, int_fields, bl_fields)

  if (is.null(fields)) {
    fields <- names(obj)
  }

  temp <- obj %>%
    as.data.frame() %>%
    # mutate(across(any_of(num_fields), signif, 4)) %>%
    mutate(across(any_of(num_fields), \(x) signif(x, 4))) %>%
    mutate(ADM = case_when(.data$AMT != 0 ~ "1", .default = ".")) %>%
    mutate(YTYPE = case_when(.data$ADM == "1" ~ ".",
                             .default = as.character(CMT - 1)
    ))

  if ("METABOLITE" %in% names(obj)) {
    temp <- temp %>%
      mutate(METABOLITE = case_when(
        is.na(.data$METABOLITE) ~ FALSE,
        .default = .data$METABOLITE
      ))
  }

  temp <- temp %>%
    mutate(across(
      any_of(num_fields),
      function(x) {
        case_when(
          is.na(x) ~ ".",
          .default = as.character(x)
        )
      }
    )) %>%
    mutate_all(as.character) %>%
    mutate(Y = .data$DV)

  if (is.null(filename)) {
    print(temp, row.names = FALSE, col.names = FALSE)
  } else {
    write.table(temp,
                file = filename, row.names = FALSE,
                sep = ",", dec = ".", quote = FALSE
    )
  }
}
