#' Write NONMEM input formatted nif object to file
#'
#' Write as space-delimited, fixed-width or a character-separated file. If the
#' 'numeric_fields_only' argument is TRUE, only columns that are numeric will be
#' exported.
#'
#' All numeric fields are reduced to 4 decimal places. For this, IEC 60559
#' is applied, i.e., rounding for a last digit of 5 is to the next even number.
#' All fields are converted to character, and NA-values are converted to '.'.
#'
#' Further, in the columns specified by the 'dot_columns' argument, zero values
#' are also converted to '.'.
#'
#' @param obj The NIF object.
#' @param fields The fields to export in addition to the minimally required
#' fields. If NULL (default), all fields will be exported.
#' @param filename The filename as string. If not filename is specified, the
#' file is printed only.
#' @param sep The separating character, e.g. ',' or ';'. If NULL (default), the
#' output has a space-separated, fixed-width format.
#' @param numeric_fields_only Include only numerical fields.
#' @param dot_columns Fields in which zeros will be replaced with ".".
#' @return Nothing.
#' @importFrom gdata write.fwf
#' @export
#'
#' @examples
#' write_nonmem(examplinib_fe_nif)
#' write_nonmem(examplinib_fe_nif, numeric_fields_only = TRUE)
#' write_nonmem(examplinib_fe_nif, dot_columns = c("DV", "AMT"))
write_nonmem <- function(
  obj,
  filename = NULL,
  fields = NULL,
  sep = NULL,
  numeric_fields_only = FALSE,
  dot_columns = NULL
) {
  # input validation
  validate_argument(
    filename, "character", allow_empty = TRUE, allow_null = TRUE)
  validate_argument(
    fields, "character", allow_null = TRUE, allow_multiple = TRUE)
  validate_argument(sep, "character", allow_null = TRUE)
  validate_argument(numeric_fields_only, "logical")
  validate_argument(
    dot_columns, "character", allow_null = TRUE, allow_multiple = TRUE)
  validate_nif(obj, c(fields, dot_columns))

  # business logic
  temp <- as.data.frame(obj)

  if (!is.null(fields)) {
    temp <- select(temp, all_of(c(minimal_nif_fields, fields)))
  }

  if (isTRUE(numeric_fields_only))
    temp <- select(temp, where(is.numeric))

  temp <- temp |>
    mutate(
      across(where(is.numeric), \(x) round(x, digits = 4)),
      across(everything(), as.character),
      across(everything(), \(x) if_else(is.na(x), ".", x))
    ) |>
    mutate(across(.cols = all_of(dot_columns), .fns = function(x) {
      case_when(
        x == 0 ~ ".",
        .default = as.character(x)
      )
    }))

  if (is.null(filename)) filename <- ""

  if (is.null(sep)) {
    temp <- rbind(colnames(temp), temp)
    write.fwf(temp, file = filename, colnames = FALSE)
  } else {
    write.table(
      temp,
      file = filename, row.names = FALSE,
      sep = sep, dec = ".", quote = FALSE
    )
  }
}


# #' Write as comma-separated file, complying with the format used by Monolix
# #'
# #' @param obj The NIF object.
# #' @param filename The filename as string. If not filename is specified, the
# #' file is printed only.
# #' @param fields The fields to export. If NULL (default), all fields will be
# #' exported.
# #' @return Nothing.
# #' @export
# #' @examples
# #' head(write_monolix(examplinib_fe_nif))
# write_monolix <- function(obj, filename = NULL, fields = NULL) {
#   double_fields <- c(
#     "NTIME", "TIME", "TAD", "AMT", "RATE", "DV", "LNDV", "DOSE",
#     "AGE", "HEIGHT", "WEIGHT", "BMI"
#   )
#   bl_fields <- names(obj)[str_detect(names(obj), "^BL_")]
#   int_fields <- c("REF", "ID", "MDV", "CMT", "EVID", "SEX", "TRTDY")
#   num_fields <- c(double_fields, int_fields, bl_fields)
#
#   if (is.null(fields)) {
#     fields <- names(obj)
#   }
#
#   temp <- obj |>
#     as.data.frame() |>
#     mutate(across(any_of(num_fields), \(x) signif(x, 4))) |>
#     mutate(ADM = case_when(.data$AMT != 0 ~ "1", .default = ".")) |>
#     mutate(YTYPE = case_when(.data$ADM == "1" ~ ".",
#       .default = as.character(.data$CMT - 1)
#     ))
#
#   if ("METABOLITE" %in% names(obj)) {
#     temp <- temp |>
#       mutate(METABOLITE = case_when(
#         is.na(.data$METABOLITE) ~ FALSE,
#         .default = .data$METABOLITE
#       ))
#   }
#
#   temp <- temp |>
#     mutate(across(
#       any_of(num_fields),
#       function(x) {
#         case_when(
#           is.na(x) ~ ".",
#           .default = as.character(x)
#         )
#       }
#     )) |>
#     mutate_all(as.character) |>
#     mutate(Y = .data$DV)
#
#   if (is.null(filename)) {
#     print(temp, row.names = FALSE, col.names = FALSE)
#   } else {
#     write.table(temp,
#       file = filename, row.names = FALSE,
#       sep = ",", dec = ".", quote = FALSE
#     )
#   }
# }
