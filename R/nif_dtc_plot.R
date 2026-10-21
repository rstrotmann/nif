#' Nif event timing chart
#'
#' Plot absolute times of administrations and observations.
#'
#' @description
#' `r lifecycle::badge("experimental")`
#'
#' @param x A nif object.
#' @param id The IDs to plot.
#' @param analyte The analytes to plot.
#' @param ... Further parameters.
#'
#' @returns A ggplot object.
#' @export
timing_chart <- function(
    x,
    id = NULL,
    analyte = NULL,
    ...
) {
  # input validation
  validate_nif_argument(x)


  if (is.null(id)) {
    id <- subjects(x)$ID
  }

  if (is.null(analyte)) {
    analyte <- analytes(x)
  }

  if (length(id) > 1) {
    color <- "ID"
  } else {
    color <- "EVID"
  }

  analyte_coding <- x |>
    distinct(ANALYTE, EVID) |>
    arrange(-EVID, ANALYTE) |>
    mutate(ANALYTE_ID = row_number()) |>
    mutate(LABEL = paste0(ANALYTE, ifelse(EVID == 1, "_ADMIN", "_OBS")))

  x |>
    filter(ID %in% id) |>
    left_join(analyte_coding, by = c("EVID", "ANALYTE")) |>
    ggplot(aes(x = DTC, y = as.factor(ANALYTE_ID))) +
    geom_point(aes(color = as.factor(.data[[color]]))) +
    scale_y_discrete(breaks = analyte_coding$ANALYTE_ID, labels = analyte_coding$LABEL) +
    labs(y = NULL, color = color) +
    theme_bw()
}
