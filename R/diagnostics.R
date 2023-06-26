#' Plot DV time course data from individual subject
#'
#' This function plots DV over TIME for an individual subject, id. Id can be
#'   either the ID or the USUBJID. Administration time points are indicated with
#'   vertical lines.
#'
#' @param nif The NIF data set
#' @param id The subject ID to be plotted
#' @param y.scale Y-scale. Use 'scale="log"' for a logarithmic y scale. Default
#'   is "lin".
#' @param max.time The right limit of the time scale
#' @return the plot object
#' @import dplyr
#' @import ggplot2
#' @export
nif_plot_id <- function(nif, id, y.scale="lin", max.time=NA){
  if(id %in% nif$ID) {
    plot.label <- "ID"
    nif <- nif %>% filter(ID==id)
  }
  else if(id %in% nif$USUBJID) {
    nif <- nif %>% filter(USUBJID==id)
    plot.label <- "USUBJID"
  }
  else {
    stop(paste(id, "is not an ID or USUBJID contained in the NIF data set"))
  }

  admin <- nif %>%
    dplyr::filter(EVID==1)

  p <- nif %>%
    filter(EVID!=1) %>%
    ggplot2::ggplot(ggplot2::aes(x=TIME, y=DV,
                                 group=interaction(ID, as.factor(ANALYTE)),
                                 color=as.factor(ANALYTE))) +
    ggplot2::geom_vline(data=admin, ggplot2::aes(xintercept=TIME), color="gray") +
    ggplot2::geom_line() +
    ggplot2::geom_point() +
    ggplot2::xlim(0, max.time) +
    ggplot2::labs(title=paste0(plot.label, ": ", id),
      caption="vertical lines indicate administrations",
      color="analyte") +
    ggplot2::theme_bw() +
    theme(legend.position="bottom")

  if(y.scale=="log") {
    p <- p + scale_y_log10()
  }
  return(p)
}


#' Plot dose time course data from individual subject
#'
#' This function plots AMT over TIME for an individual subject, id. Id can be
#'   either the ID or the USUBJID. Administration time points are indicated with
#'   vertical lines.
#'
#' @param nif The NIF data set.
#' @param id The subject ID to be plotted
#' @param y.scale Y-scale. Use 'scale="log"' for a logarithmic y scale. Default
#'   is "lin".
#' @param max.dose The upper limit of the dose scale
#' @param max.time The right limit of the time scale
#' @return the plot object
#' @import dplyr
#' @export
dose_plot_id <- function(nif, id, y.scale="lin", max.dose=100, max.time=NA){
  if(id %in% nif$ID) {
    plot.label <- "ID"
    nif <- nif %>% filter(ID==id)
  }
  else if(id %in% nif$USUBJID) {
    nif <- nif %>% filter(USUBJID==id)
    plot.label <- "USUBJID"
  }
  else {
    stop(paste(id, "is not an ID or USUBJID contained in the NIF data set"))
  }

  admin <- nif %>%
    dplyr::filter(EVID==1)

  p <- admin %>%
    ggplot2::ggplot(ggplot2::aes(x=TIME, y=AMT,
                                 group=interaction(ID, ANALYTE),
                                 color=ANALYTE)) +
    ggplot2::geom_line() +
    ggplot2::geom_point() +
    ggplot2::ylim(0, max.dose) +
    ggplot2::xlim(0, max.time) +
    ggplot2::labs(title=paste0(plot.label, ": ", id), color="treatment") +
    theme_bw() +
    theme(legend.position="bottom")

  if(y.scale=="log") {
    p <- p + scale_y_log10()
  }
  return(p)
}


## to do
# ex_info: missing start, end times, other problems...




