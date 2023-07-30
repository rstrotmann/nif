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
    ggplot2::theme(legend.position="bottom")

  if(y.scale=="log") {
    p <- p + scale_y_log10()
  } else {
    p <- p + scale_y_continuous(limits = c(0, NA))
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
    ggplot2::theme_bw() +
    ggplot2::theme(legend.position="bottom")

  if(y.scale=="log") {
    p <- p + scale_y_log10()
  }
  return(p)
}

#' Age histogram
#'
#' @param obj The NIF file object.
#'
#' @return A plot object.
#' @export
age_hist <- function(obj) {
  obj %>%
    distinct(ID, AGE) %>%
    ggplot(aes(x=AGE)) +
    geom_histogram(binwidth=5, fill= "grey") +
    theme_bw() +
    labs(x="age (years)", y="number of subjects")
}

#' Weight histogram
#'
#' @param obj The NIF file object.
#'
#' @return A plot object.
#' @export
weight_hist <- function(obj) {
  obj %>%
    as.data.frame() %>%
    distinct(ID, WEIGHT) %>%
    ggplot(aes(x=WEIGHT)) +
    geom_histogram(binwidth=5, fill= "grey") +
    theme_bw() +
    labs(x="body weight (kg)", y="number of subjects")
}

#' BMI histogram
#'
#' @param obj The NIF file object.
#'
#' @return A plot object.
#' @export
bmi_hist <- function(obj) {
  obj %>%
    as.data.frame() %>%
    distinct(ID, WEIGHT, HEIGHT) %>%
    mutate(BMI=WEIGHT/(HEIGHT/100)^2) %>%
    ggplot(aes(x=BMI)) +
    geom_histogram(binwidth=1, fill= "grey") +
    geom_vline(xintercept=c(18.5, 24.9, 30)) +
    theme_bw() +
    labs(x="BMI (kg/m^2)", y="number of subjects")
}

#' Weight by sex diagram
#'
#' @param obj The NIF file object.
#'
#' @return A plot object.
#' @export
wt_by_sex <- function(obj) {
  obj %>%
    as.data.frame() %>%
    group_by(ID) %>%
    mutate(bl_wt=mean(WEIGHT[TIME==0])) %>%
    ungroup() %>%
    distinct(ID, SEX, bl_wt) %>%
    group_by(SEX) %>%
    mutate(count = n(), maxwt=max(bl_wt)) %>%
    ggplot(aes(x=SEX, y=bl_wt, group=SEX)) +
    scale_x_continuous(breaks=c(0, 1)) +
    geom_boxplot(width=0.5) +
    # geom_dotplot(binaxis='y', stackdir='center', alpha=0.3,
    #              color="transparent", dotsize=1.2) +
    geom_label(aes(label= paste0("N=", count) , y = maxwt+5),
               label.size=0, position=position_dodge(width = 0.75)) +
    labs(x="sex", y="baseline weight (kg)") +
    theme_bw()
}

#' Weight by race diagram
#'
#' @param obj The NIF file object.
#'
#' @return A plot object.
#' @export
wt_by_race <- function(obj) {
  obj %>%
    as.data.frame() %>%
    group_by(ID) %>%
    mutate(bl_wt=mean(WEIGHT[TIME==0])) %>%
    ungroup() %>%
    mutate(rc=as.factor(RACE)) %>%
    distinct(ID, bl_wt, rc) %>%
    group_by(rc) %>%
    mutate(count = n(), maxwt=max(bl_wt)) %>%
    ggplot(aes(x=rc, y=bl_wt, group=rc)) +
    geom_boxplot(width=0.5) +
    # geom_dotplot(binaxis='y', stackdir='center', alpha=0.3,
    #              color="transparent", dotsize=1.2) +
    geom_label(aes(label= paste0("N=", count) , y = maxwt+5),
               label.size=0, position=position_dodge(width = 0.75)) +
    labs(x="race", y="baseline weight (kg)") +
    theme_bw()
}


