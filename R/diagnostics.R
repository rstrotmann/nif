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
    nif <- nif %>%
      filter(ID==id)
    id_label <- ""
  }
  else if(id %in% nif$USUBJID) {
    nif <- nif %>%
      filter(USUBJID==id)
    id_label <- paste0(" (ID ", nif %>% distinct(ID) %>% pull(ID), ")")
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
    ggplot2::labs(title=paste0(plot.label, ": ", id, id_label),
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
#' @param analyte The analyte of interest.
#' @param max.time The right limit of the time scale
#'
#' @return the plot object
#' @import dplyr
#' @export
dose_plot_id <- function(nif, id, y.scale="lin", max.dose=NA, max.time=NA,
                         analyte=NULL){
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

  if(!is.null(analyte)) {
    nif <- nif %>%
      filter(ANALYTE %in% analyte)
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



#' Plot NIF data set
#'
#' This function plots a NIF data set, grouped by the variable `group`. If no
#'   grouping variable is provided, `DOSE` will be used.
#'
#' @param x The NIF object to be plotted.
#' @param y_scale Type of y-axis scale. Can be 'log' or 'lin'. Default is "lin".
#' @param max_x Maximal x (time) scale value.
#' @param analyte The analyte to be plotted If this argument is not supplied
#'   (or set to NULL), all analytes are shown.
#' @param mean Boolean value to indicate whether the mean value by dose and
#'   analyte is to be plotted. In that case, the nominal time (NTIME) is shown
#'   on the x axis. 'mean=T' will cast an error if multiple analytes are in the
#'   data set of have been defined with 'analyte'.
#' @param doses The doses to be plotted. Can be a scalar value or a numeric
#'   vector or NULL to plot all doses.
#' @param points Boolean value to define whether points should be plotted.
#' @param id Numerical scalar or vector of IDs to be plotted.
#' @param usubjid Character scalar or vector of USUBJIDs to be plotted.
#' @param group Character scalar to define a grouping variable. If specified,
#'   this will cast an error if multiple analytes are in the data set or have
#'   been defined by 'analyte'.
#' @param administrations Boolean value to indicate whether vertical lines
#'   marking the administration times should be plotted. Has no function if
#'   mean=F.
#' @param nominal_time Boolean to indicate whether NTIME rather than TIME should
#'   be plotted on the x-axis.
#' @param title The plot title as string.
#' @param ... Further arguments.
#' @param min_x Minimum x (time) scale value.
#' @param lines Boolean to define whether lines are to be drawn.
#' @param alpha Numerical value between 0 and 1, passed on as alpha to ggplot.
#'
#' @return The plot object
#' @seealso [nif_viewer()]
#' @examples
#' plot(examplinib_nif)
#' plot(examplinib_nif, analyte="RS2023", points=TRUE)
#' plot(examplinib_nif, analyte="RS2023", group="FASTED")
#' plot(examplinib_nif, max_x=24, point=TRUE)
#' plot(examplinib_nif, analyte="RS2023", group="FASTED", nominal_time=TRUE)
#' plot(examplinib_nif, analyte="RS2023", group="FASTED", mean=TRUE)
#' plot(examplinib_nif, analyte="RS2023", group="FASTED", mean=TRUE, max_x=24)
#'
#' @export
plot.nif <- function(x, y_scale="lin", min_x=0, max_x=NA, analyte=NULL,
                     mean=FALSE, doses=NULL, points=F, id=NULL, usubjid=NULL,
                     group=NULL, administrations=F, nominal_time=F, lines=T,
                     alpha=1, title=NULL, ...) {
  if(!is.null(id)) {
    x <- x %>%
      dplyr::filter(ID %in% id)
  }

  if(!is.null(usubjid)) {
    x <- x %>%
      dplyr::filter(USUBJID %in% usubjid)
  }

  if(!is.null(analyte)) {
    x <- x %>%
      dplyr::filter(ANALYTE==analyte)
  }

  if(!is.null(doses)){
    x <- x %>%
      dplyr::filter(DOSE %in% doses)
  }

  if(!is.null(group)){
    cov <- group
    if(is.null(analyte) | length(analyte) > 1){
      stop(paste0("Plotting multiple analytes in the same graph does not make ",
                  "sense. Consider selecting a (single) analyte!"))
    }
  } else {
    cov <- "ANALYTE"
  }

  if(mean==TRUE){
    if(!is.null(group) & is.null(analyte) & length(analytes(x)>1)){
      stop(paste0("Plotting means over multiple analytes does not make sense! ",
                  "Consider selecting a specific analyte"))
    }
    temp <- x %>%
      as.data.frame() %>%
      filter(NTIME > 0) %>%
      filter(EVID==0) %>%
      dplyr::filter(!is.na(DOSE)) %>%
      dplyr::group_by(NTIME, .data[[cov]], DOSE) %>%
      dplyr::summarize(mean=mean(DV, na.rm=TRUE), sd=sd(DV, na.rm=TRUE),
                       n=n(), .groups="drop") %>%
      mutate(max_y=mean+sd)

    p <- temp %>%
      ggplot2::ggplot(ggplot2::aes(
        x=NTIME, y=mean,
        group=as.factor(.data[[cov]]),
        color=as.factor(.data[[cov]]))) +
      ggplot2::geom_ribbon(aes(ymin=mean-sd, ymax=mean+sd,
                               fill=as.factor(.data[[cov]])),
                           alpha=0.3, color=NA, show.legend=F) +
      #ggplot2::geom_line() +
      ggplot2::facet_wrap(~DOSE) +
      ggplot2::theme_bw() +
      ylim(0, max(temp$max_y, na.rm=T)) +
      ggplot2::theme(legend.position="bottom")
  } else {
    # if mean == FALSE
    temp <- x %>%
      dplyr::filter(!is.na(DOSE)) %>%
      filter(EVID==0) %>%
      filter(!is.na(DV)) %>%
      as.data.frame()

    if(nominal_time){
      p <- temp %>%
        ggplot2::ggplot(ggplot2::aes(
          x=NTIME, y=DV,
          group=interaction(USUBJID, ANALYTE, as.factor(.data[[cov]])),
          color=as.factor(.data[[cov]])))
    } else {
      p <- temp %>%
        ggplot2::ggplot(ggplot2::aes(
          x=TIME, y=DV,
          group=interaction(USUBJID, ANALYTE, as.factor(.data[[cov]])),
          color=as.factor(.data[[cov]])))
    }

    if(administrations) {
      adm <- x %>%
        as.data.frame() %>%
        filter(EVID==1) %>%
        mutate(GROUP=as.factor(.data[[cov]])) %>%
        distinct(TIME, GROUP, DOSE)

      p <- p +
        geom_vline(aes(xintercept=TIME), data=adm, color="grey")
    }

    p <- p +
      #ggplot2::geom_line() +
      ggplot2::facet_wrap(~DOSE) +
      ggplot2::theme_bw() +
      ggplot2::theme(legend.position="bottom")
  }

  if(!is.null(title)) {
    p <- p + ggtitle(title)
  }

  p <- p + ggplot2::xlim(min_x, max_x)

  if(y_scale=="log"){
    p <- p + ggplot2::scale_y_log10()
  }

  if(points){
    p <- p + ggplot2::geom_point(alpha=alpha)
  }

  if(lines) {
    p <- p + ggplot2::geom_line(alpha=alpha)
  }

  if(length(analyte) == 1){
    p <- p + labs(y=analyte)
  }

  if(!is.null(group)) {
    p <- p + labs(color=group)
  } else {
    p <- p + labs(color="ANALYTE")
  }

  return(p)
}



#' NIF data set overview
#'
#' @param object The NIF data set.
#' @param ... Further arguments.
#'
#' @return A summary_nif object.
#' @export
summary.nif <- function(object, ...) {
  subjects <- subjects(object)
  analytes <- analytes(object)
  parents <- object %>%
    as.data.frame() %>%
    distinct(PARENT) %>%
    filter(PARENT != "") %>%
    pull(PARENT)
  dose_red_sbs <- lapply(parents,
                         function(x) {dose_red_sbs(object, analyte=x)})
  names(dose_red_sbs) <- parents
  observations <- object %>%
    as.data.frame() %>%
    filter(EVID==0) %>%
    group_by(ANALYTE) %>%
    summarize(N=n(), .groups="drop") %>%
    as.data.frame()

  n_studies <- object %>%
    as.data.frame() %>%
    filter(EVID==1) %>%
    group_by(STUDYID) %>%
    summarize(N=n_distinct(USUBJID))

  n_sex <- object %>%
    dplyr::distinct(USUBJID, SEX) %>%
    dplyr::group_by(SEX) %>%
    dplyr::summarize(n=n())

  n_males <- n_sex %>%
    dplyr::filter(SEX==0) %>%
    dplyr::pull(n)
  if(length(n_males)==0) {n_males=0}

  n_females <- n_sex %>%
    dplyr::filter(SEX==1) %>%
    dplyr::pull(n)
  if(length(n_females)==0) {n_females=0}

  out <- list(
    nif = object,
    studies = studies(object),
    subjects = subjects,
    dose_red_sbs=dose_red_sbs,
    n_studies = n_studies,
    n_subj = nrow(subjects),
    n_males = n_males,
    n_females = n_females,
    # n_obs = object %>%
    #   filter(EVID==0) %>%
    #   nrow(),
    n_obs = observations,
    analytes=analytes,
    n_analytes = length(analytes),
    drugs = parents,
    dose_levels = dose_levels(object,
                              grouping=any_of(c("PART", "COHORT", "GROUP"))),
    administration_duration = administration_summary(object)
  )
  class(out) <- "summary_nif"
  return(out)
}


#' Print NIF summary
#'
#' @param x A summary_nif object.
#' @param ... Further parameters.
#'
#' @return Nothing.
#' @export
print.summary_nif <- function(x, ...) {
  cat(paste("NONMEM input file (NIF) data set summary\n\n"))

  cat(paste("Data from", sum(x$n_studies$N), "subjects across", length(x$studies)), "studies:\n")
  #cat(paste0(paste(x$studies, collapse="\n"), "\n\n"))
  cat(paste0(df.to.string(x$n_studies), "\n\n"))

  cat(paste0("Males: ", x$n_males, ", females: ", x$n_females, " (",
             round(x$n_females/(x$n_males + x$n_females)*100, 1), "%)\n\n"))

  cat(paste0("Analytes:\n", paste(x$analytes, collapse=", "), "\n\n"))

  cat(paste(sum(x$n_obs$N), "observations:\n"))
  cat(paste0(df.to.string(x$n_obs), "\n\n"))


  cat(paste0("Administered drugs:\n", paste(x$drugs, collapse=", "), "\n\n"))

  cat("Dose levels:\n")
  cat(df.to.string(x$dose_levels))
  cat("\n\n")

  dr_summary <- lapply(x$dose_red_sbs, length) %>%
    data.frame()
  cat("Subjects with dose reductions\n")
  cat(df.to.string((dr_summary)))
  cat("\n\n")

  cat("Treatment duration overview:\n")
  cat(df.to.string(x$administration_duration))
  invisible(x)
}


#' Plot NIF summary
#'
#' @param x A NIF data set.
#' @param ... Further arguments.
#'
#' @return Nothing.
#' @export
plot.summary_nif <- function(x, ...) {
  nif <- x$nif

  plots <- list(
    age_hist(nif),
    weight_hist(nif)
  )
  invisible(
    capture.output(
      suppressWarnings(
        print(plots)
      )
    )
  )

  if("HEIGHT" %in% colnames(nif)){
    plots <- list(
      bmi_hist(nif),
      wt_by_ht(nif))
    invisible(
      capture.output(
        suppressWarnings(
          print(plots))
      )
    )
  }

  plots <- list(
    wt_by_sex(nif),
    wt_by_race(nif),
    lapply(x$analytes, function(a) {
      nif %>%
        plot(analyte=a,
             y_scale = "log",
             points=T,
             line=F,
             alpha=0.3,
             title=paste(a, "overview by dose"),
             max_x=max_observation_time(x$nif, a))}))

  invisible(
    capture.output(
      suppressWarnings(
        print(plots))
    )
  )
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
    labs(x="age (years)", y="number of subjects") +
    ggtitle("Age distribution")
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
    labs(x="body weight (kg)", y="number of subjects") +
    ggtitle("Body weight distribution")
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
    labs(x="BMI (kg/m^2)", y="number of subjects") +
    ggtitle("BMI distribution")
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
    theme_bw() +
    ggtitle("Body weight by sex")
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
    mutate(rc=as.factor(case_match(as.character(RACE),
                         "WHITE"~"White",
                         "BLACK OR AFRICAN AMERICAN"~"Black",
                         "ASIAN"~"Asian",
                         "NATIVE HAWAIIAN OR OTHER PACIFIC ISLANDER"~"Pacific",
                         "AMERICAN INDIAN OR ALASKA NATIVE"~"Native",
                         "OTHER"~"Other",
                         .default=RACE))) %>%
    distinct(ID, bl_wt, rc) %>%
    mutate(maxwt=max(bl_wt, na.rm=T)) %>%
    group_by(rc) %>%
    mutate(count = n()) %>%
    ggplot(aes(x=rc, y=bl_wt, group=rc)) +
    geom_boxplot(width=0.5) +
    geom_label(aes(label= paste0("N=", count), y=maxwt+5),
               label.size=0) +
    labs(x="", y="baseline weight (kg)") +
    theme_bw() +
    ggtitle("Body weight by race")
}

#' Weight by height scatterplot
#'
#' @param obj The NIF object.
#'
#' @return A plot object.
#' @export
wt_by_ht <- function(obj) {
  obj %>%
    as.data.frame() %>%
    distinct(ID, HEIGHT, WEIGHT) %>%
    ggplot(aes(x=HEIGHT, y=WEIGHT)) +
    geom_smooth(method="lm", formula = 'y ~ x', alpha=0.3) +
    geom_point(size=2) +
    labs(x="height (cm)", y="baseline weight (kg)") +
    ggtitle("Body weight by height") +
    theme_bw()
}



#' Overview on the number of administrations in the subjects by parent
#'
#' @param obj A NIF data set.
#'
#' @return A data frame.
#' @importFrom stats median
#' @export
#' @examples
#' administration_summary(examplinib_nif)
#'
administration_summary <- function(obj) {
  obj %>%
    n_administrations() %>%
    filter(PARENT!="") %>%
    group_by(across(any_of(c("PARENT")))) %>%
    summarize(min=min(N, na.rm=T), max=max(N, na.rm=T), mean=mean(N, na.rm=T),
              median=stats::median(N, na.rm=T)) %>%
    as.data.frame()
}


#' Mean dose plot
#'
#' This function plots the mean dose per day over time
#'
#' @param obj A NIF data set.
#' @param analyte The compound as character (i.e., the ANALYTE within the data
#'   set).
#'
#' @return A ggplot object.
#' @export
mean_dose_plot <- function(obj, analyte=NULL) {
  if(is.null(analyte)) {
    analyte <- guess_analyte(obj)
  }

  obj %>%
    as.data.frame() %>%
    mutate(DAY=floor(TIME/24)+1) %>%
    filter(EVID==1, ANALYTE==analyte) %>%
    group_by(DAY) %>%
    summarize("mean dose (mg)"=mean(DOSE, na.rm=T), "N"=n(), .groups="drop") %>%
    pivot_longer(cols=-DAY, names_to="PARAM", values_to="VAL") %>%
    ggplot(aes(x=DAY, y=VAL)) +
    geom_line() +
    facet_grid(PARAM ~ ., scales="free_y") +
    labs(x="time (days)", y="") +
    theme_bw() +
    ggtitle(paste0("Mean ", analyte, " dose over time"))
}


sbs_by_dl <- function(obj) {

}
