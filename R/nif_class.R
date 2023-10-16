#' nif class constructor
#'
#' @param obj A data frame containing the actual NIF data or a sdtm object.
#' @import dplyr
#' @return A nif object from the input data set.
#' @export
new_nif <- function(obj) {
  if(class(obj)[1] == "sdtm"){
    temp <- make_nif(obj)
  } else {
    temp <- obj
    class(temp) <- c("nif", "data.frame")
  }
  return(temp)
}


#' print() implementation for nif objects
#'
#' @param x A nif object.
#' @param ... Additional parameters
#'
#' @export
print.nif <- function(x, ...){
  cat(paste("NONMEM input file (NIF) data set with data from",
            length(studies(x)),
            "studies\n"))
  n.obs <- x %>%
    filter(EVID==0) %>%
    nrow()
  cat(paste(n.obs, "observations from",
            length(subjects(x)), "subjects\n"))

  n.sex <- x %>%
    dplyr::distinct(USUBJID, SEX) %>%
    dplyr::group_by(SEX) %>%
    dplyr::summarize(n=n())

  n_males <- n.sex %>%
    dplyr::filter(SEX==0) %>%
    dplyr::pull(n)
  if(length(n_males)==0) {n_males=0}

  n_females <- n.sex %>%
    dplyr::filter(SEX==1) %>%
    dplyr::pull(n)
  if(length(n_females)==0) {n_females=0}

  cat(paste0("Males: ", n_males, ", females: ", n_females, " (",
             round(n_females/(n_males + n_females)*100, 1), "%)\n\n"))

  cat(paste0("Studies:\n", paste(studies(x), collapse="\n"), "\n\n"))

  cat("Dose levels:\n")
  cat(df.to.string(dose_levels(x, grouping=any_of(c("PART", "COHORT", "GROUP")))))
  cat("\n\n")

  cat("Columns:\n")
  cat(paste(names(x), collapse=", "), "\n")

  temp <- x[1:15, names(x) %in% c("ID", "NTIME", "TIME", "ANALYTE", "EVID",
                                   "CMT", "AMT", "DOSE", "DV")] %>%
    as.data.frame()

  temp <- temp %>%
    df.to.string()
  cat(paste0("\nFirst rows of NIF data (selected columns):\n", temp))
  invisible(x)
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
    n_subj = length(subjects),
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

  invisible(capture.output(
    suppressWarnings(
      print(list(
        age_hist(nif),
        weight_hist(nif))))))

  if("HEIGHT" %in% colnames(x$nif)){
    invisible(capture.output(suppressWarnings(print(
      bmi_hist(nif)))))
  }

  plots <- list(
    wt_by_sex(nif),
    wt_by_race(nif),
    lapply(x$analytes, function(a) {
      nif %>%
        plot(analyte=a,
             y_scale = "log",
             points=F,
             title=paste(a, "overview by dose"),
             max_x=max_observation_time(x$nif, a))}))

  invisible(
    capture.output(
      suppressWarnings(
        print(plots))
      )
    )
}


#' Subjects within a nif object
#'
#' @param obj A nif object
#' @import dplyr
#' @return A character vector of all USUBJIDs in the data set.
#' @export
subjects <- function(obj) {
  obj %>%
    dplyr::distinct(USUBJID) %>%
    dplyr::pull(USUBJID)
}


#' Subjects with dose reduction
#'
#' @param obj A NIF data set object.
#' @param analyte The analyte of interest as string.
#'
#' @return The USUBJIDs as string
#' @export
dose_red_sbs <- function(obj, analyte="") {
  if(analyte!="") {
    obj <- obj %>%  filter(ANALYTE %in% analyte)
  }

  obj %>%
    as.data.frame() %>%
    index_nif() %>%
    filter(EVID==1) %>%
    group_by(ID, ANALYTE) %>%
    mutate(initial_dose=DOSE[row_number()==1]) %>%
    filter(DOSE < initial_dose & DOSE != 0) %>%
    ungroup() %>%
    distinct(USUBJID) %>%
    pull(USUBJID)
}

#' Studies within a nif object
#'
#' @param obj A nif object
#' @import dplyr
#' @return A character vector of all STUDYIDs in the data set.
#' @export
studies <- function(obj) {
  obj %>%
    dplyr::distinct(STUDYID) %>%
    dplyr::pull(STUDYID)
}


#' Doses within a nif object
#'
#' @param obj A nif object
#' @import dplyr
#' @return A number vector of all doses (AMT) in the data set.
#' @export
doses <- function(obj){
  obj %>%
    dplyr::filter(AMT!=0) %>%
    dplyr::distinct(AMT) %>%
    dplyr::arrange(as.numeric(AMT)) %>%
    dplyr::pull(AMT)
}


#' Dose levels
#'
#' In studies with multiple administrations, dose modifications may occur. The
#' `dose_levels()` function only summarizes the initial (i.e., the first)
#' administrations.
#'
#' @param obj A nif object.
#' @param grouping Further fields to be included (and to be grouped by) in the
#'   output.
#'
#' @return A data frame
#' @export
#'
#' @examples
#' dose_levels(examplinib_nif)
#' dose_levels(examplinib_nif, grouping="SEX")
#' dose_levels(examplinib_nif, grouping=c("SEX", "FASTED"))
dose_levels <- function(obj, grouping=NULL) {
  obj %>%
    filter(METABOLITE==FALSE) %>%
    filter(PARENT != "", !is.na(DOSE), AMT != 0) %>%
    group_by(ID, ANALYTE, across(any_of(grouping))) %>%
    arrange(ID, TIME) %>%
    filter(TIME==min(TIME)) %>%
    select(ID, ANALYTE, DOSE, any_of(grouping)) %>%
    pivot_wider(names_from="ANALYTE", values_from="DOSE", values_fill = 0) %>%

    group_by(across(c(-ID))) %>%
    summarize(N=n()) %>%
    as.data.frame()
}


#' Analytes within a nif object
#'
#' @param obj A nif object
#' @import dplyr
#' @return A character vector of all analytes in the data set.
#' @export
analytes <- function(obj){
  obj %>%
    dplyr::distinct(ANALYTE) %>%
    dplyr::pull(ANALYTE)
}


#' Implementation of the head function
#'
#' @param n Number of rows to return.
#' @param obj A nif object
#'
#' @import dplyr
#' @return None
#' @import utils
#' @export
head <- function(obj, n=6) {
  obj %>%
    as.data.frame() %>%
    utils::head(n=n)
}


#' Export a nif object as csv file
#'
#' @param obj A nif object.
#' @param filename The filename for the exported file.
#' @import dplyr
#' @export
write_csv.nif <- function(obj, filename){
  temp <- obj %>%
    dplyr::mutate(across(c("TIME", "LNDV"), round, 3))
  write.csv(temp, filename, quote=F, row.names=F)
}


#' Standard nif fields
#'
#' @return A character vector of the standard NIF fields
#' @export
standard_nif_fields <- c("REF", "STUDYID", "ID", "USUBJID", "NTIME", "TIME",
                         "ANALYTE", "AMT", "RATE", "DV", "LNDV", "CMT", "EVID",
                         "DOSE", "AGE", "SEX", "RACE", "HEIGHT", "WEIGHT",
                         "ACTARMCD", "ANALYTE", "PARENT", "METABOLITE")



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
                     group=NULL, administrations=F, nominal_time=F,
                     title=NULL, ...) {
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
      ggplot2::geom_line() +
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
    ggplot2::geom_line() +
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
    p <- p + ggplot2::geom_point()
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


#' Index dosing invervals
#'
#' This function adds a column `DI` that indicates the dosing interval. All
#' baseline observations before the first dosing interval get assigned to the
#' first dosing interval, too.
#' In addition to `DI`, the function also calls `index_nif()`, thus creating
#' the field `REF` as a side effect.
#'
#' @param obj The NIF data set.
#'
#' @return A new NIF data set.
#' @export
index_dosing_interval <- function(obj){
  obj <- obj %>%
    index_nif() %>%
    select(-any_of("DI"))

  di <- obj %>%
    as.data.frame() %>%
    filter(EVID==1) %>%
    group_by(ID, ANALYTE) %>%
    arrange(TIME) %>%
    mutate(DI=row_number()) %>%
    ungroup() %>%
    select(REF, DI) %>%
    as.data.frame()

  obj %>%
    as.data.frame() %>%
    left_join(di, by="REF") %>%
    group_by(ID, ANALYTE) %>%
    arrange(REF) %>%
    fill(DI, .direction="down") %>%
    # all baseline before the first administration gets assigned to the first
    #   dosing interval, too:
    fill(DI, .direction="up") %>%
    ungroup() %>%
    new_nif()
}


#' Number of observations per dosing interval
#'
#' This function returns the number of observations by subject and analyte per
#' dosing interval.
#'
#' @param obj The NIF data set.
#' @param analyte The analyte to filter for as character. Optional.
#'
#' @return A data frame.
#' @export
#' @examples
#' n_observations_per_dosing_interval(examplinib_nif)
#'
n_observations_per_dosing_interval <- function(obj, analyte=NULL) {
  if(!is.null(analyte)) {
    obj <- obj %>%
      filter(ANALYTE %in% analyte)
  }

  obj %>%
    index_dosing_interval() %>%
    group_by(across(any_of(c("ID", "USUBJID", "ANALYTE", "PARENT", "DI")))) %>%
    summarize(N=sum(EVID==0), .groups="drop") %>%
    as.data.frame()
}


#' Number of administrations per subject
#'
#' @param obj A NIF data set.
#'
#' @return A data frame.
#' @export
#' @examples
#' n_administrations(examplinib_nif)
#'
n_administrations <- function(obj) {
  obj %>%
    index_dosing_interval() %>%
    group_by(across(any_of(c("ID", "USUBJID", "ANALYTE", "PARENT")))) %>%
    summarize(N=max(DI), .groups="drop") %>%
    as.data.frame()
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


#' Maximal administration time
#'
#' This function returns the time in hours of the last administration within
#' the data set.
#'
#' @param obj The NIF data set
#' @param analytes The analyte or analytes to filter for.
#'
#' @return A scalar representing the time in hours.
#' @export
#' @examples
#' max_admin_time(examplinib_nif)
#'
max_admin_time <- function(obj, analytes=NULL) {
  if(!is.null(analytes)) {
    obj <- obj %>%
      filter(ANALYTE %in% analytes)
  }

  obj %>%
    as.data.frame() %>%
    filter(EVID==1) %>%
    pull(TIME) %>%
    max()
}

#' Maximal ovservation time
#'
#' This function returns the time in hours of the last observation within the
#' data set.
#'
#' @param obj The NIF data set
#' @param analytes The analyte or analytes to filter for.
#'
#' @return A scalar representing the time in hours.
#' @export
#' @examples
#' max_observation_time(examplinib_nif)
#'
max_observation_time <- function(obj, analytes=NULL) {
  if(!is.null(analytes)) {
    obj <- obj %>%
      filter(ANALYTE %in% analytes)
  }

  obj %>%
    as.data.frame() %>%
    filter(EVID==0) %>%
    pull(TIME) %>%
    max(na.rm=TRUE)
}

#' Guess the most likely meant analyte
#'
#' @param obj A NIF data set
#'
#' @return The ANALYTE as character
guess_analyte <- function(obj) {
  temp <- obj %>%
    as.data.frame() %>%
    filter(EVID==0) %>%
    group_by(ANALYTE, METABOLITE) %>%
    summarize(n=n(), .groups="drop") %>%
    arrange(METABOLITE, -n)
  (temp %>% pull(ANALYTE))[1]
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

