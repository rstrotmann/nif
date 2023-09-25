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
    # select(ID, ANALYTE, DOSE, any_of(c("PART", "COHORT", "GROUP"))) %>%
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
    # dplyr::filter(AMT!=0) %>%
    dplyr::distinct(ANALYTE) %>%
    # dplyr::arrange(as.numeric(AMT)) %>%
    dplyr::pull(ANALYTE)
}


#' Implementation of the head function
#'
#' @param obj A nif object
#' @param ... Further parameters
#'
#' @import dplyr
#' @return None
#' @import utils
#' @export
head <- function(obj, ...) {
  obj %>%
    as.data.frame() %>%
    utils::head()
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
#' @param ... Further arguments.
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
plot.nif <- function(x, y_scale="lin", max_x=NULL, analyte=NULL, mean=FALSE,
                     doses=NULL, points=F, id=NULL, usubjid=NULL,
                     group=NULL, administrations=F, nominal_time=F, ...) {
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
    temp <- x %>%
      dplyr::filter(!is.na(DOSE))

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
      p <- p +
      ggplot2::geom_line() +
      ggplot2::facet_wrap(~DOSE) +
      ggplot2::theme_bw() +
      ggplot2::theme(legend.position="bottom")
    if(administrations) {
      adm <- x %>%
        as.data.frame() %>%
        filter(EVID==1) %>%
        mutate(GROUP=as.factor(.data[[cov]])) %>%
        distinct(TIME, GROUP)
      p <- p +
        geom_vline(aes(xintercept=TIME), data=adm, color="grey")
    }
  }

  if(!is.null(max_x)) {
    p <- p + ggplot2::xlim(0, max_x)
  }

  if(y_scale=="log"){
    p <- p + ggplot2::scale_y_log10()
  }

  if(points){
    p <- p + ggplot2::geom_point()
  }

  # if(!is.null(analyte) & length(analyte) == 1){
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



