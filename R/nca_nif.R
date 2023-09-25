#' Non-compartmental analysis
#'
#' @param obj The source NIF object.
#' @param group The grouping variable. If none specified, defaults to SEX.
#' @param analyte The analyte. If none specified and multiple analytes are in
#'   the dataset, defaults to the first analyte.
#' @return A PKNCA results object
#' @import dplyr
#' @import PKNCA
#' @export
nca <- function(obj, group="SEX", analyte=NULL){
  UseMethod("nca")
}



#' Non-compartmental analysis
#'
#' Note: This function is currently in expermental stage. It currently only
#'   supports single-dose NCA, and there must be a grouping variable!
#'
#' @param obj The source NIF object.
#' @param group The grouping variable. If none specified, defaults to SEX.
#' @param analyte The analyte. If none specified and multiple analytes are in
#'   the dataset, defaults to the first analyte.
#' @return A PKNCA results object
#' @import dplyr
#' @import PKNCA
#' @export
nca.nif <- function(obj, group="SEX", analyte=NULL){
  if(length(group) != 1) stop("group must be a scalar!")

  if(is.null(analyte)){
    analytes <- obj %>%
      as.data.frame() %>%
      dplyr::distinct(ANALYTE) %>%
      dplyr::pull(ANALYTE)
    current_analyte <- analytes[1]
    if(length(analytes)>1){
      message(paste0("No analyte specified, selected ",
                     current_analyte, " as the most likely!"))
    }
  } else {
    current_analyte <- analyte
  }

  # temp <- obj %>%
  #   filter(EVID==1) %>%
  #   group_by(ID, TIME) %>%
  #   arrange(TIME) %>%
  #   mutate(first_admin_time=min(TIME)) %>%
  #   select(ID, first_admin_time) %>%
  #   as.data.frame()

  obj <- obj %>%
    dplyr::filter(ANALYTE==current_analyte) %>%
    dplyr::mutate(GROUPING=as.factor(.data[[group]])) %>%
    dplyr::mutate(DV=case_when(is.na(DV) ~ 0, .default=DV)) %>%
    as.data.frame()

  admin <- obj %>%
    dplyr::filter(EVID==1) %>%
    select(ID, TIME, GROUPING, DOSE, DV) %>%
    as.data.frame()

  conc <- obj %>%
    dplyr::filter(EVID==0) %>%
    #dplyr::mutate(DV=case_when(EVID==1 ~ 0, .default=DV)) %>%
    # dplyr::filter(!is.na(DV)) %>%
    dplyr::select(ID, TIME, GROUPING, DV) %>%
    #dplyr::select(ID, NTIME, GROUPING, DV) %>%
    dplyr::distinct() %>%
    as.data.frame()

  intervals_manual <- data.frame(
    start=0,
    end=c(24, Inf),
    cmax=c(FALSE, TRUE),
    tmax=c(FALSE, TRUE),
    auclast=TRUE,
    aucinf.obs=c(FALSE, TRUE)
  )

  conc_obj <- PKNCA::PKNCAconc(
    conc, DV~TIME|ID+GROUPING
    #conc, DV~NTIME|ID+GROUPING
  )

  dose_obj <- PKNCA::PKNCAdose(
    admin, DOSE~TIME|ID+GROUPING
    #admin, DOSE~NTIME|ID+GROUPING
  )

  data_obj <- PKNCA::PKNCAdata(
    conc_obj, dose_obj,
    impute = "start_predose,start_conc0")

  # data_obj <- PKNCA::PKNCAdata(
  #   conc_obj, dose_obj, intervals=intervals_manual)

  results_obj <- PKNCA::pk.nca(data_obj)
  temp <- results_obj$result %>%
    rename_with(~group, GROUPING) %>%
    as.data.frame()

  return(temp)
}
