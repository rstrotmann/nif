
#' @export
nca <- function(obj, group="SEX"){
  UseMethod("nca")
}



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
nca.nif <- function(obj, group="SEX", analyte=NULL){
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

  obj <- obj %>%
    dplyr::filter(ANALYTE==current_analyte) %>%
    dplyr::mutate(GROUPING=as.factor(.data[[group]])) %>%
    as.data.frame()

  admin <- obj %>%
    dplyr::filter(EVID==1) %>%
    as.data.frame()

  conc <- obj %>%
    dplyr::mutate(DV=case_when(EVID==1 ~ 0, .default=DV)) %>%
    dplyr::filter(!is.na(DV)) %>%
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
  )

  dose_obj <- PKNCA::PKNCAdose(
    admin, DOSE~TIME|ID+GROUPING
  )

  data_obj <- PKNCA::PKNCAdata(conc_obj, dose_obj)
  results_obj <- PKNCA::pk.nca(data_obj)
  temp <- results_obj$result %>%
    as.data.frame()

  return(temp)
}
