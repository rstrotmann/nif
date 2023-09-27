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


nca1 <- function(obj, analyte=NULL, keep=NULL){
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
    dplyr::mutate(DV=case_when(is.na(DV) ~ 0, .default=DV)) %>%
    as.data.frame()

  # preserve the columns to keep
  keep_colunmns <- obj %>%
    filter(EVID==1) %>%
    as.data.frame() %>%
    select(c(ID, any_of(keep))) %>%
    distinct()

  admin <- obj %>%
    dplyr::filter(EVID==1) %>%
    select(ID, TIME, DOSE, DV) %>%
    as.data.frame()

  conc <- obj %>%
    dplyr::filter(EVID==0) %>%
    dplyr::select(ID, TIME, DV) %>%
    dplyr::group_by(ID, TIME) %>%
    dplyr::summarize(DV=mean(DV), .groups="drop") %>%
    # dplyr::ungroup() %>%
    # dplyr::distinct() %>%
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
    conc, DV~TIME|ID
  )

  dose_obj <- PKNCA::PKNCAdose(
    admin, DOSE~TIME|ID
  )

  data_obj <- PKNCA::PKNCAdata(
    conc_obj, dose_obj,
    impute = "start_predose, start_conc0")

  results_obj <- PKNCA::pk.nca(data_obj)
  temp <- results_obj$result %>%
    as.data.frame()

  return(results_obj$result %>% as.data.frame())
}







#' Test for dose linearity
#'
#' Using the power model described by [Hummel, 2009](https://doi.org/10.1002/pst.326).
#' In brief, a power model is fitted with
#'
#' ln(AUC) = mu + beta* ln(dose)
#'
#' and the 90% CI of beta compared to the below acceptance criteria, assuming
#'
#' theta_L of 0.8 and theta_U of 1.25:
#'
#' (beta_L, beta_U) = ( 1 + ln(theta_L) / ln(r), 1 + lntheta_U) / ln(r) )
#'
#' with ln(r) the logarithm of the ratio of the highest dose to the lowerst dose.
#'
#'
#' @param nca The non-compartmental analysis data.
#' @param parameters The NCA parameters to investigate for dose linearity.
#' @param lower The lower threshold for Rdnm.
#' @param upper the upper threshold for Rdnm.
#'
#' @return A data frame.
#' @export
dose_lin <- function(nca, parameters=c("aucinf.obs", "cmax"),
                     lower=0.8, upper=1.25) {
  pp <- nca %>%
    mutate(ldose=log(DOSE), lpp=log(PPORRES))

  power_model <- t(sapply(parameters,
                   function(x){as.numeric(
                     confint(
                       lm(data=(pp %>% filter(PPTESTCD==x)), formula=lpp~ldose),
                       "ldose",
                       level=0.9))
                   }))
  lnr <- log(max(pp$DOSE)/min(pp$DOSE))
  bl <- 1 + log(lower)/lnr
  bu <- 1 + log(upper)/lnr
  power_model = rbind("thresholds"=c(bl, bu), power_model)
  colnames(power_model) <- c("lower", "upper")
  power_model %>%
    as.data.frame() %>%
    mutate(dose_linear=case_when(
      row_number()==1 ~ NA,
      .default= lower>lower[1] & upper<upper[1]))
}





