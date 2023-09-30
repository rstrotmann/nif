#' Non-compartmental analysis
#'
#' @param obj The source NIF object.
#' @param analyte The analyte. If none specified and multiple analytes are in
#' #'   the dataset, defaults to the first analyte.
#' @param grouping The grouping variable as string.
#' @param keep A vector of fields to keep in the output.
#' @import dplyr
#' @return A data frame.
#' @export
nca <- function(obj, analyte=NULL, keep=NULL, grouping=NULL){
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

  obj1 <- obj %>%
    dplyr::filter(ANALYTE==current_analyte) %>%
    dplyr::mutate(DV=case_when(is.na(DV) ~ 0, .default=DV)) %>%
    as.data.frame()

  # preserve the columns to keep
  keep_colunmns <- obj1 %>%
    filter(EVID==1) %>%
    as.data.frame() %>%
    select(c(ID, any_of(keep))) %>%
    distinct()

  admin <- obj1 %>%
    dplyr::filter(EVID==1) %>%
    select(ID, TIME, DOSE, DV, grouping) %>%
    as.data.frame()

  conc <- obj1 %>%
    dplyr::filter(EVID==0) %>%
    dplyr::select(ID, TIME, DV, grouping)

  if(!is.null(grouping)){
    conc <- conc %>%
      dplyr::group_by(ID, TIME, .data[[grouping]])
  } else {
    conc <- conc %>% dplyr::group_by(ID, TIME)
  }

  conc <- conc %>%
    dplyr::summarize(DV=mean(DV), .groups="drop") %>%
    # dplyr::ungroup() %>%
    # dplyr::distinct() %>%
    as.data.frame()

  intervals_manual <- data.frame(
    start=0,
    #end=c(24, Inf),
    end = Inf,
    #cmax=c(FALSE, TRUE),
    cmax = TRUE,
    #tmax=c(FALSE, TRUE),
    tmax = TRUE,
    auclast=TRUE,
    #auclast=c(FALSE, TRUE),
    #aucinf.obs=c(FALSE, TRUE)
    aucinf.obs = TRUE
  )

  conc_formula <- "DV~TIME|ID"
  dose_formula <- "DOSE~TIME|ID"
  if(!is.null(grouping)) {
    conc_formula <- paste0(conc_formula, "+", grouping)
    dose_formula <- paste0(dose_formula, "+", grouping)
  }

  conc_obj <- PKNCA::PKNCAconc(
    conc,
    stats::as.formula(conc_formula)
  )

  dose_obj <- PKNCA::PKNCAdose(
    admin,
    stats::as.formula(dose_formula)
    # DOSE~TIME|ID+grouping
  )

  data_obj <- PKNCA::PKNCAdata(
    conc_obj,
    dose_obj,
    intervals=intervals_manual,
    impute = "start_predose, start_conc0")

  results_obj <- PKNCA::pk.nca(data_obj)

  temp <- results_obj$result %>%
    as.data.frame() %>%
    left_join(keep_colunmns, by="ID")

  if(!is.null(grouping)) {
    temp <- temp %>%
    mutate(!!grouping := factor(.data[[grouping]]))
    # mutate({{grouping}} := factor(.data[[grouping]])) %>%
  }

  return(temp)
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
    filter(PPORRES!=0) %>%
    dplyr::mutate(ldose=log(DOSE), lpp=log(PPORRES))

  power_model <- t(sapply(parameters,
                   function(x){as.numeric(
                     stats::confint(
                       stats::lm(data=(pp %>% filter(PPTESTCD==x)), formula=lpp~ldose),
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
    dplyr::mutate(dose_linear=case_when(
      row_number()==1 ~ NA,
      .default= lower>lower[1] & upper<upper[1]))
}





