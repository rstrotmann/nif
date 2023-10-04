#' Identify and index rich PK sampling intervals
#'
#' Currently experimental. Don't use in production!
#'
#' @param obj The NIF data set.
#' @param min_n The minimum number of PK samples per analyte to qualify as rich
#'   sampling.
#'
#' @return A new NIF data set.
#' @export
index_rich_sampling_intervals <- function(obj, min_n=4) {
  obj1 <- obj %>%
    index_nif() %>%
    index_dosing_interval() %>%
    as.data.frame()

  npdi <- obj %>%
    group_by(ANALYTE) %>%
    n_observations_per_dosing_interval() %>%
    ungroup() %>%
    select(ID, DI, ANALYTE, N)

  temp <- obj1 %>%
    left_join(npdi, by=c("ID", "DI", "ANALYTE")) %>%
    mutate(RICHINT_TEMP=(N>min_n)) %>%

    # add last observation before administration to rich interval
    group_by(ID, ANALYTE) %>%
    mutate(LEAD=lead(RICHINT_TEMP)) %>%
    mutate(RICHINT= RICHINT_TEMP | (LEAD & EVID==0)) %>%

    #mutate(RICHINT=lead(RICHINT_TEMP, n=1) | RICHINT_TEMP) %>%

    fill(RICHINT, .direction="down") %>%
    ungroup() %>%
    select(-c("RICHINT_TEMP", "LEAD")) %>%
    group_by(ID, ANALYTE) %>%
    mutate(FLAG=(RICHINT!=lag(RICHINT) | row_number()==1)) %>%
    ungroup() %>%
    as.data.frame()

  rich_index <- temp %>%
    filter(FLAG==TRUE & RICHINT==TRUE) %>%
    group_by(ID, ANALYTE) %>%
    mutate(RICH_N=row_number()) %>%
    ungroup() %>%
    select(REF, RICH_N)

  temp %>%
    left_join(rich_index, by="REF") %>%
    group_by(RICHINT) %>%
    fill(RICH_N, .direction="down") %>%
    ungroup() %>%
    select(-c("N", "FLAG")) %>%
    new_nif()
}


#' Non-compartmental analysis
#'
#' @param obj The source NIF object.
#' @param analyte The analyte. If none specified and multiple analytes are in
#'    the dataset, defaults to the first analyte.
#' @param group The grouping variable as string.
#' @param nominal_time A boolean to indicate whether nominal time rather than
#'    actual time should be used for NCA.
#' @param keep A vector of fields to keep in the output.
#' @param silent No message output.
#' @param average_duplicates Boolean to indicate whether duplicate entries
#'   should be averaged.
#'
#' @import dplyr
#' @return A data frame.
#' @export
#' @examples
#' nca(examplinib_nif)
#' nca(examplinib_nif, group=c("FASTED", "SEX"), analyte="RS2023")
#'
nca <- function(obj, analyte=NULL, keep=NULL, group=NULL, nominal_time=F,
                silent=F, average_duplicates=T){
  # guess analyte if not defined
  if(is.null(analyte)) {
    current_analyte <- guess_analyte(obj)
    if(silent==FALSE) {
      message(paste("NCA: No analyte specified. Selected",
                    current_analyte, "as the most likely."))
    }
  } else {
    current_analyte <- analyte
  }

  # filter for analyte, set selected TIME
  obj1 <- obj %>%
    as.data.frame() %>%
    dplyr::filter(ANALYTE==current_analyte) %>%
    dplyr::mutate(TIME=case_when(nominal_time==TRUE~NTIME, .default=TIME)) %>%
    dplyr::mutate(DV=case_when(is.na(DV) ~ 0, .default=DV)) %>%
    as.data.frame()

  # preserve the columns to keep
  keep_columns <- obj1 %>%
    dplyr::filter(EVID==1) %>%
    as.data.frame() %>%
    dplyr::select(c(ID, any_of(keep))) %>%
    dplyr::distinct()

  # administration times
  admin <- obj1 %>%
    dplyr::filter(EVID==1) %>%
    dplyr::select(any_of(c("REF", "ID", "TIME", "DOSE", "DV", group))) %>%
    as.data.frame()

  # concentration data
  conc <- obj1 %>%
    dplyr::filter(EVID==0) %>%
    dplyr::select(any_of(c("ID", "TIME", "DV", group)))

  if(average_duplicates==T){
    conc <- conc %>%
      group_by(across(any_of(c("ID", "TIME", group)))) %>%
      summarize(DV=mean(DV, na.rm=T))
  }

  if(!is.null(group)){
    conc <- conc %>%
      dplyr::group_by(ID, TIME, across(any_of(group)))
  } else {
    conc <- conc %>% dplyr::group_by(ID, TIME)
  }

  # generate formulae for the conc and admin objects, depending on whether there
  conc_formula <- "DV~TIME|ID"
  dose_formula <- "DOSE~TIME|ID"
  if(!is.null(group)) {
    group_string <- paste(group, collapse="+")
    if(silent==F){
      message(paste("NCA: Group by", group_string))
    }
    conc_formula <- paste0("DV~TIME|", group_string, "+ID")
    dose_formula <- paste0("DOSE~TIME|", group_string, "+ID")
  }

  conc_obj <- PKNCA::PKNCAconc(
    conc,
    stats::as.formula(conc_formula)
  )

  dose_obj <- PKNCA::PKNCAdose(
    admin,
    stats::as.formula(dose_formula)
  )

  data_obj <- PKNCA::PKNCAdata(
    conc_obj,
    dose_obj,
    impute = "start_conc0"
    # impute = "start_predose, start_conc0"
    )

  results_obj <- PKNCA::pk.nca(data_obj)

  temp <- results_obj$result %>%
    as.data.frame() %>%
    dplyr::left_join(keep_columns, by="ID")

  if(!is.null(group)) {
    temp <- temp %>%
      dplyr::mutate_at(group, as.factor)
  }

  return(temp)
}


#' Test for dose linearity
#'
#' Currently experimental. Don't use in production!
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





