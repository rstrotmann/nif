#' Non-compartmental analysis
#'
#' This function is a wrapper around the NCA functions provided by the
#' [PKNCA](https://CRAN.R-project.org/package=PKNCA) package.
#'
#' @param obj The source NIF object.
#' @param analyte The analyte. If none specified and multiple analytes are in
#'    the dataset, defaults to the first analyte.
#' @param group The grouping variable as string.
#' @param nominal_time A boolean to indicate whether nominal time rather than
#'    actual time should be used for NCA.
#' @param keep A vector of fields to keep in the output.
#' @param silent `r lifecycle::badge("deprecated")` Dummy option for
#' compatibility, set the global option [nif_option()] with `silent = TRUE` to
#' suppress messages.
#' @param average_duplicates Boolean to indicate whether duplicate entries
#'   should be averaged.
#' @param parent The parent compound to derive the administration information
#'   from. By default, equal to the analyte.
#'
#' @import dplyr
#' @return A data frame.
#' @export
#' @examples
#' nca(examplinib_fe_nif)
#' nca(examplinib_fe_nif, group = c("FASTED", "SEX"), analyte = "RS2023")
#'
nca <- function(obj, analyte = NULL, parent = NULL, keep = "DOSE",
                group = NULL,
                nominal_time = FALSE,
                average_duplicates = TRUE,
                silent = deprecated()) {
  # guess analyte if not defined
  if (is.null(analyte)) {
    current_analyte <- guess_analyte(obj)
    conditional_message(paste(
      "NCA: No analyte specified. Selected",
      current_analyte, "as the most likely."
    ))
  } else {
    current_analyte <- analyte
  }

  if (is.null(parent)) {
    parent <- current_analyte
  }

  # filter for analyte, set selected TIME
  obj1 <- obj %>%
    as.data.frame() %>%
    dplyr::filter(ANALYTE == current_analyte) %>%
    dplyr::mutate(TIME = case_when(nominal_time == TRUE ~ NTIME,
                                   .default = TIME)) %>%
    dplyr::mutate(DV = case_when(is.na(DV) ~ 0, .default = DV)) %>%
    as.data.frame()

  # preserve the columns to keep
  keep_columns <- obj1 %>%
    dplyr::filter(EVID == 1) %>%
    as.data.frame() %>%
    dplyr::select(c(ID, any_of(keep))) %>%
    dplyr::distinct()

  admin <- obj %>%
    as.data.frame() %>%
    dplyr::filter(ANALYTE == parent) %>%
    dplyr::mutate(TIME = case_when(
      nominal_time == TRUE ~ NTIME,
      .default = TIME
    )) %>%
    dplyr::mutate(DV = case_when(is.na(DV) ~ 0, .default = DV)) %>%
    dplyr::filter(EVID == 1) %>%
    mutate(PPRFTDTC = .data$DTC) %>%
    dplyr::select(any_of(
      c("REF", "ID", "TIME", "DOSE", "DV", "PPRFTDTC", group))) %>%
    as.data.frame()

  # concentration data
  conc <- obj1 %>%
    dplyr::filter(EVID == 0) %>%
    dplyr::select(any_of(c("ID", "TIME", "DV", "DOSE", group)))

  if (average_duplicates == TRUE) {
    conc <- conc %>%
      group_by(across(any_of(c("ID", "TIME", "DOSE", group)))) %>%
      summarize(DV = mean(DV, na.rm = TRUE), .groups = "drop")
  }

  if (!is.null(group)) {
    conc <- conc %>%
      dplyr::group_by(ID, TIME, across(any_of(group)))
  } else {
    conc <- conc %>% dplyr::group_by(ID, TIME)
  }

  # generate formulae for the conc and admin objects
  conc_formula <- "DV~TIME|ID"
  # dose_formula <- "DOSE~TIME|ID"
  dose_formula <- "DOSE~TIME|PPRFTDTC+ID"
  if (!is.null(group)) {
    group_string <- paste(group, collapse = "+")
    if(get("silent", .nif_env) == FALSE) {
      message(paste("NCA: Group by", group_string))
    }
    conc_formula <- paste0("DV~TIME|", group_string, "+ID")
    dose_formula <- paste0("DOSE~TIME|", group_string, "+PPRFTDTC+ID")
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
  )

  results_obj <- PKNCA::pk.nca(data_obj)

  temp <- results_obj$result %>%
    as.data.frame() %>%
    dplyr::left_join(keep_columns, by = "ID")

  if (!is.null(group)) {
    temp <- temp %>%
      dplyr::mutate_at(group, as.factor)
  }

  return(temp)
}


#' Non-compartmental analysis of NIF data
#'
#' This function is a wrapper around the NCA functions provided by the
#' [PKNCA](https://CRAN.R-project.org/package=PKNCA) package.
#'
#' @description
#' `r lifecycle::badge("experimental")`
#'
#' @param nif A nif object.
#' @param analyte The analyte as character.
#' @param parent The parent as character.
#' @param keep Columns to keep, as character.
#' @param group Columns to group by, as character.
#' @param time The time field as character.
#' @param average_duplicates Average duplicate concentration values, as logical.
#'
#' @return A data frame.
#' @export
#'
#' @examples
#' nca1(examplinib_sad_nif, time = "TAD")
#' nca1(examplinib_fe_nif, time = "TAD", group = "FASTED")
nca1 <- function(nif,
                 analyte = NULL, parent = NULL,
                 keep = NULL, group = NULL,
                 time = "TIME",
                 average_duplicates = TRUE) {

  allowed_times <- c("TIME", "NTIME", "TAFD", "TAD")
  if(!time %in% allowed_times) {
    stop(paste0("'time' parameter must be one of ",
                nice_enumeration(allowed_times, conjunction = "or")))
  }

  ## check that analyte and parent are scalars!

  # guess analyte if not defined
  if(is.null(analyte)) {
    current_analyte <- guess_analyte(nif)
    conditional_message(
      "NCA: No analyte specified. Selected ",
      current_analyte, " as the most likely."
    )
  } else {
    current_analyte <- analyte
  }

  # assign parent if not defined
  if (is.null(parent)) {
    analytes_parents <- analyte_overview(nif)
    parent <- analytes_parents[analytes_parents$ANALYTE == current_analyte,
                               "PARENT"]
  }

  # make observation and administration data sets
  obj <- nif %>%
    ensure_time() %>%
    index_dosing_interval() %>%
    as.data.frame() %>%
    mutate(TIME = .data[[time]]) %>%
    mutate(DV = case_when(is.na(DV) ~ 0, .default = DV)) #%>%
    # select(any_of(
    #   c("ID", "TIME", "DI", "EVID", "ANALYTE", "DOSE", "DV", group)))

  # preserve the columns to keep
  keep_columns <- obj %>%
    # as.data.frame() %>%
    # filter(EVID == 1) %>%
    select(c("ID", "DOSE", "DI", any_of(c(keep)))) %>%
    distinct()

  # dosint data
  admin <- obj %>%
    filter(.data$ANALYTE == parent) %>%
    filter(.data$EVID == 1) %>%
    select(any_of(
      c("ID", "TIME", "DI", "EVID", "ANALYTE", "DOSE", "DV", group)))

  # concentration data
  conc <- obj %>%
    filter(.data$ANALYTE == current_analyte) %>%
    filter(.data$EVID == 0) %>%
    select(any_of(
      c("ID", "TIME", "DI", "EVID", "ANALYTE", "DOSE", "DV", group)))

  if (average_duplicates == TRUE) {
    conc <- conc %>%
      group_by(across(any_of(c("ID", "TIME", "DOSE", "DI", group)))) %>%
      summarize(DV = mean(DV, na.rm = TRUE), .groups = "drop")
  }

  group_string <- paste(group, collapse = "+")
  if(group_string != ""){
    conditional_message(paste("NCA: Group by", group_string))
    group_string <- paste0(group_string, "+")
  }

  conc_formula <- paste0("DV~TIME|", group_string, "ID+DI")
  dose_formula <- paste0("DOSE~TIME|", group_string, "ID+DI")

  nca <- PKNCA::pk.nca(
    PKNCA::PKNCAdata(
      PKNCA::PKNCAconc(conc, stats::as.formula(conc_formula)),
      PKNCA::PKNCAdose(admin, stats::as.formula(dose_formula)),
      impute = "start_conc0")
    )

  nca$result %>%
    left_join(keep_columns, by = c("ID", "DI")) %>%
    as.data.frame() %>%
    dplyr::mutate_at(group, as.factor)
}


#' Generate NCA table from the SDTM.PP domain
#'
#' @param obj A nif object.
#' @param sdtm_data A stdm data object.
#' @param analyte The analyte as character.
#' @param keep Column names to keep, as character.
#' @param observation_filter Observation filter term, as character.
#' @param group Grouping variable as character.
#'
#' @return A data frame.
#' @export
nca_from_pp <- function(
    obj, sdtm_data,
    analyte = NULL, keep = NULL, group = NULL, observation_filter = "TRUE") {
  if (is.null(analyte)) {
    current_analyte <- guess_analyte(obj)
    conditional_message(paste(
      "NCA: No analyte specified. Selected", current_analyte,
      "as the most likely."))
  } else {
    current_analyte <- analyte
  }

  # preserve the columns to keep
  keep_data <- obj %>%
    as.data.frame() %>%
    filter(ANALYTE == current_analyte) %>%
    filter(.data$EVID == 1) %>%
    select(c("ID", "USUBJID", any_of(keep), any_of(c("AGE", "SEX", "RACE", "WEIGHT",
      "HEIGHT", "BMI", "PART", "COHORT", "DOSE")), starts_with("BL_"))) %>%
    distinct()

  if(!"pp" %in% names(sdtm_data$domains)) {
    stop("Domain PP is not included in the sdtm object!")
  }

  sdtm_data$domains[["pp"]] %>%
    # filter(.data$PPCAT == current_analyte) %>%
    filter(eval(parse(text = observation_filter))) %>%
    select(any_of(c("USUBJID", "PPTESTCD", "ANALYTE" = current_analyte,
                    "PPSTRESN", "PPSPEC", "PPCAT", "PPRFTDTC", group))) %>%
    left_join(keep_data, by = "USUBJID")
}


#' PK parameter summary statistics by dose
#'
#' @param nca The NCA results as provided by `nca`, as data frame.
#' @param parameters The NCA paramters to be tabulated as character,
#' @param group The grouping variable, defaults to DOSE.
#'
#' @return A data frame
#' @importFrom PKNCA geomean
#' @importFrom PKNCA geocv
#' @importFrom stats IQR
#' @export
#'
#' @examples
#' nca_summary(nca(examplinib_sad_nif, analyte = "RS2023"))
nca_summary <- function(
    nca,
    parameters = c("auclast", "cmax", "tmax", "half.life", "aucinf.obs",
                   "AUCLST", "CMAX", "TMAX", "LAMZHL", "AUCIFP"),
    group = NULL) {
  nca %>%
    filter(PPTESTCD %in% parameters) %>%
    {if("exclude" %in% names(.)) filter(., is.na(.data$exclude)) else .} %>%
    {if(!"PPORRES" %in% names(.) & "PPSTRESN" %in% names(.))
        mutate(., PPORRES = .data$PPSTRESN) else .} %>%
    group_by_at(c(group, "DOSE", "PPTESTCD")) %>%
    summarize(
      geomean = PKNCA::geomean(PPORRES, na.rm = TRUE),
      geocv = PKNCA::geocv(PPORRES, na.rm = TRUE),
      median = median(PPORRES, na.rm = TRUE), iqr = IQR(PPORRES, na.rm = TRUE),
      min = min(PPORRES, na.rm = TRUE),
      max = max(PPORRES, na.rm = TRUE),
      n = n()
    )
}


#' PK parameter summary statistics table by dose
#'
#' @param nca The NCA results as provided by `nca`, as data frame.
#' @param parameters The NCA paramters to be tabulated as character,
#' @param digits The number of significant digits to be displayed.
#' @param group The grouping variable, defaults to DOSE.
#' @return A data frame
#'
#' @importFrom PKNCA geomean
#' @importFrom PKNCA geocv
#' @export
#'
#' @examples
#' nca_summary_table(nca(examplinib_sad_nif, analyte = "RS2023"))
nca_summary_table <- function(
    nca,
    parameters = c("auclast", "cmax", "tmax", "half.life", "aucinf.obs",
                   "AUCLST", "CMAX", "TMAX", "LAMZHL", "AUCIFP"),
    digits = 2,
    group = NULL) {
  s <- nca_summary(nca, parameters, group = group)

  median_parameters <- c("tlast", "tmax", "lambda.z.n.points", "TMAX",
                         "LAMZNPT")
  s %>%
    mutate(
      center = case_when(
        PPTESTCD %in% median_parameters ~ as.character(round(median,
          digits = digits
        )),
        .default = as.character(round(geomean, digits = digits))
      ),
      dispersion = case_when(
        PPTESTCD %in% median_parameters ~ paste0(
          as.character(round(min, digits = digits)), "; ",
          as.character(round(max, digits = digits))
        ),
        .default = as.character(round(geocv))
      )
    ) %>%
    mutate(value = paste0(center, " (", dispersion, ")")) %>%
    tidyr::pivot_wider(
      id_cols = c(any_of(c(group, "DOSE")), "n"),
      names_from = "PPTESTCD", values_from = value
    )
}


#' Test for dose linearity
#'
#' @description
#' `r lifecycle::badge("experimental")`
#'
#' Currently experimental. Don't use in production!
#'
#' Using the power model described by
#' [Hummel, 2009](https://doi.org/10.1002/pst.326).
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
#' with ln(r) the logarithm of the ratio of the highest dose to the lowest dose.
#' @param nca The non-compartmental analysis data.
#' @param parameters The NCA parameters to investigate for dose linearity.
#' @param lower The lower threshold for Rdnm.
#' @param upper the upper threshold for Rdnm.
#'
#' @return A data frame.
#' @export
dose_lin <- function(nca, parameters = c("aucinf.obs", "cmax"),
                     lower = 0.8, upper = 1.25) {
  pp <- nca %>%
    filter(PPORRES != 0) %>%
    dplyr::mutate(ldose = log(DOSE), lpp = log(PPORRES))

  power_model <- t(sapply(
    parameters,
    function(x) {
      as.numeric(
        stats::confint(
          stats::lm(
            data = (pp %>% filter(PPTESTCD == x)),
            formula = lpp ~ ldose
          ),
          "ldose",
          level = 0.9
        )
      )
    }
  ))
  lnr <- log(max(pp$DOSE) / min(pp$DOSE))
  bl <- 1 + log(lower) / lnr
  bu <- 1 + log(upper) / lnr
  power_model <- rbind("thresholds" = c(bl, bu), power_model)
  colnames(power_model) <- c("lower", "upper")
  power_model %>%
    as.data.frame() %>%
    dplyr::mutate(dose_linear = case_when(
      row_number() == 1 ~ NA,
      .default = lower > lower[1] & upper < upper[1]
    ))
}


#' Power fit for PK parameters
#'
#' @description
#' `r lifecycle::badge("experimental")`
#'
#' Plot the PK parameter defined by `parameter` over `DOSE` and overlay with
#' a linear model of the log-transformed parameter and the log-transformed dose.
#' The slope is printed in the caption line.
#'
#' All zero values for the selected paramter are filtered out before analysis.
#'
#' @param nca PK parameters as data frame.
#' @param group Grouping parameter as character.
#' @param parameter The PK parameter as character.
#' @param title The title as character.
#' @param size The point size as numeric.
#' @param alpha The alpha value for the data points as numeric.
#'
#' @return A list of ggplot2 objects.
#' @importFrom stats lm predict.lm
#' @export
#' @examples
#' nca_power_model(nca(examplinib_sad_nif, analyte = "RS2023"), "aucinf.obs")
#' nca_power_model(nca(examplinib_sad_nif, analyte = "RS2023"),
#'   c("cmax", "aucinf.obs"))
nca_power_model <- function(nca, parameter = NULL,
    group = NULL, title = NULL, size = 2, alpha = 1) {

  if(is.null(parameter)) {
    std_parameters = c("cmax", "aucinf.obs", "CMAX", "AUCIFP")
    parameter <- std_parameters[which(std_parameters %in% unique(nca$PPTESTCD))]
  }

  pm_plot <- function(param) {
    pp <- nca %>%
      filter(PPTESTCD == param) %>%
      {if(!"PPORRES" %in% names(.) & "PPSTRESN" %in% names(.))
        mutate(., PPORRES = .data$PPSTRESN) else .} %>%
      filter(PPORRES != 0)

    pm <- pp %>%
      lm(log(PPORRES) ~ log(DOSE), data = .)

    color_label <- nice_enumeration(unique(group))

    pp %>%
      {if(!is.null(group))
        tidyr::unite(., COLOR, all_of(group), sep = "-", remove = FALSE) else .} %>%
      bind_cols(predict.lm(pm, pp, interval = 'prediction', se.fit = T,
                           level = 0.9)$fit) %>%
      ggplot2::ggplot(ggplot2::aes(x = DOSE, y = exp(.data$fit))) +
      ggplot2::geom_line() +
      ggplot2::geom_ribbon(ggplot2::aes(x = DOSE, ymin = exp(.data$lwr),
                                        ymax = exp(.data$upr)),
                           fill = 'lightgrey', alpha = 0.5) +
      {if(!is.null(group)) ggplot2::geom_point(
        ggplot2::aes(x = DOSE, y = PPORRES, color = COLOR), size = size, alpha = alpha)
          else
            ggplot2::geom_point(ggplot2::aes(x = DOSE, y = PPORRES),
                                size = size, alpha = alpha)} +
      ggplot2::theme_bw() +
      ggplot2::expand_limits(x = 0) +
      ggplot2::labs(x = "dose (mg)", y = param,
           caption = paste0("mean and 90% PI, slope = ",
                            round(pm$coefficients[2], 3))) +
      {if(!is.null(group)) ggplot2::labs(color = color_label)} +
      {if(!is.null(title)) ggplot2::ggtitle(title)} +
      watermark(cex = 1.5)
  }
  out <- lapply(parameter, pm_plot)
  names(out) <- parameter
  return(out)
}











