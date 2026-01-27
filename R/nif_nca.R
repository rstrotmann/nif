#' Non-compartmental analysis of NIF data
#'
#' This function is a wrapper around the NCA functions provided by the
#' [PKNCA](https://CRAN.R-project.org/package=PKNCA) package.
#'
#' NA values are set to zero!
#' Negative concentrations are set to zero!
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
#' @param silent Suppress messages.
#' @param duplicates Selection how to deal with duplicate observations with
#'   respect to the USUBJID, ANALYTE and DTC fields:
#'   * 'stop': Stop execution and produce error message
#'   * 'identify': Return a list of duplicate entries
#'   * 'resolve': Resolve duplicates, applying the `duplicate_function` to the
#'   duplicate entries.
#' @param duplicate_function Function to resolve duplicate values, defaults to
#'   `mean`.
#'
#' @return A data frame.
#' @export
#'
#' @examples
#' head(nca(examplinib_sad_nif, time = "TAD"))
#' head(nca(examplinib_fe_nif, time = "TIME", group = "FASTED"))
nca <- function(
    nif,
    analyte = NULL,
    parent = NULL,
    keep = NULL,
    group = NULL,
    time = "TIME",
    duplicates = "stop",
    duplicate_function = mean,
    silent = NULL
    ) {
  # input validation
  validate_nif(nif)

  allowed_times <- c("TIME", "NTIME", "TAFD", "TAD")
  if (!time %in% allowed_times) {
    stop(paste0(
      "'time' parameter must be one of ",
      nice_enumeration(allowed_times, conjunction = "or")
    ))
  }

  ## check that analyte and parent are scalars!

  # guess analyte if not defined
  if (is.null(analyte)) {
    current_analyte <- guess_analyte(nif)
    conditional_cli({
      cli_alert_warning("No analyte specified for NCA!")
      cli_text(paste0("Selected ", current_analyte,
                      " as the most likely analyte!"))
    },
    silent = silent)
  } else {
    current_analyte <- analyte
  }

  if (is.na(current_analyte) || !current_analyte %in% nif$ANALYTE)
    stop(paste0("Invalid analyte (", current_analyte, ")!"))

  # assign parent if not defined
  if (is.null(parent)) {
    analytes_parents <- analyte_overview(nif)

    if (nrow(analytes_parents) == 0) {
      stop("Cannot determine parent.")
    }

    if (!current_analyte %in% analytes_parents$ANALYTE) {
      stop(paste0(
        "Analyte '", current_analyte,
        "' not found in analyte_overview(). Cannot determine parent."
      ))
    }

    parent_rows <- analytes_parents[analytes_parents$ANALYTE == current_analyte, ]

    if (nrow(parent_rows) == 0) {
      stop(paste0(
        "Could not find parent for analyte '", current_analyte,
        "' in analyte_overview()."
      ))
    }

    if (nrow(parent_rows) > 1) {
      stop(paste0(
        "Multiple parent entries found for analyte '", current_analyte,
        "' in analyte_overview()."
      ))
    }

    parent <- parent_rows$PARENT[1]

    if (is.na(parent) || length(parent) == 0 || (is.character(parent) && nchar(parent) == 0)) {
      stop(paste0(
        "Could not determine parent for analyte '", current_analyte,
        "'. Parent is missing or empty."
      ))
    }

    conditional_cli(
      cli_alert_info(paste0("Parent set to ", parent)),
      silent = silent
    )
  }

  if (!parent %in% parents(nif))
    stop("Parent not found (", parent, ")!")

  # make observation and administration data sets
  obj <- nif |>
    ensure_time() |>
    ensure_analyte() |>
    ensure_parent() |>
    index_dosing_interval() |>
    as.data.frame() |>
    mutate(TIME = .data[[time]]) |>
    mutate(DV = case_when(is.na(.data$DV) ~ 0, .default = .data$DV))

  # # preserve the columns to keep
  # keep_columns <- obj |>
  #   select(c("ID", "DOSE", "DI", any_of(c(keep)))) |>
  #   distinct()

  # dosing data
  admin <- obj |>
    filter(.data$ANALYTE == parent) |>
    filter(.data$EVID == 1) |>
    select(any_of(
      c("ID", "TIME", "DI", "EVID", "ANALYTE", "DOSE", "DV", group)
    ))

  # concentration data
  conc <- obj |>
    filter(.data$ANALYTE == current_analyte) |>
    filter(.data$EVID == 0) |>
    select(any_of(
      # unique(c("ID", "TIME", time, "DI", "EVID", "ANALYTE", "DOSE", "DV", group))
      unique(c("ID", "TIME", time, "DI", "EVID", "ANALYTE", "DOSE", "DV", group, keep))
    ))

  # preserve the columns to keep
  keep_columns <- conc |>
    select(any_of(c("ID", "DOSE", "DI", keep))) |>
    distinct()

  # deal with negative concentrations
  n_negative <- nrow(filter(conc, .data$DV < 0))

  if (n_negative > 0) {
    conditional_cli(
      cli_alert_warning(paste0(n_negative,
                               " negative concentrations set to zero!")),
      silent = silent
    )
    conc <- conc |>
      mutate(DV = case_when(.data$DV < 0 ~ 0, .default = .data$DV))
  }

  # dealing with duplicates
  dupl_fields <- c("ID", "ANALYTE", time)

  n_dupl <- find_duplicates(
    conc, fields = dupl_fields, count_only = TRUE)

  if (n_dupl != 0) {
    conditional_cli(
      cli_alert_warning(paste0(
        n_dupl, " duplicate observations in NCA for ", current_analyte)),
      silent = silent
    )

    if (duplicates == "stop") {
      stop(paste0(
        n_dupl, " duplicate ",
        plural("observation", n_dupl > 1), " found with respect to ",
        nice_enumeration(dupl_fields), ".\n\n",
        "Identify duplicates using the `duplicates = 'identify'` parameter, ",
        "or have duplicates automatically resolved with ",
        "`duplicates = 'resolve'` where the resolution function is specified ",
        "by the `duplicate_function` parameter (default is `mean`)."
      ))
    }

    if (duplicates == "identify") {
      cli::cli({
        cli::cli_alert_danger("Only duplicate observations returned")
        cli::cli_text(paste0(
          n_dupl, " duplicate observations found with respect to ",
          nice_enumeration(dupl_fields), "."
        ))
        cli_text()
      })

      d <- find_duplicates(conc, fields = dupl_fields)
      return(d)
    }

    if (duplicates == "resolve") {
      conditional_cli(
        cli::cli({
          cli_alert_warning(paste0(
            n_dupl, " duplicate observations for ", analyte,
            " resolved, applying ",
            function_name(duplicate_function)
          ))
        }),
        silent = silent
      )

      conc <- resolve_duplicates(
        conc,
        fields = dupl_fields,
        dependent_variable = "DV",
        duplicate_function = duplicate_function,
        na_rm = TRUE
      )
    }
  }

  # grouping
  group_string <- if (!is.null(group)) paste(group, collapse = "+") else ""
  if (group_string != "") {
    conditional_cli(
      cli_alert_info(paste0("NCA: Group by ", group_string)),
      silent = silent
    )
    group_string <- paste0(group_string, "+")
  }

  # conduct NCA
  conc_formula <- paste0("DV~TIME|", group_string, "ID+DI")
  dose_formula <- paste0("DOSE~TIME|", group_string, "ID+DI")

  nca <- PKNCA::pk.nca(
    PKNCA::PKNCAdata(
      PKNCA::PKNCAconc(conc, stats::as.formula(conc_formula)),
      PKNCA::PKNCAdose(admin, stats::as.formula(dose_formula)),
      impute = "start_conc0"
    )
  )

  result <- nca$result |>
    left_join(keep_columns, by = c("ID", "DI")) |>
    as.data.frame()

  if (!is.null(group)) {
    result <- result |>
      dplyr::mutate(dplyr::across(dplyr::all_of(group), as.factor))
  }

  result
}


#' Generate NCA table from the SDTM.PP domain
#'
#' @param obj A nif object.
#' @param sdtm_data A SDTM data object containing a PP domain.
#' @param analyte The analyte as character. If NULL, will be guessed from the
#'   nif object.
#' @param keep Columns to keep from the input nif object, as character.
#' @param observation_filter Observation filter term as character. Must be valid
#'   R code that can be evaluated on the PP domain.
#' @param group Grouping variable from the pp domain, as character.
#' @param silent Suppress message output.
#' @param ppcat The value for PPCAT (Test category) to filter the PP domain for.
#'   If NULL, no filtering is done.
#' @param ppscat The value for PPSCAT (Test subcategory) to filter the PP domain
#'   for. If NULL, no filtering is done.
#'
#' @return A data frame containing the filtered and joined PP domain data.
#' @export
nca_from_pp <- function(
  obj,
  sdtm_data,
  analyte = NULL,
  ppcat = NULL,
  ppscat = NULL,
  keep = NULL,
  group = NULL,
  observation_filter = "TRUE",
  silent = NULL
) {
  # Input validation
  if (!inherits(obj, "nif")) {
    stop("nif must be an nif object")
  }

  if (!inherits(sdtm_data, "sdtm")) {
    stop("sdtm_data must be an sdtm object")
  }

  validate_char_param(analyte, "analyte", allow_null = TRUE)
  validate_char_param(ppcat, "ppcat", allow_null = TRUE)
  validate_char_param(ppscat, "ppscat", allow_null = TRUE)
  validate_char_param(keep, "keep", allow_null = TRUE, allow_multiple = TRUE)
  validate_char_param(group, "group", allow_null = TRUE)
  validate_char_param(observation_filter, "observation_filter")

  if (!"pp" %in% names(sdtm_data$domains)) {
    stop("Domain PP is not included in the sdtm object")
  }

  # Validate observation filter
  tryCatch(
    {
      parse(text = observation_filter)
    },
    error = function(e) {
      stop("Invalid observation_filter")
    }
  )

  # Guess analyte if not provided
  if (is.null(analyte)) {
    current_analyte <- guess_analyte(obj)
    conditional_message(
      "NCA: No analyte specified. Selected", current_analyte,
      "as the most likely.",
      silent = silent
    )
  } else {
    current_analyte <- analyte
  }

  # preserve the columns to keep from the nif object
  keep_data <- obj |>
    as.data.frame() |>
    filter(.data$ANALYTE == current_analyte) |>
    select(
      c(
        "ID", "USUBJID", any_of(keep),
        any_of(c(
          "AGE", "SEX", "RACE", "WEIGHT", "HEIGHT", "BMI",
          "PART", "COHORT", "DOSE"
        )),
        starts_with("BL_")
      )
    ) |>
    distinct()

  pp <- domain(sdtm_data, "pp")

  # validate ppcat and ppscat
  if (!is.null(ppcat)) {
    if (!"PPCAT" %in% names(pp)) {
      stop("PPCAT not found in PP domain")
    }
    if (!ppcat %in% unique(pp$PPCAT)) {
      stop(paste0(
        "PPCAT of ", ppcat, " not found in PP domain"
      ))
    }
  }

  if (!is.null(ppscat)) {
    if (!"PPSCAT" %in% names(pp)) {
      stop("PPSCAT not found in PP domain")
    }
    if (!ppscat %in% unique(pp$PPSCAT)) {
      stop(paste0(
        "PPSCAT of ", ppscat, " not found in PP domain"
      ))
    }
  }

  result <- pp

  if (!is.null(ppcat))
    result <- filter(result, .data$PPCAT == ppcat)

  if (!is.null(ppscat))
    result <- filter(result, .data$PPSCAT == ppscat)

  result <- result |>
    filter(eval(parse(text = observation_filter))) |>
    select(any_of(c(
      "USUBJID", "PPTESTCD", "PPSTRESN", "PPSPEC",
      "PPCAT", "PPRFTDTC", group
    ))) |>
    mutate(ANALYTE = current_analyte) |>
    left_join(keep_data, by = "USUBJID")

  # Validate result
  if (nrow(result) == 0) {
    warning("No data found after applying filters")
  }

  if ("PPCAT" %in% names(result)) {
    ppcats <- unique(result$PPCAT)
    if (length(ppcats) > 1) {
      stop(paste0(
        "Multiple PPCAT in result:\n", nice_enumeration(ppcats)
      ))
    }
  }
  result
}


#' PK parameter summary statistics by dose
#'
#' @param nca The NCA results as provided by `nca`, as data frame.
#' @param parameters The NCA parameters to be tabulated as character,
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
  parameters = c(
    "auclast", "cmax", "tmax", "half.life", "aucinf.obs",
    "AUCLST", "CMAX", "TMAX", "LAMZHL", "AUCIFP"
  ),
  group = NULL
) {
  out <- nca |>
    filter(.data$PPTESTCD %in% parameters)

  if ("exclude" %in% names(out))
    out <- filter(out, is.na(.data$exclude))

  if (!"PPORRES" %in% names(out) && "PPSTRESN" %in% names(out))
    out <- mutate(out, PPORRES = .data$PPSTRESN)

  out |>
    group_by_at(c(group, "DOSE", "PPTESTCD")) |>
    summarize(
      geomean = PKNCA::geomean(.data$PPORRES, na.rm = TRUE),
      geocv = PKNCA::geocv(.data$PPORRES, na.rm = TRUE),
      median = median(.data$PPORRES, na.rm = TRUE),
      iqr = IQR(.data$PPORRES, na.rm = TRUE),
      min = min(.data$PPORRES, na.rm = TRUE),
      max = max(.data$PPORRES, na.rm = TRUE),
      n = n()
    )
}


#' PK parameter summary statistics table by dose
#'
#' @param nca The NCA results as provided by `nca`, as data frame.
#' @param parameters The NCA parameters to be tabulated as character,
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
  parameters = c(
    "auclast", "cmax", "tmax", "half.life", "aucinf.obs",
    "AUCLST", "CMAX", "TMAX", "LAMZHL", "AUCIFP"
  ),
  digits = 2,
  group = NULL
) {
  s <- nca_summary(nca, parameters, group = group)

  median_parameters <- c(
    "tlast", "tmax", "lambda.z.n.points", "TMAX",
    "LAMZNPT"
  )
  s |>
    mutate(
      center = case_when(
        .data$PPTESTCD %in% median_parameters ~ as.character(round(.data$median,
          digits = digits
        )),
        .default = as.character(round(.data$geomean, digits = digits))
      ),
      dispersion = case_when(
        .data$PPTESTCD %in% median_parameters ~ paste0(
          as.character(round(.data$min, digits = digits)), "; ",
          as.character(round(.data$max, digits = digits))
        ),
        .default = as.character(round(.data$geocv))
      )
    ) |>
    mutate(value = paste0(.data$center, " (", .data$dispersion, ")")) |>
    tidyr::pivot_wider(
      id_cols = c(any_of(c(group, "DOSE")), "n"),
      names_from = "PPTESTCD", values_from = "value"
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
#' Hummel, 2009, <doi:10.1002/pst.326>.
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
  pp <- nca |>
    filter(.data$PPORRES != 0) |>
    dplyr::mutate(ldose = log(.data$DOSE), lpp = log(.data$PPORRES))

  power_model <- t(sapply(
    parameters,
    function(x) {
      as.numeric(
        stats::confint(
          stats::lm(
            data = (pp |> filter(.data$PPTESTCD == x)),
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
  power_model |>
    as.data.frame() |>
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
#' All zero values for the selected parameter are filtered out before analysis.
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
#' nca_power_model(
#'   nca(examplinib_sad_nif, analyte = "RS2023"),
#'   c("cmax", "aucinf.obs")
#' )
nca_power_model <- function(
  nca, parameter = NULL,
  group = NULL, title = NULL, size = 2, alpha = 1
) {
  if (is.null(parameter)) {
    std_parameters <- c("cmax", "aucinf.obs", "CMAX", "AUCIFP")
    parameter <- std_parameters[which(std_parameters %in% unique(nca$PPTESTCD))]
  }

  pm_plot <- function(param) {
    pp <- nca |>
      filter(.data$PPTESTCD == param)

    if (!"PPORRES" %in% names(pp) && "PPSTRESN" %in% names(pp))
      pp <- mutate(pp, PPORRES = .data$PPSTRESN)

    pp <- filter(pp, .data$PPORRES != 0)

    pm <- lm(log(PPORRES) ~ log(DOSE), data = pp)

    color_label <- nice_enumeration(unique(group))

    if (!is.null(group))
      pp <- tidyr::unite(pp, "COLOR", all_of(group), sep = "-", remove = FALSE)

    out <- pp |>
      bind_cols(predict.lm(pm, pp, interval = "prediction", se.fit = TRUE,
                           level = 0.9)$fit) |>
      ggplot2::ggplot(ggplot2::aes(x = .data$DOSE, y = exp(.data$fit))) +
      ggplot2::geom_line() +
      ggplot2::geom_ribbon(
        ggplot2::aes(
          x = .data$DOSE, ymin = exp(.data$lwr),
          ymax = exp(.data$upr)
        ),
        fill = "lightgrey", alpha = 0.5
      )

    if (!is.null(group)) {
      out <- out +
        geom_point(
          aes(x = .data$DOSE, y = .data$PPORRES, color = .data$COLOR),
          size = size, alpha = alpha
        )
    } else {
      out <- out +
        geom_point(ggplot2::aes(x = .data$DOSE, y = .data$PPORRES),
                   size = size, alpha = alpha)
    }

    out <- out +
      ggplot2::theme_bw() +
      ggplot2::expand_limits(x = 0) +
      ggplot2::labs(
        x = "dose (mg)", y = param,
        caption = paste0(
          "mean and 90% PI, slope = ",
          round(pm$coefficients[2], 3)
        )
      )

    if (!is.null(group))
      out <- out +
        ggplot2::labs(color = color_label)

    if (!is.null(title))
      out <- out + ggtitle(title)

    out + watermark(cex = 1.5)
  }

  out <- lapply(parameter, pm_plot)
  names(out) <- parameter
  out
}
