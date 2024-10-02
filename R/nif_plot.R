make_plot_data_set <- function(
    nif,
    analyte = NULL,
    dose = NULL,
    time = "TAFD",
    color = NULL,
    min_time = NULL,
    max_time = NULL,
    cfb = FALSE,
    dose_norm = FALSE,
    facet = "DOSE"
) {
  if (is.null(dose)){
    dose <- unique(nif$DOSE[nif$EVID == 0])}
  if(is.null(analyte)){
    analyte <- analytes(nif)}

  out <- nif %>%
    as.data.frame() %>%
    index_dosing_interval() %>%
    mutate(DI = case_match(EVID, 1 ~ NA, .default = DI)) %>%
    {if(cfb == TRUE) mutate(., DV = DVCFB) else .} %>%
    mutate(active_time = .data[[time]]) %>%
    filter(DOSE %in% dose) %>%
    filter(ANALYTE %in% analyte | EVID == 1) %>%
    {if(dose_norm == TRUE) mutate(., DV = DV/DOSE) else .} %>%
    {if(!is.null(min_time)) filter(., .data$active_time >= min_time) else .} %>%
    {if(!is.null(max_time)) filter(., .data$active_time <= max_time) else .} %>%
    group_by(ID, ANALYTE) %>%
    mutate(n_obs = sum(EVID == 0)) %>%
    ungroup() %>%
    as.data.frame()

  if(length(analyte) > 1) {
    color <- unique(c("ANALYTE", color))
  }

  out <- out %>%
    arrange("ID", "DOSE") %>%
    {if(length(color) != 0)
      unite(., COLOR, all_of(color), sep = "-", remove = FALSE) else
        mutate(., COLOR = TRUE)} %>%
    {if(length(facet) > 0)
      if(length(facet) == 1) mutate(., FACET = .data[[facet]]) else
        unite(., FACET, all_of(facet), sep = "-", remove = FALSE) else .} %>%
    arrange("ID", "COLOR", "DOSE", "FACET") #%>%

  return(list(data = out, group = "ID", color = color, facet = facet))
}


make_mean_plot_data_set <- function(data_set) {
  data <- data_set$data
  group <- data_set$group
  color <- data_set$color
  facet <- data_set$facet

  out <- data %>%
    # filter(EVID == 0) %>%
    mutate(active_time = NTIME) %>%
    select(-c(NTIME)) %>%
    reframe(ID=1, n = n(), mean = safe_mean(DV), sd = safe_sd(DV),
            .by = any_of(c("active_time", "COLOR", "FACET", "EVID"))) %>%
    rename(DV = mean)

  return(list(data = out, group = TRUE, color = color, facet = facet))
}



plot1.nif <- function(nif, analyte = NULL, dose = NULL, time = "TAFD",
                      color = NULL, min_time = NULL, max_time = NULL,
                      cfb = FALSE, dose_norm = FALSE, facet = "DOSE",
                      admin = NULL, points = FALSE, lines = TRUE,
                      log = FALSE, mean = FALSE, title = NULL){
  temp <- make_plot_data_set(
    nif, analyte, dose, time, color, min_time, max_time, cfb, dose_norm, facet)
  if(isTRUE(mean)) temp = make_mean_plot_data_set(temp)

  legend = FALSE

  admin_data <- temp$data %>%
    filter(EVID == 1)

  p <- temp$data %>%
    filter(EVID == 0) %>%
    bind_rows(
      admin_data %>%
        mutate(DV = NA)
    ) %>%
    arrange(FACET) %>%
    ggplot(aes(x = active_time,
               y = DV,
               group = interaction(ID, COLOR, FACET),
               color = COLOR)) +
    {if(!is.null(admin)) ggplot2::geom_vline(
      data = admin_data,
      ggplot2::aes(
        xintercept = .data$active_time),
      color = "gray")} +
    {if(isTRUE(lines)) geom_line()} +
    {if(isTRUE(points)) geom_point()} +
    {if(isTRUE(mean)) geom_ribbon(
      aes(ymin = pos_diff(DV, sd),
          ymax = DV + sd,
          fill = COLOR),
      alpha = 0.3, color = NA, show.legend = FALSE)} +
    {if(!is.null(temp$facet)) facet_wrap(~FACET)} +
    {if(isTRUE(log)) scale_y_log10()} +
    labs(color = nice_enumeration(temp$color)) +
    {if(is.null(temp$color)) theme(legend.position = "none")} +
    ggplot2::theme_bw() +
    ggplot2::theme(
      legend.position = ifelse(
        legend == TRUE,
        "bottom",
        "none")) +
    ggplot2::ggtitle(title) +
    watermark(cex = 1.5)

  suppressWarnings(print(p))
}



