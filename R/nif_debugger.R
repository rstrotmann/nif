#' NIF file debugger
#'
#' @description
#' `r lifecycle::badge("experimental")`
#'
#' This shiny app helps identifying suspicious data in the NIF data set. This
#' may be due to missing data in the underlying SDTM data set or bad imputations
#' during the generation of the NIF data set.
#'
#' @param nif_data A NIF data set.
#' @param sdtm_data The underlying SDTM data set.
#' @param analyte The analyte as character. If NULL (default), the most likely
#'   analyte will be selected.
#' @param usubjid The subject USUBJID to filter for. If NULL (default), all
#'   subjects are shown.
#' @param extrt The EXTRT field.
#' @param pctestcd The PCTESTCD field.
#' @return Nothing
#' @rawNamespace import(shiny, except = c(dataTableOutput, renderDataTable))
#' @import DT
#' @import dplyr
#' @import htmltools
#' @importFrom shinyjs useShinyjs enable disable
#' @export
nif_debugger <- function(
    nif_data, sdtm_data,
    analyte = NULL, extrt = NULL, pctestcd = NULL, usubjid = NULL) {
  if(is.null(usubjid)) usubjid <- unique(nif_data$USUBJID)

  if(is.null(analyte)) analyte = guess_analyte(nif_data)

  if(is.null(extrt)) {
    extrt <- analyte
    if(!extrt %in% unique(domain(sdtm_data, "ex")$EXTRT))
      stop(paste0(extrt, " not found in EXTRT!"))
  }

  if(is.null(pctestcd)) {
    pctestcd <- analyte
    if(!pctestcd %in% unique(domain(sdtm_data, "pc")$PCTESTCD))
      stop(paste0(pctestcd, " not found in PCTESTCD!"))
  }

  nif <- nif_data %>%
    ensure_time() %>%
    index_nif() %>%
    as.data.frame() %>%
    select(-any_of(c("EXTRT"))) %>%
    filter(.data$ANALYTE %in% analyte) %>%
    filter(.data$USUBJID %in% usubjid) %>%
    mutate(admin_REF = case_when(.data$EVID == 1 ~ .data$REF)) %>%
    mutate(admin_SEQ = .data$SRC_SEQ[REF == admin_REF]) %>%
    group_by(.data$ID, .data$PARENT) %>%
    tidyr::fill(.data$admin_REF, .direction = "down") %>%
    tidyr::fill(.data$admin_SEQ, .direction = "down") %>%
    ungroup() %>%
    mutate(admin_id = interaction(
      .data$USUBJID, .data$PARENT, .data$admin_SEQ)) %>%
    mutate(obs_id = interaction(
      .data$USUBJID, .data$ANALYTE, .data$SRC_DOMAIN, .data$SRC_SEQ)) %>%
    filter(!(is.na(.data$DV) & .data$EVID == 0)) %>%
    mutate(display_time = .data$TIME) %>%
    as.data.frame()

  doses <- as.character(sort(unique(nif$DOSE)))

  max_time <- function(dose) {
    max(nif[which(nif$DOSE==dose), "TAD"], na.rm=T)
  }

  ## user interface
  deb.ui <- shiny::fluidPage(
    # shinyjs::useShinyjs(),

    # Row 1
    shiny::fluidRow(
      # Column 1
      shiny::column(10,
        shiny::plotOutput("plot_nif", click = "plot_nif_click")
      ),

      # Column 2
      shiny::column(2,
        shiny::fluidRow(
          shiny::selectInput(
            "dose", label = "dose filter",
            choices = doses, selected = doses[[1]])
        ),

        shiny::fluidRow(
          shiny::selectInput(
            "time", label = "time field",
            choices = c("TIME", "TAFD", "TAD", "NTIME"),
            selected = "TIME"
          )
        )
      )
    ),

    shiny::fluidRow(
      style = "padding:10px;",
      DT::DTOutput("nif_table"),
      DT::DTOutput("ex_table"),
      DT::DTOutput("pc_table")
    )

  )


  ## server
  deb.server <- function(input, output, session) {
    selection <- shiny::reactiveVal(NULL)
    current_nif <- shiny::reactiveVal(NULL)
    current_pc <- shiny::reactiveVal(NULL)
    current_ex <- shiny::reactiveVal(NULL)

    # Plot
    output$plot_nif <- shiny::renderPlot({
      nif %>%
        filter(DOSE == input$dose) %>%
        filter(EVID == 0) %>%
        ggplot2::ggplot(ggplot2::aes(
          x = TAD, y = DV, color = !(REF %in% selection()$REF))) +
        ggplot2::geom_point(size = 4, alpha = .6) +
        ggplot2::scale_y_log10() +
        ggplot2::theme_bw() +
        ggplot2::theme(legend.position = "none")},
      height = 350)

    ## NIF table output
    output$nif_table <- DT::renderDT(
      current_nif(),
      options = list(dom = ''),
      caption = htmltools::tags$caption(
        "NIF data",
        style = "color:black; background-color:#e0e0e0; font-size:120%;
        font-weight:bold; padding:5px;")
    )

    ## EX table output
    output$ex_table <- DT::renderDT(
      current_ex(),
      options = list(dom = ""),
      caption = htmltools::tags$caption(
        "EX domain",
        style = "color:black; background-color:#e0e0e0; font-size:120%;
        font-weight:bold; padding:5px; margin-top:20px")
    )

    ## PC table output
    output$pc_table <- DT::renderDT(
      current_pc(),
      options = list(dom = ""),
      caption = htmltools::tags$caption(
        "PC domain",
        style = "color:black; background-color:#e0e0e0; font-size:120%;
        font-weight:bold; padding:5px; margin-top:20px")
    )

    # shiny::observeEvent(input$time, {
    #   active_nif(
    #     active_nif %>%
    #       mutate(active_time = .data[[input$time]])
    #   )
    # })

    shiny::observeEvent(input$plot_nif_click, {
      # plot selection
      selection(
        as.data.frame(
          nearPoints(nif, input$plot_nif_click, addDist = T)) %>%
        filter(DOSE %in% input$dose)
      )

      ## current nif
      current_nif(
        nif %>%
        # active_nif() %>%
          filter(DOSE %in% input$dose) %>%
          mutate(across(c("TAFD", "TAD", "TIME"), round, 1)) %>%
          mutate(across(c("DV"), signif, 3)) %>%
          select(any_of(c(
            "REF", "ID", "USUBJID", "DTC", "TIME", "TAFD", "TAD", "NTIME", "AMT",
            "DOSE", "ANALYTE", "EVID", "DV", "SRC_DOMAIN", "SRC_SEQ"))) %>%
          filter(
            (REF %in% selection()$REF & EVID == 0) |
              (REF %in% (filter(nif, .data$REF %in% selection())$admin_REF) & EVID == 1)
          )
      )

      ## current pc
      current_pc(
        domain(sdtm_data, "pc") %>%
          mutate(obs_id = interaction(.data$USUBJID, pctestcd, "PC", .data$PCSEQ)) %>%
          filter(.data$obs_id %in% selection()$obs_id)
      )

      ## current ex
      current_ex(
        domain(sdtm_data, "ex") %>%
          mutate(admin_id = interaction(
            .data$USUBJID, .data$EXTRT, .data$EXSEQ)) %>%
          filter(admin_id %in% unique(selection()$admin_id))
      )
    })
  }

  shiny::shinyApp(deb.ui, deb.server)
}

