#' NIF file debugger
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
#' @return Nothing
#' @rawNamespace import(shiny, except = c(dataTableOutput, renderDataTable))
#' @import DT
#' @import dplyr
#' @importFrom shinyjs useShinyjs enable disable
#' @export
nif_debugger <- function(nif_data, sdtm_data, analyte = NULL, usubjid = NULL) {
  if(is.null(analyte)) {
    analyte = guess_analyte(nif_data)
  }

  extrt <- sdtm_data$analyte_mapping %>%
    filter(.data$PCTESTCD %in% analyte) %>%
    pull(.data$EXTRT)

  if(is.null(usubjid)) {
    usubjid <- nif_data %>%
      subjects() %>%
      pull(.data$USUBJID)
  }

  nif <- nif_data %>%
    ensure_tad() %>%
    index_nif() %>%
    as.data.frame() %>%
    select(-any_of(c("EXTRT"))) %>%
    filter(.data$ANALYTE %in% analyte) %>%
    filter(.data$USUBJID %in% usubjid) %>%
    mutate(admin_REF = case_when(.data$EVID==1 ~ .data$REF)) %>%
    group_by(.data$ID, .data$PARENT) %>%
    tidyr::fill(admin_REF, .direction = "down") %>%
    ungroup() %>%
    left_join(sdtm_data$analyte_mapping, by=c("ANALYTE"="PCTESTCD")) %>%
    filter(!(is.na(.data$DV) & EVID == 0))


  # doses <- nif %>%
  #   dplyr::distinct(.data$DOSE) %>%
  #   dplyr::arrange(.data$DOSE) %>%
  #   dplyr::pull(.data$DOSE) %>%
  #   as.character()

  doses <- as.character(sort(unique(nif$DOSE)))

  max_time <- function(dose) {
    max(nif[which(nif$DOSE==dose), "TAD"], na.rm=T)
  }

  ## user interface
  nif_debugger.ui <- shiny::fluidPage(
    title = "NIF debugger",
    shinyjs::useShinyjs(),
    shiny::fluidRow(
      style = "padding:10px; spacing:10",

      ## Column 1
      shiny::column(10,
        shiny::plotOutput("plot_nif", click = "plot_nif_click"),
        shiny::fluidRow(
          shiny::column(5, shiny::sliderInput(
            "min_x", "min time", min = 0, max = 100, value = 0)),
          shiny::column(5, shiny::sliderInput(
            "max_x", "max time", min = 0, max = 100, value = 100))
      )),

      ## Column 2
      shiny::column(2,
        shiny::selectInput(
        "dose", label = "dose filter",
        choices = doses, selected = doses[[1]]))
    ),
    shiny::fluidRow(
      style = "padding:10px; spacing:10",
      DT::DTOutput("nif_table"),
      DT::DTOutput("ex_table"),
      DT::DTOutput("pc_table")

      # DT::dataTableOutput("nif_table"),
      # DT::dataTableOutput("ex_table"),
      # DT::dataTableOutput("pc_table")
    )
  )


  ## server
  nif_debugger.server <- function(input, output, session) {
    id <- shiny::reactiveVal(nif %>%
                        filter(EVID == 0) %>%
                        filter(row_number() == 1))

    selected_ref <- shiny::reactiveVal()

    # observeEvent(
    #   input$dose, {
    #     temp <- max_time(as.numeric(input$dose))
    #     updateSliderInput(session, "min_x", max = temp, value = 0)
    #     updateSliderInput(session, "max_x", max = temp, value = temp)
    #   }
    # )

    output$plot_nif <- shiny::renderPlot({
      nif %>%
        filter(DOSE == input$dose) %>%
        filter(EVID == 0) %>%
        # filter(TAD >= input$min_x, TAD <= input$max_x) %>%
        # ggplot(aes(x = TAD, y = DV, color=!(REF %in% (id() %>% pull(REF))))) +
        ggplot2::ggplot(ggplot2::aes(
          x = TIME,
          y = DV,
          color=!(REF %in% (id() %>% pull(REF))))) +
        ggplot2::geom_point(size=4, alpha=.6) +
        ggplot2::scale_y_log10() +
        ggplot2::theme_bw() +
        ggplot2::ggtitle(paste0("Analyte: ", analyte)) +
        ggplot2::theme(legend.position = "none")},
        height = 350)

    ## NIF table output
    # output$nif_table <- DT::renderDT(
    # # output$nif_table <- DT::renderDataTable(
    #   nif %>%
    #     mutate_at(c("TIME", "TAD", "LNDV"), round, 2) %>%
    #     filter(
    #       REF %in% selected_ref() |
    #       REF %in% (nif %>% filter(REF %in% selected_ref()) %>% pull(admin_REF))),
    #   options = list(dom = '')
    # )

    ## EX table output
    # output$ex_table <- DT::renderDT(
    # # output$ex_table <- DT::renderDataTable(
    #     sdtm_data$ex %>%
    #       filter(.data$USUBJID %in% (id() %>% pull(.data$USUBJID))) %>%
    #       filter(.data$EXSEQ %in% (id() %>% pull(.data$EXSEQ))) %>%
    #       filter(.data$EXTRT %in% (id() %>% pull(.data$EXTRT))),
    #     options = list(dom = "")
    # )

    ## PC table output
    # output$pc_table <- DT::renderDT(
    # # output$pc_table <- DT::renderDataTable(
    #     sdtm_data$pc %>%
    #       filter(PCTESTCD %in% analyte) %>%
    #       filter(PCREFID %in% (id() %>%pull(PCREFID))),
    #     options = list(dom = '')
    # )

    # shiny::observeEvent(input$plot_nif_click, {
    #   temp <- nearPoints(nif, input$plot_nif_click, addDist=T)
    #   id(as.data.frame(temp))
    #   selected_ref(temp$REF)
    # })
  }

  shiny::shinyApp(nif_debugger.ui, nif_debugger.server)
}
