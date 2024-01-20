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
#' @import shiny
#' @import DT
#' @import dplyr
#' @importFrom shinyjs useShinyjs enable disable
#' @export
nif_debugger <- function(nif_data, sdtm_data, analyte = NULL, usubjid = NULL) {
  if(is.null(analyte)) {
    analyte = guess_analyte(nif_data)
  }

  extrt <- sdtm_data$analyte_mapping %>%
    filter(PCTESTCD %in% analyte) %>%
    pull(EXTRT)

  if(is.null(usubjid)) {
    usubjid <- nif_data %>%
      subjects() %>%
      pull(USUBJID)
  }

  nif <- nif_data %>%
    index_nif() %>%
    as.data.frame() %>%
    select(-any_of(c("EXTRT"))) %>%
    filter(ANALYTE %in% analyte) %>%
    filter(USUBJID %in% usubjid) %>%
    mutate(admin_REF = case_when(EVID==1 ~ REF)) %>%
    group_by(ID, PARENT) %>%
    fill(admin_REF, .direction = "down") %>%
    ungroup() %>%
    left_join(sdtm_data$analyte_mapping, by=c("ANALYTE"="PCTESTCD")) %>%
    filter(!(is.na(DV) & EVID == 0))

  doses <- nif %>%
    dplyr::distinct(DOSE) %>%
    dplyr::arrange(DOSE) %>%
    dplyr::pull(DOSE) %>%
    as.character()


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
      # shiny::tableOutput("nif_table"),
      # shiny::tableOutput("ex_table"),
      # shiny::tableOutput("pc_table")
    )
  )


  ## server
  nif_debugger.server <- function(input, output, session) {
    id <- shiny::reactiveVal(nif %>%
                        filter(EVID==0) %>%
                        filter(row_number()==1))

    selected_ref <- shiny::reactiveVal()

    observeEvent(
      input$dose, {
        temp <- max_time(as.numeric(input$dose))
        updateSliderInput(session, "min_x", max = temp, value = 0)
        updateSliderInput(session, "max_x", max = temp, value = temp)
      }
    )

    output$plot_nif <- shiny::renderPlot({
      nif %>%
        filter(DOSE == input$dose) %>%
        filter(EVID == 0) %>%
        filter(TAD >= input$min_x, TAD <= input$max_x) %>%
        ggplot(aes(x=TAD, y=DV, color=!(REF %in% (id() %>% pull(REF))))) +
        geom_point(size=4, alpha=.6) +
        scale_y_log10() +
        theme_bw() +
        ggtitle(paste0("Analyte: ", analyte)) +
        theme(legend.position = "none")},
        height = 350)

    ## NIF table output
    output$nif_table <- DT::renderDT(
    # output$nif_table <- shiny::renderDataTable(
      nif %>%
        mutate_at(c("TIME", "TAD", "LNDV"), round, 2) %>%
        filter(
          REF %in% selected_ref() |
          REF %in% (nif %>% filter(REF %in% selected_ref()) %>% pull(admin_REF))),
      options = list(dom = '')
    )

    ## EX table output
    output$ex_table <- DT::renderDT(
    # output$ex_table <- shiny::renderDataTable(
        sdtm_data$ex %>%
          filter(USUBJID %in% (id() %>% pull(USUBJID))) %>%
          filter(EXSEQ %in% (id() %>% pull(EXSEQ))) %>%
          filter(EXTRT %in% (id() %>% pull(EXTRT))),
        options = list(dom = "")
    )

    ## PC table output
    output$pc_table <- DT::renderDT(
    # output$pc_table <- shiny::renderDataTable(
        sdtm_data$pc %>%
          filter(PCTESTCD %in% analyte) %>%
          filter(PCREFID %in% (id() %>%pull(PCREFID))),
        options = list(dom = '')
    )

    shiny::observeEvent(input$plot_nif_click, {
      temp <- nearPoints(nif, input$plot_nif_click, addDist=T)
      id(as.data.frame(temp))
      selected_ref(temp$REF)
    })
  }

  shiny::shinyApp(nif_debugger.ui, nif_debugger.server)
}
