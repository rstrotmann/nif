#' Title
#'
#' @param nif A NIF data set.
#'
#' @return Nothing
#' @import DT
#' @export
nif_debugger <- function(nif, sdtm, analyte = NULL) {
  if(is.null(analyte)) {
    analyte = guess_analyte(nif)
  }

  nif <- nif %>%
    as.data.frame() %>%
    filter(ANALYTE == analyte)

  doses <- nif %>%
    dplyr::distinct(DOSE) %>%
    dplyr::arrange(DOSE) %>%
    dplyr::pull(DOSE) %>%
    as.character()

  print(doses)

  max_time <- function(dose) {
    max(nif %>%
      filter(DOSE == dose) %>%
      filter(EVID == 0) %>%
      pull(TAD), na.rm = TRUE)
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
        fluidRow(
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
      DTOutput("nif_table"),
      DTOutput("ex_table"),
      DTOutput("pc_table")
    )
  )


  ## server
  nif_debugger.server <- function(input, output, session) {
    id <- reactiveVal(nif %>% filter(row_number()==1))
    # id <- NULL

    observeEvent(
      input$dose, {
        # print(input$dose)
        temp <- max_time(as.numeric(input$dose))
        updateSliderInput(session, "min_x", max = temp, value = 0)
        updateSliderInput(session, "max_x", max = temp, value = temp)
      }
    )

    output$plot_nif <- shiny::renderPlot({
      nif %>%
        filter(DOSE == input$dose) %>%
        filter(TAD >= input$min_x, TAD <= input$max_x) %>%
        ggplot(aes(x=TAD, y=DV, color=!(REF %in% (id() %>% pull(REF))))) +
        geom_point(size=4, alpha=.8) +
        scale_y_log10() +
        theme_bw() +
        theme(legend.position = "none")},
        height = 350)

    output$ex_table <- renderDT(
        sdtm$ex %>%
          filter(USUBJID %in% (id() %>% pull(USUBJID))) %>%
          filter(EXSEQ %in% (id() %>% pull(EXSEQ))) %>%
          filter(EXTRT %in% (id() %>% pull(EXTRT))),
        options = list(dom = '')
    )

    output$nif_table <- renderDT(
      id() %>%
        mutate(across(where(is.numeric), signif, 2)),
      options = list(dom = '')
    )

    output$pc_table <- renderDT(
        sdtm$pc %>%
        filter(PCREFID %in% (id() %>% pull(PCREFID))),
        options = list(dom = '')
    )

    observeEvent(input$plot_nif_click, {
      temp <- nearPoints(nif, input$plot_nif_click, addDist=T)
      # id(as.data.frame(temp %>% select(USUBJID, PCREFID, EXSEQ)))
      id(as.data.frame(temp))
    })
  }

  shiny::shinyApp(nif_debugger.ui, nif_debugger.server)
}
