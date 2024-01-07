#' Title
#'
#' @param nif A NIF data set.
#'
#' @return Nothing
#' @export
nif_debugger <- function(nif, analyte = NULL) {
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
      dataTableOutput("pc_tqble")
    )
  )


  ## server
  nif_debugger.server <- function(input, output, session) {
    id <- reactiveVal()
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
        # suppressWarnings(print(
          nif %>%
            filter(DOSE == input$dose) %>%
            # nif_spaghetti_plot(
            #   tad = TRUE, points = TRUE, lines = FALSE, log = TRUE,
            #   point_size=4, min_x = input$min_x, max_x = input$max_x) +
            # theme(legend.position = "none")
            filter(TAD >= input$min_x, TAD <= input$max_x) %>%
            ggplot(aes(x=TAD, y=DV)) +
            geom_point(size=4, alpha=.3) +
            scale_y_log10() +
            theme_bw()
        # ))
        },
        height = 350)

    output$pc_tqble <- renderDataTable(id())

    observeEvent(input$plot_nif_click, {
      temp <- nearPoints(nif, input$plot_nif_click, addDist=T)
      id(temp %>% select(USUBJID, PCREFID, EXSEQ))
      # print(id$PCREFID)
      # output$pc_table <- renderTable(as.data.frame(id))
    })
  }

  shiny::shinyApp(nif_debugger.ui, nif_debugger.server)
}
