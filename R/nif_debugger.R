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
    # filter(ANALYTE==analyte) %>%
    dplyr::distinct(DOSE) %>%
    dplyr::arrange(DOSE) %>%
    dplyr::pull(DOSE) %>%
    as.character()

  print(doses)

  max_time <- function(dose) {
    max(nif %>%
      filter(DOSE == dose) %>%
      # filter(ANALYTE == analyte) %>%
      filter(EVID == 0) %>%
      pull(TAD), na.rm = TRUE)
  }

  nif_debugger.ui <- shiny::fluidPage(
    title = "NIF debugger",
    shinyjs::useShinyjs(),
    shiny::fluidRow(
      style = "padding:10px; spacing:10",

      ## Column 1
      shiny::column(10,
        shiny::plotOutput("plot_pc"),
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
    )
  )


  nif_debugger.server <- function(input, output, session) {
    observeEvent(
      input$dose, {
        print(input$dose)
        temp <- max_time(as.numeric(input$dose))
        updateSliderInput(session, "min_x", max = temp, value = 0)
        updateSliderInput(session, "max_x", max = temp, value = temp)
      }
    )

    output$plot_pc <- shiny::renderPlot({
        # suppressWarnings(print(
          nif %>%
            filter(DOSE == input$dose) %>%
            nif_spaghetti_plot(
              tad = TRUE, points = TRUE, lines = FALSE, log = TRUE,
              point_size=3, min_x = input$min_x, max_x = input$max_x) +
            theme(legend.position = "none")
        # ))
        },
        height = 350)
  }

  shiny::shinyApp(nif_debugger.ui, nif_debugger.server)
}
