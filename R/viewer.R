
#' NIF viewer
#'
#' This function creates an interactive viewer for NIF data sets.
#'
#' @param nif The NIF data set.
#' @return None
#' @import shiny
#' @import dplyr
#' @importFrom shinyjs useShinyjs enable disable
#' @seealso
#' @seealso [plot()]
#' @export
nif_viewer <- function(nif) {
  sbs <- nif %>%
    dplyr::distinct(USUBJID) %>%
    dplyr::pull(USUBJID)

  max.dose <- nif %>%
    dplyr::pull(AMT) %>%
    max()

  nif_viewer.ui <- shiny::fluidPage(
    title="NIF viewer",
    shinyjs::useShinyjs(),
    shiny::fluidRow(
      style="padding:10px; spacing:10",
      shiny::column(3,
        shiny::selectInput("subject", label="subject", choices=sbs),
        shiny::actionButton("prev.sb", "previous"),
        shiny::actionButton("next.sb", "next")
      ),

      shiny::column(2,
        shiny::radioButtons(
          "timeselect",
          "time axis limit",
          choices= c("Individual max" = "indiv",
                     "Global max" = "global",
                     "Custom" = "custom"))
      ),

      shiny::column(2,
        shiny::numericInput("maxtime", "max display time", value=NA)),

      shiny::column(4, checkboxInput("log_yscale", "log y-scale"))
    ),
    hr(),
    shiny::plotOutput("plot.pc"),
    shiny::plotOutput("plot.dose"),
    tags$style(type="text/css",
      ".shiny-output-error { visibility: hidden; }",
      ".shiny-output-error:before { visibility: hidden; }"
    )
  )


  nif_viewer.server <- function(input, output, session) {
    max.time <- function() {
      if(input$timeselect=="indiv") {
        return(nif %>%
          dplyr::filter(USUBJID==input$subject) %>%
          dplyr::pull(TIME) %>%
          max())
      }
      else if(input$timeselect=="global"){
        return(max(nif$TIME))
      }
      else if(input$timeselect=="custom"){
        return(input$maxtime)
      }
    }

    output$plot.pc <- shiny::renderPlot({
      y_scale_type <- ifelse(input$log_yscale, "log", "lin")
      suppressWarnings(print(
        nif::nif_plot_id(nif, input$subject, max.time=max.time(), y.scale=y_scale_type)))
    }, height=350)

    output$plot.dose <- shiny::renderPlot({
      suppressWarnings(print(
        nif::dose_plot_id(nif, input$subject, max.dose=max.dose, max.time=max.time())))
    }, height=250)

    shiny::observeEvent(input$prev.sb, {
      current <- which(sbs==input$subject)
      if(current > 0) {
        shiny::updateSelectInput(session, "subject", choices=sbs, selected=sbs[current-1])
      }
    })

    shiny::observeEvent(input$next.sb, {
      current <- which(sbs==input$subject)
      if(current < length(sbs)) {
        shiny::updateSelectInput(session, "subject", choices=sbs, selected=sbs[current+1])
      }
    })

    shiny::observeEvent(input$timeselect, {
      if(input$timeselect != "custom") {
        shinyjs::disable("maxtime")
      } else {
        shinyjs::enable("maxtime")
      }
    })
  }

  shiny::shinyApp(nif_viewer.ui, nif_viewer.server)
}


