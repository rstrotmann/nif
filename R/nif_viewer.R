#' NIF viewer
#'
#' This function creates an interactive viewer for NIF data sets.
#'
#' @param nif The NIF data set.
#' @return None
#' @rawNamespace import(shiny, except = c(dataTableOutput, renderDataTable))
#' @import dplyr
#' @importFrom shinyjs useShinyjs enable disable
#' @seealso [plot()]
#' @export
#'
nif_viewer <- function(nif) {
  sbs <- nif %>%
    dplyr::distinct(.data$USUBJID) %>%
    dplyr::pull(.data$USUBJID)

  doses <- nif %>%
    dplyr::distinct(.data$AMT) %>%
    dplyr::arrange(.data$AMT) %>%
    dplyr::pull(.data$AMT) %>%
    as.character()

  analytes <- nif %>%
    as.data.frame() %>%
    dplyr::distinct(.data$ANALYTE) %>%
    dplyr::pull(.data$ANALYTE)

  imps <- nif %>%
    as.data.frame() %>%
    distinct(.data$PARENT) %>%
    pull(.data$PARENT)

  max_dose <- nif %>%
    dplyr::pull(.data$AMT) %>%
    max()

  nif_viewer.ui <- shiny::fluidPage(
    title = "NIF viewer",
    shinyjs::useShinyjs(),
    shiny::fluidRow(
      style = "padding:10px; spacing:10",

      ## Column 1
      shiny::column(
        3,
        shiny::selectInput("subject",
          label = "subject", choices = sbs,
          selected = sbs[1]
        ),
        shiny::actionButton("prev.sb", "previous"),
        shiny::actionButton("next.sb", "next") #,
        # shiny::textInput("search", "", placeholder = "Search subject")
      ),

      # Column 2
      shiny::column(
        2,
        shiny::radioButtons(
          "timeselect",
          "time axis limit",
          choices = c(
            "individual max" = "indiv",
            "global max" = "global",
            "custom" = "custom"
          )
        ),
        #checkboxInput(" %>% ", "TAD"),
        shiny::numericInput("maxtime", "max display time", value = NA)
      ),

      # Column 3
      shiny::column(
        2,
        shiny::radioButtons(
          "time",
          "time axis",
          choices = c(
            "TIME", "TAFD", "TAD"
          )
        )
      ),

      # Column 4
      shiny::column(
        2,
        shiny::checkboxGroupInput("analytes", "analyte filter",
          choices = analytes, selected = analytes
        ),
        checkboxInput("log_yscale", "log y-scale")
      ),

      # Column 5
      shiny::column(
        2,
        shiny::selectInput("dose",
          label = "dose filter",
          choices = c("all", doses), selected = "all"
        ),
        shiny::selectInput("admin",
          label = "administrations",
          choices = c(imps, "none"), selected = imps[1]
        )
      ),
    ),
    hr(),
    shiny::plotOutput("plot.pc"),
    shiny::plotOutput("plot.dose"),
    tags$style(
      type = "text/css",
      ".shiny-output-error { visibility: hidden; }",
      ".shiny-output-error:before { visibility: hidden; }"
    )
  )



  nif_viewer.server <- function(input, output, session) {
    current_nif <- reactiveVal(nif)
    current_sbs <- reactiveVal(sbs)
    current_analytes <- reactiveVal(analytes)

    max_time <- function() {
      if (input$timeselect == "indiv") {
        return(nif %>%
          dplyr::filter(.data$USUBJID == input$subject) %>%
          dplyr::pull(.data$TIME) %>%
          max())
      } else if (input$timeselect == "global") {
        return(max(nif$TIME))
      } else if (input$timeselect == "custom") {
        return(input$maxtime)
      }
    }

    output$plot.pc <- shiny::renderPlot(
      {
        y_scale_type <- ifelse(input$log_yscale, "log", "lin")
        suppressWarnings(print(
          nif::nif_plot_id(
            current_nif(), # %>%
            # filter(ANALYTE %in% input$analytes),
            time_field = input$time,
            input$subject,
            analyte = input$analytes, # %>%
            max_time = max_time(),
            # y_scale = y_scale_type,
            log = input$log_yscale,
            # tad = input$tad,
            # tad = input$time == "TAD",
            # lines = !input$tad,
            point_size = 3,
            imp = input$admin
          )
        ))
      },
      height = 350
    )

    output$plot.dose <- shiny::renderPlot(
      {
        suppressWarnings(print(
          nif::dose_plot_id(current_nif(),
                            input$subject,
                            time_field = input$time,
                            point_size = 3,
                            max_dose = max_dose,
                            max_time = max_time())
        ))
      },
      height = 250
    )

    shiny::observeEvent(input$prev.sb, {
      current <- which(current_sbs() == input$subject)
      if (current > 0) {
        shiny::updateSelectInput(session, "subject",
          choices = current_sbs(),
          selected = current_sbs()[current - 1]
        )
      }
    })

    shiny::observeEvent(input$next.sb, {
      current <- which(current_sbs() == input$subject)
      if (current < length(current_sbs())) {
        shiny::updateSelectInput(session, "subject",
          choices = current_sbs(),
          selected = current_sbs()[current + 1]
        )
      }
    })

    shiny::observeEvent(input$timeselect, {
      if (input$timeselect != "custom") {
        shinyjs::disable("maxtime")
      } else {
        shinyjs::enable("maxtime")
      }
    })

    shiny::observeEvent(input$dose, {
      if (input$dose != "all") {
        current_nif(
          nif %>%
            filter(DOSE == as.numeric(input$dose))
        )
      } else {
        current_nif(nif)
      }

      current_sbs(current_nif() %>%
        distinct(.data$USUBJID) %>%
        pull(.data$USUBJID))

      updateSelectInput(
        session, "subject",
        choices = current_sbs(), selected = current_sbs()[1]
      )
    })

    # shiny::observeEvent(input$search, {
    #   # id <- if(nchar(input$search)) {
    #   #   as.numeric(input$search)
    #   # }
    #   # if(id %in% input$subject) {message(id)}
    #   # shiny::updateSelectInput(session, "subject",
    #   #                          choices = current_sbs(),
    #   #                          selected = current_sbs()[current - 1]
    #   # )
    # })
  }

  shiny::shinyApp(nif_viewer.ui, nif_viewer.server)
}
