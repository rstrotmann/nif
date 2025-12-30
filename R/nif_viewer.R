#' NIF viewer
#'
#' This function creates an interactive viewer for NIF data sets.
#'
#' @param nif The NIF data set.
#' @return None
#' @rawNamespace import(shiny, except = c(dataTableOutput, renderDataTable))
#' @import dplyr
#' @importFrom shinyjs useShinyjs enable disable
#' @seealso [nif::plot()]
#' @export
#'
nif_viewer <- function(nif) {
  # Input validation
  if (!inherits(nif, "nif")) {
    stop("Input must be a nif object")
  }

  # Check for required columns
  required_cols <- c("ID", "TIME", "AMT", "DV", "EVID", "USUBJID", "ANALYTE", "PARENT")
  missing_cols <- setdiff(required_cols, names(nif))
  if (length(missing_cols) > 0) {
    stop("Missing required columns: ", paste(missing_cols, collapse = ", "))
  }

  # Validate data types
  if (!is.numeric(nif$TIME) || !is.numeric(nif$AMT) || !is.numeric(nif$DV)) {
    stop("TIME, AMT, and DV columns must be numeric")
  }

  if (!is.numeric(nif$EVID)) {
    stop("EVID column must be numeric")
  }

  # Check for empty dataset
  if (nrow(nif) == 0) {
    stop("nif dataset is empty")
  }

  # Check for invalid values
  if (any(is.na(nif$ID)) || any(is.na(nif$TIME)) || any(is.na(nif$EVID))) {
    warning("Dataset contains missing values in ID, TIME, or EVID columns")
  }

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
        shiny::actionButton("next.sb", "next") # ,
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
        shiny::numericInput("maxtime", "max display time", value = 24)
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
      tryCatch(
        {
          if (input$timeselect == "indiv") {
            return(nif %>%
              dplyr::filter(.data$USUBJID == input$subject) %>%
              dplyr::pull(.data$TIME) %>%
              max())
          } else if (input$timeselect == "global") {
            return(max(nif$TIME))
          } else if (input$timeselect == "custom") {
            if (is.na(input$maxtime) || input$maxtime <= 0) {
              stop("Custom max time must be a positive number")
            }
            return(input$maxtime)
          }
        },
        error = function(e) {
          shiny::showNotification(
            paste("Error calculating max time:", e$message),
            type = "error"
          )
          return(NA)
        }
      )
    }

    output$plot.pc <- shiny::renderPlot(
      {
        tryCatch(
          {
            y_scale_type <- ifelse(input$log_yscale, "log", "lin")
            suppressWarnings(print(
              nif::nif_plot_id(
                current_nif(),
                time_field = input$time,
                input$subject,
                analyte = input$analytes,
                max_time = max_time(),
                log = input$log_yscale,
                point_size = 3,
                imp = input$admin
              )
            ))
          },
          error = function(e) {
            shiny::showNotification(
              paste("Error creating concentration plot:", e$message),
              type = "error"
            )
            # Return empty plot with error message
            ggplot2::ggplot() +
              ggplot2::annotate("text",
                x = 0, y = 0,
                label = paste("Error:", e$message)
              ) +
              ggplot2::theme_void()
          }
        )
      },
      height = 350
    )

    output$plot.dose <- shiny::renderPlot(
      {
        tryCatch(
          {
            suppressWarnings(print(
              nif::dose_plot_id(current_nif(),
                input$subject,
                time_field = input$time,
                point_size = 3,
                max_dose = max_dose,
                max_time = max_time()
              )
            ))
          },
          error = function(e) {
            shiny::showNotification(
              paste("Error creating dose plot:", e$message),
              type = "error"
            )
            # Return empty plot with error message
            ggplot2::ggplot() +
              ggplot2::annotate("text",
                x = 0, y = 0,
                label = paste("Error:", e$message)
              ) +
              ggplot2::theme_void()
          }
        )
      },
      height = 250
    )

    shiny::observeEvent(input$prev.sb, {
      tryCatch(
        {
          current <- which(current_sbs() == input$subject)
          if (current > 0) {
            shiny::updateSelectInput(session, "subject",
              choices = current_sbs(),
              selected = current_sbs()[current - 1]
            )
          }
        },
        error = function(e) {
          shiny::showNotification(
            paste("Error navigating to previous subject:", e$message),
            type = "error"
          )
        }
      )
    })

    shiny::observeEvent(input$next.sb, {
      tryCatch(
        {
          current <- which(current_sbs() == input$subject)
          if (current < length(current_sbs())) {
            shiny::updateSelectInput(session, "subject",
              choices = current_sbs(),
              selected = current_sbs()[current + 1]
            )
          }
        },
        error = function(e) {
          shiny::showNotification(
            paste("Error navigating to next subject:", e$message),
            type = "error"
          )
        }
      )
    })

    shiny::observeEvent(input$timeselect, {
      tryCatch(
        {
          if (input$timeselect != "custom") {
            shinyjs::disable("maxtime")
          } else {
            shinyjs::enable("maxtime")
          }
        },
        error = function(e) {
          shiny::showNotification(
            paste("Error updating time selection:", e$message),
            type = "error"
          )
        }
      )
    })

    shiny::observeEvent(input$dose, {
      tryCatch(
        {
          if (input$dose != "all") {
            filtered_nif <- nif %>%
              filter(.data$DOSE == as.numeric(input$dose))

            if (nrow(filtered_nif) == 0) {
              stop("No data available for selected dose")
            }

            current_nif(filtered_nif)
          } else {
            current_nif(nif)
          }

          current_sbs(current_nif() %>%
            distinct(.data$USUBJID) %>%
            pull(.data$USUBJID))

          if (length(current_sbs()) == 0) {
            stop("No subjects available for selected dose")
          }

          updateSelectInput(
            session, "subject",
            choices = current_sbs(), selected = current_sbs()[1]
          )
        },
        error = function(e) {
          shiny::showNotification(
            paste("Error updating dose filter:", e$message),
            type = "error"
          )
        }
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
