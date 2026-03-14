#' Interactive debug plot for NIF objects
#'
#' Launches a Shiny app that renders an interactive version of [nif::plot.nif()].
#' Data points are clickable; clicking a point displays the corresponding
#' source SDTM record (identified via `SRC_DOMAIN` and `SRC_SEQ`) in a table
#' below the plot.
#'
#' The NIF object must contain `SRC_DOMAIN` and `SRC_SEQ` columns. These are
#' present when the NIF is built with `debug = TRUE` or when the global
#' `nif::nif_option("debug")` is set.
#'
#' @param nif A nif object containing `SRC_DOMAIN` and `SRC_SEQ` columns.
#' @param sdtm A sdtm object used to look up source records.
#' @param analyte The analyte(s) to be plotted, as character.
#' @param dose The dose(s) to be filtered for.
#' @param time The time field for the x-axis. One of 'TIME', 'NTIME', 'TAFD'
#'   or 'TAD'.
#' @param color The column(s) to be used for coloring.
#' @param facet The column(s) to be used for faceting.
#' @param min_time The minimal time, as numeric.
#' @param max_time The maximal time, as numeric.
#' @param cfb Plot change from baseline, as logical.
#' @param dose_norm Dose-normalized values, as logical.
#' @param log Logarithmic y axis, as logical.
#' @param lines Plot lines, as logical.
#' @param size The `size` parameter to [ggplot2::geom_point()], as numeric.
#' @param alpha The `alpha` parameter to [ggplot2::geom_point()], as numeric.
#' @param scales The `scales` parameter to [ggplot2::facet_wrap()].
#'
#' @return A Shiny app object.
#' @export
#' @seealso [nif::plot.nif()], [nif::nif_viewer()]
#'
#' @examples
#' \dontrun{
#' debug_plot(examplinib_sad_nif, examplinib_sad)
#' }
debug_plot <- function(
  nif,
  sdtm,
  analyte = NULL,
  dose = NULL,
  time = "TAFD",
  color = NULL,
  facet = "DOSE",
  min_time = NULL,
  max_time = NULL,
  cfb = FALSE,
  dose_norm = FALSE,
  log = FALSE,
  lines = TRUE,
  size = 2,
  alpha = 1,
  scales = "fixed"
) {
  if (!inherits(nif, "nif")) {
    stop("'nif' must be a nif object")
  }
  if (!inherits(sdtm, "sdtm")) {
    stop("'sdtm' must be a sdtm object")
  }
  if (!all(c("SRC_DOMAIN", "SRC_SEQ") %in% names(nif))) {
    stop(
      "NIF object must contain SRC_DOMAIN and SRC_SEQ columns. ",
      "Rebuild with debug = TRUE."
    )
  }

  plot_data_set <- make_plot_data_set(
    nif, analyte, dose, time, color, min_time, max_time, cfb, dose_norm, facet
  )

  plot_data <- plot_data_set$data

  if (isTRUE(log)) {
    plot_data <- dplyr::mutate(
      plot_data, DV = dplyr::case_when(.data$DV == 0 ~ NA, .default = .data$DV)
    )
  }

  plot_data <- plot_data |>
    tidyr::unite("GROUP", dplyr::any_of(
      c(plot_data_set$group, plot_data_set$color, plot_data_set$facet)),
      sep = "-", remove = FALSE
    )

  obs_data <- plot_data |>
    dplyr::filter(.data$EVID == 0) |>
    dplyr::filter(!is.na(.data$DV))

  analyte_values <- unique(obs_data$ANALYTE)
  y_label <- ifelse(length(analyte_values) == 1, analyte_values, "DV")
  if (isTRUE(dose_norm)) y_label <- paste0(y_label, " / DOSE")

  plot_title <- nice_enumeration(analyte_values)
  if (isTRUE(cfb)) plot_title <- paste0(plot_title, " change from baseline")
  if ("FACET" %in% names(obs_data) && length(unique(obs_data$FACET)) > 1) {
    plot_title <- paste0(plot_title, " by ", plot_data_set$facet)
  }

  ui <- shiny::fluidPage(
    title = "NIF debug plot",
    shiny::h3("NIF debug plot"),
    shiny::plotOutput("main_plot", click = "plot_click", height = "500px"),
    shiny::hr(),
    shiny::h4("Selected observation"),
    shiny::fluidRow(
      shiny::column(
        12,
        shiny::conditionalPanel(
          condition = "output.has_selection",
          shiny::h5(shiny::textOutput("selection_info")),
          shiny::tableOutput("source_table")
        ),
        shiny::conditionalPanel(
          condition = "!output.has_selection",
          shiny::p(shiny::em("Click a data point to display the source SDTM record."))
        )
      )
    )
  )

  server <- function(input, output, session) {
    selected_source <- shiny::reactiveVal(NULL)
    selected_info <- shiny::reactiveVal("")

    output$main_plot <- shiny::renderPlot({
      p <- obs_data |>
        dplyr::arrange(.data$GROUP, .data$active_time) |>
        ggplot2::ggplot(ggplot2::aes(
          x = .data$active_time,
          y = .data$DV,
          group = .data$GROUP,
          color = .data$COLOR
        ))

      if (isTRUE(lines)) {
        p <- p + ggplot2::geom_line(na.rm = TRUE)
      }

      p <- p + ggplot2::geom_point(size = size, alpha = alpha, na.rm = TRUE)

      if (!is.null(plot_data_set$facet)) {
        if ("FACET" %in% names(obs_data) &&
            length(unique(obs_data[["FACET"]])) > 1) {
          p <- p + ggplot2::facet_wrap(~FACET, scales = scales)
        }
      }

      if (isTRUE(log)) {
        p <- p + ggplot2::scale_y_log10()
      }

      p <- p +
        ggplot2::theme_bw() +
        ggplot2::theme(
          legend.position = if (length(plot_data_set$color) > 0) {
            "bottom"
          } else {
            "none"
          }
        ) +
        ggplot2::ggtitle(plot_title) +
        ggplot2::labs(
          x = time, y = y_label,
          color = nice_enumeration(plot_data_set$color)
        )

      suppressWarnings(p)
    })

    shiny::observeEvent(input$plot_click, {
      clicked <- shiny::nearPoints(
        obs_data, input$plot_click,
        xvar = "active_time", yvar = "DV",
        threshold = 10, maxpoints = 1
      )

      if (nrow(clicked) == 0) {
        selected_source(NULL)
        selected_info("")
        return()
      }

      src_domain <- clicked$SRC_DOMAIN[1]
      src_seq <- clicked$SRC_SEQ[1]

      if (is.na(src_domain) || src_domain == "IMPORT") {
        selected_source(
          data.frame(Note = "Source: imported data (no SDTM source record)")
        )
        selected_info(paste0(
          "Subject ", clicked$USUBJID[1],
          " | ", clicked$ANALYTE[1],
          " | ", time, " = ", round(clicked$active_time[1], 2),
          " | DV = ", round(clicked$DV[1], 4),
          " | Source: IMPORT"
        ))
        return()
      }

      if (is.na(src_seq)) {
        selected_source(
          data.frame(Note = "SRC_SEQ is NA; cannot look up source record.")
        )
        selected_info(paste0(
          "Subject ", clicked$USUBJID[1],
          " | ", clicked$ANALYTE[1],
          " | Domain: ", src_domain
        ))
        return()
      }

      tryCatch({
        src_data <- domain(sdtm, tolower(src_domain))
        seq_col <- paste0(toupper(src_domain), "SEQ")

        if (!seq_col %in% names(src_data)) {
          selected_source(
            data.frame(Note = paste0("Column ", seq_col, " not found in ",
                                     src_domain, " domain."))
          )
          selected_info(paste0("Domain: ", src_domain))
          return()
        }

        matched <- src_data[src_data[[seq_col]] == src_seq, , drop = FALSE]

        if ("USUBJID" %in% names(matched)) {
          matched <- matched[matched$USUBJID == clicked$USUBJID[1], ,
                             drop = FALSE]
        }

        if (nrow(matched) == 0) {
          selected_source(
            data.frame(Note = paste0(
              "No matching record found in ", src_domain,
              " for ", seq_col, " = ", src_seq
            ))
          )
        } else {
          selected_source(matched)
        }

        selected_info(paste0(
          "Subject ", clicked$USUBJID[1],
          " | ", clicked$ANALYTE[1],
          " | ", time, " = ", round(clicked$active_time[1], 2),
          " | DV = ", round(clicked$DV[1], 4),
          " | Source: ", src_domain, " (", seq_col, " = ", src_seq, ")"
        ))
      },
      error = function(e) {
        selected_source(
          data.frame(Note = paste0("Error looking up source: ", e$message))
        )
        selected_info(paste0("Domain: ", src_domain, " (lookup failed)"))
      })
    })

    output$has_selection <- shiny::reactive({
      !is.null(selected_source())
    })
    shiny::outputOptions(output, "has_selection", suspendWhenHidden = FALSE)

    output$selection_info <- shiny::renderText({
      selected_info()
    })

    output$source_table <- shiny::renderTable({
      shiny::req(selected_source())
      selected_source()
    }, striped = TRUE, bordered = TRUE, hover = TRUE, width = "100%")
  }

  shiny::shinyApp(ui, server)
}
