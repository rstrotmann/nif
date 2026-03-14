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

  render_highlight_table <- function(df, highlight_rows = NULL) {
    header <- paste0(
      "<tr>",
      paste0("<th>", htmltools::htmlEscape(names(df)), "</th>", collapse = ""),
      "</tr>"
    )

    if (is.null(highlight_rows)) highlight_rows <- rep(FALSE, nrow(df))

    rows <- vapply(seq_len(nrow(df)), function(i) {
      cls <- if (highlight_rows[i]) ' class="highlight-row"' else ""
      cells <- paste0(
        "<td>",
        vapply(df[i, ], function(v) {
          htmltools::htmlEscape(as.character(v))
        }, character(1)),
        "</td>",
        collapse = ""
      )
      paste0("<tr", cls, ">", cells, "</tr>")
    }, character(1))

    shiny::HTML(paste0(
      '<div style="overflow-x: auto;">',
      '<table class="table table-bordered table-striped table-hover"',
      ' style="width: 100%;">',
      "<thead>", header, "</thead>",
      "<tbody>", paste(rows, collapse = ""), "</tbody>",
      "</table></div>"
    ))
  }

  render_sdtm_table <- function(df, seq_col, highlight_seq) {
    hl <- rep(FALSE, nrow(df))
    if (!is.null(highlight_seq) && !is.null(seq_col) &&
        seq_col %in% names(df)) {
      hl <- !is.na(df[[seq_col]]) & df[[seq_col]] == highlight_seq
    }
    render_highlight_table(df, hl)
  }

  lookup_domain_neighbors <- function(sdtm_obj, domain_name, usubjid,
                                      target_seq) {
    src_data <- domain(sdtm_obj, tolower(domain_name))
    seq_col <- paste0(toupper(domain_name), "SEQ")

    if (!seq_col %in% names(src_data)) {
      return(list(data = NULL, seq_col = seq_col, seq_val = target_seq))
    }

    subj_data <- src_data
    if ("USUBJID" %in% names(src_data)) {
      subj_data <- src_data[src_data$USUBJID == usubjid, , drop = FALSE]
    }
    subj_data <- subj_data[order(subj_data[[seq_col]]), , drop = FALSE]

    match_idx <- which(subj_data[[seq_col]] == target_seq)
    if (length(match_idx) == 0) {
      return(list(data = NULL, seq_col = seq_col, seq_val = target_seq))
    }

    idx <- match_idx[1]
    neighbor_idx <- seq(max(1, idx - 1), min(nrow(subj_data), idx + 1))
    list(
      data = subj_data[neighbor_idx, , drop = FALSE],
      seq_col = seq_col,
      seq_val = target_seq
    )
  }

  ui <- shiny::fluidPage(
    title = "NIF debug plot",
    shiny::tags$style(shiny::HTML(
      ".highlight-row { background-color: #fff3cd !important;
         font-weight: bold; }
       .sdtm-table table { font-size: 12px; }
       .sdtm-table td, .sdtm-table th { padding: 4px 8px; }"
    )),
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
          shiny::div(class = "sdtm-table", shiny::uiOutput("source_table")),
          shiny::uiOutput("ex_section"),
          shiny::uiOutput("nif_section")
        ),
        shiny::conditionalPanel(
          condition = "!output.has_selection",
          shiny::p(shiny::em(
            "Click a data point to display the source SDTM record."
          ))
        )
      )
    )
  )

  server <- function(input, output, session) {
    selected_point <- shiny::reactiveVal(NULL)
    selected_source <- shiny::reactiveVal(NULL)
    selected_seq <- shiny::reactiveVal(NULL)
    selected_info <- shiny::reactiveVal("")
    selected_ex <- shiny::reactiveVal(NULL)
    selected_ex_seq <- shiny::reactiveVal(NULL)
    selected_nif_rows <- shiny::reactiveVal(NULL)
    selected_nif_highlight <- shiny::reactiveVal(NULL)

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

      sel <- selected_point()
      if (!is.null(sel)) {
        p <- p +
          ggplot2::geom_point(
            data = sel, size = size + 3, shape = 21, stroke = 2,
            fill = NA, color = "black", na.rm = TRUE
          )
      }

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

    find_related_ex <- function(clicked_row) {
      usubjid <- clicked_row$USUBJID[1]
      parent <- clicked_row$PARENT[1]
      obs_time <- clicked_row$TIME[1]

      if (is.na(parent) || is.na(obs_time)) return(NULL)

      admins <- nif[nif$EVID == 1 &
                    nif$USUBJID == usubjid &
                    nif$ANALYTE == parent &
                    !is.na(nif$TIME) &
                    nif$TIME <= obs_time, , drop = FALSE]

      if (nrow(admins) == 0) return(NULL)

      admins <- admins[order(admins$TIME, decreasing = TRUE), , drop = FALSE]
      closest_admin <- admins[1, , drop = FALSE]

      if (!"SRC_SEQ" %in% names(closest_admin) ||
          is.na(closest_admin$SRC_SEQ[1])) {
        return(NULL)
      }

      closest_admin$SRC_SEQ[1]
    }

    shiny::observeEvent(input$plot_click, {
      clicked <- shiny::nearPoints(
        obs_data, input$plot_click,
        xvar = "active_time", yvar = "DV",
        threshold = 10, maxpoints = 1
      )

      if (nrow(clicked) == 0) {
        selected_point(NULL)
        selected_source(NULL)
        selected_seq(NULL)
        selected_info("")
        selected_ex(NULL)
        selected_ex_seq(NULL)
        selected_nif_rows(NULL)
        selected_nif_highlight(NULL)
        return()
      }

      selected_point(clicked[1, , drop = FALSE])

      src_domain <- clicked$SRC_DOMAIN[1]
      src_seq <- clicked$SRC_SEQ[1]

      if (is.na(src_domain) || src_domain == "IMPORT") {
        selected_source(
          data.frame(Note = "Source: imported data (no SDTM source record)")
        )
        selected_seq(NULL)
        selected_info(paste0(
          "Subject ", clicked$USUBJID[1],
          " | ", clicked$ANALYTE[1],
          " | ", time, " = ", round(clicked$active_time[1], 2),
          " | DV = ", round(clicked$DV[1], 4),
          " | Source: IMPORT"
        ))
        selected_ex(NULL)
        selected_ex_seq(NULL)
        selected_nif_rows(NULL)
        selected_nif_highlight(NULL)
        return()
      }

      if (is.na(src_seq)) {
        selected_source(
          data.frame(Note = "SRC_SEQ is NA; cannot look up source record.")
        )
        selected_seq(NULL)
        selected_info(paste0(
          "Subject ", clicked$USUBJID[1],
          " | ", clicked$ANALYTE[1],
          " | Domain: ", src_domain
        ))
        selected_ex(NULL)
        selected_ex_seq(NULL)
        selected_nif_rows(NULL)
        selected_nif_highlight(NULL)
        return()
      }

      tryCatch({
        result <- lookup_domain_neighbors(sdtm, src_domain,
                                          clicked$USUBJID[1], src_seq)

        if (is.null(result$data)) {
          selected_source(
            data.frame(Note = paste0(
              "No matching record found in ", src_domain,
              " for ", result$seq_col, " = ", src_seq
            ))
          )
          selected_seq(NULL)
        } else {
          selected_source(result$data)
          selected_seq(src_seq)
        }

        selected_info(paste0(
          "Subject ", clicked$USUBJID[1],
          " | ", clicked$ANALYTE[1],
          " | ", time, " = ", round(clicked$active_time[1], 2),
          " | DV = ", round(clicked$DV[1], 4),
          " | Source: ", src_domain, " (", result$seq_col, " = ", src_seq, ")"
        ))
      },
      error = function(e) {
        selected_source(
          data.frame(Note = paste0("Error looking up source: ", e$message))
        )
        selected_seq(NULL)
        selected_info(paste0("Domain: ", src_domain, " (lookup failed)"))
      })

      tryCatch({
        ex_seq <- find_related_ex(clicked[1, , drop = FALSE])
        if (!is.null(ex_seq) && has_domain(sdtm, "ex")) {
          ex_result <- lookup_domain_neighbors(sdtm, "EX",
                                               clicked$USUBJID[1], ex_seq)
          selected_ex(ex_result$data)
          selected_ex_seq(ex_seq)
        } else {
          selected_ex(NULL)
          selected_ex_seq(NULL)
        }
      },
      error = function(e) {
        selected_ex(NULL)
        selected_ex_seq(NULL)
      })

      tryCatch({
        subj_nif <- nif[nif$USUBJID == clicked$USUBJID[1] &
                        nif$EVID == 0, , drop = FALSE]
        subj_nif <- subj_nif[order(subj_nif$TIME), , drop = FALSE]

        match_idx <- which(
          !is.na(subj_nif$SRC_DOMAIN) & subj_nif$SRC_DOMAIN == src_domain &
          !is.na(subj_nif$SRC_SEQ) & subj_nif$SRC_SEQ == src_seq &
          subj_nif$ANALYTE == clicked$ANALYTE[1]
        )

        if (length(match_idx) > 0) {
          idx <- match_idx[1]
          neighbor_idx <- seq(max(1, idx - 1),
                              min(nrow(subj_nif), idx + 1))
          nif_subset <- subj_nif[neighbor_idx, , drop = FALSE]
          hl <- rep(FALSE, length(neighbor_idx))
          hl[which(neighbor_idx == idx)] <- TRUE
          selected_nif_rows(nif_subset)
          selected_nif_highlight(hl)
        } else {
          selected_nif_rows(NULL)
          selected_nif_highlight(NULL)
        }
      },
      error = function(e) {
        selected_nif_rows(NULL)
        selected_nif_highlight(NULL)
      })
    })

    output$has_selection <- shiny::reactive({
      !is.null(selected_source())
    })
    shiny::outputOptions(output, "has_selection", suspendWhenHidden = FALSE)

    output$selection_info <- shiny::renderText({
      selected_info()
    })

    output$source_table <- shiny::renderUI({
      shiny::req(selected_source())
      df <- selected_source()
      sel_seq <- selected_seq()
      src_dom <- selected_point()$SRC_DOMAIN[1]
      seq_col <- if (!is.na(src_dom)) paste0(toupper(src_dom), "SEQ") else NULL

      render_sdtm_table(df, seq_col, sel_seq)
    })

    output$ex_section <- shiny::renderUI({
      ex_data <- selected_ex()
      if (is.null(ex_data)) return(NULL)

      shiny::tagList(
        shiny::hr(),
        shiny::h4("Related administration (EX)"),
        shiny::div(
          class = "sdtm-table",
          render_sdtm_table(ex_data, "EXSEQ", selected_ex_seq())
        )
      )
    })

    output$nif_section <- shiny::renderUI({
      nif_data <- selected_nif_rows()
      if (is.null(nif_data)) return(NULL)

      shiny::tagList(
        shiny::hr(),
        shiny::h4("NIF record"),
        shiny::div(
          class = "sdtm-table",
          render_highlight_table(nif_data, selected_nif_highlight())
        )
      )
    })
  }

  shiny::shinyApp(ui, server)
}
