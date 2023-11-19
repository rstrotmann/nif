wt_viewer <- function(nif, analyte=NA) {
  if(is.na(analyte)) {
    analyte <- guess_analyte(nif)
  }

  temp <- nif %>%
    as.data.frame() %>%
    mutate(BMI = WEIGHT/(HEIGHT/100)^2)

  dm <- temp %>%
    as.data.frame() %>%
    filter(EVID==0) %>%
    distinct(USUBJID, HEIGHT, WEIGHT, BMI, ID)

  pc <- temp %>%
    as.data.frame() %>%
    filter(ANALYTE==analyte) %>%
    filter(EVID==0)

  sbs <- unique(dm$USUBJID)

  test_viewer.ui <- shiny::fluidPage(
    title="NIF viewer",

    shiny::fluidRow(
      style="padding:10px; spacing:10",
      shiny::column(3,
        shiny::selectInput("subject", label="Subject", choices=sbs,
                           selected=sbs[1]),
        shiny::actionButton("prev.sb", "previous"),
        shiny::actionButton("next.sb", "next")
      )
    ),

    shiny::fluidRow(
      shiny::column(6, shiny::plotOutput("plot.dm", click = "plot_dm_click")),
      shiny::column(6, shiny::plotOutput("plot.bmi", click = "plot_bmi_click"))
    ),

    # h4("ID"),
    # shiny::verbatimTextOutput("id_info"),
    shiny::plotOutput("plot.pc", click = "plot_pc_click")
  )



  test_viewer.server <- function(input, output, session) {
    active_sb <- reactiveVal(sbs)

    output$plot.dm <- shiny::renderPlot({
      dm %>%
        ggplot(aes(x=WEIGHT, y=HEIGHT)) +
        geom_point(size=4, color="grey") +
        geom_point(data=dm %>% filter(USUBJID==input$subject),
                   color="red", size=4) +
        theme_bw() +
        theme(legend.position="none")
    }, height=350)#, width=350)

    output$plot.bmi <- shiny::renderPlot({
      dm %>%
        ggplot(aes(x=WEIGHT, y=BMI)) +
        geom_hline(yintercept=c(18.5, 25, 30)) +
        geom_point(size=4, color="grey") +
        geom_point(data=dm %>% filter(USUBJID==input$subject),
                   color="red", size=4) +
        theme_bw() +
        theme(legend.position="none")
    }, height=350)#, width=350)

    output$plot.pc <- shiny::renderPlot({
      pc %>%
        ggplot(aes(x=TIME, y=DV, group=USUBJID)) +
        geom_line(size=1.2, alpha=1, color="grey") +
        geom_point(size=4, color="grey") +
        xlim(0, 200) +
        labs(y=analyte) +
        scale_y_log10() +
        geom_line(data=pc %>% filter(USUBJID ==input$subject),
                  aes(x=TIME, y=DV), color="red", size=1) +
        geom_point(data=pc %>% filter(USUBJID==input$subject),
                  color="red", size=4) +
        theme_bw() +
        theme(legend.position="none")
    }, height=350)

    observeEvent(input$plot_dm_click, {
      id <- nearPoints(dm, input$plot_dm_click, addDist=T)
      shiny::updateSelectInput(
        session, "subject",
        choices= sbs,
        selected=sbs[which(sbs==id$USUBJID)])
    })

    observeEvent(input$plot_bmi_click, {
      id <- nearPoints(dm, input$plot_bmi_click, addDist=T)
      shiny::updateSelectInput(
        session, "subject",
        choices= sbs,
        selected=sbs[which(sbs==id$USUBJID)])
    })

    observeEvent(input$plot_pc_click, {
      id <- nearPoints(pc, input$plot_pc_click, addDist=T)
      #print(id$ID)
      shiny::updateSelectInput(
        session, "subject", choices= sbs, selected=sbs[which(sbs==id$USUBJID)])
    })

    shiny::observeEvent(input$prev.sb, {
      current <- which(sbs==input$subject)
      if(current > 1) {
        shiny::updateSelectInput(
          session, "subject", choices=sbs, selected=sbs[current-1])
      }
    })

    shiny::observeEvent(input$next.sb, {
      current <- which(sbs==input$subject)
      if(current < length(sbs)) {
        shiny::updateSelectInput(
          session, "subject", choices=sbs, selected=sbs[current+1])
      }
    })

  }


  shiny::shinyApp(test_viewer.ui, test_viewer.server)
}
