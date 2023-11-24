wt_viewer <- function(nif, analyte=NA) {
  if(is.na(analyte)) {
    analyte <- guess_analyte(nif)
  }

  temp <- nif %>%
    as.data.frame() #%>%
    #mutate(BMI = WEIGHT/(HEIGHT/100)^2)

  dm <- temp %>%
    as.data.frame() %>%
    filter(EVID==0) %>%
    # distinct(USUBJID, HEIGHT, WEIGHT, BMI, ID)
    distinct(USUBJID, AGE, WEIGHT, BILI, CRCL, SALT, ALP, SALB, BL_BILI, BL_CRCL)

  age_hist <- dm %>%
    ggplot(aes(x=AGE)) +
    geom_histogram(binwidth=5, position="identity", fill="grey")

  age_hist_data <- ggplot_build(age_hist)$data[[1]]

  # cov <- dm %>%
  #   mutate(CRCL=case_match(CRCL, -99~NA, .default=CRCL)) %>%
  #   pivot_longer(cols=-USUBJID, names_to="PARAM", values_to="VALUE")

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
                           multiple = TRUE, selected=sbs[1]),
        shiny::actionButton("prev.sb", "previous"),
        shiny::actionButton("next.sb", "next")
      )
    ),

    shiny::fluidRow(
      shiny::column(4, shiny::plotOutput("plot.dm", click = "plot_dm_click")),
      shiny::column(4, shiny::plotOutput("plot.cov", click = "plot_cov_click")),
      # shiny::column(6, shiny::plotOutput("plot.bmi", click = "plot_bmi_click"),
      shiny::column(4, plotOutput("plot_age_hist", click="plot_age_hist_click"))
    ),

    shiny::plotOutput("plot.pc", click = "plot_pc_click")
  )



  test_viewer.server <- function(input, output, session) {
    active_sb <- reactiveVal(sbs)

    output$plot.dm <- shiny::renderPlot({
      dm %>%
        ggplot(aes(x=WEIGHT, y=AGE)) +
        geom_point(size=4, color="grey") +
        geom_point(data=dm %>% filter(USUBJID %in% input$subject),
                   color="red", size=4) +
        theme_bw() +
        theme(legend.position="none")
    }, height=350)#, width=350)

    output$plot.cov <- shiny::renderPlot({
      dm %>%
        distinct(USUBJID, BL_BILI, BL_CRCL) %>%
        ggplot(aes(x=BL_BILI, y=BL_CRCL)) +
        geom_point(size=4, color="grey") +
        geom_point(data=dm %>% filter(USUBJID %in% input$subject),
                   color="red", size=4) +
        # xlim(0, NA) +
        # ylim(0, NA) +
        scale_x_log10() +
        scale_y_log10() +
        theme_bw() +
        theme(legend.position="none")
    }, height=350)#, width=350)

    # output$plot.bmi <- shiny::renderPlot({
    #   dm %>%
    #     ggplot(aes(x=WEIGHT, y=BMI)) +
    #     geom_hline(yintercept=c(18.5, 25, 30)) +
    #     geom_point(size=4, color="grey") +
    #     geom_point(data=dm %>% filter(USUBJID==input$subject),
    #                color="red", size=4) +
    #     theme_bw() +
    #     theme(legend.position="none")
    # }, height=350)#, width=350)

    output$plot.pc <- shiny::renderPlot({
      pc %>%
        ggplot(aes(x=TIME, y=DV, group=USUBJID)) +
        geom_line(size=1.2, alpha=0.3, color="grey") +
        geom_point(size=4, alpha=0.3, color="grey") +
        xlim(0, 200) +
        labs(y=analyte) +
        scale_y_log10() +
        geom_line(data=pc %>% filter(USUBJID %in% input$subject),
                  aes(x=TIME, y=DV), color="red", size=1) +
        geom_point(data=pc %>% filter(USUBJID %in% input$subject),
                  color="red", size=4) +
        theme_bw() +
        theme(legend.position="none")
    }, height=350)

    output$plot_age_hist <- shiny::renderPlot({
      # p <- dm %>%
      #   ggplot(aes(x=AGE)) +
      #   geom_histogram(binwidth=5, position="identity", fill="grey") +
      age_hist +
      geom_vline(
        xintercept=dm[which(dm$USUBJID==input$subject), "AGE"],
        color="red", size=1) +
      theme_bw()

    }, height=350)

    # output$plot_cov <- shiny::renderPlot({
    #   cov %>%
    #     ggplot(aes(x=PARAM, y=VALUE)) +
    #     #geom_dotplot(binaxis="y", stackdir="center") +
    #     geom_boxplot() +
    #     #geom_jitter() +
    #     #geom_dotplot(binaxis='y', stackdir='center', size=5) +
    #     theme_bw()
    # }, height=350)

    observeEvent(input$plot_dm_click, {
      id <- nearPoints(dm, input$plot_dm_click, addDist=T)
      shiny::updateSelectInput(
        session, "subject",
        choices= sbs,
        selected=sbs[which(sbs==id$USUBJID)])
    })

    # observeEvent(input$plot_bmi_click, {
    #   id <- nearPoints(dm, input$plot_bmi_click, addDist=T)
    #   shiny::updateSelectInput(
    #     session, "subject",
    #     choices= sbs,
    #     selected=sbs[which(sbs==id$USUBJID)])
    # })

    observeEvent(input$plot_pc_click, {
      id <- nearPoints(pc, input$plot_pc_click, addDist=T)
      #print(id$ID)
      shiny::updateSelectInput(
        session, "subject", choices= sbs, selected=sbs[which(sbs==id$USUBJID)])
    })

    observeEvent(input$plot_age_hist_click, {
      #id <- nearPoints(pc, input$plot_age_hist_click, addDist=T)
      id <- input$plot_age_hist_click
      click_x = id$x
      print(click_x)

      temp <- age_hist_data %>%
        filter(xmax>click_x, xmin<click_x)

      min_age <- temp$xmin
      max_age <- temp$xmax

      sel <- dm %>%
        filter(AGE >= min_age, AGE < max_age) %>%
        pull(USUBJID)

      print(sel)

      shiny::updateSelectInput(
        session, "subject", choices=sbs, selected=sel)
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












