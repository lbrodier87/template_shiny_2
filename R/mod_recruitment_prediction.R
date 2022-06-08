#' Recruitment prediction module
#'
#' Predicts when a trial will reach it's target recruitment, based on recruitment to date
#' @rdname mod_recruitment_prediction
#' @param id standard shiny id argument
#' @param label standard shiny label argument
#' 
#' @importFrom shiny NS tagList
#' @importFrom accrualPlot accrual_create_df gg_accrual_plot_predict accrual_table
#' @importFrom plotly ggplotly renderPlotly layout

mod_recruitment_prediction_ui <- function(id, label) {
  ns <- NS(id)
  tabItem(
    tabName = label,
    fluidRow(
      tabBox(
        width = 12,
        title = "",
        id = "tabset1", height = "450px",
        selected = "Predicted end date by site (Accrual target: 150)",
        tabPanel(
          "Predicted end date by site (Accrual target: 150)",
          plotlyOutput(ns("predictplot_site"), height = "400"),
          tags$br()
        ),
        tabPanel(
          "Predicted end date for recruitment overall (Accrual target: 150)",
          plotlyOutput(ns("predictplot"), height = "400"),
          tags$br()
        )
      )
    )#,
    # fluidRow(
    #   tabBox(
    #     width = 12,
    #     title = "",
    #     id = "tabset2", height = "450px",
    #     selected = "Predicted end date by site",
    #     tabPanel(
    #       "Predicted end date by site",
    #       plotlyOutput(ns("predictplot_site"), height = "400"),
    #       tags$br()
    #     )
    #   )
    # )
    
  )
}

#' @rdname mod_recruitment_prediction
#' @param input standard shiny input argument
#' @param output standard shiny output argument
#' @param session standard shiny session argument
#' @param data.randomized reactive data containing randomization info
#' @param locations parameters on site level
#' @param study_params general study parameter
#' @param all_data non reactive data 

mod_recruitment_prediction_server <- function(input, output, session, data.randomized, locations, study_params, all_data) {
  ns <- session$ns

  acc <- reactive({
    accrualPlot::accrual_create_df(data.randomized()$rando_date.date,
                                   by = data.randomized()$centre.short)
  })
  acc2 <- accrualPlot::accrual_create_df(all_data$rando_date.date,
                                         by = all_data$centre.short)
  
  # # recruitment targets
  # site_info_rx <- reactive({
  #   targ <- locations %>% 
  #     # filter(centre.short %in% names(acc())) %>% 
  #     filter(centre.short == input$center)
  # })

  ## Prediction plot
  output$predictplot <- renderPlotly({
    
    p <- accrualPlot::gg_accrual_plot_predict(acc(), target = 150) +
      theme_bw() +
      scale_x_date(labels = function(x) format(x, format = "%d %b %Y"))
    
    plotly::ggplotly(p) %>% layout(legend = list(y = 0.5))
  })
  
  ## Prediction plot by site
  output$predictplot_site <- renderPlotly({
    message(locations$target)
    # p <- accrualPlot::gg_accrual_plot_predict(acc(), target = append(locations$target, study_params$acc_target)) +
    p <- accrualPlot::gg_accrual_plot_predict(acc(), target = append(c(30, 30, 30, 30, 30), study_params$acc_target)) +
      theme_bw() +
      scale_x_date(labels = function(x) format(x, format = "%d %b %Y"))
    
    plotly::ggplotly(p) %>% layout(legend = list(y = 0.5))
  })
  }
