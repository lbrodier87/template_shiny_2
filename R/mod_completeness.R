#' Completeness
#' 
#' @import naniar
#'
#' In development
#' @rdname mod_completeness
#' @param id standard shiny id argument
#' @param label standard shiny label argument
mod_completeness_ui <- function(id, label){
  ns <- NS(id)
  tabItem(tabName = label,
          
          fluidRow(
            tabBox(width = 12,
                   title = "",
                   id = "tabset2",
                   height = "850px",
                   selected = "Completeness"
                   , tabPanel("Overview form",
                            height = "400",
                            plotlyOutput(ns('vis_miss'), height = "750")
                   )
                   , tabPanel("Variable missingness",
                              height = "400",
                              plotlyOutput(ns('var_miss'), height = "750")
                   )
                   , tabPanel("Case missingness",
                              height = "400",
                              plotlyOutput(ns('case_miss'), height = "750")
                   )
                   , tabPanel("Missingness pattern",
                              height = "300",
                              plotOutput(ns('miss_pattern'), height = "750")
                   )
            )
          )
  )
}

#' @rdname mod_completeness
#' @param input standard shiny input argument
#' @param output standard shiny output argument
#' @param session standard shiny session argument
#' @param data data for use in calculations
mod_completeness_server <- function(input, output, session, data_form){
  
  ns <- session$ns
  
  output$vis_miss <- renderPlotly({
    data_form() %>% vis_miss %>% ggplotly
  })
  
  output$var_miss <- renderPlotly({
    data_form() %>% gg_miss_var(show_pct = TRUE) %>% ggplotly
  })
  
  output$case_miss <- renderPlotly({
    data_form() %>% gg_miss_case(show_pct = TRUE) %>% ggplotly
  })
  
  output$miss_pattern <- renderPlot({
    data_form() %>% gg_miss_upset(text.scale = 3, nsets = ncol(data_form()))
  })

}
