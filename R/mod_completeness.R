#' Completeness
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
                   selected = "Completeness",
                   tabPanel("Completeness",
                            # height = "400"
                            plotOutput(ns('plot'), height = "750")
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
mod_completeness_server <- function(input, output, session, data){

  ns <- session$ns
  
  output$plot <- renderPlot({
  
    ggplot(data = data, aes(x = age, y = weight)) + geom_point()

  })


  # message(dat())

}
