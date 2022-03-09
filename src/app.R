#' Run the Shiny Application
#'
#' @param ... A series of options to be used inside the app.
#'
#' @export
#' @importFrom shiny shinyApp
    
source('./app_server.R')
source('./app_ui.R')
      
# initialize the shiny application
app <- shinyApp(ui = app_ui, server = app_server)

# return the application for shiny to run the server
return(app)