#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd

library(shiny)

app_server <- function(input, output, session ) {
  ## Get all module names
  mod <- get_modules()

  ## To get reactive data
  rx.data <- get_reactive_data(input = input)
  data <- get_data()

  ## Recruitment tab
  callModule(mod_recruitment2_server, mod$recruit2, data.randomized = rx.data$rx_random)
  callModule(mod_recruitment_server, mod$recruit, data.randomized = rx.data$rx_random)
}


