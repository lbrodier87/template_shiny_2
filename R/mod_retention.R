#' Retention
#'
#' In development
#' @rdname mod_retention
#' @param id standard shiny id argument
#' @param label standard shiny label argument
mod_retention_ui <- function(id, label){
  ns <- NS(id)
  tabItem(tabName = label,
          fluidRow(
            tabBox(width = 12,
                   title = "",
                   id = "tabset2",
                   height = "450px",
                   tabPanel("Retention",
                            "This module is under development",
                            # height = "400"
                            )
                   )
            )
          )
}

#' @rdname mod_retention
#' @param input standard shiny input argument
#' @param output standard shiny output argument
#' @param session standard shiny session argument
#' @param data data for use in calculations
mod_retention_server <- function(input, output, session, data){

  ns <- session$ns

  # message(dat())

}
