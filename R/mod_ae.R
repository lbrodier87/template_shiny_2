#' Adverse Events
#'
#' In development
#' @rdname mod_ae
#' @param id standard shiny id argument
#' @param label standard shiny label argument
mod_ae_ui <- function(id, label){
  ns <- NS(id)
  tabItem(tabName = label,
          tabsetPanel(id = ns("switcher"), type="hidden", 
                      tabPanelBody("loading", icon('transfer', lib = 'glyphicon'), HTML('&nbsp;&nbsp;'), "loading... ", ),
                      tabPanelBody("not_authorized", icon("lock", "fa-2x"), HTML('&nbsp;&nbsp;'), "You are not authorized to access this module."), 
                      tabPanelBody("authorized",
                                    fluidRow(
                                      tabBox(width = 12,
                                             title = "",
                                             id = "tabset2",
                                             height = "450px",
                                             tabPanel("Adverse Events",
                                                      "This module is under development",
                                                      # height = "400"
                                              )
                                      )
                                    )
                      )
          )
  )
}

#' @rdname mod_ae
#' @param input standard shiny input argument
#' @param output standard shiny output argument
#' @param session standard shiny session argument
#' @param data data for use in calculations
mod_ae_server <- function(input, output, session, data, auth){
  ns <- session$ns
  
  #### test login ####
  access_granted = reactive({
    return(auth$access_ae)
  })
  #switch tabsetpanel depending on user rights for the module
  observe({
    req(access_granted())
    if(access_granted())
      updateTabsetPanel(inputId = "switcher", selected = "authorized")
    else{
      updateTabsetPanel(inputId = "switcher", selected = "not_authorized")
    }
  })
  
  
}
