

library(leaflet)
library(dplyr)

mod_recruitment_map_ui <- function(id, label){
  ns <- NS(id)
  tabItem(tabName = label,
          leafletOutput(ns("leaflet_map"))
          )
}


mod_recruitment_map_server <- function(input, output, session, dat, locations){

  ns <- session$ns

  # message(dat())

  d <- reactive({
    dat() %>%
      group_by(centre.short) %>%
      summarize(n = n(),
                first = min(rando_date.date)) %>%
      left_join(locations)
    })


  output$leaflet_map <- renderLeaflet({
    leaflet(d()) %>%
    addTiles() %>%
    addCircles(radius = ~ 100 * n, popup = ~ paste("<b>", centre.short, "</b>",
                                                   "<br/>N randomized =", n,
                                                   "<br/>Open since", first))
  })

}
