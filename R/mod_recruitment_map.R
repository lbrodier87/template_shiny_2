

library(leaflet)
library(dplyr)

mod_recruitment_map_ui <- function(id, label){
  ns <- NS(id)
  tabItem(tabName = label,
          fluidRow(
            tabBox(width = 12,
                   title = "",
                   id = "tabset2",
                   height = "450px",
                   tabPanel("Site locations",
                            leafletOutput(ns("leaflet_map")),
                            height = "400"
                            ))
          ),

          fluidRow(
            tabBox(
              width = 12,
              title = "",
              id = "tabset1", height = "450px",
              selected = "Recruitment over time by center",
              tabPanel("Recruitment over time by center",plotlyOutput(ns('recruitplot_map'), height = "400"))

            )
          )
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

  acc <- reactive({
    accrualPlot::accrual_create_df(dat()$rando_date.date,
                                   by = dat()$centre.short)
  })

  # selected site
  site_data <- reactive({
    site <- input$leaflet_map_marker_click$id
    message("site = ", site)
    if(length(site) > 0){
      x <- acc()[[site]]
      x$site <- site
      return(x)
    }
  })

  ## Recruitment plot
  output$recruitplot_map <- renderPlotly({

    p <- accrualPlot::gg_accrual_plot_cum(acc()) +
      theme_bw() +
      scale_x_date(labels = function(x) format(x, format = "%d %b %Y"))

    message("length(site_data) = ", length(site_data()))

    if(length(site_data()) > 0) p <- p + geom_step(data = site_data(),
                                            aes(x = Date, y = Cumulative),
                                            size = 2)

    plotly::ggplotly(p) %>% layout(legend = list(y = 0.5))

  })

  output$leaflet_map <- renderLeaflet({
    leaflet(d()) %>%
    addTiles() %>%
    # addCircles(radius = ~ 100 * n, popup = ~ paste("<b>", centre.short, "</b>",
    #                                                "<br/>N randomized =", n,
    #                                                "<br/>Open since", first))
    addMarkers(popup = ~ paste("<b>", centre.short, "</b>",
                                     "<br/>N randomized =", n,
                                     "<br/>Open since", first),
               layerId=~centre.short)
  })

}
