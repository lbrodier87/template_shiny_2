#' Recruitment module Functions
#'
#' @description This module presents a recruitment plot and table using the accrualPlot package
#' @rdname mod_recruitment2
#' @param id,input,output,session,label Internal parameters for {shiny}.
#'
#' @importFrom shiny NS tagList
#' @importFrom accrualPlot accrual_create_df gg_accrual_plot_cum accrual_table
#' @importFrom gt render_gt gt tab_options
#' @importFrom plotly ggplotly renderPlotly layout
#'

mod_recruitment2_ui <- function(id, label){

  ns <- NS(id)

  tabItem(tabName = label,

          ##
          tags$style(HTML("


      .box.box-solid.box-primary>.box-header {
      color:#fff;
      background:#1ea5a5
                    }

      .box.box-solid.box-primary{
      border-bottom-color:#1ea5a5;
      border-left-color:#1ea5a5;
      border-right-color:#1ea5a5;
      border-top-color:#1ea5a5;
                    }

                  ")),


          fluidRow(
            tags$style(".small-box.bg-yellow { background-color: #a5d7d2 !important; color: #333333 !important; }"),
            tags$style(".small-box.bg-red { background-color: #d20537 !important; color: #333333 !important; }"),
            valueBoxOutput(ns("randomized"), width = 6),
            valueBoxOutput(ns("estimated"), width = 6)
          ),

          fluidRow(
            tabBox(
              width = 12,
              title = "",
              id = "tabset1", height = "850px",
              selected = "Recruitment over time by center",
              tabPanel("Recruitment over time by center",
                       plotlyOutput(ns('recruitplot'), height = "750"),
                       tags$br(),
                       gt::gt_output(ns('recruittable')))

            )
          )
  )
}

#' recruitment Server Function
#' @rdname mod_recruitment2
#' @param data.randomized reactive data containing randomization info
#'   (see {mod_recruitment_prediction} for example without reactivity)
#'
mod_recruitment2_server <- function(input, output, session, data.randomized){

  ns <- session$ns

  output$randomized <- renderValueBox({


    perc <- round(nrow(data.randomized())/150*100, digits = 1)
    valueBox(value = paste0(nrow(data.randomized()), " out of ", "150", " (", perc, "%)" ),
             subtitle = "Randomized from pre-registered patients (those with inadequate skin flaps are not randomized)",
             icon = icon("user-plus"), color = "yellow")
  })

  output$estimated <- renderValueBox({

    perc <- round(nrow(data.randomized())/300*100, digits = 1)
    valueBox(value = paste0(nrow(data.randomized()), " out of ", 300, " (", perc, "%)" ),
             subtitle = "actual out of estimated recruitment", icon = icon("user-plus"), color = "yellow")
  })

  acc <- reactive({
    accrualPlot::accrual_create_df(data.randomized()$rando_date.date,
                                   by = data.randomized()$centre.short)
  })


  ## Recruitment plot
  output$recruitplot <- renderPlotly({

    p <- accrualPlot::gg_accrual_plot_cum(acc()) +
      theme_bw() +
      scale_x_date(labels = function(x) format(x, format = "%d %b %Y"))

    plotly::ggplotly(p) %>% layout(legend = list(y = 0.5))
  })

  output$recruittable <- gt::render_gt({
    gt::gt(accrualPlot::accrual_table(acc(), unit = "month", format_table_date = "%b %Y")) %>%
      gt::tab_options(column_labels.hidden = TRUE)
  })

}



