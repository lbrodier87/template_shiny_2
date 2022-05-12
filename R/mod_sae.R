#' Serious Adverse Events
#'
#' In development
#' @rdname mod_sae
#' @param id standard shiny id argument
#' @param label standard shiny label argument
#' 
#' @author Laurent Brodier
#' @import ggplot
#' @import plotly
#' 

# tmp list of choices for SAE characteristics... 
# (next will get them directly from the dataset in server code)
sae_severity_list <- c("Mild", "Moderate", "Severe")
sae_outcome_list <- c("Resolved without sequelae", "Resolved with sequelae", "Ongoing", "Death", "Other", "Unknown")
sae_causality_list <- c("Certain", "Probable", "Possible", "Unlikely", "Not related", "Not assessable")
sae_expectedness_list <- c("Expected", "Unexpected")

mod_sae_ui <- function(id, label){
  ns <- NS(id)
  tabItem(tabName = label,
          fluidRow(
            # SAE filters (for all tabs)
            box(width = 12,
              #TODO:populate list from server side, based on dataset content
              fluidRow( 
                column(3, selectInput(ns("sae_filter_severity"), label = "Filter by severity", 
                            choices = sae_severity_list, multiple = T, 
                            selected = sae_severity_list)), 
                column(3, selectInput(ns("sae_filter_outcome"), label = "Filter by outcome", 
                                      choices = sae_outcome_list, multiple = T, 
                                      selected = sae_outcome_list)),
                column(3, selectInput(ns("sae_filter_causality"), label = "Filter by causality", 
                                      choices = sae_causality_list, multiple = T, 
                                      selected = sae_causality_list)),
                column(3, selectInput(ns("sae_filter_expectedness"), label = "Filter by expectedness", 
                                      choices = sae_expectedness_list, multiple = T, 
                                      selected = sae_expectedness_list))
              )
            ),
            
            # Tabs
            tabBox(width = 12,
                   title = "",
                   id = "tabset2",
                   height = "450px",
                  
                   tabPanel("SAE occurence", 
                            "Cummulative SAE occurence overtime:",
                            plotlyOutput(ns("sae_plot_1"))
                            ),
                   tabPanel("SAE occurence by center",
                            "Cummulative SAE occurence overtime by center:",
                           plotlyOutput(ns("sae_plot_2"))
                           ), 
                   tabPanel("SAE number by characteristics", 
                            "SAE number by center and by characteristics: ", 
                            selectInput(ns("sae_fact_sel"), 
                                        choices = c("None", "Severity level", "Outcome", "Causality", 
                                                    "Expectedness", "Death", "Life threatening", "Persistant disability", 
                                                    "Hospitalization", "Congenital anomaly / birth defect"), 
                                        label = "SAE characteristics as color: "),
                            #"add filters with code generated choices",
                            #Check order of levels on selection! 
                            plotlyOutput(ns("sae_plot_3"))
                            ), 
                   tabPanel(width=12, "SAE list",
                            dataTableOutput(ns("sae_table_1"))
                   )
              )
            )
          )
}

#' @rdname mod_sae
#' @param input standard shiny input argument
#' @param output standard shiny output argument
#' @param session standard shiny session argument
#' @param data.sae data for use in calculations
mod_sae_server <- function(input, output, session, data.sae){

  ns <- session$ns
  
  #varname mapping - to map to your dataset variable names (useful?)
  record_id <- "pat_id"
  center <- "centre.short"
  sae_date <- "sae_date"

  #filtered data based on SAE characteristics, used in plots below
  data.sae.filtered <- reactive({
    d <- data.sae[data.sae$severity_level %in% input$sae_filter_severity,]
    d <- d[d$outcome %in% input$sae_filter_outcome,]
    d <- d[d$causality %in% input$sae_filter_causality,]
    d <- d[d$expectedness %in%  input$sae_filter_expectedness,]
    d
  })
  
  ## plot of cumulative SAE number (all centers)
  output$sae_plot_1 <- renderPlotly({
    d <- data.sae.filtered()
    d <- d[order(d[,sae_date]),] #order the SAE by date
    d$cumul <- seq_along(d[,sae_date]) #get cumulative count of SAE
    a <- aggregate(data = d, as.formula(paste0("cumul ~ ", sae_date)), max) #aggregate max (to acount for >1 SAE on same date)
    #do the plot (+add custom label to hover text as points)
    p <- ggplot() + geom_line(aes(x=a[,sae_date], y=a$cumul)) + 
      geom_point(aes(x=a[,sae_date], y=a$cumul, text=paste("SAE date:", a[,sae_date], 
                                                           "<br>SAE count:", a$cumul)), size=0.5) +
      theme_bw() + labs(x="SAE date", y="SAE cumulative number")
    #convert to plotly (+specify tooltip)
    plotly::ggplotly(p, tooltip="text")
  })
  
  ## plot of cumulative SAE number (color by center)
  output$sae_plot_2 <- renderPlotly({
    d <- data.sae.filtered()
    #order the SAE by date
    d <- d[order(d[,sae_date]),] 
    
    #get cumulative SAE count by center
    center_list <- sort(unique(d[,center]))
    d$cumul <- 0
    for(c in center_list){
      d$cumul[d[,center]==c] <- seq_along(d$cumul[d[,center]==c])
    }
    
    #do the plot (+add custom label to hover text as points)
    p <- ggplot() +  geom_line(aes(x=d[,sae_date], y=d$cumul, color=d[,center])) + 
      geom_point(aes(x=d[,sae_date], y=d$cumul, color=d[,center], 
                    text=paste('SAE date: ', d[,sae_date],
                               '<br>Center:', d[,center],
                               '<br>SAE count:', d$cumul)), size=0.5) + 
      theme_bw() + labs(x="SAE date", y="SAE cumulative number", color="Center")
    
    #convert to plotly (+specify tooltip)
    plotly::ggplotly(p, tooltip="text")
  })
  
  ## plot SAE barplot, color by sae characteristics
  output$sae_plot_3 <- renderPlotly({
    #get input: what factor to use as color? + map to df col name
    f <- input$sae_fact_sel
    if(f=="None") f <- NULL 
    else if(f=="Severity level") f <- "severity_level"
    else if(f=="Outcome") f <- "outcome"
    else if(f=="Causality") f <- "causality"
    else if(f=="Expectedness") f <- "expectedness"
    else if(f=="Death") f <- "death"
    else if(f=="Life threatening") f <- "life_threatening"
    else if(f=="Persistant disability") f <- "persistant_disability"
    else if(f=="Hospitalization") f <- "hospitalization"
    else if(f=="Congenital anomaly / birth defect") f <- "congenital_anomyla_birth_defect"
    
    d <- data.sae.filtered()
    if(!is.null(f)){
      a <- aggregate(data = d, as.formula(paste0(record_id, " ~ ", center, "*", f)), length)
      p <- ggplot(data = NULL, aes(x=a[,center], y=a[,record_id], fill=a[,f])) + 
        geom_col(aes(text=paste("Center:", a[,center], 
                                "<br>Outcome:", a[,f],
                                "<br>SAE count:", a[,record_id]))) + 
        theme_bw() + labs(x = "Center", y="SAE count", fill=input$sae_fact_sel)
      plotly::ggplotly(p, tooltip="text")
    }else{
      a <- aggregate(data = d, as.formula(paste0(record_id, " ~ ", center)), length)
      p <- ggplot(data = NULL, aes(x=a[,center], y=a[,record_id])) + 
        geom_col(aes(text=paste("Center:", a[,center], 
                                 "<br>SAE count:", a[,record_id]))) + 
        theme_bw() + labs(x = "Center", y="SAE count")
      plotly::ggplotly(p, tooltip="text")
    }
  })
  
  ## DataTable of SAE
  output$sae_table_1 <- renderDataTable({
    data.sae.filtered()[order(data.sae.filtered()[,sae_date]),]
  })

}
