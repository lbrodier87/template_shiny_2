#' Serious Adverse Events
#'
#' In development
#' @rdname mod_sae
#' @param id standard shiny id argument
#' @param label standard shiny label argument
#' @author Laurent Brodier
mod_sae_ui <- function(id, label){
  ns <- NS(id)
  tabItem(tabName = label,
          fluidRow(
            tabBox(width = 12,
                   title = "",
                   id = "tabset2",
                   height = "450px",
                   
                   tabPanel("SAE Plot", 
                            "SAE plot overtime:",
                            plotlyOutput(ns("sae_plot_1"))
                            ),
                   tabPanel("SAE Plot by center",
                            "SAE plot overtime by center:",
                           plotlyOutput(ns("sae_plot_2"))
                           ), 
                   tabPanel("SAE count by factor", 
                            "SAE count by center and by characteristics: ", 
                            selectInput(ns("sae_fact_sel"), 
                                        choices = c("None", "Severity level", "Outcome", "Causality", 
                                                    "Expectedness", "Death", "Life threatening", "Persistant disability", 
                                                    "Hospitalization", "Congenital anomaly / birth defect"), 
                                        label = "Select SAE characteristics to display as color: "),
                            #"add filters with code generated choices",
                            #Check order of levels on selection! 
                            plotlyOutput(ns("sae_plot_3"))
                            ), 
                   tabPanel("SAE List",
                            "SAE List: ",
                            tableOutput(ns("sae_table_1"))
                   )
              )
            )
          )
}

#' @rdname mod_sae
#' @param input standard shiny input argument
#' @param output standard shiny output argument
#' @param session standard shiny session argument
#' @param data data for use in calculations
mod_sae_server <- function(input, output, session, data.sae){

  ns <- session$ns
  
  #constants - to map to your dataset variable names
  record_id <- "pat_id"
  center <- "centre.short"
  sae_date <- "sae_date"

  #plot of cumulative SAE number (all centers)
  output$sae_plot_1 <- renderPlotly({
    d <- data.sae[order(data.sae[,sae_date]),] #order the SAE by date
    d$cumul <- seq_along(d[,sae_date]) #get cumulative count of SAE
    a <- aggregate(data = d, as.formula(paste0("cumul ~ ", sae_date)), max) #aggregate max (to acount for <1 SAE on same date)
    p <- ggplot(data = NULL, aes(x=d[,sae_date], y=d$cumul)) + geom_line() + 
      theme_bw() + labs(x="date", y="SAE cumulative number")
    plotly::ggplotly(p)
  })
  
  #plot of cumulative SAE number (color by center)
  output$sae_plot_2 <- renderPlotly({
    d <- data.sae[order(data.sae[,sae_date]),] #order the SAE by date
    d$cumul <- seq_along(d[,sae_date]) #get cumulative count of SAE
    a <- aggregate(data = d, as.formula(paste0("cumul ~ ", sae_date)), max) #aggregate max (to acount for <1 SAE on same date)
    p <- ggplot(data = NULL, aes(x=d[,sae_date], y=d$cumul, color=d[,center])) + geom_line() + 
      theme_bw() + labs(x="date", y="SAE cumulative number", color="Center")
    plotly::ggplotly(p)
  })
  
  #plot SAE bar, color by category
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
    
    if(!is.null(f)){
      a <- aggregate(data = data.sae, as.formula(paste0(record_id, " ~ ", center, "*", f)), length)
      p <- ggplot(data = NULL, aes(x=a[,center], y=a[,record_id], fill=a[,f])) + 
        geom_col() + theme_bw() + labs(x = "Center name", y="SAE count", fill=input$sae_fact_sel)
    }else{
      a <- aggregate(data = data.sae, as.formula(paste0(record_id, " ~ ", center)), length)
      p <- ggplot(data = NULL, aes(x=a[,center], y=a[,record_id])) + 
        geom_col() + theme_bw() + labs(x = "center name", y="SAE count")
    }
  })
  
  output$sae_table_1 <- renderTable({
    data.sae
  })

}
