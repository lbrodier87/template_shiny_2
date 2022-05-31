#' Serious Adverse Events
#'
#' In development
#' @rdname mod_sae
#' @param id standard shiny id argument
#' @param label standard shiny label argument
#' 
#' @author Laurent Brodier
#' @import ggplot2
#' @import plotly
#' @import reshape2
#' @import DT
mod_sae_ui <- function(id, label){
  ns <- NS(id)
  tabItem(tabName = label,
          fluidRow(
            # SAE filters (for all tabs)
            box(width = 12,
              #Filters choices are generated on server side, based on dataset content
              fluidRow( 
                column(4, uiOutput(ns("sae_filter_report_type_ui"))), 
                column(4, uiOutput(ns("sae_filter_severity_ui"))),
                column(4, uiOutput(ns("sae_filter_expectedness_ui")))
              ),
              fluidRow( 
                column(6, uiOutput(ns("sae_filter_outcome_ui"))), 
                column(6, uiOutput(ns("sae_filter_causality_ui")))
              )
            ),
            
            # Tabs with plots
            tabBox(width = 12,
                   title = "",
                   id = "tabset2",
                  
                   tabPanel("SAE occurence overtime", 
                            h2("Cummulative SAE occurence overtime:"),
                            plotlyOutput(ns("sae_plot_1")), 
                            br(),
                            h2("Cummulative SAE occurence overtime by center:"),
                            plotlyOutput(ns("sae_plot_2"))
                            ),
                   tabPanel("SAE number by characteristics", 
                            h2("SAE number by center and by characteristics: "), 
                            uiOutput(ns("sae_fact_sel_ui")),
                            plotlyOutput(ns("sae_plot_3")), 
                            br(),
                            h2("SAE count table:"), 
                            uiOutput(ns("sae_table_detail_var_ui")), 
                            tableOutput(ns("sae_table")),
                            ), 
                   tabPanel(width=12, "SAE list",
                            h2("SAE list:"),
                            div(DT::dataTableOutput(ns("sae_table_1")), style = "font-size: 90%; width: 100%")
                            ),
                   tabPanel(width=12, "SAE Follow-up",
                            h2("SAE Follow-up:"),
                            p("in development...")
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
  
  # varname mapping - to map to your dataset variable names (useful?)
  record_id <- "pat_id"
  center <- "centre.short"
  sae_date <- "sae_date"
  sae_report_type <- "sae_report_type"
  severity_level <- "severity_level"
  expectedness <- "expectedness"
  causality <- "causality"
  outcome <- "outcome"

  # reactive filtered data based on SAE characteristics, used in plots below
  data.sae.filtered <- reactive({
    d <- data.sae()
    d <- d[d[,severity_level] %in% input$sae_filter_severity,]
    d <- d[d[,outcome] %in% input$sae_filter_outcome,]
    d <- d[d[,causality] %in% input$sae_filter_causality,]
    d <- d[d[,expectedness] %in%  input$sae_filter_expectedness,]
    d <- d[d[,sae_report_type] %in% input$sae_filter_report_type,]
    d
  })
  
  
  ## plot of cumulative SAE number (all centers)
  output$sae_plot_1 <- renderPlotly({
    d <- data.sae.filtered()
    d <- d[order(d[,sae_date]),] #order the SAE by date
    d$cumul <- seq_along(d[,sae_date]) #get cumulative count of SAE
    a <- aggregate(data = d, as.formula(paste0("cumul ~ ", sae_date)), max) #aggregate max (to acount for >1 SAE on same date)
    #do the plot (+add custom label to hover text as points)
    p <- ggplot() + geom_line(aes(x=a[,sae_date], y=a$cumul, color=paste0("All (n=", max(a$cumul), ")"))) + 
      geom_point(aes(x=a[,sae_date], y=a$cumul, text=paste("SAE date:", a[,sae_date], 
                                                           "<br>SAE count:", a$cumul)), size=0.5) +
      theme_bw() + labs(x="SAE date", y="SAE cumulative number", color="Center") + 
      scale_color_manual(values=c("#000000"))
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
    
    #get total SAE nb by center in data frame, to add in center label on plot
    m <- aggregate(data = d, as.formula(paste0("cumul ~ ", center)), max)
    names(m)[2] <- "tot"
    d <- merge(x = d, y = m, by = center, all.x = T)
    d <- d[order(d[,sae_date]),]
    
    #do the plot (+add custom label to hover text as points)
    p <- ggplot() +  geom_line(aes(x=d[,sae_date], y=d$cumul, color=paste0(d[,center], " (n=", d$tot, ")") )) + 
      geom_point(aes(x=d[,sae_date], y=d$cumul, color=paste0(d[,center], " (n=", d$tot, ")"), 
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
        geom_col(aes(text=paste0("Center: ", a[,center], 
                                "<br>", input$sae_fact_sel, ": ", a[,f],
                                "<br>SAE count: ", a[,record_id]))) + 
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

  # Data Table of SAE count aggregate
  output$sae_table <- renderTable({
    if(input$sae_table_detail_var != "None"){
      sae_table <- aggregate(data = data.sae.filtered(), as.formula(paste0(record_id, "~", input$sae_table_detail_var, "*", center)), length)
      sae_table <- reshape2::dcast(sae_table, formula = as.formula(paste0(center, " ~ ", input$sae_table_detail_var)))
      if( length(unique(sae_table[,center])) > 1 ){
        sae_sum <- apply(as.data.frame(sae_table[,2:ncol(sae_table)]), 2, function(x){sum(x, na.rm = T)})
        sae_table <- rbind(c("All", sae_sum), sae_table)
      }
      sae_table <- sae_table[, c(1, ncol(sae_table):2)]
      names(sae_table)[1] <- "Center"
      if(ncol(sae_table) > 2){
        sae_table$Total <- apply(sae_table[,2:ncol(sae_table)], 1, function(x){as.character(sum(as.numeric(x), na.rm=T))})
      }
    }else{
      sae_table <- aggregate(data=data.sae.filtered(), as.formula(paste0(record_id, " ~ ", center)), length)
      names(sae_table) <- c("Center", "Total")
      if( length(unique(sae_table$Center)) > 1 ){
        sae_table <- rbind( c("All", as.character(sum(as.numeric(sae_table$Total)))), sae_table )
      }
    }
    sae_table
  })
    
  ## DataTable of all SAE
  output$sae_table_1 <- DT::renderDataTable({
    DT::datatable(data.sae.filtered()[order(data.sae.filtered()[,sae_date]),], 
                  rownames = FALSE, 
                  options = list(scrollX = T, pageLength = 10))
  })

  ## Dynamic UI code
  #list of content - report type
  sae_report_type_list <- reactive({
    unique(data.sae()[, sae_report_type])
  })
  #list of content - SAE severity level
  severity_level_list <- reactive({
    unique(data.sae()[, severity_level])
  })
  #list of content - SAE expectedness
  sae_expectedness_list <- reactive({
    unique(data.sae()[, expectedness])
  })
  #list of content - SAE causality
  sae_causality_list <- reactive({
    unique(data.sae()[, causality])
  })
  #list of content - SAE outcome
  sae_outcome_list <- reactive({
    unique(data.sae()[, outcome])
  })
  #list of content - SAE variables
  sae_var_list <- reactive({
    names(data.sae())[!names(data.sae()) %in% c(record_id, sae_date, center)]
  })
  
  # Dynamic UI - report type
  output$sae_filter_report_type_ui <- renderUI({
    choices <- sae_report_type_list()
    selectInput(inputId = ns('sae_filter_report_type'),
                label = "Filter by report type", 
                choices = choices, multiple = T, 
                selected = choices)
  })
  # Dynamic UI - SAE severity
  output$sae_filter_severity_ui <- renderUI({
    choices <- severity_level_list()
    selectInput(ns("sae_filter_severity"), 
                label = "Filter by severity", 
                choices = choices, multiple = T, 
                selected = choices) 
  })
  # Dynamic UI - SAE expectedness
  output$sae_filter_expectedness_ui <- renderUI({
    choices <- sae_expectedness_list()
    selectInput(ns("sae_filter_expectedness"), 
                label = "Filter by expectedness",  
                choices = choices, multiple = T, 
                selected = choices) 
  })
  # Dynamic UI - SAE causality
  output$sae_filter_causality_ui <- renderUI({
    choices <- sae_causality_list()
    selectInput(ns("sae_filter_causality"), 
                label = "Filter by causality",  
                choices = choices, multiple = T, 
                selected = choices) 
  })
  # Dynamic UI - SAE outcome
  output$sae_filter_outcome_ui <- renderUI({
    choices <- sae_outcome_list()
    selectInput(ns("sae_filter_outcome"), 
                label = "Filter by outcome",  
                choices = choices, multiple = T, 
                selected = choices) 
  })
  # Dynamic UI - SAE variables
  output$sae_table_detail_var_ui <- renderUI({
    choices <- c("None", sae_var_list())
    selectInput(ns("sae_table_detail_var"), 
                label = "Detail by variable",  
                choices = choices, multiple = F, 
                selected = "None") 
  })
  output$sae_fact_sel_ui <- renderUI({
    choices <- c("None", sae_var_list())
    selectInput(ns("sae_fact_sel"), 
                label = "Detail by variable",  
                choices = choices, multiple = F, 
                selected = "None") 
  })
}
