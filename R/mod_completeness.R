#' Completeness
#' 
#' In development
#' @rdname mod_completeness
#' @param id standard shiny id argument
#' @param label standard shiny label argument
#' @import naniar
#' @importFrom waiter autoWaiter
mod_completeness_ui <- function(id, label){
  
  ns <- NS(id)
  
  tabItem(tabName = label,
          
          fluidRow(
            
            box(
              width = 3
              ## Forms filter
              , uiOutput(ns("form_name_selector"))
            )
          )
          
          , fluidRow(
            autoWaiter(),
            tabBox(width = 12,
                   title = "",
                   id = "tabset3",
                   height = "850px",
                   selected = ""
                   # Overview ----
                   , tabPanel(
                     "Overview"
                     , tabsetPanel(
                       tabPanel("Variables",
                                  height = "400",
                                  plotOutput(ns('vis_miss'), height = "750")
                       )
                       , tabPanel("Variable missingness",
                                  height = "400",
                                  plotlyOutput(ns('var_miss'), height = "750")
                       )
                       , tabPanel("Case missingness",
                                  height = "400",
                                  plotlyOutput(ns('case_miss'), height = "750")
                       )
                       , tabPanel("Missingness pattern",
                                  height = "300",
                                
                                  plotOutput(ns('miss_pattern'), height = "750")
                       )
                     )
                   )
                   # % missingness  ----
                   , tabPanel(
                     "Missingness - relationship with factor variables"
                     , height = "800", width = "400"
                     , column(
                       width = 3,
                      uiOutput(ns("fct_for_display_out")) 
                     , uiOutput(ns("variable_missingness_to_display_out")) 
                     )
                     , column(
                       width = 9,
                       plotlyOutput(ns("plot_missingness_factor"))
                     )
                   )
                   # ----
                   , tabPanel(
                     "Missingness - relationship with date"
                     , height = "400", width = "400"
                     , column(
                       width = 3,
                       uiOutput(ns("date_for_display_out")) 
                       , uiOutput(ns("variable_missingness_to_display_out_2")) 
                     ),
                     column(
                       width = 9,
                       plotlyOutput(ns("plot_missingness_with_time"))
                     )
                   )
                   # ----
                   , tabPanel(
                     "Missingness - relationship between variables"
                     , height = "400", width = "400"
                     , column(
                       width = 3
                       , uiOutput(ns("var_for_display_out"))
                       , uiOutput(ns("var_of_interest_out")) 
                     ) 
                     , column(
                       width = 9
                       , plotOutput(ns("plot_missingness")) 
                     )
                    )
            )
          )
  )
}

#' @rdname mod_completeness
#' @param input standard shiny input argument
#' @param output standard shiny output argument
#' @param session standard shiny session argument
#' @param data data for use in calculations
mod_completeness_server <- function(input, output, session, data){
  
  ns <- session$ns
  
  # set the font size for the plots
  set_font_size = theme(
    axis.text.x = element_text(size = 12)
    , axis.title.x = element_text(size = 14)
    , axis.text.y = element_text(size = 12)
    , axis.title.y = element_text(size = 14)
    , legend.text = element_text(size = 12)
    )
  
  # for overview tab ----
  # select form name
  output$form_name_selector <- renderUI({
    
    selectInput(
      inputId = ns("form_name"),
      label = "Select form:",
      choices = names(data()),
      selected = names(data())[1]
    )
    
  })
  
  form_selected <- reactive({
    
    return(
      data()[[input$form_name]]
      )
    
  })
  
  output$vis_miss <- renderPlot({
    form_selected() %>% visdat::vis_dat() + labs(y = "Patients") + coord_flip() +
      set_font_size
  })
  
  output$var_miss <- renderPlotly({
    form_selected() %>% gg_miss_var(show_pct = TRUE) %>% ggplotly
  })
  
  output$case_miss <- renderPlotly({
    p <- form_selected() %>% gg_miss_case(show_pct = TRUE) + labs(x = "Patients") 
    ggplotly(p)
  })
  
  
  
  output$miss_pattern <- renderPlot({
    form_selected() %>% gg_miss_upset(text.scale = 3, nset = 10)
  })
  

  # for missingness relationship with factor variable
  output$fct_for_display_out <- renderUI({
    # browser()
    selectInput(
      inputId = ns("fct_of_interest")
      ,label = "Factor variable selected:"
      , choices = prepare_choice_options( form_selected() %>% select_if(is.factor) )
    )
    
  })  
  
  output$variable_missingness_to_display_out <- renderUI({
    
    selectInput(inputId = ns("variable_missingness_to_display_in"),
                label = "Select variables to display missingness (by default only variable with misssing values are selected)",
                choices = prepare_choice_options( form_selected() %>% select( names( form_selected() ) %>% sort ) ),
                selected = form_selected() %>% miss_var_summary() %>% 
                  filter(pct_miss > 0) %>% pull(variable) %>% sort,
                multiple = TRUE)
    
  })

  output$plot_missingness_factor <- renderPlotly({

    if ( !is.null(input$fct_of_interest) ){
      
      ggplotly(
        gg_miss_fct(
          x = form_selected() %>% select(!!!syms(c(input$fct_of_interest, input$variable_missingness_to_display_in)))
          , fct = !!sym(input$fct_of_interest)
          ) + ylab("")
      )
      # browser()
    }
  })
  
  # for missingness relationship with time
  output$date_for_display_out <- renderUI({
    # browser()
    selectInput(
      inputId = ns("date_variable_of_interest")
      ,label = "Date variable selected:"
      , choices = prepare_choice_options( form_selected() %>% select_if( function(col) is.Date(col) | is.POSIXct(col) ) )
    )
    
  })  
  
  output$variable_missingness_to_display_out_2 <- renderUI({
    
    selectInput(inputId = ns("variable_missingness_to_display_in_2"),
                label = "Select variables to display missingness\n(by default only variable with misssing values are selected)",
                choices = prepare_choice_options(form_selected()),
                selected = form_selected() %>% miss_var_summary() %>% 
                  filter(pct_miss > 0) %>% pull(variable),
                multiple = TRUE)
    
  })
  
  output$plot_missingness_with_time <- renderPlotly({
    
    if ( !is.null(input$date_variable_of_interest) ){
      
      ggplotly(
        gg_miss_fct(
          x = form_selected() %>% select(!!!syms(c(input$date_variable_of_interest, input$variable_missingness_to_display_in_2))),
          fct = !!sym(input$date_variable_of_interest)
          ) + ylab("")
      )  
      
    }
  })
  
  # for missingness relationship between variables
  output$var_for_display_out <- renderUI({

    selectInput(
      inputId = ns("var_of_interest_displ")
      , label = "Display value of variable:"
      , choices = prepare_choice_options( form_selected() )
      , selected = prepare_choice_options(form_selected()[,1])
    )
    
  })
  
  output$var_of_interest_out <- renderUI({
    
    selectInput(
      inputId = ns("var_of_interest_miss")
      ,label = "depending on missingness of:"
      , choices = prepare_choice_options( form_selected() )
      , selected = prepare_choice_options( form_selected()[,2] )
    )
    
  })
  
  output$plot_missingness <- renderPlot({

      d = form_selected() %>% select(c(input$var_of_interest_miss, input$var_of_interest_displ)) %>% 
      bind_shadow()
    
    if ( is.factor( d[[2]] ) ) {
      # browser()
      p1 = ggplot(data = d, aes(fill = .data[[ paste0(input$var_of_interest_miss, "_NA")]], x = .data[[input$var_of_interest_displ]])) + 
        geom_bar()
      p2 = ggplot(data = d, aes(fill = .data[[ paste0(input$var_of_interest_miss, "_NA")]], x = .data[[input$var_of_interest_displ]])) + 
        geom_bar(position = "fill") + ylab("proportion")
      p = ggpubr::ggarrange(p1, p2, ncol = 2, common.legend = TRUE, legend = "bottom") + set_font_size
      return(p)
      
    } else if ( is.numeric( d[[2]]) ) {
      p = ggplot(data = d, aes(fill = .data[[ paste0(input$var_of_interest_miss, "_NA")]], x = .data[[input$var_of_interest_displ]])) + 
        geom_density(alpha = .3) + set_font_size
      return(p)
    }

  })
  
  
}