#' Completeness
#' 
#' In development
#' @rdname mod_completeness
#' @param id standard shiny id argument
#' @param label standard shiny label argument
#' @import naniar
#' @importFrom waiter autoWaiter
mod_completeness_ui <- function(id, label){
  
  # few settings ----
  ns <- NS(id)
  
  tabItem(tabName = label,
          tabsetPanel(
            # Box general info----
            fluidRow(
              valueBoxOutput(ns("box_overall_missing"), width = 6) 
            )
            , fluidRow(
              tabsetPanel(
                # Main Tab: all forms ----
                tabPanel(
                  "Completeness all forms"
                  , tabsetPanel(
                    tabPanel(
                      "Overview form missingness",
                      plotOutput(ns("overview_form_missingness"), height = "500",  width = "1200")
                    )
                    , tabPanel(
                      "Missingness over time",
                      plotOutput(ns("overview_cum_missigness"), height = "500",  width = "1200"),
                    )
                  )
                )
                # Main Tab: by form ----
                , tabPanel(
                  "Completeness by form"
                  , fluidRow(
                    br()
                    , box(
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
                           # TAB: Overview ----
                           , tabPanel(
                             # ----
                             "Overview"
                             , tabPanel(
                               # ----
                               "Overview"
                               , tabsetPanel(
                                 tabPanel("Variables",
                                          plotOutput(ns('vis_miss'), height = "500", width = "1200")
                                 )
                                 , tabPanel("Variable missingness",
                                            plotlyOutput(ns('var_miss'), height = "500", width = "1200")
                                 )
                                 , tabPanel("Case missingness",
                                            plotlyOutput(ns('case_miss'), height = "500", width = "1200")
                                 )
                                 , tabPanel("Missingness pattern",
                                            column(
                                              width = 3,
                                              uiOutput(ns("nset_slider_out")),
                                              uiOutput(ns("upset_text_scale_out"))
                                            )
                                            , column(
                                              width = 9,
                                              plotOutput(ns('miss_pattern'), height = "500")
                                            )
                                 )
                               )
                             )
                           )
                           # TAB: Missigness_relationship ----
                           , tabPanel(
                             # ----
                             "Missingness relationship"
                             , tabsetPanel(
                               # Sub-TAB: % missingness ~ fct variables  ----
                               tabPanel(
                                 "with factor variables"
                                 , column(
                                   width = 3
                                   , br()
                                   , uiOutput(ns("fct_for_display_out")) 
                                   , uiOutput(ns("variable_missingness_against_factor_out"))
                                   , p("Note: by default only variable with missing values are selected")
                                 )
                                 , column(
                                   width = 9,
                                   br()
                                   , plotlyOutput(ns("plot_missingness_against_factor"), height = "650", width = "750")
                                 )
                               )
                               # Sub-TAB: missingness ~ date ----
                               , tabPanel(
                                 "with date"
                                 , column(
                                   width = 3
                                   , br()
                                   ,uiOutput(ns("date_for_display_out")) 
                                   , uiOutput(ns("variable_missingness_against_date_out")) 
                                   , p("Note: by default only variable with missing values are selected")
                                 ),
                                 column(
                                   width = 9
                                   , br()
                                   , plotlyOutput(ns("plot_missingness_against_date"), height = "650")
                                 )
                               )
                               # Sub-TAB: ditrib quant var ~ missingness other variable ----
                               , tabPanel(
                                 "distribution ~ missingness"
                                 , column(
                                   width = 3
                                   , uiOutput(ns("var_distrib_against_missingness_out"))
                                   , uiOutput(ns("var_missingness_for_distrib_out")) 
                                 ) 
                                 , column(
                                   width = 9
                                   , tags$head(
                                     tags$style(HTML("
                                      .shiny-output-error-validation {
                                        color: #ff0000;
                                        font-weight: bold;
                                      }
                                    "))
                                   )
                                   , plotOutput(ns("plot_distrib_against_missingness"), height = "650", width = "750") 
                                 )
                               )
                               
                               # Sub-TAB: quant var ~ other quant var ----
                               , tabPanel(
                                 "quantitative variables"
                                 , column(
                                   width = 3
                                   , uiOutput(ns("var_1_out"))
                                   , uiOutput(ns("var_2_out")) 
                                 ) 
                                 , column(
                                   width = 9
                                   , plotOutput(ns("plot_missing_relation_2_var"), height = "650", width = "750") 
                                 )
                               )
                             )
                           )
                           # ----
                    )
                  )
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
  
  # few settings ----
  ns <- session$ns
  
  set_font_size = theme(
    axis.text.x = element_text(size = 12)
    , axis.title.x = element_text(size = 14)
    , axis.text.y = element_text(size = 12)
    , axis.title.y = element_text(size = 14)
    , legend.text = element_text(size = 12)
    )
  
  # Box general info ----
  output$box_overall_missing <- renderValueBox({
    d = data()
    n_forms = length(d)
    n_var = purrr::map_dbl(d, ncol) %>% sum
    mean_miss = round( sum( purrr::map_dbl(d, ~ sum(is.na(.x))) ) / sum( purrr::map_dbl(d, ~prod(dim( .x))) ) * 100, 2 )
    valueBox(value = paste0(n_forms, " forms / ", n_var, " variables / ", mean_miss, "% missing"),
             subtitle = "", color = "yellow")
  })
  # Main Tab: all forms ----
  output$overview_form_missingness <- renderPlot({
    tibble( form = names(data()), perc.miss = purrr::map_dbl(d, pct_miss) ) %>% 
      ggplot(aes(x = form, y=perc.miss)) + geom_col() + ylim(0,100) + coord_flip() + 
      labs(x = "Forms", y = "Percentage missing") + theme_minimal() + 
      theme(
        axis.text.x = element_text(size = 16)
        , axis.title.x = element_text(size = 20)
        , axis.text.y = element_text(size = 16)
        , axis.title.y = element_text(size = 20)
      )
  })
  
  output$overview_cum_missigness <- renderPlot({
    d = data()
    len = length(d)
    n_col_plot = 3
    n_row_plot = len %/% n_col_plot + 1
    plot_list = list()
    for (p in 1:len){
      plot_list[[p]] =  d[[p]] %>% arrange(rando_date.date) %>% 
        gg_miss_case_cumsum() + 
        labs(title = names(d)[p], x = "Patient (sorted by randomization date)") + 
        theme(
          title = element_text(size = 16)
          , axis.text.x = element_text(size = 12)
          , axis.title.x = element_text(size = 14)
          , axis.text.y = element_text(size = 12)
          , axis.title.y = element_text(size = 14)
          , legend.text = element_text(size = 12)
        )
    }
    
    ggpubr::ggarrange(
      plotlist = plot_list, ncol = n_col_plot, nrow = n_row_plot, common.legend = TRUE
    )
    
  })
  # Main Tab: by form ----
  # TAB Overview ----
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
  
  output$nset_slider_out <- renderUI({
    
    sliderInput(ns("nset_slider_in"),
                "Choose number of sets to display:",
                min = 2,
                max = ncol( form_selected() ),
                value = 10,
                step = 1
    )
    
  })
  
  output$upset_text_scale_out <- renderUI({
    
    sliderInput(ns("upset_text_scale_in"),
                "Choose text scale:",
                min =1,
                max = 5,
                value = 2,
                step = .1
    )
    
  })
  
  output$miss_pattern <- renderPlot({
    if ( is.null( input$nset_slider_in ) ){
      form_selected() %>% gg_miss_upset(text.scale = input$upset_text_scale_in)
    } else {
      form_selected() %>% gg_miss_upset(text.scale = input$upset_text_scale_in, nset = input$nset_slider_in) 
    }
  })
  
  # TAB: Missigness_relationship ----
  # Sub-TAB:  % missingness ~ fct variables ----
  output$fct_for_display_out <- renderUI({
    # browser()
    selectInput(
      inputId = ns("fct_of_interest")
      ,label = "Factor variable selected for grouping:"
      , choices = prepare_choice_options( form_selected() %>% select_if(is.factor) )
    )
    
  })  
  
  output$variable_missingness_against_factor_out <- renderUI({
    
    selectInput(inputId = ns("variable_missingness_against_factor_in"),
                label = "Select variables to display missingness:",
                choices = prepare_choice_options( form_selected() %>% select( names( form_selected() ) %>% sort ) ),
                selected = form_selected() %>% get_colnames_missing,
                multiple = TRUE)
    
  })

  output$plot_missingness_against_factor <- renderPlotly({

    if ( !is.null(input$fct_of_interest) ){
      
      ggplotly(
        gg_miss_fct(
          x = form_selected() %>% select(!!!syms(c(input$fct_of_interest, input$variable_missingness_against_factor_in)))
          , fct = !!sym(input$fct_of_interest)
          ) + ylab("")
      )
    }
  })
  
  # Sub-TAB: missingness ~ date ----
  output$date_for_display_out <- renderUI({
    # browser()
    selectInput(
      inputId = ns("date_variable_of_interest")
      ,label = "Date variable selected:"
      , choices = prepare_choice_options( form_selected() %>% select_if( function(col) is.Date(col) | is.POSIXct(col) ) )
    )
    
  })  
  
  output$variable_missingness_against_date_out <- renderUI({
    
    selectInput(inputId = ns("variable_missingness_against_factor_in_2"),
                label = "Select variables to display missingness:",
                choices = prepare_choice_options(form_selected()),
                selected = form_selected() %>% get_colnames_missing,
                multiple = TRUE)
    
  })
  
  output$plot_missingness_against_date <- renderPlotly({
    
    if ( !is.null(input$date_variable_of_interest) ){
      
      ggplotly(
        gg_miss_fct(
          x = form_selected() %>% select(!!!syms(c(input$date_variable_of_interest, input$variable_missingness_against_factor_in_2))),
          fct = !!sym(input$date_variable_of_interest)
          ) + ylab("")
      )  
      
    }
  })
  
  # Sub-TAB: ditrib quant var ~ missingness other variable ----
  output$var_distrib_against_missingness_out <- renderUI({

    selectInput(
      inputId = ns("var_distrib_against_missingness_in")
      , label = "Display distribution of variable:"
      , choices = prepare_choice_options( form_selected() %>% select_if(is.numeric) )
      , selected = prepare_choice_options( (form_selected() %>% select_if(is.numeric))[,1])
    )
    
  })
  
  output$var_missingness_for_distrib_out <- renderUI({
    
    selectInput(
      inputId = ns("var_missingness_for_distrib_miss")
      ,label = "depending on missingness of:"
      , choices = prepare_choice_options( form_selected()[form_selected() %>% get_colnames_missing ] )
      , selected = prepare_choice_options( form_selected()[form_selected() %>% get_colnames_missing ] )
    )
    
  })
  
  output$plot_distrib_against_missingness <- renderPlot({

      d = form_selected() %>% select(c(input$var_missingness_for_distrib_miss, input$var_distrib_against_missingness_in)) %>% 
      bind_shadow()
      
      if (input$var_missingness_for_distrib_miss == input$var_distrib_against_missingness_in){
        br()
        validate("Please, chose a different variable for missingness than for the distribution.")
      } else{
        ggplot(data = d, aes(fill = .data[[ paste0(input$var_missingness_for_distrib_miss, "_NA")]], x = .data[[input$var_distrib_against_missingness_in]])) + 
          geom_density(alpha = .3) + theme_minimal() + set_font_size
      }
    
  })
  
  # Sub-TAB: quant var ~ other quant var ----
  output$var_1_out <- renderUI({
    
    selectInput(
      inputId = ns("var_1_in")
      , label = "First variable:"
      , choices = prepare_choice_options( form_selected() %>% select_if( function(col) is.numeric(col) && !is.integer(col) ) )
      , selected = prepare_choice_options( ( form_selected() %>% select_if( function(col) is.numeric(col) && !is.integer(col) ) )[,1] )
    )
    
  })
  
  output$var_2_out <- renderUI({
    
    selectInput(
      inputId = ns("var_2_in")
      ,label = "Second variable:"
      , choices = prepare_choice_options( form_selected() %>% select_if( function(col) is.numeric(col) && !is.integer(col) ) )
      , selected = prepare_choice_options( ( form_selected() %>% select_if( function(col) is.numeric(col) && !is.integer(col) ) )[,1] )
    )
    
  })
  
  output$plot_missing_relation_2_var <- renderPlot({
    
    if (is.null(input$var_1_in) || is.null(input$var_2_in)){
      validate("Select variables to be plotted")
    } else if (input$var_1_in == input$var_2_in){
      validate("Select two distinct variable")
    } else {
      d = form_selected() %>% select(c(input$var_1_in, input$var_2_in))
      ggplot(data = d, aes(x = .data[[ input$var_1_in ]], y = .data[[ input$var_2_in ]])) + 
        geom_miss_point() + theme_minimal() + set_font_size
    }
      
  })
  
  
}
