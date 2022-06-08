#' Consistency
#'
#' In development
#' @rdname mod_consistency
#' @param id standard shiny id argument
#' @param label standard shiny label argument
#' 
#' @author Silvia Grieder
#' @import ggplot2
#' @import dplyr
#' @import stringr
#' @import DT
#' @import ggdist
#' 
mod_consistency_ui <- function(id, label){
  
  ns <- NS(id)
  
  tabItem(tabName = label,
          fluidRow(
            tabBox(width = 12,
                   title = "",
                   id = "tabset2",
                   height = "450px",
                   tabPanel("Numeric variables",
                            uiOutput(ns("numvar_out")),
                            selectInput(inputId = ns("round_num"),
                                        label = "Number of digits to round to:",
                                        choices = 0:5,
                                        selected = 0),
                            uiOutput(ns("desmin")),
                            uiOutput(ns("desmax")),
                            tableOutput(outputId = ns("minmax")),
                            tabsetPanel(tabPanel("Distribution",
                                                 plotOutput(outputId = ns("histPlot"))),
                                        tabPanel("List of entries",
                                                 br(),
                                                 h4("Shows entries that fall below minimum or exceed maximum"),
                                                 br(),
                                                 uiOutput(ns("idvar1_out")),
                                                 DTOutput(outputId = ns("numEntryList"))
                                                 )
                                        )
                   ),
                   tabPanel("Character variables",
                            uiOutput(ns("charvar_out")),
                            textInput(inputId = ns("filtText"),
                                      label = "Filter entries for this text"),
                            checkboxInput(inputId = ns("matchCase"),
                                          label = "Match case?",
                                          value = FALSE),
                            checkboxInput(inputId = ns("alphanum"),
                                          label = "Ignore all non-alphanumeric characters?",
                                          value = FALSE),
                            tabsetPanel(tabPanel("Frequencies",
                                                 uiOutput(ns("char_freq_out")),
                                                 conditionalPanel(condition = "input.char_freq_in == 'plot'",
                                                                  ns = ns,
                                                                  plotOutput(outputId = ns("freqPlot"))),
                                                 conditionalPanel(condition = "input.char_freq_in == 'table'",
                                                                  ns = ns,
                                                                  DTOutput(outputId = ns("freqTab")))),
                                        tabPanel("List of entries",
                                                 br(),
                                                 h4("Shows either all entries or, if a text is inputted in the filter box, the entries including this text."),
                                                 br(),
                                                 uiOutput(ns("idvar2_out")),
                                                 tableOutput(outputId = ns("hitsEntries")),
                                                 br(),
                                                 DTOutput(outputId = ns("charEntryList"))
                                        ),
                                        tabPanel("List of unique entries",
                                                 br(),
                                                 h4("Shows either all unique entries or, if a text is inputted in the filter box, the unique entries including this text."),
                                                 tableOutput(outputId = ns("hitsUniqueEntries")),
                                                 br(),
                                                 DTOutput(outputId = ns("charUniqueEntryList"))
                                        )
                                        )
                            )
                   
                   )
            )
          
          )
  
}

#' @rdname mod_consistency
#' @param input standard shiny input argument
#' @param output standard shiny output argument
#' @param session standard shiny session argument
#' @param data data for use in calculations
mod_consistency_server <- function(input, output, session, data){
  
  ns <- session$ns
  
  # Reactive select inputs
  output$numvar_out <- renderUI({

    selectInput(inputId = ns("numvar_in"),
                label = "Select variable of interest:",
                choices = colnames(data() %>% select(where(is.numeric))),
                selected = colnames(data() %>% select(where(is.numeric)))[1])

  })

  output$charvar_out <- renderUI({

    selectInput(inputId = ns("charvar_in"),
                label = "Select variable:",
                choices = colnames(data() %>% select(where(is.character))),
                selected = colnames(data() %>% select(where(is.numeric)))[1])

  })

  output$idvar1_out <- renderUI({

    selectInput(inputId = ns("idvar1_in"),
                label = "Select additional variable(s) to be displayed:",
                choices = colnames(data()),
                selected = colnames(data() %>% select(where(is.numeric)))[1],
                multiple = TRUE)

  })
  
  output$idvar2_out <- renderUI({
    
    selectInput(inputId = ns("idvar2_in"),
                label = "Select variable:",
                choices = colnames(data()),
                selected = colnames(data() %>% select(where(is.numeric)))[1])
    
  })
  
  output$char_freq_out <- renderUI({
  
   selectInput(inputId = ns("char_freq_in"),
               label = "Choose plot or table:",
               choices = c(Plot = "plot", Table = "table"),
               selected = ifelse(length(levels(react_char_dat() %>% pull(input$charvar_in))) <= 15,
                                 "plot", "table"))

  })

  # Reactive slider inputs: set min and max according to min and max of actual data
  # Select desired minimum and maximum
  output$desmin <- renderUI({

    sliderInput(ns("minSlider"),
                "Choose allowed minimum",
                min = round(min(data()[, input$numvar_in]), as.numeric(input$round_num)),
                max = round(max(data()[, input$numvar_in]), as.numeric(input$round_num)),
                value = round(min(data()[, input$numvar_in]), as.numeric(input$round_num)),
                round = -as.numeric(input$round_num),
                step = ifelse(as.numeric(input$round_num) == 0, 1,
                              as.numeric(paste0("0.",
                                         paste0(rep("0", (as.numeric(input$round_num) - 1)),
                                                collapse = ""),
                                         "1")))
                )
    
  })

  output$desmax <- renderUI({

    sliderInput(ns("maxSlider"),
                "Choose allowed maximum",
                min = round(min(data()[, input$numvar_in]), as.numeric(input$round_num)),
                max = round(max(data()[, input$numvar_in]), as.numeric(input$round_num)),
                value = round(max(data()[, input$numvar_in]), as.numeric(input$round_num)),
                round = -as.numeric(input$round_num),
                step = ifelse(as.numeric(input$round_num) == 0, 1,
                              as.numeric(paste0("0.",
                                                paste0(rep("0", (as.numeric(input$round_num) - 1)),
                                                       collapse = ""),
                                                "1"))))

  })

  # Create histogram for selected numeric variable
  output$histPlot <- renderPlot({

    #ggplotly(
      ggplot(data = data(), aes(x = centre.short, y = .data[[input$numvar_in]])) +
        geom_violin(fill = "#00BA38", alpha = 0.5) +
        geom_boxplot(width = .12, fill = "white", color = "black") +
        #stat_summary(fun.data = "mean_sdl", mult = 1, geom = "pointrange") +
        geom_jitter(position = position_jitter(0.2),
                    shape = 16) +
        xlab("Centre") +
        annotate("rect", xmin = -Inf, xmax = Inf, ymin = as.numeric(input$maxSlider), ymax = Inf,
                 alpha = 0.2, fill = "red") +
        annotate("rect", xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = as.numeric(input$minSlider),
                 alpha = 0.2, fill = "red") +
        theme_bw()
    #) %>% 
      # This does not work with plotly, annotate() does not work either
      # layout(shapes = list(
      #        list(type = "rect",
      #             fillcolor = "red", line = list(color = "blue"), opacity = 0.3,
      #             x0 = -1, x1 = 2, xref = "x",
      #             y0 = 0, y1 = 12.5, yref = "y")))

  })

  # Create table with actual minimum and maximum values of the selected numeric variable
  output$minmax <- renderTable({

    data() %>%
      summarize(`Actual minimum` = min(.data[[input$numvar_in]]),
                `Actual maximum` = max(.data[[input$numvar_in]]),
                `Number of entries below allowed minimum` = sum(.data[[input$numvar_in]] < as.numeric(input$minSlider)),
                `Number of entries above allowed maximum` = sum(.data[[input$numvar_in]] > as.numeric(input$maxSlider)))

  })

  # List all entries of selected numeric variable
  output$numEntryList <- renderDT({

    data() %>%
      select(!!!syms(c(input$idvar1_in, input$numvar_in))) %>%
      filter(.data[[input$numvar_in]] < as.numeric(input$minSlider) |
               .data[[input$numvar_in]] > as.numeric(input$maxSlider)) %>%
      mutate(across(where(is.numeric), ~ round(.x, digits = as.numeric(input$round_num)))) %>% 
      arrange(.data[[input$numvar_in]])

  })
  
  # Render reactive data element according to filter variables
  react_char_dat <- reactive({

    # Convert all character variables to factors for later use in plots
    react_dat <- data() %>% 
      mutate(across(where(is.character), as.factor))
    
    # Filter for input text, if provided.
    if(!is.na(input$filtText) & !is.null(input$filtText) & input$filtText != ""){
      
      # Match case and ignore all non-alphanumeric characters, if both required
      # Note: the \x7f-\xff is needed to match German Umlaute and other special letters
      if(isTRUE(input$matchCase) & isTRUE(input$alphanum)){
        
        react_dat <- react_dat %>%
          filter(str_detect(str_remove_all(.data[[input$charvar_in]],
                                           "[^a-zA-Z0-9\x7f-\xff]"),
                            str_remove_all(input$filtText,
                                           "[^a-zA-Z0-9\x7f-\xff]")))
        
        # Only match case
      } else if(isTRUE(input$matchCase) & isFALSE(input$alphanum)) {
        
        react_dat <- react_dat %>%
          filter(str_detect(.data[[input$charvar_in]], input$filtText))
        
        # Ignore non-alphanumeric characters, but do not match case
      } else if(isFALSE(input$matchCase) & isTRUE(input$alphanum)) {
        
        react_dat <- react_dat %>%
          filter(str_detect(tolower(str_remove_all(.data[[input$charvar_in]],
                                                   "[^a-zA-Z0-9\x7f-\xff]")),
                            tolower(str_remove_all(input$filtText,
                                                   "[^a-zA-Z0-9\x7f-\xff]"))))
        
        # Do not match case or ignore non-alphanumeric characters if none of the check boxes is checked
      } else {
        
        react_dat <- react_dat %>%
          filter(str_detect(tolower(.data[[input$charvar_in]]), tolower(input$filtText)))
        
      }
      
    }
      
    # If no text is provided, return the all entries (= unchanged react_dat)
    
    return(react_dat)
    
  })
  

  # Barplot with frequencies (with bars sideways to handle many categories)
  output$freqPlot <- renderPlot({
    
    if(!is.na(input$filtText) & 
       !is.null(input$filtText) &
       input$filtText != "" & 
       !(input$filtText %in% react_char_dat()[input$charvar_in])){
      validate("The string pattern was not found in the data for the selected variable")
    }
    
    # convert character vectors to factors for plotting
    sumvar_name <- paste0(input$charvar_in, "_sum")
    
    react_char_dat_mod <- react_char_dat() %>% 
      group_by(.data[[input$charvar_in]]) %>% 
      mutate(!!sym(sumvar_name) := n())

    ggplot(data = react_char_dat_mod, aes(.data[[input$charvar_in]], 
                                          fill = .data[[input$charvar_in]])) +
      geom_bar() +
      coord_flip() +
      geom_label(aes(y = .data[[sumvar_name]] ,
                    label = .data[[sumvar_name]]),
                 position = "dodge",
                 show.legend = FALSE) +
      # shorten factor labels if they are too long
      scale_x_discrete(label = function(x) stringr::str_trunc(x, 15)) +
      theme_bw() +
      theme(legend.position = "none",
            axis.title.x = element_blank())
    
  })
  
  # Table with frequencies
  output$freqTab <- renderDT({
    
    react_char_dat() %>% 
      group_by(.data[[input$charvar_in]]) %>% 
      summarize(Frequency = n())

  })


  # List all entries of selected character variable.
  output$charEntryList <- renderDT({

    react_char_dat() %>%
        select(.data[[input$idvar2_in]], .data[[input$charvar_in]]) %>%
        arrange(.data[[input$charvar_in]])

  })


  # List unique entries of selected character variable.
  output$charUniqueEntryList <- renderDT({

    react_char_dat() %>%
        arrange(.data[[input$charvar_in]]) %>%
        distinct(.data[[input$charvar_in]])

  })

  # Show number of hits for entries
  output$hitsEntries <- renderTable({

    react_char_dat() %>%
      summarize(`Number of hits` = nrow(.))

  })

  # Show number of hits for unique entries
  output$hitsUniqueEntries <- renderTable({

    react_char_dat() %>%
      distinct(.data[[input$charvar_in]]) %>%
      summarize(`Number of hits` = nrow(.))

  })

}
