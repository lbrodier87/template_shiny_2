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
#' 
mod_consistency_ui <- function(id, label){
  
  ns <- NS(id)
  
  numvars <- colnames(data %>% select(where(is.numeric)))
  charvars <- colnames(data %>% select(where(is.character)))
  allvars <- colnames(data)
  
  tabItem(tabName = label,
          fluidRow(
            tabBox(width = 12,
                   title = "",
                   id = "tabset2",
                   height = "450px",
                   tabPanel("Numeric variables",
                            selectInput(inputId = ns("numvar"), 
                                        label = "Select variable:",
                                        choices = numvars),
                            uiOutput(ns("desmin")),
                            uiOutput(ns("desmax")),
                            tableOutput(outputId = ns("minmax")),
                            tabsetPanel(tabPanel("Frequencies",
                                                 plotOutput(outputId = ns("histPlot"))),
                                        tabPanel("List of entries",
                                                 br(),
                                                 h4("Shows entries that fall below minimum or exceed maximum"),
                                                 br(),
                                                 selectInput(inputId = ns("idvar1"), 
                                                             label = "Choose identifier variable:",
                                                             choices = allvars),
                                                 tableOutput(outputId = ns("numEntryList"))
                                                 )
                                        )
                   ),
                   tabPanel("Character variables",
                            selectInput(inputId = ns("charvar"), 
                                        label = "Select variable:",
                                        choices = charvars),
                            textInput(inputId = ns("filtText"),
                                      label = "Filter entries for this text"),
                            checkboxInput(inputId = ns("matchCase"),
                                          label = "Match case?",
                                          value = FALSE),
                            checkboxInput(inputId = ns("alphanum"),
                                          label = "Ignore all non-alphanumeric characters?",
                                          value = FALSE),
                            tabsetPanel(tabPanel("Frequencies"),
                                        tabPanel("List of entries",
                                                 br(),
                                                 h4("Shows either all entries or, if a text is inputted in the filter box, the entries including this text."),
                                                 br(),
                                                 selectInput(inputId = ns("idvar2"), 
                                                             label = "Choose identifier variable:",
                                                             choices = allvars),
                                                 tableOutput(outputId = ns("hitsEntries")),
                                                 br(),
                                                 tableOutput(outputId = ns("charEntryList"))
                                        ),
                                        tabPanel("List of unique entries",
                                                 br(),
                                                 h4("Shows either all unique entries or, if a text is inputted in the filter box, the unique entries including this text."),
                                                 tableOutput(outputId = ns("hitsUniqueEntries")),
                                                 tableOutput(outputId = ns("charUniqueEntryList"))
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
  
  # Reactive slider inputs: set min and max according to min and max of actual data
  # Select desired minimum and maximum
  output$desmin <- renderUI({
    
    sliderInput(ns("minSlider"), 
                "Choose allowed minimum", 
                min = round(min(data[, input$numvar])),
                max = round(max(data[, input$numvar])),
                value = min(data[, input$numvar]))
    
  })
  
  output$desmax <- renderUI({
    
    sliderInput(ns("maxSlider"), 
                "Choose allowed maximum", 
                min = round(min(data[, input$numvar])),
                max = round(max(data[, input$numvar])),
                value = max(data[, input$numvar]))
    
  })
  
  # Create histogram for selected numeric variable
  output$histPlot <- renderPlot({
    
    ggplot(data = data, aes(.data[[input$numvar]])) +
      geom_histogram() +
      geom_vline(xintercept = input$minSlider, col = "red") +
      geom_vline(xintercept = input$maxSlider, col = "red") +
      ylab("Count") +
      theme_bw()
    
  })
  
  # Create table with actual minimum and maximum values of the selected numeric variable
  output$minmax <- renderTable({
    
    data %>% 
      summarize(`Actual minimum` = min(.data[[input$numvar]]),
                `Actual maximum` = max(.data[[input$numvar]]),
                `Number of entries below allowed minimum` = sum(.data[[input$numvar]] < input$minSlider),
                `Number of entries above allowed maximum` = sum(.data[[input$numvar]] > input$maxSlider))
    
  })
  
  # List all entries of selected numeric variable
  output$numEntryList <- renderTable({
    
    data %>% 
      select(.data[[input$idvar1]], .data[[input$numvar]]) %>%
      filter(.data[[input$numvar]] < input$minSlider |
               .data[[input$numvar]] > input$maxSlider) %>% 
      arrange(.data[[input$numvar]])
    
  })
  
  # Render reactive data element according to filter variables
  react_char_dat <- reactive({
    
    # Filter for input text, if provided.
    if(!is.na(input$filtText)){
      
      # Match case and ignore all non-alphanumeric characters, if both required
      # Note: the \x7f-\xff is needed to match German Umlaute and other special letters
      if(isTRUE(input$matchCase) & isTRUE(input$alphanum)){
        
        data %>% 
          filter(str_detect(str_remove_all(.data[[input$charvar]],
                                           "[^a-zA-Z0-9\x7f-\xff]"), 
                            str_remove_all(input$filtText,
                                           "[^a-zA-Z0-9\x7f-\xff]")))
        
        # Only match case
      } else if(isTRUE(input$matchCase) & isFALSE(input$alphanum)) {
        
        data %>% 
          filter(str_detect(.data[[input$charvar]], input$filtText))
        
        # Ignore non-alphanumeric characters, but do not match case
      } else if(isFALSE(input$matchCase) & isTRUE(input$alphanum)) {
        
        data %>% 
          filter(str_detect(tolower(str_remove_all(.data[[input$charvar]],
                                                   "[^a-zA-Z0-9\x7f-\xff]")), 
                            tolower(str_remove_all(input$filtText,
                                                   "[^a-zA-Z0-9\x7f-\xff]"))))
        
        # Do not match case or ignore non-alphanumeric characters if none of the check boxes is checked
      } else {
        
        data %>% 
          filter(str_detect(tolower(.data[[input$charvar]]), tolower(input$filtText)))
        
      }
      
      # If no text is provided, return the all entries
    } else {
      
      data
      
    }
    
  })
  
  
  # List all entries of selected character variable. 
  output$charEntryList <- renderTable({
    
    react_char_dat() %>% 
        select(.data[[input$idvar2]], .data[[input$charvar]]) %>%
        arrange(.data[[input$charvar]])
    
  })
  
  
  # List unique entries of selected character variable. 
  output$charUniqueEntryList <- renderTable({
    
    react_char_dat() %>% 
        arrange(.data[[input$charvar]]) %>% 
        distinct(.data[[input$charvar]])
    
  })
  
  # Show number of hits for entries
  output$hitsEntries <- renderTable({
    
    react_char_dat() %>% 
      summarize(`Number of hits` = nrow(.))
    
  })
  
  # Show number of hits for unique entries
  output$hitsUniqueEntries <- renderTable({
    
    react_char_dat() %>% 
      distinct(.data[[input$charvar]]) %>% 
      summarize(`Number of hits` = nrow(.))
    
  })
  

}
