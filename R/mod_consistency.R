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
  
  tabItem(tabName = label,
          fluidRow(
            tabBox(width = 12,
                   title = "",
                   id = "tabset2",
                   height = "450px",
                   tabPanel("Numeric variables",
                            uiOutput(ns("numvar_out")),
                            uiOutput(ns("desmin")),
                            uiOutput(ns("desmax")),
                            tableOutput(outputId = ns("minmax")),
                            tabsetPanel(tabPanel("Frequencies",
                                                 plotOutput(outputId = ns("histPlot"))),
                                        tabPanel("List of entries",
                                                 br(),
                                                 h4("Shows entries that fall below minimum or exceed maximum"),
                                                 br(),
                                                 uiOutput(ns("idvar1_out")),
                                                 tableOutput(outputId = ns("numEntryList"))
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
                                                 plotOutput(ns("freqPlot"))),
                                        tabPanel("List of entries",
                                                 br(),
                                                 h4("Shows either all entries or, if a text is inputted in the filter box, the entries including this text."),
                                                 br(),
                                                 uiOutput(ns("idvar2_out")),
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
  
  # Reactive select inputs
  output$numvar_out <- renderUI({

    selectInput(inputId = ns("numvar_in"),
                label = "Select variable:",
                choices = colnames(data %>% select(where(is.numeric))))

  })

  output$charvar_out <- renderUI({

    selectInput(inputId = ns("charvar_in"),
                label = "Select variable:",
                choices = colnames(data %>% select(where(is.character))))

  })

  output$idvar1_out <- renderUI({

    selectInput(inputId = ns("idvar1_in"),
                label = "Select variable:",
                choices = colnames(data))

  })
  
  output$idvar2_out <- renderUI({
    
    selectInput(inputId = ns("idvar2_in"),
                label = "Select variable:",
                choices = colnames(data))
    
  })

  # Reactive slider inputs: set min and max according to min and max of actual data
  # Select desired minimum and maximum
  output$desmin <- renderUI({

    sliderInput(ns("minSlider"),
                "Choose allowed minimum",
                min = round(min(data[, input$numvar_in])),
                max = round(max(data[, input$numvar_in])),
                value = round(min(data[, input$numvar_in])))

  })

  output$desmax <- renderUI({

    sliderInput(ns("maxSlider"),
                "Choose allowed maximum",
                min = round(min(data[, input$numvar_in])),
                max = round(max(data[, input$numvar_in])),
                value = round(max(data[, input$numvar_in])))

  })

  # Create histogram for selected numeric variable
  output$histPlot <- renderPlot({

    ggplot(data = data, aes(.data[[input$numvar_in]])) +
      geom_histogram() +
      geom_vline(xintercept = input$minSlider, col = "red") +
      geom_vline(xintercept = input$maxSlider, col = "red") +
      ylab("Count") +
      theme_bw()

  })

  # Create table with actual minimum and maximum values of the selected numeric variable
  output$minmax <- renderTable({

    data %>%
      summarize(`Actual minimum` = min(.data[[input$numvar_in]]),
                `Actual maximum` = max(.data[[input$numvar_in]]),
                `Number of entries below allowed minimum` = sum(.data[[input$numvar_in]] < input$minSlider),
                `Number of entries above allowed maximum` = sum(.data[[input$numvar_in]] > input$maxSlider))

  })

  # List all entries of selected numeric variable
  output$numEntryList <- renderTable({

    data %>%
      select(.data[[input$idvar1_in]], .data[[input$numvar_in]]) %>%
      filter(.data[[input$numvar_in]] < input$minSlider |
               .data[[input$numvar_in]] > input$maxSlider) %>%
      arrange(.data[[input$numvar_in]])

  })

  # Render reactive data element according to filter variables
  react_char_dat <- reactive({

    # Filter for input text, if provided.
    if(!is.na(input$filtText) & !is.null(input$filtText) & input$filtText != ""){

      # Match case and ignore all non-alphanumeric characters, if both required
      # Note: the \x7f-\xff is needed to match German Umlaute and other special letters
      if(isTRUE(input$matchCase) & isTRUE(input$alphanum)){

        data %>%
          filter(str_detect(str_remove_all(.data[[input$charvar_in]],
                                           "[^a-zA-Z0-9\x7f-\xff]"),
                            str_remove_all(input$filtText,
                                           "[^a-zA-Z0-9\x7f-\xff]")))

        # Only match case
      } else if(isTRUE(input$matchCase) & isFALSE(input$alphanum)) {

        data %>%
          filter(str_detect(.data[[input$charvar_in]], input$filtText))

        # Ignore non-alphanumeric characters, but do not match case
      } else if(isFALSE(input$matchCase) & isTRUE(input$alphanum)) {

        data %>%
          filter(str_detect(tolower(str_remove_all(.data[[input$charvar_in]],
                                                   "[^a-zA-Z0-9\x7f-\xff]")),
                            tolower(str_remove_all(input$filtText,
                                                   "[^a-zA-Z0-9\x7f-\xff]"))))

        # Do not match case or ignore non-alphanumeric characters if none of the check boxes is checked
      } else {

        data %>%
          filter(str_detect(tolower(.data[[input$charvar_in]]), tolower(input$filtText)))

      }

      # If no text is provided, return the all entries
    } else {

      data

    }

  })
  
  
  # Barplot with frequencies (with bars sideways to handle many categories)
  output$freqPlot <- renderPlot({
    
    # TODO: shorten factor labels if they are too long (with str_trunc)
    # convert character vectors to factors for plotting
    sumvar_name <- paste0(input$charvar_in, "_sum")
    
    react_char_dat_mod <- react_char_dat() %>% 
      mutate(across(where(is.character), as.factor)) %>% 
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
      theme(legend.position = "none") +
      theme_bw()
    
  })


  # List all entries of selected character variable.
  output$charEntryList <- renderTable({

    react_char_dat() %>%
        select(.data[[input$idvar2_in]], .data[[input$charvar_in]]) %>%
        arrange(.data[[input$charvar_in]])

  })


  # List unique entries of selected character variable.
  output$charUniqueEntryList <- renderTable({

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
