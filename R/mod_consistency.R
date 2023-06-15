#' Consistency
#'
#' This module is intended to aid in uncovering inconsistencies in the data and
#' is designed to work generically with all variables of the inputted data set. 
#' For each variable, it shows different information, depending on its variable
#' type. Specifically, there is one panel for each variable type. 
#' 
#' For numeric variables, one can choose a minimum and maximum value and a table 
#' gives an overview over the actual minimum and maximum value in the data and 
#' the number of entries falling below the allowed minimum and above the allowed
#' maximum. Furthermore, the distribution of the variable is visualized and 
#' a list of entries outside the allowed data range is shown in a table.
#' 
#' For character variables, the entries can be filtered for a specific text 
#' (particularly useful for freetext fields). Moreover, a frequency plot or
#' table can be selected for display, and both a list of the (filtered) entries 
#' as well as a list of unique values are shown in tables.
#' 
#' For date variables, comparisons with fixed ranges as well as comparisons 
#' between different date variables can be performed. For fixed ranges, one can 
#' choose a date range in which the variable needs to fall. The actual minimum 
#' and maximum date is shown, as well as the number of entries below the minimum 
#' and above the maximum. In addition, all entries outside the allowed date range
#' are listed in a table. For date comparisons, two dates that shall be compared
#' are selected and the number of entries where the dates are equal, where the 
#' first date is below the second, and where the second date is below the first
#' is shown in a table. NA values are ignored for these comparisons. Moreover, 
#' the total number of entries for which both dates are present is shown, as well
#' as a list of entries with equal and unequal dates. 
#' 
#' Finally, multiple variables can be compared regardless of their type in a
#' separate panel. The variables that shall be compared are selected and 
#' contingency tables are created for these variables, as well as a filterable 
#' list of all entries for the selected variables.
#' 
#' For all tables listing raw entries, the patient id and the center are included 
#' (if present in the data) in addition to the selected variables.
#' 
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
#' @import lubridate
mod_consistency_ui <- function(id, label){
  
  # Initialize namespaced IDs for inputs and outputs
  ns <- NS(id)
  
  # Build up the UI
  # Note: For documentation of uiOutputs, see mod_consistency_server
  tabItem(tabName = label,
          
          # For selection of the form for which the consistency shall be checked
          fluidRow(
            box(uiOutput(ns("form_out")))),
          
          # Check consistency
          fluidRow(
            tabBox(width = 12,
                   title = "",
                   id = "tabset2",
                   height = "450px",
                   
                   # for numeric variables
                   tabPanel("Numeric variables",
                            uiOutput(ns("numvar_out")),
                            # Select input to choose the number of digits to round to
                            selectInput(inputId = ns("round_num"),
                                        label = "Number of digits to round to:",
                                        choices = 0:5,
                                        selected = 0),
                            uiOutput(ns("minSlider_out")),
                            uiOutput(ns("maxSlider_out")),
                            tableOutput(outputId = ns("minmax")),
                            tabsetPanel(tabPanel("Distribution",
                                                 plotOutput(outputId = ns("freqPlot"))),
                                        tabPanel("List of entries",
                                                 br(),
                                                 h4("Shows entries that fall below minimum or exceed maximum"),
                                                 br(),
                                                 uiOutput(ns("idvar_num_out")),
                                                 DTOutput(outputId = ns("numEntryList"))
                                        )
                            )
                   ),
                   
                   # for character variables
                   tabPanel("Character variables",
                            uiOutput(ns("charvar_out")),
                            # Text input for the text used to filter the entries
                            # in the selected character variable
                            textInput(inputId = ns("filtText"),
                                      label = "Filter entries for this text"),
                            # Checkbox input to choose whether the case should
                            # be matched between the string entered in filtText 
                            # and the variable entries
                            checkboxInput(inputId = ns("matchCase"),
                                          label = "Match case?",
                                          value = FALSE),
                            # Checkbox input to choose whether non-alphanumeric
                            # characters (i.e., [^a-zA-Z0-9\x7f-\xff]) shall be 
                            # ignored when comparing the string entererd in 
                            # filtText and the variable entries
                            checkboxInput(inputId = ns("alphanum"),
                                          label = "Ignore all non-alphanumeric characters?",
                                          value = FALSE),
                            tabsetPanel(tabPanel("Frequencies",
                                                 uiOutput(ns("char_freq_out")),
                                                 # Conditional Panel. Depending on the input in char_freq_in,
                                                 # display a plot or a table
                                                 conditionalPanel(condition = "input.char_freq_in == 'plot'",
                                                                  ns = ns,
                                                                  plotOutput(outputId = ns("barPlot"))),
                                                 conditionalPanel(condition = "input.char_freq_in == 'table'",
                                                                  ns = ns,
                                                                  DTOutput(outputId = ns("freqTab")))
                                                 ),
                                        tabPanel("List of entries",
                                                 br(),
                                                 h4("Shows either all entries or, if a text is inputted in the filter box, the entries including this text."),
                                                 br(),
                                                 uiOutput(ns("idvar_char_out")),
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
                   ),
                   
                   # for date variables
                   tabPanel("Date variables",
                            tabsetPanel(tabPanel("Fixed range",
                                                 br(),
                                                 uiOutput(ns("datevar_out")),
                                                 uiOutput(ns("dateRange_out")),
                                                 br(),
                                                 tableOutput(outputId = ns("minmax_date")),
                                                 br(),
                                                 h4("Shows entries that fall below minimum or exceed maximum"),
                                                 br(),
                                                 uiOutput(ns("idvar_date1_out")),
                                                 DTOutput(outputId = ns("dateEntryList"))
                            ),
                            tabPanel("Date comparisons",
                                     br(),
                                     uiOutput(ns("datevar1_out")),
                                     uiOutput(ns("datevar2_out")),
                                     br(),
                                     tableOutput(outputId = ns("datecomp_tab")),
                                     br(),
                                     uiOutput(ns("idvar_date2_out")),
                                     tabsetPanel(tabPanel("Both dates equal",
                                                          DTOutput(outputId = ns("dateEqualList")),
                                     ),
                                     tabPanel("Date 1 < Date 2",
                                              DTOutput(outputId = ns("dateSmallerList"))
                                     ),
                                     tabPanel("Date 1 > Date 2",
                                              DTOutput(outputId = ns("dateLargerList"))
                                     )
                                     )
                            )
                            )
                   ),
                   
                   # Comparison of multiple variables
                   tabPanel("Multiple variables",
                            uiOutput(ns("crossvar_out")),
                            tableOutput(outputId = ns("crosstab")),
                            br(),
                            uiOutput(ns("idvar_mult_out")),
                            DTOutput(outputId = ns("cross_dt"))
                            )
            )
          )

  )
  
}

#' @rdname mod_consistency
#' @param input standard shiny input argument
#' @param output standard shiny output argument
#' @param session standard shiny session argument
#' @param all_data data for use in calculations, plots and figures. Needs to be 
#' a list of reactive data frames.
mod_consistency_server <- function(input, output, session, all_data){
  
  # Get Namespaces IDs
  ns <- session$ns
  
  # Select form from which variables shall be taken ----
  # TODO: it would be better if variables of all forms could be included together.
  # But how to join them without creating duplicated entries because of repetition
  # groups / NA entries for empty forms?
  output$form_out <- renderUI({
    
    selectInput(inputId = ns("form_in"),
                label = "Select form of interest:",
                choices = names(all_data),
                selected = names(all_data)[1])
    
  })

  # Create reactive data for selected form
  # All variables shall be taken from this data frame
  data <- reactive({

    all_data[[input$form_in]]()

    })
  
  ## ----------------------
  ## Numeric variables ----
  ## ----------------------
  
  # Select and slider inputs -------
  
  # Select input for variable of interest within the selected form
  output$numvar_out <- renderUI({

    selectInput(inputId = ns("numvar_in"),
                label = "Select variable of interest:",
                choices = colnames(data() %>% select(where(is.numeric))),
                selected = colnames(data() %>% select(where(is.numeric)))[1])

  })
  
  # Slider inputs: set min and max according to min and max of actual data
  
  # Slider input to select desired minimum
  output$minSlider_out <- renderUI({
    
    # Break and show this message if all entries in the selected numeric variable 
    # are NA
    if(all(is.na(data() %>% pull(input$numvar_in)))){
      validate("All entries for the selected variable are NA")
    }
    
    sliderInput(ns("minSlider_in"),
                "Choose allowed minimum",
                min = round(min(data()[, input$numvar_in], na.rm = TRUE),
                            as.numeric(input$round_num)),
                max = round(max(data()[, input$numvar_in], na.rm = TRUE), 
                            as.numeric(input$round_num)),
                value = round(min(data()[, input$numvar_in], na.rm = TRUE), 
                              as.numeric(input$round_num)),
                round = -as.numeric(input$round_num),
                step = ifelse(as.numeric(input$round_num) == 0, 1,
                              as.numeric(paste0("0.",
                                                paste0(rep("0", 
                                                           (as.numeric(input$round_num) - 1)),
                                                       collapse = ""),
                                                "1")))
    )
    
  })
  
  # Slider input to select desired maximum
  output$maxSlider_out <- renderUI({
    
    # Break and show this message if all entries in the selected numeric variable 
    # are NA
    if(all(is.na(data() %>% pull(input$numvar_in)))){
      validate("All entries for the selected variable are NA")
    }
    
    sliderInput(ns("maxSlider_in"),
                "Choose allowed maximum",
                min = round(min(data()[, input$numvar_in], na.rm = TRUE),
                            as.numeric(input$round_num)),
                max = round(max(data()[, input$numvar_in], na.rm = TRUE), 
                            as.numeric(input$round_num)),
                value = round(max(data()[, input$numvar_in], na.rm = TRUE), 
                              as.numeric(input$round_num)),
                round = -as.numeric(input$round_num),
                step = ifelse(as.numeric(input$round_num) == 0, 1,
                              as.numeric(paste0("0.",
                                                paste0(rep("0", 
                                                           (as.numeric(input$round_num) - 1)),
                                                       collapse = ""),
                                                "1"))))
    
  })
  
  # Table minimum and maximum -----
  
  # Create table with actual minimum and maximum values of the selected numeric 
  # variable, as well as the number of entries falling below the allowed minimum
  # or above the allowed maximum
  output$minmax <- renderTable({
    
    # Break and show this message if all entries in the selected numeric variable 
    # are NA
    if(all(is.na(data() %>% pull(input$numvar_in)))){
      validate("All entries for the selected variable are NA")
    }
    
    data() %>%
      summarize(`Actual minimum` = min(.data[[input$numvar_in]], na.rm = TRUE),
                `Actual maximum` = max(.data[[input$numvar_in]], na.rm = TRUE),
                `Number of entries below allowed minimum` = 
                  sum(.data[[input$numvar_in]] < as.numeric(input$minSlider_in,
                                                            na.rm = TRUE)),
                `Number of entries above allowed maximum` = 
                  sum(.data[[input$numvar_in]] > as.numeric(input$maxSlider_in), 
                      na.rm = TRUE))
    
  })
  
  # Plot distribution -----
  
  # Create plot to visualize the distribution of the selected numeric variable
  output$freqPlot <- renderPlot({
    
    # Break and show this message if all entries in the selected numeric variable 
    # are NA
    if(all(is.na(data() %>% pull(input$numvar_in)))){
      validate("All entries for the selected variable are NA")
    }
    
    # TODO: Make this work with ggplotly!
    #ggplotly(
    ggplot(data = data(), aes(x = centre.short, y = .data[[input$numvar_in]])) +
      geom_violin(fill = "#00BA38") +
      geom_boxplot(width = .12, fill = "white", color = "black") +
      #stat_summary(fun.data = "mean_sdl", mult = 1, geom = "pointrange") +
      geom_jitter(position = position_jitter(0.2),
                  shape = 16) +
      xlab("Centre") +
      annotate("rect", xmin = -Inf, xmax = Inf, 
               ymin = as.numeric(input$maxSlider_in), ymax = Inf,
               alpha = 0.8, fill = "darkgrey") +
      annotate("rect", xmin = -Inf, xmax = Inf, ymin = -Inf, 
               ymax = as.numeric(input$minSlider_in),
               alpha = 0.8, fill = "darkgrey") +
      theme_bw()
    #) %>%
    # This does not work with plotly, annotate() does not work either
    # layout(shapes = list(
    #        list(type = "rect",
    #             fillcolor = "red", line = list(color = "blue"), opacity = 0.3,
    #             x0 = -1, x1 = 2, xref = "x",
    #             y0 = 0, y1 = 12.5, yref = "y")))
    
  })
  
  # Datatable to show all entries -------
  
  # Datatable to show all entries falling below minimum or above maximum for the 
  # selected numeric variable
  
  # Select input to select additional variables to be displayed in the datatable,
  # apart from the selected numeric variable
  output$idvar_num_out <- renderUI({

    selectInput(inputId = ns("idvar_num_in"),
                label = "Select additional variable(s) to be displayed:",
                selected = c("pat_id", "centre.short"),
                choices = colnames(data()),
                multiple = TRUE)

  })
  
  # List all entries falling below minimum or above maximum for the selected
  # numeric variable
  output$numEntryList <- renderDT({

    DT::datatable(data() %>%
                    select(!!!syms(c(input$idvar_num_in, input$numvar_in))) %>%
                    filter(.data[[input$numvar_in]] < as.numeric(input$minSlider_in) |
                             .data[[input$numvar_in]] > as.numeric(input$maxSlider_in)) %>%
                    mutate(across(where(is.numeric), ~ 
                                    round(.x, digits = as.numeric(input$round_num)))) %>%
                    arrange(.data[[input$numvar_in]]),
                  rownames = FALSE)

  })
  
  ## -------------------------
  ## Character variables -----
  ## -------------------------
  
  # Select inputs ------
  
  # Select input to select character variable of interest
  output$charvar_out <- renderUI({

    selectInput(inputId = ns("charvar_in"),
                label = "Select variable of interest:",
                choices = colnames(data() %>% select(where(is.character))),
                selected = colnames(data() %>% select(where(is.character)))[1])

  })

  # Select input to choose between plot and table for display of the distribution
  output$char_freq_out <- renderUI({

    selectInput(inputId = ns("char_freq_in"),
                label = "Choose plot or table:",
                choices = c(Plot = "plot", Table = "table"),
                selected = ifelse(length(levels(react_char_dat() %>% 
                                                  pull(input$charvar_in))) <= 15,
                                  "plot", "table"))

  })
  
  # Reactive data element -----
  
  # Render reactive data element that filters entries for the selected character
  # variable according to the string provided in filtText
  react_char_dat <- reactive({
    
    # HIER STEHENGEBLIEBEN

    # Convert all character variables to factors for later use in plots
    react_dat <- data() %>%
      mutate(across(where(is.character), as.factor))

    # Filter for input text, if provided.
    if(!is.na(input$filtText) & !is.null(input$filtText) & input$filtText != ""){

      # Match case and ignore all non-alphanumeric characters, if both required
      # based on checkbox input (matchCase and alphanum)
      # Note: the \x7f-\xff is needed to match German Umlaute and other special letters
      if(isTRUE(input$matchCase) & isTRUE(input$alphanum)){

        react_dat <- react_dat %>%
          filter(str_detect(str_remove_all(.data[[input$charvar_in]],
                                           "[^a-zA-Z0-9\x7f-\xff]"),
                            str_remove_all(input$filtText,
                                           "[^a-zA-Z0-9\x7f-\xff]")))

        # Match case and include non-alphanumeric characters
      } else if(isTRUE(input$matchCase) & isFALSE(input$alphanum)) {

        react_dat <- react_dat %>%
          filter(str_detect(.data[[input$charvar_in]], input$filtText))

        # Ignore non-alphanumeric characters and do not match case
      } else if(isFALSE(input$matchCase) & isTRUE(input$alphanum)) {

        react_dat <- react_dat %>%
          filter(str_detect(tolower(str_remove_all(.data[[input$charvar_in]],
                                                   "[^a-zA-Z0-9\x7f-\xff]")),
                            tolower(str_remove_all(input$filtText,
                                                   "[^a-zA-Z0-9\x7f-\xff]"))))

        # Do not match case or ignore non-alphanumeric characters if none of the 
        # check boxes is checked
      } else {

        react_dat <- react_dat %>%
          filter(str_detect(tolower(.data[[input$charvar_in]]), tolower(input$filtText)))

      }

    }

    # If no text is provided in filtText, return all entries unchanged

    return(react_dat)

  })
  
  output$idvar_char_out <- renderUI({

    selectInput(inputId = ns("idvar_char_in"),
                label = "Select additional variable(s) to be displayed:",
                choices = colnames(data()),
                selected = c("pat_id", "centre.short"),
                multiple = TRUE)

  })

  # Barplot with frequencies (with bars sideways to handle many categories)
  output$barPlot <- renderPlot({

    # Break and show this message if the string pattern provided in filtText is
    # not found in any of the entries for the selected character variable
    if(nrow(react_char_dat()) == 0){
      validate("The string pattern was not found in the data for the selected variable")
    }
    
    # Break and show this message if all entries in the selected character 
    # variable are NA
    if(all(is.na(react_char_dat() %>% pull(input$charvar_in)))){
      validate("All entries for the selected variable are NA")
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

    DT::datatable(react_char_dat() %>%
                    group_by(.data[[input$charvar_in]]) %>%
                    summarize(Frequency = n()),
                  rownames = FALSE, options = list(searching = FALSE))

  })


  # List all entries of selected character variable.
  output$charEntryList <- renderDT({

    DT::datatable(react_char_dat() %>%
                    select(!!!syms(c(input$idvar_char_in, input$charvar_in))) %>%
                    arrange(.data[[input$charvar_in]]),
                  rownames = FALSE, options = list(searching = FALSE))

  })


  # List unique entries of selected character variable.
  output$charUniqueEntryList <- renderDT({

    DT::datatable(react_char_dat() %>%
                    arrange(.data[[input$charvar_in]]) %>%
                    distinct(.data[[input$charvar_in]]),
                  rownames = FALSE, options = list(searching = FALSE))

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

  ## --------------------
  ## Date variables -----
  ## --------------------
  
  # Select inputs
  output$datevar_out <- renderUI({

    selectInput(inputId = ns("datevar_in"),
                label = "Select variable of interest:",
                choices = colnames(data() %>% select(where(~(is.Date(.x) | is.POSIXt(.x))))),
                selected = colnames(data() %>% select(where(~(is.Date(.x) | is.POSIXt(.x)))))[1])

  })
  
  # Select minimum and maximum date allowed
  output$dateRange_out <- renderUI({
    
    dateRangeInput(ns("dateRange_in"),
                   "Choose date range where the date must fall into",
                   start = min(data() %>% pull(input$datevar_in), na.rm = TRUE),
                   end = max(data() %>% pull(input$datevar_in), na.rm = TRUE)
    )
    
  })
  
  # Create table with actual minimum and maximum values of the selected date variable
  output$minmax_date <- renderTable({
    
    # Break and show this message if the date range is illogical (i.e., the 
    # selected minimum date is larger than the selected maximum date)
    if(as.Date(input$dateRange_in[1]) > as.Date(input$dateRange_in[2])){
      validate("Selected minimum date is larger than selected maximum date")
    }
    
    # Break and show this message if all entries in the selected date variable 
    # are NA
    if(all(is.na(data() %>% pull(input$numvar_in)))){
      validate("All entries for the selected variable are NA")
    }
    
    data() %>%
      summarize(`Actual minimum` = as.character(min(.data[[input$datevar_in]], na.rm = TRUE)),
                `Actual maximum` = as.character(max(.data[[input$datevar_in]], na.rm = TRUE)),
                `Number of entries below allowed minimum` = sum(.data[[input$datevar_in]] < as.Date(input$dateRange_in[1]), na.rm = TRUE),
                `Number of entries above allowed maximum` = sum(.data[[input$datevar_in]] > as.Date(input$dateRange_in[2]), na.rm = TRUE))
    
  })
  
  output$idvar_date1_out <- renderUI({

    selectInput(inputId = ns("idvar_date1_in"),
                label = "Select additional variable(s) to be displayed:",
                choices = colnames(data()),
                selected = c("pat_id", "centre.short"),
                multiple = TRUE)

  })

  # List all entries of selected date variable
  output$dateEntryList <- renderDT({
    
    # Break and show this message if the date range is illogical (i.e., the 
    # selected minimum date is larger than the selected maximum date)
    if(as.Date(input$dateRange_in[1]) > as.Date(input$dateRange_in[2])){
      validate("Selected minimum date is larger than selected maximum date")
    }

    DT::datatable(data() %>%
                    select(!!!syms(c(input$idvar_date1_in, input$datevar_in))) %>%
                    filter(.data[[input$datevar_in]] < as.Date(input$dateRange_in[1]) |
                             .data[[input$datevar_in]] > as.Date(input$dateRange_in[2])) %>%
                    arrange(.data[[input$datevar_in]]) %>% 
                    mutate(across(where(is.POSIXt), as.character)),
                  rownames = FALSE)

  })
  
  output$datevar1_out <- renderUI({

    selectInput(inputId = ns("datevar1_in"),
                label = "Select first date variable:",
                choices = colnames(data() %>% select(where(~(is.Date(.x) | is.POSIXt(.x))))),
                selected = colnames(data() %>% select(where(~(is.Date(.x) | is.POSIXt(.x)))))[1])

  })

  output$datevar2_out <- renderUI({

    selectInput(inputId = ns("datevar2_in"),
                label = "Select second date variable:",
                choices = colnames(data() %>% select(where(~(is.Date(.x) | is.POSIXt(.x))))),
                selected = colnames(data() %>% select(where(~(is.Date(.x) | is.POSIXt(.x)))))[1])

  })

  # Create table with comparisons between the two selected date variables
  output$datecomp_tab <- renderTable({

    datecomp_names <- c(paste0(input$datevar1_in, " = ", input$datevar2_in),
                        paste0(input$datevar1_in, " < ", input$datevar2_in),
                        paste0(input$datevar1_in, " > ", input$datevar2_in),
                        "Total both dates present")

    data() %>%
      summarize(V1 = sum(.data[[input$datevar1_in]] ==
                           .data[[input$datevar2_in]], na.rm = TRUE),
                V2 = sum(.data[[input$datevar1_in]] <
                           .data[[input$datevar2_in]], na.rm = TRUE),
                V3 = sum(.data[[input$datevar1_in]] >
                           .data[[input$datevar2_in]], na.rm = TRUE),
                V4 = sum(!is.na(.data[[input$datevar1_in]]) & !is.na(.data[[input$datevar2_in]]))) %>%
      `colnames<-`(datecomp_names)

  })

  output$idvar_date2_out <- renderUI({

    selectInput(inputId = ns("idvar_date2_in"),
                label = "Select additional variable(s) to be displayed in tables below",
                choices = colnames(data()),
                selected = c("pat_id", "centre.short"),
                multiple = TRUE)

  })

  # List all entries where the two selected date variables are equal
  output$dateEqualList <- renderDT({

    DT::datatable(data() %>%
                    select(!!!syms(c(input$idvar_date2_in, input$datevar1_in, input$datevar2_in))) %>%
                    filter(.data[[input$datevar1_in]] == .data[[input$datevar2_in]]) %>%
                    arrange(.data[[input$datevar1_in]]) %>% 
                    mutate(across(where(is.POSIXt), is.character)),
                  rownames = FALSE)

  })

  # List all entries where the first selected date variable is smaller than the second
  output$dateSmallerList <- renderDT({

    DT::datatable(data() %>%
                    select(!!!syms(c(input$idvar_date2_in, input$datevar1_in, input$datevar2_in))) %>%
                    filter(.data[[input$datevar1_in]] < .data[[input$datevar2_in]]) %>%
                    arrange(.data[[input$datevar1_in]]) %>% 
                    mutate(across(where(is.POSIXt), as.character)),
                  rownames = FALSE)

  })

  # List all entries where the two selected date variables are equal
  output$dateLargerList <- renderDT({

    DT::datatable(data() %>%
                    select(!!!syms(c(input$idvar_date2_in, input$datevar1_in, input$datevar2_in))) %>%
                    filter(.data[[input$datevar1_in]] > .data[[input$datevar2_in]]) %>%
                    arrange(.data[[input$datevar1_in]]) %>% 
                    mutate(across(where(is.POSIXt), as.character)),
                  rownames = FALSE)

  })

  ## ----------------
  ## Crosstable -----
  ## ----------------

  output$crossvar_out <- renderUI({

    selectizeInput(inputId = ns("crossvar_in"),
                   label = "Select variables to cross",
                   choices = colnames(data()),
                   selected = "centre.short",
                   multiple = TRUE
                   )

  })
  
  # Create a crosstable
  output$crosstab <- renderTable({
    
    # Break and show respective message if no variables are selected or too
    # many variables are selected, resulting in too many entries for the 
    # crosstable
    if(length(input$crossvar_in) == 0){
      validate("No variables selected")
    } else if(data() %>% summarize(across(all_of(input$crossvar_in), n_distinct)) %>% prod() > 10000) {
      validate("Crossing the selected variables results in too many entries for display in a crosstable")
    }
    
    ftab <- data() %>%
      select(!!!syms(input$crossvar_in)) %>% 
      ftable(exclude = NULL, col.vars = 1) %>% 
      format(quote = FALSE)
    
    out <- data.frame(ftab[-1,], stringsAsFactors = FALSE)
    names(out) <- ftab[1,]
    out
    
  })
  
  output$idvar_mult_out <- renderUI({
    
    selectInput(inputId = ns("idvar_mult_in"),
                label = "Select additional variable(s) to be displayed in tables below",
                choices = colnames(data()),
                selected = c("pat_id", "centre.short"),
                multiple = TRUE)
    
  })
  
  # Show all data
  output$cross_dt <- renderDT({
    
    DT::datatable(data() %>%
                    select(!!!syms(c(input$idvar_mult_in, input$crossvar_in))) %>% 
                    arrange(!!!syms(input$crossvar_in)),
                  rownames = FALSE, filter = "top")
    
  })
  
}
