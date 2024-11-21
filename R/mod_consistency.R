#' Consistency
#'
#' This module is intended to aid in uncovering inconsistencies in the data and
#' is designed to work generically with all variables of the inputted data set. 
#' For each variable, it shows different information, depending on its type. 
#' Specifically, there is one panel for each variable type. 
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
#' For date or datetime variables, comparisons with fixed ranges as well as 
#' comparisons between different date or datetime variables can be performed. 
#' For fixed ranges, one can choose a date range in which the variable needs to 
#' fall. The actual minimum and maximum date is shown, as well as the number of 
#' entries below the chosen minimum and above the chosen maximum. In addition, 
#' all entries outside the allowed date range are listed in a table. 
#' For date or datetime comparisons, two dates or datetimes that shall be compared
#' are selected. The number of entries where the dates or datetimes are equal, 
#' where the first is below the second, where the second is below 
#' the first, where the first is NA, where the second is NA, and where both
#' are NA are shown in a table. Moreover, lists of entries falling in the 
#' categories described above are shown.
#' 
#' Finally, multiple variables can be compared regardless of their type in a
#' separate panel. The variables that shall be compared are selected and a 
#' contingency table is created for these variables, as well as a filterable 
#' list of all entries for the selected variables.
#' 
#' For all tables listing raw entries, the patient id and the center are included 
#' (if present in the data) in addition to the selected variables.
#' 
#' @rdname mod_consistency
#' @param id standard shiny id argument
#' @param label standard shiny label argument
#' 
#' @author Silvia Steiner
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
          
          # Select input for the form (i.e., dataframe) of interest
          fluidRow(
            box(width = 12,
                uiOutput(ns("form_out")))
          ),
          
          # Check consistency for all variables in the selected form
          fluidRow(
            tabBox(width = 12,
                   id = "tabset2",
                   height = "450px",
                   
                   # Numeric variables ----
                   tabPanel("Numeric variables",
                            # Select input to select numeric variable of interest
                            uiOutput(ns("numvar_out")),
                            br(),
                            # Numeric inputs for allowed minimum and maximum
                            fluidRow(
                              column(3, 
                                     uiOutput(ns("min_num_out"))),
                              column(3, 
                                     uiOutput(ns("max_num_out")))
                            ),
                            br(),
                            # Value boxes with minimum and maximum
                            fluidRow(
                              valueBoxOutput(ns("actual_min_num"), width = 3),
                              valueBoxOutput(ns("actual_max_num"), width = 3),
                              valueBoxOutput(ns("below_min_num"), width = 3),
                              valueBoxOutput(ns("above_max_num"), width = 3)
                            ),
                            br(),
                            # Display distribution of selected variable and 
                            # show all entries falling below minimum or above
                            # maximum
                            tabsetPanel(
                              tabPanel("Distribution",
                                       # Plot for distribution of selected variable
                                       plotOutput(outputId = ns("distr_plot_num"))
                              ),
                              tabPanel("List of entries",
                                       br(),
                                       h5("The following table shows entries that fall below minimum or exceed maximum"),
                                       br(),
                                       # Select input to choose additional 
                                       # variables to display in table
                                       uiOutput(ns("idvar_num_out")),
                                       # Table showing all entries below minimum
                                       # or above maximum
                                       DTOutput(outputId = ns("list_entries_num"))
                              )
                            )
                   ),
                   
                   # Character variables ----
                   tabPanel("Character variables",
                            # Select input to select character variable of interest
                            uiOutput(ns("charvar_out")),
                            # Text input for the text used to filter the entries
                            # in the selected character variable
                            textInput(inputId = ns("filt_text"),
                                      label = "Filter entries for this text"),
                            # Checkbox input to choose whether the case should
                            # be matched between the string entered in filt_text 
                            # and the variable entries
                            checkboxInput(inputId = ns("match_case"),
                                          label = "Match case?",
                                          value = FALSE),
                            # Checkbox input to choose whether non-alphanumeric
                            # characters (i.e., [^a-zA-Z0-9\x7f-\xff]) shall be 
                            # ignored when comparing the string entererd in 
                            # filt_text and the variable entries
                            checkboxInput(inputId = ns("alphanum"),
                                          label = "Ignore all non-alphanumeric characters?",
                                          value = FALSE),
                            # Display frequencies of selected character variable
                            # and show all and unique entries that match the 
                            # filter text
                            tabsetPanel(
                              tabPanel("Frequencies",
                                       br(),
                                       # Select input to choose between plot and
                                       # table
                                       uiOutput(ns("char_freq_out")),
                                       # Conditional Panel. Depending on the input in char_freq_in,
                                       # display a plot or a table with frequencies
                                       conditionalPanel(condition = "input.char_freq_in == 'plot'",
                                                        ns = ns,
                                                        plotOutput(outputId = ns("freq_plot_char"))
                                       ),
                                       conditionalPanel(condition = "input.char_freq_in == 'table'",
                                                        ns = ns,
                                                        DTOutput(outputId = ns("freq_tab_char"))
                                       )
                              ),
                              tabPanel("List of entries",
                                       br(),
                                       h5("The following table shows either all entries or, if a text is inputted in the filter box, the entries including this text."),
                                       br(),
                                       # Select input to choose additional variables
                                       # to display in table
                                       uiOutput(ns("idvar_char_out")),
                                       br(),
                                       # Table showing all entries that match
                                       # the filter text
                                       DTOutput(outputId = ns("list_entries_char"))
                              ),
                              tabPanel("List of unique entries",
                                       br(),
                                       h5("The following table shows either all unique entries or, if a text is inputted in the filter box, the unique entries including this text."),
                                       br(),
                                       # Table showing all unique entries that
                                       # match the filter text
                                       DTOutput(outputId = ns("list_unique_entries_char"))
                              )
                            )
                   ),
                   
                   # Date and datetime variables ----
                   tabPanel("Date variables",
                            # First panel to compare a date or datetime of 
                            # interest against a fixed minimum and maximum date
                            tabsetPanel(
                              tabPanel("Fixed range",
                                       br(),
                                       # Select input to select date or datetime
                                       # variable of interest
                                       uiOutput(ns("datevar_out")),
                                       # Date range input to select allowed minimum
                                       # and maximum date
                                       uiOutput(ns("date_range_out")),
                                       br(),
                                       # Value boxes with minimum and maximum
                                       fluidRow(
                                         valueBoxOutput(ns("actual_min_date"), width = 3),
                                         valueBoxOutput(ns("actual_max_date"), width = 3),
                                         valueBoxOutput(ns("below_min_date"), width = 3),
                                         valueBoxOutput(ns("above_max_date"), width = 3)
                                       ),
                                       br(),
                                       h5("The following table shows entries that fall below minimum or exceed maximum"),
                                       br(),
                                       # Select input to choose additional variables
                                       # to display in table
                                       uiOutput(ns("idvar_date1_out")),
                                       # Table showing all entries below minimum or
                                       # above maximum
                                       DTOutput(outputId = ns("list_entries_date"))
                              ),
                              # Second panel to compare two date or datetime 
                              # variables
                              tabPanel("Date comparisons",
                                       br(),
                                       # Select inputs to select the two date 
                                       # or datetime variables to compare
                                       fluidRow(
                                         column(3, 
                                                uiOutput(ns("datevar1_out"))),
                                         column(3, 
                                                uiOutput(ns("datevar2_out")))
                                       ),
                                       br(),
                                       # Numeric input to choose allowed time 
                                       # difference (in days) between selected
                                       # dates or datetimes
                                       uiOutput(ns("difftime_out")),
                                       textOutput(ns("max_diff_out")),
                                       br(),
                                       # Value boxes showing number of differing entries
                                       fluidRow(
                                         valueBoxOutput(ns("sum_date_equal"), width = 2),
                                         valueBoxOutput(ns("sum_date_smaller"), width = 2),
                                         valueBoxOutput(ns("sum_date_larger"), width = 2),
                                         valueBoxOutput(ns("sum_date1_na"), width = 2),
                                         valueBoxOutput(ns("sum_date2_na"), width = 2),
                                         valueBoxOutput(ns("sum_both_na"), width = 2)
                                       ),
                                       br(),
                                       # Select input to choose additional variables
                                       # to display in table
                                       uiOutput(ns("idvar_date2_out")),
                                       # Tables showing all entries falling into
                                       # specific categories
                                       tabsetPanel(
                                         tabPanel("Both dates equal",
                                                  br(),
                                                  DTOutput(outputId = ns("date_equal_list")),
                                         ),
                                         tabPanel("Date 1 < Date 2",
                                                  br(),
                                                  DTOutput(outputId = ns("date_smaller_list"))
                                         ),
                                         tabPanel("Date 1 > Date 2",
                                                  br(),
                                                  DTOutput(outputId = ns("date_larger_list"))
                                         ),
                                         tabPanel("Date 1 NA",
                                                  br(),
                                                  DTOutput(outputId = ns("date_1_na"))
                                         ),
                                         tabPanel("Date 2 NA",
                                                  br(),
                                                  DTOutput(outputId = ns("date_2_na"))
                                         ),
                                         tabPanel("Both dates NA",
                                                  br(),
                                                  DTOutput(outputId = ns("date_both_na"))
                                         )
                                       )
                              )
                            )
                   ),
                   
                   # Comparison of multiple variables ----
                   tabPanel("Multiple variables",
                            # Select input to select the variables to compare
                            uiOutput(ns("crossvar_out")),
                            # Cross table across selected variables
                            tableOutput(outputId = ns("cross_tab")),
                            br(),
                            # Select input to choose additional variables
                            # to display in table
                            uiOutput(ns("idvar_mult_out")),
                            # Table showing all entries of the selected variables
                            DTOutput(outputId = ns("list_entries_cross"))
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
  output$form_out <- renderUI({
    
    selectInput(inputId = ns("form_in"),
                label = "Select form of interest:",
                choices = names(all_data),
                selected = names(all_data)[1])
    
  })

  # Create reactive data for selected form
  # All variables shall be taken from this data frame
  data <- reactive({
    
    validate(need(input$form_in, "Please select a form"))

    all_data[[input$form_in]]()

    })
  
  ## ----------------------
  ## Numeric variables ----
  ## ----------------------
  
  # Select and numeric inputs -------
  
  # Select input for numeric variable of interest within the selected form
  output$numvar_out <- renderUI({
    
    # Break and show this message if there are no numeric variables
    if(length(colnames(data() %>% select(where(is.numeric)))) == 0){
      validate("There are no numeric variables in the selected form")
    }

    selectInput(inputId = ns("numvar_in"),
                label = "Select variable of interest:",
                choices = colnames(data() %>% select(where(is.numeric))),
                selected = colnames(data() %>% select(where(is.numeric)))[1])

  })
  
  # Numeric inputs: set min and max according to min and max of actual data
  
  # Numeric input to select desired minimum
  output$min_num_out <- renderUI({
    
    # Break and show this message if all entries in the selected numeric variable 
    # are NA
    if(all(is.na(data() %>% pull(input$numvar_in)))){
      validate("All entries for the selected variable are NA")
    }
    
    numericInput(ns("min_num_in"),
                 label = "Choose allowed minimum",
                 min = min(data()[, input$numvar_in], na.rm = TRUE),
                 max = max(data()[, input$numvar_in], na.rm = TRUE),
                 value = min(data()[, input$numvar_in], na.rm = TRUE),
                 width = "400px"
    )
    
  })
  
  # Numeric input to select desired maximum
  output$max_num_out <- renderUI({
    
    # Break and show this message if all entries in the selected numeric variable 
    # are NA
    if(all(is.na(data() %>% pull(input$numvar_in)))){
      validate("All entries for the selected variable are NA")
    }
    
    numericInput(ns("max_num_in"),
                 label = "Choose allowed maximum",
                 min = min(data()[, input$numvar_in], na.rm = TRUE),
                 max = max(data()[, input$numvar_in], na.rm = TRUE),
                 value = max(data()[, input$numvar_in], na.rm = TRUE),
                 width = "400px"
    )
    
  })
  
  # Value boxes with minimum and maximum -----
  
  # Create value boxes with with actual minimum and maximum values of the selected 
  # numeric variable, as well as the number of entries falling below the allowed 
  # minimum or above the allowed maximum
  output$actual_min_num <- renderValueBox({
    
    # Break and show this message if all entries in the selected numeric variable
    # are NA
    if(all(is.na(data() %>% pull(input$numvar_in)))){
      validate("All entries for the selected variable are NA")
    }
    
    # Break and show this message if minimum or maximum are empty
    if(is.na(input$min_num_in) | is.na(input$max_num_in)){
      validate("Please choose an allowed minimum and maximum")
    }
    
    # Break and show this message if the allowed minimum is larger than the 
    # allowed maximum
    if(input$min_num_in > input$max_num_in){
      validate("Allowed minimum is larger than allowed maximum")
    }
    
    myval <- min(data()[[input$numvar_in]], na.rm = TRUE)
    
    valueBox(value = myval,
             subtitle = "Actual minimum",
             color = "blue"
    )
    
  })
  
  output$actual_max_num <- renderValueBox({
    
    # Break and show this message if all entries in the selected numeric variable
    # are NA
    if(all(is.na(data() %>% pull(input$numvar_in)))){
      validate("All entries for the selected variable are NA")
    }
    
    # Break and show this message if minimum or maximum are empty
    if(is.na(input$min_num_in) | is.na(input$max_num_in)){
      validate("Please choose an allowed minimum and maximum")
    }
    
    # Break and show this message if the allowed minimum is larger than the 
    # allowed maximum
    if(input$min_num_in > input$max_num_in){
      validate("Allowed minimum is larger than allowed maximum")
    }
    
    myval <- max(data()[[input$numvar_in]], na.rm = TRUE)
    
    valueBox(value = myval,
             subtitle = "Actual maximum",
             color = "blue"
    )
    
  })
  
  output$below_min_num <- renderValueBox({
    
    # Break and show this message if all entries in the selected numeric variable
    # are NA
    if(all(is.na(data() %>% pull(input$numvar_in)))){
      validate("All entries for the selected variable are NA")
    }
    
    # Break and show this message if minimum or maximum are empty
    if(is.na(input$min_num_in) | is.na(input$max_num_in)){
      validate("Please choose an allowed minimum and maximum")
    }
    
    # Break and show this message if the allowed minimum is larger than the 
    # allowed maximum
    if(input$min_num_in > input$max_num_in){
      validate("Allowed minimum is larger than allowed maximum")
    }
    
    myval <- sum(data()[[input$numvar_in]] < 
                   as.numeric(input$min_num_in), na.rm = TRUE)
    
    valueBox(value = myval,
             subtitle = "Number of entries below allowed minimum",
             color = ifelse(myval == 0, "green", "red")
    )
    
  })
  
  output$above_max_num <- renderValueBox({
    
    # Break and show this message if all entries in the selected numeric variable
    # are NA
    if(all(is.na(data() %>% pull(input$numvar_in)))){
      validate("All entries for the selected variable are NA")
    }
    
    # Break and show this message if minimum or maximum are empty
    if(is.na(input$min_num_in) | is.na(input$max_num_in)){
      validate("Please choose an allowed minimum and maximum")
    }
    
    # Break and show this message if the allowed minimum is larger than the 
    # allowed maximum
    if(input$min_num_in > input$max_num_in){
      validate("Allowed minimum is larger than allowed maximum")
    }
    
    myval <- sum(data()[[input$numvar_in]] > 
                   as.numeric(input$max_num_in), na.rm = TRUE)
    
    valueBox(value = myval,
             subtitle = "Number of entries above allowed maximum",
             color = ifelse(myval == 0, "green", "red")
    )
    
  })
  
  
  # Plot distribution -----
  
  # Create plot to visualize the distribution of the selected numeric variable
  output$distr_plot_num <- renderPlot({
    
    # Break and show this message if all entries in the selected numeric variable
    # are NA
    if(all(is.na(data() %>% pull(input$numvar_in)))){
      validate("All entries for the selected variable are NA")
    }
    
    # Break and show this message if minimum or maximum are empty
    if(is.na(input$min_num_in) | is.na(input$max_num_in)){
      validate("Please choose an allowed minimum and maximum")
    }
    
    # Break and show this message if the allowed minimum is larger than the 
    # allowed maximum
    if(input$min_num_in > input$max_num_in){
      validate("Allowed minimum is larger than allowed maximum")
    }
    
    ggplot(data = data(), aes(x = .data[[input$numvar_in]])) +
      ggdist::stat_halfeye(
        adjust = .5,
        width = .6,
        .width = 0,
        justification = -.2,
        point_colour = NA,
        fill = "#1ea5a5"
      ) +
      geom_boxplot(
        width = .15,
        color = "#1ea5a5"
      ) +
      # add shading for areas below minimum and above maximum
      annotate("rect", ymin = -Inf, ymax = Inf,
               xmin = as.numeric(input$max_num_in), xmax = Inf,
               alpha = 0.8, fill = "darkgrey") +
      annotate("rect", ymin = -Inf, ymax = Inf, xmin = -Inf,
               xmax = as.numeric(input$min_num_in),
               alpha = 0.8, fill = "darkgrey") +
      # Remove y-axis
      guides(y = "none") +
      labs(y = "") +
      theme_bw()

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
                choices = unique(colnames(data())),
                multiple = TRUE)

  })
  
  # List all entries falling below minimum or above maximum for the selected
  # numeric variable
  output$list_entries_num <- renderDT({
    
    # Break and show this message if all entries in the selected numeric variable
    # are NA
    if(all(is.na(data() %>% pull(input$numvar_in)))){
      validate("All entries for the selected variable are NA")
    }
    
    # Break and show this message if minimum or maximum are empty
    if(is.na(input$min_num_in) | is.na(input$max_num_in)){
      validate("Please choose an allowed minimum and maximum")
    }
    
    # Break and show this message if the allowed minimum is larger than the 
    # allowed maximum
    if(input$min_num_in > input$max_num_in){
      validate("Allowed minimum is larger than allowed maximum")
    }

    data() %>%
      select(!!!syms(c(input$idvar_num_in, input$numvar_in))) %>%
      filter(.data[[input$numvar_in]] < as.numeric(input$min_num_in) |
               .data[[input$numvar_in]] > as.numeric(input$max_num_in)) %>%
      arrange(.data[[input$numvar_in]])
    
  }, rownames = FALSE, filter = "top", escape = FALSE)
  
  ## -------------------------
  ## Character variables -----
  ## -------------------------
  
  # Select input for character variable of interest within the selected form
  output$charvar_out <- renderUI({
    
    # Break and show this message if there are no character variables
    if(length(colnames(data() %>% select(where(is.character)))) == 0){
      validate("There are no character variables in the selected form")
    }

    selectInput(inputId = ns("charvar_in"),
                label = "Select variable of interest:",
                choices = colnames(data() %>% select(where(is.character))),
                selected = colnames(data() %>% select(where(is.character)))[1])

  })
  
  # Reactive data element -----
  
  # Render reactive data element that filters entries for the selected character
  # variable according to the string provided in filt_text
  react_char_dat <- reactive({

    # Convert all character variables to factors for later use in plots
    react_dat <- data() %>%
      mutate(across(where(is.character), as.factor))

    # Filter for input text, if provided.
    if(!is.na(input$filt_text) & !is.null(input$filt_text) & input$filt_text != ""){

      # Match case and ignore all non-alphanumeric characters, if both required
      # based on checkbox input (match_case and alphanum)
      # Note: the \x7f-\xff is needed to match German "Umlaute" and other special letters
      if(isTRUE(input$match_case) & isTRUE(input$alphanum)){

        react_dat <- react_dat %>%
          filter(str_detect(str_remove_all(.data[[input$charvar_in]],
                                           "[^a-zA-Z0-9\x7f-\xff]"),
                            str_remove_all(input$filt_text,
                                           "[^a-zA-Z0-9\x7f-\xff]")))

        # Match case and include non-alphanumeric characters
      } else if(isTRUE(input$match_case) & isFALSE(input$alphanum)) {

        react_dat <- react_dat %>%
          filter(str_detect(.data[[input$charvar_in]], input$filt_text))

        # Ignore non-alphanumeric characters and do not match case
      } else if(isFALSE(input$match_case) & isTRUE(input$alphanum)) {

        react_dat <- react_dat %>%
          filter(str_detect(tolower(str_remove_all(.data[[input$charvar_in]],
                                                   "[^a-zA-Z0-9\x7f-\xff]")),
                            tolower(str_remove_all(input$filt_text,
                                                   "[^a-zA-Z0-9\x7f-\xff]"))))

        # Do not match case or ignore non-alphanumeric characters if none of the 
        # check boxes is checked
      } else {

        react_dat <- react_dat %>%
          filter(str_detect(tolower(.data[[input$charvar_in]]), 
                            tolower(input$filt_text)))

      }

    }

    # If no text is provided in filt_text, return all entries unchanged
    return(react_dat)

  })

  # Display frequencies ------
  
  # Select input to choose between plot and table for display of the frequencies
  output$char_freq_out <- renderUI({
    
    selectInput(inputId = ns("char_freq_in"),
                label = "Choose plot or table:",
                choices = c(Plot = "plot", Table = "table"),
                selected = ifelse(length(levels(react_char_dat() %>% 
                                                  pull(input$charvar_in))) <= 15,
                                  "plot", "table"))
    
  })
  
  # Barplot with frequencies of selected character variable
  output$freq_plot_char <- renderPlot({

    # Break and show this message if the string pattern provided in filt_text is
    # not found in any of the entries for the selected character variable
    if(nrow(react_char_dat()) == 0){
      validate("The string pattern was not found in the data for the selected variable")
    }
    
    # Break and show this message if all entries in the selected character 
    # variable are NA
    if(all(is.na(react_char_dat() %>% pull(input$charvar_in)))){
      validate("All entries for the selected variable are NA")
    }

    # Convert character vectors to factors for plotting
    sumvar_name <- paste0(input$charvar_in, "_sum")

    # Create data with frequencies for plot
    react_char_dat_mod <- react_char_dat() %>%
      group_by(.data[[input$charvar_in]]) %>%
      mutate(!!sym(sumvar_name) := n())

    # Create the plot
    ggplot(data = react_char_dat_mod, aes(.data[[input$charvar_in]])) +
      geom_bar(fill = "#1ea5a5") +
      coord_flip() +
      geom_label(aes(y = .data[[sumvar_name]] ,
                     label = .data[[sumvar_name]]),
                 position = "dodge",
                 show.legend = FALSE) +
      # Shorten factor labels if they are too long
      scale_x_discrete(label = function(x) stringr::str_trunc(x, 15)) +
      theme_bw() +
      theme(legend.position = "none",
            axis.title.x = element_blank())

  })

  # Datatable with frequencies of selected character variable
  output$freq_tab_char <- renderDT({
    
    react_char_dat() %>%
      group_by(.data[[input$charvar_in]]) %>%
      summarize(Frequency = n())
    
  }, rownames = FALSE, options = list(searching = FALSE), filter = "top")

  # Datatables to show all entries -----
  
  # Datatable to show all entries of the selected character variable that match
  # the text entered in the filter box
  
  # Select input to select additional variables to be displayed in the datatable,
  # apart from the selected character variable
  output$idvar_char_out <- renderUI({
    
    selectInput(inputId = ns("idvar_char_in"),
                label = "Select additional variable(s) to be displayed:",
                choices = colnames(data()),
                selected = c("pat_id", "centre.short"),
                multiple = TRUE)
    
  })
  
  # List all entries of the selected character variable
  output$list_entries_char <- renderDT({
    
    react_char_dat() %>%
      select(!!!syms(c(input$idvar_char_in, input$charvar_in))) %>%
      arrange(.data[[input$charvar_in]])
    
  }, rownames = FALSE, options = list(searching = FALSE), filter = "top",
  escape = FALSE)


  # List unique entries of the selected character variable
  output$list_unique_entries_char <- renderDT({
    
    react_char_dat() %>%
      arrange(.data[[input$charvar_in]]) %>%
      distinct(.data[[input$charvar_in]])
    
  }, rownames = FALSE, options = list(searching = FALSE), filter = "top")

  ## ---------------------------------
  ## Date and datetime variables -----
  ## ---------------------------------
  
  # Compare date or datetime variable against fixed minimum and maximum ------
  
  # Select inputs ------
  
  # Select date or datetime variable of interest within the selected form
  output$datevar_out <- renderUI({
    
    # Break and show this message if there are no date or datetime variables
    if(length(colnames(data() %>% select(where(~(is.Date(.x) | is.POSIXt(.x)))))) == 0){
      validate("There are no date or datetime variables in the selected form")
    }

    selectInput(inputId = ns("datevar_in"),
                label = "Select variable of interest:",
                choices = colnames(data() %>% 
                                     select(where(~(is.Date(.x) | 
                                                      is.POSIXt(.x))))),
                selected = colnames(data() %>% 
                                      select(where(~(is.Date(.x) | 
                                                       is.POSIXt(.x)))))[1])

  })
  
  # Select minimum and maximum date allowed
  output$date_range_out <- renderUI({
    
    dateRangeInput(ns("date_range_in"),
                   "Choose date range where the date must fall into",
                   start = min(data() %>% pull(input$datevar_in), na.rm = TRUE),
                   end = max(data() %>% pull(input$datevar_in), na.rm = TRUE)
    )
    
  })
  
  # Value boxes with minimum and maximum ------
  
  # Value boxes with actual minimum and maximum values of the selected date or 
  # datetime variable and the number of entries that fall below the 
  # selected minimum and above the selected maximum
  output$actual_min_date <- renderValueBox({
    
    # Break and show this message if the allowed minimum is larger than the 
    # allowed maximum
    if(as.Date(input$date_range_in[1]) > as.Date(input$date_range_in[2])){
      validate("Selected minimum date is larger than selected maximum date")
    }
    
    # Break and show this message if all entries in the selected numeric variable 
    # are NA
    if(all(is.na(data() %>% pull(input$datevar_in)))){
      validate("All entries for the selected variable are NA")
    }
    
    myval <- min(data()[[input$datevar_in]], na.rm = TRUE)
    
    valueBox(value = myval,
             subtitle = "Actual minimum",
             color = "blue"
    )
    
  })
  
  output$actual_max_date <- renderValueBox({
    
    # Break and show this message if the allowed minimum is larger than the 
    # allowed maximum
    if(as.Date(input$date_range_in[1]) > as.Date(input$date_range_in[2])){
      validate("Selected minimum date is larger than selected maximum date")
    }
    
    # Break and show this message if all entries in the selected numeric variable 
    # are NA
    if(all(is.na(data() %>% pull(input$datevar_in)))){
      validate("All entries for the selected variable are NA")
    }
    
    myval <- max(data()[[input$datevar_in]], na.rm = TRUE)
    
    valueBox(value = myval,
             subtitle = "Actual maximum",
             color = "blue"
    )
    
  })
  
  output$below_min_date <- renderValueBox({
    
    # Break and show this message if the allowed minimum is larger than the 
    # allowed maximum
    if(as.Date(input$date_range_in[1]) > as.Date(input$date_range_in[2])){
      validate("Selected minimum date is larger than selected maximum date")
    }
    
    # Break and show this message if all entries in the selected numeric variable 
    # are NA
    if(all(is.na(data() %>% pull(input$datevar_in)))){
      validate("All entries for the selected variable are NA")
    }
    
    myval <- sum(data()[[input$datevar_in]] < 
                   as.numeric(input$date_range_in[1]), na.rm = TRUE)
    
    valueBox(value = myval,
             subtitle = "Number of entries below allowed minimum",
             color = ifelse(myval == 0, "green", "red")
    )
    
  })
  
  output$above_max_date <- renderValueBox({
    
    # Break and show this message if the allowed minimum is larger than the 
    # allowed maximum
    if(as.Date(input$date_range_in[1]) > as.Date(input$date_range_in[2])){
      validate("Selected minimum date is larger than selected maximum date")
    }
    
    # Break and show this message if all entries in the selected numeric variable 
    # are NA
    if(all(is.na(data() %>% pull(input$datevar_in)))){
      validate("All entries for the selected variable are NA")
    }
    
    myval <- sum(data()[[input$datevar_in]] > 
                   as.numeric(input$date_range_in[2]), na.rm = TRUE)
    
    valueBox(value = myval,
             subtitle = "Number of entries above allowed maximum",
             color = ifelse(myval == 0, "green", "red")
    )
    
  })
  
  # Datatable to show all entries -------
  
  # Datatable to show all entries falling below minimum or above maximum for the 
  # selected date or datetime variable
  
  # Select input to select additional variables to be displayed in the datatable,
  # apart from the selected date or datetime variable variable
  output$idvar_date1_out <- renderUI({

    selectInput(inputId = ns("idvar_date1_in"),
                label = "Select additional variable(s) to be displayed:",
                choices = colnames(data()),
                selected = c("pat_id", "centre.short"),
                multiple = TRUE)

  })

  # List all entries falling below minimum or above maximum for the selected
  # date or datetime variable
  output$list_entries_date <- renderDT({
    
    # Break and show this message if the date range is illogical (i.e., the 
    # selected minimum date is larger than the selected maximum date)
    if(as.Date(input$date_range_in[1]) > as.Date(input$date_range_in[2])){
      validate("Selected minimum date is larger than selected maximum date")
    }

    # Create the table
    data() %>%
      select(!!!syms(c(input$idvar_date1_in, input$datevar_in))) %>%
      filter(.data[[input$datevar_in]] < as.Date(input$date_range_in[1]) |
               .data[[input$datevar_in]] > as.Date(input$date_range_in[2])) %>%
      arrange(.data[[input$datevar_in]]) %>% 
      mutate(across(where(is.POSIXt), as.character))

  }, rownames = FALSE, filter = "top")
  
  # Compare two date or datetime variables --------
  
  # Select and numeric inputs ------
  
  # Select inputs to select the date or datetime variables to compare
  output$datevar1_out <- renderUI({
    
    # Break and show this message if there are no date or datetime variables
    if(length(colnames(data() %>% select(where(~(is.Date(.x) | is.POSIXt(.x)))))) == 0){
      validate("There are no date or datetime variables in the selected form")
    }

    selectInput(inputId = ns("datevar1_in"),
                label = "Select first date variable:",
                choices = colnames(data() %>% 
                                     select(where(~(is.Date(.x) | 
                                                      is.POSIXt(.x))))),
                selected = colnames(data() %>% 
                                      select(where(~(is.Date(.x) |
                                                       is.POSIXt(.x)))))[1])

  })

  output$datevar2_out <- renderUI({

    selectInput(inputId = ns("datevar2_in"),
                label = "Select second date variable:",
                choices = colnames(data() %>% 
                                     select(where(~(is.Date(.x) | 
                                                      is.POSIXt(.x))))),
                selected = colnames(data() %>% 
                                      select(where(~(is.Date(.x) | 
                                                       is.POSIXt(.x)))))[1])

  })
  
  # Numeric input for time difference between dates or datetimes (in days)
  output$difftime_out <- renderUI({
    
    numericInput(ns("difftime_in"),
                label = "Time difference (in days) larger than or equal to:",
                min = 0,
                max = round(as.numeric(max(abs(difftime(data()[[input$datevar1_in]],
                                                        data()[[input$datevar2_in]],
                                                        units = "days")),
                                           na.rm = TRUE))),
                value = 0
    )
    
  })
  
  # Maximum difference between dates
  output$max_diff_out <- renderText({
    
    paste0("The maximum difference between these dates is ",
           round(as.numeric(max(abs(difftime(data()[[input$datevar1_in]],
                                             data()[[input$datevar2_in]],
                                             units = "days")),
                                na.rm = TRUE))),
           " days.")
    
  })

  # Value boxes with comparisons ----
  
  # Data filtered by allowed time difference
  data_datecomp <- reactive({
    
    data() %>%
      filter(as.numeric(abs(difftime(.data[[input$datevar1_in]],
                                     .data[[input$datevar2_in]],
                                     units = "days"))) >= input$difftime_in |
               is.na(.data[[input$datevar1_in]]) | 
               is.na(.data[[input$datevar2_in]]))
    
  })
  
  # Value boxes with comparisons between the two selected date or datetime variables
  output$sum_date_equal <- renderValueBox({
    
    myval <- sum(data_datecomp()[[input$datevar1_in]] ==
                   data_datecomp()[[input$datevar2_in]], na.rm = TRUE)
    
    valueBox(value = myval,
             subtitle = paste0(input$datevar1_in, " = ", input$datevar2_in),
             color = "green"
    )
    
  })
  
  output$sum_date_smaller <- renderValueBox({
    
    myval <- sum(data_datecomp()[[input$datevar1_in]] <
                   data_datecomp()[[input$datevar2_in]], na.rm = TRUE)
    
    valueBox(value = myval,
             subtitle = paste0(input$datevar1_in, " < ", input$datevar2_in),
             color = ifelse(myval == 0, "green", "red")
    )
    
  })
  
  output$sum_date_larger <- renderValueBox({
    
    myval <- sum(data_datecomp()[[input$datevar1_in]] >
                   data_datecomp()[[input$datevar2_in]], na.rm = TRUE)
    
    valueBox(value = myval,
             subtitle = paste0(input$datevar1_in, " > ", input$datevar2_in),
             color = ifelse(myval == 0, "green", "red")
    )
    
  })
  
  output$sum_date1_na <- renderValueBox({
    
    myval <- sum(is.na(data_datecomp()[[input$datevar1_in]]) &
                   !is.na(data_datecomp()[[input$datevar2_in]]))
    
    valueBox(value = myval,
             subtitle = paste0(input$datevar1_in, " is NA"),
             color = ifelse(myval == 0, "green", "red")
    )
    
  })
  
  output$sum_date2_na <- renderValueBox({
    
    myval <- sum(!is.na(data_datecomp()[[input$datevar1_in]]) &
                   is.na(data_datecomp()[[input$datevar2_in]]))
    
    valueBox(value = myval,
             subtitle = paste0(input$datevar2_in, " is NA"),
             color = ifelse(myval == 0, "green", "red")
    )
    
  })
  
  output$sum_both_na <- renderValueBox({
    
    myval <- sum(is.na(data_datecomp()[[input$datevar1_in]]) &
                   is.na(data_datecomp()[[input$datevar2_in]]))
    
    valueBox(value = myval,
             subtitle = "Both dates are NA",
             color = ifelse(myval == 0, "green", "red")
    )
    
  })
  
  # Datatables to show all entries -----
  
  # Datatable to show all entries falling into specific categories (both equal,
  # date 1 > date 2, date 2 > date 1, date 1 NA, date 2 NA, both NA)
  
  # Select input to select additional variables to be displayed in the datatable,
  # apart from the selected date or datetime variables
  output$idvar_date2_out <- renderUI({

    selectInput(inputId = ns("idvar_date2_in"),
                label = "Select additional variable(s) to be displayed in tables below",
                choices = colnames(data()),
                selected = c("pat_id", "centre.short"),
                multiple = TRUE)

  })

  # List all entries where the two selected date or datetime variables are equal
  output$date_equal_list <- renderDT({

    data_datecomp() %>%
      select(!!!syms(c(input$idvar_date2_in, input$datevar1_in, 
                       input$datevar2_in))) %>%
      filter(.data[[input$datevar1_in]] == .data[[input$datevar2_in]]) %>%
      arrange(.data[[input$datevar1_in]]) %>% 
      mutate(across(where(is.POSIXt), as.character))

  }, rownames = FALSE, filter = "top")

  # List all entries where the first selected date or datetime variable is 
  # smaller than the second
  output$date_smaller_list <- renderDT({

    data_datecomp() %>%
      select(!!!syms(c(input$idvar_date2_in, input$datevar1_in, 
                       input$datevar2_in))) %>%
      filter(.data[[input$datevar1_in]] < .data[[input$datevar2_in]]) %>%
      arrange(.data[[input$datevar1_in]]) %>% 
      mutate(across(where(is.POSIXt), as.character))

  }, rownames = FALSE, filter = "top")

  # List all entries where the first selected date or datetime variable is 
  # larger than the second
  output$date_larger_list <- renderDT({

    data_datecomp() %>%
      select(!!!syms(c(input$idvar_date2_in, input$datevar1_in, 
                       input$datevar2_in))) %>%
      filter(.data[[input$datevar1_in]] > .data[[input$datevar2_in]]) %>%
      arrange(.data[[input$datevar1_in]]) %>% 
      mutate(across(where(is.POSIXt), as.character))

  }, rownames = FALSE, filter = "top")
  
  # List all entries where date 1 is NA
  output$date_1_na <- renderDT({
    
    data_datecomp() %>%
      select(!!!syms(c(input$idvar_date2_in, input$datevar1_in,
                       input$datevar2_in))) %>%
      filter(is.na(.data[[input$datevar1_in]]) & 
               !is.na(.data[[input$datevar2_in]])) %>%
      arrange(.data[[input$datevar1_in]]) %>%
      mutate(across(where(is.POSIXt), as.character))
    
  }, rownames = FALSE, filter = "top")
  
  # List all entries where date 2 is NA
  output$date_2_na <- renderDT({
    
    data_datecomp() %>%
      select(!!!syms(c(input$idvar_date2_in, input$datevar1_in,
                       input$datevar2_in))) %>%
      filter(!is.na(.data[[input$datevar1_in]]) &
               is.na(.data[[input$datevar2_in]])) %>%
      arrange(.data[[input$datevar2_in]]) %>%
      mutate(across(where(is.POSIXt), as.character))
    
  }, rownames = FALSE, filter = "top")
  
  # List all entries where both dates are NA
  output$date_both_na <- renderDT({
    
    data_datecomp() %>%
      select(!!!syms(c(input$idvar_date2_in, input$datevar1_in,
                       input$datevar2_in))) %>%
      filter(is.na(.data[[input$datevar1_in]]) &
               is.na(.data[[input$datevar2_in]])) %>%
      mutate(across(where(is.POSIXt), as.character))
    
  }, rownames = FALSE, filter = "top")
  
  
  ## --------------------------------
  ## Compare multiple variables -----
  ## --------------------------------

  # Select input -----
  
  # Select input to select the variables to cross and display
  output$crossvar_out <- renderUI({

    selectizeInput(inputId = ns("crossvar_in"),
                   label = "Select variables to cross",
                   choices = colnames(data()),
                   selected = "centre.short",
                   multiple = TRUE
                   )

  })
  
  # Cross table -----
  
  # Cross table across the selected variables
  output$cross_tab <- renderTable({
    
    # Break and show respective message if no variables are selected or too
    # many variables are selected, resulting in too many entries for the 
    # cross table
    if(length(input$crossvar_in) == 0){
      validate("No variables selected")
    } else if(data() %>% summarize(across(all_of(input$crossvar_in), n_distinct)) %>% prod() > 10000) {
      validate("Crossing the selected variables results in too many entries for display in a cross table")
    }
    
    # Create the table
    ftab <- data() %>%
      # Select variables to cross
      select(!!!syms(input$crossvar_in)) %>%
      # Create "flat" cross table
      ftable(exclude = NULL, col.vars = 1) %>% 
      format(quote = FALSE)
    
    # Prepare the table for display (data frame instead of ftable object)
    out <- data.frame(ftab[-1,], stringsAsFactors = FALSE)
    names(out) <- ftab[1,]
    out
    
  })
  
  # Datatable to show all entries ------
  
  # Datatable to show all entries of the selected variables
  
  # Select input to select additional variables to be displayed in the datatable,
  # apart from the selected variables above
  output$idvar_mult_out <- renderUI({
    
    selectInput(inputId = ns("idvar_mult_in"),
                label = "Select additional variable(s) to be displayed in table below",
                choices = colnames(data()),
                selected = c("pat_id", "centre.short"),
                multiple = TRUE)
    
  })
  
  # List all entries for the selected variables
  output$list_entries_cross <- renderDT({
    
    data() %>%
      select(!!!syms(c(input$idvar_mult_in, input$crossvar_in))) %>% 
      arrange(!!!syms(input$crossvar_in))
    
  }, rownames = FALSE, filter = "top")
  
}
