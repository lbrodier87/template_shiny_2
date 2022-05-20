#' Get reactive data
#'
#' @param input inputs from UI
#' #@param output
#' #@param session
#' @return
#' @noRd
get_reactive_data <- function(input){

  data <- get_data()

  random_period <- reactive(
    
    if(input$center != "All"){
      filter(data$randomized, centre.short == input$center & rando_date.date >= input$period[1] & rando_date.date <= input$period[2])
      
    } else{
      filter(data$randomized, rando_date.date >= input$period[1] & rando_date.date <= input$period[2])
    })
  
  all_period <- reactive(

    if(input$center != "All"){
      filter(data$all, centre.short == input$center & rando_date.date >= input$period[1] & rando_date.date <= input$period[2])

    } else{
      filter(data$all, rando_date.date >= input$period[1] & rando_date.date <= input$period[2])
    })
  
  # missing_period <- reactive(
  #   
  #   if(input$center != "All"){
  #     filter(data$missing, centre.short == input$center & rando_date.date >= input$period[1] & rando_date.date <= input$period[2])
  #     
  #   } else{
  #     filter(data$missing, rando_date.date >= input$period[1] & rando_date.date <= input$period[2])
  #   })
  
  queries_period <- reactive({
    
    nr.rows <- all_period() %>% nrow()
    df <- purrr::map_dfr(1:nr.rows, get_queries, df = all_period())
    no <- df %>% nrow()
    set.seed(12481498)
    df %<>% mutate(querystatus = sample(c("answered", "open", "closed"), no, replace = TRUE, prob = c(0.5, 0.2, 0.3)),
                   queryform = sample(c("Adverse events", "Diagnosis", "Biobanking", "MRI", "Laboratory"), no, replace = TRUE, prob = c(0.2, 0.2, 0.2, 0.2, 0.2)),
                   query = sample(c("Date:Please enter a date", "Date:Event date is greater than current date", "Description: Value required"), no, replace = TRUE, prob = c(0.5, 0.3, 0.2))) %>% 
      separate(query, c("query.field", "query"), sep = ":")
    return(df)
    
  })

  ## Save reactive datafranes into list

  reactive_data <- list(
    rx_random = random_period,
    rx_all = all_period,
    rx_queries = queries_period
    # , rx_missing = missing_period
  )

  return(reactive_data)
}

