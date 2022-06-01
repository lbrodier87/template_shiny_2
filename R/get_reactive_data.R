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

  consistency_period <- reactive(
    
    if(input$center != "All"){
      filter(data$consistency, centre.short == input$center & rando_date.date >= input$period[1] & rando_date.date <= input$period[2])
      
    } else{
      filter(data$consistency, rando_date.date >= input$period[1] & rando_date.date <= input$period[2])
    })

  ## Save reactive datafranes into list

  reactive_data <- list(
    rx_random = random_period,
    rx_all = all_period
    # , rx_missing = missing_period
    , rx_consistency = consistency_period
  )

  return(reactive_data)
}

