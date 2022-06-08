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
  
  locations_filtered <- reactive(
    if(input$center != "All"){
      filter(data$locations, centre.short == input$center)
    } else data$locations <- data$locations
  )
  
  # missing_period <- reactive(
  #   
  #   if(input$center != "All"){
  #     filter(data$missing, centre.short == input$center & rando_date.date >= input$period[1] & rando_date.date <= input$period[2])
  #     
  #   } else{
  #     filter(data$missing, rando_date.date >= input$period[1] & rando_date.date <= input$period[2])
  #   })


  ## Save reactive dataframes into list

  reactive_data <- list(
    rx_random = random_period,
    rx_all = all_period
    # , rx_missing = missing_period
    , rx_locations = locations_filtered
  )

  return(reactive_data)
}

