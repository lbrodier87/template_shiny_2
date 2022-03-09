#' Get reactive data
#'
#' @param input,output,session
#' @return
#' @export
#'
#' @examples
get_reactive_data <- function(input){
  
  data <- get_data()
  
  random_period <- reactive(
    
    if(input$center != "All"){
      filter(data$randomized, centre.short == input$center & rando_date.date >= input$period[1] & rando_date.date <= input$period[2])
      
    } else{
      filter(data$randomized, rando_date.date >= input$period[1] & rando_date.date <= input$period[2])
    })
 
  
  ## Save reactive datafranes into list
  
  reactive_data <- list(
    rx_random = random_period
  )
  
  return(reactive_data)
}

