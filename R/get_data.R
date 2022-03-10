
get_data <- function(){
  #######################################################################################################################
  ###                                                     LIBRARIES                                                   ###
  #######################################################################################################################
  library(secuTrialR)
  library(dplyr)
  library(magrittr)
  library(stringr)
  library(tidyr)
  library(lubridate)
  library(janitor)
  library(tables)
  
  #######################################################################################################################
  ###                                                     LOAD DATA                                                   ###
  #######################################################################################################################

  data.extraction.date <- Sys.Date()
  
  set.seed(12481498)
  ## Required vars: centre.short, rando_date.date
  randomized <- data.frame(pat_id = sample(c(1:100), 100),
                           centre.short = sample(c("A", "B", "C", "D", "E"), 100, replace = TRUE),
                           rando_date.date = sample(seq(as.Date('2017/12/01'), as.Date('2022/03/01'), by="day"), 100)) 
  
  #######################################################################################################################
  ###                                                     SAVE DATA                                                   ###
  #######################################################################################################################
  
  data <- list(
    data.extraction.date = data.extraction.date,
    randomized = randomized
  )
  
  return(data)
}

