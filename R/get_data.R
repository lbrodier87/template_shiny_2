
#' Load data
#'
#' @return
#' @export
#'
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
  library(missMethods)

  #######################################################################################################################
  ###                                                     LOAD DATA                                                   ###
  #######################################################################################################################

  data.extraction.date <- Sys.Date()

  set.seed(12481498)
  ## Required vars: centre.short, rando_date.date
  randomized <- data.frame(pat_id = sample(c(1:100), 100),
                           centre.short = sample(c("A", "B", "C", "D", "E"), 100, replace = TRUE),
                           rando_date.date = sample(seq(as.Date('2017/12/01'), as.Date('2022/03/01'), by="day"), 100))

  locations <- data.frame(centre.short = LETTERS[1:5],
                          long = c(6.5848, 8.9857, 8.9632, 7.4688, 10.2411),
                          lat = c(46.5980, 46.0868, 47.1502, 47.3604, 46.6630),
                          monthly = c(2,1,2,1,5),
                          target = c(35, 25, 30, 30, 30))
  
  study_params <- data.frame(acc_target = 150,
                            study_start = as.Date('2017/12/01'))
  
  sae <- data.frame(pat_id = sample(randomized$pat_id, 50, replace = T),
                    sae_date = sample(seq(as.Date('2017/12/01'), as.Date('2022/03/01'), by="day"), 50), 
                    severity_level = sample(c("Mild", "Moderate", "Severe"), 50, replace = T), 
                    causality = sample(c("Certain", "Probable", "Possible", "Unlikely", "Not related", "Not assessable"), 50, replace = T), 
                    expectedness = sample(c("Expected", "Unexpected"), 50, replace = T), 
                    outcome = sample(c("Resolved without sequelae", "Resolved with sequelae", "Ongoing", "Death", "Unknown", "Other"), 50, replace = T), 
                    death = sample(c("Yes", "No"), 50, T), 
                    life_threatening = sample(c("Yes", "No"), 50, T),
                    persistant_disability = sample(c("Yes", "No"), 50, T),
                    hospitalization = sample(c("Yes", "No"), 50, T),
                    congenital_anomyla_birth_defect = sample(c("Yes", "No"), 50, T))
  sae$centre.short <- sapply(sae$pat_id, function(x){randomized$centre.short[randomized$pat_id == x]})
   
  ## TODO: delete
  set.seed(28991)
  missing <- tibble(
    age = rnorm(1000, 40, 10),
    gender = sample(c("m", "f"), 1000, replace = TRUE),
    weight = ifelse(
      gender == "m",
      70 + rnorm(1, 0, 20),
      60 + rnorm(1, 0, 15)
    )
  ) %>%
    missMethods::delete_MCAR(.2) 
 
  consistency <- randomized %>% 
    mutate(height = rnorm(nrow(randomized), 170, 30),
           weight = rnorm(nrow(randomized), 70, 30),
           sex = sample(c("male", "female", "unknown"), nrow(randomized), 
                        replace = TRUE),
           department = sample(c("ICU", "icu", "Icu", "Ic", "Oncology", 
                                 "oncology", "ONcology", "Onco", "ncology"), 
                               nrow(randomized), 
                               replace = TRUE),
           diagnosis = paste0(sample(c("This patient has",
                                            paste0(rep("some long text", 20), collapse = " "),
                                            paste0(rep("some shorter text", 5), collapse = " ")),
                                            nrow(randomized), replace = TRUE),
                              sample(c(" high blood pressure ",
                                       " low blood pressure ",
                                       " lung cancer ",
                                       " breast cancer ",
                                       " fever ",
                                       " an S.aureus infection ",
                                       " an S. aureus infection",
                                       " an E.-Coli infection ",
                                       " an E coli infection "), 
                                     nrow(randomized), replace = TRUE),
                              sample(c(".",
                                       paste0(rep("some long text", 20), collapse = " "),
                                       paste0(rep("some shorter text", 5), collapse = " ")),
                                     nrow(randomized), replace = TRUE)
                              )
           )
  

  #######################################################################################################################
  ###                                                     SAVE DATA                                                   ###
  #######################################################################################################################

  data <- list(
    data.extraction.date = data.extraction.date,
    randomized = randomized,
    locations = locations,
    study_params = study_params,
    sae = sae,
    missing = missing,
    consistency = consistency
  )

  return(data)
}

