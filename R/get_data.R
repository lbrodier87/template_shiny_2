
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
                          monthly = c(2,1,2,1,5))
  
  ## read secuTrial test data to illustrate the completeness module
  st_data = system.file("extdata/sT_exports/exp_opt/s_export_CSV-xls_CTU05_all_info.zip",
              package = "secuTrialR") %>%
    read_secuTrial() %>%
    magrittr::extract(c(
      "esurgeries", "baseline", "outcome", "treatment", "allmedi", "studyterminat", "ae", "sae"
    )) %>%
    purrr::map(tibble) %>%
    purrr::map(~ .x %>% select(-contains(".factor")))

  #######################################################################################################################
  ###                                                     SAVE DATA                                                   ###
  #######################################################################################################################

  data <- list(
    data.extraction.date = data.extraction.date,
    randomized = randomized,
    locations = locations,
    st_data = st_data
    )

  return(data)
}

