#' Create initial users for shiny app / execute only once
#' @import shinymanager

if(!exists("users.sqlite")){
  #create initial users credentials
  credentials <- data.frame(
    user = c("admin", "ubri", "guest"),             # mandatory
    password = c("admin", "ubri", "guest"),         # mandatory
    start = c("2023-04-18"),                          # optional (all others)
    expire = c(NA, NA, "2023-04-30"),
    admin = c(TRUE, FALSE, FALSE),
    comment = c(""),
    role = c("admin", "PI", "guest"),             # additional custom parameter
    access_recruitment_prediction = c(TRUE, TRUE, FALSE),
    access_sae = c(TRUE, TRUE, FALSE),
    access_ae = c(FALSE, TRUE, FALSE),
    stringsAsFactors = FALSE
  )
  #write to sqlite db
  shinymanager::create_db(
    credentials_data = credentials,
    sqlite_path = "users.sqlite",
    passphrase = "scto_rshiny_app"
  )
}