#' The application User-Interface
#'
#' @import lubridate
#' @import shiny
#' @import shinydashboard
#'
#' @noRd

app_ui <- function() {



    data <- get_data()
    ## Import module label ids
    mod <- get_modules()

    ## List the first level UI elements here
    fluidPage(

      dashboardPage(skin = "blue",

                    ## Header
                    dashboardHeader(title = paste0("Study as of ",
                                                   data$data.extraction.date
                                                   ), titleWidth = 300),

                    ## Sidebar
                    dashboardSidebar(

                      tags$head(tags$style(HTML('.logo {
                              background-color: #1ea5a5 !important;

                              }
                              .navbar {
                              background-color: #1ea5a5  !important;

                              }

                              .navbar-custom .navbar-text {
                                color: black;
                              }
                              '))),

                      sidebarMenu(
                        ## Overview tab
                        menuItem("Performance measures", startExpanded = TRUE,
                                 ## 1st SIDEBAR TAB: Recruitment and retention
                                 menuItem("Recruitment", tabName = mod$recruit, icon = icon("chart-line")),
                                 menuItem("Retention", tabName = mod$retention, startExpanded = TRUE, icon = icon("door-open")),
                                 menuItem("Data quality", startExpanded = TRUE, icon = icon("database"),
                                          menuItem("Completeness", tabName = mod$completeness),
                                          menuItem("Consistency", tabName = mod$consistency, badgeLabel = "Upcoming", badgeColor = "green"),
                                          menuItem("Timeliness", tabName = mod$timeliness),
                                          menuItem("Queries", tabName = mod$queries))),
                        menuItem("Study management", startExpanded = TRUE,
                                 ## 1st SIDEBAR TAB: Recruitment and retention
                                 menuItem("Follow-up visits", startExpanded = TRUE, icon = icon("clinic-medical")),
                                 menuItem("Patient characteristics", tabName = mod$patient, icon = icon("address-card")),
                                 menuItem("Safety management", startExpanded = TRUE, icon = icon("notes-medical"),
                                          menuItem("Serious adverse events", tabName = mod$sae),
                                          menuItem("Adverse events", tabName = mod$ae))),

                        ## Date range filter
                        dateRangeInput("period", "Randomization date:",
                                       start = as.POSIXct("2017-07-01"),
                                       end   = as.POSIXct(today())),
                        ## Center filter
                        selectInput("center", "Acute center", choices = c("All", "A", "B", "C", "D", "E"), selected = "All"),
                        width = "350")),

                    ## Body
                    dashboardBody(

                      tabItems(

                        ## Recruitment tab
                        mod_recruitment2_ui(mod$recruit2, label = mod$recruit2),
                        mod_recruitment_ui(mod$recruit, label = mod$recruit)
                      )
                    ) ## dashboardBody
      ) ## dashboardPage
    )



}


