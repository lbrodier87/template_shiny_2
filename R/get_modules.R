#' Retrieves a list of all module aliases
#'
#' Module aliases retrieved via get_modules() are used to define module IDs in shiny::callModule(),
#' and are also used as tabnames for the sidebar, and for the shiny UI label and id.
#'
#' @return list of strings containing module aliases
#' @seealso \code{\link{app_ui}}, \code{\link{app_srv}}
#' @export
#'
get_modules <- function(){
  # a list of all module names
  mod <- list(
    ## Performance measures
    recruit = "mod_recruit",
    recruit2 = "mod_recruit2",
    completeness = "mod_completeness",
    consistency = "mod_consistency",
    timeliness = "mod_timeliness",
    queries = "mod_queries",
    ## Study management
    visits = "mod_visits",
    completed = "mod_completed",
    patient = "mod_patient",
    safetymgm = "mod_safetymgm",
    sae = "mod_sae",
    ae = "mod_ae"
  )
  return(mod)
}
