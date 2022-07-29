#' selected_data UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_selected_data_ui <- function(id){
  ns <- NS(id)
  tagList(
    
  )
}

#' selected_data Server Functions
#'
#' @noRd 
mod_selected_data_server <- function(id, r){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    
    data_select <- reactive({
      req(r$quest_choose)
      req(r$varViewId)
      #print(r$varViewId)
      df <- NULL
      if (r$quest_choose == "demanda") {
       df <- dataDemanda[[r$varViewId]]
      } else if (r$quest_choose == "factores") {
      df <- dataFactores[[r$varViewId]]
      } else if (r$quest_choose == "conservacion") {
      df <- dataConservacion[[r$varViewId]]
      } else if (r$quest_choose == "presion") {
      df <- dataPresion[[r$varViewId]]
      } else {
        return()
      }
      df
    })
    
    
    observe({
      r$d_sel <- data_select()
    })
    

    
  })
}

## To be copied in the UI
# mod_selected_data_ui("selected_data_ui_1")

## To be copied in the server
# mod_selected_data_server("selected_data_ui_1")
