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
      req(r$dataAll)
      req(r$quest_choose)
      req(r$varViewId)
      df <- r$dataAll
      df <- df[[r$quest_choose]][[r$varViewId]]
      df
    })
    
    dic <- reactive({
      req(r$dataAll)
      dic <- r$dataAll$dic
      dic %>% dplyr::select(-`No se encuentra en el dashboard`, -`mapa`)
    })
    
    
    observe({
      r$d_sel <- data_select()
      r$dic <- dic()
    })
    
    
    
  })
}

## To be copied in the UI
# mod_selected_data_ui("selected_data_ui_1")

## To be copied in the server
# mod_selected_data_server("selected_data_ui_1")
