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
      df <- readr::read_rds("https://github.com/datasketch/dane-economia-circular-app/blob/main/data/dataDane.rds?raw=true")
      df <- df[[r$quest_choose]][[r$varViewId]]
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
