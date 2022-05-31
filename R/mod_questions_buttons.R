#' questions_buttons UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_questions_buttons_ui <- function(id){
  ns <- NS(id)
  tagList(
    uiOutput(ns("generalFilters"))
  )
}

#' questions_buttons Server Functions
#'
#' @noRd 
mod_questions_buttons_server <- function(id, r){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    
    output$generalFilters <- renderUI({
      question_buttons(c("extraccion", "consumo", "produccion", "cierre"), 
                       c("Extracción de reservas mineras",
                         "Consumo y uso", 
                         "Producción de bienes y servicios",
                         "Cierre y optimización en ciclos de vida de los materiales y productos")
      )
    })
    
 
  })
}

## To be copied in the UI
# mod_questions_buttons_ui("questions_buttons_ui_1")

## To be copied in the server
# mod_questions_buttons_server("questions_buttons_ui_1")
