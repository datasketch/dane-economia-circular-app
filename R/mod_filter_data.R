#' filter_data UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_filter_data_ui <- function(id){
  ns <- NS(id)
  tagList(
    
  )
}

#' filter_data Server Functions
#'
#' @noRd 
mod_filter_data_server <- function(id, r){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    
    data_filter <- reactive({
      tryCatch({
        if(is.null(r$d_sel)) {
          return()
        } else {
          req(r$selViewId)
          req(r$varNumId)
          varNumId <- r$varNumId
          df <- r$d_sel
          if (varNumId != "Porcentaje") {
            varNumId <- names(df)[grepl("Valor|Estimador Total",names(df))]
          }
          df[,c(r$selViewId, varNumId)]
        }
      },
      error = function(cond) {
        return()
      })
    })
    
    observe({
      r$d_fil <- data_filter()
    })
    
  })
}

## To be copied in the UI
# mod_filter_data_ui("filter_data_ui_1")

## To be copied in the server
# mod_filter_data_server("filter_data_ui_1")