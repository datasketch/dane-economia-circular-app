#' viz_data UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_viz_data_ui <- function(id){
  ns <- NS(id)
  tagList(
    
  )
}

#' viz_data Server Functions
#'
#' @noRd 
mod_viz_data_server <- function(id, r){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    
    
    data_viz <- reactive({
      tryCatch({
        req(r$d_fil)
        req(r$dic_var)
        df <- r$d_fil
        varNum <- unique(r$dic_var$variables_cantidad)
        df[[grep(varNum, names(df))]] <- round(df[[grep(varNum, names(df))]], unique(r$dic_var$num_dic)[1])
        
        
        if (ncol(df) > 3) {
          if ("Año" %in% names(df)) {
            df <- EcotoneFinder::arrange.vars(df, vars =c("Año" = 2))
          }
          if ("Trimestre" %in% names(df)) {
            df <- EcotoneFinder::arrange.vars(df, vars =c("Trimestre" = 2))
          }
        }
        
        if (!is.null(r$varToVizId)) {
          if (all(r$varToVizId %in% names(df))) {
            
            df <- df[,c(r$varToVizId, varNum, "label")]
            if ("Año" %in% r$varToVizId) {
              if (length(r$varToVizId) != 1) {
                varToplot <- setdiff(r$varToVizId, "Año")
                df <- df[,c(varToplot, "Año", varNum, "label")]
              }
            }
            if ("Trimestre" %in% r$varToVizId) {
              if (length(r$varToVizId) != 1) {
                varToplot <- setdiff(r$varToVizId, "Trimestre")
                df <- df[,c(varToplot, "Trimestre", varNum, "label")]
              }
            }
            if (length(r$varToVizId) == 1) {
              df <- df[,c(r$varToVizId, varNum, "label")]
            }
          }
        } 
        
        if ("Año" %in% names(df)) {
          if (length(unique(df$Año)) == 1) {
            df <- df[,-grep("Año", names(df))]
          }
        }
        
        if ("Trimestre" %in% names(df)) {
          if (length(unique(df$Trimestre)) == 1) {
            df <- df[,-grep("Trimestre", names(df))]
          }
        }
      },
      error = function(cond) {
        return()
      })
      df
    })
    
    observe({
      r$d_viz <- data_viz()
    })
    
    
  })
}

## To be copied in the UI
# mod_viz_data_ui("viz_data_1")

## To be copied in the server
# mod_viz_data_server("viz_data_1")
