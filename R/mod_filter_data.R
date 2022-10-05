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
    
    data_clean <- reactive({
      tryCatch({
        
        print(r$d_sel)
        
        if(is.null(r$d_sel)) {
          return()
        } else {
          req(r$quest_choose)
          req(r$varViewId)
          req(r$dic)
          
          df <- r$d_sel
          
          ind <- data.frame(indicador = unique(r$dic$indicador[r$dic$id %in% r$quest_choose]))
    
          dicFilters <- r$dic %>% dplyr::filter( idIndicador %in% r$varViewId) %>% tidyr::drop_na(variables)
          dicFilters$temV <- make.names(dicFilters$variables)

          
          purrr::map(1:nrow(dicFilters), function(i) {
         
            ids <- make.names( dicFilters$variables[i])
            if(!is.null(r[[ids]])) {
              varS <- make.names(r[[ids]])
              varF <- dicFilters %>% dplyr::filter(temV %in% ids) 
              indR <- grep(paste0("^",varS, "$", collapse = "|"), make.names(df[[unique(varF$variables)]]))
              df <<- df[indR,]
              df
            } 
          })
      
    
        
          idFor <- grep("formatNum", names(df))
          if (!identical(idFor, integer())) {
            df <- df[,-idFor]
          }
          
          idEt <- grep("Etiqueta", names(df))
          if (!identical(idEt, integer())) {
            df <- df[,-idEt]
          }
          
         
          if (ncol(df) <= 4) {
            if ("Año" %in% names(df)) {
            df <- df %>% dplyr::select(Año, dplyr::everything())
            }
            if ("Trimestre" %in% names(df)) {
              print("aqui")
              df <- df %>% dplyr::select(Trimestre, dplyr::everything())  
            }
          } else {
            if (!is.null(r$selViewId)) {
              if ("Variable" %in% names(df)) {
              df <- df %>% dplyr::filter(Variable %in% r$selViewId)
              df <- df %>% dplyr::select(-Variable)
              }
            }
            if ("Año" %in% names(df)) {
            df <- EcotoneFinder::arrange.vars(df, vars =c("Año" = 2))
            }
            if ("Trimestre" %in% names(df)) {
              df <- EcotoneFinder::arrange.vars(df, vars =c("Trimestre" = 2))
            }
          }
       
          df
        }
      },
      error = function(cond) {
        return()
      })
    })
    
    data_filter <- reactive({
      req(data_clean())
      df <- data_clean()
      print(df)
      idUn <- grep("Unidad", names(df)) 
      
      if (length(unique(df[[idUn]])) > 1) {
        req(r$unidadId)
        df <- df[df[[idUn]] == r$unidadId,]
      }
        
     
      if (!identical(idUn, integer())) {
        df <- df[,-idUn]
      }
      
      print(df)
      
      df
    })
    
    
    
    observe({
      r$d_cl <- data_clean()
      r$d_fil <- data_filter()
    })
    
  })
}

## To be copied in the UI
# mod_filter_data_ui("filter_data_ui_1")

## To be copied in the server
# mod_filter_data_server("filter_data_ui_1")
