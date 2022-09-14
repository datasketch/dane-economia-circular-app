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
          req(r$quest_choose)
          req(r$varViewId)
          #req(r$selViewId)
          
          df <- r$d_sel
          
          ind <- data.frame(indicador = unique(indiceDane$indicador[indiceDane$id %in% r$quest_choose]))
          dicFilters <- indiceDane %>% dplyr::filter( idIndicador %in% r$varViewId) %>% tidyr::drop_na(variables)
          dicFilters$temV <- make.names(dicFilters$variables)

          
          purrr::map(1:nrow(dicFilters), function(i) {
         
            ids <- make.names( dicFilters$variables[i])
            if(!is.null(r[[ids]])) {
              varS <- make.names(r[[ids]])
              varF <- dicFilters %>% dplyr::filter(temV %in% ids) 
              indR <- grep(paste0(varS, collapse = "|"), make.names(df[[unique(varF$variables)]]))
              df <<- df[indR,]
              df
            } 
          })
      
          idUn <- grep("Unidad", names(df))
          if (!identical(idUn, integer())) {
            df <- df[,-idUn]
          }
          idFor <- grep("formatNum", names(df))
          if (!identical(idFor, integer())) {
            df <- df[,-idFor]
          }
       
          if (ncol(df) <= 3) {
            df <- df %>% dplyr::select("Año", dplyr::everything())
          } else {
            if (!is.null(r$selViewId)) {
              if ("Variable" %in% names(df)) {
              df <- df %>% dplyr::filter(Variable %in% r$selViewId)
              df <- df %>% dplyr::select(-Variable)
              }
            }
            df <- EcotoneFinder::arrange.vars(df, vars =c("Año" = 2))
          }
          
 
          df
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
