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
          
          req(r$selViewId)
          req(r$varNumId)
          varNumId <- r$varNumId
          df <- r$d_sel
          
        
          ind <- data.frame(indicador = unique(indiceDane$indicador[indiceDane$id %in% r$quest_choose]))
          dicFilters <- indiceDane %>% dplyr::filter( idIndicador %in% r$varViewId) %>% tidyr::drop_na(variables)
          dicFilters$temV <- tolower(stringi::stri_trans_general(str = dicFilters$variables, id = "Latin-ASCII"))
          print("$$$$$$$$$$$$$$$$4")
          print(dicFilters)
          
          purrr::map(1:nrow(dicFilters), function(i) {
         
            ids <- tolower(stringi::stri_trans_general(str = dicFilters$variables[i], id = "Latin-ASCII"))
            if(!is.null(r[[ids]])) {
              varS <- r[[ids]]
              print(df)
              varF <- dicFilters %>% dplyr::filter(temV %in% ids) 
              indR <- grepl(paste0(varS, collapse = "|"), df[[varF$variables]])
              print("######3")
              print(indR)
              df <<- df[indR,]
            } 
          })
         
          # if (varNumId != "Porcentaje") {
          #   varNumId <- names(df)[grepl("Valor|Estimador Total",names(df))]
          # }
          #if (r$varViewId == "") {
          indNa <- is.na(df[[varNumId]])
          df <- df[!indNa,]
          #}
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
