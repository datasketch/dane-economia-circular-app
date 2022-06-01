#' load_parmesan UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_load_parmesan_ui <- function(id){
  ns <- NS(id)
  tagList(
    uiOutput(ns("controls"))
  )
}

#' load_parmesan Server Functions
#'
#' @noRd 
mod_load_parmesan_server <- function(id, r){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    
    var_opts <- reactive({
      req(r$quest_choose)
      
      ind <- data.frame(indicador = unique(indiceDane$indicador[indiceDane$id %in% r$quest_choose]))
      df <- indiceDane %>% dplyr::filter(indicador %in% ind$indicador) %>% dplyr::distinct(idIndicador, indicador)
      setNames(df$idIndicador, df$indicador)
      
    })
    
    
    selec_opts <- reactive({
      req(r$varViewId)
      df <- indiceDane %>% dplyr::filter(idIndicador %in% r$varViewId)
      #print(df)
      if(all(is.na(df$variables))) return()
      if(all(is.na(df$varId))) {
        unique(df$variables)
      } else {
        setNames(df$varId, df$variables)
      }
      
    })
    
    selec_opts_def <- reactive({
      req(selec_opts())
      selec_opts()[1]
    })
    
    
    # Initialize parmesan
    path <- app_sys("app/app_config/parmesan")
    parmesan <- parmesan::parmesan_load(path)
    parmesan_input <- parmesan::parmesan_watch(input, parmesan)
    
    parmesan::output_parmesan("controls",
                              parmesan = parmesan,
                              #r = r,
                              input = input,
                              output = output,
                              session = session,
                              env = environment())
    # # ======================================================================================================================
    # Pass all inputs from parmesan to other parts of the app as reactiveValues
    parmesan_inputs <- purrr::map(parmesan, function(.x) { purrr::map_chr(.x$inputs, "id")}) %>% unlist(use.names = FALSE)
    
    observe({
      for(parmesan_input in parmesan_inputs){
        get_input <- input[[parmesan_input]]
        #if(!is.null(get_input)){
        r[[parmesan_input]] <- get_input
        #}
      }
    })
    
    observe({
      r$parmesan_input <- parmesan_input()
    })
    
    
  })
}

## To be copied in the UI
# mod_load_parmesan_ui("load_parmesan_ui_1")

## To be copied in the server
# mod_load_parmesan_server("load_parmesan_ui_1")
