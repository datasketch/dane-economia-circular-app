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
    uiOutput(ns("controls")),
    uiOutput(ns("filterOptions"))
  )
}

#' load_parmesan Server Functions
#'
#' @noRd 
mod_load_parmesan_server <- function(id, r){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    
    
    dicFilter <- reactive({
      req(r$quest_choose)
      req(r$varViewId)
      df <- indiceDane %>% dplyr::filter(id %in% r$quest_choose, idIndicador %in% r$varViewId)
      df
    })
    
    output$filterOptions <- renderUI({
      req(dicFilter())
      df <- dicFilter() %>% dplyr::distinct(variables, .keep_all = T)
      if (nrow(df) > 0) {
        purrr::map(1:nrow(df), function(i) {
          print("variable de filtro")
          print(make.names(df$variables[i]))
          shiny::selectizeInput(inputId = ns(make.names(df$variables[i])), 
                                label = paste0("Filtrar ", df$variables[i]), 
                                choices = setdiff(unique(r$d_sel[[df$variables[i]]]), NA),
                                selected = NULL, multiple = TRUE, 
                                options = list(placeholder = "Todos", 
                                               plugins = list(
                                                 "remove_button",
                                                 "drag_drop"))
          )
        })
      }
      
    })
    
    
    
    observe({
      req(dicFilter())
      df <- dicFilter()
      extra_inputs <- make.names( df$variables)
      r$titleViz <- dicFilter()
      for(extra_input in extra_inputs){
        get_extraInput <- input[[extra_input]]
        r[[extra_input]] <- get_extraInput
      }
    })
    
    
    
    var_opts <- reactive({
      req(r$quest_choose)
      
      df <- indiceDane %>% dplyr::filter(id %in% r$quest_choose) %>% dplyr::distinct(idIndicador, indicador)
      setNames(df$idIndicador, df$indicador)
      
    })
    
    
    selec_opts <- reactive({
      req(r$d_sel)
      df <- r$d_sel
      ch <- NULL
      if ("Variable" %in% names(df)) {
        ch <- unique(df$Variable)
      }
      ch
    })
  
    selec_opts_def <- reactive({
      if (is.null(selec_opts())) return()
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
