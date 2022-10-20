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
      req(r$dataAll$dic)
      req(r$quest_choose)
      req(r$varViewId)
      df <- r$dataAll$dic %>% dplyr::filter(id %in% r$quest_choose, idIndicador %in% r$varViewId)
      df
    })
    
    
    varToViz <- reactive({
      req(dicFilter())
      dic <- dicFilter()
      if (length(unique(dic$variables)) > 2 | "Departamento" %in% dic$variables) {
        sort(unique(dic$variables))
      } else {
        return()
      }
    })
    
    varTo_opts <- reactive({
      req(varToViz())
      varToViz()[c(3,2)]
    })
    
    
    
    # output$varExtraFilter <- renderUI({
    #   if (is.null(dicFilter())) return()
    #   varV <- setdiff(r$varToVizId, c("Trimestre", "Departamento"))
    #   # if (!identical(varV, character())) {
    #   #   if (nameLabel %in% r$varToVizId) {
    #   #     sel_var <- choices_var[1]
    #   #   }
    #   # }
    # 
    # })
    
    
    output$filterOptions <- renderUI({
      req(dicFilter())
      req(r$d_sel)
      df <- dicFilter() %>% dplyr::distinct(variables, .keep_all = T)
   
      if (nrow(df) > 0) {
        purrr::map(1:nrow(df), function(i) {
          print("variable de filtro")
          print(make.names(df$variables[i]))
          nameLabel <- df$variables[i]
          sel_var <- NULL
          choices_var <- setdiff(unique(r$d_sel[[df$variables[i]]]), NA)
          print(varToViz())
          if (!is.null(varToViz())) {
            varV <- setdiff(varToViz(), c("Trimestre", "Departamento", "Año", r$varToVizId))
            print("%%%%%%%%%%%%%%%")
            print(varV)
            print("%%%%%%%%%%%%%%%")
            if (!identical(varV, character())) {
              if (nameLabel %in% varV) {
              sel_var <- choices_var[1]
              }
            }
          }
          if (!nameLabel %in% c("Año", "Trimestre", "Departamento")) nameLabel <- "Variable"
          shiny::selectizeInput(inputId = ns(make.names(df$variables[i])), 
                                label = paste0("Filtrar ", nameLabel), 
                                choices = choices_var,
                                selected = sel_var, multiple = TRUE, 
                                options = list(placeholder = "Todos", 
                                               plugins = list(
                                                 "remove_button",
                                                 "drag_drop"))
          )
        })
      }
      
    })
    
    unidad_ch <- reactive({
      req(r$d_cl)
      idUn <- grep("Unidad", names(r$d_cl)) 
      length(unique(r$d_cl[[idUn]])) > 1
    })
    
    unidad_opts <- reactive({
      req(r$d_cl)
      idUn <- grep("Unidad", names(r$d_cl)) 
      unique(r$d_cl[[idUn]])
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
      #print(r$dataAll$dic)
      #req(r$dataAll$dic)      
      df <- r$dataAll$dic %>% dplyr::filter(id %in% r$quest_choose) %>% dplyr::distinct(idIndicador, indicador)
      setNames(df$idIndicador, df$indicador)
      
    })
    
    
    # selec_opts <- reactive({
    #   req(r$d_sel)
    #   df <- r$d_sel
    #   ch <- NULL
    #   #vARIABLE CAMBIAR
    #   
    #   vars <- grep("Variable asociada|Departamento", names(df))
    #   if (!identical(vars, integer())) {
    #     ch <- unique(df$Variable)
    #   }
    #   ch
    # })
    # 
    # selec_opts_def <- reactive({
    #   if (is.null(selec_opts())) return()
    #   selec_opts()[1]
    # })
    
    
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
      r$dic_var <- dicFilter()
    })
    
    
  })
}

## To be copied in the UI
# mod_load_parmesan_ui("load_parmesan_ui_1")

## To be copied in the server
# mod_load_parmesan_server("load_parmesan_ui_1")
