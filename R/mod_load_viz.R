#' load_viz UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_load_viz_ui <- function(id){
  ns <- NS(id)
  tagList(
   uiOutput(ns("viz_print"))
  )
}

#' load_viz Server Functions
#'
#' @noRd 
mod_load_viz_server <- function(id, r){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    
   
    output$aver <- renderPrint({
      #print("viiiiiiiz")
      #print(r$v_type)
      r$d_fil
    })
    
    title_viz <- reactive({
      tx <- r$titleViz
      
      if (is.null(tx)) return()
      if (nrow(tx) == 0) tx <- " "

      req(r$varNumId)
      req(r$selViewId)
      tx <- tx %>% dplyr::filter(variables %in% r$selViewId, variables_cantidad %in% r$varNumId)
      tx <- unique(tx$`Nombre gráfica`)
    })
    
    caption_viz <- reactive({
      tx <- r$titleViz
      
      if (is.null(tx)) return()
      if (nrow(tx) == 0) {
        tx <- " "
      } else {
      req(r$varNumId)
      req(r$selViewId)
      tx <- tx %>% dplyr::filter(variables %in% r$selViewId, variables_cantidad %in% r$varNumId)
      tx <- unique(tx$`Nota del indicador`)
      }
      tx
    })
    
    
    viz_opts <- reactive({
      if (is.null(r$active_viz)) return()
      req(r$varNumId)
      agg_tog <- "sum"
      format_sample_num <- "1,234."
      if (r$varNumId != "Total") {
      format_sample_num <- "1,234.56"
      agg_tog <- "mean"
      }
    
      opts_viz <- list(
        data = r$d_fil,
        palette_colors = c("#22776A", "#43A292", "#0B5D78", "#2A819C", "#84CDE4", "#A7A6A6", "#575756"),
        na_color = "#dddddd",
        hor_title = " ",
        ver_title = " ",
        orientation = "hor",
        drop_na = TRUE,
        title = title_viz(),
        #caption = caption_viz(),
        #drop_na_
        agg = agg_tog,
        background_color = "#fafafa",
        format_sample_num = format_sample_num,
        title_size = 15,
        title_align = "center",
        title_color = "#0F0F0F",
        text_color = "#0F0F0F", 
        #tooltip = tooltip_viz(),
        text_family = "Roboto",#"Fira Sans",
        title_family = "Roboto",#"Fira Sans",
        label_wrap = 30,
        label_wrap_legend = 100,
        marker_radius = 7,
        dataLabels_show = TRUE,
        #sort = "desc", ##dbd9d9 grid color
        grid_x_width = 0,
        map_min_zoom = 5
      )
      
      if (r$active_viz %in% c("treemap", "pie")) {
        opts_viz$legend_show <- FALSE
        opts_viz$color_by <- names(r$d_fil)[1]
        #opts_viz$palette_type <- "sequential"
        #opts_viz$palette_colors <- c("#22776A", "#43A292", "#0B5D78","#84CDE4", "#7C7B7B", "#A7A6A6", "#D9D9D9")
      }
      
      # if (r$active_viz == "bar") {
      #   if (ncol(r$d_fil) == 3) {
      #     if (r$stackedId) {
      #       opts_viz$graph_type <- "stacked"
      #     }
      #   }
      # }
      # 
      if (r$active_viz == "map") {
        opts_viz$map_name <- "col_departments"
        #opts_viz$map_tiles <- "OpenStreetMap.Mapnik"
        opts_viz$palette_colors <- c( "#EBF6FB", "#B4DEE0", "#6ABFA2", "#2CA361", "#0B7032")
      }
      
      opts_viz
    })
    
    r_viz <- reactive({
      #print("Esto es un ej")
      #print(r$v_type)
      tryCatch({
        if (is.null(r$active_viz)) return()
        if (is.null(r$v_type)) return()
        library(hgchmagic)
        do.call(eval(parse(text=r$v_type)),
                viz_opts()
        )
      },
      error = function(cond) {
        return()
      })
    })
    
    output$viz_hgch <- highcharter::renderHighchart({
      if (r$active_viz == "map") return()
      if (r$active_viz == "table") return()
      r_viz()
    })
    
    output$viz_lflt <- leaflet::renderLeaflet({
      if (r$active_viz != "map") return()
      r_viz()
    })
    
    
    output$table_dt <- DT::renderDataTable({
      if (r$active_viz != "table") return()
      req(r$d_fil)
      df <- r$d_fil
      dtable <- DT::datatable(df,
                              rownames = F,
                              selection = 'none',
                              options = list(
                                #autoWidth = TRUE,
                                language = list(url = '//cdn.datatables.net/plug-ins/1.10.11/i18n/Spanish.json'),
                                #                        lengthChange = F,
                                #                        pageLength = 4,
                                scrollX = T,
                                fixedColumns = TRUE,
                                fixedHeader = TRUE,
                                scrollY = "500px"#,
                                #                        initComplete = htmlwidgets::JS(
                                #                          "function(settings, json) {",
                                #                          "$(this.api().table().header()).css({'background-color': '#a13e1f', 'color': '#fff'});",
                                #                          "}")
                                #                      )) %>%
                                # DT::formatStyle( 0 , target= 'row',color = '#0A446B', fontSize ='13px', lineHeight='15px')
                                
                              ))
      dtable
    })
    
    output$viz_print <- renderUI({
      if (is.null(r$active_viz)) return("No hay datos disponibles")
      if (r$active_viz == "table") {
        DT::dataTableOutput(ns("table_dt"), width = 920)
      } else if (r$active_viz == "map") {
        leaflet::leafletOutput(ns("viz_lflt"), height = 600)
      } else {
        highcharter::highchartOutput(ns("viz_hgch"), height = 600)
      }
      
    })
    
    
    observe({
      r$downViz <- r_viz()
      r$textCap <- caption_viz()
    })
    
    
    
     
  })
}

## To be copied in the UI
# mod_load_viz_ui("load_viz_ui_1")

## To be copied in the server
# mod_load_viz_server("load_viz_ui_1")
