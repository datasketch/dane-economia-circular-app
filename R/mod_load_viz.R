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
    
    
    
    viz_opts <- reactive({
      if (is.null(r$active_viz)) return()
      print("inputt")
     print(r[["departamento"]])
     print(r[["area"]])
      req(r$varNumId)
      agg_tog <- "sum"
      format_sample_num <- "1,234."
      if (r$varNumId != "Total") {
      format_sample_num <- "1,234.56"
      agg_tog <- "mean"
      }
      
      opts_viz <- list(
        data = r$d_fil,
        palette_colors = c("#267284", "#22776A", "#43A292", "#0B5D78","#84CDE4", "#7C7B7B", "#A7A6A6", "#D9D9D9"),
        na_color = "#c8c8c8",
        hor_title = " ",
        ver_title = " ",
        orientation = "hor",
        drop_na = TRUE,
        #drop_na_
        agg = agg_tog,
        background_color = "transparent",
        format_sample_num = format_sample_num,
        title_size = 15,
        title_align = "center",
        title_color = "#212121",
        text_color = "#333333",
        #tooltip = tooltip_viz(),
        text_family = "Fira Sans",
        title_family = "Fira Sans",
        label_wrap = 30,
        label_wrap_legend = 100,
        marker_radius = 7,
        dataLabels_show = TRUE,
        #sort = "desc", ##dbd9d9 grid color
        grid_x_width = 0,
        map_zoom = 5,
        map_min_zoom = 5
      )
      
      if (r$active_viz %in% c("treemap", "pie")) {
        opts_viz$legend_show <- FALSE
        opts_viz$color_by <- names(r$d_fil)[1]
        #opts_viz$palette_type <- "sequential"
        opts_viz$palette_colors <- c("#22776A", "#43A292", "#0B5D78","#84CDE4", "#7C7B7B", "#A7A6A6", "#D9D9D9")
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
        opts_viz$palette_colors <- c("#0B7032", "#B4DEE0", "#6ABFA2", "#B4DEE0", "#0B7032")
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
        leaflet::leafletOutput(ns("viz_lflt"), height = 680)
      } else {
        highcharter::highchartOutput(ns("viz_hgch"), height = 680)
      }
      
    })
    
    
    observe({
      r$downViz <- r_viz()
    })
    
    
    
     
  })
}

## To be copied in the UI
# mod_load_viz_ui("load_viz_ui_1")

## To be copied in the server
# mod_load_viz_server("load_viz_ui_1")
