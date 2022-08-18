library(googlesheets4)
library(tidyverse)

indiceDane <- read_sheet("https://docs.google.com/spreadsheets/d/1mMJAoyptnpatVmdJW--yJG0Axly0U59B5ttow_rVZAg/edit#gid=0")
indiceDane$idIndicador <- indiceDane %>% group_by(`Nombre hoja`)%>% group_indices(`Nombre hoja`)
indiceDane$idIndicador <- paste0(indiceDane$id, indiceDane$idIndicador)
indiceDane <- indiceDane %>% rename(c("indicador_general" = "Filtro 1",
                                      "indicador" = "Indicadores de interés", 
                                      "variables" = "Variables a visualizar",
                                      "variables_cantidad" = "Variable cantidad"))

usethis::use_data(indiceDane, overwrite = TRUE)

#"demanda"      "conservacion" "presion"      "factores" 

# Demanda
demanda <- indiceDane %>% filter(id %in% "demanda")
ls <- purrr::map(unique(demanda$idIndicador), function (i) {
  #i <- "demanda14"
  fd <- demanda %>% filter(idIndicador %in% i) %>% distinct(indicador, .keep_all = T)
  df <- read_sheet("https://docs.google.com/spreadsheets/d/1s3RPwgNdPrTVq0kARqZwTSoKzk-8jGC5Ed2_ERXJoIg/edit#gid=1909007839", sheet = fd$`Nombre hoja`, col_types = "c")
  names(df) <- trimws(stringr::str_replace_all(names(df), "[\r\n]" , " "))
  
  indTotal <- grep("Total|total", df)
  if (!identical(indTotal, integer())) {
    df <- df[-grep("Total|total", df[[indTotal[1]]]),]
  }
  cn <- purrr::map(unique(fd$variables_cantidad), function (n) {
    df[[n]] <<- as.numeric(gsub("\\,", ".",  df[[n]]))
  })
  df
})
names(ls) <- unique(demanda$idIndicador)
dataDemanda <- ls
usethis::use_data(dataDemanda, overwrite = TRUE)

# Conservacion
conservacion <- indiceDane %>% filter(id %in% "conservacion")
ls <- purrr::map(unique(conservacion$idIndicador), function (i) {

  fd <- conservacion %>% filter(idIndicador %in% i) %>% distinct(indicador, .keep_all = T)
  df <- read_sheet("https://docs.google.com/spreadsheets/d/1s3RPwgNdPrTVq0kARqZwTSoKzk-8jGC5Ed2_ERXJoIg/edit#gid=1909007839", sheet = fd$`Nombre hoja`, col_types = "c", na = c("NA", "", "-"))
  names(df) <- trimws(stringr::str_replace_all(names(df), "[\r\n]" , " "))
  

  indTotal <- grep("Total|total", df)
  if (!identical(indTotal, integer())) {
    df <- df[-grep("Total|total", df[[indTotal[1]]]),]
  }
  cn <- purrr::map(unique(fd$variables_cantidad), function (n) {
    df[[n]] <<- as.numeric(gsub(",", ".",  df[[n]]))
  })
  df
})
names(ls) <- unique(conservacion$idIndicador)
fff <- ls$conservacion37
dataConservacion <- ls

usethis::use_data(dataConservacion, overwrite = TRUE)


# Presion
presion <- indiceDane %>% filter(id %in% "presion")
ls <- purrr::map(unique(presion$idIndicador), function (i) {

  fd <- presion %>% filter(idIndicador %in% i) %>% distinct(indicador, .keep_all = T)
  df <- read_sheet("https://docs.google.com/spreadsheets/d/1s3RPwgNdPrTVq0kARqZwTSoKzk-8jGC5Ed2_ERXJoIg/edit#gid=1909007839", sheet = fd$`Nombre hoja`, col_types = "c", na = c("NA", "", "-"))
  names(df) <- trimws(stringr::str_replace_all(names(df), "[\r\n]" , " "))
  
  
  indTotal <- grep("Total|total", df)
  if (!identical(indTotal, integer())) {
    df <- df[-grep("Total|total", df[[indTotal[1]]]),]
  }
  cn <- purrr::map(unique(fd$variables_cantidad), function (n) {
    df[[n]] <<- as.numeric(gsub(",", ".",  df[[n]]))
  })
  df
})
names(ls) <- unique(presion$idIndicador)
fff <- ls$presion29
dataPresion <- ls

usethis::use_data(dataPresion, overwrite = TRUE)

# Factores
factores <- indiceDane %>% filter(id %in% "factores")
ls <- purrr::map(unique(factores$idIndicador), function (i) {
  
  fd <- factores %>% filter(idIndicador %in% i) %>% distinct(indicador, .keep_all = T)
  df <- read_sheet("https://docs.google.com/spreadsheets/d/1s3RPwgNdPrTVq0kARqZwTSoKzk-8jGC5Ed2_ERXJoIg/edit#gid=1909007839", sheet = fd$`Nombre hoja`, col_types = "c", na = c("NA", "", "-"))
  names(df) <- trimws(stringr::str_replace_all(names(df), "[\r\n]" , " "))
  
  
  indTotal <- grep("Total|total", df)
  if (!identical(indTotal, integer())) {
    df <- df[-grep("Total|total", df[[indTotal[1]]]),]
  }
  cn <- purrr::map(unique(fd$variables_cantidad), function (n) {
    df[[n]] <<- as.numeric(gsub(",", ".",  df[[n]]))
  })
  df
})
names(ls) <- unique(factores$idIndicador)
dataFactores <- ls

usethis::use_data(dataFactores, overwrite = TRUE)


# indiceDane <- read_sheet("https://docs.google.com/spreadsheets/d/13vYDDM1orPdl_t2pMlRsyDrycha0bxas1J_7_PtEL6Q/edit#gid=45273471", 3)
# indiceDane <- indiceDane %>% tidyr::fill(id, idIndicador, indicador)
# 
# 
# # Data var consumo
# reducir_energia <- read_sheet("https://docs.google.com/spreadsheets/d/1s3RPwgNdPrTVq0kARqZwTSoKzk-8jGC5Ed2_ERXJoIg/edit#gid=1782642204", "Cuadro 15")
# names(reducir_energia) <- as.character(reducir_energia[1,] %>% unlist())
# names(reducir_energia)[5] <- "Estimador Total"
# reducir_energia <- reducir_energia[-1,]
# reducir_energia$`Estimador Total` <- as.numeric(reducir_energia$`Estimador Total`)
# reducir_energia$Porcentaje <- as.numeric(reducir_energia$Porcentaje)
# reducir_energia$Año <- as.character(reducir_energia$Año)
# reducir_energia <- reducir_energia[!grepl("Total", reducir_energia$Área),]
# reducir_energia <- reducir_energia[!grepl("Total", reducir_energia$Departamento),]
# reducir_energia <- reducir_energia[!grepl("Total", reducir_energia$Variable),]
# 
# alimentos_elct <- read_sheet("https://docs.google.com/spreadsheets/d/1s3RPwgNdPrTVq0kARqZwTSoKzk-8jGC5Ed2_ERXJoIg/edit#gid=1782642204", "Cuadro 17")
# names(alimentos_elct ) <- gsub("\n", " ",as.character(alimentos_elct [1,] %>% unlist()))
# alimentos_elct <- alimentos_elct[-1,]
# alimentos_elct$Año <- as.character(alimentos_elct$Año)
# alimentos_elct$`Estimador Total` <- as.numeric(alimentos_elct$`Estimador Total`)
# alimentos_elct$Porcentaje <- as.numeric(alimentos_elct$Porcentaje)
# alimentos_elct <- alimentos_elct[!grepl("Total", alimentos_elct$Área),]
# alimentos_elct <- alimentos_elct[!grepl("Total", alimentos_elct$Departamento),]
# alimentos_elct <- alimentos_elct[!grepl("Total", alimentos_elct$Variable),]
# 
# 
# 
# dataConsumo <- list("consumo15" = reducir_energia,
#                     "consumo26" = alimentos_elct)
# 
# usethis::use_data(dataConsumo, overwrite = TRUE)
# 
# 
# # Data var cierre
# 
# edificaciones_ahorro <- read_sheet("https://docs.google.com/spreadsheets/d/1s3RPwgNdPrTVq0kARqZwTSoKzk-8jGC5Ed2_ERXJoIg/edit#gid=1042378887", "PESAExDEPTO")
# edificaciones_ahorro <- edificaciones_ahorro[!grepl("Total", edificaciones_ahorro$Departamento),]
# edificaciones_ahorro <- edificaciones_ahorro[!grepl("Total", edificaciones_ahorro$Variable),]
# 
# 
# 
# edi_energ_alt <- read_sheet("https://docs.google.com/spreadsheets/d/1s3RPwgNdPrTVq0kARqZwTSoKzk-8jGC5Ed2_ERXJoIg/edit#gid=1863782856", "PESEA")
# edi_energ_alt$Valor[edi_energ_alt$Valor %in% c("-","NULL")] <- NA
# edi_energ_alt$Porcentaje[edi_energ_alt$Porcentaje %in% c("-","NULL")] <- NA
# edi_energ_alt <- edi_energ_alt %>% tidyr::unnest(Valor)
# edi_energ_alt <- edi_energ_alt %>% tidyr::unnest(Porcentaje)
# 
# 
# 
# edif_ahor_ener <- read_sheet("https://docs.google.com/spreadsheets/d/1s3RPwgNdPrTVq0kARqZwTSoKzk-8jGC5Ed2_ERXJoIg/edit#gid=1609088838", "PESAE")
# names(edif_ahor_ener) <- gsub("\n", " ",as.character(edif_ahor_ener[3,] %>% unlist()))
# edif_ahor_ener <- edif_ahor_ener[-(1:3),] 
# edif_ahor_ener$Valor[edif_ahor_ener$Valor %in% c("-","NULL")] <- NA
# edif_ahor_ener$Porcentaje[edif_ahor_ener$Porcentaje %in% c("-","NULL")] <- NA
# edif_ahor_ener <- edif_ahor_ener %>% tidyr::unnest(Valor)
# edif_ahor_ener <- edif_ahor_ener %>% tidyr::unnest(Porcentaje)
# 
# 
# edif_ahor_agua <- read_sheet("https://docs.google.com/spreadsheets/d/1s3RPwgNdPrTVq0kARqZwTSoKzk-8jGC5Ed2_ERXJoIg/edit#gid=1609088838", "PEAA")
# names(edif_ahor_agua) <- gsub("\n", " ",as.character(edif_ahor_agua[3,] %>% unlist()))
# edif_ahor_agua <- edif_ahor_agua[-(1:3),] 
# edif_ahor_agua$Valor[edif_ahor_agua$Valor %in% c("-","NULL")] <- NA
# edif_ahor_agua$Porcentaje[edif_ahor_agua$Porcentaje %in% c("-","NULL")] <- NA
# edif_ahor_agua <- edif_ahor_agua %>% tidyr::unnest(Valor)
# edif_ahor_agua <- edif_ahor_agua %>% tidyr::unnest(Porcentaje)
# 
# hogares_prac_rec <- read_sheet("https://docs.google.com/spreadsheets/d/1s3RPwgNdPrTVq0kARqZwTSoKzk-8jGC5Ed2_ERXJoIg", "Cuadro 16")
# names(hogares_prac_rec) <- gsub("\n", " ",as.character(hogares_prac_rec[1,] %>% unlist()))
# hogares_prac_rec <- hogares_prac_rec[-1,] 
# hogares_prac_rec$`Estimador Total`[hogares_prac_rec$`Estimador Total` %in% c("-","NULL")] <- NA
# hogares_prac_rec$Porcentaje[hogares_prac_rec$Porcentaje %in% c("-","NULL")] <- NA
# hogares_prac_rec <- hogares_prac_rec %>% tidyr::unnest(Año)
# hogares_prac_rec <- hogares_prac_rec %>% tidyr::unnest(`Estimador Total`)
# hogares_prac_rec <- hogares_prac_rec %>% tidyr::unnest(Porcentaje)
# hogares_prac_rec <- hogares_prac_rec %>% dplyr::filter(Variable != "Total hogares")
# 
# hogares_elim <- read_sheet("https://docs.google.com/spreadsheets/d/1s3RPwgNdPrTVq0kARqZwTSoKzk-8jGC5Ed2_ERXJoIg", "Cuadro 24")
# names(hogares_elim) <- gsub("\n", " ",as.character(names(hogares_elim) %>% unlist()))
# 
# hogares_clas <- read_sheet("https://docs.google.com/spreadsheets/d/1s3RPwgNdPrTVq0kARqZwTSoKzk-8jGC5Ed2_ERXJoIg", "Cuadro 25")
# names(hogares_clas) <- gsub("\n", " ",as.character(hogares_clas[1,] %>% unlist()))
# hogares_clas <- hogares_clas[-1,]
# hogares_clas$Año <- as.character(hogares_clas$Año %>% unlist())
# hogares_clas$`Estimador Total` <- as.numeric(hogares_clas$`Estimador Total` %>% unlist())
# hogares_clas$Porcentaje <- as.numeric(hogares_clas$Porcentaje %>% unlist())
# 
# dataCierre <- list(
#   "cierre18" = edificaciones_ahorro,
#   "cierre19" = edi_energ_alt,
#   "cierre20" = edif_ahor_ener,
#   "cierre21" = edif_ahor_agua,
#   "cierre53" = hogares_prac_rec,
#   "cierre60" = hogares_elim,
#   "cierre61" = hogares_clas
# )
# 
# usethis::use_data(dataCierre, overwrite = TRUE)
# 
