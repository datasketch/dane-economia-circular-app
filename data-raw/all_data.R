library(googlesheets4)
library(tidyverse)

gs4_deauth()
indiceDane <- read_sheet("https://docs.google.com/spreadsheets/d/1S-4cYxqXxcU3vPDzGHTkfueBQUQEXlYqHL7gp9Pyl9Y/edit?usp=sharing", sheet = "indice")
indiceDane <- indiceDane %>% group_by(`Nombre hoja`)%>% mutate(idIndicador = cur_group_id())
indiceDane$idIndicador <- paste0(indiceDane$id, indiceDane$idIndicador)
usethis::use_data(indiceDane, overwrite = TRUE)

demanda <- indiceDane %>% filter(id %in% "demanda")

ls <- purrr::map(unique(demanda$idIndicador), function (i) {

  fd <- demanda %>% filter(idIndicador %in% i) %>% distinct(indicador, .keep_all = T)
  df <- read_sheet("https://docs.google.com/spreadsheets/d/1S-4cYxqXxcU3vPDzGHTkfueBQUQEXlYqHL7gp9Pyl9Y/edit?usp=sharing", sheet = fd$`Nombre hoja`, col_types = "c")
  print(grep("Valor", names(df)))
  if (!identical(grep("Valor", names(df)), integer())) {
    df[[grep("Valor", names(df))]] <- as.numeric(df[[grep("Valor", names(df))]])
    df$formatNum <- paste0(format(round(df[[grep("Valor", names(df))]], 2),  decimal.mark = ",", big.mark  = "."), " ",df$`Unidad de medida`)
    varTool <- names(df)[(-grep("Unidad|Valor|Variable", names(df)))]
    df$label <- purrr::map(1:nrow(df), function(i) {
      purrr::map(varTool, function(j){
        paste0(gsub("formatNum", "Total ", j), ": ", df[[j]][i])
      }) %>% paste0(collapse = "<br/>")
    }) %>% unlist()
  }
  print(df)
  df
})
names(ls) <- unique(demanda$idIndicador)
dataDemanda <- ls
usethis::use_data(dataDemanda, overwrite = TRUE)

# Conservacion
conservacion <- indiceDane %>% filter(id %in% "conservacion")
ls <- purrr::map(unique(conservacion$idIndicador), function (i) {

  fd <- conservacion %>% filter(idIndicador %in% i) %>% distinct(indicador, .keep_all = T)
  df <- read_sheet("https://docs.google.com/spreadsheets/d/1S-4cYxqXxcU3vPDzGHTkfueBQUQEXlYqHL7gp9Pyl9Y/edit?usp=sharing", sheet = fd$`Nombre hoja`, col_types = "c", na = c("NA", "", "-"))
  print(grep("Valor", names(df)))
  if (!identical(grep("Valor", names(df)), integer())) {
    df[[grep("Valor", names(df))]] <- as.numeric(df[[grep("Valor", names(df))]])
    df$formatNum <- paste0(format(round(df[[grep("Valor", names(df))]], 2),  decimal.mark = ",", big.mark  = "."), " ",df$`Unidad de medida`)
    varTool <- names(df)[(-grep("Unidad|Valor|Variable", names(df)))]
    df$label <- purrr::map(1:nrow(df), function(i) {
      purrr::map(varTool, function(j){
        paste0(gsub("formatNum", "Total ", j), ": ", df[[j]][i])
      }) %>% paste0(collapse = "<br/>")
    }) %>% unlist()
  }
  print(df)
  df
})
names(ls) <- unique(conservacion$idIndicador)
dataConservacion <- ls

usethis::use_data(dataConservacion, overwrite = TRUE)


# Presion
presion <- indiceDane %>% filter(id %in% "presion")
ls <- purrr::map(unique(presion$idIndicador), function (i) {

  fd <- presion %>% filter(idIndicador %in% i) %>% distinct(indicador, .keep_all = T)
  print(fd$`Nombre hoja`)
  df <- read_sheet("https://docs.google.com/spreadsheets/d/1S-4cYxqXxcU3vPDzGHTkfueBQUQEXlYqHL7gp9Pyl9Y/edit?usp=sharing", sheet = fd$`Nombre hoja`, col_types = "c", na = c("NA", "", "-"))
  print(grep("Valor", names(df)))
  if (!identical(grep("Valor", names(df)), integer())) {
    df[[grep("Valor", names(df))]] <- as.numeric(df[[grep("Valor", names(df))]])
    df$formatNum <- paste0(format(round(df[[grep("Valor", names(df))]], 2),  decimal.mark = ",", big.mark  = "."), " ",df$`Unidad de medida`)
    varTool <- names(df)[(-grep("Unidad|Valor|Variable", names(df)))]
    df$label <- purrr::map(1:nrow(df), function(i) {
      purrr::map(varTool, function(j){
        paste0(gsub("formatNum", "Total ", j), ": ", df[[j]][i])
      }) %>% paste0(collapse = "<br/>")
    }) %>% unlist()
  }
  print(df)
  df
})
names(ls) <- unique(presion$idIndicador)
dataPresion <- ls

usethis::use_data(dataPresion, overwrite = TRUE)

# Factores
factores <- indiceDane %>% filter(id %in% "factores")
#Participación porcentual del gasto de la industria manufacturera según actividad de gestión de recursos
ls <- purrr::map(unique(factores$idIndicador), function (i) {
  fd <- factores %>% filter(idIndicador %in% i) %>% distinct(indicador, .keep_all = T)
  df <- read_sheet("https://docs.google.com/spreadsheets/d/1S-4cYxqXxcU3vPDzGHTkfueBQUQEXlYqHL7gp9Pyl9Y/edit?usp=sharing", sheet = fd$`Nombre hoja`, col_types = "c", na = c("NA", "", "-"))
  print(grep("Valor", names(df)))
  if (!identical(grep("Valor", names(df)), integer())) {
    df[[grep("Valor", names(df))]] <- as.numeric(df[[grep("Valor", names(df))]])
    df$formatNum <- paste0(format(round(df[[grep("Valor", names(df))]], 2),  decimal.mark = ",", big.mark  = "."), " ",df$`Unidad de medida`)
    varTool <- names(df)[(-grep("Unidad|Valor|Variable", names(df)))]
    df$label <- purrr::map(1:nrow(df), function(i) {
      purrr::map(varTool, function(j){
        paste0(gsub("formatNum", "Total ", j), ": ", df[[j]][i])
      }) %>% paste0(collapse = "<br/>")
    }) %>% unlist()
  }
  print(df)
  df
})
names(ls) <- unique(factores$idIndicador)
dataFactores <- ls

usethis::use_data(dataFactores, overwrite = TRUE)

