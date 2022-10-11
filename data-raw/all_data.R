library(googlesheets4)
library(tidyverse)

gs4_deauth()
indiceDane <- read_sheet("https://docs.google.com/spreadsheets/d/1S-4cYxqXxcU3vPDzGHTkfueBQUQEXlYqHL7gp9Pyl9Y/edit?usp=sharing", sheet = "indice")
indiceDane <- indiceDane %>% group_by(`Nombre hoja`)%>% mutate(idIndicador = cur_group_id())
indiceDane$idIndicador <- paste0(indiceDane$id, indiceDane$idIndicador)
indiceDane <- indiceDane %>% rename("num_dic" = `Número de decimales en el índice`)
indiceDane <- indiceDane %>% drop_na(variables)
indiceDane$variables_cantidad <- gsub("\\(|\\)", "", indiceDane$variables_cantidad)

label_df <- function(valor, num_dec, df) {
  if (!identical(grep(valor, names(df)), integer())) {
    df[[grep(valor, names(df))]] <- as.numeric(df[[grep(valor, names(df))]])
    df$formatNum <- paste0(format(df[[grep(valor, names(df))]], nsmall = num_dec,  decimal.mark = ",", big.mark  = "."), " ",df[[grep("unidad", tolower(names(df)))]])
    varTool <- names(df)[(-grep(paste0(c("Unidad", valor), collapse = "|"), names(df)))]
    df$label <- purrr::map(1:nrow(df), function(i) {
      purrr::map(varTool, function(j){
        paste0(gsub("formatNum", "Total ", j), ": ", df[[j]][i])
      }) %>% paste0(collapse = "<br/>")
    }) %>% unlist()
  }
  df
}


demanda <- indiceDane %>% filter(id %in% "demanda")

ls <- purrr::map(unique(demanda$idIndicador), function (i) {
  fd <- demanda %>% filter(idIndicador %in% i) %>% distinct(indicador, .keep_all = T)
  valor <- unique(fd$variables_cantidad)
  num_dec <- unique(fd$num_dic)[1]
  if (is.null(num_dec)) num_dec <- 1
  print(num_dec)
  print(fd$`Nombre hoja`)
  df <- read_sheet("https://docs.google.com/spreadsheets/d/1S-4cYxqXxcU3vPDzGHTkfueBQUQEXlYqHL7gp9Pyl9Y/edit?usp=sharing", sheet = fd$`Nombre hoja`, col_types = "c")
  names(df) <- gsub("\\(|\\)", "", names(df))
  
  df <- label_df(valor, num_dec, df)
  print(df)
  df
})
names(ls) <- unique(demanda$idIndicador)
dataDemanda <- ls

# Conservacion
conservacion <- indiceDane %>% filter(id %in% "conservacion")
ls <- purrr::map(unique(conservacion$idIndicador), function (i) {
 #i <- "conservacion10"
  fd <- conservacion %>% filter(idIndicador %in% i) %>% distinct(indicador, .keep_all = T)
  df <- read_sheet("https://docs.google.com/spreadsheets/d/1S-4cYxqXxcU3vPDzGHTkfueBQUQEXlYqHL7gp9Pyl9Y/edit?usp=sharing", sheet = fd$`Nombre hoja`, col_types = "c", na = c("NA", "", "-"))
  names(df) <- gsub("\\(|\\)", "", names(df))
  valor <- unique(fd$variables_cantidad)
  num_dec <- unique(fd$num_dic)[1]
  if (is.null(num_dec)) num_dec <- 1
  print(grep(valor, names(df)))
  df <- label_df(valor, num_dec, df)
  print(df)
  df
})
names(ls) <- unique(conservacion$idIndicador)
dataConservacion <- ls


# Presion
presion <- indiceDane %>% filter(id %in% "presion")
ls <- purrr::map(unique(presion$idIndicador), function (i) {

  fd <- presion %>% filter(idIndicador %in% i) %>% distinct(indicador, .keep_all = T)
  print(fd$`Nombre hoja`)
  df <- read_sheet("https://docs.google.com/spreadsheets/d/1S-4cYxqXxcU3vPDzGHTkfueBQUQEXlYqHL7gp9Pyl9Y/edit?usp=sharing", sheet = fd$`Nombre hoja`, col_types = "c", na = c("NA", "", "-"))
  names(df) <- gsub("\\(|\\)", "", names(df))
  valor <- unique(fd$variables_cantidad)
  num_dec <- unique(fd$num_dic)[1]
  if (is.null(num_dec)) num_dec <- 1
  print(grep(valor, names(df)))
  df <- label_df(valor, num_dec, df)
  print(df)
  df
})
names(ls) <- unique(presion$idIndicador)
dataPresion <- ls

#usethis::use_data(dataPresion, overwrite = TRUE)

# Factores
factores <- indiceDane %>% filter(id %in% "factores")
#Participación porcentual del gasto de la industria manufacturera según actividad de gestión de recursos
ls <- purrr::map(unique(factores$idIndicador), function (i) {
  fd <- factores %>% filter(idIndicador %in% i) %>% distinct(indicador, .keep_all = T)
  df <- read_sheet("https://docs.google.com/spreadsheets/d/1S-4cYxqXxcU3vPDzGHTkfueBQUQEXlYqHL7gp9Pyl9Y/edit?usp=sharing", sheet = fd$`Nombre hoja`, col_types = "c", na = c("NA", "", "-"))
  names(df) <- gsub("\\(|\\)", "", names(df))
  valor <- unique(fd$variables_cantidad)
  num_dec <- unique(fd$num_dic)[1]
  if (is.null(num_dec)) num_dec <- 1
  print(grep(valor, names(df)))
  df <- label_df(valor, num_dec, df)
  print(df)
  df
})
names(ls) <- unique(factores$idIndicador)
dataFactores <- ls

dataDane <- list(
  dic = indiceDane,
  demanda = dataDemanda,
  conservacion = dataConservacion,
  presion = dataPresion, 
  factores = dataFactores
)



saveRDS(dataDane, "data/dataDane.rds")

