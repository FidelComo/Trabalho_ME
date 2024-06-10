library(tidyverse)
library(emmeans)
library(dplyr)

jul_nov <- read_delim("data/Dados_monitorar_jul_nov_22-11-2022 DQAA.csv", 
delim = ";", escape_double = FALSE, col_types = cols(
Data = col_date(format = "%d/%m/%Y"),
Concentracao = col_number(), 
iqar = col_number()), locale = locale(decimal_mark = ",", 
grouping_mark = ".", encoding = "ISO-8859-1"), 
trim_ws = TRUE)

jan_mar <- read_delim("data/Dados_monitorar_jan_mar_22-11-2022 DQAA.csv", 
delim = ";", escape_double = FALSE, col_types = cols(
Data = col_date(format = "%d/%m/%Y"),
Concentracao = col_number(), 
iqar = col_number()), locale = locale(decimal_mark = ",", 
grouping_mark = ".", encoding = "ISO-8859-1"), 
trim_ws = TRUE)

abr_jun <- read_delim("data/Dados_monitorar_abr_jun_22-11-2022 DQAA.csv", 
delim = ";", escape_double = FALSE, col_types = cols(
Data = col_date(format = "%d/%m/%Y"),
Concentracao = col_number(), 
iqar = col_number()), locale = locale(decimal_mark = ",", 
grouping_mark = ".", encoding = "ISO-8859-1"), 
trim_ws = TRUE)

dfs <- list(jul_nov,jan_mar,abr_jun)

sigla <- unique(c(
  as.vector(jul_nov$Sigla),
  as.vector(jan_mar$Sigla),
  as.vector(abr_jun$Sigla)))

columns <- c("Nome_do_Municipio","Data", sigla)

QualidadeAr <- data.frame(matrix(nrow = 0, ncol = length(columns)))
colnames(QualidadeAr) = columns

for (df in dfs){
  municipios <- unique(as.vector(df$Nome_do_Município))
  data <- unique(df$Data)
  for (i in municipios){
    filtro <- df[df$Nome_do_Município == i, ]
    for (j in data){
      filtro2 <- filtro[filtro$Data == j, ]
      row <- c()
      for (k in sigla){
        row <- append(row, mean(na.omit(as.vector(filtro2[filtro2$Sigla == k, ]$Concentracao))))
      }
      if (!(prod(is.na(row)))){
        QualidadeAr <- rbind(QualidadeAr, c(i,as.character(as.Date(j)),row))
      }
    }
  }
}

colnames(QualidadeAr) = columns
QualidadeAr[, 3:26] <- lapply(QualidadeAr[, 3:26],as.numeric)