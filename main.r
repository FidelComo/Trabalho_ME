library(tidyverse)
library(emmeans)
library(dplyr)

censo <- read_delim("data/censo.csv", 
delim = ";", escape_double = FALSE, trim_ws = TRUE,col_types = cols(`Código IBGE` = "c"))

INFLUD <- read_delim("data/INFLUD.csv", 
delim = ";", escape_double = FALSE, col_types = cols(
DT_VAC_MAE = col_date(format = "%d/%m/%Y"),
OUT_SOR = col_character(),
DT_NOTIFIC = col_date(format = "%d/%m/%Y"), 
DT_SIN_PRI = col_date(format = "%d/%m/%Y"), 
DT_NASC    = col_date(format = "%d/%m/%Y"), 
DT_UT_DOSE = col_date(format = "%d/%m/%Y"), 
DT_VAC_MAE = col_date(format = "%d/%m/%Y"), 
DT_DOSEUNI = col_date(format = "%d/%m/%Y"), 
DT_1_DOSE  = col_date(format = "%d/%m/%Y"), 
DT_2_DOSE  = col_date(format = "%d/%m/%Y"), 
DT_ANTIVIR = col_date(format = "%d/%m/%Y"), 
DT_INTERNA = col_date(format = "%d/%m/%Y"), 
DT_ENTUTI  = col_date(format = "%d/%m/%Y"), 
DT_SAIDUTI = col_date(format = "%d/%m/%Y"), 
DT_RAIOX   = col_date(format = "%d/%m/%Y"), 
DT_COLETA  = col_date(format = "%d/%m/%Y"), 
DT_PCR     = col_date(format = "%d/%m/%Y"), 
DT_TOMO    = col_date(format = "%d/%m/%Y"), 
DT_RES_AN  = col_date(format = "%d/%m/%Y"), 
DT_CO_SOR  = col_date(format = "%d/%m/%Y"), 
DT_RES     = col_date(format = "%d/%m/%Y"), 
DT_TRT_COV = col_date(format = "%d/%m/%Y"),), 
trim_ws = TRUE)

jul_nov <- read_delim("data/jul_nov.csv", 
delim = ";", escape_double = FALSE, col_types = cols(
Data = "c",
Concentracao = col_number(), 
iqar = col_number()), locale = locale(decimal_mark = ",", 
grouping_mark = "."), 
trim_ws = TRUE)

jan_mar <- read_delim("data/jan_mar.csv", 
delim = ";", escape_double = FALSE, col_types = cols(
Data = "c",
Concentracao = col_number(), 
iqar = col_number()), locale = locale(decimal_mark = ",", 
grouping_mark = "."), 
trim_ws = TRUE)

abr_jun <- read_delim("data/abr_jun.csv", 
delim = ";", escape_double = FALSE, col_types = cols(
Data = "c",
Concentracao = col_number(), 
iqar = col_number()), locale = locale(decimal_mark = ",", 
grouping_mark = "."), 
trim_ws = TRUE)



dfs <- list(jul_nov,jan_mar,abr_jun)

sigla <- unique(c(
  as.vector(jul_nov$Sigla),
  as.vector(jan_mar$Sigla),
  as.vector(abr_jun$Sigla)))

columns <- c("Código_do_Município","Nome_do_Município","Data", sigla)

QualidadeAr <- data.frame(matrix(nrow = 0, ncol = length(columns)))
colnames(QualidadeAr) = columns

for (df in dfs){
  df <- df[df$Situação == "VA",]
  municipios <- unique(as.vector(df$Código_do_Município))
  data <- unique(df$Data)
  for (i in municipios){
    filtro <- df[df$Código_do_Município == i, ]
    nome <- filtro$Nome_do_Município[1]
    for (j in data){
      filtro2 <- filtro[filtro$Data == j, ]
      row <- c()
      for (k in sigla){
        row <- append(row, mean(na.omit(as.vector(filtro2[filtro2$Sigla == k, ]$Concentracao))))
      }
      if (!(prod(is.na(row)))){
        QualidadeAr <- rbind(QualidadeAr, c(i,nome,as.character(as.Date(j)),row))
      }
    }
  }
}

colnames(QualidadeAr) = columns
QualidadeAr[, 4:26] <- lapply(QualidadeAr[, 4:26],as.numeric)

N_NOTIF <- c()
N_SIN_PRI <- c()
for (i in 1:nrow(QualidadeAr)){
  municipio <- QualidadeAr[i,1]
  abitantes <- censo[censo$`Código IBGE` == municipio, ]$População
  data <- QualidadeAr[i,3]
  filtro <- INFLUD[INFLUD$CO_MUN_NOT == municipio, ]
  N_SIN_PRI <- append(N_SIN_PRI, sum(filtro$DT_SIN_PRI == data)*100000/abitantes)
  N_NOTIF <- append(N_NOTIF, sum(filtro$DT_NOTIFIC == data)*100000/abitantes)
}

QualidadeAr["N_NOTIF"] <- N_NOTIF
QualidadeAr["N_SIN_PRI"] <- N_SIN_PRI

write.table(QualidadeAr, file = "R/procesados/QualidadeAr.csv", sep = ";", row.names = F)

QualidadeAr <- read_delim("R/procesados/QualidadeAr.csv", 
delim = ";", escape_double = FALSE, col_types = cols(Código_do_Município = col_character(), 
Data = col_character(), `PM 2,5` = col_double(), 
H2S = col_double()), trim_ws = TRUE)