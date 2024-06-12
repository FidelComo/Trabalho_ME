library(tidyverse)
library(emmeans)
library(dplyr)

Lista_EstaçSes_MonitorAr_Nov_2022 <- read_delim("R/Lista EstaçSes MonitorAr - Nov-2022.csv", 
delim = ";", escape_double = FALSE, col_types = cols(`Código IBGE do Município` = col_character()), 
trim_ws = TRUE)

Lista_EstaçSes_MonitorAr_Nov_2022$`Código IBGE do Município` = substr(Lista_EstaçSes_MonitorAr_Nov_2022$`Código IBGE do Município`, 1, nchar(Lista_EstaçSes_MonitorAr_Nov_2022$`Código IBGE do Município`)-1)

censo2022 <- read_csv("R/censo2022.csv", 
col_types = cols(`Código IBGE` = "c", 
População = col_number()), locale = locale(grouping_mark = " "))

censo2022$`Código IBGE` = substr(censo2022$`Código IBGE`, 1, nchar(censo2022$`Código IBGE`)-1)

INFLUD22_03_04_2023 <- read_delim("R/INFLUD22-03-04-2023.csv", 
delim = ";", escape_double = FALSE, col_types = cols(
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
DT_TRT_COV = col_date(format = "%d/%m/%Y"), 
OUT_ANIM   = "c",
PCR_FLUBLI = "c",
FLUASU_OUT = "c",
FLUBLI_OUT = "c",
CO_MUN_NOT = "c"), 
trim_ws = TRUE)

ID_MUNICIPIO <- unique(INFLUD22_03_04_2023$CO_MUN_NOT)

jul_nov <- read_delim("R/Dados_monitorar_jul_nov_22-11-2022 DQAA.csv", 
delim = ";", escape_double = FALSE, col_types = cols(
Data = col_date(format = "%d/%m/%Y"),
Concentracao = col_number(), 
iqar = col_number()), locale = locale(decimal_mark = ",", 
grouping_mark = ".", encoding = "ISO-8859-1"), 
trim_ws = TRUE)

jan_mar <- read_delim("R/Dados_monitorar_jan_mar_22-11-2022 DQAA.csv", 
delim = ";", escape_double = FALSE, col_types = cols(
Data = col_date(format = "%d/%m/%Y"),
Concentracao = col_number(), 
iqar = col_number()), locale = locale(decimal_mark = ",", 
grouping_mark = ".", encoding = "ISO-8859-1"), 
trim_ws = TRUE)

abr_jun <- read_delim("R/Dados_monitorar_abr_jun_22-11-2022 DQAA.csv", 
delim = ";", escape_double = FALSE, col_types = cols(
Data = col_date(format = "%d/%m/%Y"),
Concentracao = col_number(), 
iqar = col_number()), locale = locale(decimal_mark = ",", 
grouping_mark = ".", encoding = "ISO-8859-1"), 
trim_ws = TRUE)



jul_nov["Código_do_Município"] <- "0"
for (i in unique(Lista_EstaçSes_MonitorAr_Nov_2022$`Nome do Município`)){
  codigo <- Lista_EstaçSes_MonitorAr_Nov_2022[Lista_EstaçSes_MonitorAr_Nov_2022$`Nome do Município` == i,]$`Código IBGE do Município`[1]
  jul_nov[jul_nov$Nome_do_Município == i,]["Código_do_Município"] <- codigo
}

jan_mar["Código_do_Município"] <- "0"
for (i in unique(Lista_EstaçSes_MonitorAr_Nov_2022$`Nome do Município`)){
  codigo <- Lista_EstaçSes_MonitorAr_Nov_2022[Lista_EstaçSes_MonitorAr_Nov_2022$`Nome do Município` == i,]$`Código IBGE do Município`[1]
  jan_mar[jan_mar$Nome_do_Município == i,]["Código_do_Município"] <- codigo
}

abr_jun["Código_do_Município"] <- "0"
for (i in unique(Lista_EstaçSes_MonitorAr_Nov_2022$`Nome do Município`)){
  codigo <- Lista_EstaçSes_MonitorAr_Nov_2022[Lista_EstaçSes_MonitorAr_Nov_2022$`Nome do Município` == i,]$`Código IBGE do Município`[1]
  abr_jun[abr_jun$Nome_do_Município == i,]["Código_do_Município"] <- codigo
}

municip_tot <- intersect(unique(Lista_EstaçSes_MonitorAr_Nov_2022$`Código IBGE do Município`), ID_MUNICIPIO)

censo2022 <- censo2022[censo2022$`Código IBGE` %in% municip_tot, ]
INFLUD22_03_04_2023 <- INFLUD22_03_04_2023[INFLUD22_03_04_2023$CO_MUN_NOT %in% municip_tot, ]
jul_nov <- jul_nov[jul_nov$Código_do_Município %in% municip_tot, ]
jan_mar <- jan_mar[jan_mar$Código_do_Município %in% municip_tot, ]
jan_mar[jan_mar$Sigla == "NH?",]$Sigla <- "NH3"
jan_mar[jan_mar$Sigla == "CH?",]$Sigla <- "CH4"
abr_jun <- abr_jun[abr_jun$Código_do_Município %in% municip_tot, ]
abr_jun[abr_jun$Sigla == "NH?",]$Sigla <- "NH3"
abr_jun[abr_jun$Sigla == "CH?",]$Sigla <- "CH4"

write.table(censo2022, file = "data/censo.csv", sep = ";", row.names = F)
write.table(INFLUD22_03_04_2023, file = "data/INFLUD.csv", sep = ";", row.names = F)
write.table(jul_nov, file = "data/jul_nov.csv", sep = ";", row.names = F)
write.table(jan_mar, file = "data/jan_mar.csv", sep = ";", row.names = F)
write.table(abr_jun, file = "data/abr_jun.csv", sep = ";", row.names = F)