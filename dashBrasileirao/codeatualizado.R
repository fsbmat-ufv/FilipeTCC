rm(list = ls())
graphics.off()
cat("\014")

library("openxlsx")
library(ggplot2)
library(ggalt)
library(compareDF)
library(stringr)
library(tidyverse)
library("car")
library(pdftools)
library(scatterplot3d)
library(data.table)
library(lubridate)
library(ISwR)
library(dtplyr)
library(stringr)
library(plotly)

library(tidyverse)
library(lubridate)
library(plotly)
library(devtools)


dt20 <- fread("campeonato_brasileiro_2020.csv")
#dt20 <- as.data.frame(dt20)
#dt20 <- read.xlsx("C:/Users/Filipe Fulgêncio/Documents/github/FilipeTCC/campeonato_brasileiro_2020.xlsx", colNames = TRUE)
names(dt20)<- c( "ID"             ,
                 "Rodada"         ,
                 "Data"           ,
                 "Horário"        ,
                 "Dia"            ,
                 "Mandante"       ,
                 "Visitante"      ,
                 "Vencedor"       ,
                 "Arena"          ,
                 "GolsMan"        ,
                 "GolsVisit"      ,
                 "EstadoMan"      ,
                 "EstadoVisit"    ,
                 "EstadoVenc")

dt20$Mandante <- iconv(dt20$Mandante, from="UTF-8", to = "latin1")
dt20$Mandante <- abjutils::rm_accent(dt20$Mandante)
dt20$Mandante <- str_squish(dt20$Mandante)
dt20$Mandante <- str_trim(dt20$Mandante, side = c("both", "left", "right"))
dt20$Mandante <- str_to_upper(dt20$Mandante)
dt20$Visitante <- iconv(dt20$Visitante, from="UTF-8", to = "latin1")
dt20$Visitante <- abjutils::rm_accent(dt20$Visitante)
dt20$Visitante <- str_squish(dt20$Visitante)
dt20$Visitante <- str_trim(dt20$Visitante, side = c("both", "left", "right"))
dt20$Visitante <- str_to_upper(dt20$Visitante)
dt20$Dia <- iconv(dt20$Dia, from="UTF-8", to = "latin1")
dt20$Dia <- abjutils::rm_accent(dt20$Dia)
dt20$Dia <- str_squish(dt20$Dia)
dt20$Dia <- str_trim(dt20$Dia, side = c("both", "left", "right"))
dt20$Dia <- str_to_upper(dt20$Dia)
dt20$Arena <- iconv(dt20$Arena, from="UTF-8", to = "latin1")
dt20$Arena <- abjutils::rm_accent(dt20$Arena)
dt20$Arena <- str_squish(dt20$Arena)
dt20$Arena <- str_trim(dt20$Arena, side = c("both", "left", "right"))
dt20$Arena <- str_to_upper(dt20$Arena)
dt20$EstadoVenc <- str_to_upper(dt20$EstadoVenc)
dt20$Vencedor <- iconv(dt20$Vencedor, from="UTF-8", to = "latin1")
dt20$Vencedor <- abjutils::rm_accent(dt20$Vencedor)
dt20$Vencedor <- str_squish(dt20$Vencedor)
dt20$Vencedor <- str_trim(dt20$Vencedor, side = c("both", "left", "right"))
dt20$Vencedor <- str_to_upper(dt20$Vencedor)
dt20$Rodada <- abjutils::rm_accent(dt20$Rodada)
dt20$Rodada <- str_squish(dt20$Rodada)
dt20$Rodada <- str_trim(dt20$Rodada, side = c("both", "left", "right"))
dt20$Rodada <- str_to_upper(dt20$Rodada)
dt20$Rodada <- str_replace_all(dt20$Rodada, "ª", "")
dt20$Data <- as.Date((dt20$Data), format = "%d-%m-%Y")
#dt20 <- readRDS("dt20.Rds")
saveRDS(dt20,"resultado20.rds") 


dt23 <- dt20 %>% filter(Data>=as.Date("2003-01-01"))
###############################################
#Correcao de erros encontrados no data.frame
###############################################
dt23$Data[dt23$Data=="2007-05-17"] <- "2008-05-17"
dt23$Mandante[dt23$Data=="2009-07-19"&dt23$Mandante=="BOTAFOGO-RJ"] <- "FLAMENGO"
dt23$Visitante[dt23$Data=="2009-07-19"&dt23$Visitante=="FLAMENGO"] <- "BOTAFOGO-RJ"
###############################################
###############################################
dt23$Temporada <- year(dt23$Data)
dt23$Temporada[dt23$Temporada=="2021"] <- "2020"
dt23$Vencedor[dt23$Vencedor=="-"] <- "EMPATE"
dt23$EstadoVenc[dt23$EstadoVenc=="-"] <- "EMPATE"
dt23$Derrotado <- ifelse(dt23$Vencedor=="EMPATE", "EMPATE",ifelse(dt23$Visitante==dt23$Vencedor,dt23$Mandante, dt23$Visitante))
dt23$PontMandante <- ifelse(dt23$Vencedor=="EMPATE", 1,ifelse(dt23$Mandante==dt23$Vencedor&dt23$Vencedor!="EMPATE",3, 0))
dt23$PontVisitante <- ifelse(dt23$Vencedor=="EMPATE", 1,ifelse(dt23$Visitante==dt23$Vencedor&dt23$Vencedor!="EMPATE",3, 0))
dt23$ID <- NULL

dt23 <- dt23 %>%
  select(Temporada,Data,Dia,Horário,Rodada,Arena,Mandante,Visitante,Vencedor,Derrotado,
         GolsMan,GolsVisit,PontMandante,PontVisitante,EstadoMan,EstadoVisit,EstadoVenc) 


saveRDS(dt23,"d2021.rds") 

  


