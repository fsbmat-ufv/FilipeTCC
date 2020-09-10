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


#dt2000 <- read.xlsx("C:/Users/Filipe Fulgêncio/Documents/github/FilipeTCC/Campeonato2000.xlsx", colNames = TRUE)
#names(dt2000)<- c("Hora"          ,
#"Dia"           ,
#"Data"          , 
#"Mandante"      ,
#"Visitante"     ,
#"Vencedor"      , 
#"Rodada"        ,
#"Arena"         ,
#"GolsMan"       ,
#"GolsVisit"     , 
#"EstadoMan"     ,
#"EstadoVisit"     ,
#"EstadoVenc")  

#dt2000$Mandante <- abjutils::rm_accent(dt2000$Mandante)
#dt2000$Mandante <- str_squish(dt2000$Mandante)
#dt2000$Mandante <- str_trim(dt2000$Mandante, side = c("both", "left", "right"))
#dt2000$Mandante <- str_to_upper(dt2000$Mandante)

#dt2000$Visitante <- abjutils::rm_accent(dt2000$Visitante)
#dt2000$Visitante <- str_squish(dt2000$Visitante)
#dt2000$Visitante <- str_trim(dt2000$Visitante, side = c("both", "left", "right"))
#dt2000$Visitante <- str_to_upper(dt2000$Visitante)

#dt2000$Dia <- abjutils::rm_accent(dt2000$Dia)
#dt2000$Dia <- str_squish(dt2000$Dia)
#dt2000$Dia <- str_trim(dt2000$Dia, side = c("both", "left", "right"))
#dt2000$Dia <- str_to_upper(dt2000$Dia)

#dt2000$Arena <- abjutils::rm_accent(dt2000$Arena)
#dt2000$Arena <- str_squish(dt2000$Arena)
#dt2000$Arena <- str_trim(dt2000$Arena, side = c("both", "left", "right"))
#dt2000$Arena <- str_to_upper(dt2000$Arena)

#dt2000$EstadoVenc <- str_to_upper(dt2000$EstadoVenc)

#dt2000$Vencedor <- abjutils::rm_accent(dt2000$Vencedor)
#dt2000$Vencedor <- str_squish(dt2000$Vencedor)
#dt2000$Vencedor <- str_trim(dt2000$Vencedor, side = c("both", "left", "right"))
#dt2000$Vencedor <- str_to_upper(dt2000$Vencedor)

#dt2000$Rodada <- abjutils::rm_accent(dt2000$Rodada)
#dt2000$Rodada <- str_squish(dt2000$Rodada)
#dt2000$Rodada <- str_trim(dt2000$Rodada, side = c("both", "left", "right"))
#dt2000$Rodada <- str_to_upper(dt2000$Rodada)





#saveRDS(dt2000, file = "dt2000.Rds")
dt2000 <- readRDS("dt2000.Rds")
dt2000$Rodada <- str_replace_all(dt2000$Rodada, "ª", "")
#dt2000$Data <- as.Date(as.character(dt2000$Data), format = "%d/%m/%Y")


ano03 <- dt2000 %>% filter(Data>=37709 & Data<=37969)
vitorias03 <- ano03 [c(1:552),c("Vencedor","Rodada","EstadoVenc")]

ano04 <- dt2000 %>% filter(Data>=38098 & Data<=38340)
vitorias04 <- ano04 [c(1:552),c("Vencedor","Rodada","EstadoVenc")]

ano05 <- dt2000 %>% filter(Data>=38465 & Data<=38690)
vitorias05 <- ano05 [c(1:552),c("Vencedor","Rodada","EstadoVenc")]

ano06 <- dt2000 %>% filter(Data>=38822 & Data<=39054)
vitorias06 <- ano06 [c(1:552),c("Vencedor","Rodada","EstadoVenc")]

ano07 <- dt2000 %>% filter(Data>=39214 & Data<=39418)
vitorias07 <- ano07 [c(1:552),c("Vencedor","Rodada","EstadoVenc")]

ano08 <- dt2000 %>% filter(Data>=39578 & Data<=39789)
vitorias08 <- ano08 [c(1:552),c("Vencedor","Rodada","EstadoVenc")]

ano09 <- dt2000 %>% filter(Data>=39942 & Data<=40153)
vitorias09 <- ano09 [c(1:552),c("Vencedor","Rodada","EstadoVenc")]

ano10 <- dt2000 %>% filter(Data>=40306 & Data<=40517)
vitorias10 <- ano10 [c(1:552),c("Vencedor","Rodada","EstadoVenc")]

ano11 <- dt2000 %>% filter(Data>=40684 & Data<=40881)
vitorias11 <- ano11 [c(1:552),c("Vencedor","Rodada","EstadoVenc")]

ano12 <- dt2000 %>% filter(Data>=41048 & Data<=41245)
vitorias12 <- ano12 [c(1:552),c("Vencedor","Rodada","EstadoVenc")]

ano13 <- dt2000 %>% filter(Data>=41419 & Data<=41616)
vitorias13 <- ano13 [c(1:552),c("Vencedor","Rodada","EstadoVenc")]

ano14 <- dt2000 %>% filter(Data>=41748 & Data<=41980)
vitorias14 <- ano14 [c(1:552),c("Vencedor","Rodada","EstadoVenc")]

ano15 <- dt2000 %>% filter(Data>=42133 & Data<=42344)
vitorias15 <- ano15 [c(1:552),c("Vencedor","Rodada","EstadoVenc")]

ano16 <- dt2000 %>% filter(Data>=42504 & Data<=42715)
vitorias16 <- ano16 [c(1:552),c("Vencedor","Rodada","EstadoVenc")]

ano17 <- dt2000 %>% filter(Data>=42868 & Data<=43072)
vitorias17 <- ano17 [c(1:552),c("Vencedor","Rodada","EstadoVenc")]

ano18 <- dt2000 %>% filter(Data>=43204 & Data<=43436)
vitorias18 <- ano18 [c(1:552),c("Vencedor","Rodada","EstadoVenc")]

ano19 <- dt2000 %>% filter(Data>=43582 & Data<=43807)
vitorias19 <- ano19 [c(1:552),c("Vencedor","Rodada","EstadoVenc")]



