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
library(dplyr)
library(readr)
library(animation)
library(ggthemes)

# dt2000 <- fread("C:/Users/Filipe Fulgêncio/Documents/github/FilipeTCC/Campeonato2000.csv")
# #dt2000 <- read.xlsx("C:/Users/Filipe Fulgêncio/Documents/github/FilipeTCC/Campeonato2000.xlsx", colNames = TRUE)
# names(dt2000)<- c("Hora"          ,
# "Dia"           ,
# "Data"          ,
# "Mandante"      ,
# "Visitante"     ,
# "Vencedor"      ,
# "Rodada"        ,
# "Arena"         ,
# "GolsMan"       ,
# "GolsVisit"     ,
# "EstadoMan"     ,
# "EstadoVisit"     ,
# "EstadoVenc")
# 
# dt2000$Mandante <- abjutils::rm_accent(dt2000$Mandante)
# dt2000$Mandante <- str_squish(dt2000$Mandante)
# dt2000$Mandante <- str_trim(dt2000$Mandante, side = c("both", "left", "right"))
# dt2000$Mandante <- str_to_upper(dt2000$Mandante)
# dt2000$Visitante <- abjutils::rm_accent(dt2000$Visitante)
# dt2000$Visitante <- str_squish(dt2000$Visitante)
# dt2000$Visitante <- str_trim(dt2000$Visitante, side = c("both", "left", "right"))
# dt2000$Visitante <- str_to_upper(dt2000$Visitante)
# dt2000$Dia <- abjutils::rm_accent(dt2000$Dia)
# dt2000$Dia <- str_squish(dt2000$Dia)
# dt2000$Dia <- str_trim(dt2000$Dia, side = c("both", "left", "right"))
# dt2000$Dia <- str_to_upper(dt2000$Dia)
# dt2000$Arena <- abjutils::rm_accent(dt2000$Arena)
# dt2000$Arena <- str_squish(dt2000$Arena)
# dt2000$Arena <- str_trim(dt2000$Arena, side = c("both", "left", "right"))
# dt2000$Arena <- str_to_upper(dt2000$Arena)
# dt2000$EstadoVenc <- str_to_upper(dt2000$EstadoVenc)
# dt2000$Vencedor <- abjutils::rm_accent(dt2000$Vencedor)
# dt2000$Vencedor <- str_squish(dt2000$Vencedor)
# dt2000$Vencedor <- str_trim(dt2000$Vencedor, side = c("both", "left", "right"))
# dt2000$Vencedor <- str_to_upper(dt2000$Vencedor)
# dt2000$Rodada <- abjutils::rm_accent(dt2000$Rodada)
# dt2000$Rodada <- str_squish(dt2000$Rodada)
# dt2000$Rodada <- str_trim(dt2000$Rodada, side = c("both", "left", "right"))
# dt2000$Rodada <- str_to_upper(dt2000$Rodada)
# dt2000$Rodada <- str_replace_all(dt2000$Rodada, "ª", "")
# dt2000$Data <- as.Date((dt2000$Data), format = "%d-%m-%Y")


#saveRDS(dt2000, file = "dt2000.Rds")
dt2000 <- readRDS("dt2000.Rds")
dt2000$Data <- as.Date(as.character(dt2000$Data), format = "%Y-%m-%d")
dt2000$Vencedor[dt2000$Vencedor=="-"] <- "EMPATE"


#dt2000$Data <- format(dt2000$Data,"%d-%m-%Y")
dt2003 <- dt2000 %>% filter(Data>=as.Date("2003-01-01"))
dt2003$Ano <- year(dt2003$Data)

dt2003$Derrotado <- ifelse(dt2003$Vencedor=="EMPATE", "EMPATE",ifelse(dt2003$Visitante==dt2003$Vencedor,dt2003$Mandante, dt2003$Visitante))
dt2003$PontMandante <- ifelse(dt2003$Vencedor=="EMPATE", 1,ifelse(dt2003$Mandante==dt2003$Vencedor&dt2003$Vencedor!="EMPATE",3, 0))
dt2003$PontVisitante <- ifelse(dt2003$Vencedor=="EMPATE", 1,ifelse(dt2003$Visitante==dt2003$Vencedor&dt2003$Vencedor!="EMPATE",3, 0))


saveRDS(dt2003,"dados.rds") 

# dt1 <- dt2003 %>% filter(Ano=="2019", Mandante=="CSA") %>% select(Mandante, PontMandante)
# dt2 <- dt2003 %>% filter(Ano=="2019", Visitante=="CSA") %>% select(Visitante, PontVisitante)

dt1 <- dt2003 %>% filter(Ano=="2019") %>% group_by(Mandante) %>% summarise(PontMan=sum(PontMandante))
dt2 <- dt2003 %>% filter(Ano=="2019") %>% group_by(Visitante) %>% summarise(PontVis=sum(PontVisitante))

dt <- data.frame(dt1[,1:2],dt2[,2])
dt$Pontos <- rowSums(dt[,2:3])


dt$PontVisitante <- ifelse(dt$PontVisitante==dt$PontMandante, 0, dt$PontVisitante)
colSums(dt[,2:3])

#analise grafica
head(dt2000)
v2003 <- 
  dt2000 %>% filter(Data>=as.Date("2003-01-01") & Data<=as.Date("2003-12-31")) %>% 
  group_by(Vencedor) %>% summarise(Quant=n(),.groups="drop")   
 v2003 %>% ggplot(aes(Quant,reorder(Vencedor,Quant),fill=Vencedor))+
  geom_col(show.legend = FALSE)+
   theme(panel.background = element_rect(fill = "white", colour = "black")) +
   theme(panel.grid.major = element_line(colour = "Black", linetype = "solid")) + 
   xlab("Quantidades de Vitórias/Empates")+
  ylab("Times que disputaram o Campeonato")+
  geom_text(aes(label=Quant),nudge_x = 4)+
  theme_bw()+
  ggtitle("Número de vitórias no Brasileirão 2003")
ggplotly(v2003)

are2003 <- dt2000 %>% filter(Data>=as.Date("2003-01-01") & Data<=as.Date("2003-12-31")) %>% 
  group_by(Arena) %>% summarise(Quant=n(),.groups="drop") 
  are2003 %>%  ggplot(aes(Quant,reorder(Arena,Quant),fill=Arena))+
  geom_col(show.legend = FALSE)+
  theme(panel.background = element_rect(fill = "white", colour = "black")) +
  theme(panel.grid.major = element_line(colour = "Black", linetype = "solid")) + 
   xlab("Quantidades de jogos nas Arenas")+
  ylab("Árenas que participaram do Campeonato")+
  geom_text(aes(label=Quant),nudge_x = 4)+
  ggtitle("Número de jogos disputados nas árenas no Brasileirão 2003")
ggplotly(a2003)

head(dt2000)
gm2003 <- dt2000 %>% filter(Data>=as.Date("2003-01-01") & Data<=as.Date("2003-12-31")) %>% 
  group_by(GolsMan) %>% summarise(Quant=n(),.groups="drop") 
  gm2003 %>% ggplot(aes(GolsMan,Quant,fill=GolsMan))+
  geom_col(show.legend = FALSE)+
  theme(panel.background = element_rect(fill = "white", colour = "black")) +
  theme(panel.grid.major = element_line(colour = "Black", linetype = "solid")) + 
  xlab("Gols dos times mandantes")+
  ylab("Quantidade de gols por jogos dos times mandandtes")+
  geom_text(aes(label=Quant),nudge_y = 5)+
  ggtitle("Número de gols dos mandantes no Brasileirão 2003")
ggplotly(gm2003)

head(dt2000)
gv2003 <- dt2000 %>% filter(Data>=as.Date("2003-01-01") & Data<=as.Date("2003-12-31")) %>% 
  group_by(GolsVisit) %>% summarise(Quant=n(),.groups="drop") 
gv2003 %>%  ggplot(aes(GolsVisit,Quant,fill=GolsVisit))+
  geom_col(show.legend = FALSE)+
  theme(panel.background = element_rect(fill = "white", colour = "black")) +
  theme(panel.grid.major = element_line(colour = "Black", linetype = "solid")) + 
  xlab("Gols dos times visitantes")+
  ylab("Quantidade de gols por jgos pelos times visitantes")+
    geom_text(aes(label=Quant),nudge_y = 5)+
  ggtitle("Número de gols dos visitantes no Brasileirão 2003")
ggplotly(gv2003)

head(dt2000)
venare2003 <- dt2000 %>% filter(Data>=as.Date("2003-01-01") & Data<=as.Date("2003-12-31")) %>% 
  group_by(Vencedor,Arena) %>% summarise(Quant=n(),.groups="drop") 
#rod2003 %>%  ggplot(aes(Quant,reorder(Arena,Quant),fill=Quant))+
 # geom_col(show.legend = FALSE)+
#  theme(panel.background = element_rect(fill = "white", colour = "black")) +
#  theme(panel.grid.major = element_line(colour = "Black", linetype = "solid")) + 
#  xlab("Gols dos times visitantes")+
#  ylab("Quantidade de gols por jgos pelos times visitantes")+
#  geom_text(aes(label=Quant),nudge_X = 5)+
#  ggtitle("Número de gols dos visitantes no Brasileirão 2003")
#ggplotly(gv2003)

head(dt2000)
vgm2003 <- dt2000 %>% filter(Data>=as.Date("2003-01-01") & Data<=as.Date("2003-12-31")) %>% 
  group_by(Vencedor,GolsMan) %>% summarise(Quant=n(),.groups="drop") 

head(dt2000)
vgv2003 <- dt2000 %>% filter(Data>=as.Date("2003-01-01") & Data<=as.Date("2003-12-31")) %>% 
  group_by(Vencedor,GolsVisit) %>% summarise(Quant=n(),.groups="drop") 

head(dt2000)
vrodare <- dt2000 %>% filter(Data>=as.Date("2003-01-01") & Data<=as.Date("2003-12-31")) %>% 
  group_by(Vencedor,Rodada,Arena) %>% summarise(Quant=n(),.groups="drop") 

head(dt2000)
mandvenare <- dt2000 %>% filter(Data>=as.Date("2003-01-01") & Data<=as.Date("2003-12-31")) %>% 
  group_by(Vencedor,Mandante,Arena) %>% summarise(Quant=n(),.groups="drop") 

head(dt2000)
visitvenare <- dt2000 %>% filter(Data>=as.Date("2003-01-01") & Data<=as.Date("2003-12-31")) %>% 
  group_by(Vencedor,Visitante,Arena) %>% summarise(Quant=n(),.groups="drop") 

head(dt2000)
visitvenndia <- dt2000 %>% filter(Data>=as.Date("2003-01-01") & Data<=as.Date("2003-12-31")) %>% 
  group_by(Vencedor,Visitante,Dia) %>% summarise(Quant=n(),.groups="drop") 

head(dt2000)
mandvenndia <- dt2000 %>% filter(Data>=as.Date("2003-01-01") & Data<=as.Date("2003-12-31")) %>% 
  group_by(Vencedor,Mandante,Dia) %>% summarise(Quant=n(),.groups="drop") 

head(dt2000)
gmvenndia <- dt2000 %>% filter(Data>=as.Date("2003-01-01") & Data<=as.Date("2003-12-31")) %>% 
  group_by(Vencedor,GolsMan,Dia) %>% summarise(Quant=n(),.groups="drop") 


head(dt2000)
gvvenndia <- dt2000 %>% filter(Data>=as.Date("2003-01-01") & Data<=as.Date("2003-12-31")) %>%
  group_by(Vencedor,GolsVisit,Dia) %>% summarise(Quant=n(),.groups="drop") 


head(dt2000)
venestdv <- dt2000 %>% filter(Data>=as.Date("2003-01-01") & Data<=as.Date("2003-12-31")) %>%
  group_by(Vencedor,EstadoVenc) %>% summarise(Quant=n(),.groups="drop")
venestdv %>%  ggplot(aes(EstadoVenc,Quant,fill=EstadoVenc))+
  geom_col(show.legend = FALSE)+
  theme(panel.background = element_rect(fill = "white", colour = "black")) +
  theme(panel.grid.major = element_line(colour = "Black", linetype = "solid")) + 
  xlab("Estados Vencedores")+
  ylab("Quantidade de vitórias")+
ggtitle("Número de vitórias dos times de cada estado no Brasileirão 2003")
ggplotly(venestdv)

head(dt2003)
vitot <- dt2003 %>%
  group_by(Vencedor) %>% summarise(Quant=n(),.groups="drop")
  vitot %>%  ggplot(aes(Quant,reorder(Vencedor,Quant),fill=Vencedor))+
  geom_col(show.legend = FALSE)+
  theme(panel.background = element_rect(fill = "white", colour = "black")) +
  theme(panel.grid.major = element_line(colour = "Black", linetype = "solid")) + 
  xlab("Estados Vencedores")+
  ylab("Quantidade de vitórias gerais")+
  geom_text(aes(label=Quant),nudge_x = 30)+
  ggtitle("Número de vitórias dos times nas edições de pontos corridos do Brasileirão")

head(dt2000)
ar <- dt2000 %>%
  group_by(Arena) %>% summarise(Quant=n(),.groups="drop")
ar %>%  ggplot(aes(Quant,reorder(Arena,Quant),fill=Arena))+
  geom_col(show.legend = FALSE)+
  theme(panel.background = element_rect(fill = "white", colour = "black")) +
  theme(panel.grid.major = element_line(colour = "Black", linetype = "solid")) + 
  xlab("Estádios")+
  ylab("Quantidade de jogos nos estádios")+
  geom_text(aes(label=Quant),nudge_x = 30)+
    ggtitle("Número de jogos em cada estádio nas edições do Brasileirão")

ar %>% select("Arena")
regular <- ar %>% filter(Quant<=5)
regular %>%  ggplot(aes(Quant,reorder(Arena,Quant),fill=Arena))+
  geom_col(show.legend = FALSE)+
  theme(panel.background = element_rect(fill = "white", colour = "black")) +
  theme(panel.grid.major = element_line(colour = "Black", linetype = "solid")) + 
  xlab("Estádios")+
  ylab("Quantidade de jogos nos estádios")+
  geom_text(aes(label=Quant),nudge_x = 10)+
  ggtitle("Os estádios com menos jogos no Brasileirão de pontos corridos")

ar %>% select("Arena")
bom <- ar %>% filter(Quant>255)
bom %>%  ggplot(aes(Quant,reorder(Arena,Quant),fill=Arena))+
  geom_col(show.legend = FALSE)+
  theme(panel.background = element_rect(fill = "white", colour = "black")) +
  theme(panel.grid.major = element_line(colour = "Black", linetype = "solid")) + 
  xlab("Estádios")+
  ylab("Quantidade de jogos nos estádios")+
  geom_text(aes(label=Quant),nudge_x = 30)+
  ggtitle("Os 10 estádios com mais jogos no Brasileirão de pontos corridos")

  

  
  head(dt2000)
  gm200067 <- dt2000 %>% filter(Data>=as.Date("2006-01-01") & Data<=as.Date("2007-12-31")) %>% 
    group_by(GolsMan) %>% summarise(Quant=n(),.groups="drop") 
  gm200067 %>%    ggplot(aes(GolsMan,Quant,fill=GolsMan))+
    geom_col(show.legend = FALSE)+
    theme(panel.background = element_rect(fill = "white", colour = "black")) +
    theme(panel.grid.major = element_line(colour = "Black", linetype = "solid")) + 
    xlab("Gols dos times Mandantes")+
    ylab("Quantidade de gols por jogos pelos times mandantes")+
    geom_text(aes(label=Quant),nudge_y = 5)+
    ggtitle("Número de gols dos mandantes no Brasileirão 2006-07")
  
  
  
  head(dt2000)
  gm20001819 <- dt2000 %>% filter(Data>=as.Date("2018-01-01") & Data<=as.Date("2019-12-31")) %>% 
    group_by(GolsMan) %>% summarise(Quant=n(),.groups="drop") 
  gm20001819 %>%  ggplot(aes(GolsMan,Quant,fill=GolsMan))+
    geom_col(show.legend = FALSE)+
    theme(panel.background = element_rect(fill = "white", colour = "black")) +
    theme(panel.grid.major = element_line(colour = "Black", linetype = "solid")) + 
    xlab("Gols dos times Mandantes")+
    ylab("Quantidade de gols por jogos pelos times mandantes")+
    geom_text(aes(label=Quant),nudge_y = 5)+
    ggtitle("Número de gols dos mandantes no Brasileirão 2018-19")

  head(dt2000)
  gv200067 <- dt2000 %>% filter(Data>=as.Date("2006-01-01") & Data<=as.Date("2007-12-31")) %>% 
    group_by(GolsVisit) %>% summarise(Quant=n(),.groups="drop") 
  gv200067 %>%    ggplot(aes(GolsVisit,Quant,fill=GolsVisit))+
    geom_col(show.legend = FALSE)+
    theme(panel.background = element_rect(fill = "white", colour = "black")) +
    theme(panel.grid.major = element_line(colour = "Black", linetype = "solid")) + 
    xlab("Gols dos times Visitantes")+
    ylab("Quantidade de gols por jogos pelos times visitantes")+
    geom_text(aes(label=Quant),nudge_y = 5)+
    ggtitle("Número de gols dos visitantes no Brasileirão 2006-07")
  
  head(dt2000)
  gv20001819 <- dt2000 %>% filter(Data>=as.Date("2018-01-01") & Data<=as.Date("2019-12-31")) %>% 
    group_by(GolsVisit) %>% summarise(Quant=n(),.groups="drop") 
  gv20001819 %>%  ggplot(aes(GolsVisit,Quant,fill=GolsVisit))+
    geom_col(show.legend = FALSE)+
    theme(panel.background = element_rect(fill = "white", colour = "black")) +
    theme(panel.grid.major = element_line(colour = "Black", linetype = "solid")) + 
    xlab("Gols dos times Visitantes")+
    ylab("Quantidade de gols por jogos pelos times visitantes")+
    geom_text(aes(label=Quant),nudge_y = 5)+
    ggtitle("Número de gols dos visitantes no Brasileirão 2018-19")
  
  
    v2000304 <- dt2000 %>% filter(Data>=as.Date("2004-01-01") & Data<=as.Date("2004-12-31")) %>% 
    group_by(Vencedor) %>% summarise(Quant=n(),.groups="drop")    
  v2000304 %>%  ggplot(aes(Quant,reorder(Vencedor,Quant),fill=Vencedor))+
       geom_col(show.legend = FALSE)+
    theme(panel.background = element_rect(fill = "white", colour = "black")) +
    theme(panel.grid.major = element_line(colour = "Black", linetype = "solid")) + 
    xlab("Quantidades de Vitórias/Empates")+
    ylab("Times que disputaram o Campeonato")+
    geom_text(aes(label=Quant),nudge_x = 4)+
    theme_bw()+
    ggtitle("Número de vitórias no Brasileirão 2003-04")

  
  v2001819 <- 
    dt2000 %>% filter(Data>=as.Date("2018-01-01") & Data<=as.Date("2019-12-31")) %>% 
    group_by(Vencedor) %>% summarise(Quant=n(),.groups="drop")    
  v2001819 %>%  ggplot(aes(Quant,reorder(Vencedor,Quant),fill=Vencedor))+
    geom_col(show.legend = FALSE)+
    theme(panel.background = element_rect(fill = "white", colour = "black")) +
    theme(panel.grid.major = element_line(colour = "Black", linetype = "solid")) + 
    xlab("Quantidades de Vitórias/Empates")+
    ylab("Times que disputaram o Campeonato")+
    geom_text(aes(label=Quant),nudge_x = 4)+
    theme_bw()+
    ggtitle("Número de vitórias no Brasileirão 2018-19")


  gv2001819 <- 
    dt2000 %>% filter(Data>=as.Date("2018-01-01") & Data<=as.Date("2019-12-31")) %>% 
    group_by(Vencedor,GolsMan) %>% summarise(Quant=n(),.groups="drop")    

  
  str(dt2003)
dados <- dt2000 %>% filter(Data>=as.Date("2003-01-01"))
dados$Ano <- year(dados$Data)
saveRDS(dados,"dados.rds")  

numcruzM <- dados %>% filter(Mandante =="CRUZEIRO") %>% summarise(golsM=sum(GolsMan))
numcruzV <- dados %>% filter(Visitante =="CRUZEIRO") %>% summarise(golsV=sum(GolsVisit))
(numcruz <- numcruzM + numcruzV)

mandanteNG <- dados %>% group_by(Mandante) %>% summarise(golsM=sum(GolsMan))
names(mandanteNG) <- c("Time","GolsM")
visitanteNG <- dados %>% group_by(Visitante) %>% summarise(golsM=sum(GolsVisit))
names(visitanteNG) <- c("Time","GolsV")
totGols <- left_join(mandanteNG,visitanteNG,by="Time")
totGols$Total <- rowSums(totGols[,2:3])
totGols %>%  ggplot(aes(GolsM,reorder(Time,GolsM),fill=GolsM))+
  geom_col(show.legend = FALSE)+
  theme(panel.background = element_rect(fill = "white", colour = "black")) +
  theme(panel.grid.major = element_line(colour = "Black", linetype = "solid")) + 
  xlab("Quantidades de gols no Brasileirão como mandante")+
  ylab("Times que disputaram o Campeonato")+
  geom_text(aes(label=GolsM),nudge_x = 4)+
  theme_bw()+
  ggtitle("Número de gols dos times como mandante no Brasileirão")

totGols %>%  ggplot(aes(GolsV,reorder(Time,GolsV),fill=GolsV))+
  geom_col(show.legend = FALSE)+
  theme(panel.background = element_rect(fill = "white", colour = "black")) +
  theme(panel.grid.major = element_line(colour = "Black", linetype = "solid")) + 
  xlab("Quantidades de gols no Brasileirão como visitante")+
  ylab("Times que disputaram o Campeonato")+
  geom_text(aes(label=GolsV),nudge_x = 10)+
  theme_bw()+
  ggtitle("Número de gols dos times como visitantes no Brasileirão")

totGols %>%  ggplot(aes(Total,reorder(Time,Total),fill=Total))+
  geom_col(show.legend = FALSE)+
  theme(panel.background = element_rect(fill = "white", colour = "black")) +
  theme(panel.grid.major = element_line(colour = "Black", linetype = "solid")) + 
  xlab("Quantidades de gols no Brasileirão")+
  ylab("Times que disputaram o Campeonato")+
  geom_text(aes(label=Total),nudge_x = 18)+
  theme_bw()+
  ggtitle("Total de gols dos times no Brasileirão")

dados$vitMan <- ifelse(dados$Mandante==dados$Vencedor,1,0)
testeM <- dados %>% group_by(Mandante) %>% summarise(vitM=sum(vitMan))
names(testeM) <- c("Time","VitM")
testeM %>%  ggplot(aes(VitM,reorder(Time,VitM),fill=VitM))+
  geom_col(show.legend = FALSE)+
  theme(panel.background = element_rect(fill = "white", colour = "black")) +
  theme(panel.grid.major = element_line(colour = "Black", linetype = "solid")) + 
  xlab("Quantidades de vitórias como mandante no Brasileirão")+
  ylab("Times que disputaram o Campeonato")+
  geom_text(aes(label=VitM),nudge_x = 8)+
  theme_bw()+
  ggtitle("Quantidades de vitórias como mandante no Brasileirão de pontos corridos")

dados$vitVist <- ifelse(dados$Visitante==dados$Vencedor,1,0)
testeV <- dados %>% group_by(Visitante) %>% summarise(vitV=sum(vitVist))
names(testeV) <- c("Time","VitV")
testeV %>%  ggplot(aes(VitV,reorder(Time,VitV),fill=VitV))+
  geom_col(show.legend = FALSE)+
  theme(panel.background = element_rect(fill = "white", colour = "black")) +
  theme(panel.grid.major = element_line(colour = "Black", linetype = "solid")) + 
  xlab("Quantidades de vitórias como visitante no Brasileirão")+
  ylab("Times que disputaram o Campeonato")+
  geom_text(aes(label=VitV),nudge_x = 8)+
  theme_bw()+
  ggtitle("Quantidades de vitórias como visitante no Brasileirão de pontos corridos")

totvit <- left_join(testeM,testeV,by="Time")
totvit$Total <- rowSums(totvit[,2:3])


dados$derM <- ifelse(dados$Mandante==dados$Vencedor,0,1)
testeDM <- dados %>% group_by(Mandante) %>% summarise(derm=sum(derM))
names(testeDM) <- c("Time","DerM")

dados$derV <- ifelse(dados$Visitante==dados$Vencedor,0,1)
testeDV <- dados %>% group_by(Visitante) %>% summarise(derm=sum(derV))
names(testeDV) <- c("Time","DerV")

totdet <- left_join(testeDM,testeDV,by="Time")
totdet$Total <- rowSums(totdet[,2:3])

##
dados$empM <- (dados$vencedor="EMPATE")
testeEmpM <- dados %>% group_by(Vencedor) %>% summarise(p=sum(empM))
names(testeEmpM) <- c("Time","EmpM")

dados$eM <- ifelse(dados$Mandante==dados$Vencedor,1,1)
testeEM <- dados %>% group_by(Vencedor) %>% summarise(em=sum(eM))
names(testeEM) <- c("Time","EmpM")

dados$Em <- ifelse(dados$Visitante==dados$Vencedor,1,1)
testeEV <- dados %>% group_by(Vencedor) %>% summarise(me=sum(Em))
names(testeVM) <- c("Time","EmpM")


head(dt2000)
venestdv19 <- dt2000 %>% filter(Data>=as.Date("2019-01-01") & Data<=as.Date("2019-12-31")) %>%
  group_by(Mandante,Visitante,Vencedor) %>% summarise(Quant=n(),.groups="drop")

venestdv19$vv19 <- ifelse(venestdv19$Mandante==venestdv19$Vencedor,3,0)
testeV19 <- venestdv19 %>% group_by(Mandante) %>% summarise(v19=sum(vv19))
names(testeV19) <- c("Time","VitM")
venestdv19$vvl19 <- ifelse(venestdv19$Visitante==venestdv19$Vencedor,3,0)
testeVV19 <- venestdv19 %>% group_by(Visitante) %>% summarise(vl19=sum(vvl19))
names(testeVV19) <- c("Time","VitV")
tot19<- left_join(testeV19,testeVV19,by="Time")
tot19$Total <- rowSums(tot19[,2:3])

venestdv19$Em19 <- ifelse(venestdv19$Visitante==venestdv19$Vencedor,1,1)
testeEM19 <- venestdv19 %>% group_by(Vencedor) %>% summarise(me19=sum(Em19))
names(testeEM19) <- c("Time","EmpM")










head(dados)
testeEmpV <- dados %>% filter(dados$Vencedor=="EMPATE") %>% 
  group_by(Vencedor) %>% summarise(Quant=n(),.groups="drop") 

