rm(list = ls())
graphics.off()
cat("\014")

library("openxlsx")
library(ggplot2)
library(ggalt)
library(compareDF)
library(imdb)
library(stringr)
library(tidyverse)
library("car")
library(pdftools)
library(scatterplot3d)

 #dt2016 <- read.xlsx("C:/Users/Filipe Fulgêncio/Documents/github/FilipeTCC/Brasileiro2016.xlsx", colNames = TRUE, startRow = 3, sheet = 1, rows = seq(3:25))
 #names(dt2016)<- c("Classificacao" ,  
                #"Time"          , 
                #"Pontos"        ,
                #"Jogos"         ,
                #"Vitorias"      , 
                #"Empates"       , 
                #"Derrotas"      , 
                #"GolsPro"      ,
                #"GolsContra"   , 
                #"SaldoGols" ,
                #"Aproveitamento")
 
 #dt2016$Time <- abjutils::rm_accent(dt2016Time)
 #dt2016$Time <- str_squish(dt2016$Time)
 #dt2016$Time <- str_trim(dt2016$Time, side = c("both", "left", "right"))
 #dt2016$Time <- str_to_upper(dt2016$Time)
 #saveRDS(dt2016, file = "dt2016.Rds")
dt2016 <- readRDS("dt2016.Rds")

dt2016 %>% ggplot(aes(Pontos, Time,fill=Time,col="blue"))+
  geom_col(show.legend = FALSE)+
  labs(subtitle = "Campanha dos times no Campeonato Brasileiro de 2016", 
       x= "Pontos", 
       y= "Times", 
       title = "Tabela de Dados")


dt2016 %>% select("Time")
bom <- dt2016 %>% filter(Pontos>=60)

bom %>% ggplot(aes(Pontos, Time,fill=Time,col="blue"))+
  geom_point(show.legend = TRUE)+
  labs(subtitle = "Aproveitamento dos times 'bons' no Campeonato Brasileiro de 2016", 
       x= "Pontos", 
       y= "Times", 
       title = "Tabela de Dados")

##

dt2016 %>% select("Time")
regular <- dt2016 %>% filter(Pontos>=49 & Pontos<=60)

regular %>% ggplot(aes(Pontos, Time,fill=Time,col="blue"))+
  geom_point(show.legend = FALSE)+
  labs(subtitle = "Aproveitamento dos times 'regulares' no Campeonato Brasileiro de 2016", 
       x= "Pontos", 
       y= "Times", 
       title = "Tabela de Dados")

##

dt2016 %>% select("Time")
fraco <- dt2016 %>% filter(Pontos<=49)

fraco %>% ggplot(aes(Pontos, Time,fill=Time,col="blue"))+
  geom_point(show.legend = FALSE)+
  labs(subtitle = "Aproveitamento dos times 'fracos' no Campeonato Brasileiro de 2016", 
       x= "Pontos", 
       y= "Times", 
       title = "Tabela de Dados")

##

vitorioso <- dt2016 [c(1:20), c(2,5)]
vitorioso %>% ggplot(aes(Vitorias, Time,fill=Time))+
  geom_col(show.legend = TRUE)+
  scale_color_gradient(low = "blue", high = "red") +
  theme(legend.position = "bottom") +
  theme(panel.background = element_rect(fill = "white", colour = "black")) +
  theme(panel.grid.major = element_line(colour = "Black", linetype = "solid")) +
  scale_fill_viridis_d()+
  labs(subtitle = "Vitorias dos times no Campeonato Brasileiro de 2016", 
       y= "Times", 
       x= "Vitorias", 
       title = "Tabela de Dados")  

##

derrotado <- dt2016 [c(1:20), c(2,7)]  
derrotado %>% ggplot(aes(Derrotas, Time,fill=Time))+
  geom_col(show.legend = TRUE)+
  theme(legend.position = "bottom") +
  theme(panel.background = element_rect(fill = "white", colour = "grey10")) +
  theme(panel.grid.major = element_line(colour = "gray", linetype = "solid")) +
  labs(subtitle = "Derrotas dos times no Campeonato Brasileiro de 2016", 
       y= "Times", 
       x= "Derrotas", 
       title = "Tabela de Dados")

## 

empatado <- dt2016 [c(1:20), c(2,6)]  
empatado %>% ggplot(aes(Empates, Time,fill=Time))+
  geom_col(show.legend = TRUE)+
  theme(legend.position = "bottom") +
  theme(panel.background = element_rect(fill = "white", colour = "grey10")) +
  theme(panel.grid.major = element_line(colour = "gray", linetype = "solid")) +
  labs(subtitle = "Empates dos times no Campeonato Brasileiro de 2016", 
       y= "Times", 
       x= "Empates", 
       title = "Tabela de Dados")

##

dt2016$Aproveitamento <- round(dt2016$Aproveitamento,digits = 3)

aprov <- dt2016 [c(1:20), c(2,11)]  
aprov %>% ggplot(aes(Aproveitamento, Time, fill=Time))+
  geom_col(show.legend = TRUE)+
  scale_color_gradient(low = "blue", high = "red") +
  theme(legend.position = "bottom") +
  theme(panel.background = element_rect(fill = "white", colour = "grey10")) +
  theme(panel.grid.major = element_line(colour = "gray", linetype = "solid")) +
  scale_fill_manual(values = c("salmon","green","blue","orange","black","brown","orange", "yellow","blue","green","brown","brown","red","yellow","yellow","yellow", "yellow","pink", "brown","black")) +
  labs(subtitle = "Aproveitamento dos times no Campeonato Brasileiro de 2016", 
       x= "Aproveitamento", 
       y= "Times", 
       title = "Tabela de Dados")






media <- dt2016 %>% filter(Pontos<=60) %>% summarise(mean(Pontos))
mediatotal <- dt2016 %>% summarise(mean(Pontos))
mediavitorias <- dt2016 %>% summarise(mean(Vitorias))
mediaderrotas <- dt2016 %>% summarise(mean(Derrotas))
mediaempates <- dt2016 %>% summarise(mean(Empates))
mediaaprov <- dt2016 %>% summarise(mean(Aproveitamento))

##  

cruzeiroeatleticomg <- dt2016 [c(4,12), c(2:11)]   
y <- cruzeiroeatleticomg$Time
x <- cruzeiroeatleticomg$Pontos
cruzeiroeatleticomg %>%  ggplot(aes(Pontos, Time, fill=Time)) +
  geom_col(show.legend = TRUE) +
  scale_color_gradient(low = "blue", high = "red") +
  theme(legend.position = "bottom") +
  theme(panel.background = element_rect(fill = "white", colour = "grey10")) +
  theme(panel.grid.major = element_line(colour = "gray", linetype = "solid")) +
  scale_fill_manual(values = c("salmon","green")) +
  labs(subtitle = "Aproveitamento dos times mineiros no Campeonato Brasileiro de 2016", 
       x= "Pontos", 
       y= "Times", 
       title = "Tabela de Dados")

