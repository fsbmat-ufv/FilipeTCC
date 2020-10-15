rm(list = ls())
#para certificar-se de que você tenha um ambiente limpo de R
graphics.off()
#fechando todos os dispositivos abertos
cat("\014")
#limpar a tela

# chamar um pacote do R
library(openxlsx)
#ajudam a ler dados de planilhas Excel 
library(ggplot2) #pacote do R voltado para a criação de gráficos estatísticos
library(ggalt) #fazer faixas de confiança para gráficos do tipo escada
library(stringr)
library(tidyverse)
library("car")
library(pdftools)
library(scatterplot3d)
library(data.table)

#  dt2017 <- read.xlsx("C:/Users/Filipe Fulgêncio/Documents/github/FilipeTCC/Brasileiro2017.xlsx", colNames = TRUE, startRow = 3, sheet = 1, rows = seq(3:25))
# names(dt2017)<- c("Classificacao" ,
#                   "Time"          ,
#                   "Pontos"        ,
#                   "Jogos"         ,
#                   "Vitorias"      ,
#                   "Empates"       ,
#                   "Derrotas"      ,
#                   "GolsPro"      ,
#                   "GolsContra"   ,
#                   "SaldoGols" ,
#                   "Aproveitamento")
# 
# dt2017$Time <- abjutils::rm_accent(dt2017$Time)
# dt2017$Time <- str_squish(dt2017$Time)
# dt2017$Time <- str_trim(dt2017$Time, side = c("both", "left", "right"))
# dt2017$Time <- str_to_upper(dt2017$Time)
# saveRDS(dt2017, file = "dt2017.Rds")
dt2017 <- readRDS("dt2017.Rds")
uf <- fread("Estado.csv")
uf$Time <- str_to_upper(dt2017$Time)
dt2017 <- left_join(dt2017,uf,by="Time")

dt2017 %>% ggplot(aes(Pontos, Time,fill=Time,col="blue"))+
  geom_col(show.legend = FALSE)+
  labs(subtitle = "Campanha dos times no Campeonato Brasileiro de 2017", 
       x= "Pontos", 
       y= "Times", 
       title = "Tabela de Dados")

##

dt2017 %>% select("Time")
bom <- dt2017 %>% filter(Pontos>=60)
 
bom %>% ggplot(aes(Pontos, Time,fill=Time,col="blue"))+
  geom_point(show.legend = TRUE)+
  scale_fill_manual(values = c("salmon","green","blue","orange")) +
    labs(subtitle = "Aproveitamento dos times 'bons' no Campeonato Brasileiro de 2017", 
       x= "Pontos", 
       y= "Times", 
       title = "Tabela de Dados")

##

dt2017 %>% select("Time")
regular <- dt2017 %>% filter(Pontos>=49 & Pontos<=60)

regular %>% ggplot(aes(Pontos, Time,fill=Time,col="blue"))+
  geom_point(show.legend = FALSE)+
  labs(subtitle = "Aproveitamento dos times 'regulares' no Campeonato Brasileiro de 2017", 
       x= "Pontos", 
       y= "Times", 
       title = "Tabela de Dados")

##

dt2017 %>% select("Time")
fraco <- dt2017 %>% filter(Pontos<=49)

fraco %>% ggplot(aes(Pontos, Time,fill=Time,col="blue"))+
  geom_point(show.legend = FALSE)+
  labs(subtitle = "Aproveitamento dos times 'fracos' no Campeonato Brasileiro de 2017", 
       x= "Pontos", 
       y= "Times", 
       title = "Tabela de Dados")

##

vitorioso <- dt2017 [c(1:20), c(2,5)]
vitorioso %>% ggplot(aes(Vitorias, Time,fill=Time))+
  geom_col(show.legend = TRUE)+
  scale_color_gradient(low = "blue", high = "red") +
  theme(legend.position = "bottom") +
  theme(panel.background = element_rect(fill = "white", colour = "black")) +
  theme(panel.grid.major = element_line(colour = "Black", linetype = "solid")) +
  scale_fill_viridis_d()+
  labs(subtitle = "Vitorias dos times no Campeonato Brasileiro de 2017", 
         y= "Times", 
         x= "Vitorias", 
         title = "Tabela de Dados")  
 
##
  
derrotado <- dt2017 [c(1:20), c(2,7)]  
derrotado %>% ggplot(aes(Derrotas, Time,fill=Time))+
  geom_col(show.legend = TRUE)+
  theme(legend.position = "bottom") +
  theme(panel.background = element_rect(fill = "white", colour = "grey10")) +
  theme(panel.grid.major = element_line(colour = "gray", linetype = "solid")) +
  labs(subtitle = "Derrotas dos times no Campeonato Brasileiro de 2017", 
       y= "Times", 
       x= "Derrotas", 
       title = "Tabela de Dados")
  
## 

empatado <- dt2017 [c(1:20), c(2,6)]  
empatado %>% ggplot(aes(Empates, Time,fill=Time))+
  geom_col(show.legend = TRUE)+
  theme(legend.position = "bottom") +
  theme(panel.background = element_rect(fill = "white", colour = "grey10")) +
  theme(panel.grid.major = element_line(colour = "gray", linetype = "solid")) +
  labs(subtitle = "Empates dos times no Campeonato Brasileiro de 2017", 
       y= "Times", 
       x= "Empates", 
       title = "Tabela de Dados")

##

dt2017$Aproveitamento <- round(dt2017$Aproveitamento,digits = 3)

aprov <- dt2017 [c(1:20), c(2,11)]  
dt2017 %>% ggplot(aes(Aproveitamento, Time, fill=Estado))+
  geom_col(show.legend = TRUE)+
  scale_color_gradient(low = "blue", high = "red") +
    theme(legend.position = "bottom") +
  theme(panel.background = element_rect(fill = "white", colour = "grey10")) +
  theme(panel.grid.major = element_line(colour = "gray", linetype = "solid")) +
  scale_fill_manual(values = c("salmon","green","blue","orange","black","brown","orange", "yellow","blue","green","brown","brown","red","yellow","yellow","yellow", "yellow","pink", "brown","black")) +
    labs(subtitle = "Aproveitamento dos times no Campeonato Brasileiro de 2017", 
        x= "Aproveitamento", 
        y= "Times", 
        title = "Tabela de Dados")





  
  media <- dt2017 %>% filter(Pontos<=60) %>% summarise(mean(Pontos))
  mediatotal <- dt2017 %>% summarise(mean(Pontos))
  mediavitorias <- dt2017 %>% summarise(mean(Vitorias))
  mediaderrotas <- dt2017 %>% summarise(mean(Derrotas))
  mediaempates <- dt2017 %>% summarise(mean(Empates))
  mediaaprov <- dt2017 %>% summarise(mean(Aproveitamento))

##  

crXat <- dt2017 %>% filter(Time %in% c("CRUZEIRO","ATLETICO-MG"))
y <- crXat$Time
x <- crXat$Pontos
crXat %>%  ggplot(aes(Pontos, Time, fill=Time)) +
  geom_col(show.legend = TRUE) +
  scale_color_gradient(low = "blue", high = "red") +
  theme(legend.position = "bottom") +
  theme(panel.background = element_rect(fill = "white", colour = "grey10")) +
  theme(panel.grid.major = element_line(colour = "gray", linetype = "solid")) +
  scale_fill_manual(values = c("salmon","green")) +
  labs(subtitle = "Aproveitamento dos times mineiros no Campeonato Brasileiro de 2017", 
       x= "Pontos", 
       y= "Times", 
       title = "Tabela de Dados")



 

##
  
scatterplot(Vitorias ~ Pontos, data = dt2017)    
   
x <- dt2017$Pontos
y <- dt2017$Aproveitamento
z <- dt2017$Vitorias

scatterplot3d(x, y, z, pch = 16, grid = TRUE,box = TRUE,
              xlab = "Pontos", ylab = "Aproveitamento", zlab = "Vitorias")+
  geom_col(show.legend = TRUE) +
  scale_fill_manual(values = c("salmon","green","blue","orange","black","brown","orange", "yellow","blue","green","brown","brown","red","yellow","yellow","yellow", "yellow","pink", "brown","black"))
 
     

# csv, txt, Rds             
# pacote stringr                            



