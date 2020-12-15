rm(list = ls())
graphics.off()
cat("\014")


library(tidyverse)
library(lubridate)
library(plotly)
library(devtools)



#saveRDS(dt2000, file = "dt2000.Rds")

dados <- readRDS("dados.rds")

###########################Grafico 1##########################

#v2003 <- 
#  dados %>% filter(Ano=="2003", Vencedor!="EMPATE") %>% 
#  group_by(Vencedor) %>% summarise(Quant=n(),.groups="drop") %>% 
#  ggplot(aes(Quant,reorder(Vencedor,Quant),fill=Vencedor, text=paste("Núm. de Vitórias =", Quant, "<br>",
#                                                                     "Time= ", Vencedor)))+
#  geom_col(show.legend = FALSE)+
#  theme(panel.background = element_rect(fill = "white", colour = "black")) +
#  theme(panel.grid.major = element_line(colour = "Black", linetype = "solid")) + 
#  xlab("Quantidades de Vitórias")+
#  ylab("Times que disputaram o Campeonato")+
#  geom_text(aes(label=Quant),nudge_x = 1)+
#  theme_bw()+
#  ggtitle("Número de vitórias no Brasileirão 2003")+
#  geom_point()
#vv2003 <- ggplotly(v2003, tooltip = "text") %>% layout(showlegend = FALSE) %>% style(textposition = "top")
#htmlwidgets::saveWidget(as_widget(vv2003), "vitorias.html")



#head(dados)
#ar <- dados %>%
#  group_by(Arena) %>% summarise(Quant=n(),.groups="drop")
#ar %>% select("Arena")
#bom <- ar %>% filter(Quant>255)

#bom %>%  ggplot(aes(Quant,reorder(Arena,Quant),fill=Arena, text=paste("Núm. de Vitórias =", Quant, "<br>",
#                                                                      "Time= ", Arena)))+
#  geom_col(show.legend = FALSE)+
#  theme(panel.background = element_rect(fill = "white", colour = "black")) +
#  theme(panel.grid.major = element_line(colour = "Black", linetype = "solid")) + 
#  xlab("Número de jogos nos estádios")+
#  ylab("Nome dos estádios")+
#  theme_bw()+
#  geom_text(aes(label=Quant),nudge_x = 17)+
#  ggtitle("Os estádios com mais jogos no Brasileirão")
#ggplotly(bom, tooltip = "text") %>% layout(showlegend = FALSE) %>% style(textposition = "top")



########################### GERAL ##########################


########################### ARENA T ##########################

head(dados)
ar <- dados %>% 
  #filter(Ano=="2019")%>% 
  group_by(Arena) %>% summarise(Quant=n(),.groups="drop")
ar %>%  ggplot(aes(Quant,reorder(Arena,Quant),fill=Arena))+
  geom_col(show.legend = FALSE)+
  theme(panel.background = element_rect(fill = "white", colour = "black")) +
  theme(panel.grid.major = element_line(colour = "Black", linetype = "solid")) + 
  xlab("Número de jogos nos estádios")+
  ylab("Nome dos estádios")+
  theme_bw()+
  geom_text(aes(label=Quant),nudge_x = 1)+
  ggtitle("Quantidade de jogos totais em cada estádio nas edições do Brasileirão")

########################### ARENA + ##########################

head(dados)
ar <- dados %>%
  group_by(Arena) %>% summarise(Quant=n(),.groups="drop")
ar %>% select("Arena")
bom <- ar %>% filter(Quant>255)

bom %>%  ggplot(aes(Quant,reorder(Arena,Quant),fill=Arena))+
  geom_col(show.legend = FALSE)+
  theme(panel.background = element_rect(fill = "white", colour = "black")) +
  theme(panel.grid.major = element_line(colour = "Black", linetype = "solid")) + 
  xlab("Número de jogos nos estádios")+
  ylab("Nome dos estádios")+
  theme_bw()+
  geom_text(aes(label=Quant),nudge_x = 17)+
  ggtitle("Os estádios com mais jogos no Brasileirão")

########################### ARENA - ##########################

head(dados)
ar <- dados %>%
  group_by(Arena) %>% summarise(Quant=n(),.groups="drop")
ar %>% select("Arena")
regular <- ar %>% filter(Quant<=5)

regular %>%  ggplot(aes(Quant,reorder(Arena,Quant),fill=Arena))+
  geom_col(show.legend = FALSE)+
  theme(panel.background = element_rect(fill = "white", colour = "black")) +
  theme(panel.grid.major = element_line(colour = "Black", linetype = "solid")) + 
  xlab("Número de jogos nos estádios")+
  ylab("Nome dos stádios")+
  theme_bw()+
  geom_text(aes(label=Quant),nudge_x = 0.2)+
  ggtitle("Os estádios com menos jogos no Brasileirão ")

########################### GOLS M ##########################

mandanteNG <- dados %>%  
  #filter(Ano=="2019")%>% 
  group_by(Mandante) %>% summarise(golsM=sum(GolsMan))
names(mandanteNG) <- c("Time","GolsM")

visitanteNG <- dados %>% 
  #filter(Ano=="2019")%>% 
  group_by(Visitante) %>% summarise(golsM=sum(GolsVisit))
names(visitanteNG) <- c("Time","GolsV")


totGols <- left_join(mandanteNG,visitanteNG,by="Time")
totGols$Total <- rowSums(totGols[,2:3])
totGols %>%  ggplot(aes(GolsM,reorder(Time,GolsM),fill=GolsM))+
  geom_col(show.legend = FALSE)+
  theme(panel.background = element_rect(fill = "white", colour = "black")) +
  theme(panel.grid.major = element_line(colour = "Black", linetype = "solid")) + 
  xlab("Número de gols no Brasileirão como mandante")+
  ylab("Times que disputaram o Brasileirão")+
  geom_text(aes(label=GolsM),nudge_x = 15)+
  theme_bw()+
  ggtitle("Quantidade de gols dos times como mandante no Brasileirão")

########################### GOLS V ##########################

totGols %>%  ggplot(aes(GolsV,reorder(Time,GolsV),fill=GolsV))+
  geom_col(show.legend = FALSE)+
  theme(panel.background = element_rect(fill = "white", colour = "black")) +
  theme(panel.grid.major = element_line(colour = "Black", linetype = "solid")) + 
  xlab("Número de gols no Brasileirão como visitante")+
  ylab("Times que disputaram o Brasileirão")+
  geom_text(aes(label=GolsV),nudge_x = 10)+
  theme_bw()+
  ggtitle("Quantidade de gols dos times como visitantes no Brasileirão")

########################### GOLS T ##########################

totGols %>%  ggplot(aes(Total,reorder(Time,Total),fill=Total))+
  geom_col(show.legend = FALSE)+
  theme(panel.background = element_rect(fill = "white", colour = "black")) +
  theme(panel.grid.major = element_line(colour = "Black", linetype = "solid")) + 
  xlab("Número de gols no Brasileirão")+
  ylab("Times que disputaram o Brasileirão")+
  geom_text(aes(label=Total),nudge_x = 27)+
  theme_bw()+
  ggtitle("Quantidade total de gols dos times nas edições do Brasileirão")

########################### VIT M ##########################

dados$vitMan <- ifelse(dados$Mandante==dados$Vencedor,1,0)
testeM <- dados %>% filter(Vencedor!="EMPATE") %>% 
  #filter(Ano=="2019",Vencedor!="EMPATE")%>% 
  group_by(Mandante) %>% summarise(vitM=sum(vitMan))
names(testeM) <- c("Time","VitM")
testeM %>%  ggplot(aes(VitM,reorder(Time,VitM),fill=VitM))+
  geom_col(show.legend = FALSE)+
  theme(panel.background = element_rect(fill = "white", colour = "black")) +
  theme(panel.grid.major = element_line(colour = "Black", linetype = "solid")) + 
  xlab("Número de vitórias como mandante no Brasileirão")+
  ylab("Times que disputaram o Brasileirão")+
  geom_text(aes(label=VitM),nudge_x = 4)+
  theme_bw()+
  ggtitle("Quantidades de vitórias como mandante no Brasileirão")

########################### VIT V ##########################

dados$vitVist <- ifelse(dados$Visitante==dados$Vencedor,1,0)
testeV <- dados %>% filter(Vencedor!="EMPATE") %>% 
  #filter(Ano=="2019")%>% 
  group_by(Visitante) %>% summarise(vitV=sum(vitVist))
names(testeV) <- c("Time","VitV")
testeV %>%  ggplot(aes(VitV,reorder(Time,VitV),fill=VitV))+
  geom_col(show.legend = FALSE)+
  theme(panel.background = element_rect(fill = "white", colour = "black")) +
  theme(panel.grid.major = element_line(colour = "Black", linetype = "solid")) + 
  xlab("Número de vitórias como visitante no Brasileirão")+
  ylab("Times que disputaram o Brasileirão")+
  geom_text(aes(label=VitV),nudge_x = 3)+
  theme_bw()+
  ggtitle("Quantidades de vitórias como visitante no Brasileirão")

########################### VIT T ##########################

head(dados)
vitot <- dados %>% filter(Vencedor!="EMPATE") %>% 
  #filter(Ano=="2019",Vencedor!="EMPATE")%>%
  group_by(Vencedor) %>% summarise(Quant=n(),.groups="drop")
vitot %>%  ggplot(aes(Quant,reorder(Vencedor,Quant),fill=Vencedor))+
  geom_col(show.legend = FALSE)+
  theme(panel.background = element_rect(fill = "white", colour = "black")) +
  theme(panel.grid.major = element_line(colour = "Black", linetype = "solid")) + 
  xlab("Número de vitórias gerais")+
  ylab("Times que disputaram o Brasileirão")+
  theme_bw()+
  geom_text(aes(label=Quant),nudge_x = 7)+
  ggtitle("Quantidade total de vitórias dos times nas edições do Brasileirão")

########################### DER M ##########################

dados$derMand <- ifelse(dados$Mandante==dados$Derrotado,1,0)
testederM <- dados %>% filter(Derrotado!="EMPATE")%>%
  #filter(Ano=="2019",Derrotado!="EMPATE")%>% 
  group_by(Mandante) %>% summarise(dm=sum(derMand))
names(testederM) <- c("Time","DerM")

testederM %>%  ggplot(aes(DerM,reorder(Time,DerM),fill=DerM))+
  geom_col(show.legend = FALSE)+
  theme(panel.background = element_rect(fill = "white", colour = "black")) +
  theme(panel.grid.major = element_line(colour = "Black", linetype = "solid")) + 
  xlab("Número de derrotas como mandante no Brasileirão")+
  ylab("Times que disputaram o Brasileirão")+
  geom_text(aes(label=DerM),nudge_x = 1.5)+
  theme_bw()+
  ggtitle("Quantidades de derrotas como mandante no Brasileirão")

########################### DER V ##########################

dados$derVist <- ifelse(dados$Visitante==dados$Derrotado,1,0)
testederV <- dados %>% filter(Derrotado!="EMPATE")%>%  
  #filter(Ano=="2019",Derrotado!="EMPATE")%>%  
  group_by(Visitante) %>% summarise(dV=sum(derVist))
names(testederV) <- c("Time","DerV")

testederV %>%  ggplot(aes(DerV,reorder(Time,DerV),fill=DerV))+
  geom_col(show.legend = FALSE)+
  theme(panel.background = element_rect(fill = "white", colour = "black")) +
  theme(panel.grid.major = element_line(colour = "Black", linetype = "solid")) + 
  xlab("Número de derrotas como visitante no Brasileirão")+
  ylab("Times que disputaram o Brasileirão")+
  geom_text(aes(label=DerV),nudge_x = 3.5)+
  theme_bw()+
  ggtitle("Quantidades de derrotas como visitante no Brasileirão")

########################### DER T ##########################

head(dados)
derotpt <- dados %>% filter(Derrotado!="EMPATE")%>%
  #filter(Ano=="2019",Derrotado!="EMPATE")%>%
  group_by(Derrotado) %>% summarise(Quant=n(),.groups="drop")
derotpt %>%  ggplot(aes(Quant,reorder(Derrotado,Quant),fill=Derrotado))+
  geom_col(show.legend = FALSE)+
  theme(panel.background = element_rect(fill = "white", colour = "black")) +
  theme(panel.grid.major = element_line(colour = "Black", linetype = "solid")) + 
  xlab("Número de derrotas gerais")+
  ylab("Times que disputaram o Brasileirão")+
  theme_bw()+
  geom_text(aes(label=Quant),nudge_x = 5.5)+
  ggtitle("Quantidade total de derrotas dos times nas edições do Brasileirão")

########################### PTS ##########################

ptM <- dados %>% 
  #filter(Ano=="2003")%>% 
  group_by(Mandante) %>% summarise(PontMan=sum(PontMandante))
ptM %>%  ggplot(aes(PontMan,reorder(Mandante,PontMan),fill=PontMan))+
  geom_col(show.legend = FALSE)+
  theme(panel.background = element_rect(fill = "white", colour = "black")) +
  theme(panel.grid.major = element_line(colour = "Black", linetype = "solid")) + 
  xlab("Número de pontos dos times como mandante")+
  ylab("Times que disputaram o Brasileirão")+
  geom_text(aes(label=PontMan),nudge_x = 15)+
  theme_bw()+
  ggtitle("Quantidades de pontos dos times como mandante no Brasileirão")

ptV <- dados %>% 
  #filter(Ano=="2003")%>% 
  group_by(Visitante) %>% summarise(PontVis=sum(PontVisitante))
ptV %>%  ggplot(aes(PontVis,reorder(Visitante,PontVis),fill=PontVis))+
  geom_col(show.legend = FALSE)+
  theme(panel.background = element_rect(fill = "white", colour = "black")) +
  theme(panel.grid.major = element_line(colour = "Black", linetype = "solid")) + 
  xlab("Número de pontos dos times como visitantes")+
  ylab("Times que disputaram o Brasileirão")+
  geom_text(aes(label=PontVis),nudge_x = 10)+
  theme_bw()+
  ggtitle("Quantidades de pontos dos times como visitante no Brasileirão")


ptfinal <- data.frame(ptM[,1:2],ptV[,2])

ptfinal$Pontos <- rowSums(ptfinal[,2:3])

ptff <- ptfinal %>%
  distinct(Mandante,Pontos)
 
ptfinal %>%  ggplot(aes(Pontos,reorder(Mandante,Pontos),fill=Pontos))+
  geom_col(show.legend = FALSE)+
  theme(panel.background = element_rect(fill = "white", colour = "black")) +
  theme(panel.grid.major = element_line(colour = "Black", linetype = "solid")) + 
  xlab("Número de pontos dos times")+
  ylab("Times que disputaram o Brasileirão")+
  theme_bw()+
  geom_text(aes(label=Pontos),nudge_x = 30)+
  ggtitle("Pontuação final no Brasileirão")


########################### EMPT M ##########################

head(dados)
empate <- dados %>% filter(Vencedor=="EMPATE") %>%
  filter(Ano=="2019",Vencedor=="EMPATE")%>%
  group_by(Mandante,Visitante,Vencedor) %>% summarise(Quant=n(),.groups="drop")

empate$emm <- ifelse(empate$Mandante!=empate$Vencedor,1,0)
emm <- empate %>%  
  group_by(Mandante) %>% summarise(eee=sum(emm))
names(emm) <- c("Time","EmpM")

emm %>%  ggplot(aes(EmpM,reorder(Time,EmpM),fill=EmpM))+
  geom_col(show.legend = FALSE)+
  theme(panel.background = element_rect(fill = "white", colour = "black")) +
  theme(panel.grid.major = element_line(colour = "Black", linetype = "solid")) + 
  xlab("Número de empates como mandante no Brasileirão")+
  ylab("Times que disputaram o Brasileirão")+
  geom_text(aes(label=EmpM),nudge_x = 1)+
  theme_bw()+
  ggtitle("Quantidades de empates como mandante no Brasileirão")

########################### EMPT V ##########################

empate$emv <- ifelse(empate$Visitante!=empate$Vencedor,1,0)
emv <- empate %>%  
  group_by(Visitante) %>% summarise(ee=sum(emv))
names(emv) <- c("Time","EmpV")

emv %>%  ggplot(aes(EmpV,reorder(Time,EmpV),fill=EmpV))+
  geom_col(show.legend = FALSE)+
  theme(panel.background = element_rect(fill = "white", colour = "black")) +
  theme(panel.grid.major = element_line(colour = "Black", linetype = "solid")) + 
  xlab("Número de empates como visitante no Brasileirão")+
  ylab("Times que disputaram o Brasileirão")+
  geom_text(aes(label=EmpV),nudge_x = 1)+
  theme_bw()+
  ggtitle("Quantidades de empates como visitante no Brasileirão")

########################### EMPT T ##########################
totEmpate <- left_join(emv,emm,by="Time")
totEmpate$Total <- rowSums(totEmpate[,2:3])
#head(dados)
#totEmpate <- dados  %>% 
  #filter(Ano=="2019") %>% 
  #group_by(Time) %>% summarise(Quant=n(),.groups="drop")

totEmpate %>%  ggplot(aes(Total,reorder(Time,Total),fill=Total))+
  geom_col(show.legend = FALSE)+
  theme(panel.background = element_rect(fill = "white", colour = "black")) +
  theme(panel.grid.major = element_line(colour = "Black", linetype = "solid")) + 
  xlab("Número de empates gerais")+
  ylab("Times que disputaram o Brasileirão")+
  theme_bw()+
  geom_text(aes(label=Total),nudge_x = 1)+
  ggtitle("Quantidade total de empates dos times nas edições do Brasileirão")

########################### JOGOS M ##########################

head(dados)
jogosM <- dados %>% 
  #filter(Ano=="2019")%>% 
  group_by(Mandante) %>% summarise(Quant=n(),.groups="drop")
names(jogosM) <- c("Time","JogosM")
jogosM %>%  ggplot(aes(JogosM,reorder(Time,JogosM),fill=Time))+
  geom_col(show.legend = FALSE)+
  theme(panel.background = element_rect(fill = "white", colour = "black")) +
  theme(panel.grid.major = element_line(colour = "Black", linetype = "solid")) + 
  xlab("Número de jogos como mandante")+
  ylab("Equipes mandantes")+
  theme_bw()+
  geom_text(aes(label=JogosM),nudge_x = 7)+
  ggtitle("Quantidade de jogos dos times como mandante")

########################### JOGOS V ##########################

head(dados)
jogosV <- dados %>% 
  #filter(Ano=="2019")%>% 
  group_by(Visitante) %>% summarise(Quant=n(),.groups="drop")
names(jogosV) <- c("Time","JogosV")
jogosV %>%  ggplot(aes(JogosV,reorder(Time,JogosV),fill=Time))+
  geom_col(show.legend = FALSE)+
  theme(panel.background = element_rect(fill = "white", colour = "black")) +
  theme(panel.grid.major = element_line(colour = "Black", linetype = "solid")) + 
  xlab("Número de jogos como visitante")+
  ylab("Equipes mandantes")+
  theme_bw()+
  geom_text(aes(label=JogosV),nudge_x = 8)+
  ggtitle("Quantidade de jogos dos times como visitante")

########################### JOGOS T ##########################

totJogos <- left_join(jogosM,jogosV,by="Time")
totJogos$Total <- rowSums(totJogos[,2:3])

totJogos %>%  ggplot(aes(Total,reorder(Time,Total),fill=Total))+
  geom_col(show.legend = FALSE)+
  theme(panel.background = element_rect(fill = "white", colour = "black")) +
  theme(panel.grid.major = element_line(colour = "Black", linetype = "solid")) + 
  xlab("Número de jogos gerais")+
  ylab("Times que disputaram o Brasileirão")+
  theme_classic()+
  geom_text(aes(label=Total),nudge_x = 15)+
  ggtitle("Quantidade total de jogos dos times nas edições do Brasileirão")









########################### POR ANO ##########################


#############################################################





########################### ARENA T ##########################

head(dados)
ar <- dados %>% 
  filter(Ano=="2019")%>% 
  group_by(Arena) %>% summarise(Quant=n(),.groups="drop")
ar %>%  ggplot(aes(Quant,reorder(Arena,Quant),fill=Arena))+
  geom_col(show.legend = FALSE)+
  theme(panel.background = element_rect(fill = "white", colour = "black")) +
  theme(panel.grid.major = element_line(colour = "Black", linetype = "solid")) + 
  xlab("Número de jogos nos estádios")+
  ylab("Nome dos estádios")+
  theme_bw()+
  geom_text(aes(label=Quant),nudge_x = 1)+
  ggtitle("Quantidade de jogos totais em cada estádio nas edições do Brasileirão")

########################### ARENA + ##########################

head(dados)
ar <- dados %>%
  group_by(Arena) %>% summarise(Quant=n(),.groups="drop")
ar %>% select("Arena")
bom <- ar %>% filter(Quant>255)

bom %>%  ggplot(aes(Quant,reorder(Arena,Quant),fill=Arena))+
  geom_col(show.legend = FALSE)+
  theme(panel.background = element_rect(fill = "white", colour = "black")) +
  theme(panel.grid.major = element_line(colour = "Black", linetype = "solid")) + 
  xlab("Número de jogos nos estádios")+
  ylab("Nome dos estádios")+
  theme_bw()+
  geom_text(aes(label=Quant),nudge_x = 17)+
  ggtitle("Os estádios com mais jogos no Brasileirão")

########################### ARENA - ##########################

head(dados)
ar <- dados %>%
  group_by(Arena) %>% summarise(Quant=n(),.groups="drop")
ar %>% select("Arena")
regular <- ar %>% filter(Quant<=5)

regular %>%  ggplot(aes(Quant,reorder(Arena,Quant),fill=Arena))+
  geom_col(show.legend = FALSE)+
  theme(panel.background = element_rect(fill = "white", colour = "black")) +
  theme(panel.grid.major = element_line(colour = "Black", linetype = "solid")) + 
  xlab("Número de jogos nos estádios")+
  ylab("Nome dos stádios")+
  theme_bw()+
  geom_text(aes(label=Quant),nudge_x = 0.2)+
  ggtitle("Os estádios com menos jogos no Brasileirão ")

########################### GOLS M ##########################

mandanteNG <- dados %>%  
  filter(Ano=="2019")%>% 
  group_by(Mandante) %>% summarise(golsM=sum(GolsMan))
names(mandanteNG) <- c("Time","GolsM")

visitanteNG <- dados %>% 
  filter(Ano=="2019")%>% 
  group_by(Visitante) %>% summarise(golsM=sum(GolsVisit))
names(visitanteNG) <- c("Time","GolsV")


totGols <- left_join(mandanteNG,visitanteNG,by="Time")
totGols$Total <- rowSums(totGols[,2:3])
totGols %>%  ggplot(aes(GolsM,reorder(Time,GolsM),fill=GolsM))+
  geom_col(show.legend = FALSE)+
  theme(panel.background = element_rect(fill = "white", colour = "black")) +
  theme(panel.grid.major = element_line(colour = "Black", linetype = "solid")) + 
  xlab("Número de gols no Brasileirão como mandante")+
  ylab("Times que disputaram o Brasileirão")+
  geom_text(aes(label=GolsM),nudge_x = 1)+
  theme_bw()+
  ggtitle("Quantidade de gols dos times como mandante no Brasileirão")

########################### GOLS V ##########################

totGols %>%  ggplot(aes(GolsV,reorder(Time,GolsV),fill=GolsV))+
  geom_col(show.legend = FALSE)+
  theme(panel.background = element_rect(fill = "white", colour = "black")) +
  theme(panel.grid.major = element_line(colour = "Black", linetype = "solid")) + 
  xlab("Número de gols no Brasileirão como visitante")+
  ylab("Times que disputaram o Brasileirão")+
  geom_text(aes(label=GolsV),nudge_x = 1)+
  theme_bw()+
  ggtitle("Quantidade de gols dos times como visitantes no Brasileirão")

########################### GOLS T ##########################

totGols %>%  ggplot(aes(Total,reorder(Time,Total),fill=Total))+
  geom_col(show.legend = FALSE)+
  theme(panel.background = element_rect(fill = "white", colour = "black")) +
  theme(panel.grid.major = element_line(colour = "Black", linetype = "solid")) + 
  xlab("Número de gols no Brasileirão")+
  ylab("Times que disputaram o Brasileirão")+
  geom_text(aes(label=Total),nudge_x = 2)+
  theme_bw()+
  ggtitle("Quantidade total de gols dos times nas edições do Brasileirão")

########################### VIT M ##########################

dados$vitMan <- ifelse(dados$Mandante==dados$Vencedor,1,0)
testeM <- dados %>% filter(Ano=="2019",Vencedor!="EMPATE") %>% 
  #filter(Ano=="2019",Vencedor!="EMPATE")%>% 
  group_by(Mandante) %>% summarise(vitM=sum(vitMan))
names(testeM) <- c("Time","VitM")
testeM %>%  ggplot(aes(VitM,reorder(Time,VitM),fill=VitM))+
  geom_col(show.legend = FALSE)+
  theme(panel.background = element_rect(fill = "white", colour = "black")) +
  theme(panel.grid.major = element_line(colour = "Black", linetype = "solid")) + 
  xlab("Número de vitórias como mandante no Brasileirão")+
  ylab("Times que disputaram o Brasileirão")+
  geom_text(aes(label=VitM),nudge_x = 0.2)+
  theme_bw()+
  ggtitle("Quantidades de vitórias como mandante no Brasileirão")

########################### VIT V ##########################

dados$vitVist <- ifelse(dados$Visitante==dados$Vencedor,1,0)
testeV <- dados %>% filter(Ano=="2019",Vencedor!="EMPATE") %>% 
  #filter(Ano=="2019")%>% 
  group_by(Visitante) %>% summarise(vitV=sum(vitVist))
names(testeV) <- c("Time","VitV")
testeV %>%  ggplot(aes(VitV,reorder(Time,VitV),fill=VitV))+
  geom_col(show.legend = FALSE)+
  theme(panel.background = element_rect(fill = "white", colour = "black")) +
  theme(panel.grid.major = element_line(colour = "Black", linetype = "solid")) + 
  xlab("Número de vitórias como visitante no Brasileirão")+
  ylab("Times que disputaram o Brasileirão")+
  geom_text(aes(label=VitV),nudge_x = 0.2)+
  theme_bw()+
  ggtitle("Quantidades de vitórias como visitante no Brasileirão")

########################### VIT T ##########################

head(dados)
vitot <- dados %>% filter(Ano=="2019",Vencedor!="EMPATE") %>% 
  #filter(Ano=="2019",Vencedor!="EMPATE")%>%
  group_by(Vencedor) %>% summarise(Quant=n(),.groups="drop")
vitot %>%  ggplot(aes(Quant,reorder(Vencedor,Quant),fill=Vencedor))+
  geom_col(show.legend = FALSE)+
  theme(panel.background = element_rect(fill = "white", colour = "black")) +
  theme(panel.grid.major = element_line(colour = "Black", linetype = "solid")) + 
  xlab("Número de vitórias gerais")+
  ylab("Times que disputaram o Brasileirão")+
  theme_bw()+
  geom_text(aes(label=Quant),nudge_x = 1)+
  ggtitle("Quantidade total de vitórias dos times nas edições do Brasileirão")

########################### DER M ##########################

dados$derMand <- ifelse(dados$Mandante==dados$Derrotado,1,0)
testederM <- dados %>% filter(Ano=="2019",Derrotado!="EMPATE")%>%
  #filter(Ano=="2019",Derrotado!="EMPATE")%>% 
  group_by(Mandante) %>% summarise(dm=sum(derMand))
names(testederM) <- c("Time","DerM")

testederM %>%  ggplot(aes(DerM,reorder(Time,DerM),fill=DerM))+
  geom_col(show.legend = FALSE)+
  theme(panel.background = element_rect(fill = "white", colour = "black")) +
  theme(panel.grid.major = element_line(colour = "Black", linetype = "solid")) + 
  xlab("Número de derrotas como mandante no Brasileirão")+
  ylab("Times que disputaram o Brasileirão")+
  geom_text(aes(label=DerM),nudge_x = 0.2)+
  theme_bw()+
  ggtitle("Quantidades de derrotas como mandante no Brasileirão")

########################### DER V ##########################

dados$derVist <- ifelse(dados$Visitante==dados$Derrotado,1,0)
testederV <- dados %>% filter(Ano=="2019",Derrotado!="EMPATE")%>%  
  #filter(Ano=="2019",Derrotado!="EMPATE")%>%  
  group_by(Visitante) %>% summarise(dV=sum(derVist))
names(testederV) <- c("Time","DerV")

testederV %>%  ggplot(aes(DerV,reorder(Time,DerV),fill=DerV))+
  geom_col(show.legend = FALSE)+
  theme(panel.background = element_rect(fill = "white", colour = "black")) +
  theme(panel.grid.major = element_line(colour = "Black", linetype = "solid")) + 
  xlab("Número de derrotas como visitante no Brasileirão")+
  ylab("Times que disputaram o Brasileirão")+
  geom_text(aes(label=DerV),nudge_x = 0.2)+
  theme_bw()+
  ggtitle("Quantidades de derrotas como visitante no Brasileirão")

########################### DER T ##########################

head(dados)
derotpt <- dados %>% filter(Ano=="2019",Derrotado!="EMPATE")%>%
  #filter(Ano=="2019",Derrotado!="EMPATE")%>%
  group_by(Derrotado) %>% summarise(Quant=n(),.groups="drop")
derotpt %>%  ggplot(aes(Quant,reorder(Derrotado,Quant),fill=Derrotado))+
  geom_col(show.legend = FALSE)+
  theme(panel.background = element_rect(fill = "white", colour = "black")) +
  theme(panel.grid.major = element_line(colour = "Black", linetype = "solid")) + 
  xlab("Número de derrotas gerais")+
  ylab("Times que disputaram o Brasileirão")+
  theme_bw()+
  geom_text(aes(label=Quant),nudge_x = 1)+
  ggtitle("Quantidade total de derrotas dos times nas edições do Brasileirão")

########################### PTS ##########################

ptM <- dados %>% filter(Ano=="2019") %>% group_by(Mandante) %>% summarise(PontMan=sum(PontMandante))
ptM %>%  ggplot(aes(PontMan,reorder(Mandante,PontMan),fill=PontMan))+
  geom_col(show.legend = FALSE)+
  theme(panel.background = element_rect(fill = "white", colour = "black")) +
  theme(panel.grid.major = element_line(colour = "Black", linetype = "solid")) + 
  xlab("Número de pontos dos times como mandante")+
  ylab("Times que disputaram o Brasileirão")+
  geom_text(aes(label=PontMan),nudge_x = 1)+
  theme_bw()+
  ggtitle("Quantidades de pontos dos times como mandante no Brasileirão")

ptV <- dados %>% filter(Ano=="2019") %>% group_by(Visitante) %>% summarise(PontVis=sum(PontVisitante))
ptV %>%  ggplot(aes(PontVis,reorder(Visitante,PontVis),fill=PontVis))+
  geom_col(show.legend = FALSE)+
  theme(panel.background = element_rect(fill = "white", colour = "black")) +
  theme(panel.grid.major = element_line(colour = "Black", linetype = "solid")) + 
  xlab("Número de pontos dos times como visitantes")+
  ylab("Times que disputaram o Brasileirão")+
  geom_text(aes(label=PontVis),nudge_x = 1)+
  theme_bw()+
  ggtitle("Quantidades de pontos dos times como visitante no Brasileirão")


ptfinal <- data.frame(ptM[,1:2],ptV[,2])

ptfinal$Pontos <- rowSums(ptfinal[,2:3])


ptfinal %>%  ggplot(aes(Pontos,reorder(Mandante,Pontos),fill=Pontos))+
  geom_col(show.legend = FALSE)+
  theme(panel.background = element_rect(fill = "white", colour = "black")) +
  theme(panel.grid.major = element_line(colour = "Black", linetype = "solid")) + 
  xlab("Número de pontos dos times")+
  ylab("Times que disputaram o Brasileirão")+
  theme_bw()+
  geom_text(aes(label=Pontos),nudge_x = 2)+
  ggtitle("Pontuação final no Brasileirão")

########################### EMPT M ##########################

head(dados)
empate <- dados %>% filter(Ano=="2019",Vencedor=="EMPATE") %>%
  group_by(Mandante,Visitante,Vencedor) %>% summarise(Quant=n(),.groups="drop")

empate$emm <- ifelse(empate$Mandante!=empate$Vencedor,1,0)
emm <- empate %>%  
  group_by(Mandante) %>% summarise(eee=sum(emm))
names(emm) <- c("Time","EmpM")

emm %>%  ggplot(aes(EmpM,reorder(Time,EmpM),fill=EmpM))+
  geom_col(show.legend = FALSE)+
  theme(panel.background = element_rect(fill = "white", colour = "black")) +
  theme(panel.grid.major = element_line(colour = "Black", linetype = "solid")) + 
  xlab("Número de empates como mandante no Brasileirão")+
  ylab("Times que disputaram o Brasileirão")+
  geom_text(aes(label=EmpM),nudge_x = 0.1)+
  theme_bw()+
  ggtitle("Quantidades de empates como mandante no Brasileirão")

########################### EMPT V ##########################

empate$emv <- ifelse(empate$Visitante!=empate$Vencedor,1,0)
emv <- empate %>%  
  group_by(Visitante) %>% summarise(ee=sum(emv))
names(emv) <- c("Time","EmpV")

emv %>%  ggplot(aes(EmpV,reorder(Time,EmpV),fill=EmpV))+
  geom_col(show.legend = FALSE)+
  theme(panel.background = element_rect(fill = "white", colour = "black")) +
  theme(panel.grid.major = element_line(colour = "Black", linetype = "solid")) + 
  xlab("Número de empates como visitante no Brasileirão")+
  ylab("Times que disputaram o Brasileirão")+
  geom_text(aes(label=EmpV),nudge_x = 0.1)+
  theme_bw()+
  ggtitle("Quantidades de empates como visitante no Brasileirão")

########################### EMPT T ##########################

totEmpate <- left_join(emv,emm,by="Time")
totEmpate$Total <- rowSums(totEmpate[,2:3])

totEmpate %>%  ggplot(aes(Total,reorder(Time,Total),fill=Total))+
  geom_col(show.legend = FALSE)+
  theme(panel.background = element_rect(fill = "white", colour = "black")) +
  theme(panel.grid.major = element_line(colour = "Black", linetype = "solid")) + 
  xlab("Número de empates gerais")+
  ylab("Times que disputaram o Brasileirão")+
  theme_bw()+
  geom_text(aes(label=Total),nudge_x = 0.25)+
  ggtitle("Quantidade total de empates dos times nas edições do Brasileirão")

########################### JOGOS M ##########################


head(dados)
jogosM <- dados %>% 
  filter(Ano=="2019")%>% 
  group_by(Mandante) %>% summarise(Quant=n(),.groups="drop")
names(jogosM) <- c("Time","JogosM")
jogosM %>%  ggplot(aes(JogosM,reorder(Time,JogosM),fill=Time))+
  geom_col(show.legend = FALSE)+
  theme(panel.background = element_rect(fill = "white", colour = "black")) +
  theme(panel.grid.major = element_line(colour = "Black", linetype = "solid")) + 
  xlab("Número de jogos como mandante")+
  ylab("Equipes mandantes")+
  theme_bw()+
  geom_text(aes(label=JogosM),nudge_x = 0.25)+
  ggtitle("Quantidade de jogos dos times como mandante")

########################### JOGOS V ##########################

head(dados)
jogosV <- dados %>% 
  filter(Ano=="2019")%>% 
  group_by(Visitante) %>% summarise(Quant=n(),.groups="drop")
names(jogosV) <- c("Time","JogosV")
jogosV %>%  ggplot(aes(JogosV,reorder(Time,JogosV),fill=Time))+
  geom_col(show.legend = FALSE)+
  theme(panel.background = element_rect(fill = "white", colour = "black")) +
  theme(panel.grid.major = element_line(colour = "Black", linetype = "solid")) + 
  xlab("Número de jogos como visitante")+
  ylab("Equipes mandantes")+
  theme_bw()+
  geom_text(aes(label=JogosV),nudge_x = 0.25)+
  ggtitle("Quantidade de jogos dos times como visitante")

########################### JOGOS T ##########################

totJogos <- left_join(jogosM,jogosV,by="Time")
totJogos$Total <- rowSums(totJogos[,2:3])

totJogos %>%  ggplot(aes(Total,reorder(Time,Total),fill=Total))+
  geom_col(show.legend = FALSE)+
  theme(panel.background = element_rect(fill = "white", colour = "black")) +
  theme(panel.grid.major = element_line(colour = "Black", linetype = "solid")) + 
  xlab("Número de jogos gerais")+
  ylab("Times que disputaram o Brasileirão")+
  theme_bw()+
  geom_text(aes(label=Total),nudge_x = 1)+
  ggtitle("Quantidade total de jogos dos times nas edições do Brasileirão")





head(dados)
A07 <- dados %>% filter(Ano=="2007") %>%
  group_by(Mandante,Visitante,Vencedor) 

head(dados)
A08 <- dados %>% filter(Ano=="2008") %>%
  group_by(Mandante,Visitante,Vencedor)


head(dados)
A09BV <- dados %>% filter(Ano=="2009", Visitante=="BOTAFOGO-RJ") %>%
  group_by(Mandante,Visitante) 

head(dados)
A09BM <- dados %>% filter(Ano=="2009",Mandante=="BOTAFOGO-RJ") %>%
  group_by(Mandante,Visitante) 

head(dados)
A9FV <- dados %>% filter(Ano=="2009", Visitante=="FLAMENGO") %>%
  group_by(Mandante,Visitante,Vencedor) 

head(dados)
A9FM <- dados %>% filter(Ano=="2009", Mandante=="FLAMENGO") %>%
  group_by(Mandante,Visitante,Vencedor)  


####################################
dt1 <- dados %>% filter(Ano=="2004") %>% group_by(Mandante) %>% summarise(PontMan=sum(PontMandante))
dt2 <- dados %>% filter(Ano=="2004") %>% group_by(Visitante) %>% summarise(PontVis=sum(PontVisitante))

dt <- data.frame(dt1[,1:2],dt2[,2])

dt$Pontos <- rowSums(dt[,2:3])

tres <- dt %>% top_n(3, Pontos)
names(tres) <- c("Time", "PontMan", "PontVis","Pontos")

#saveRDS(tres, "tres.rds")

library(png)
ouro <- png::readPNG('Ouro.png')
prata <- png::readPNG('Prata.png')
bronze <- png::readPNG('Bronze.png')   

plot <- tres %>% ggplot(aes(Time, Pontos, 
                            fill=Time, 
                            text=paste("Time:", Time, "<br>", 
                                       "Pontuação: ", Pontos)))+
  geom_col(show.legend = FALSE)+
  theme_bw()+
  geom_text(aes(label=Pontos),nudge_y = 2)

ggplotly(plot, tooltip = "text", width = 600, height = 600)%>% 
  layout(images = list(list(
    source = raster2uri(as.raster(ouro)),
    x = 0.75, y = 75, 
    sizex = 0.5, sizey = 15.1,
    xref = "x", yref = "y",
    xanchor = "left", yanchor = "bottom",
    sizing = "stretch"
  ), list(
    source = raster2uri(as.raster(prata)),
    x = 1.75, y = 59, 
    sizex = 0.5, sizey = 15.1,
    xref = "x", yref = "y",
    xanchor = "left", yanchor = "bottom",
    sizing = "stretch"
  ), list(
    source = raster2uri(as.raster(bronze)),
    x = 2.75, y = 59, 
    sizex = 0.5, sizey = 15.1,
    xref = "x", yref = "y",
    xanchor = "left", yanchor = "bottom",
    sizing = "stretch"
  )),
  showlegend = FALSE, 
  title = list(text = paste0('Os três primeiros colocados',
                             '<br>',
                             '<sup>',
                             'Campeonato Brasileiro de 2019',
                             '</sup>')), 
  margin=0) %>%
  style(textposition = "top")



  dt3 <- dados %>% filter(Ano=="2019") %>% group_by(Mandante) %>% summarise(PontMan=sum(PontMandante))
  dt4 <- dados %>% filter(Ano=="2019") %>% group_by(Visitante) %>% summarise(PontVis=sum(PontVisitante))
  
  dt4 <- data.frame(dt3[,1:2],dt4[,2])
  
  dt4$Pontos <- rowSums(dt4[,2:3])
  
  quatro <- dt4 %>% top_n(4, -Pontos) %>%  arrange(desc(Pontos)) %>% head(4)
  names(quatro) <- c("Time", "PontMan", "PontVis","Pontos") 

  


  confrontos <- dados %>% 
    filter(Mandante=="CRUZEIRO",Visitante=="FLAMENGO")%>% 
    group_by(Mandante,Visitante) 
  
  
  
  
  
  head(dados)
  confrotnos <- dados %>% 
    #filter(Ano=="2019")%>% 
    group_by(Mandante) %>% summarise(Quant=n(),.groups="drop")
  
  ptM <- dados %>% 
    #filter(Ano=="2019") %>% 
    group_by(Mandante) %>% summarise(PontMan=sum(PontMandante))
  ptM %>%  ggplot(aes(PontMan,reorder(Mandante,PontMan),fill=PontMan))+
    geom_col(show.legend = FALSE)+
    theme(panel.background = element_rect(fill = "white", colour = "black")) +
    theme(panel.grid.major = element_line(colour = "Black", linetype = "solid")) + 
    xlab("Número de pontos dos times como mandante")+
    ylab("Times que disputaram o Brasileirão")+
    geom_text(aes(label=PontMan),nudge_x = 1)+
    theme_bw()+
    ggtitle("Quantidades de pontos dos times como mandante no Brasileirão")
  .
  
  
  
  
  ptM <- dados %>% 
    #filter(Ano=="2003")%>% 
    group_by(Mandante) %>% summarise(PontMan=sum(PontMandante))
  ptM %>%  ggplot(aes(PontMan,reorder(Mandante,PontMan),fill=PontMan))+
    geom_col(aes(x = 700), fill="white", color = "grey", width = 0.85) +
    geom_col(aes(x = PontMan), alpha = 1.5, width = 0.5) +
    geom_col(width = 0.5) +
    scale_fill_identity() +
    scale_x_continuous(breaks = c(10,50, 100, 150, 200, 250, 300, 350, 400, 450, 500, 550 ,600, 
                                  650, 700)) +
    theme_minimal() +

    xlab("Número de pontos dos times como mandante")+
    ylab("Times que disputaram o Brasileirão")+
    geom_text(aes(label=PontMan),nudge_x = 15)+
    theme_classic()+
    ggtitle("Quantidades de pontos dos times como mandante no Brasileirão")  

  
  
  library(png)
  library("openxlsx")
  links <- read.xlsx("C:/Users/Filipe Fulgêncio/Documents/github/FilipeTCC/dashBrasileirao/LinksClubes.xlsx", colNames = TRUE)
  links$Time <- abjutils::rm_accent(links$Time)
  links$Time <- str_squish(links$Time)
  links$Time <- str_to_upper(links$Time)
  links$Time <- str_trim(links$Time,side = c("both", "left", "right"))
  link_to_img <- function(x, width = 30) {
    glue::glue("<img src='{x}' ,width='{width}'/>")
  }
  names(links) <- c("Mandante","Links")
  df <- inner_join(dados,links,by="Mandante")%>%
    mutate(logos=link_to_img(Links))
    df %>% 
    group_by(Mandante,logos) %>% summarise(PontMan=sum(PontMandante))%>%
    ggplot(aes(PontMan,reorder(logos,PontMan),fill=PontMan))+
    geom_col(aes(x = 700), fill="white", color = "grey", width = 0.85) +
    geom_col(aes(x = PontMan), alpha = 1.5, width = 0.5) +
    geom_col(width = 0.5) +
    scale_fill_identity() +
    scale_x_continuous(breaks = c(10,50, 100, 150, 200, 250, 300, 350, 400, 450, 500, 550 ,600, 
                                  650, 700)) +
    theme_minimal() +
    theme(
      text = element_text(family = "Chivo"),
      panel.grid.major.y = element_blank(),
      panel.grid.minor = element_blank(),
      panel.grid.major.x = element_line(color = "grey", size = 0.2),
      panel.ontop = TRUE,
      axis.text.y = element_text(margin = margin(r = -25, unit = "pt")),
      axis.text.x = element_text(size = 16, color = "grey"),
      plot.title = element_text(size = 36, face = "bold"),
      plot.subtitle = element_text(size = 24),
      plot.margin = unit(c(0.5, 1.5, 0.5, 1.5), "cm")
    ) +
    xlab("Número de pontos dos times como mandante")+
    ylab("Times que disputaram o Brasileirão")+
    geom_text(aes(label=PontMan),nudge_x = 15)+
    theme_classic()+
    ggtitle("Quantidades de pontos dos times como mandante no Brasileirão")  

  
  
   

  
 
  

  
  
