rm(list = ls())
graphics.off()
cat("\014")


library(tidyverse)
library(lubridate)
library(plotly)
library(devtools)




d2021 <- readRDS("d2021.rds")

###########################Grafico 1##########################

v2003 <- 
  d2021 %>% filter(Temporada=="2003", Vencedor!="EMPATE") %>% 
  group_by(Vencedor) %>% summarise(Quant=n(),.groups="drop") %>% 
  ggplot(aes(Quant,reorder(Vencedor,Quant),fill=Vencedor, text=paste("Núm. de Vitórias =", Quant, "<br>",
                                                                     "Time = ", Vencedor)))+
  geom_col(show.legend = FALSE)+
  theme(panel.background = element_rect(fill = "white", colour = "black")) +
  theme(panel.grid.major = element_line(colour = "Black", linetype = "solid")) + 
  xlab("Quantidades de Vitórias")+
  ylab("Times que disputaram o Campeonato")+
  geom_text(aes(label=Quant),nudge_x = 1)+
  theme_bw()+
  ggtitle("Número de vitórias no Brasileirão 2003")+
  geom_point()
vv2003 <- ggplotly(v2003, tooltip = "text") %>% layout(showlegend = FALSE) %>% style(textposition = "top")
htmlwidgets::saveWidget(as_widget(vv2003), "vitorias.html")


########################### GERAL ##########################


########################### ARENA T ##########################

head(d2021)
ar <- d2021 %>% 
  #filter(Temporada=="2019")%>% 
  group_by(Arena) %>% summarise(Quant=n(),.groups="drop")
ar %>%  ggplot(aes(Quant,reorder(Arena,Quant),fill=Arena, text=paste("Núm. de Jogos =", Quant, "<br>",
                                                                     "Arena = ", Arena)))+
  geom_col(show.legend = FALSE)+
  theme(panel.background = element_rect(fill = "white", colour = "black")) +
  theme(panel.grid.major = element_line(colour = "Black", linetype = "solid")) + 
  xlab("Número de jogos nos estádios")+
  ylab("Nome dos estádios")+
  theme_classic()+
  #geom_text(aes(label=Quant),nudge_x = 1)+
  ggtitle("Quantidade de jogos totais em cada estádio nas edições do Brasileirão")
ggplotly(ar, tooltip = "text") %>% layout(showlegend = FALSE) %>% style(textposition = "top")

########################### ARENA + ##########################

head(d2021)
ar <- d2021 %>%
  group_by(Arena) %>% summarise(Quant=n(),.groups="drop")
ar %>% select("Arena")
bom <- ar %>% filter(Quant>255)

bom %>%  ggplot(aes(Quant,reorder(Arena,Quant),fill=Arena, text=paste("Núm. de Jogos =", Quant, "<br>",
                                                                      "Arena = ", Arena)))+
  geom_col(show.legend = FALSE)+
  theme(panel.background = element_rect(fill = "white", colour = "black")) +
  theme(panel.grid.major = element_line(colour = "Black", linetype = "solid")) + 
  xlab("Número de jogos nos estádios")+
  ylab("Nome dos estádios")+
  theme_classic()+
  #geom_text(aes(label=Quant),nudge_x = 17)+
  ggtitle("Os estádios com mais jogos no Brasileirão")
ggplotly(bom, tooltip = "text") %>% layout(showlegend = FALSE) %>% style(textposition = "top")

########################### ARENA - ##########################

head(d2021)
ar <- d2021 %>%
  group_by(Arena) %>% summarise(Quant=n(),.groups="drop")
ar %>% select("Arena")
regular <- ar %>% filter(Quant<=5)

regular %>%  ggplot(aes(Quant,reorder(Arena,Quant),fill=Arena, text=paste("Núm. de Jogos =", Quant, "<br>",
                                                                          "Arena = ", Arena)))+
  geom_col(show.legend = FALSE)+
  theme(panel.background = element_rect(fill = "white", colour = "black")) +
  theme(panel.grid.major = element_line(colour = "Black", linetype = "solid")) + 
  xlab("Número de jogos nos estádios")+
  ylab("Nome dos stádios")+
  theme_classic()+
  #geom_text(aes(label=Quant),nudge_x = 0.2)+
  ggtitle("Os estádios com menos jogos no Brasileirão ")
ggplotly(regular, tooltip = "text") %>% layout(showlegend = FALSE) %>% style(textposition = "top")

########################### GOLS M ##########################

mandanteNG <- d2021 %>%  
  #filter(Temporada=="2019")%>% 
  group_by(Mandante) %>% summarise(golsM=sum(GolsMan))
names(mandanteNG) <- c("Time","GolsM")

visitanteNG <- d2021 %>% 
  #filter(Temporada=="2019")%>% 
  group_by(Visitante) %>% summarise(golsM=sum(GolsVisit))
names(visitanteNG) <- c("Time","GolsV")


plot2 <- left_join(mandanteNG,visitanteNG,by="Time")
plot2$Total <- rowSums(totGols[,2:3])
plot2 %>%  ggplot(aes(GolsM,reorder(Time,GolsM),fill=GolsM, text=paste("Núm. de Gols =", GolsM, "<br>",
                                                                       "Time = ", Time)))+
  geom_col(show.legend = FALSE)+
  theme(panel.background = element_rect(fill = "white", colour = "black")) +
  theme(panel.grid.major = element_line(colour = "Black", linetype = "solid")) + 
  xlab("Número de gols no Brasileirão como mandante")+
  ylab("Times que disputaram o Brasileirão")+
  #geom_text(aes(label=GolsM),nudge_x = 15)+
  theme_classic()+
  ggtitle("Quantidade de gols dos times como mandante no Brasileirão")
ggplotly(plot2, tooltip = "text") %>% layout(showlegend = FALSE) %>% style(textposition = "top")

########################### GOLS V ##########################

plot2 %>%  ggplot(aes(GolsV,reorder(Time,GolsV),fill=GolsV, text=paste("Núm. de Gols =", GolsV, "<br>",
                                                                       "Time = ", Time)))+
  geom_col(show.legend = FALSE)+
  theme(panel.background = element_rect(fill = "white", colour = "black")) +
  theme(panel.grid.major = element_line(colour = "Black", linetype = "solid")) + 
  xlab("Número de gols no Brasileirão como visitante")+
  ylab("Times que disputaram o Brasileirão")+
  #geom_text(aes(label=GolsV),nudge_x = 10)+
  theme_classic()+
  ggtitle("Quantidade de gols dos times como visitantes no Brasileirão")
ggplotly(totGols, tooltip = "text") %>% layout(showlegend = FALSE) %>% style(textposition = "top")

########################### GOLS T ##########################

totGols %>%  ggplot(aes(Total,reorder(Time,Total),fill=Total, text=paste("Núm. de Gols =", Total, "<br>",
                                                                         "Time = ", Time)))+
  geom_col(show.legend = FALSE)+
  theme(panel.background = element_rect(fill = "white", colour = "black")) +
  theme(panel.grid.major = element_line(colour = "Black", linetype = "solid")) + 
  xlab("Número de gols no Brasileirão")+
  ylab("Times que disputaram o Brasileirão")+
  #geom_text(aes(label=Total),nudge_x = 27)+
  theme_bw()+
  ggtitle("Quantidade total de gols dos times nas edições do Brasileirão")
ggplotly(totGols, tooltip = "text") %>% layout(showlegend = FALSE) %>% style(textposition = "top")

########################### VIT M ##########################

d2021$vitMan <- ifelse(d2021$Mandante==d2021$Vencedor,1,0)
testeM <- d2021 %>% filter(Vencedor!="EMPATE") %>% 
  #filter(Temporada=="2019",Vencedor!="EMPATE")%>% 
  group_by(Mandante) %>% summarise(vitM=sum(vitMan))
names(testeM) <- c("Time","VitM")
testeM %>%  ggplot(aes(VitM,reorder(Time,VitM),fill=VitM, text=paste("Núm. de Vitórias =", VitM, "<br>",
                                                                     "Time = ", Time)))+
  geom_col(show.legend = FALSE)+
  theme(panel.background = element_rect(fill = "white", colour = "black")) +
  theme(panel.grid.major = element_line(colour = "Black", linetype = "solid")) + 
  xlab("Número de vitórias como mandante no Brasileirão")+
  ylab("Times que disputaram o Brasileirão")+
  #geom_text(aes(label=VitM),nudge_x = 4)+
  theme_classic()+
  ggtitle("Quantidades de vitórias como mandante no Brasileirão")
ggplotly(testeM, tooltip = "text") %>% layout(showlegend = FALSE) %>% style(textposition = "top")

########################### VIT V ##########################

d2021$vitVist <- ifelse(d2021$Visitante==d2021$Vencedor,1,0)
testeV <- d2021 %>% filter(Vencedor!="EMPATE") %>% 
  #filter(Temporada=="2019")%>% 
  group_by(Visitante) %>% summarise(vitV=sum(vitVist))
names(testeV) <- c("Time","VitV")
testeV %>%  ggplot(aes(VitV,reorder(Time,VitV),fill=VitV, text=paste("Núm. de Vitórias =", VitV, "<br>",
                                                                     "Time = ", Time)))+
  geom_col(show.legend = FALSE)+
  theme(panel.background = element_rect(fill = "white", colour = "black")) +
  theme(panel.grid.major = element_line(colour = "Black", linetype = "solid")) + 
  xlab("Número de vitórias como visitante no Brasileirão")+
  ylab("Times que disputaram o Brasileirão")+
  #geom_text(aes(label=VitV),nudge_x = 3)+
  theme_classic()+
  ggtitle("Quantidades de vitórias como visitante no Brasileirão")
ggplotly(testeV, tooltip = "text") %>% layout(showlegend = FALSE) %>% style(textposition = "top")

########################### VIT T ##########################

head(d2021)
vitot <- d2021 %>% filter(Vencedor!="EMPATE") %>% 
  #filter(Temporada=="2019",Vencedor!="EMPATE")%>%
  group_by(Vencedor) %>% summarise(Quant=n(),.groups="drop")
vitot %>%  ggplot(aes(Quant,reorder(Vencedor,Quant),fill=Vencedor, text=paste("Núm. de Vitórias =", Quant, "<br>",
                                                                              "Time = ", Vencedor)))+
  geom_col(show.legend = FALSE)+
  theme(panel.background = element_rect(fill = "white", colour = "black")) +
  theme(panel.grid.major = element_line(colour = "Black", linetype = "solid")) + 
  xlab("Número de vitórias gerais")+
  ylab("Times que disputaram o Brasileirão")+
  theme_classic()+
  #geom_text(aes(label=Quant),nudge_x = 7)+
  ggtitle("Quantidade total de vitórias dos times nas edições do Brasileirão")
ggplotly(vitot, tooltip = "text") %>% layout(showlegend = FALSE) %>% style(textposition = "top")

########################### DER M ##########################

d2021$derMand <- ifelse(d2021$Mandante==d2021$Derrotado,1,0)
testederM <- d2021 %>% filter(Derrotado!="EMPATE")%>%
  #filter(Temporada=="2019",Derrotado!="EMPATE")%>% 
  group_by(Mandante) %>% summarise(dm=sum(derMand))
names(testederM) <- c("Time","DerM")

testederM %>%  ggplot(aes(DerM,reorder(Time,DerM),fill=DerM, text=paste("Núm. de Derrotas =", DerM, "<br>",
                                                                        "Time = ", Time)))+
  geom_col(show.legend = FALSE)+
  theme(panel.background = element_rect(fill = "white", colour = "black")) +
  theme(panel.grid.major = element_line(colour = "Black", linetype = "solid")) + 
  xlab("Número de derrotas como mandante no Brasileirão")+
  ylab("Times que disputaram o Brasileirão")+
  #geom_text(aes(label=DerM),nudge_x = 1.5)+
  theme_classic()+
  ggtitle("Quantidades de derrotas como mandante no Brasileirão")
ggplotly(testederM, tooltip = "text") %>% layout(showlegend = FALSE) %>% style(textposition = "top")

########################### DER V ##########################

d2021$derVist <- ifelse(d2021$Visitante==d2021$Derrotado,1,0)
testederV <- d2021 %>% filter(Derrotado!="EMPATE")%>%  
  #filter(Temporada=="2019",Derrotado!="EMPATE")%>%  
  group_by(Visitante) %>% summarise(dV=sum(derVist))
names(testederV) <- c("Time","DerV")

testederV %>%  ggplot(aes(DerV,reorder(Time,DerV),fill=DerV, text=paste("Núm. de Derrotas =", DerV, "<br>",
                                                                        "Time = ", Time)))+
  geom_col(show.legend = FALSE)+
  theme(panel.background = element_rect(fill = "white", colour = "black")) +
  theme(panel.grid.major = element_line(colour = "Black", linetype = "solid")) + 
  xlab("Número de derrotas como visitante no Brasileirão")+
  ylab("Times que disputaram o Brasileirão")+
  #geom_text(aes(label=DerV),nudge_x = 3.5)+
  theme_classic()+
  ggtitle("Quantidades de derrotas como visitante no Brasileirão")
ggplotly(testederV, tooltip = "text") %>% layout(showlegend = FALSE) %>% style(textposition = "top")

########################### DER T ##########################

head(d2021)
derotpt <- d2021 %>% filter(Derrotado!="EMPATE")%>%
  #filter(Temporada=="2019",Derrotado!="EMPATE")%>%
  group_by(Derrotado) %>% summarise(Quant=n(),.groups="drop")
derotpt %>%  ggplot(aes(Quant,reorder(Derrotado,Quant),fill=Derrotado, text=paste("Núm. de Derrtoas =", Quant, "<br>",
                                                                                  "Time = ", Derrotado)))+
  geom_col(show.legend = FALSE)+
  theme(panel.background = element_rect(fill = "white", colour = "black")) +
  theme(panel.grid.major = element_line(colour = "Black", linetype = "solid")) + 
  xlab("Número de derrotas gerais")+
  ylab("Times que disputaram o Brasileirão")+
  theme_classic()+
  #geom_text(aes(label=Quant),nudge_x = 5.5)+
  ggtitle("Quantidade total de derrotas dos times nas edições do Brasileirão")
ggplotly(derotpt, tooltip = "text") %>% layout(showlegend = FALSE) %>% style(textposition = "top")

########################### PTS ##########################

ptM <- d2021 %>% 
  #filter(Temporada=="2020")%>% 
  group_by(Mandante) %>% summarise(PontMan=sum(PontMandante))
ptM %>%  ggplot(aes(PontMan,reorder(Mandante,PontMan),fill=PontMan, text=paste("Núm. de Pontos =", PontMan, "<br>",
                                                                               "Time = ", Mandante)))+
  geom_col(show.legend = FALSE)+
  theme(panel.background = element_rect(fill = "white", colour = "black")) +
  theme(panel.grid.major = element_line(colour = "Black", linetype = "solid")) + 
  xlab("Número de pontos dos times como mandante")+
  ylab("Times que disputaram o Brasileirão")+
  geom_text(aes(label=PontMan),nudge_x = 15)+
  theme_classic()+
  ggtitle("Quantidades de pontos dos times como mandante no Brasileirão")
ggplotly(ptM, tooltip = "text") %>% layout(showlegend = FALSE) %>% style(textposition = "top")

ptV <- d2021 %>% 
  #filter(Temporada=="2020")%>% 
  group_by(Visitante) %>% summarise(PontVis=sum(PontVisitante))
ptV %>%  ggplot(aes(PontVis,reorder(Visitante,PontVis),fill=PontVis, text=paste("Núm. de Pontos =", PontVis, "<br>",
                                                                                "Time = ", Visitante)))+
  geom_col(show.legend = FALSE)+
  theme(panel.background = element_rect(fill = "white", colour = "black")) +
  theme(panel.grid.major = element_line(colour = "Black", linetype = "solid")) + 
  xlab("Número de pontos dos times como visitantes")+
  ylab("Times que disputaram o Brasileirão")+
  geom_text(aes(label=PontVis),nudge_x = 10)+
  theme_classic()+
  ggtitle("Quantidades de pontos dos times como visitante no Brasileirão")
ggplotly(ptV, tooltip = "text") %>% layout(showlegend = FALSE) %>% style(textposition = "top")


ptfinal <- data.frame(ptM[,1:2],ptV[,2])

ptfinal$Pontos <- rowSums(ptfinal[,2:3])

ptfinal %>%  ggplot(aes(Pontos,reorder(Mandante,Pontos),fill=Pontos, text=paste("Pontuação =", Pontos, "<br>",
                                                                                "Time = ", Mandante)))+
  geom_col(show.legend = FALSE)+
  theme(panel.background = element_rect(fill = "white", colour = "black")) +
  theme(panel.grid.major = element_line(colour = "Black", linetype = "solid")) + 
  xlab("Número de pontos dos times")+
  ylab("Times que disputaram o Brasileirão")+
  theme_classic()+
  geom_text(aes(label=Pontos),nudge_x = 30)+
  ggtitle("Pontuação final no Brasileirão")
ggplotly(ptfinal, tooltip = "text") %>% layout(showlegend = FALSE) %>% style(textposition = "top")


########################### EMPT M ##########################

head(d2021)
empate <- d2021 %>% filter(Vencedor=="EMPATE") %>%
  #filter(Temporada=="2019",Vencedor=="EMPATE")%>%
  group_by(Mandante,Visitante,Vencedor) %>% summarise(Quant=n(),.groups="drop")

empate$emm <- ifelse(empate$Mandante!=empate$Vencedor,1,0)
emm <- empate %>%  
  group_by(Mandante) %>% summarise(eee=sum(emm))
names(emm) <- c("Time","EmpM")

emm %>%  ggplot(aes(EmpM,reorder(Time,EmpM),fill=EmpM, text=paste("Núm. de Empates =", EmpM, "<br>",
                                                                  "Time = ", Time)))+
  geom_col(show.legend = FALSE)+
  theme(panel.background = element_rect(fill = "white", colour = "black")) +
  theme(panel.grid.major = element_line(colour = "Black", linetype = "solid")) + 
  xlab("Número de empates como mandante no Brasileirão")+
  ylab("Times que disputaram o Brasileirão")+
  #geom_text(aes(label=EmpM),nudge_x = 1)+
  theme_classic()+
  ggtitle("Quantidades de empates como mandante no Brasileirão")
ggplotly(emm, tooltip = "text") %>% layout(showlegend = FALSE) %>% style(textposition = "top")

########################### EMPT V ##########################

empate$emv <- ifelse(empate$Visitante!=empate$Vencedor,1,0)
emv <- empate %>%  
  group_by(Visitante) %>% summarise(ee=sum(emv))
names(emv) <- c("Time","EmpV")

emv %>%  ggplot(aes(EmpV,reorder(Time,EmpV),fill=EmpV, text=paste("Núm. de Empates =", EmpV, "<br>",
                                                                  "Time = ", Time)))+
  geom_col(show.legend = FALSE)+
  theme(panel.background = element_rect(fill = "white", colour = "black")) +
  theme(panel.grid.major = element_line(colour = "Black", linetype = "solid")) + 
  xlab("Número de empates como visitante no Brasileirão")+
  ylab("Times que disputaram o Brasileirão")+
  #geom_text(aes(label=EmpV),nudge_x = 1)+
  theme_classic()+
  ggtitle("Quantidades de empates como visitante no Brasileirão")
ggplotly(emv, tooltip = "text") %>% layout(showlegend = FALSE) %>% style(textposition = "top")

########################### EMPT T ##########################

totEmpate <- left_join(emv,emm,by="Time")
totEmpate$Total <- rowSums(totEmpate[,2:3])

totEmpate %>%  ggplot(aes(Total,reorder(Time,Total),fill=Total, text=paste("Núm. de Empates =", Total, "<br>",
                                                                           "Time = ", Time)))+
  geom_col(show.legend = FALSE)+
  theme(panel.background = element_rect(fill = "white", colour = "black")) +
  theme(panel.grid.major = element_line(colour = "Black", linetype = "solid")) + 
  xlab("Número de empates gerais")+
  ylab("Times que disputaram o Brasileirão")+
  theme_classic()+
  #geom_text(aes(label=Total),nudge_x = 1)+
  ggtitle("Quantidade total de empates dos times nas edições do Brasileirão")
ggplotly(totEmpate, tooltip = "text") %>% layout(showlegend = FALSE) %>% style(textposition = "top")

########################### JOGOS M ##########################

head(d2021)
jogosM <- d2021 %>% 
  #filter(Temporada=="2019")%>% 
  group_by(Mandante) %>% summarise(Quant=n(),.groups="drop")
names(jogosM) <- c("Time","JogosM")
jogosM %>%  ggplot(aes(JogosM,reorder(Time,JogosM),fill=Time, text=paste("Núm. de Jogos =", JogosM, "<br>",
                                                                         "Time = ", Time)))+
  geom_col(show.legend = FALSE)+
  theme(panel.background = element_rect(fill = "white", colour = "black")) +
  theme(panel.grid.major = element_line(colour = "Black", linetype = "solid")) + 
  xlab("Número de jogos como mandante")+
  ylab("Equipes mandantes")+
  theme_classic()+
  #geom_text(aes(label=JogosM),nudge_x = 7)+
  ggtitle("Quantidade de jogos dos times como mandante")
ggplotly(jogosM, tooltip = "text") %>% layout(showlegend = FALSE) %>% style(textposition = "top")

########################### JOGOS V ##########################

head(d2021)
jogosV <- d2021 %>% 
  #filter(Temporada=="2019")%>% 
  group_by(Visitante) %>% summarise(Quant=n(),.groups="drop")
names(jogosV) <- c("Time","JogosV")
jogosV %>%  ggplot(aes(JogosV,reorder(Time,JogosV),fill=Time, text=paste("Núm. de Empates =", JogosV, "<br>",
                                                                         "Time = ", Time)))+
  geom_col(show.legend = FALSE)+
  theme(panel.background = element_rect(fill = "white", colour = "black")) +
  theme(panel.grid.major = element_line(colour = "Black", linetype = "solid")) + 
  xlab("Número de jogos como visitante")+
  ylab("Equipes mandantes")+
  theme_classic()+
  #geom_text(aes(label=JogosV),nudge_x = 8)+
  ggtitle("Quantidade de jogos dos times como visitante")
ggplotly(jogosV, tooltip = "text") %>% layout(showlegend = FALSE) %>% style(textposition = "top")

########################### JOGOS T ##########################

totJogos <- left_join(jogosM,jogosV,by="Time")
totJogos$Total <- rowSums(totJogos[,2:3])

totJogos %>%  ggplot(aes(Total,reorder(Time,Total),fill=Total, text=paste("Núm. de Jogos =", Total, "<br>",
                                                                          "Time = ", Time)))+
  geom_col(show.legend = FALSE)+
  theme(panel.background = element_rect(fill = "white", colour = "black")) +
  theme(panel.grid.major = element_line(colour = "Black", linetype = "solid")) + 
  xlab("Número de jogos gerais")+
  ylab("Times que disputaram o Brasileirão")+
  theme_classic()+
  #geom_text(aes(label=Total),nudge_x = 15)+
  ggtitle("Quantidade total de jogos dos times nas edições do Brasileirão")
ggplotly(totJogos, tooltip = "text") %>% layout(showlegend = FALSE) %>% style(textposition = "top")









########################### POR Temporada ##########################


#############################################################





########################### ARENA T ##########################

head(d2021)
ar <- d2021 %>% 
  filter(Temporada=="2019")%>% 
  group_by(Arena) %>% summarise(Quant=n(),.groups="drop")
ar %>%  ggplot(aes(Quant,reorder(Arena,Quant),fill=Arena))+
  geom_col(show.legend = FALSE)+
  theme(panel.background = element_rect(fill = "white", colour = "black")) +
  theme(panel.grid.major = element_line(colour = "Black", linetype = "solid")) + 
  xlab("Número de jogos nos estádios")+
  ylab("Nome dos estádios")+
  theme_classic()+
  geom_text(aes(label=Quant),nudge_x = 1)+
  ggtitle("Quantidade de jogos totais em cada estádio nas edições do Brasileirão")

########################### ARENA + ##########################

head(d2021)
ar <- d2021 %>%
  group_by(Arena) %>% summarise(Quant=n(),.groups="drop")
ar %>% select("Arena")
bom <- ar %>% filter(Quant>255)

bom %>%  ggplot(aes(Quant,reorder(Arena,Quant),fill=Arena))+
  geom_col(show.legend = FALSE)+
  theme(panel.background = element_rect(fill = "white", colour = "black")) +
  theme(panel.grid.major = element_line(colour = "Black", linetype = "solid")) + 
  xlab("Número de jogos nos estádios")+
  ylab("Nome dos estádios")+
  theme_classic()+
  geom_text(aes(label=Quant),nudge_x = 17)+
  ggtitle("Os estádios com mais jogos no Brasileirão")

########################### ARENA - ##########################

head(d2021)
ar <- d2021 %>%
  group_by(Arena) %>% summarise(Quant=n(),.groups="drop")
ar %>% select("Arena")
regular <- ar %>% filter(Quant<=5)

regular %>%  ggplot(aes(Quant,reorder(Arena,Quant),fill=Arena))+
  geom_col(show.legend = FALSE)+
  theme(panel.background = element_rect(fill = "white", colour = "black")) +
  theme(panel.grid.major = element_line(colour = "Black", linetype = "solid")) + 
  xlab("Número de jogos nos estádios")+
  ylab("Nome dos stádios")+
  theme_classic()+
  geom_text(aes(label=Quant),nudge_x = 0.2)+
  ggtitle("Os estádios com menos jogos no Brasileirão ")

########################### GOLS M ##########################

mandanteNG <- d2021 %>%  
  filter(Temporada=="2019")%>% 
  group_by(Mandante) %>% summarise(golsM=sum(GolsMan))
names(mandanteNG) <- c("Time","GolsM")

visitanteNG <- d2021 %>% 
  filter(Temporada=="2019")%>% 
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
  theme_classic()+
  ggtitle("Quantidade de gols dos times como mandante no Brasileirão")

########################### GOLS V ##########################

totGols %>%  ggplot(aes(GolsV,reorder(Time,GolsV),fill=GolsV))+
  geom_col(show.legend = FALSE)+
  theme(panel.background = element_rect(fill = "white", colour = "black")) +
  theme(panel.grid.major = element_line(colour = "Black", linetype = "solid")) + 
  xlab("Número de gols no Brasileirão como visitante")+
  ylab("Times que disputaram o Brasileirão")+
  geom_text(aes(label=GolsV),nudge_x = 1)+
  theme_classic()+
  ggtitle("Quantidade de gols dos times como visitantes no Brasileirão")

########################### GOLS T ##########################

totGols %>%  ggplot(aes(Total,reorder(Time,Total),fill=Total))+
  geom_col(show.legend = FALSE)+
  theme(panel.background = element_rect(fill = "white", colour = "black")) +
  theme(panel.grid.major = element_line(colour = "Black", linetype = "solid")) + 
  xlab("Número de gols no Brasileirão")+
  ylab("Times que disputaram o Brasileirão")+
  geom_text(aes(label=Total),nudge_x = 2)+
  theme_classic()+
  ggtitle("Quantidade total de gols dos times nas edições do Brasileirão")

########################### VIT M ##########################

d2021$vitMan <- ifelse(d2021$Mandante==d2021$Vencedor,1,0)
testeM <- d2021 %>% filter(Temporada=="2019",Vencedor!="EMPATE") %>% 
  #filter(Temporada=="2019",Vencedor!="EMPATE")%>% 
  group_by(Mandante) %>% summarise(vitM=sum(vitMan))
names(testeM) <- c("Time","VitM")
testeM %>%  ggplot(aes(VitM,reorder(Time,VitM),fill=VitM))+
  geom_col(show.legend = FALSE)+
  theme(panel.background = element_rect(fill = "white", colour = "black")) +
  theme(panel.grid.major = element_line(colour = "Black", linetype = "solid")) + 
  xlab("Número de vitórias como mandante no Brasileirão")+
  ylab("Times que disputaram o Brasileirão")+
  geom_text(aes(label=VitM),nudge_x = 0.2)+
  theme_classic()+
  ggtitle("Quantidades de vitórias como mandante no Brasileirão")

########################### VIT V ##########################

d2021$vitVist <- ifelse(d2021$Visitante==d2021$Vencedor,1,0)
testeV <- d2021 %>% filter(Temporada=="2019",Vencedor!="EMPATE") %>% 
  #filter(Temporada=="2019")%>% 
  group_by(Visitante) %>% summarise(vitV=sum(vitVist))
names(testeV) <- c("Time","VitV")
testeV %>%  ggplot(aes(VitV,reorder(Time,VitV),fill=VitV))+
  geom_col(show.legend = FALSE)+
  theme(panel.background = element_rect(fill = "white", colour = "black")) +
  theme(panel.grid.major = element_line(colour = "Black", linetype = "solid")) + 
  xlab("Número de vitórias como visitante no Brasileirão")+
  ylab("Times que disputaram o Brasileirão")+
  geom_text(aes(label=VitV),nudge_x = 0.2)+
  theme_classic()+
  ggtitle("Quantidades de vitórias como visitante no Brasileirão")

########################### VIT T ##########################

head(d2021)
vitot <- d2021 %>% filter(Temporada=="2019",Vencedor!="EMPATE") %>% 
  #filter(Temporada=="2019",Vencedor!="EMPATE")%>%
  group_by(Vencedor) %>% summarise(Quant=n(),.groups="drop")
vitot %>%  ggplot(aes(Quant,reorder(Vencedor,Quant),fill=Vencedor))+
  geom_col(show.legend = FALSE)+
  theme(panel.background = element_rect(fill = "white", colour = "black")) +
  theme(panel.grid.major = element_line(colour = "Black", linetype = "solid")) + 
  xlab("Número de vitórias gerais")+
  ylab("Times que disputaram o Brasileirão")+
  theme_classic()+
  geom_text(aes(label=Quant),nudge_x = 1)+
  ggtitle("Quantidade total de vitórias dos times nas edições do Brasileirão")

########################### DER M ##########################

d2021$derMand <- ifelse(d2021$Mandante==d2021$Derrotado,1,0)
testederM <- d2021 %>% filter(Temporada=="2019",Derrotado!="EMPATE")%>%
  #filter(Temporada=="2019",Derrotado!="EMPATE")%>% 
  group_by(Mandante) %>% summarise(dm=sum(derMand))
names(testederM) <- c("Time","DerM")

testederM %>%  ggplot(aes(DerM,reorder(Time,DerM),fill=DerM))+
  geom_col(show.legend = FALSE)+
  theme(panel.background = element_rect(fill = "white", colour = "black")) +
  theme(panel.grid.major = element_line(colour = "Black", linetype = "solid")) + 
  xlab("Número de derrotas como mandante no Brasileirão")+
  ylab("Times que disputaram o Brasileirão")+
  geom_text(aes(label=DerM),nudge_x = 0.2)+
  theme_classic()+
  ggtitle("Quantidades de derrotas como mandante no Brasileirão")

########################### DER V ##########################

d2021$derVist <- ifelse(d2021$Visitante==d2021$Derrotado,1,0)
testederV <- d2021 %>% filter(Temporada=="2019",Derrotado!="EMPATE")%>%  
  #filter(Temporada=="2019",Derrotado!="EMPATE")%>%  
  group_by(Visitante) %>% summarise(dV=sum(derVist))
names(testederV) <- c("Time","DerV")

testederV %>%  ggplot(aes(DerV,reorder(Time,DerV),fill=DerV))+
  geom_col(show.legend = FALSE)+
  theme(panel.background = element_rect(fill = "white", colour = "black")) +
  theme(panel.grid.major = element_line(colour = "Black", linetype = "solid")) + 
  xlab("Número de derrotas como visitante no Brasileirão")+
  ylab("Times que disputaram o Brasileirão")+
  geom_text(aes(label=DerV),nudge_x = 0.2)+
  theme_classic()+
  ggtitle("Quantidades de derrotas como visitante no Brasileirão")

########################### DER T ##########################

head(d2021)
derotpt <- d2021 %>% filter(Temporada=="2019",Derrotado!="EMPATE")%>%
  #filter(Temporada=="2019",Derrotado!="EMPATE")%>%
  group_by(Derrotado) %>% summarise(Quant=n(),.groups="drop")
derotpt %>%  ggplot(aes(Quant,reorder(Derrotado,Quant),fill=Derrotado))+
  geom_col(show.legend = FALSE)+
  theme(panel.background = element_rect(fill = "white", colour = "black")) +
  theme(panel.grid.major = element_line(colour = "Black", linetype = "solid")) + 
  xlab("Número de derrotas gerais")+
  ylab("Times que disputaram o Brasileirão")+
  theme_classic()+
  geom_text(aes(label=Quant),nudge_x = 1)+
  ggtitle("Quantidade total de derrotas dos times nas edições do Brasileirão")

########################### PTS ##########################

ptM <- d2021 %>% filter(Temporada=="2019") %>% group_by(Mandante) %>% summarise(PontMan=sum(PontMandante))
ptM %>%  ggplot(aes(PontMan,reorder(Mandante,PontMan),fill=PontMan))+
  geom_col(show.legend = FALSE)+
  theme(panel.background = element_rect(fill = "white", colour = "black")) +
  theme(panel.grid.major = element_line(colour = "Black", linetype = "solid")) + 
  xlab("Número de pontos dos times como mandante")+
  ylab("Times que disputaram o Brasileirão")+
  geom_text(aes(label=PontMan),nudge_x = 1)+
  theme_classic()+
  ggtitle("Quantidades de pontos dos times como mandante no Brasileirão")

ptV <- d2021 %>% filter(Temporada=="2019") %>% group_by(Visitante) %>% summarise(PontVis=sum(PontVisitante))
ptV %>%  ggplot(aes(PontVis,reorder(Visitante,PontVis),fill=PontVis))+
  geom_col(show.legend = FALSE)+
  theme(panel.background = element_rect(fill = "white", colour = "black")) +
  theme(panel.grid.major = element_line(colour = "Black", linetype = "solid")) + 
  xlab("Número de pontos dos times como visitantes")+
  ylab("Times que disputaram o Brasileirão")+
  geom_text(aes(label=PontVis),nudge_x = 1)+
  theme_classic()+
  ggtitle("Quantidades de pontos dos times como visitante no Brasileirão")


ptfinal <- data.frame(ptM[,1:2],ptV[,2])

ptfinal$Pontos <- rowSums(ptfinal[,2:3])

ptfinal %>%  ggplot(aes(Pontos,reorder(Mandante,Pontos),fill=Pontos))+
  geom_col(show.legend = FALSE)+
  theme(panel.background = element_rect(fill = "white", colour = "black")) +
  theme(panel.grid.major = element_line(colour = "Black", linetype = "solid")) + 
  xlab("Número de pontos dos times")+
  ylab("Times que disputaram o Brasileirão")+
  theme_classic()+
  geom_text(aes(label=Pontos),nudge_x = 2)+
  ggtitle("Pontuação final no Brasileirão")

########################### EMPT M ##########################

head(d2021)
empate <- d2021 %>% filter(Temporada=="2019",Vencedor=="EMPATE") %>%
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
  theme_classic()+
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
  theme_classic()+
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
  theme_classic()+
  geom_text(aes(label=Total),nudge_x = 0.25)+
  ggtitle("Quantidade total de empates dos times nas edições do Brasileirão")

########################### JOGOS M ##########################

head(d2021)
jogosM <- d2021 %>% 
  filter(Temporada=="2019")%>% 
  group_by(Mandante) %>% summarise(Quant=n(),.groups="drop")
names(jogosM) <- c("Time","JogosM")
jogosM %>%  ggplot(aes(JogosM,reorder(Time,JogosM),fill=Time))+
  geom_col(show.legend = FALSE)+
  theme(panel.background = element_rect(fill = "white", colour = "black")) +
  theme(panel.grid.major = element_line(colour = "Black", linetype = "solid")) + 
  xlab("Número de jogos como mandante")+
  ylab("Equipes mandantes")+
  theme_classic()+
  geom_text(aes(label=JogosM),nudge_x = 0.25)+
  ggtitle("Quantidade de jogos dos times como mandante")

########################### JOGOS V ##########################

head(d2021)
jogosV <- d2021 %>% 
  filter(Temporada=="2019")%>% 
  group_by(Visitante) %>% summarise(Quant=n(),.groups="drop")
names(jogosV) <- c("Time","JogosV")
jogosV %>%  ggplot(aes(JogosV,reorder(Time,JogosV),fill=Time))+
  geom_col(show.legend = FALSE)+
  theme(panel.background = element_rect(fill = "white", colour = "black")) +
  theme(panel.grid.major = element_line(colour = "Black", linetype = "solid")) + 
  xlab("Número de jogos como visitante")+
  ylab("Equipes mandantes")+
  theme_classic()+
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
  theme_classic()+
  geom_text(aes(label=Total),nudge_x = 1)+
  ggtitle("Quantidade total de jogos dos times nas edições do Brasileirão")