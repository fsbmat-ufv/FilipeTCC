rm(list = ls())
graphics.off()
cat("\014")

library("tidyverse")
library("lubridate")
library("plotly")

dados <- readRDS("dados.rds")

###########################Grafico 1##########################

v2003 <- 
  dados %>% filter(Ano=="2003", Vencedor!="EMPATE") %>% 
  group_by(Vencedor) %>% summarise(Quant=n(),.groups="drop") %>% 
  ggplot(aes(Quant,reorder(Vencedor,Quant),fill=Vencedor, text=paste("Núm. de Vitórias =", Quant, "<br>",
                                                                     "Time= ", Vencedor)))+
  geom_col(show.legend = FALSE)+
  theme(panel.background = element_rect(fill = "white", colour = "black")) +
  theme(panel.grid.major = element_line(colour = "Black", linetype = "solid")) + 
  xlab("Quantidades de Vitórias/Empates")+
  ylab("Times que disputaram o Campeonato")+
  geom_text(aes(label=Quant),nudge_x = 1)+
  theme_bw()+
  ggtitle("Número de vitórias no Brasileirão 2003")
ggplotly(v2003, tooltip = "text") %>% layout(showlegend = FALSE) %>% style(textposition = "top")
