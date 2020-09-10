rm(list = ls())
#para certificar-se de que você tenha um ambiente limpo de R
graphics.off()
#fechando todos os dispositivos abertos
cat("\014")
#limpar a tela
options(scipen = 999)

# chamar um pacote do R
library(openxlsx)
#ajudam a ler dados de planilhas Excel
library(ggplot2)
#pacote do R voltado para a criação de gráficos estatísticos
library(ggalt)
#fazer faixas de confiança para gráficos do tipo escada
library(pdftools)
library(formattable)

Brasileiro2016 <- read.xlsx("C:/Users/Filipe Fulgêncio/Documents/github/FilipeTCC/Brasileiro2016.xlsx", colNames = TRUE)

Pontos_geral <- Brasileiro2016 [c(2:21), c(2,3)]

y <- Pontos_geral$X2 
x <- Pontos_geral$`Campeonato.Brasileiro.de.Futebol.-.Série.A.-.2016` 

ggplot(Pontos_geral, aes(x, y)) +
  geom_point(color = "black") +
  geom_smooth(method = "loess", formula = x ~ y) +
  labs(subtitle = "Pontos dos times no Campeonato Brasileiro de 2016", 
       y= "Times", 
       x= "Pontuação", 
       title = "Tabela de Dados")


Vitorias_geral <- Brasileiro2016 [c(2:21), c(2,5)]

y <- Vitorias_geral$X2 
x <- Vitorias_geral$X5 

ggplot(Vitorias_geral, aes(x, y)) +
  geom_point(color = "red") +
  geom_smooth(method = "loess", formula = x ~ y) +
  labs(subtitle = "Vitorias dos times no Campeonato Brasileiro de 2016", 
       y= "Times", 
       x= "Vitorias", 
       title = "Tabela de Dados")

Derrotas_geral <- Brasileiro2016 [c(2:21), c(2,7)]

y <- Derrotas_geral$X2 
x <- Derrotas_geral$X7

ggplot(Derrotas_geral, aes(x, y)) +
  geom_point(color = "blue") +
  geom_smooth(method = "loess", formula = x ~ y) +
  labs(subtitle = "Derrotas dos times no Campeonato Brasileiro de 2016", 
       y= "Times", 
       x= "Derrotas", 
       title = "Tabela de Dados")


Gols_pro <- Brasileiro2016 [c(2:21), c(2,8)]

y <- Gols_pro$X2 
x <- Gols_pro$X8
#x <- formattable(z, digits = , format = "f")
ggplot(Gols_pro, aes(x, y)) +
  geom_point(color = "green") +
  geom_smooth(method = "loess", formula = x ~ y) +
  labs(subtitle = "Gols Pró dos times no Campeonato Brasileiro de 2016", 
       y= "Times", 
       x= "Gols Pró", 
       title = "Tabela de Dados")




Pontos_geral <- Brasileiro2017 [c(2:21), c(2,3)]

y <- Pontos_geral$X2 
x <- Pontos_geral$`Campeonato.Brasileiro.de.Futebol.-.Série.A.-.2017` 

ggplot(Pontos_geral, aes(x, y)) +
  geom_point(color = "red") +
  geom_smooth(method = "loess", formula = x ~ y) +
  labs(subtitle = "Pontos dos times no Campeonato Brasileiro de 2017", 
       y= "Times", 
       x= "Pontuação", 
       title = "Tabela de Dados")

Pontos_paulistas <- Brasileiro2017 [c(2,3,4,14), c(2,3)]   
Pontos_cariocas <- Brasileiro2017 [c(7,8,11,15), c(2,3)]


y <- Pontos_paulistas$X2 
x <- Pontos_paulistas$`Campeonato.Brasileiro.de.Futebol.-.Série.A.-.2017` 
ggplot(Pontos_paulistas, aes(x, y)) +
  geom_point(color = "blue") +
  geom_smooth(method = "loess", formula = x ~ y) +
  labs(subtitle = "Pontos dos times Paulistas no Campeonato Brasileiro de 2017", 
       y= "Times", 
       x= "Pontuação", 
       title = "Tabela de Dados")


y <- Pontos_cariocas$X2 
x <- Pontos_cariocas$`Campeonato.Brasileiro.de.Futebol.-.Série.A.-.2017` 
ggplot(Pontos_cariocas, aes(x, y)) +
  geom_point(color = "blue") +
  geom_smooth(method = "loess", formula = x ~ y) +
  labs(subtitle = "Pontos dos times Cariocas no Campeonato Brasileiro de 2017", 
       y= "Times", 
       x= "Pontuação", 
       title = "Tabela de Dados")

Vitorias_geral <- Brasileiro2017 [c(2:21), c(2,5)]

y <- Vitorias_geral$X2 
x <- Vitorias_geral$X5 

ggplot(Vitorias_geral, aes(x, y)) +
  geom_point(color = "red") +
  geom_smooth(method = "loess", formula = x ~ y) +
  labs(subtitle = "Vitorias dos times no Campeonato Brasileiro de 2017", 
       y= "Times", 
       x= "Vitorias", 
       title = "Tabela de Dados")

Derrotas_geral <- Brasileiro2017 [c(2:21), c(2,7)]

y <- Derrotas_geral$X2 
x <- Derrotas_geral$X7

ggplot(Derrotas_geral, aes(x, y)) +
  geom_point(color = "red") +
  geom_smooth(method = "loess", formula = x ~ y) +
  labs(subtitle = "Derrotas dos times no Campeonato Brasileiro de 2017", 
       y= "Times", 
       x= "Derrotas", 
       title = "Tabela de Dados")




Brasileiro2017 <- read.xlsx("C:/Users/Filipe Fulgêncio/Documents/github/FilipeTCC/Brasileiro2017.xlsx", colNames = TRUE)



y <- Brasileiro2017$X2
x <- Brasileiro2017$`Campeonato.Brasileiro.de.Futebol.-.Série.A.-.2017`



ggplot(Brasileiro2017, aes(y, x)) +
  geom_line()