<<<<<<< HEAD
library(tabulizer)
library(dplyr)
library(stringi)
url <- 'http://www2.alerj.rj.gov.br/leideacesso/spic/arquivo/folha-de-pagamento-2018-01.pdf'
d <- extract_tables(url, encoding = "UTF-8", pages = 1)

if (!require("remotes")) {
  install.packages("remotes")
}
# on 64-bit Windows
remotes::install_github(c("ropensci/tabulizer"), INSTALL_opts = "--no-multiarch")
# elsewhere
remotes::install_github(c("ropensci/tabulizerjars", "ropensci/tabulizer"))
=======
rm(list = ls())
graphics.off()
cat("\014")

library("openxlsx")
library(ggplot2)
library(ggalt)
library(compareDF)
#Brasileiro2017 <- read.xlsx("C:/Users/Filipe Fulgêncio/Documents/github/FilipeTCC/Brasileiro2017.xlsx", colNames = TRUE)

#Brasileiro2016 <- read.xlsx("C:/Users/Filipe Fulgêncio/Documents/github/FilipeTCC/Brasileiro2016.xlsx", colNames = TRUE)


#vitorias16 <- Brasileiro2016 [c(4,5,13),c(2,5)]

 
#vitorias17 <- Brasileiro2017 [c(6,7,10),c(2,5) ]

#plot(vitorias16, type = "p", col = "blue")
#par(new=TRUE)


#media_vitorias16 <- length(vitorias16)
#media_vitorias17 <- length(vitorias17)




#mediana_vitorias16 <- median(vitorias16, na.rm = TRUE)
#mediana_vitorias17 <- median(vitorias17, na.rm = TRUE)

#mediana_saldogols16 <- median(saldogols16, na.rm = TRUE)
#mediana_saldogols17 <- median(saldogols17 , na.rm = TRUE)

#mediana_aproveit16 <- median(aproveit16, na.rm = TRUE)
#mediana_aproveit17 <- median(aproveit17, na.rm = TRUE)

#media_vitorias16 <- mean(vitorias16, na.rm = TRUE)
#media_vitorias17 <- mean(vitorias17, na.rm = TRUE)

#media_saldogols16 <- mean(saldogols16, na.rm = TRUE)
#media_saldogols17 <- mean(saldogols17, na.rm = TRUE)

#media_aproveit16 <- mean(aproveit16, na.rm = TRUE)
#media_aproveit17 <- mean(aproveit17, na.rm = TRUE)

#var_vitoria16 <- var(vitorias16, na.rm = TRUE)
#var_vitoria17 <- var(vitorias17, na.rm = TRUE)

#var_saldogols16 <- var(saldogols16, na.rm = TRUE)
#var_saldogols17 <- var(saldosgols17, na.rm = TRUE)

#var_aproveit16 <- var(aproveit16, na.rm = TRUE)
#var_aproveit17 <- var(aproveit17, na.rm = TRUE)

#desvio_vitorias <- sqrt(var_vitoria16)

#plot(vitorias16, type = "l", col = "blue")
#par(new=TRUE)

#barplot(vitorias17, type = "l", col = "pink")
#par(new=FALSE)

#boxplot(vitorias16)

#t.test(vitorias16, vitorias17)  


Campeonato2012 <- read.xlsx("C:/Users/Filipe Fulgêncio/Documents/github/FilipeTCC/Campeonato2012.xlsx", colNames = TRUE)

vitorias <- Campeonato2012 [c(1:760),c(3,6,7,11,12,13)]


x <- Campeonato2012$Away
y <- Campeonato2012$PA
plot(x, y,  main = "Campeonato2012",
     xlab = "Probabilidade vitorias visitantes", ylab = "Visitantes"),
abline(lm(y ~ x, data = mtcars), col = "red")



#ggplot(Campeonato2012, aes(x=PA,y=Away)
#geom_smooth(method = "loess", se=T)


#labs(subtitle=probabilidade vitorias visitantes,
#     y="Away",
#     X="PA",
#     title="vitorias")



>>>>>>> 17999b6cc2aee36a947036c2de153cf898afec67
