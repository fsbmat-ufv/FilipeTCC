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
