
#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

# Define UI for application

######################## Imagem inicial ############################

shinyUI(fluidPage(
  navbarPage(title="Brasileirão 2003-2019", theme = shinytheme("flatly"),
             tabPanel("Inicio",
                      #tabsetPanel(
                        #tabPanel(" ",
                                 fluidRow(
                                   column(12,
                                          img(src = "Brasileirao.jpg", height = 680, width = 1550)#DT::dataTableOutput("table1")
                                          ))#)
                        ##
                        
                        
                        
                      #)# barra de navegacao interna
             ),# barra de navegacao superior (Dados do Participante)
             
             ####################### Aba Tabela ############################
             
             tabPanel("Tabela Brasileirão",
                      tabsetPanel(
                        tabPanel("Tabela Brasileirão de 2003-2019",
                                 fluidRow(
                                   column(12,
                                          DT::dataTableOutput("table1")
                                   )))
                        ##
                        
                        
                        
                      )# barra de navegacao interna
             ),# barra de navegacao superior (Dados do Participante)
             
             ####################### PLANO ARENA ############################
             
             tabPanel("Jogos por Arena",
                      tabsetPanel(
                        ##
                        tabPanel("Número de jogos por ano em cada Arena",
                                 fluidRow(column(3, 
                                                 selectInput("Ano1", 
                                                             strong("Escolha uma opção:"), 
                                                             choices=c("2003"="2003",
                                                                       "2004"="2004",
                                                                       "2005"="2005",
                                                                       "2006"="2006",
                                                                       "2007"="2007",
                                                                       "2008"="2008",
                                                                       "2009"="2009",
                                                                       "2010"="2010",
                                                                       "2011"="2011",
                                                                       "2012"="2012",
                                                                       "2013"="2013",
                                                                       "2014"="2014",
                                                                       "2015"="2015",
                                                                       "2016"="2016",
                                                                       "2017"="2017",
                                                                       "2018"="2018",
                                                                       "2019"="2019"),
                                                             selected = "2019")),
                                          column(9,
                                                 plotlyOutput("plot21", height = 800)))),
                        
                        tabPanel("Arena com mais jogos",
                                 fluidRow(column(9,
                                                 plotlyOutput("plot1", height = 800)))),
                        tabPanel("Arena com jogos",
                                 fluidRow(column(9,
                                                 plotlyOutput("plot2", height = 800))))
                        
                      )# barra de navegacao interna
             ),# barra de navegacao superior (Dados da Escola)
             
             ######################## PLANO PONTOS ############################
           
             tabPanel("Pontuação das equipes",
                      tabsetPanel(
                        ##
                       tabPanel("Pontuação Geral",
                                fluidRow(column(3, 
                                                selectInput("Ano53", 
                                                            strong("Escolha uma opção:"), 
                                                            choices=c("Todos os anos"= "Todos os anos",
                                                                      "2003"="2003",
                                                                      "2004"="2004",
                                                                      "2005"="2005",
                                                                      "2006"="2006",
                                                                      "2007"="2007",
                                                                      "2008"="2008",
                                                                      "2009"="2009",
                                                                      "2010"="2010",
                                                                      "2011"="2011",
                                                                      "2012"="2012",
                                                                      "2013"="2013",
                                                                      "2014"="2014",
                                                                      "2015"="2015",
                                                                      "2016"="2016",
                                                                      "2017"="2017",
                                                                      "2018"="2018",
                                                                      "2019"="2019"),
                                                            selected = "2019")),
                                         column(9,
                                                plotlyOutput("plot5", height = 800)))),
                        tabPanel("Pontuação como Mandante",
                                 fluidRow(column(3, 
                                                 selectInput("Ano51", 
                                                             strong("Escolha uma opção:"), 
                                                             choices=c("2003"="2003",
                                                                       "2004"="2004",
                                                                       "2005"="2005",
                                                                       "2006"="2006",
                                                                       "2007"="2007",
                                                                       "2008"="2008",
                                                                       "2009"="2009",
                                                                       "2010"="2010",
                                                                       "2011"="2011",
                                                                       "2012"="2012",
                                                                       "2013"="2013",
                                                                       "2014"="2014",
                                                                       "2015"="2015",
                                                                       "2016"="2016",
                                                                       "2017"="2017",
                                                                       "2018"="2018",
                                                                       "2019"="2019"),
                                                             selected = "2019")),
                                          column(9,
                                                 plotlyOutput("plot3", height = 800)))),
                       
                        tabPanel("Pontuação como Visitante",
                                 fluidRow(column(3, 
                                                 selectInput("Ano52", 
                                                             strong("Escolha uma opção:"), 
                                                             choices=c("2003"="2003",
                                                                       "2004"="2004",
                                                                       "2005"="2005",
                                                                       "2006"="2006",
                                                                       "2007"="2007",
                                                                       "2008"="2008",
                                                                       "2009"="2009",
                                                                       "2010"="2010",
                                                                       "2011"="2011",
                                                                       "2012"="2012",
                                                                       "2013"="2013",
                                                                       "2014"="2014",
                                                                       "2015"="2015",
                                                                       "2016"="2016",
                                                                       "2017"="2017",
                                                                       "2018"="2018",
                                                                       "2019"="2019"),
                                                             selected = "2019")),
                                          column(9,
                                                 plotlyOutput("plot4", height = 800))))
                        
                      )# barra de navegacao interna
             ),# barra de navegacao superior (Dados da Escola)  
             
             ######################## PLANO VITORIAS ############################
             
             tabPanel("Vitórias das equipes",
                      tabsetPanel(
                        ##
                        tabPanel("Viórias Geral",
                                 fluidRow(column(9,
                                                 plotlyOutput("plot8", height = 800)))),
                        tabPanel("Vitórias como Mandante",
                                 fluidRow(column(3, 
                                                 selectInput("Ano22", 
                                                             strong("Escolha uma opção:"), 
                                                             choices=c("2003"="2003",
                                                                       "2004"="2004",
                                                                       "2005"="2005",
                                                                       "2006"="2006",
                                                                       "2007"="2007",
                                                                       "2008"="2008",
                                                                       "2009"="2009",
                                                                       "2010"="2010",
                                                                       "2011"="2011",
                                                                       "2012"="2012",
                                                                       "2013"="2013",
                                                                       "2014"="2014",
                                                                       "2015"="2015",
                                                                       "2016"="2016",
                                                                       "2017"="2017",
                                                                       "2018"="2018",
                                                                       "2019"="2019"),
                                                             selected = "2019")),
                                          column(9,
                                                 plotlyOutput("plot6", height = 800)))),
                        tabPanel("Vitórias como Visitante",
                                 fluidRow(column(3, 
                                                 selectInput("Ano42", 
                                                             strong("Escolha uma opção:"), 
                                                             choices=c("2003"="2003",
                                                                       "2004"="2004",
                                                                       "2005"="2005",
                                                                       "2006"="2006",
                                                                       "2007"="2007",
                                                                       "2008"="2008",
                                                                       "2009"="2009",
                                                                       "2010"="2010",
                                                                       "2011"="2011",
                                                                       "2012"="2012",
                                                                       "2013"="2013",
                                                                       "2014"="2014",
                                                                       "2015"="2015",
                                                                       "2016"="2016",
                                                                       "2017"="2017",
                                                                       "2018"="2018",
                                                                       "2019"="2019"),
                                                             selected = "2019")),
                                          column(9,
                                                 plotlyOutput("plot7", height = 800)))),
                        tabPanel("Vitórias por ano",
                                 fluidRow(column(3, 
                                                 selectInput("Ano2", 
                                                             strong("Escolha uma opção:"), 
                                                             choices=c("2003"="2003",
                                                                       "2004"="2004",
                                                                       "2005"="2005",
                                                                       "2006"="2006",
                                                                       "2007"="2007",
                                                                       "2008"="2008",
                                                                       "2009"="2009",
                                                                       "2010"="2010",
                                                                       "2011"="2011",
                                                                       "2012"="2012",
                                                                       "2013"="2013",
                                                                       "2014"="2014",
                                                                       "2015"="2015",
                                                                       "2016"="2016",
                                                                       "2017"="2017",
                                                                       "2018"="2018",
                                                                       "2019"="2019"),
                                                             selected = "2019")),
                                          column(9,
                                                 plotlyOutput("plot22", height = 800))))
                        
                      )# barra de navegacao interna
             ),# barra de navegacao superior (Dados da Escola)  
             
             ######################## PLANO EMPATES ############################
             
             tabPanel("Empates das equipes",
                      tabsetPanel(
                        ##
                        tabPanel("Empates Geral",
                                 fluidRow(column(9,
                                                 plotlyOutput("plot9", height = 800)))),
                        tabPanel("Empates como Mandante",
                                 fluidRow(column(9,
                                                 plotlyOutput("plot10", height = 800)))),
                        tabPanel("Empates como Visitante",
                                 fluidRow(column(9,
                                                 plotlyOutput("plot11", height = 800)))),
                        tabPanel("Empates por ano",
                                 fluidRow(column(3, 
                                                 selectInput("Ano49", 
                                                             strong("Escolha uma opção:"), 
                                                             choices=c("2003"="2003",
                                                                       "2004"="2004",
                                                                       "2005"="2005",
                                                                       "2006"="2006",
                                                                       "2007"="2007",
                                                                       "2008"="2008",
                                                                       "2009"="2009",
                                                                       "2010"="2010",
                                                                       "2011"="2011",
                                                                       "2012"="2012",
                                                                       "2013"="2013",
                                                                       "2014"="2014",
                                                                       "2015"="2015",
                                                                       "2016"="2016",
                                                                       "2017"="2017",
                                                                       "2018"="2018",
                                                                       "2019"="2019"),
                                                             selected = "2019")),
                                          column(9,
                                                 plotlyOutput("plot49", height = 800))))
                        
                      )# barra de navegacao interna
             ),# barra de navegacao superior (Dados da Escola)  
             
             ######################## PLANO DERROTAS ############################
             
             tabPanel("Derrotas das equipes",
                      tabsetPanel(
                        ##
                        tabPanel("Derrotas Geral",
                                 fluidRow(column(9,
                                                 plotlyOutput("plot12", height = 800)))),
                        tabPanel("Derrotas como Mandante",
                                 fluidRow(column(9,
                                                 plotlyOutput("plot13", height = 800)))),
                        tabPanel("Derrotas como Visitante",
                                 fluidRow(column(9,
                                                 plotlyOutput("plot14", height = 800)))),
                        tabPanel("Derrotas por ano",
                                 fluidRow(column(3, 
                                                 selectInput("Ano3", 
                                                             strong("Escolha uma opção:"), 
                                                             choices=c("2003"="2003",
                                                                       "2004"="2004",
                                                                       "2005"="2005",
                                                                       "2006"="2006",
                                                                       "2007"="2007",
                                                                       "2008"="2008",
                                                                       "2009"="2009",
                                                                       "2010"="2010",
                                                                       "2011"="2011",
                                                                       "2012"="2012",
                                                                       "2013"="2013",
                                                                       "2014"="2014",
                                                                       "2015"="2015",
                                                                       "2016"="2016",
                                                                       "2017"="2017",
                                                                       "2018"="2018",
                                                                       "2019"="2019"),
                                                             selected = "2019")),
                                          column(9,
                                                 plotlyOutput("plot23", height = 800))))
                        
                      )# barra de navegacao interna
             ),# barra de navegacao superior (Dados da Escola)  
             
             ######################## PLANO GOLS ############################
             
             tabPanel("Gols das equipes",
                      tabsetPanel(
                        ##
                        tabPanel("Gols Geral",
                                 fluidRow(column(9,
                                                 plotlyOutput("plot15", height = 800)))),
                        tabPanel("Gols como Mandante",
                                 fluidRow(column(9,
                                                 plotlyOutput("plot16", height = 800)))),
                        tabPanel("Gols como Visitante",
                                 fluidRow(column(9,
                                                 plotlyOutput("plot17", height = 800)))),
                        tabPanel("Gols por ano",
                                 fluidRow(column(3, 
                                                 selectInput("Ano59", 
                                                             strong("Escolha uma opção:"), 
                                                             choices=c("2003"="2003",
                                                                       "2004"="2004",
                                                                       "2005"="2005",
                                                                       "2006"="2006",
                                                                       "2007"="2007",
                                                                       "2008"="2008",
                                                                       "2009"="2009",
                                                                       "2010"="2010",
                                                                       "2011"="2011",
                                                                       "2012"="2012",
                                                                       "2013"="2013",
                                                                       "2014"="2014",
                                                                       "2015"="2015",
                                                                       "2016"="2016",
                                                                       "2017"="2017",
                                                                       "2018"="2018",
                                                                       "2019"="2019"),
                                                             selected = "2019")),
                                          column(9,
                                                 plotlyOutput("plot59", height = 800))))
                        
                      )# barra de navegacao interna
             ),# barra de navegacao superior (Dados da Escola)            
             
             ######################## PLANO JOGOS ############################
             
             tabPanel("Jogos das equipes",
                      tabsetPanel(
                        ##
                        tabPanel("Jogoss Geral",
                                 fluidRow(column(9,
                                                 plotlyOutput("plot18", height = 800)))),
                        tabPanel("Jogos como Mandante",
                                 fluidRow(column(9,
                                                 plotlyOutput("plot19", height = 800)))),
                        tabPanel("Jogos como Visitante",
                                 fluidRow(column(9,
                                                 plotlyOutput("plot20", height = 800))))
                        
                      )# barra de navegacao interna
             ),# barra de navegacao superior (Dados da Escola)  
           
               ######################## TESTE ############################
            
             tabPanel("Teste",
                      tabsetPanel(
                        ##
                                tabPanel("TT2",
                                 fluidRow(column(3, 
                                                 selectInput("Ano10", 
                                                             strong("Escolha uma opção:"), 
                                                             choices=c("2003"="2003",
                                                                       "2004"="2004",
                                                                       "2005"="2005",
                                                                       "2006"="2006",
                                                                       "2007"="2007",
                                                                       "2008"="2008",
                                                                       "2009"="2009",
                                                                       "2010"="2010",
                                                                       "2011"="2011",
                                                                       "2012"="2012",
                                                                       "2013"="2013",
                                                                       "2014"="2014",
                                                                       "2015"="2015",
                                                                       "2016"="2016",
                                                                       "2017"="2017",
                                                                       "2018"="2018",
                                                                       "2019"="2019"),
                                                             selected = "2019")),
                                          column(9,
                                                 plotlyOutput("plot30", height = 800)))),  
                                tabPanel("TT1",
                                         fluidRow(column(3, 
                                                         selectInput("Ano11", 
                                                                     strong("Escolha uma opção:"), 
                                                                     choices=c("2003"="2003",
                                                                               "2004"="2004",
                                                                               "2005"="2005",
                                                                               "2006"="2006",
                                                                               "2007"="2007",
                                                                               "2008"="2008",
                                                                               "2009"="2009",
                                                                               "2010"="2010",
                                                                               "2011"="2011",
                                                                               "2012"="2012",
                                                                               "2013"="2013",
                                                                               "2014"="2014",
                                                                               "2015"="2015",
                                                                               "2016"="2016",
                                                                               "2017"="2017",
                                                                               "2018"="2018",
                                                                               "2019"="2019"),
                                                                     selected = "2019")),
                                                  column(9,
                                                         plotlyOutput("plot34", height = 800))))  
               
                      )# barra de navegacao interna
             )# barra de navegacao superior (Dados da Escola)  
             
                    
    )#navbarPage
)#fluidPage
)#shinyUI
