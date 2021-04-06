#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

# Define server logic required to draw a histogram
shinyServer(function(input, output, session) {
  
  ##################################################################
  #########################d2021 do Participante####################
  ##################################################################
  output$table1 <- DT::renderDataTable({
    DT::datatable(d2021,  
                  class = 'cell-border stripe',
                  extensions = 'Buttons', options = list(
                    dom = 'Bfrtip',
                    buttons = c('pageLength','copy', 'csv', 'excel', 'pdf', 'print')
                  ))
  })
  
  ################################# GRAFICO ARENA ###################
  
  output$plot21 <- renderPlotly({
    
    #head(d2021)
    
    plot21 <- d2021 %>% 
      filter(Temporada==input$Temporada1)%>% 
      group_by(Arena) %>% summarise(Quant=n(),.groups="drop") %>%  
      ggplot(aes(Quant, reorder(Arena,Quant), 
                 fill=Arena, 
                 text=paste("Núm. de Jogos =", Quant, "<br>",
                            "Arena = ", Arena)))+
      geom_col(show.legend = FALSE)+
      theme(panel.background = element_rect(fill = "white", colour = "black")) +
      theme(panel.grid.major = element_line(colour = "Black", linetype = "solid")) + 
      xlab("Número de jogos nas Arenas")+
      ylab("Nome das Arenas")+
      theme_classic()+
      geom_text(aes(label=Quant),nudge_x = 1)+
      ggtitle("Quantidade de jogos em cada Arena nas edições do Brasileirão",input$Temporada1)
    ggplotly(plot21, tooltip = "text") %>% layout(showlegend = FALSE) %>% style(textposition = "right")
    
  })
  
  output$plot1 <- renderPlotly({  
    
    # ar <- d2021 %>%
    #    group_by(Arena) %>% summarise(Quant=n(),.groups="drop") %>% filter(Quant>=255)
    
    plot1 <- d2021 %>%
      group_by(Arena) %>% summarise(Quant=n(),.groups="drop") %>% 
      filter(Quant>=255) %>%  
      ggplot(aes(Quant, reorder(Arena,Quant), 
                 fill=Arena, 
                 text=paste("Núm. de Jogos =", Quant, "<br>",
                            "Arena = ", Arena)))+
      geom_col(show.legend = FALSE)+
      theme(panel.background = element_rect(fill = "white", colour = "black")) +
      theme(panel.grid.major = element_line(colour = "Black", linetype = "solid")) + 
      xlab("Número de jogos nas Arenas")+
      ylab("Nome das Arenas")+
      theme_classic()+
      geom_text(aes(label=Quant),nudge_x = 17)+
      ggtitle("As Arenas que receberam mais jogos no Brasileirão")
    ggplotly(plot1, tooltip = "text") %>% layout(showlegend = FALSE) %>% style(textposition = "right")
  }) 
  
  
  output$plot2 <- renderPlotly({
    
    plot2 <- d2021 %>%
      group_by(Arena) %>% summarise(Quant=n(),.groups="drop") %>% filter(Quant<=5) %>%  ggplot(aes(Quant,reorder(Arena,Quant),fill=Arena, text=paste("Núm. de Jogos =", Quant, "<br>",
                                                                                                                                                     "Arena = ", Arena)))+
      geom_col(show.legend = FALSE)+
      theme(panel.background = element_rect(fill = "white", colour = "black")) +
      theme(panel.grid.major = element_line(colour = "Black", linetype = "solid")) + 
      xlab("Número de jogos nas Arenas")+
      ylab("Nome das Arenas")+
      theme_classic()+
      geom_text(aes(label=Quant),nudge_x = 0.1)+
      ggtitle("As Arenas que receberam menos jogos no Brasileirão ")
    ggplotly(plot2, tooltip = "text") %>% layout(showlegend = FALSE) %>% style(textposition = "right")
  }) 
  
  
  
  ################################# GRAFICO PONTOS ###################
  
  output$plot3 <- renderPlotly({
    
    if(input$Temporada51=="Todas Temporada") {
      plot3 <- d2021 %>% #filter(Temporada==input$Temporada51)%>% 
        group_by(Mandante) %>% summarise(PontMan=sum(PontMandante)) %>% ggplot(aes(PontMan,reorder(Mandante,PontMan),fill=Mandante, text=paste("Núm. de Pontos =", PontMan, "<br>",
                                                                                                                                               "Time = ", Mandante)))+
        geom_col(show.legend = FALSE)+
        theme(panel.background = element_rect(fill = "white", colour = "black")) +
        theme(panel.grid.major = element_line(colour = "Black", linetype = "solid")) + 
        xlab("Número de pontos dos times como mandante")+
        ylab("Times que disputaram o Brasileirão")+
        geom_text(aes(label=PontMan),nudge_x = 1)+
        theme_classic()+
        ggtitle("Quantidades de pontos dos times como mandante no Brasileirão")
      ggplotly(plot3, tooltip = "text") %>% layout(showlegend = FALSE) %>% style(textposition = "right")
      
    } else {
      plot3 <- d2021 %>% filter(Temporada==input$Temporada51)%>% 
        group_by(Mandante) %>% summarise(PontMan=sum(PontMandante)) %>% ggplot(aes(PontMan,reorder(Mandante,PontMan),fill=PontMan, text=paste("Núm. de Pontos =", PontMan, "<br>",
                                                                                                                                              "Time = ", Mandante)))+
        geom_col(show.legend = FALSE)+
        theme(panel.background = element_rect(fill = "white", colour = "black")) +
        theme(panel.grid.major = element_line(colour = "Black", linetype = "solid")) + 
        xlab("Número de pontos dos times como mandante")+
        ylab("Times que disputaram o Brasileirão")+
        geom_text(aes(label=PontMan),nudge_x = 1)+
        theme_classic()+
        ggtitle("Quantidades de pontos dos times como mandante no Brasileirão")
      ggplotly(plot3, tooltip = "text") %>% layout(showlegend = FALSE) %>% style(textposition = "right")
      
      
    }
    
  }) 
  
  output$plot4 <- renderPlotly({
    
    if(input$Temporada52=="Todas Temporada") {   
      plot4 <- d2021 %>% 
        #filter(Temporada==input$Temporada52)%>%  
        group_by(Visitante) %>% summarise(PontVis=sum(PontVisitante)) %>% ggplot(aes(PontVis,reorder(Visitante,PontVis),fill=Visitante, text=paste("Núm. de Pontos =", PontVis, "<br>",
                                                                                                                                                   "Time = ", Visitante)))+
        geom_col(show.legend = FALSE)+
        theme(panel.background = element_rect(fill = "white", colour = "black")) +
        theme(panel.grid.major = element_line(colour = "Black", linetype = "solid")) + 
        xlab("Número de pontos dos times como visitantes")+
        ylab("Times que disputaram o Brasileirão")+
        geom_text(aes(label=PontVis),nudge_x = 1)+
        theme_classic()+
        ggtitle("Quantidades de pontos dos times como visitante no Brasileirão")
      ggplotly(plot4, tooltip = "text") %>% layout(showlegend = FALSE) %>% style(textposition = "right")
      
    }else {
      plot4 <- d2021 %>% 
        filter(Temporada==input$Temporada52)%>%  
        group_by(Visitante) %>% summarise(PontVis=sum(PontVisitante)) %>% ggplot(aes(PontVis,reorder(Visitante,PontVis),fill=PontVis, text=paste("Núm. de Pontos =", PontVis, "<br>",
                                                                                                                                                 "Time = ", Visitante)))+
        geom_col(show.legend = FALSE)+
        theme(panel.background = element_rect(fill = "white", colour = "black")) +
        theme(panel.grid.major = element_line(colour = "Black", linetype = "solid")) + 
        xlab("Número de pontos dos times como visitantes")+
        ylab("Times que disputaram o Brasileirão")+
        geom_text(aes(label=PontVis),nudge_x = 1)+
        theme_classic()+
        ggtitle("Quantidades de pontos dos times como visitante no Brasileirão")
      ggplotly(plot4, tooltip = "text") %>% layout(showlegend = FALSE) %>% style(textposition = "right")
      
    }
    
  }) 
  
  
  output$plot5 <- renderPlotly({
    
    if(input$Temporada53=="Todas Temporada") {
      
      ptM <- d2021 %>% 
        #filter(Temporada==input$Temporada53)%>% 
        group_by(Mandante) %>% summarise(PontMan=sum(PontMandante))
      
      ptV <- d2021 %>% 
        #filter(Temporada==input$Temporada53)%>% 
        group_by(Visitante) %>% summarise(PontVis=sum(PontVisitante))
      
      ptfinal <- data.frame(ptM[,1:2],ptV[,2])
      
      ptfinal$Pontos <- rowSums(ptfinal[,2:3])
      
      plot5 <- ptfinal  %>% ggplot(aes(Pontos,reorder(Mandante,Pontos),fill=Mandante, text=paste("Pontuação =", Pontos, "<br>",
                                                                                                 "Time = ", Mandante)))+
        geom_col(show.legend = FALSE)+
        theme(panel.background = element_rect(fill = "white", colour = "black")) +
        theme(panel.grid.major = element_line(colour = "Black", linetype = "solid")) + 
        xlab("Número de pontos dos times")+
        ylab("Times que disputaram o Brasileirão")+
        theme_classic()+
        geom_text(aes(label=Pontos),nudge_x = 1)+
        ggtitle("Pontuação final no Brasileirão")
      ggplotly(plot5, tooltip = "text") %>% layout(showlegend = FALSE) %>% style(textposition = "right")
      
    } else {
      
      ptM <- d2021 %>% 
        filter(Temporada==input$Temporada53)%>% 
        group_by(Mandante) %>% summarise(PontMan=sum(PontMandante))
      
      ptV <- d2021 %>% 
        filter(Temporada==input$Temporada53)%>% 
        group_by(Visitante) %>% summarise(PontVis=sum(PontVisitante))
      
      ptfinal <- data.frame(ptM[,1:2],ptV[,2])
      
      ptfinal$Pontos <- rowSums(ptfinal[,2:3])
      
      plot5 <- ptfinal  %>% ggplot(aes(Pontos,reorder(Mandante,Pontos),fill=Pontos, text=paste("Pontuação =", Pontos, "<br>",
                                                                                               "Time = ", Mandante)))+
        geom_col(show.legend = FALSE)+
        theme(panel.background = element_rect(fill = "white", colour = "black")) +
        theme(panel.grid.major = element_line(colour = "Black", linetype = "solid")) + 
        xlab("Número de pontos dos times")+
        ylab("Times que disputaram o Brasileirão")+
        theme_classic()+
        geom_text(aes(label=Pontos),nudge_x = 1)+
        ggtitle("Pontuação final no Brasileirão")
      ggplotly(plot5, tooltip = "text") %>% layout(showlegend = FALSE) %>% style(textposition = "right")
      
    }
    
  }) 
  
  output$tabTemporada53 <- renderTable({
    
    if(input$Temporada53=="2003"){
      
      tab2003 <- data.frame("Informações"= c("1", "2", "3","4","5"), "Descrição"=c("Ponte Preta escalou irregularmente o jogador Roberto nas partidas contra Internacional e Juventude, pelas rodadas 1 e 2 respectivamente, e perdeu os pontos conquistados nessas partidas. Internacional e Juventude ganharam 3 pontos cada.", 
                                                                               "O Paysandu escalou irregularmente os jogadores Júnior Amorim e Aldrovani e por esse motivo perdeu oito pontos. Desses pontos, três foram para a Ponte Preta, três para o São Caetano, dois para o Corinthians e mais dois para o Fluminense.", 
                                                                               "Libertadores 2004: Cruzeiro, Santos, São Paulo, São Caetano, Coritiba",
                                                                               "Sul-Americana 2004: Internacional, Atlético-MG, Flamengo, Goias, Paraná, Figueirense, Grêmio.",
                                                                               "Rebaixados para Série B: Bahia, Fortaleza."))
      
      tab2003
      
    } else{
      
      if(input$Temporada53=="2004"){
        
        tab2004 <- data.frame("Informações"= c("1", "2","3","4"), "Descrição"=c("O São Caetano foi punido com a perda de 24 pontos pelo STJD, pela morte do jogador Serginho.",
                                                                            "Libertadores 2005: Santos, Atlético-PR, São Paulo, Palmeiras.",
                                                                            "Sul-Americana 2005: Corinthians, Goiás, Juventude, Internacional, Fluminense, Cruzeiro.",
                                                                            "Rebaixados para Série B: Criciúma, Guarani, Vitoria, Grêmio."))
        
        tab2004
        
      } else {
        
        if(input$Temporada53=="2005"){
          
          tab2005 <- data.frame("Informações"= c("1", "2","3","4"), "Descrição"=c("São Paulo tinha vaga garantida na Copa Libertadores de 2006 por ser campeão da Copa Libertadores 2005. O Corinthians, por ser o campeão, também teve direito a disputar a Copa Sul-Americana de 2006.", 
                                                                              "Libertadores 2006: Corinthias, Internacional, Goiás, Palmeiras, São Paulo",
                                                                              "Sul-Americana 2006: Fluminense, Atlético-PR, Paraná, Cruzeiro, Botafogo, Santos, Vasco da Gama.",
                                                                              "Rebaixados para Série B: Coritiba, Atlético-MG, Brasiliense, Paysandu."))
          tab2005                                                                  
          
          
        } else {
          
          if(input$Temporada53=="2006"){
            
            tab2006 <- data.frame("Informações"= c("1", "2","3","4"), "Descrição"=c("Internacional e Flamengo tinham vaga garantida Libertadores 2007 por serem campeões da Libertadores 2006 e da Copa do Brasil de 2006, respectivamente. Além disso, o São Paulo, por ser campeão, também ganhava o direito de participar da Libertadores 2007.", 
                                                                                "Libertadores 2007: São Paulo, Internacional, Grêmio, Santos, Paraná, Flamengo.",
                                                                                "Sul-Americana 2007: Vasco da Gama, Figueirense, Goiás, Corinthians, Cruzeiro, Botafogo, Atlético-PR.",
                                                                                "Rebaixados para Série B: Ponte Preta, Fortaleza, São Caetano, Santa Cruz."))
            tab2006   
            
          } else {
            
            if(input$Temporada53=="2007"){
              
              tab2007 <- data.frame("Informações"= c("1", "2","3","4"), "Descrição"=c("O Fluminense já estava classificado para a Libertadores 2008 por ser campeão da Copa do Brasil de 2007. O São Paulo, por ser o campeão, também teve direito a disputar a Copa Sul-Americana de 2008.", 
                                                                                  "Libertadores 2008: São Paulo, Santos, Flamengo, Fluminense, Cruzeiro.",
                                                                                  "Sul-Americana 2008: Grêmio, Palmeiras, Atlético-MG, Botafogo, Vasco da Gama, Internacional, Atlético-PR.",
                                                                                  "Rebaixados para Série B: Corinthians, Paraná, Juventude, América-RN."))
              tab2007
              
            } else {
              
              if(input$Temporada53=="2008"){
                
                tab2008 <- data.frame("Informações"= c("1", "2","3","4","5"), "Descrição"=c("O Sport teve vaga garantida na Copa Libertadores da América de 2009 por ser campeão da Copa do Brasil de 2008.", 
                                                                                        "O Internacional, por ser campeão da Copa Sul-Americana de 2008, garantiu a vaga na competição do Temporada seguinte e abriu mais uma vaga para a Copa Sul-Americana de 2009.",
                                                                                        "Libertadores 2009: São Paulo, Grêmio, Cruzeiro, Palmeiras, Sport.",
                                                                                        "Sul-Americana 2009: Flamengo, Internacional, Botafogo, Goiás, Coritiba, Vitória, Atlético-MG, Atlético-PR, Fluminense.",
                                                                                        "Rebaixados para Série B: Figueirense, Vasco, Portuguesa, Ipatinga."))
                tab2008
                
              } else {
                
                if(input$Temporada53=="2009"){
                  
                  tab2009 <- data.frame("Informações"= c("1", "2","3","4"), "Descrição"=c("O Corinthians tinha vaga garantida na Libertadores de 2010 por ter sido campeão da Copa do Brasil de 2009.", 
                                                                                      "Libertadores 2010: Flamengo, Internacional, São Paulo, Cruzeiro, Corinthians.",
                                                                                      "Sul-Americana 2010: Palmeias, Avaí, Atlético-MG, Grêmio, Goiás, Grêmio Barueri, Santos, Vitória.",
                                                                                      "Rebaixados para Série B: Coritiba, Santo André, Náutico, Sport."))
                  tab2009
                  
                } else {
                  
                  if(input$Temporada53=="2010"){
                    
                    tab2010 <- data.frame("Informações"= c("1", "2","3","4","5","6"), "Descrição"=c("O Grêmio Prudente foi punido com a perda de três pontos devido à escalação irregular do atleta Paulão, que havia sido suspenso dois dias antes na partida contra o Flamengo pela 3ª rodada.",
                                                                                            "Internacional e Santos tinham vaga garantida na Copa Libertadores de 2011 por serem campeões da Copa Libertadores 2010 e da Copa do Brasil de 2010, respectivamente.", 
                                                                                            "Libertadores 2011: Fluminense, Cruzeiro, Corinthians, Grêmio, Internacional, Santos.",
                                                                                            "Sul-Americana 2011: Atlético-PR, Botafogo, São Paulo, Palmeiras, Vasco da Gama, Ceará, Atlético-MG, Flamengo.",
                                                                                            "Rebaixados para Série B: Vitória*, Guarani, Góias, Grêmio Prudente.",
                                                                                            "*O Vitória foi rebaixado para a Série B pelo critério de desempate, visto que terminou  com 9 vitórias e o Atlético-GO com 11 vitórias."))
                    tab2010
                    
                  } else {
                    
                    if(input$Temporada53=="2011"){
                      
                      tab2011 <- data.frame("Informações"= c("1", "2","3","4"), "Descrição"=c("Santos e Vasco da Gama tinham vaga garantida na Copa Libertadores de 2012 por serem campeões da Copa Libertadores 2011 e Copa do Brasil de 2011, respectivamente.",
                                                                                          "Libertadores 2012: Corinthians, Vasco da Gama, Fluminense, Flamengo, Internacional.",
                                                                                          "Sul-Americana 2012: São Paulo, Figueirense, Coritiba, Botafogo, Palmeiras, Grêmio, Atlético-GO, Bahia.",
                                                                                          "Rebaixados para Série B: Atlético-PR, Ceará, América-MG, Avaí."))
                      tab2011
                      
                    } else {
                      
                      if(input$Temporada53=="2012"){
                        
                        tab2012 <- data.frame("Informações"= c("1", "2","3","4"), "Descrição"=c("Corinthians e Palmeiras tinham vaga garantida na Copa Libertadores de 2013 por serem campeões da Copa Libertadores de 2012 e Copa do Brasil de 2012, respectivamente e o São Paulo
                                                                                                           tinha vaga na Sul-Americana 2013 por ter sido da Sul-Americana 2012",
                                                                                            "Libertadores 2013: Fluminense, Atlético-MG, Grêmio, São Paulo, Corinthians.",
                                                                                            "Sul-Americana 2013: São Paulo, Náutico, Coritiba, Ponte Preta, Bahia, Portuguesa, Criciúma, Vitória, Sport.",
                                                                                            "Rebaixados para Série B: Sport, Palmeiras, Figueirense, Atlético-GO."))
                        tab2012
                        
                      } else {
                        
                        if(input$Temporada53=="2013"){
                          
                          tab2013 <- data.frame("Informações"= c("1", "2","3","4","5","6","7"), "Descrição"=c("Atlético Mineiro e Flamengo tinham vaga garantida na Copa Libertadores de 2014 por serem campeões, respectivamente, da Copa Libertadores de 2013 e da Copa do Brasil de 2013.",
                                                                                                      "O Flamengo foi punido pelo STJD com a perda de 4 pontos por escalação de jogador irregular.",
                                                                                                      "A Portuguesa foi punida pelo STJD com a perda de 4 pontos por escalação de jogador irregular.",
                                                                                                      "Libertadores 2014: Cruzeiro, Grêmio, Atlético-PR, Botafogo, Atlético-MG, Flamengo.",
                                                                                                      "Sul-Americana 2014: Vitória, Goiás, São Paulo, Bahia, Internacional, Criciúma, Fluminense, Sport.",
                                                                                                      "Rebaixados para Série B: Portuguesa*, Vasco da Gama, Ponte Preta, Náutico.",
                                                                                                      "*A Portuguesa acabou rebaixada para a Série B pela perda de 4 pontos no STJD, com isso o Criciúma não foi rebaixado."))
                          tab2013
                          
                        } else {
                          
                          if(input$Temporada53=="2014"){
                            
                            tab2014 <- data.frame("Informações"= c("1", "2","3","4"), "Descrição"=c("Atlético Mineiro tem vaga garantida na Copa Libertadores de 2015 por ter sido campeão da Copa do Brasil de 2014.",
                                                                                                "Libertadores 2015: Cruzeiro, São Paulo, Internacional, Corinthians, Atlético-MG.",
                                                                                                "Sul-Americana 2015: Atlético-PR, Sport, Goiás, Chapecoense, Joinville, Ponte Preta, Bahia.",
                                                                                                "Rebaixados para Série B: Vitória, Bahia, Botafogo, Criciúma."))
                            tab2014
                            
                          } else {
                            
                            if(input$Temporada53=="2015"){
                              
                              tab2015 <- data.frame("Informações"= c("1", "2","3","4"), "Descrição"=c("Palmeiras tem vaga garantida na Copa Libertadores de 2016 por ser campeão da Copa do Brasil de 2015.",
                                                                                                  "Libertadores 2016: Corinthians, Atlético-MG, Grêmio, São Paulo, Palmeiras.",
                                                                                                  "Sul-Americana 2016: Sport, Flamengo, Chapecoense, Coritiba, Figueirense, Vitória, Santa Cruz.",
                                                                                                  "Rebaixados para Série B: Avaí, Vasco da Gama, Goiás, Joinville."))
                              tab2015
                              
                            } else {
                              
                              if(input$Temporada53=="2016"){
                                
                                tab2016 <- data.frame("Informações"= c("1", "2","3","4","5"), "Descrição"=c("Grêmio e Chapecoense têm vaga garantida na Copa Libertadores de 2017 por serem campeões, respectivamente, da Copa do Brasil de 2016 e da Copa Sul-Americana de 2016.",
                                                                                                        "O Santa Cruz foi penalizado com a perda de três pontos por atraso no pagamentos de salários.",
                                                                                                        "Libertadores 2017: Palmeiras, Santos, Flamengo, Atlético-MG, Botafogo, Atlético-PR, Grêmio, Chapecoense.",
                                                                                                        "Sul-Americana 2017: Corinthians, Ponte Preta, São Paulo, Cruzeiro, Fluminense, Sport.",
                                                                                                        "Rebaixados para Série B: Internacional, Figueirense, Santa Cruz, América-MG."))
                                tab2016
                                
                              } else {
                                
                                if(input$Temporada53=="2017"){
                                  
                                  tab2017 <- data.frame("Informações"= c("1", "2","3","4"), "Descrição"=c("Cruzeiro e Grêmio têm vaga garantida na Copa Libertadores de 2018 por serem campeões, respectivamente, da Copa do Brasil de 2017 e da Copa Libertadores de 2017.",
                                                                                                      "Libertadores 2018: Corinthians, Palmeiras, Santos, Grêmio, Cruzeiro, Flamengo, Vasco da Gama, Chapecoense.",
                                                                                                      "Sul-Americana 2018: Atlético-MG,Botafogo, Athletico-PR, Bahia, São Paulo, Fluminense.",
                                                                                                      "Rebaixados para Série B: Coritiba, Avaí, Ponte Preta, Atlético-GO."))
                                  tab2017
                                  
                                } else {
                                  
                                  if(input$Temporada53=="2018"){
                                    
                                    tab2018 <- data.frame("Informações"= c("1", "2","3","4"), "Descrição"=c("Cruzeiro e Atlético Paranaense têm vaga garantida na Copa Libertadores de 2019 por serem campeões, respectivamente, da Copa do Brasil de 2018 e da Copa Sul-Americana de 2018.",
                                                                                                        "Libertadores 2019: Palmeiras, Flamengo, Internacional, Grêmio, São Paulo, Atlético-MG, Athletico-PR, Cruzeiro.",
                                                                                                        "Sul-Americana 2019: Botafogo, Santos,Bahia, Fluminense, Corinthians, Chapecoense.",
                                                                                                        "Rebaixados para Série B: Sport, América-MG, Vitória, Paraná."))
                                    tab2018
                                    
                                  } else {
                                    
                                    if(input$Temporada53=="2019"){
                                      
                                      tab2019 <- data.frame("Informações"= c("1", "2","3","4"), "Descrição"=c("Flamengo e Athletico Paranaense têm vagas garantidas na Copa Libertadores de 2020 por serem campeões da Copa Libertadores de 2019 e da Copa do Brasil de 2019, respectivamente.",
                                                                                                          "Libertadores 2020: Flamengo, Santos, Palmeiras, Grêmio, Athletico-PR, São Paulo, Internacional, Corinthians.",
                                                                                                          "Sul-Americana 2020: Fortaleza, Góias, Bahia, Vasco da Gama, Atlético-MG, Fluminense.",
                                                                                                          "Rebaixados para Série B: Cruzeiro, CSA, Chapecoense, Avaí."))
                                      tab2019
                                      
                                    } else {
                                      
                                      if(input$Temporada53=="2020"){
                                      
                                      tab2020 <- data.frame("Informações"= c("1", "2","3","4","5","6"), "Descrição"=c("Devido às restrições provacada pelo COVID-19 o Brasileirão de 2020 finalizou no Temporada de 2021.",
                                                                                                                      "Palmeiras têm vagas garantidas na Copa Libertadores de 2021 por ser campeão da Copa Libertadores de 2020 e da Copa do Brasil de 2020. Com isso, o Grêmio herdou a vaga para Libertadores 2021 pelo Brasileirão.",
                                                                                                                      "Libertadores 2021: Flamengo, Internacional, Atlético-MG, São Paulo, Fluminense, Grêmio, Palmeiras.",
                                                                                                                      "Sul-Americana 2021: Santos, Athletico-PR, Bragantino, Ceará, Corinthians, Atlético-GO, Bahia.",
                                                                                                                      "Rebaixados para Série B: Vasco da Gama*, Goiás, Coritiba, Botafogo.",
                                                                                                                      "*O Vasco da Gama foi rebaixado para a Série B pelo critério de desempate, visto que terminou com o Saldo de Gols -19 e o Fortaleza com Saldo de Gols -10."))
                                      tab2020
                                      
                                      } 
                                        
                                        #Text
                                    }
                                  }
                                }
                              }
                            }
                          }
                        }
                      }
                    }
                  }
                }
              }
            }
          }
        }
      }
      
      }
    
    
  
    })
  
  output$plot99 <- renderPlotly({
    #input$Temporada99
    dt1 <- d2021 %>% filter(Temporada==input$Temporada99) %>% group_by(Mandante) %>% summarise(PontMan=sum(PontMandante))
    dt2 <- d2021 %>% filter(Temporada==input$Temporada99) %>% group_by(Visitante) %>% summarise(PontVis=sum(PontVisitante))
    
    dt <- data.frame(dt1[,1:2],dt2[,2])
    
    dt$Pontos <- rowSums(dt[,2:3])
    
    tres <- dt %>% top_n(3, Pontos) %>%  arrange(desc(Pontos)) %>% head(3)
    names(tres) <- c("Time", "PontMan", "PontVis","Pontos") 
    
    
    
    ouro <- png::readPNG('Ouro.png')
    prata <- png::readPNG('Prata.png')
    bronze <- png::readPNG('Bronze.png')    
    #max(tres$Pontos)
    num <- sort(tres$Pontos, decreasing = T)
    
    plot <- tres %>% ggplot(aes(reorder(Time,-Pontos), Pontos,
                                fill=Pontos, 
                                text=paste("Time:", Time, "<br>", 
                                           "Pontuação: ", Pontos)))+
      geom_col(show.legend = FALSE)+
      theme_bw()+xlab(paste0("Maiores pontuadores do Temporada ", input$Temporada99))+
      geom_text(aes(label=Pontos),nudge_y = 1)
    
    ggplotly(plot, tooltip = "text", width = 600, height = 600)%>% 
      layout(images = list(list(
        source = raster2uri(as.raster(ouro)),
        x = 0.75, y = (head(num)[1]-15), 
        sizex = 0.5, sizey = 15.1,
        xref = "x", yref = "y",
        xanchor = "left", yanchor = "bottom",
        sizing = "stretch"
      ), list(
        source = raster2uri(as.raster(prata)),
        x = 1.75, y = (head(num)[2]-15), 
        sizex = 0.5, sizey = 15.1,
        xref = "x", yref = "y",
        xanchor = "left", yanchor = "bottom",
        sizing = "stretch"
      ), list(
        source = raster2uri(as.raster(bronze)),
        x = 2.75, y = (head(num)[3]-15), 
        sizex = 0.5, sizey = 15.1,
        xref = "x", yref = "y",
        xanchor = "left", yanchor = "bottom",
        sizing = "stretch"
      )),
      showlegend = FALSE, 
      title = list(text = paste0('Os três primeiros colocados',
                                 '<br>',
                                 '<sup>',
                                 'Campeonato Brasileiro',
                                 '</sup>')), 
      margin=0) %>%
      style(textposition = "right")
    
  }) 
  
  output$plot98 <- renderPlotly({
    dt3 <- d2021 %>% filter(Temporada==input$Temporada98) %>% group_by(Mandante) %>% summarise(PontMan=sum(PontMandante))
    dt4 <- d2021 %>% filter(Temporada==input$Temporada98) %>% group_by(Visitante) %>% summarise(PontVis=sum(PontVisitante))
    
    dt4 <- data.frame(dt3[,1:2],dt4[,2])
    
    dt4$Pontos <- rowSums(dt4[,2:3])
    
    quatro <- dt4 %>% top_n(4, -Pontos) %>%  arrange(desc(Pontos)) %>% head(4)
    names(quatro) <- c("Time", "PontMan", "PontVis","Pontos") 
    
    plot <- quatro %>% ggplot(aes(reorder(Time,-Pontos), Pontos,
                                  fill=Pontos, 
                                  text=paste("Time:", Time, "<br>", 
                                             "Pontuação: ", Pontos)))+
      geom_col(show.legend = FALSE)+
      theme_bw()+xlab(paste0("Menores pontuadores do Temporada ", input$Temporada98))+
      geom_text(aes(label=Pontos),nudge_y = 1)
    
    ggplotly(plot, tooltip = "text", width = 600, height = 600)
    
    
    
    
  })     
  
  
  
  
  
  
  
  
  ################################# GRAFICO VITORIAS ###################
  
  
  
  output$plot6 <- renderPlotly({
    
    if(input$Temporada22=="Todas Temporada") {
      
      d2021$vitMan <- ifelse(d2021$Mandante==d2021$Vencedor,1,0)
      testeM <- d2021 %>% filter(Vencedor!="EMPATE") %>% 
        group_by(Mandante) %>% summarise(vitM=sum(vitMan))
      names(testeM) <- c("Time","VitM")
      plot6 <- testeM %>%  ggplot(aes(VitM,reorder(Time,VitM),fill=Time, text=paste("Núm. de Vitórias =", VitM, "<br>",
                                                                                    "Time = ", Time)))+
        geom_col(show.legend = FALSE)+
        theme(panel.background = element_rect(fill = "white", colour = "black")) +
        theme(panel.grid.major = element_line(colour = "Black", linetype = "solid")) + 
        xlab("Número de vitórias como mandante no Brasileirão")+
        ylab("Times que disputaram o Brasileirão")+
        geom_text(aes(label=VitM),nudge_x = 0.2)+
        theme_classic()+
        ggtitle("Quantidades de vitórias como mandante no Brasileirão")
      ggplotly(plot6, tooltip = "text") %>% layout(showlegend = FALSE) %>% style(textposition = "right")
    }
    
    else
    {d2021$vitMan <- ifelse(d2021$Mandante==d2021$Vencedor,1,0)
    testeM <- d2021 %>% filter(Temporada==input$Temporada22,Vencedor!="EMPATE") %>% 
      group_by(Mandante) %>% summarise(vitM=sum(vitMan))
    names(testeM) <- c("Time","VitM")
    plot6 <- testeM %>%  ggplot(aes(VitM,reorder(Time,VitM),fill=VitM, text=paste("Núm. de Vitórias =", VitM, "<br>",
                                                                                  "Time = ", Time)))+
      geom_col(show.legend = FALSE)+
      theme(panel.background = element_rect(fill = "white", colour = "black")) +
      theme(panel.grid.major = element_line(colour = "Black", linetype = "solid")) + 
      xlab("Número de vitórias como mandante no Brasileirão")+
      ylab("Times que disputaram o Brasileirão")+
      geom_text(aes(label=VitM),nudge_x = 0.2)+
      theme_classic()+
      ggtitle("Quantidades de vitórias como mandante no Brasileirão")
    ggplotly(plot6, tooltip = "text") %>% layout(showlegend = FALSE) %>% style(textposition = "right")
    }
    
  }) 
  
  output$plot7 <- renderPlotly({
    
    if(input$Temporada42=="Todas Temporada") {
      
      d2021$vitVist <- ifelse(d2021$Visitante==d2021$Vencedor,1,0)
      testeV <- d2021 %>% filter(Vencedor!="EMPATE") %>% 
        group_by(Visitante) %>% summarise(vitV=sum(vitVist))
      names(testeV) <- c("Time","VitV")
      plot7 <- testeV %>%  ggplot(aes(VitV,reorder(Time,VitV),fill=Time, text=paste("Núm. de Vitórias =", VitV, "<br>",
                                                                                    "Time = ", Time)))+
        geom_col(show.legend = FALSE)+
        theme(panel.background = element_rect(fill = "white", colour = "black")) +
        theme(panel.grid.major = element_line(colour = "Black", linetype = "solid")) + 
        xlab("Número de vitórias como visitante no Brasileirão")+
        ylab("Times que disputaram o Brasileirão")+
        geom_text(aes(label=VitV),nudge_x = 0.2)+
        theme_classic()+
        ggtitle("Quantidades de vitórias como visitante no Brasileirão")
      ggplotly(plot7, tooltip = "text") %>% layout(showlegend = FALSE) %>% style(textposition = "right")
    }
    
    else {
      
      d2021$vitVist <- ifelse(d2021$Visitante==d2021$Vencedor,1,0)
      testeV <- d2021 %>% filter(Temporada==input$Temporada42,Vencedor!="EMPATE") %>% 
        group_by(Visitante) %>% summarise(vitV=sum(vitVist))
      names(testeV) <- c("Time","VitV")
      plot7 <- testeV %>%  ggplot(aes(VitV,reorder(Time,VitV),fill=VitV, text=paste("Núm. de Vitórias =", VitV, "<br>",
                                                                                    "Time = ", Time)))+
        geom_col(show.legend = FALSE)+
        theme(panel.background = element_rect(fill = "white", colour = "black")) +
        theme(panel.grid.major = element_line(colour = "Black", linetype = "solid")) + 
        xlab("Número de vitórias como visitante no Brasileirão")+
        ylab("Times que disputaram o Brasileirão")+
        geom_text(aes(label=VitV),nudge_x = 0.2)+
        theme_classic()+
        ggtitle("Quantidades de vitórias como visitante no Brasileirão")
      ggplotly(plot7, tooltip = "text") %>% layout(showlegend = FALSE) %>% style(textposition = "right")
      
    }
  }) 
  
  output$plot22 <- renderPlotly({
    
    if(input$Temporada2=="Todas Temporada") {
      
      plot22 <- d2021 %>% filter(Vencedor!="EMPATE") %>% 
        
        group_by(Vencedor) %>% summarise(Quant=n(),.groups="drop") %>%  ggplot(aes(Quant,reorder(Vencedor,Quant),fill=Vencedor,
                                                                                   text=paste("Núm. de Vitórias =", Quant, "<br>",
                                                                                              "Time = ", Vencedor)))+
        geom_col(show.legend = FALSE)+
        theme(panel.background = element_rect(fill = "white", colour = "black")) +
        theme(panel.grid.major = element_line(colour = "Black", linetype = "solid")) + 
        xlab("Número de vitórias gerais")+
        ylab("Times que disputaram o Brasileirão")+
        theme_classic()+
        geom_text(aes(label=Quant),nudge_x = 0.2)+
        ggtitle("Quantidade total de vitórias dos times nas edições do Brasileirão")
      ggplotly(plot22, tooltip = "text") %>% layout(showlegend = FALSE) %>% style(textposition = "right")
    } 
    else {
      plot22 <- d2021 %>% filter(Vencedor!="EMPATE") %>% 
        filter(Temporada==input$Temporada2)%>%
        group_by(Vencedor) %>% summarise(Quant=n(),.groups="drop") %>%  ggplot(aes(Quant,reorder(Vencedor,Quant),fill=Quant,
                                                                                   text=paste("Núm. de Vitórias =", Quant, "<br>",
                                                                                              "Time = ", Vencedor)))+
        geom_col(show.legend = FALSE)+
        theme(panel.background = element_rect(fill = "white", colour = "black")) +
        theme(panel.grid.major = element_line(colour = "Black", linetype = "solid")) + 
        xlab("Número de vitórias gerais")+
        ylab("Times que disputaram o Brasileirão")+
        theme_classic()+
        geom_text(aes(label=Quant),nudge_x = 0.2)+
        ggtitle("Quantidade total de vitórias dos times nas edições do Brasileirão")
      ggplotly(plot22, tooltip = "text") %>% layout(showlegend = FALSE) %>% style(textposition = "right")
    } 
  })    
  
  
  
  ################################# GRAFICO EMPATES ###################
  
  
  output$plot9 <- renderPlotly({
    
    if(input$Temporada91=="Todas Temporada") {
      
      empate <- d2021 %>% filter(Vencedor=="EMPATE") %>%
        group_by(Mandante,Visitante,Vencedor) %>% summarise(Quant=n(),.groups="drop")
      
      empate$emm <- ifelse(empate$Mandante!=empate$Vencedor,1,0)
      emm <- empate %>%  
        group_by(Mandante) %>% summarise(eee=sum(emm))
      names(emm) <- c("Time","EmpM")
      
      empate$emv <- ifelse(empate$Visitante!=empate$Vencedor,1,0)
      emv <- empate %>%  
        group_by(Visitante) %>% summarise(ee=sum(emv))
      names(emv) <- c("Time","EmpV")
      
      totEmpate <- left_join(emv,emm,by="Time")
      totEmpate$Total <- rowSums(totEmpate[,2:3])
      
      
      plot9 <- totEmpate %>%  ggplot(aes(Total,reorder(Time,Total),fill=Time, text=paste("Núm. de Empates =", Total, "<br>",
                                                                                         "Time = ", Time)))+
        geom_col(show.legend = FALSE)+
        theme(panel.background = element_rect(fill = "white", colour = "black")) +
        theme(panel.grid.major = element_line(colour = "Black", linetype = "solid")) + 
        xlab("Número de empates gerais")+
        ylab("Times que disputaram o Brasileirão")+
        theme_classic()+
        geom_text(aes(label=Total),nudge_x = 0.2)+
        ggtitle("Quantidade total de empates dos times nas edições do Brasileirão")
      ggplotly(plot9, tooltip = "text") %>% layout(showlegend = FALSE) %>% style(textposition = "right")
    }
    else{
      
      empate <- d2021 %>% filter(Temporada==input$Temporada91,Vencedor=="EMPATE") %>%
        group_by(Mandante,Visitante,Vencedor) %>% summarise(Quant=n(),.groups="drop")
      
      empate$emm <- ifelse(empate$Mandante!=empate$Vencedor,1,0)
      emm <- empate %>%  
        group_by(Mandante) %>% summarise(eee=sum(emm))
      names(emm) <- c("Time","EmpM")
      
      empate$emv <- ifelse(empate$Visitante!=empate$Vencedor,1,0)
      emv <- empate %>%  
        group_by(Visitante) %>% summarise(ee=sum(emv))
      names(emv) <- c("Time","EmpV")
      
      totEmpate <- left_join(emv,emm,by="Time")
      totEmpate$Total <- rowSums(totEmpate[,2:3])
      
      plot9 <- totEmpate %>%  ggplot(aes(Total,reorder(Time,Total),fill=Total, text=paste("Núm. de Empates =", Total, "<br>",
                                                                                          "Time = ", Time)))+
        geom_col(show.legend = FALSE)+
        theme(panel.background = element_rect(fill = "white", colour = "black")) +
        theme(panel.grid.major = element_line(colour = "Black", linetype = "solid")) + 
        xlab("Número de empates gerais")+
        ylab("Times que disputaram o Brasileirão")+
        theme_classic()+
        geom_text(aes(label=Total),nudge_x = 0.2)+
        ggtitle("Quantidade total de empates dos times nas edições do Brasileirão")
      ggplotly(plot9, tooltip = "text") %>% layout(showlegend = FALSE) %>% style(textposition = "right")
    }
    
    
  }) 
  
  output$plot10 <- renderPlotly({
    
    if(input$Temporada92=="Todas Temporada") {
      empate <- d2021 %>% filter(Vencedor=="EMPATE") %>%
        group_by(Mandante,Visitante,Vencedor) %>% summarise(Quant=n(),.groups="drop")
      
      empate$emm <- ifelse(empate$Mandante!=empate$Vencedor,1,0)
      emm <- empate %>%  
        group_by(Mandante) %>% summarise(eee=sum(emm))
      names(emm) <- c("Time","EmpM")
      
      
      plot10 <- emm %>%  ggplot(aes(EmpM,reorder(Time,EmpM),fill=Time, text=paste("Núm. de Empates =", EmpM, "<br>",
                                                                                  "Time = ", Time)))+
        geom_col(show.legend = FALSE)+
        theme(panel.background = element_rect(fill = "white", colour = "black")) +
        theme(panel.grid.major = element_line(colour = "Black", linetype = "solid")) + 
        xlab("Número de empates como mandante no Brasileirão")+
        ylab("Times que disputaram o Brasileirão")+
        geom_text(aes(label=EmpM),nudge_x = 0.2)+
        theme_classic()+
        ggtitle("Quantidades de empates como mandante no Brasileirão")
      ggplotly(plot10, tooltip = "text") %>% layout(showlegend = FALSE) %>% style(textposition = "right")
      
    }
    
    else {
      empate <- d2021 %>% filter(Temporada==input$Temporada92,Vencedor=="EMPATE") %>%
        group_by(Mandante,Visitante,Vencedor) %>% summarise(Quant=n(),.groups="drop")
      
      empate$emm <- ifelse(empate$Mandante!=empate$Vencedor,1,0)
      emm <- empate %>%  
        group_by(Mandante) %>% summarise(eee=sum(emm))
      names(emm) <- c("Time","EmpM")
      
      
      plot10 <- emm %>%  ggplot(aes(EmpM,reorder(Time,EmpM),fill=EmpM, text=paste("Núm. de Empates =", EmpM, "<br>",
                                                                                  "Time = ", Time)))+
        geom_col(show.legend = FALSE)+
        theme(panel.background = element_rect(fill = "white", colour = "black")) +
        theme(panel.grid.major = element_line(colour = "Black", linetype = "solid")) + 
        xlab("Número de empates como mandante no Brasileirão")+
        ylab("Times que disputaram o Brasileirão")+
        geom_text(aes(label=EmpM),nudge_x = 0.2)+
        theme_classic()+
        ggtitle("Quantidades de empates como mandante no Brasileirão")
      ggplotly(plot10, tooltip = "text") %>% layout(showlegend = FALSE) %>% style(textposition = "right")
    }
    
  }) 
  
  output$plot11 <- renderPlotly({
    
    if(input$Temporada93=="Todas Temporada"){
      empate <- d2021 %>% filter(Vencedor=="EMPATE") %>%
        group_by(Mandante,Visitante,Vencedor) %>% summarise(Quant=n(),.groups="drop")
      
      empate$emv <- ifelse(empate$Visitante!=empate$Vencedor,1,0)
      emv <- empate %>%  
        group_by(Visitante) %>% summarise(ee=sum(emv))
      names(emv) <- c("Time","EmpV")
      
      
      plot11 <- emv %>%  ggplot(aes(EmpV,reorder(Time,EmpV),fill=Time, text=paste("Núm. de Empates =", EmpV, "<br>",
                                                                                  "Time = ", Time)))+
        geom_col(show.legend = FALSE)+
        theme(panel.background = element_rect(fill = "white", colour = "black")) +
        theme(panel.grid.major = element_line(colour = "Black", linetype = "solid")) + 
        xlab("Número de empates como visitante no Brasileirão")+
        ylab("Times que disputaram o Brasileirão")+
        geom_text(aes(label=EmpV),nudge_x = 0.2)+
        theme_classic()+
        ggtitle("Quantidades de empates como visitante no Brasileirão")
      ggplotly(plot11, tooltip = "text") %>% layout(showlegend = FALSE) %>% style(textposition = "right")
      
      
    }
    
    else{ 
      empate <- d2021 %>% filter(Temporada==input$Temporada93,Vencedor=="EMPATE") %>%
        group_by(Mandante,Visitante,Vencedor) %>% summarise(Quant=n(),.groups="drop")
      
      empate$emv <- ifelse(empate$Visitante!=empate$Vencedor,1,0)
      emv <- empate %>%  
        group_by(Visitante) %>% summarise(ee=sum(emv))
      names(emv) <- c("Time","EmpV")
      
      
      plot11 <- emv %>%  ggplot(aes(EmpV,reorder(Time,EmpV),fill=EmpV, text=paste("Núm. de Empates =", EmpV, "<br>",
                                                                                  "Time = ", Time)))+
        geom_col(show.legend = FALSE)+
        theme(panel.background = element_rect(fill = "white", colour = "black")) +
        theme(panel.grid.major = element_line(colour = "Black", linetype = "solid")) + 
        xlab("Número de empates como visitante no Brasileirão")+
        ylab("Times que disputaram o Brasileirão")+
        geom_text(aes(label=EmpV),nudge_x = 0.2)+
        theme_classic()+
        ggtitle("Quantidades de empates como visitante no Brasileirão")
      ggplotly(plot11, tooltip = "text") %>% layout(showlegend = FALSE) %>% style(textposition = "right")
      
      
    }
  })     
  
  
  ################################# GRAFICO DERROTAS ###################
  
  
  output$plot23 <- renderPlotly({
    
    if(input$Temporada3=="Todas Temporada") {
      
      derotpt <- d2021 %>% 
        filter(Vencedor!="EMPATE") %>%  
        group_by(Derrotado) %>% summarise(Quant=n(),.groups="drop")
      plot23 <- derotpt %>%  ggplot(aes(Quant,reorder(Derrotado,Quant),fill=Derrotado, text=paste("Núm. de Derrotas =", Quant, "<br>",
                                                                                                  "Derrotado = ", Derrotado)))+
        geom_col(show.legend = FALSE)+
        theme(panel.background = element_rect(fill = "white", colour = "black")) +
        theme(panel.grid.major = element_line(colour = "Black", linetype = "solid")) + 
        xlab("Número de derrotas gerais")+
        ylab("Times que disputaram o Brasileirão")+
        theme_classic()+
        geom_text(aes(label=Quant),nudge_x = 1)+
        ggtitle("Quantidade total de derrotas dos times nas edições do Brasileirão ")
      ggplotly(plot23, tooltip = "text") %>% layout(showlegend = FALSE) %>% style(textposition = "right")
    }   
    else{
      
      derotpt <- d2021 %>% 
        filter(Temporada==input$Temporada3, Vencedor!="EMPATE") %>%  
        group_by(Derrotado) %>% summarise(Quant=n(),.groups="drop")
      plot23 <- derotpt %>%  ggplot(aes(Quant,reorder(Derrotado,Quant),fill=Quant, text=paste("Núm. de Derrotas =", Quant, "<br>",
                                                                                              "Derrotado = ", Derrotado)))+
        geom_col(show.legend = FALSE)+
        theme(panel.background = element_rect(fill = "white", colour = "black")) +
        theme(panel.grid.major = element_line(colour = "Black", linetype = "solid")) + 
        xlab("Número de derrotas gerais")+
        ylab("Times que disputaram o Brasileirão")+
        theme_classic()+
        geom_text(aes(label=Quant),nudge_x = 1)+
        ggtitle("Quantidade total de derrotas dos times nas edições do Brasileirão ")
      ggplotly(plot23, tooltip = "text") %>% layout(showlegend = FALSE) %>% style(textposition = "right")
    }   
    
    
  })
  
  output$plot13 <- renderPlotly({
    
    if(input$Temporada95=="Todas Temporada") {
      
      d2021$derMand <- ifelse(d2021$Mandante==d2021$Derrotado,1,0)
      testederM <- d2021 %>% filter(Derrotado!="EMPATE")%>%
        #filter(Temporada=="2019",Derrotado!="EMPATE")%>% 
        group_by(Mandante) %>% summarise(dm=sum(derMand))
      names(testederM) <- c("Time","DerM")
      
      plot13 <- testederM %>%  ggplot(aes(DerM,reorder(Time,DerM),fill=Time, text=paste("Núm. de Derrotas =", DerM, "<br>",
                                                                                        "Time = ", Time)))+
        geom_col(show.legend = FALSE)+
        theme(panel.background = element_rect(fill = "white", colour = "black")) +
        theme(panel.grid.major = element_line(colour = "Black", linetype = "solid")) + 
        xlab("Número de derrotas como mandante no Brasileirão")+
        ylab("Times que disputaram o Brasileirão")+
        geom_text(aes(label=DerM),nudge_x = 0.2)+
        theme_classic()+
        ggtitle("Quantidades de derrotas como mandante no Brasileirão")
      ggplotly(plot13, tooltip = "text") %>% layout(showlegend = FALSE) %>% style(textposition = "right")
    }
    else {
      d2021$derMand <- ifelse(d2021$Mandante==d2021$Derrotado,1,0)
      testederM <- d2021 %>% filter(Temporada==input$Temporada95,Derrotado!="EMPATE")%>%
        group_by(Mandante) %>% summarise(dm=sum(derMand))
      names(testederM) <- c("Time","DerM")
      
      plot13 <- testederM %>%  ggplot(aes(DerM,reorder(Time,DerM),fill=DerM, text=paste("Núm. de Derrotas =", DerM, "<br>",
                                                                                        "Time = ", Time)))+
        geom_col(show.legend = FALSE)+
        theme(panel.background = element_rect(fill = "white", colour = "black")) +
        theme(panel.grid.major = element_line(colour = "Black", linetype = "solid")) + 
        xlab("Número de derrotas como mandante no Brasileirão")+
        ylab("Times que disputaram o Brasileirão")+
        geom_text(aes(label=DerM),nudge_x = 0.2)+
        theme_classic()+
        ggtitle("Quantidades de derrotas como mandante no Brasileirão")
      ggplotly(plot13, tooltip = "text") %>% layout(showlegend = FALSE) %>% style(textposition = "right")
    }
  }) 
  
  output$plot14 <- renderPlotly({
    if(input$Temporada94=="Todas Temporada") {
      
      d2021$derVist <- ifelse(d2021$Visitante==d2021$Derrotado,1,0)
      testederV <- d2021 %>% filter(Derrotado!="EMPATE")%>%  
        #filter(Temporada=="2019",Derrotado!="EMPATE")%>%  
        group_by(Visitante) %>% summarise(dV=sum(derVist))
      names(testederV) <- c("Time","DerV")
      
      plot14 <- testederV %>%  ggplot(aes(DerV,reorder(Time,DerV),fill=Time, text=paste("Núm. de Derrotas =", DerV, "<br>",
                                                                                        "Time = ", Time)))+
        geom_col(show.legend = FALSE)+
        theme(panel.background = element_rect(fill = "white", colour = "black")) +
        theme(panel.grid.major = element_line(colour = "Black", linetype = "solid")) + 
        xlab("Número de derrotas como visitante no Brasileirão")+
        ylab("Times que disputaram o Brasileirão")+
        geom_text(aes(label=DerV),nudge_x = 0.2)+
        theme_classic()+
        ggtitle("Quantidades de derrotas como visitante no Brasileirão")
      ggplotly(plot14, tooltip = "text") %>% layout(showlegend = FALSE) %>% style(textposition = "right")
    }
    else{
      d2021$derVist <- ifelse(d2021$Visitante==d2021$Derrotado,1,0)
      testederV <- d2021 %>% filter(Temporada==input$Temporada94,Derrotado!="EMPATE")%>%  
        group_by(Visitante) %>% summarise(dV=sum(derVist))
      names(testederV) <- c("Time","DerV")
      
      plot14 <- testederV %>%  ggplot(aes(DerV,reorder(Time,DerV),fill=DerV, text=paste("Núm. de Derrotas =", DerV, "<br>",
                                                                                        "Time = ", Time)))+
        geom_col(show.legend = FALSE)+
        theme(panel.background = element_rect(fill = "white", colour = "black")) +
        theme(panel.grid.major = element_line(colour = "Black", linetype = "solid")) + 
        xlab("Número de derrotas como visitante no Brasileirão")+
        ylab("Times que disputaram o Brasileirão")+
        geom_text(aes(label=DerV),nudge_x = 0.2)+
        theme_classic()+
        ggtitle("Quantidades de derrotas como visitante no Brasileirão")
      ggplotly(plot14, tooltip = "text") %>% layout(showlegend = FALSE) %>% style(textposition = "right")
    }
  })     
  
  ################################# GRAFICO GOLS ###################
  
  
  output$plot16 <- renderPlotly({
    if(input$Temporada84=="Todas Temporada") {
      
      mandanteNG <- d2021 %>%  
        #filter(Temporada=="2019")%>% 
        group_by(Mandante) %>% summarise(golsM=sum(GolsMan))
      names(mandanteNG) <- c("Time","GolsM")
      
      
      plot16 <-  mandanteNG %>%  ggplot(aes(GolsM,reorder(Time,GolsM),fill=Time, text=paste("Núm. de Gols =", GolsM, "<br>",
                                                                                            "Time = ", Time)))+
        geom_col(show.legend = FALSE)+
        theme(panel.background = element_rect(fill = "white", colour = "black")) +
        theme(panel.grid.major = element_line(colour = "Black", linetype = "solid")) + 
        xlab("Número de gols no Brasileirão como mandante")+
        ylab("Times que disputaram o Brasileirão")+
        geom_text(aes(label=GolsM),nudge_x = 1)+
        theme_classic()+
        ggtitle("Quantidade de gols dos times como mandante no Brasileirão")
      ggplotly(plot16, tooltip = "text") %>% layout(showlegend = FALSE) %>% style(textposition = "right")
    }
    
    else{
      mandanteNG <- d2021 %>%  
        filter(Temporada==input$Temporada84)%>% 
        group_by(Mandante) %>% summarise(golsM=sum(GolsMan))
      names(mandanteNG) <- c("Time","GolsM")
      
      
      plot16 <-  mandanteNG %>%  ggplot(aes(GolsM,reorder(Time,GolsM),fill=GolsM, text=paste("Núm. de Gols =", GolsM, "<br>",
                                                                                             "Time = ", Time)))+
        geom_col(show.legend = FALSE)+
        theme(panel.background = element_rect(fill = "white", colour = "black")) +
        theme(panel.grid.major = element_line(colour = "Black", linetype = "solid")) + 
        xlab("Número de gols no Brasileirão como mandante")+
        ylab("Times que disputaram o Brasileirão")+
        geom_text(aes(label=GolsM),nudge_x = 1)+
        theme_classic()+
        ggtitle("Quantidade de gols dos times como mandante no Brasileirão")
      ggplotly(plot16, tooltip = "text") %>% layout(showlegend = FALSE) %>% style(textposition = "right")
    }
  }) 
  
  output$plot17 <- renderPlotly({
    if(input$Temporada85=="Todas Temporada") {
      
      visitanteNG <- d2021 %>% 
        #filter(Temporada=="2019")%>% 
        group_by(Visitante) %>% summarise(golsM=sum(GolsVisit))
      names(visitanteNG) <- c("Time","GolsV")
      
      
      plot17 <- visitanteNG %>%  ggplot(aes(GolsV,reorder(Time,GolsV),fill=Time, text=paste("Núm. de Gols =", GolsV, "<br>",
                                                                                            "Time = ", Time)))+
        geom_col(show.legend = FALSE)+
        theme(panel.background = element_rect(fill = "white", colour = "black")) +
        theme(panel.grid.major = element_line(colour = "Black", linetype = "solid")) + 
        xlab("Número de gols no Brasileirão como visitante")+
        ylab("Times que disputaram o Brasileirão")+
        geom_text(aes(label=GolsV),nudge_x = 1)+
        theme_classic()+
        ggtitle("Quantidade de gols dos times como visitantes no Brasileirão")
      ggplotly(plot17, tooltip = "text") %>% layout(showlegend = FALSE) %>% style(textposition = "right")
    }
    else{
      visitanteNG <- d2021 %>% 
        filter(Temporada==input$Temporada85)%>% 
        group_by(Visitante) %>% summarise(golsM=sum(GolsVisit))
      names(visitanteNG) <- c("Time","GolsV")
      
      
      plot17 <- visitanteNG %>%  ggplot(aes(GolsV,reorder(Time,GolsV),fill=GolsV, text=paste("Núm. de Gols =", GolsV, "<br>",
                                                                                             "Time = ", Time)))+
        geom_col(show.legend = FALSE)+
        theme(panel.background = element_rect(fill = "white", colour = "black")) +
        theme(panel.grid.major = element_line(colour = "Black", linetype = "solid")) + 
        xlab("Número de gols no Brasileirão como visitante")+
        ylab("Times que disputaram o Brasileirão")+
        geom_text(aes(label=GolsV),nudge_x = 1)+
        theme_classic()+
        ggtitle("Quantidade de gols dos times como visitantes no Brasileirão")
      ggplotly(plot17, tooltip = "text") %>% layout(showlegend = FALSE) %>% style(textposition = "right")
    }
  })     
  
  
  output$plot59 <- renderPlotly({
    if(input$Temporada59=="Todas Temporada") {
      
      mandanteNG <- d2021 %>%  
        #filter(Temporada=="2019")%>% 
        group_by(Mandante) %>% summarise(golsM=sum(GolsMan))
      names(mandanteNG) <- c("Time","GolsM")
      
      visitanteNG <- d2021 %>% 
        #filter(Temporada=="2019")%>% 
        group_by(Visitante) %>% summarise(golsM=sum(GolsVisit))
      names(visitanteNG) <- c("Time","GolsV")
      
      
      totGols <- left_join(mandanteNG,visitanteNG,by="Time")
      totGols$Total <- rowSums(totGols[,2:3])
      
      plot59 <- totGols %>%  ggplot(aes(Total,reorder(Time,Total),fill=Time, text=paste("Núm. de Gols =", Total, "<br>",
                                                                                        "Time = ", Time)))+
        geom_col(show.legend = FALSE)+
        theme(panel.background = element_rect(fill = "white", colour = "black")) +
        theme(panel.grid.major = element_line(colour = "Black", linetype = "solid")) + 
        xlab("Número de gols no Brasileirão")+
        ylab("Times que disputaram o Brasileirão")+
        geom_text(aes(label=Total),nudge_x = 1)+
        theme_classic()+
        ggtitle("Quantidade total de gols dos times nas edições do Brasileirão")
      ggplotly(plot59, tooltip = "text") %>% layout(showlegend = FALSE) %>% style(textposition = "right")
    }
    else{ 
      mandanteNG <- d2021 %>%  
        filter(Temporada==input$Temporada59)%>% 
        group_by(Mandante) %>% summarise(golsM=sum(GolsMan))
      names(mandanteNG) <- c("Time","GolsM")
      
      visitanteNG <- d2021 %>% 
        filter(Temporada==input$Temporada59)%>% 
        group_by(Visitante) %>% summarise(golsM=sum(GolsVisit))
      names(visitanteNG) <- c("Time","GolsV")
      
      
      totGols <- left_join(mandanteNG,visitanteNG,by="Time")
      totGols$Total <- rowSums(totGols[,2:3])
      
      plot59 <- totGols %>%  ggplot(aes(Total,reorder(Time,Total),fill=Total, text=paste("Núm. de Gols =", Total, "<br>",
                                                                                         "Time = ", Time)))+
        geom_col(show.legend = FALSE)+
        theme(panel.background = element_rect(fill = "white", colour = "black")) +
        theme(panel.grid.major = element_line(colour = "Black", linetype = "solid")) + 
        xlab("Número de gols no Brasileirão")+
        ylab("Times que disputaram o Brasileirão")+
        geom_text(aes(label=Total),nudge_x = 1)+
        theme_classic()+
        ggtitle("Quantidade total de gols dos times nas edições do Brasileirão")
      ggplotly(plot59, tooltip = "text") %>% layout(showlegend = FALSE) %>% style(textposition = "right")
    }
  })
  
  
  ################################# GRAFICO GOLS SO ###################
  
  
  output$plot300 <- renderPlotly({
    if(input$Temporada300=="Todas Temporada") {
      
      mandanteNGS <- d2021 %>%  
        #filter(Temporada=="2019")%>% 
        group_by(Mandante) %>% summarise(golsM=sum(GolsVisit))
      names(mandanteNGS) <- c("Time","GolsM")
      
      
      plot300 <-  mandanteNGS %>%  ggplot(aes(GolsM,reorder(Time,GolsM),fill=Time, text=paste("Núm. de Gols =", GolsM, "<br>",
                                                                                            "Time = ", Time)))+
        geom_col(show.legend = FALSE)+
        theme(panel.background = element_rect(fill = "white", colour = "black")) +
        theme(panel.grid.major = element_line(colour = "Black", linetype = "solid")) + 
        xlab("Número de gols sofridos no Brasileirão como mandante")+
        ylab("Times que disputaram o Brasileirão")+
        geom_text(aes(label=GolsM),nudge_x = 1)+
        theme_classic()+
        ggtitle("Quantidade de gols sofridos dos times como mandante no Brasileirão")
      ggplotly(plot300, tooltip = "text") %>% layout(showlegend = FALSE) %>% style(textposition = "right")
    }
    
    else{
      mandanteNGS <- d2021 %>%  
        filter(Temporada==input$Temporada300)%>% 
        group_by(Mandante) %>% summarise(golsM=sum(GolsVisit))
      names(mandanteNGS) <- c("Time","GolsM")
      
      
      plot300 <-  mandanteNGS %>%  ggplot(aes(GolsM,reorder(Time,GolsM),fill=GolsM, text=paste("Núm. de Gols =", GolsM, "<br>",
                                                                                             "Time = ", Time)))+
        geom_col(show.legend = FALSE)+
        theme(panel.background = element_rect(fill = "white", colour = "black")) +
        theme(panel.grid.major = element_line(colour = "Black", linetype = "solid")) + 
        xlab("Número de gols sofridos no Brasileirão como mandante")+
        ylab("Times que disputaram o Brasileirão")+
        geom_text(aes(label=GolsM),nudge_x = 1)+
        theme_classic()+
        ggtitle("Quantidade de gols sofridos dos times como mandante no Brasileirão")
      ggplotly(plot300, tooltip = "text") %>% layout(showlegend = FALSE) %>% style(textposition = "right")
    }
  }) 
  
  output$plot301 <- renderPlotly({
    if(input$Temporada301=="Todas Temporada") {
      
      visitanteNGS <- d2021 %>% 
        #filter(Temporada=="2019")%>% 
        group_by(Visitante) %>% summarise(golsM=sum(GolsMan))
      names(visitanteNGS) <- c("Time","GolsV")
      
      
      plot301 <- visitanteNGS %>%  ggplot(aes(GolsV,reorder(Time,GolsV),fill=Time, text=paste("Núm. de Gols =", GolsV, "<br>",
                                                                                            "Time = ", Time)))+
        geom_col(show.legend = FALSE)+
        theme(panel.background = element_rect(fill = "white", colour = "black")) +
        theme(panel.grid.major = element_line(colour = "Black", linetype = "solid")) + 
        xlab("Número de gols sofridos no Brasileirão como visitante")+
        ylab("Times que disputaram o Brasileirão")+
        geom_text(aes(label=GolsV),nudge_x = 1)+
        theme_classic()+
        ggtitle("Quantidade de gols sofridos dos times como visitantes no Brasileirão")
      ggplotly(plot301, tooltip = "text") %>% layout(showlegend = FALSE) %>% style(textposition = "right")
    }
    else{
      visitanteNGS <- d2021 %>% 
        filter(Temporada==input$Temporada301)%>% 
        group_by(Visitante) %>% summarise(golsM=sum(GolsMan))
      names(visitanteNGS) <- c("Time","GolsV")
      
      
      plot301 <- visitanteNGS %>%  ggplot(aes(GolsV,reorder(Time,GolsV),fill=GolsV, text=paste("Núm. de Gols =", GolsV, "<br>",
                                                                                             "Time = ", Time)))+
        geom_col(show.legend = FALSE)+
        theme(panel.background = element_rect(fill = "white", colour = "black")) +
        theme(panel.grid.major = element_line(colour = "Black", linetype = "solid")) + 
        xlab("Número de gols sofridos no Brasileirão como visitante")+
        ylab("Times que disputaram o Brasileirão")+
        geom_text(aes(label=GolsV),nudge_x = 1)+
        theme_classic()+
        ggtitle("Quantidade de gols sofridos dos times como visitantes no Brasileirão")
      ggplotly(plot301, tooltip = "text") %>% layout(showlegend = FALSE) %>% style(textposition = "right")
    }
  })     
  
  
  output$plot302 <- renderPlotly({
    if(input$Temporada302=="Todas Temporada") {
      
      mandanteNGS <- d2021 %>%  
        #filter(Temporada=="2019")%>% 
        group_by(Mandante) %>% summarise(golsM=sum(GolsVisit))
      names(mandanteNGS) <- c("Time","GolsM")
      
      visitanteNGS <- d2021 %>% 
        #filter(Temporada=="2019")%>% 
        group_by(Visitante) %>% summarise(golsM=sum(GolsMan))
      names(visitanteNGS) <- c("Time","GolsV")
      
      
      totGols <- left_join(mandanteNGS,visitanteNGS,by="Time")
      totGols$Total <- rowSums(totGols[,2:3])
      
      plot302 <- totGols %>%  ggplot(aes(Total,reorder(Time,Total),fill=Time, text=paste("Núm. de Gols =", Total, "<br>",
                                                                                         "Time = ", Time)))+
        geom_col(show.legend = FALSE)+
        theme(panel.background = element_rect(fill = "white", colour = "black")) +
        theme(panel.grid.major = element_line(colour = "Black", linetype = "solid")) + 
        xlab("Número de gols sofridos no Brasileirão")+
        ylab("Times que disputaram o Brasileirão")+
        geom_text(aes(label=Total),nudge_x = 1)+
        theme_classic()+
        ggtitle("Quantidade total de gols sofridos dos times nas edições do Brasileirão")
      ggplotly(plot302, tooltip = "text") %>% layout(showlegend = FALSE) %>% style(textposition = "right")
    }
    else{ 
      mandanteNGS <- d2021 %>%  
        filter(Temporada==input$Temporada302)%>% 
        group_by(Mandante) %>% summarise(golsM=sum(GolsVisit))
      names(mandanteNGS) <- c("Time","GolsM")
      
      visitanteNGS <- d2021 %>% 
        filter(Temporada==input$Temporada302)%>% 
        group_by(Visitante) %>% summarise(golsM=sum(GolsMan))
      names(visitanteNGS) <- c("Time","GolsV")
      
      
      totGols <- left_join(mandanteNGS,visitanteNGS,by="Time")
      totGols$Total <- rowSums(totGols[,2:3])
      
      plot302 <- totGols %>%  ggplot(aes(Total,reorder(Time,Total),fill=Total, text=paste("Núm. de Gols =", Total, "<br>",
                                                                                          "Time = ", Time)))+
        geom_col(show.legend = FALSE)+
        theme(panel.background = element_rect(fill = "white", colour = "black")) +
        theme(panel.grid.major = element_line(colour = "Black", linetype = "solid")) + 
        xlab("Número de gols sofridos no Brasileirão")+
        ylab("Times que disputaram o Brasileirão")+
        geom_text(aes(label=Total),nudge_x = 1)+
        theme_classic()+
        ggtitle("Quantidade total de gols sofridos dos times nas edições do Brasileirão")
      ggplotly(plot302, tooltip = "text") %>% layout(showlegend = FALSE) %>% style(textposition = "right")
    }
  })
  
  
  
  ################################# GRAFICO JOGOS ###################
  
  
  output$plot18 <- renderPlotly({
    
    jogosM <- d2021 %>% 
      #filter(Temporada=="2019")%>% 
      group_by(Mandante) %>% summarise(Quant=n(),.groups="drop")
    names(jogosM) <- c("Time","JogosM")
    
    jogosV <- d2021 %>% 
      #filter(Temporada=="2019")%>% 
      group_by(Visitante) %>% summarise(Quant=n(),.groups="drop")
    names(jogosV) <- c("Time","JogosV")
    
    totJogos <- left_join(jogosM,jogosV,by="Time")
    totJogos$Total <- rowSums(totJogos[,2:3])
    
    
    plot18 <- totJogos %>%  ggplot(aes(Total,reorder(Time,Total),fill=Time, text=paste("Núm. de Jogos =", Total, "<br>",
                                                                                       "Time = ", Time)))+
      geom_col(show.legend = FALSE)+
      theme(panel.background = element_rect(fill = "white", colour = "black")) +
      theme(panel.grid.major = element_line(colour = "Black", linetype = "solid")) + 
      xlab("Número de jogos gerais")+
      ylab("Times que disputaram o Brasileirão")+
      theme_classic()+
      geom_text(aes(label=Total),nudge_x = 1)+
      ggtitle("Quantidade total de jogos dos times nas edições do Brasileirão")
    ggplotly(plot18, tooltip = "text") %>% layout(showlegend = FALSE) %>% style(textposition = "right")
    
    
    
  }) 
  
  output$plot19 <- renderPlotly({
    
    jogosM <- d2021 %>% 
      #filter(Temporada=="2019")%>% 
      group_by(Mandante) %>% summarise(Quant=n(),.groups="drop")
    names(jogosM) <- c("Time","JogosM")
    
    plot19 <- jogosM %>%  ggplot(aes(JogosM,reorder(Time,JogosM),fill=Time, text=paste("Núm. de Jogos =", JogosM, "<br>",
                                                                                       "Time = ", Time)))+
      geom_col(show.legend = FALSE)+
      theme(panel.background = element_rect(fill = "white", colour = "black")) +
      theme(panel.grid.major = element_line(colour = "Black", linetype = "solid")) + 
      xlab("Número de jogos como mandante")+
      ylab("Equipes mandantes")+
      theme_classic()+
      geom_text(aes(label=JogosM),nudge_x = 1)+
      ggtitle("Quantidade de jogos dos times como mandante")
    ggplotly(plot19, tooltip = "text") %>% layout(showlegend = FALSE) %>% style(textposition = "right")
    
    
  }) 
  
  output$plot20 <- renderPlotly({
    
    jogosV <- d2021 %>% 
      #filter(Temporada=="2019")%>% 
      group_by(Visitante) %>% summarise(Quant=n(),.groups="drop")
    names(jogosV) <- c("Time","JogosV")
    
    plot20 <- jogosV %>%  ggplot(aes(JogosV,reorder(Time,JogosV),fill=Time, text=paste("Núm. de Empates =", JogosV, "<br>",
                                                                                       "Time = ", Time)))+
      geom_col(show.legend = FALSE)+
      theme(panel.background = element_rect(fill = "white", colour = "black")) +
      theme(panel.grid.major = element_line(colour = "Black", linetype = "solid")) + 
      xlab("Número de jogos como visitante")+
      ylab("Equipes visitantes")+
      theme_classic()+
      geom_text(aes(label=JogosV),nudge_x = 1)+
      ggtitle("Quantidade de jogos dos times como visitante")
    ggplotly(plot20, tooltip = "text") %>% layout(showlegend = FALSE) %>% style(textposition = "right")
    
    
  })        
  
  
  
  
  
  
  # output$plot122 <- renderPlotly({
  #    
  #    plot122 <- d2021 %>% 
  #       filter(Mandante==input$TIME84,Visitante==input$TIME85) %>% 
  #       group_by(Mandante,Visitante) 
  #   
  # 
  #    
  #    
  # })        
  # 
  
  output$tab122 <- DT::renderDataTable({
    DT::datatable(  confrontos <- d2021 %>% 
                      filter(Mandante==input$TIME123,Visitante==input$TIME124)%>% 
                      group_by(Mandante,Visitante) 
    )
    
    
  })        
  
  
  
  
  
  
  
})

