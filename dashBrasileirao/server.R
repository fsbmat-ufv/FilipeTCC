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
    #########################Dados do Participante####################
    ##################################################################
    output$table1 <- DT::renderDataTable({
        DT::datatable(dados,  
                      class = 'cell-border stripe',
                      extensions = 'Buttons', options = list(
                          dom = 'Bfrtip',
                          buttons = c('pageLength','copy', 'csv', 'excel', 'pdf', 'print')
                      ))
    })
    
    ################################# GRAFICO ARENA ###################
    
    output$plot21 <- renderPlotly({
       
       #head(dados)
       
       plot21 <- dados %>% 
          filter(Ano==input$Ano1)%>% 
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
          ggtitle("Quantidade de jogos em cada Arena nas edições do Brasileirão",input$Ano1)
       ggplotly(plot21, tooltip = "text") %>% layout(showlegend = FALSE) %>% style(textposition = "right")
       
    })
   
     output$plot1 <- renderPlotly({  
        
        # ar <- dados %>%
        #    group_by(Arena) %>% summarise(Quant=n(),.groups="drop") %>% filter(Quant>=255)
        
     plot1 <- dados %>%
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
     
     plot2 <- dados %>%
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
     
      if(input$Ano51=="Todos os anos") {
     plot3 <- dados %>% #filter(Ano==input$Ano51)%>% 
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
         plot3 <- dados %>% filter(Ano==input$Ano51)%>% 
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
      
      if(input$Ano52=="Todos os anos") {   
      plot4 <- dados %>% 
         #filter(Ano==input$Ano52)%>%  
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
         plot4 <- dados %>% 
         filter(Ano==input$Ano52)%>%  
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
      
      if(input$Ano53=="Todos os anos") {
         
         ptM <- dados %>% 
            #filter(Ano==input$Ano53)%>% 
            group_by(Mandante) %>% summarise(PontMan=sum(PontMandante))
         
         ptV <- dados %>% 
            #filter(Ano==input$Ano53)%>% 
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
         
         ptM <- dados %>% 
            filter(Ano==input$Ano53)%>% 
            group_by(Mandante) %>% summarise(PontMan=sum(PontMandante))
         
         ptV <- dados %>% 
            filter(Ano==input$Ano53)%>% 
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
   
   output$tabAno53 <- renderTable({
      
      if(input$Ano53=="2003"){
         
         tab2003 <- data.frame("Informações"= c("1", "2", "3","4"), "Descrição"=c("Ponte Preta escalou irregularmente o jogador Roberto nas partidas contra Internacional e Juventude, pelas rodadas 1 e 2 respectivamente, e perdeu os pontos conquistados nessas partidas. Internacional e Juventude ganharam 3 pontos cada.", 
                                                                              "O Paysandu escalou irregularmente os jogadores Júnior Amorim e Aldrovani e por esse motivo perdeu oito pontos. Desses pontos, três foram para a Ponte Preta, três para o São Caetano, dois para o Corinthians e mais dois para o Fluminense.", 
                                                                              "Libertadores 2004: Cruzeiro, Santos, São Paulo, São Caetano, Coritiba",
                                                                             "Sul-Americana 2004: Internacional, Atlético-MG, Flamengo, Goias, Paraná, Figueirense, Grêmio."))
         
         tab2003
         
         } else{
         
            if(input$Ano53=="2004"){
               
               tab2004 <- data.frame("Informações"= c("1", "2","3"), "Descrição"=c("O São Caetano foi punido com a perda de 24 pontos pelo STJD, pela morte do jogador Serginho.",
                                                                               "Libertadores 2005: Santos, Atlético-PR, São Paulo, Palmeiras.",
                                                                               "Sul-Americana 2005: Corinthians, Goiás, Juventude, Internacional, Fluminense, Cruzeiro."))
               
               tab2004
               
            } else {
               
               if(input$Ano53=="2005"){
                  
                  tab2005 <- data.frame("Informações"= c("1", "2","3"), "Descrição"=c("São Paulo tinha vaga garantida na Copa Libertadores de 2006 por ser campeão da Copa Libertadores 2005. O Corinthians, por ser o campeão, também teve direito a disputar a Copa Sul-Americana de 2006.", 
                                                                                       "Libertadores 2006: Corinthias, Internacional, Goiás, Palmeiras, São Paulo",
                                                                                       "Sul-Americana 2006: Fluminense, Atlético-PR, Paraná, Cruzeiro, Botafogo, Santos, Vasco da Gama."))
                     tab2005                                                                  
                  
                  
               } else {
                  
                  if(input$Ano53=="2006"){
                     
                     tab2006 <- data.frame("Informações"= c("1", "2","3"), "Descrição"=c("Internacional e Flamengo tinham vaga garantida Libertadores 2007 por serem campeões da Libertadores 2006 e da Copa do Brasil de 2006, respectivamente. Além disso, o São Paulo, por ser campeão, também ganhava o direito de participar da Libertadores 2007.", 
                                                                                         "Libertadores 2007: São Paulo, Internacional, Grêmio, Santos, Paraná, Flamengo.",
                                                                                         "Sul-Americana 2007: Vasco da Gama, Figueirense, Goiás, Corinthians, Cruzeiro, Botafogo, Atlético-PR."))
                     tab2006   
                     
                  } else {
                     
                     if(input$Ano53=="2007"){
                        
                        tab2007 <- data.frame("Informações"= c("1", "2","3"), "Descrição"=c("O Fluminense já estava classificado para a Libertadores 2008 por ser campeão da Copa do Brasil de 2007. O São Paulo, por ser o campeão, também teve direito a disputar a Copa Sul-Americana de 2008.", 
                                                                                            "Libertadores 2008: São Paulo, Santos, Flamengo, Fluminense, Cruzeiro.",
                                                                                            "Sul-Americana 2008: Grêmio, Palmeiras, Atlético-MG, Botafogo, Vasco da Gama, Internacional, Atlético-PR."))
                        tab2007
                        
                     } else {
                        
                        if(input$Ano53=="2008"){
                           
                           tab2008 <- data.frame("Informações"= c("1", "2","3","4"), "Descrição"=c("O Sport teve vaga garantida na Copa Libertadores da América de 2009 por ser campeão da Copa do Brasil de 2008.", 
                                                                                               "O Internacional, por ser campeão da Copa Sul-Americana de 2008, garantiu a vaga na competição do ano seguinte e abriu mais uma vaga para a Copa Sul-Americana de 2009.",
                                                                                               "Libertadores 2009: São Paulo, Grêmio, Cruzeiro, Palmeiras, Sport.",
                                                                                               "Sul-Americana 2009: Flamengo, Internacional, Botafogo, Goiás, Coritiba, Vitória, Atlético-MG, Atlético-PR, Fluminense."))
                           tab2008
                           
                        } else {
                           
                           if(input$Ano53=="2009"){
                              
                              tab2009 <- data.frame("Informações"= c("1", "2","3"), "Descrição"=c("O Corinthians tinha vaga garantida na Libertadores de 2010 por ter sido campeão da Copa do Brasil de 2009.", 
                                                                                                      "Libertadores 2010: Flamengo, Internacional, São Paulo, Cruzeiro, Corinthians.",
                                                                                                      "Sul-Americana 2010: Palmeias, Avaí, Atlético-MG, Grêmio, Goiás, Grêmio Barueri, Santos, Vitória."))
                              tab2009
                              
                           } else {
                              
                              if(input$Ano53=="2010"){
                                 
                                 tab2010 <- data.frame("Informações"= c("1", "2","3","4"), "Descrição"=c("O Grêmio Prudente foi punido com a perda de três pontos devido à escalação irregular do atleta Paulão, que havia sido suspenso dois dias antes na partida contra o Flamengo pela 3ª rodada.",
                                                                                                        "Internacional e Santos tinham vaga garantida na Copa Libertadores de 2011 por serem campeões da Copa Libertadores 2010 e da Copa do Brasil de 2010, respectivamente.", 
                                                                                                         "Libertadores 2011: Fluminense, Cruzeiro, Corinthians, Grêmio, Internacional, Santos.",
                                                                                                         "Sul-Americana 2011: Atlético-PR, Botafogo, São Paulo, Palmeiras, Vasco da Gama, Ceará, Atlético-MG, Flamengo."))
                                 tab2010
                                 
                              } else {
                                 
                                 if(input$Ano53=="2011"){
                                    
                                    tab2011 <- data.frame("Informações"= c("1", "2","3"), "Descrição"=c("Santos e Vasco da Gama tinham vaga garantida na Copa Libertadores de 2012 por serem campeões da Copa Libertadores 2011 e Copa do Brasil de 2011, respectivamente.",
                                                                                                         "Libertadores 2012: Corinthians, Vasco da Gama, Fluminense, Flamengo, Internacional.",
                                                                                                         "Sul-Americana 2012: São Paulo, Figueirense, Coritiba, Botafogo, Palmeiras, Grêmio, Atlético-GO, Bahia."))
                                    tab2011
                                    
                                 } else {
                                    
                                    if(input$Ano53=="2012"){
                                       
                                       tab2012 <- data.frame("Informações"= c("1", "2","3"), "Descrição"=c("Corinthians e Palmeiras tinham vaga garantida na Copa Libertadores de 2013 por serem campeões da Copa Libertadores de 2012 e Copa do Brasil de 2012, respectivamente e o São Paulo
                                                                                                           tinha vaga na Sul-Americana 2013 por ter sido da Sul-Americana 2012",
                                                                                                           "Libertadores 2013: Fluminense, Atlético-MG, Grêmio, São Paulo, Corinthians.",
                                                                                                           "Sul-Americana 2013: São Paulo, Náutico, Coritiba, Ponte Preta, Bahia, Portuguesa, Criciúma, Vitória, Sport."))
                                       tab2012
                                       
                                    } else {
                                       
                                       if(input$Ano53=="2013"){
                                          
                                          tab2013 <- data.frame("Informações"= c("1", "2","3","4","5"), "Descrição"=c("Atlético Mineiro e Flamengo tinham vaga garantida na Copa Libertadores de 2014 por serem campeões, respectivamente, da Copa Libertadores de 2013 e da Copa do Brasil de 2013.",
                                                                                                                       "O Flamengo foi punido pelo STJD com a perda de 4 pontos por escalação de jogador irregular.",
                                                                                                                       "A Portuguesa foi punida pelo STJD com a perda de 4 pontos por escalação de jogador irregular.",
                                                                                                                       "Libertadores 2014: Cruzeiro, Grêmio, Atlético-PR, Botafogo, Atlético-MG, Flamengo.",
                                                                                                                       "Sul-Americana 2014: Vitória, Goiás, São Paulo, Bahia, Internacional, Criciúma, Fluminense, Sport."))
                                          tab2013
                                          
                                       } else {
                                          
                                          if(input$Ano53=="2014"){
                                             
                                             tab2014 <- data.frame("Informações"= c("1", "2","3"), "Descrição"=c("Atlético Mineiro tem vaga garantida na Copa Libertadores de 2015 por ter sido campeão da Copa do Brasil de 2014.",
                                                                                                                 "Libertadores 2015: Cruzeiro, São Paulo, Internacional, Corinthians, Atlético-MG.",
                                                                                                                 "Sul-Americana 2015: Atlético-PR, Sport, Goiás, Chapecoense, Joinville, Ponte Preta, Bahia."))
                                             tab2014
                                             
                                          } else {
                                             
                                             if(input$Ano53=="2015"){
                                                
                                                tab2015 <- data.frame("Informações"= c("1", "2","3"), "Descrição"=c("Palmeiras tem vaga garantida na Copa Libertadores de 2016 por ser campeão da Copa do Brasil de 2015.",
                                                                                                                    "Libertadores 2016: Corinthians, Atlético-MG, Grêmio, São Paulo, Palmeiras.",
                                                                                                                    "Sul-Americana 2016: Sport, Flamengo, Chapecoense, Coritiba, Figueirense, Vitória, Santa Cruz. "))
                                                tab2015
                                                
                                             } else {
                                                
                                                if(input$Ano53=="2016"){
                                                   
                                                   tab2016 <- data.frame("Informações"= c("1", "2","3","4"), "Descrição"=c("Grêmio e Chapecoense têm vaga garantida na Copa Libertadores de 2017 por serem campeões, respectivamente, da Copa do Brasil de 2016 e da Copa Sul-Americana de 2016.",
                                                                                                                        "O Santa Cruz foi penalizado com a perda de três pontos por atraso no pagamentos de salários.",
                                                                                                                        "Libertadores 2017: Palmeiras, Santos, Flamengo, Atlético-MG, Botafogo, Atlético-PR, Grêmio, Chapecoense.",
                                                                                                                       "Sul-Americana 2017: Corinthians, Ponte Preta, São Paulo, Cruzeiro, Fluminense, Sport."))
                                                   tab2016
                                                   
                                                } else {
                                                   
                                                   if(input$Ano53=="2017"){
                                                      
                                                      tab2017 <- data.frame("Informações"= c("1", "2","3"), "Descrição"=c("Cruzeiro e Grêmio têm vaga garantida na Copa Libertadores de 2018 por serem campeões, respectivamente, da Copa do Brasil de 2017 e da Copa Libertadores de 2017.",
                                                                                                                          "Libertadores 2018: Corinthians, Palmeiras, Santos, Grêmio, Cruzeiro, Flamengo, Vasco da Gama, Chapecoense.",
                                                                                                                          "Sul-Americana 2018: Atlético-MG,Botafogo, Athletico-PR, Bahia, São Paulo, Fluminense."))
                                                      tab2017
                                                      
                                                   } else {
                                                      
                                                      if(input$Ano53=="2018"){
                                                         
                                                         tab2018 <- data.frame("Informações"= c("1", "2","3"), "Descrição"=c("Cruzeiro e Atlético Paranaense têm vaga garantida na Copa Libertadores de 2019 por serem campeões, respectivamente, da Copa do Brasil de 2018 e da Copa Sul-Americana de 2018.",
                                                                                                                             "Libertadores 2019: Palmeiras, Flamengo, Internacional, Grêmio, São Paulo, Atlético-MG, Athletico-PR, Cruzeiro.",
                                                                                                                             "Sul-Americana 2019: Botafogo, Santos,Bahia, Fluminense, Corinthians, Chapecoense."))
                                                         tab2018
                                                         
                                                      } else {
                                                         
                                                         if(input$Ano53=="2019"){
                                                            
                                                            tab2019 <- data.frame("Informações"= c("1", "2","3"), "Descrição"=c("Flamengo e Athletico Paranaense têm vagas garantidas na Copa Libertadores de 2020 por serem campeões da Copa Libertadores de 2019 e da Copa do Brasil de 2019, respectivamente.",
                                                                                                                                "Libertadores 2020: Flamengo, Santos, Palmeiras, Grêmio, Athletico-PR, São Paulo, Internacional, Corinthians.",
                                                                                                                                "Sul-Americana 2020: Fortaleza, Góias, Bahia, Vasco da Gama, Atlético-MG, Fluminense."))
                                                            tab2019
                                                            
                                                         } else {
                                                            
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
   #input$Ano99
      dt1 <- dados %>% filter(Ano==input$Ano99) %>% group_by(Mandante) %>% summarise(PontMan=sum(PontMandante))
      dt2 <- dados %>% filter(Ano==input$Ano99) %>% group_by(Visitante) %>% summarise(PontVis=sum(PontVisitante))
      
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
      theme_bw()+xlab(paste0("Maiores pontuadores do ano ", input$Ano99))+
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
        dt3 <- dados %>% filter(Ano==input$Ano98) %>% group_by(Mandante) %>% summarise(PontMan=sum(PontMandante))
        dt4 <- dados %>% filter(Ano==input$Ano98) %>% group_by(Visitante) %>% summarise(PontVis=sum(PontVisitante))
        
        dt4 <- data.frame(dt3[,1:2],dt4[,2])
        
        dt4$Pontos <- rowSums(dt4[,2:3])
        
        quatro <- dt4 %>% top_n(4, -Pontos) %>%  arrange(desc(Pontos)) %>% head(4)
        names(quatro) <- c("Time", "PontMan", "PontVis","Pontos") 
     
        plot <- quatro %>% ggplot(aes(reorder(Time,-Pontos), Pontos,
                                    fill=Pontos, 
                                    text=paste("Time:", Time, "<br>", 
                                               "Pontuação: ", Pontos)))+
           geom_col(show.legend = FALSE)+
           theme_bw()+xlab(paste0("Menores pontuadores do ano ", input$Ano98))+
           geom_text(aes(label=Pontos),nudge_y = 1)
        
        ggplotly(plot, tooltip = "text", width = 600, height = 600)
     
   
        
        
          })     
     
     
     
     
     
     
     
   
   ################################# GRAFICO VITORIAS ###################
   
   
   
   output$plot6 <- renderPlotly({
     
      if(input$Ano22=="Todos os anos") {
         
      dados$vitMan <- ifelse(dados$Mandante==dados$Vencedor,1,0)
      testeM <- dados %>% filter(Vencedor!="EMPATE") %>% 
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
         {dados$vitMan <- ifelse(dados$Mandante==dados$Vencedor,1,0)
      testeM <- dados %>% filter(Ano==input$Ano22,Vencedor!="EMPATE") %>% 
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
     
      if(input$Ano42=="Todos os anos") {
      
      dados$vitVist <- ifelse(dados$Visitante==dados$Vencedor,1,0)
      testeV <- dados %>% filter(Vencedor!="EMPATE") %>% 
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
         
              dados$vitVist <- ifelse(dados$Visitante==dados$Vencedor,1,0)
            testeV <- dados %>% filter(Ano==input$Ano42,Vencedor!="EMPATE") %>% 
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
     
      if(input$Ano2=="Todos os anos") {
        
       plot22 <- dados %>% filter(Vencedor!="EMPATE") %>% 
           
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
         plot22 <- dados %>% filter(Vencedor!="EMPATE") %>% 
            filter(Ano==input$Ano2)%>%
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
     
      if(input$Ano91=="Todos os anos") {
        
         empate <- dados %>% filter(Vencedor=="EMPATE") %>%
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
         
         empate <- dados %>% filter(Ano==input$Ano91,Vencedor=="EMPATE") %>%
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
     
      if(input$Ano92=="Todos os anos") {
         empate <- dados %>% filter(Vencedor=="EMPATE") %>%
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
      empate <- dados %>% filter(Ano==input$Ano92,Vencedor=="EMPATE") %>%
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
     
     if(input$Ano93=="Todos os anos"){
        empate <- dados %>% filter(Vencedor=="EMPATE") %>%
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
         empate <- dados %>% filter(Ano==input$Ano93,Vencedor=="EMPATE") %>%
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
      
      if(input$Ano3=="Todos os anos") {
      
      derotpt <- dados %>% 
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
   
   derotpt <- dados %>% 
      filter(Ano==input$Ano3, Vencedor!="EMPATE") %>%  
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
     
      if(input$Ano95=="Todos os anos") {
      
      dados$derMand <- ifelse(dados$Mandante==dados$Derrotado,1,0)
      testederM <- dados %>% filter(Derrotado!="EMPATE")%>%
         #filter(Ano=="2019",Derrotado!="EMPATE")%>% 
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
         dados$derMand <- ifelse(dados$Mandante==dados$Derrotado,1,0)
         testederM <- dados %>% filter(Ano==input$Ano95,Derrotado!="EMPATE")%>%
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
      if(input$Ano94=="Todos os anos") {
      
      dados$derVist <- ifelse(dados$Visitante==dados$Derrotado,1,0)
      testederV <- dados %>% filter(Derrotado!="EMPATE")%>%  
         #filter(Ano=="2019",Derrotado!="EMPATE")%>%  
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
         dados$derVist <- ifelse(dados$Visitante==dados$Derrotado,1,0)
         testederV <- dados %>% filter(Ano==input$Ano94,Derrotado!="EMPATE")%>%  
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
      if(input$Ano84=="Todos os anos") {
      
       mandanteNG <- dados %>%  
         #filter(Ano=="2019")%>% 
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
         mandanteNG <- dados %>%  
            filter(Ano==input$Ano84)%>% 
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
      if(input$Ano85=="Todos os anos") {
         
      visitanteNG <- dados %>% 
         #filter(Ano=="2019")%>% 
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
         visitanteNG <- dados %>% 
            filter(Ano==input$Ano85)%>% 
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
   if(input$Ano59=="Todos os anos") {
  
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
      mandanteNG <- dados %>%  
         filter(Ano==input$Ano59)%>% 
         group_by(Mandante) %>% summarise(golsM=sum(GolsMan))
      names(mandanteNG) <- c("Time","GolsM")
      
      visitanteNG <- dados %>% 
         filter(Ano==input$Ano59)%>% 
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
   
   
   
   ################################# GRAFICO JOGOS ###################
   
   
   output$plot18 <- renderPlotly({
     
      jogosM <- dados %>% 
         #filter(Ano=="2019")%>% 
         group_by(Mandante) %>% summarise(Quant=n(),.groups="drop")
      names(jogosM) <- c("Time","JogosM")
      
      jogosV <- dados %>% 
         #filter(Ano=="2019")%>% 
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
     
      jogosM <- dados %>% 
         #filter(Ano=="2019")%>% 
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
     
      jogosV <- dados %>% 
         #filter(Ano=="2019")%>% 
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
   

   output$tab122 <- DT::renderDataTable({
      DT::datatable(  confrontos <- dados %>% 
                         filter(Mandante==input$TIME123,Visitante==input$TIME124)%>% 
                         group_by(Mandante,Visitante) 
                )
   })
      
      
      
   output$plot122 <- renderPlotly({
      
      plot122 <- dados %>% 
         filter(Mandante==input$TIME84,Visitante==input$TIME85) %>% 
         group_by(Mandante,Visitante) 
     

      
      
   })        
   
   
   output$tab122 <- DT::renderDataTable({
      DT::datatable(  confrontos <- dados %>% 
                         filter(Mandante==input$TIME123,Visitante==input$TIME124)%>% 
                         group_by(Mandante,Visitante) 
      )
      
      
   })        
   
   
   
 
   
   
     
   
})
