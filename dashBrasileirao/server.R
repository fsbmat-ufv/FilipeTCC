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
          xlab("Número de jogos nos estádios")+
          ylab("Nome dos estádios")+
          theme_bw()+
          geom_text(aes(label=Quant),nudge_x = 1)+
          ggtitle("Quantidade de jogos totais em cada estádio nas edições do Brasileirão",input$Ano1)
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
       ggtitle("As Arenas com mais jogos no Brasileirão")
     ggplotly(plot1, tooltip = "text") %>% layout(showlegend = FALSE) %>% style(textposition = "top")
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
       ggtitle("As arenas com menos jogos no Brasileirão ")
     ggplotly(plot2, tooltip = "text") %>% layout(showlegend = FALSE) %>% style(textposition = "right")
   }) 
   
   
   
################################# GRAFICO PONTOS ###################
   
   output$plot3 <- renderPlotly({
     
      
         
     plot3 <- dados %>% 
         filter(Ano==input$Ano51)%>% group_by(Mandante) %>% summarise(PontMan=sum(PontMandante)) %>% ggplot(aes(PontMan,reorder(Mandante,PontMan),fill=PontMan, text=paste("Núm. de Pontos =", PontMan, "<br>",
                                                                                             "Time = ", Mandante)))+
       geom_col(show.legend = FALSE)+
       theme(panel.background = element_rect(fill = "white", colour = "black")) +
       theme(panel.grid.major = element_line(colour = "Black", linetype = "solid")) + 
       xlab("Número de pontos dos times como mandante")+
       ylab("Times que disputaram o Brasileirão")+
       geom_text(aes(label=PontMan),nudge_x = 15)+
       theme_classic()+
       ggtitle("Quantidades de pontos dos times como mandante no Brasileirão")
     ggplotly(plot3, tooltip = "text") %>% layout(showlegend = FALSE) %>% style(textposition = "top")
     
   }) 
     
   output$plot4 <- renderPlotly({
      
      
     plot4 <- dados %>% 
         filter(Ano==input$Ano52)%>%  group_by(Visitante) %>% summarise(PontVis=sum(PontVisitante)) %>% ggplot(aes(PontVis,reorder(Visitante,PontVis),fill=PontVis, text=paste("Núm. de Pontos =", PontVis, "<br>",
                                                                                              "Time = ", Visitante)))+
       geom_col(show.legend = FALSE)+
       theme(panel.background = element_rect(fill = "white", colour = "black")) +
       theme(panel.grid.major = element_line(colour = "Black", linetype = "solid")) + 
       xlab("Número de pontos dos times como visitantes")+
       ylab("Times que disputaram o Brasileirão")+
       geom_text(aes(label=PontVis),nudge_x = 10)+
       theme_classic()+
       ggtitle("Quantidades de pontos dos times como visitante no Brasileirão")
     ggplotly(plot4, tooltip = "text") %>% layout(showlegend = FALSE) %>% style(textposition = "top")
     
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
            geom_text(aes(label=Pontos),nudge_x = 30)+
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
            geom_text(aes(label=Pontos),nudge_x = 30)+
            ggtitle("Pontuação final no Brasileirão")
         ggplotly(plot5, tooltip = "text") %>% layout(showlegend = FALSE) %>% style(textposition = "right")
         
      }
     
   }) 
   ################################# GRAFICO VITORIAS ###################
   
   
   
   output$plot6 <- renderPlotly({
     
      dados$vitMan <- ifelse(dados$Mandante==dados$Vencedor,1,0)
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
       geom_text(aes(label=VitM),nudge_x = 4)+
       theme_classic()+
       ggtitle("Quantidades de vitórias como mandante no Brasileirão")
     ggplotly(plot6, tooltip = "text") %>% layout(showlegend = FALSE) %>% style(textposition = "top")
     
     
   }) 
   
   output$plot7 <- renderPlotly({
     
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
       geom_text(aes(label=VitV),nudge_x = 3)+
       theme_classic()+
       ggtitle("Quantidades de vitórias como visitante no Brasileirão")
     ggplotly(plot7, tooltip = "text") %>% layout(showlegend = FALSE) %>% style(textposition = "top")
     
     
   }) 
   
   output$plot8 <- renderPlotly({
     
      
       plot8 <- vitot %>%  ggplot(aes(Quant,reorder(Vencedor,Quant),fill=Vencedor, text=paste("Núm. de Vitórias =", Quant, "<br>",
                                                                                            "Time = ", Vencedor)))+
       geom_col(show.legend = FALSE)+
       theme(panel.background = element_rect(fill = "white", colour = "black")) +
       theme(panel.grid.major = element_line(colour = "Black", linetype = "solid")) + 
       xlab("Número de vitórias gerais")+
       ylab("Times que disputaram o Brasileirão")+
       theme_classic()+
       geom_text(aes(label=Quant),nudge_x = 7)+
       ggtitle("Quantidade total de vitórias dos times nas edições do Brasileirão")
     ggplotly(plot8, tooltip = "text") %>% layout(showlegend = FALSE) %>% style(textposition = "top")
     
     
   })    
   
   output$plot22 <- renderPlotly({
      
      head(dados)
      vitot <- dados %>% 
         filter(Ano==input$Ano2, Vencedor!="EMPATE") %>%  
         group_by(Vencedor) %>% summarise(Quant=n(),.groups="drop")
      plot22 <- vitot %>%  ggplot(aes(Quant,reorder(Vencedor,Quant),fill=Vencedor,fill=Time, text=paste("Núm. de Vitorias =", Quant, "<br>",
                                                                                                        "Vencedor = ", Vencedor)))+
         geom_col(show.legend = FALSE)+
         theme(panel.background = element_rect(fill = "white", colour = "black")) +
         theme(panel.grid.major = element_line(colour = "Black", linetype = "solid")) + 
         xlab("Número de Vitórias")+
         ylab("Nome das equipes")+
         theme_bw()+
         geom_text(aes(label=Quant),nudge_x = 1)+
         ggtitle("Quantidade de vitórias nas edições do Brasileirão", input$Ano2)
      ggplotly(plot22, tooltip = "text") %>% layout(showlegend = FALSE) %>% style(textposition = "top")
      
   })
   
   ################################# GRAFICO EMPATES ###################
   
   
   output$plot9 <- renderPlotly({
     
     plot9 <- totEmpate %>%  ggplot(aes(Total,reorder(Time,Total),fill=Total, text=paste("Núm. de Empates =", Total, "<br>",
                                                                                         "Time = ", Time)))+
       geom_col(show.legend = FALSE)+
       theme(panel.background = element_rect(fill = "white", colour = "black")) +
       theme(panel.grid.major = element_line(colour = "Black", linetype = "solid")) + 
       xlab("Número de empates gerais")+
       ylab("Times que disputaram o Brasileirão")+
       theme_classic()+
       geom_text(aes(label=Total),nudge_x = 1)+
       ggtitle("Quantidade total de empates dos times nas edições do Brasileirão")
     ggplotly(plot9, tooltip = "text") %>% layout(showlegend = FALSE) %>% style(textposition = "top")
     
     
     
   }) 
   
   output$plot10 <- renderPlotly({
     
     plot10 <- emm %>%  ggplot(aes(EmpM,reorder(Time,EmpM),fill=EmpM, text=paste("Núm. de Empates =", EmpM, "<br>",
                                                                                 "Time = ", Time)))+
       geom_col(show.legend = FALSE)+
       theme(panel.background = element_rect(fill = "white", colour = "black")) +
       theme(panel.grid.major = element_line(colour = "Black", linetype = "solid")) + 
       xlab("Número de empates como mandante no Brasileirão")+
       ylab("Times que disputaram o Brasileirão")+
       geom_text(aes(label=EmpM),nudge_x = 1)+
       theme_classic()+
       ggtitle("Quantidades de empates como mandante no Brasileirão")
     ggplotly(plot10, tooltip = "text") %>% layout(showlegend = FALSE) %>% style(textposition = "top")
     
     
   }) 
   
   output$plot11 <- renderPlotly({
     
     plot11 <- emv %>%  ggplot(aes(EmpV,reorder(Time,EmpV),fill=EmpV, text=paste("Núm. de Empates =", EmpV, "<br>",
                                                                                 "Time = ", Time)))+
       geom_col(show.legend = FALSE)+
       theme(panel.background = element_rect(fill = "white", colour = "black")) +
       theme(panel.grid.major = element_line(colour = "Black", linetype = "solid")) + 
       xlab("Número de empates como visitante no Brasileirão")+
       ylab("Times que disputaram o Brasileirão")+
       geom_text(aes(label=EmpV),nudge_x = 1)+
       theme_classic()+
       ggtitle("Quantidades de empates como visitante no Brasileirão")
     ggplotly(plot11, tooltip = "text") %>% layout(showlegend = FALSE) %>% style(textposition = "top")
     
     
   })     
   
    

   output$plot49 <- renderPlotly({
      totEmpate <- left_join(emv,emm,by="Time")
      totEmpate$Total <- rowSums(totEmpate[,2:3])
     
      totEmpate <- dados %>% filter(Ano==input$Ano49) %>% 
         group_by(Time) %>% summarise(Total=sum(totalempt))
      names(totEmpate) <- c("Time","Total")
      
         plot49 <- totEmpate %>%  ggplot(aes(Total,reorder(Time,Total),fill=Time, text=paste("Núm. de Empates =", Total, "<br>",
                                                                                                     "Time = ", Time)))+
      geom_col(show.legend = FALSE)+
      theme(panel.background = element_rect(fill = "white", colour = "black")) +
      theme(panel.grid.major = element_line(colour = "Black", linetype = "solid")) + 
      xlab("Número de Vitórias")+
      ylab("Nome das equipes")+
      theme_bw()+
      geom_text(aes(label=Total),nudge_x = 1)+
      ggtitle("Quantidade de empates nas edições do Brasileirão", input$Ano49)
   ggplotly(plot49, tooltip = "text") %>% layout(showlegend = FALSE) %>% style(textposition = "top")
   
   })

   
   
  
    ################################# GRAFICO DERROTAS ###################
   

   output$plot12 <- renderPlotly({
     
     plot12 <- derotpt %>%  ggplot(aes(Quant,reorder(Derrotado,Quant),fill=Derrotado, text=paste("Núm. de Derrtoas =", Quant, "<br>",
                                                                                                 "Time = ", Derrotado)))+
       geom_col(show.legend = FALSE)+
       theme(panel.background = element_rect(fill = "white", colour = "black")) +
       theme(panel.grid.major = element_line(colour = "Black", linetype = "solid")) + 
       xlab("Número de derrotas gerais")+
       ylab("Times que disputaram o Brasileirão")+
       theme_classic()+
       geom_text(aes(label=Quant),nudge_x = 5.5)+
       ggtitle("Quantidade total de derrotas dos times nas edições do Brasileirão")
     ggplotly(plot12, tooltip = "text") %>% layout(showlegend = FALSE) %>% style(textposition = "top")
     
     
     
   }) 
   
   output$plot13 <- renderPlotly({
     
     plot13 <- testederM %>%  ggplot(aes(DerM,reorder(Time,DerM),fill=DerM, text=paste("Núm. de Derrotas =", DerM, "<br>",
                                                                                       "Time = ", Time)))+
       geom_col(show.legend = FALSE)+
       theme(panel.background = element_rect(fill = "white", colour = "black")) +
       theme(panel.grid.major = element_line(colour = "Black", linetype = "solid")) + 
       xlab("Número de derrotas como mandante no Brasileirão")+
       ylab("Times que disputaram o Brasileirão")+
       geom_text(aes(label=DerM),nudge_x = 1.5)+
       theme_classic()+
       ggtitle("Quantidades de derrotas como mandante no Brasileirão")
     ggplotly(plot13, tooltip = "text") %>% layout(showlegend = FALSE) %>% style(textposition = "top")
     
     
   }) 
   
   output$plot14 <- renderPlotly({
     
     plot14 <- testederV %>%  ggplot(aes(DerV,reorder(Time,DerV),fill=DerV, text=paste("Núm. de Derrotas =", DerV, "<br>",
                                                                                       "Time = ", Time)))+
       geom_col(show.legend = FALSE)+
       theme(panel.background = element_rect(fill = "white", colour = "black")) +
       theme(panel.grid.major = element_line(colour = "Black", linetype = "solid")) + 
       xlab("Número de derrotas como visitante no Brasileirão")+
       ylab("Times que disputaram o Brasileirão")+
       geom_text(aes(label=DerV),nudge_x = 3.5)+
       theme_classic()+
       ggtitle("Quantidades de derrotas como visitante no Brasileirão")
     ggplotly(plot14, tooltip = "text") %>% layout(showlegend = FALSE) %>% style(textposition = "top")
     
     
   })     
   
   output$plot23 <- renderPlotly({
      
      head(dados)
      derotpt <- dados %>% 
         filter(Ano==input$Ano3, Vencedor!="EMPATE") %>%  
         group_by(Derrotado) %>% summarise(Quant=n(),.groups="drop")
      plot23 <- derotpt %>%  ggplot(aes(Quant,reorder(Derrotado,Quant),fill=Derrotado,fill=Time, text=paste("Núm. de Derrotas =", Quant, "<br>",
                                                                                                            "Derrotado = ", Derrotado)))+
         geom_col(show.legend = FALSE)+
         theme(panel.background = element_rect(fill = "white", colour = "black")) +
         theme(panel.grid.major = element_line(colour = "Black", linetype = "solid")) + 
         xlab("Número de Derrotas")+
         ylab("Nome das equipes")+
         theme_bw()+
         geom_text(aes(label=Quant),nudge_x = 1)+
         ggtitle("Quantidade de derrotas nas edições do Brasileirão", input$Ano3)
      ggplotly(plot23, tooltip = "text") %>% layout(showlegend = FALSE) %>% style(textposition = "top")
      
   })
   
   ################################# GRAFICO GOLS ###################
   
   
   output$plot15 <- renderPlotly({
     
     plot15 <- totGols %>%  ggplot(aes(Total,reorder(Time,Total),fill=Total, text=paste("Núm. de Gols =", Total, "<br>",
                                                                                        "Time = ", Time)))+
       geom_col(show.legend = FALSE)+
       theme(panel.background = element_rect(fill = "white", colour = "black")) +
       theme(panel.grid.major = element_line(colour = "Black", linetype = "solid")) + 
       xlab("Número de gols no Brasileirão")+
       ylab("Times que disputaram o Brasileirão")+
       #geom_text(aes(label=Total),nudge_x = 27)+
       theme_bw()+
       ggtitle("Quantidade total de gols dos times nas edições do Brasileirão")
     ggplotly(plot15, tooltip = "text") %>% layout(showlegend = FALSE) %>% style(textposition = "top")
     
     
     
   }) 
   
   output$plot16 <- renderPlotly({
     
     plot16 <- mandanteNG %>%  ggplot(aes(GolsM,reorder(Time,GolsM),fill=GolsM, text=paste("Núm. de Gols =", GolsM, "<br>",
                                                                                      "Time = ", Time)))+
       geom_col(show.legend = FALSE)+
       theme(panel.background = element_rect(fill = "white", colour = "black")) +
       theme(panel.grid.major = element_line(colour = "Black", linetype = "solid")) + 
       xlab("Número de gols no Brasileirão como mandante")+
       ylab("Times que disputaram o Brasileirão")+
       #geom_text(aes(label=GolsM),nudge_x = 15)+
       theme_classic()+
       ggtitle("Quantidade de gols dos times como mandante no Brasileirão")
     ggplotly(plot16, tooltip = "text") %>% layout(showlegend = FALSE) %>% style(textposition = "top")
     
     
   }) 
   
   output$plot17 <- renderPlotly({
     
     plot17 <- visitanteNG %>%  ggplot(aes(GolsV,reorder(Time,GolsV),fill=GolsV, text=paste("Núm. de Gols =", GolsV, "<br>",
                                                                                            "Time = ", Time)))+
       geom_col(show.legend = FALSE)+
       theme(panel.background = element_rect(fill = "white", colour = "black")) +
       theme(panel.grid.major = element_line(colour = "Black", linetype = "solid")) + 
       xlab("Número de gols no Brasileirão como visitante")+
       ylab("Times que disputaram o Brasileirão")+
       #geom_text(aes(label=GolsV),nudge_x = 10)+
       theme_classic()+
       ggtitle("Quantidade de gols dos times como visitantes no Brasileirão")
     ggplotly(plot17, tooltip = "text") %>% layout(showlegend = FALSE) %>% style(textposition = "top")
     
     
   })     

   
  

output$plot59 <- renderPlotly({

         plot59 <- totGols %>%  ggplot(aes(Total,reorder(Time,Total),fill=Total, text=paste("Núm. de Gols =", Total, "<br>",
                                                                                         "Time = ", Time)))+
         geom_col(show.legend = FALSE)+
         theme(panel.background = element_rect(fill = "white", colour = "black")) +
         theme(panel.grid.major = element_line(colour = "Black", linetype = "solid")) + 
         xlab("Número de gols no Brasileirão")+
         ylab("Times que disputaram o Brasileirão")+
         #geom_text(aes(label=Total),nudge_x = 27)+
         theme_bw()+
         ggtitle("Quantidade total de gols dos times nas edições do Brasileirão")
      ggplotly(plot59, tooltip = "text") %>% layout(showlegend = FALSE) %>% style(textposition = "top")
   
   
})
   
   
   
   ################################# GRAFICO JOGOS ###################
   
   
   output$plot18 <- renderPlotly({
     
     plot18 <- totJogos %>%  ggplot(aes(Total,reorder(Time,Total),fill=Total, text=paste("Núm. de Jogos =", Total, "<br>",
                                                                                         "Time = ", Time)))+
       geom_col(show.legend = FALSE)+
       theme(panel.background = element_rect(fill = "white", colour = "black")) +
       theme(panel.grid.major = element_line(colour = "Black", linetype = "solid")) + 
       xlab("Número de jogos gerais")+
       ylab("Times que disputaram o Brasileirão")+
       theme_classic()+
       #geom_text(aes(label=Total),nudge_x = 15)+
       ggtitle("Quantidade total de jogos dos times nas edições do Brasileirão")
           ggplotly(plot18, tooltip = "text") %>% layout(showlegend = FALSE) %>% style(textposition = "top")
     
     
     
   }) 
   
   output$plot19 <- renderPlotly({
     
     plot19 <- jogosM %>%  ggplot(aes(JogosM,reorder(Time,JogosM),fill=Time, text=paste("Núm. de Jogos =", JogosM, "<br>",
                                                                                        "Time = ", Time)))+
       geom_col(show.legend = FALSE)+
       theme(panel.background = element_rect(fill = "white", colour = "black")) +
       theme(panel.grid.major = element_line(colour = "Black", linetype = "solid")) + 
       xlab("Número de jogos como mandante")+
       ylab("Equipes mandantes")+
       theme_classic()+
       #geom_text(aes(label=JogosM),nudge_x = 7)+
       ggtitle("Quantidade de jogos dos times como mandante")
     ggplotly(plot19, tooltip = "text") %>% layout(showlegend = FALSE) %>% style(textposition = "top")
     
     
   }) 
   
   output$plot20 <- renderPlotly({
     
     plot20 <- jogosV %>%  ggplot(aes(JogosV,reorder(Time,JogosV),fill=Time, text=paste("Núm. de Empates =", JogosV, "<br>",
                                                                                        "Time = ", Time)))+
       geom_col(show.legend = FALSE)+
       theme(panel.background = element_rect(fill = "white", colour = "black")) +
       theme(panel.grid.major = element_line(colour = "Black", linetype = "solid")) + 
       xlab("Número de jogos como visitante")+
       ylab("Equipes mandantes")+
       theme_classic()+
       geom_text(aes(label=JogosV),nudge_x = 8)+
       ggtitle("Quantidade de jogos dos times como visitante")
     ggplotly(plot20, tooltip = "text") %>% layout(showlegend = FALSE) %>% style(textposition = "top")
     
     
   })        
   
   ################################# TESTE ###################
   
   
   output$plot30 <- renderPlotly({
      mandanteNG <- dados %>%  
         filter(Ano==input$Ano10)%>% 
         group_by(Mandante) %>% summarise(golsM=sum(GolsMan))
      names(mandanteNG) <- c("Time","GolsM")
        
      plot30 <- mandanteNG %>%  ggplot(aes(GolsM,reorder(Time,GolsM),fill=GolsM, text=paste("Núm. de Gols =", GolsM, "<br>",
                                                                                                        "Time = ", Time)))+
         geom_col(show.legend = FALSE)+
         theme(panel.background = element_rect(fill = "white", colour = "black")) +
         theme(panel.grid.major = element_line(colour = "Black", linetype = "solid")) + 
         xlab("Número de gols como mandante")+
         ylab("Equipes")+
         theme_bw()+
         geom_text(aes(label=GolsM),nudge_x = 1)+
         ggtitle("Quantidade de gols dos times como mandante no Brasileirão", input$Ano10)
      ggplotly(plot30, tooltip = "text") %>% layout(showlegend = FALSE) %>% style(textposition = "top")
     
   })
  
    output$plot34 <- renderPlotly({
   ptM <- dados %>% 
      filter(Ano==input$Ano11)%>% 
      group_by(Mandante) %>% summarise(PontMan=sum(PontMandante))
   plot34 <-  ptM %>%  ggplot(aes(PontMan,reorder(Mandante,PontMan),fill=PontMan, text=paste("Núm. de Pontos =", PontMan, "<br>",
                                                                                  "Time = ", Mandante)))+
      geom_col(show.legend = FALSE)+
      theme(panel.background = element_rect(fill = "white", colour = "black")) +
      theme(panel.grid.major = element_line(colour = "Black", linetype = "solid")) + 
      xlab("Número de pontos dos times como mandante")+
      ylab("Times que disputaram o Brasileirão")+
      geom_text(aes(label=PontMan),nudge_x = 1)+
      theme_classic()+
      ggtitle("Quantidades de pontos dos times como mandante no Brasileirão")
   ggplotly(plot34, tooltip = "text") %>% layout(showlegend = FALSE) %>% style(textposition = "top")
   
    })
   
   
})
