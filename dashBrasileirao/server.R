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
    
    output$plot1 <- renderPlotly({
        
        plot1 <- dados %>% filter(Ano==input$Ano1, Vencedor!="EMPATE") %>% 
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
            ggtitle(paste("Número de vitórias no Brasileirão", input$Ano1))
        ggplotly(plot1, tooltip = "text") %>% layout(showlegend = FALSE) %>% style(textposition = "top")
        
        
        })
    
   #  output$plot2 <- renderLeaflet({
   #      dados %>% group_by(Country, latitude, longitude) %>% summarise(NrUniv=n_distinct(University)) %>% 
   #          leaflet() %>% 
   #          addTiles() %>% 
   #          addMarkers(lng = ~longitude, lat = ~latitude, popup = ~NrUniv,
   #                     clusterOptions = markerClusterOptions(maxClusterRadius = 15))
   #  })
   #  
   # output$plot3 <- renderPlotly({
   #     plot2 <- dados %>% filter(Country=="BRAZIL") %>% 
   #         filter(University=="UNIVERSIDADE FEDERAL DE VICOSA",
   #                Period=="2014–2017", 
   #                Frac_counting=="1") %>% 
   #         ggplot(aes(Field, impact_P, fill=Field, label= round(impact_P, digits = 2), 
   #                    text=paste("Produção:",impact_P, "<br>", 
   #                               "Período:", Period))) +
   #         geom_col(aes(Field, impact_P), show.legend = FALSE) + 
   #         xlab("Área Ciêntífica (2014-2017)") + ylab("Número de Publicações com Impacto") + 
   #         geom_text(position = position_dodge(width = 0.9), vjust = -0.5) + theme_bw() +
   #         theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)) 
   #     ggplotly(plot2, tooltip = "text") %>% layout(showlegend = FALSE) %>% style(textposition = "top")
   #     
   # }) 
    
})
