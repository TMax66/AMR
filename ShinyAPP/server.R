

server<-function(input, output) {
  output$newtabs<-renderUI({})
  theme_set(theme_bw())
  df<-reactive({      
  sheet <- gs_read(dati, ws="AMR" )
  #df<-gs_read_csv(sheet)
  sheet %>% 
    mutate(UO=ifelse(ZONA=='BINAGO', "U.O.5", 
          ifelse(ZONA=="PAVIA", "U.O.6", 
          ifelse(ZONA=="Sondrio media valle","U.O.2",
          ifelse(ZONA=="Sondrio alta valle","U.O.2",
          ifelse(ZONA=="Sondrio bassa valle", "U.O.2",
          ifelse(ZONA=="LECCO"|ZONA=="Lecco"|ZONA=="Lecco alpi","U.O.2",
          ifelse(ZONA=="Alto lario"|ZONA=="Sondrio Valchiavenna"|ZONA=="Sondrio valchiavenna"|
                   ZONA=="Alto Lario", "U.O.2", "U.O.1"))))))))%>% 
    dplyr::select(IDcamp,IDceppo,UO, ZONA,SPECIE,identificazione,colistina,ceftiofur,tilmicosina,kanamicina,enrofloxacin,oxacillina,
           eritromicina,gentamicina,tetraciclina,ampicillina, Specieagg, area)
  })
  
  output$dati<- DT::renderDataTable(
    df(),  rownames = FALSE, options = list(
      autoWidth = TRUE,columnDefs = list(list(width = '5%', targets = list(6:15)))
  ))
  
  df2<-reactive({      
    sheet2 <- gs_read(dati, ws="UO")
    # df2<-gs_read_csv(sheet2)
  })
  
  output$UO<- DT::renderDataTable(
    df2(),  rownames = FALSE, options = list(
      autoWidth = TRUE))
  
  tabfin<-reactive({
    sheet3<-gs_read(dati, "finanziamento")
  })
  
  output$risorse<-renderTable(
    tabfin()
  )
  
  tabcons<-reactive({
    
    sheet4<-gs_read(dati, "consumi")
    
  })
  
  
  output$cons<-renderTable(
    tabcons() %>% 
      dplyr::select(c(2,3,9))
  )
  
  x<-reactive({
    df() %>% 
      group_by(IDcamp, Specieagg, ZONA, UO) %>%
      filter(!is.na(ZONA)) %>% 
      summarise()})
  
  
  output$campioni<-renderRpivotTable({
    rpivotTable(x(), col="SPECIE", rows=c("UO","ZONA"), rendererName = "Heatmap")
  })

  
  output$summ<-renderRpivotTable({  
    df() %>% 
      dplyr::select(IDcamp, Specieagg, area, UO,identificazione) %>% 
      filter(!is.na(identificazione)) %>% 
    rpivotTable(df(),aggregatorName="Count",rendererName = "Heatmap",
                col="identificazione", rows="Specieagg") 
  } )
  
  resistenza<-reactive({ 
    tot<-ds %>% 
    dplyr::select(IDceppo,c(12:22)) %>% 
    gather(antibiotico, esito, 3:12) %>% 
    group_by(identificazione, antibiotico) %>% 
    summarise("n"=n()) 
  res<-ds %>% 
    dplyr::select(IDceppo,c(12:22)) %>% 
    gather(antibiotico, esito, 3:12) %>% 
    group_by(identificazione, antibiotico) %>% 
    filter(esito=='R') %>% 
    summarise("r"=n())  
  z<-tot %>% full_join(res)%>% 
    replace_na(list(r=0)) %>% 
    mutate("%resistenti"=(r/n)*100) %>% 
    #filter(identificazione==input$ceppo) %>% 
    filter(!is.na(identificazione)) %>% 
    as.data.frame()
    })
  
 output$plotRes<-renderPlot(
   
   # resistenza() %>% 
   # filter(identificazione==input$ceppo) %>% 
   #   arrange(`%resistenti`) %>% 
   #   mutate(antibiotico = factor(antibiotico, unique(antibiotico))) %>% 
   # ggplot(aes(x=antibiotico,`%resistenti` ))+geom_bar(stat = 'identity', fill="steelblue")+coord_flip()
   resistenza()%>% 
     filter(identificazione==input$ceppo) %>% 
     arrange(`%resistenti`) %>% 
     mutate(`%resistenti`= round(`%resistenti`,1)) %>% 
     mutate(antibiotico = factor(antibiotico, unique(antibiotico))) %>% 
     ggplot(aes(x=antibiotico,y=`%resistenti`,label=`%resistenti`))+
     geom_point(stat='identity',col="snow2", size=8)+
     geom_text(color="black", size=3)+
     geom_segment(aes(x=antibiotico, 
                      xend=antibiotico, 
                      # y=min(`%resistenti`),
                      y=0,
                      yend=`%resistenti`-2 
                  # linetype="dashed", 
                 #
                 ))+
     labs(title="% di ceppi resistenti",caption=Sys.Date()) +  
     coord_flip()
   
 )
 
 
heatm<-reactive({      
  totx<-ds %>%   
   dplyr::select(IDceppo,SPECIE,c(12:22)) %>% 
   gather(antibiotico, esito, 4:13) %>% 
   group_by(SPECIE,identificazione, antibiotico) %>% 
   summarise("n"=n())
 resx<-ds %>% 
   dplyr::select(IDceppo,SPECIE, c(12:22)) %>% 
   gather(antibiotico, esito, 4:13) %>% 
   group_by(SPECIE,identificazione, antibiotico) %>% 
   filter(esito=='R') %>% 
   summarise("r"=n())
 
 x<-totx %>% full_join(resx)%>% 
   #factor(identificazione,antibiotico) %>% 
   replace_na(list(r=0)) %>% 
   mutate("%resistenti"=(r/n)*100) %>% 
   #filter(identificazione=="Acinetobacter") %>% 
   as.data.frame() 
})
 
  
  output$hmap<-renderPlot(
    heatm() %>% 
      filter(SPECIE==input$specie) %>% 
      arrange(`%resistenti`) %>% 
      mutate(antibiotico = factor(antibiotico, unique(antibiotico))) %>% 
     ggplot(aes(antibiotico,identificazione)) + 
      geom_tile(aes(fill = `%resistenti`), colour = "white") + 
      scale_fill_gradient(low = "snow2", high = "steelblue")+ theme_grey(base_size = 9) + 
            labs(x = "", y = "", title="profilo di resistenza dei ceppi", caption=Sys.Date()) + 
            scale_x_discrete(expand = c(0, 0)) +
            scale_y_discrete(expand = c(0, 0)) 
    )

  

  
   output$MAR<-renderPlot(
     
  

  ds2 %>% 
    dplyr::select(ZONA,SPECIE, identificazione, 13:22) %>%
    filter(!is.na(identificazione)) %>%
    adorn_totals("col")  %>%
    filter(ZONA==input$zona) %>% 
    group_by(SPECIE) %>%
    summarise(n=n(),
              MAR=sum(Total)/(n*10)
    ) %>%
    mutate(MAR=round(MAR, 2)) %>%
    arrange(MAR) %>%
    mutate(SPECIE = factor(SPECIE, unique(SPECIE))) %>%
    ggplot(aes(x=SPECIE, y=MAR, label=MAR))+
    geom_point(stat='identity',col="snow2", size=8)+
    geom_text(color="black", size=3)+
    geom_segment(aes(x=SPECIE,
                     xend=SPECIE,
                     # y=min(`%resistenti`),
                     y=0,
                     yend=MAR-0.009
                     # linetype="dashed",
                     #
    ))+
    labs(title="Multiple Antimicrobial Resistance INDEX",caption=Sys.Date()) +
    coord_flip()
  )
  
  
  consumi<-reactive({  
    tabcons() %>% 
      summarise("Utilizzati"=sum(Valore)) %>%
      mutate(
        "Stanziati"=10500,
        "Residuo"= 10500-Utilizzati) %>% 
      dplyr::select(Stanziati, Utilizzati, Residuo)    
    })
 output$consumi<-renderTable(
   consumi()
 )
  
 output$timeline <- renderTimevis({
   timevis(timing)
 })
 
 
}
