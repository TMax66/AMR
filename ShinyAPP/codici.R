library(shiny)
library(DT)
library(dplyr)
library(googlesheets)
library(ggplot2)
library(janitor)
library(summarytools)
library(tidyr)
library(rpivotTable)
library(timevis)

##
dati<-gs_title("AMR")

sheet <- gs_read(dati, ws="AMR" )
sheet %>% 
mutate(UO=ifelse(ZONA=='BINAGO', 
                 "U.O.5", ifelse(ZONA=="PAVIA", "U.O.6", 
                                 ifelse(ZONA=="SONDRIO",  "U.O.3","U.O.1")))) %>% 
  group_by(UO) %>% 
  summarise('#ceppi'=n()) %>% 
  adorn_totals("row")
  
##########################################################################
dati<-gs_title("AMR")

sheet <- gs_read(dati, ws="AMR" )
tot<-sheet %>% 
  select(IDceppo,c(11:21)) %>% 
  gather(antibiotico, esito, 3:12) %>% 
  group_by(identificazione, antibiotico) %>% 
  summarise("n"=n()) 
res<-sheet %>% 
  select(IDceppo,c(11:21)) %>% 
  gather(antibiotico, esito, 3:12) %>% 
  group_by(identificazione, antibiotico) %>% 
  filter(esito=='R') %>% 
  summarise("r"=n())

x<-tot %>% full_join(res)%>% 
  factor(identificazione,antibiotico) %>% 
  replace_na(list(r=0)) %>% 
  mutate("%resistenti"=(r/n)*100) %>% 
  filter(identificazione=="Acinetobacter") %>% 
  as.data.frame() 

  

  p<-ggplot(x,aes(x=antibiotico,`%resistenti` ))+geom_bar(stat = 'identity')+coord_flip()

  





%>% 
  filter(esito=="R") %>% 
  summarise("tot"=n())



%>% 
  
  
  
  
  group_by(identificazione,antibiotico) %>% 
  filter(esito=='R') %>% 
  summarise("% ceppi resistenti"=(n()/10)*100)
 
           
################################timing###################
  
  dati<-gs_title("AMR")
  
  timing <- gs_read(dati, ws="timing" )
  
  
  library(shiny)
  library(timevis)
  
  # data <- data.frame(
  #   id      = 1:10,
  #   content = c("Inizio Progetto", "Attivazione Borsa di Studio",
  #               "Isolamento, identificazione ceppi, profilo fenotipico", "Item four"),
  #   start   = c("2017-12-15", "2018-02-01",
  #               "2018-02-01", "2019-02-15"),
  #   end     = c(NA, NA, "2019-12-15", NA)
  # )
  
  ui <- fluidPage(
    timevisOutput("timeline")
  )
  
  server <- function(input, output, session) {
    output$timeline <- renderTimevis({
      timevis(timing)
    })
  }
  
  shinyApp(ui = ui, server = server)
################################
  
  

  
  
  
  
  
  tot<-sheet %>% 
    select(IDceppo,SPECIE,c(11:21)) %>% 
    gather(antibiotico, esito, 4:13) %>% 
    group_by(SPECIE,identificazione, antibiotico) %>% 
    summarise("n"=n()) 
  
  
  
  res<-sheet %>% 
    select(IDceppo,SPECIE, c(11:21)) %>% 
    gather(antibiotico, esito, 4:13) %>% 
    group_by(SPECIE,identificazione, antibiotico) %>% 
    filter(esito=='R') %>% 
    summarise("r"=n())
  
  x<-tot %>% full_join(res)%>% 
    #factor(identificazione,antibiotico) %>% 
    replace_na(list(r=0)) %>% 
    mutate("%resistenti"=(r/n)*100) %>% 
    filter(SPECIE=="CAPRIOLO") %>% 
    as.data.frame() 
  
  
  p <- ggplot(x, aes(antibiotico,identificazione)) + 
    geom_tile(aes(fill = `%resistenti`), colour = "white") + 
    scale_fill_gradient(low = "snow2", high = "steelblue")

  base_size <- 9
  print(p + theme_grey(base_size = base_size) + 
          labs(x = "", y = "") + 
          scale_x_discrete(expand = c(0, 0)) +
          scale_y_discrete(expand = c(0, 0)) )
          # opts(legend.position = "none",
          #      axis.ticks = theme_blank(), 
          #      axis.text.x = theme_text(size = base_size *0.8, 
          #                               angle = 330, hjust = 0, colour = "grey50")))
  
  
  
  
  dati<-gs_title("AMR")
  ds <-gs_read(dati, ws="AMR" )
  ds<-ds %>% 
  filter(!is.na(identificazione))
  #####################################################################
  resistenza<-reactive({ 
    tot<-ds %>% 
      select(IDceppo,c(11:21)) %>% 
      gather(antibiotico, esito, 3:12) %>% 
      group_by(identificazione, antibiotico) %>% 
      summarise("n"=n()) 
    res<-ds %>% 
      select(IDceppo,c(11:21)) %>% 
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
  
  resistenza<-z

  
  
  resistenza%>% 
    filter(identificazione=="E.coli") %>% 
    arrange(`%resistenti`) %>% 
    mutate(`%resistenti`=round(`%resistenti`,2)) %>% 
    mutate(antibiotico = factor(antibiotico, unique(antibiotico))) 
  
  
  
  
  %>% 
    ggplot(aes(x=antibiotico,`%resistenti`))+
    geom_point(col="tomato2", size=3)+
    geom_segment(aes(x=antibiotico, 
                     xend=antibiotico, 
                     y=0, 
                     yend=max(`%resistenti`)), 
                 #linetype="dashed", 
                 size=0.1)+
    labs(title="% di ceppi resistenti",caption=Sys.Date()) +  
    coord_flip()
  
  
##########################################################################
  
 sheet<- gs_read(dati, ws="AMR" )
  sheet$colistina<-ifelse(sheet$colistina=="R",1,0)
  sheet$ceftiofur<-ifelse(sheet$ceftiofur=="R",1,0)
  sheet$tilmicosina<-ifelse(sheet$tilmicosina=="R",1,0)
  sheet$kanamicina<-ifelse(sheet$kanamicina=="R",1,0)
  sheet$enrofloxacin<-ifelse(sheet$enrofloxacin=="R",1,0)
  sheet$oxacillina<-ifelse(sheet$oxacillina=="R",1,0)
  sheet$eritromicina<-ifelse(sheet$eritromicina=="R",1,0)
  sheet$gentamicina<-ifelse(sheet$gentamicina=="R",1,0)
  sheet$tetraciclina<-ifelse(sheet$tetraciclina=="R",1,0)
  sheet$ampicillina<-ifelse(sheet$ampicillina=="R",1,0)
  
  

  
  
ds %>% 
  select(SPECIE, identificazione, 12:21) %>% 
  filter(!is.na(identificazione)) %>%
  adorn_totals("col")  %>% 
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



funz<-function(x){
  if(!is.factor(x)){
    x<-factor(x) } 
  else 
  {x}
  abs(as.numeric(x)-2)
}

funz(variabile)

set.seed(3)
dati<-data.frame(Amikacina=sample(c("R", "S"), 100, rep=T, prob=c(0.2, 0.8)),
                 Piperacillina=factor(sample(c("R", "S"), 100, rep=T, prob=c(0.1, 0.9))),#nota
                 Figapelosina=sample(c("R", "S"), 100, rep=T, prob=c(0.15, 0.85)))

head(dati)                 
dati2<-apply(dati, 2, funz)
head(dati2)
