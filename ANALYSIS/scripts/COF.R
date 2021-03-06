library("tidyverse")
library("lubridate")
library("timevis")
library("here")
library("readxl")
library(shiny)
library(DT)
library(dplyr)
library(tidyr)
library(ggplot2)
library(rpivotTable)
library(timevis)
library(janitor)
#library("vistime")

library("tidyverse")
library("timevis")
library("readxl")
library("here")
library("reshape2")
library("hrbrthemes")
library("scales")



mytime<- read_excel(sheet = "timing", col_types = c("numeric", 
                                                    "text", "date", "date", "text"), here("ANALYSIS", "data", "raw", "timing.xlsx"))



mytime %>% 
  select("event" = content, start, end) %>% 
  vistime()


timing <- read_excel("ANALYSIS/data/raw/timing.xlsx", 
                     col_types = c( "text", "date", 
                                   "date"))

timing <- timing %>% 
  mutate(event = factor(event, levels = c("Presentazione proposta", 
                                         "Approvazione CS IZSLER",
                                         "Approvazione MS e Inizio Progetto",
                                         "Inizio attività borsa di studio",
                                         "Attività di raccolta campioni di feci",
                                         "Analisi  microbiologiche isolamento, identificazione e test di suscettibilità",
                                         "Invio Relazione Parziale",
                                         "Analisi metagenomica su pool di E.coli",
                                         "Approvazione richiesta proroga durata progetto",
                                         "Analisi genoma ceppi E.coli resistenti a Ceftiofur",
                                         "Elaborazione dati genoma ceppi E.coli resistenti a Ceftiofur",
                                         "Analisi statistica dati  dei profili fenotipici di resistenza",
                                         "Analisi dati metagenomici",
                                         "Editing relazione finale",
                                         "Invio Relazione Finale", 
                                         "Sospensione per emergenza COVID-19")))  
 



  melt(timing,  measure.vars = c("start", "end")) %>% 
    mutate(event = fct_rev(event), 
           value = as.Date(value)) %>% 
  ggplot(aes(value, event,  color = event))+
  geom_line(size = 6, alpha = 0.8)+ 
  xlab(NULL) + 
  ylab(NULL) +
    theme_ipsum_rc()+
    theme(legend.position = "blank")+
    scale_x_date(labels = date_format("%m-%Y"), breaks = "6 month")+
    scale_color_manual(values=c(rep("#487DA8", 16)))


dom<-domestici %>% 
mutate(Specieagg=recode(Specieagg,"BOVIDE"="domBOVIDI"))

names(dom)[c(13:22)]<-c("COL", "CFT", "til","KAN",
                        "ENR", "ox", "er", "GEN", "TET", "AMP")
dom<-dom %>%  
select(Specieagg, 13,14,16,17,20:22)%>% 
  mutate(gruppo=rep("Domestici", 62))

  
amr<-AMR %>%
  filter(identificazione=="E.coli") %>% 
  select(Specieagg, 13,14,16,17,20:22) %>% 
  mutate(gruppo= rep("Selvatici",1:n())) %>% 
  rbind(dom) 


amr$COL<-ifelse(amr$COL=='R', 'COL',0)
amr$CFT<-ifelse(amr$CFT=='R', 'CFT',0)
amr$KAN<-ifelse(amr$KAN=='R', 'KAN',0)
amr$ENR<-ifelse(amr$ENR=='R', 'ENR',0)
amr$GEN<-ifelse(amr$GEN=='R', 'GEN',0)
amr$TET<-ifelse(amr$TET=='R', 'TET',0)
amr$AMP<-ifelse(amr$AMP=='R', 'AMP',0)
amr[,2:8]<-amr[,2:8] != 0
nomi_abb<-toupper(abbreviate(names(amr)[2:8]))
X<-  apply(amr[, 2:8], 1, function(x) nomi_abb[x])
XX<-lapply(X, paste, collapse="-")
amr$profilo<-unlist(XX)
amr<-amr %>% 
  filter(!profilo  %in% c("NA-NA-NA-NA-NA-NA-NA")) %>% 
  mutate( profilo= ifelse(profilo=="", "SUSC", profilo))

amr %>% 
  group_by(Specieagg,profilo) %>% 
  dplyr::summarise(n=n()) %>% 
  ggplot( aes(Specieagg,profilo), label=n) + 
  geom_tile(aes(fill = n)) + 
  geom_text(aes(label = n), size=2) +
  scale_fill_gradient(low = "gray", high = "red")+
  #scale_fill_gradient(low = "lightgrey",high = "steelblue")+
  scale_x_discrete(expand = c(0, 0)) + theme_ipsum_rc()+
  scale_y_discrete(expand = c(0, 0)) + labs(x="Gruppo Specie")+
  theme(legend.position = "bottom",axis.ticks = element_blank(),axis.text.x = element_text(angle = 90, hjust = 1,size=5),axis.text.y = element_text(size=5))

profili<-amr %>% 
  group_by(Specieagg,profilo) %>% 
  dplyr::summarise(n=n()) %>% 
  pivot_wider(names_from = profilo, values_from = n, values_fill=list(n = 0)) %>%
  data.frame()
renyis<-renyi(profili[-1], hill=TRUE)

