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





mytime<- read_excel(here("ANALYSIS", "data", "raw", "timing.xlsx"))

mytime <- mytime %>% 
  mutate(start = as.character(start), 
         end = as.character(end), 
         id = as.integer(id)) %>% 
  data.frame()



png(filename = "time.png")
timevis(mytime)





















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

