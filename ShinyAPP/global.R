library(shiny)
library(DT)
library(dplyr)
library(tidyr)
library(googlesheets)
library(ggplot2)
library(rpivotTable)
library(timevis)
library(janitor)

dati<-gs_title("AMR2")
ds <-gs_read(dati, ws="AMR" )
ds<-ds %>% 
  filter(!is.na(identificazione))
timing <- gs_read(dati, ws="timing" )

funz<-function(x){
  
  abs(as.numeric(as.factor(x))-2)
}

ds2<-ds
  
ds2[,13:22]<-apply(ds2[,13:22], 2 , funz)


