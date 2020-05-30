library(tidyverse)
library(DataExplorer)
library(readxl)
library(knitr)
library(knitLatex)
library(brms)
library(eRm)
library(ltm)
library(difR)

df<-AMR <- read_excel("~/gitProgetti/AMR-PRC2016020/ANALYSIS/AMR2.xlsx", 
                      sheet = "AMR")
rm(AMR)


  df<-df %>% 
  filter(!is.na(identificazione)) %>% 
  select(Specieagg,identificazione, c(13,14,16,17,20:22)) 

df<-na.omit(df)

funz<-function(x){
  
  abs(as.numeric(as.factor(x))-2)
}

df[,3:9]<-apply(df[,3:9], 2 , funz)



  

rash<- df[, 3:9]

rm<-RM(rash)

betas <- -coef(rm)     # Item difficulty parameters
round(sort(betas), 2)

plotICC(rm, item.subset = "kanamicina")
abline(v = -2.75, col = "grey")
abline(h = .5, col = "grey")


plotjointICC(rm, item.subset =  1:7, cex = .6)

plotPImap(rm, cex.gen = .55,sorted = TRUE)


#df<-as.data.frame(lapply (df, factor))

  