library(readxl)
library(tidyverse)
library(DataExplorer)
library(janitor)
library(rethinking)
library(AMR)
rm(list=ls())
AMR <- read_excel("dati/AMR_dati completi dei comuni.xlsx")

# amp<-AMR %>% 
#   filter(identificazione=="E.coli") %>% 
#   select(SPECIE, Specieagg, area, ampicillina)

AMR <- AMR %>%
  mutate(bacteria = as.mo(identificazione)) %>% 
  mutate_at(vars(colistina:ampicillina), as.rsi)

data <- eucast_rules(AMR, col_mo = "bacteria")

data <- data %>% 
  mutate(
         genus = mo_genus(bacteria),
         species = mo_species(bacteria))

R<-data%>% 
  filter(identificazione=="E.coli") %>% 
  group_by(Specieagg) %>% 
  summarise(ampicillina = portion_R(ampicillina, minimum = 1),
            oxacillina=portion_R(oxacillina,minimum = 1),
            gentamicina=portion_R(gentamicina,minimum = 1),
            tetraciclina=portion_R(tetraciclina,minimum = 1),
            colistina=portion_R(colistina,minimum = 1),
            enrofloxacin=portion_R(enrofloxacin,minimum = 1),
            tilmicosina=portion_R(tilmicosina,minimum = 1),
            ceftiofur=portion_R(ceftiofur,minimum = 1),
            kanamicina=portion_R(kanamicina,minimum = 1),
            eritromicina=portion_R(eritromicina,minimum = 1))



##################################################################################

 
totx<-data %>%   
  dplyr::select(IDceppo,Specieagg,c(12:22)) %>% 
  gather(antibiotico, esito, 4:13) %>% 
  group_by(Specieagg,identificazione, antibiotico) %>% 
  dplyr::summarise("n"=n())
resx<-data %>% 
  dplyr::select(IDceppo,Specieagg, c(12:22)) %>% 
  gather(antibiotico, esito, 4:13) %>% 
  group_by(Specieagg,identificazione, antibiotico) %>% 
  filter(esito=='R') %>% 
  dplyr::summarise("r"=n())

x<-totx %>% full_join(resx)%>% 
  #factor(identificazione,antibiotico) %>% 
  replace_na(list(r=0)) %>% 
  mutate(R=(r/n)) %>% 

  #filter(identificazione=="Acinetobacter") %>% 
  as.data.frame() 


hpd <- binom.bayes(
  x = x$r, n = x$n, type = "highest", conf.level = 0.95, tol = 1e-9)

x<-cbind(x,hpd[,6:8])
x %>% 
  filter(identificazione=="E.coli") %>% 
  #filter(Specieagg=="CERVIDI") %>% 
  ggplot(aes(x = antibiotico, y = mean))+coord_flip()+
  geom_point() + facet_grid(~Specieagg)+
  geom_segment(aes(x=antibiotico, 
                   xend=antibiotico, 
                   # y=min(`%resistenti`),
                   y=lower,
                   yend=upper 
                   # linetype="dashed", 
                   #
  ))+
  labs(x= "", y= "Resistance prevalence (95% Credibility Interval)") 













x<-x %>% 
rowwise() %>% 
  mutate(lower=as.data.frame(qbeta(c(0.05, 0.95), shape1 = r, shape2 = n))[1,]) %>% 
  mutate(upper=as.data.frame(qbeta(c(0.05, 0.95), shape1 = r, shape2 = n))[2,])


 



hpd <- binom.bayes(
  x = x$r, n = x$r, type = "highest", conf.level = 0.95, tol = 1e-9)
print(hpd)
binom.bayes.densityplot(hpd)
# Remove the extremes from the plot since they make things hard
# to see.
binom.bayes.densityplot(hpd[hpd$x != 0 & hpd$x != 10, ])