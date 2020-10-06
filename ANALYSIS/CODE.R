####DATI E PACCHETTI####


setwd("~/Library/Mobile Documents/com~apple~CloudDocs/gitProject/AMR/ANALYSIS") #CON MAC

setwd("D:/Dati/vito.tranquillo/Desktop/GitProjects/AMR/ANALYSIS") #CON PC
source('pacchetti.r')#<-carica le librerie
source('funzioni.r')#<-carica funzioni in-built
source('dataset.r')#<-carica lo script di preparazione dei dataset da usare per le analisi

options(mc.cores = parallel::detectCores()) 


#QUADRO GENERALE####
#Territorio####
###numero comuni da cui provengono i campioni
AMR_com %>% 
  filter(x=="TRUE") %>% 
  group_by(comune) %>% 
  tally()%>% 
  dim()
####MAPPA CAMPIONAMENTI  (fig.1)
#(uso i dati con tutti i comuni da cui è
#### stato fatto il campionamento non quello con i dati dell'istat che sono di meno...###
source('mappe.r')#<-carica le mappe

AMR_com<-AMR_com %>% 
  drop_na(identificazione) %>% 
  group_by(comune)%>% 
  dplyr::summarise("n"=n()) %>% 
  as.data.frame()

com<-subset(comuni, comuni@data$NOME_COM %in% AMR_com$comune)
com@data<-merge(com@data, AMR_com, by.x = "NOME_COM", by.y = "comune")

mybins=c(1,5,10,15,20,30)
mypalette = colorBin( palette="OrRd",
                      domain=com@data$n, na.color="transparent", bins=mybins, reverse = F)

map1<-leaflet(data=regione) %>% addTiles() %>% 
  addPolygons(data=com, 
              fillColor = ~mypalette(n),
              fillOpacity = 0.9,stroke=FALSE, weight = 10) %>% 
  addLegend( pal=mypalette, values= com@data$n, opacity=0.9, title = "N.Campioni", position = "bottomleft" )%>% 
  addPolygons(data=province, fill="F",color="") %>% 
  addPolygons(data=prov, fill=F, color="blue", weight=1, opacity=1) %>% 
  addPolygons(data=com, fill=F, color="black", weight=1, opacity=1) 
# %>% x aggiungere il titolo per presentazione....
#   addControl(title, position = "topleft", className="map-title")
mapshot(map1, file = "map1.png")
############CARATTERISTICHE DEL TERRITORIO
##tabella 1##
options(digits = 3)
AMR_istat %>%
  select(sup, `denpop(abkmq)`,mediana,hapasc,aziende, capi  ) %>% # select variables to summarise
  summarise_each(funs(min = min(.,na.rm=T), 
                      q25 = quantile(., 0.25, na.rm = T), 
                      median = median(.,na.rm=T), 
                      q75 = quantile(., 0.75, na.rm=T), 
                      max = max(.,na.rm=T),
                      mean = mean(.,na.rm=T), 
                      sd = sd(.,na.rm=T)))%>% 
  gather(stat, val) %>%
  separate(stat, into = c("var", "stat"), sep = "_") %>%
  spread(stat, val) %>%
  select(var, min, q25, median, q75, max, mean, sd) %>% 
  mutate("ord"=c(5,6,2,4,3,1)) %>% 
  mutate("Parametri"= plyr::revalue(var, c("aziende"="Aziende con pascolo", 
                                    "capi"="Capi al pascolo", "mediana"="Altitudine mediana", "sup"="Superficie (Kmq)",
                                    `denpop(abkmq)`="Densità di popolazione (Ab/Kmq)",
                                    "hapasc"="Superficie al pascolo (ettari)"
  ))) %>% 
  arrange(ord) %>% 
  select(Parametri, 2:8) %>% 
  kable("latex", booktabs = TRUE, caption = "Caratterizzazione territoriale e demografica dei comuni di provenienza dei campioni") %>% 
  kable_styling()

AMR_istat %>% 
  filter(x=="TRUE") %>% 
  group_by(urb) %>% 
  summarise(n())

AMR_istat %>% 
  filter(x=="TRUE") %>% 
  group_by(montano) %>% 
  summarise(n()) %>% 
  adorn_totals()




###Fauna Selvatica#####

#numero campioni di feci
AMR_com %>% 
  filter(x=="TRUE") %>%
  group_by(IDcamp) %>% 
  tally() %>% summarise(n=sum(n))

#numero di specie
length(unique(as.character(AMR$SPECIE)))##c'è un NA da togliere....

#tabella 2
AMR_com%>% 
  filter(x=="TRUE") %>% 
  group_by("Gruppo"=Specieagg, SPECIE) %>% 
  summarise(n=n()) %>% 
  mutate(prop=round(100*(n/670),  2)) %>% 
  adorn_totals(where = "row") %>% 
  kable("latex",caption = "Distribuzione del numero di campioni di feci in base alle differenti specie di fauna selvatica di origine, raggruppati per il gruppo-specie di appartenenza", booktabs = T, longtable = T) %>% 
  kable_styling(latex_options = c("repeat_header"))
  
  
  
  
#tabella 2b<-campioni by montanità####
AMR_istat %>% 
  mutate(urb=ifelse(urb==1, "densPop alta",
                    ifelse(urb==2,"densPop media", "densPop scarsa" ))) %>% 
  filter(x=="TRUE") %>% 
  group_by(Specieagg, montano) %>% 
  summarise(N=n()) %>% 
  pivot_wider(names_from = montano, values_from = N, values_fill = list(N=0)) %>% 
  adorn_totals(where = "col")  %>% 
  adorn_totals(where = "row") %>% 
  kable("latex", booktabs = TRUE, caption = "Distribuzione del numero di campioni di feci in base alle caratteristiche territoriali di provenienza per i differenti gruppo-specie di appartenenza (NM= Non Montani, P= Parzialmente Montani, T= Totalmente Montani)") %>% 
  kable_styling()
#tabella 2c<-campioni by urbanizzazione

AMR_istat %>% 
  mutate(urb=ifelse(urb==1, "densPop alta",
                    ifelse(urb==2,"densPop media", "densPop scarsa" ))) %>% 
  filter(x=="TRUE") %>% 
  group_by(Specieagg, urb) %>% 
  summarise(N=n()) %>% 
  pivot_wider(names_from = urb, values_from = N, values_fill = list(N=0)) %>% 
  adorn_totals(where = "col")  %>% 
  adorn_totals(where = "row") %>% 
  select("Gruppo Specie"=Specieagg, "UA"= "densPop alta", "UM" = "densPop media", "UB" = "densPop scarsa", "Totale"= Total) %>% 
  kable("latex",  caption = "Distribuzione del numero di campioni di feci in base al grado di urbanizzazione  dei comuni di provenienza per i differenti gruppo-specie di appartenenza (UA= Urbanizzazione ALta, UM= Urbanizzazione Media, UB= Urbanizzazione Bassa)",
        booktabs = TRUE) %>% 
  kable_styling()



##Esami Microbiologici#####  
options(knitr.kable.NA = 0)

#tabella 3
AMR %>%  
  group_by(identificazione) %>% 
  filter(identificazione!="Non identificabile") %>% 
  tally() %>% arrange(desc(n)) %>% 
  mutate("prop(%)"=round(100*prop.table(n),2)) %>% 
  adorn_totals(where = c("row")) %>% 
  kable("latex", caption = "Distribuzione del numero e proprorzione di ceppi della famiglia Enterobacteriacee suddivise per genere, isolati dai 670 campioni di feci analizzati",
        booktabs = TRUE, longtable = T) %>% 
  kable_styling(latex_options = c("repeat_header"))

##Antibiogrammi####
ab<-AMR %>% 
  filter(identificazione!="Non identificabile") %>% 
  dplyr::select(-15,-18,-19)

ab[,13:19]<-apply(ab[,13:19], 2 , funz)

ab<-ab %>% 
  mutate(MDR = rowSums(.[13:19]))###antibiogram length###
ab$R<- ifelse(ab$MDR==0, 0, 1)
ab$MR<-ifelse(ab$MDR==0, "S", 
              ifelse(ab$MDR>=1 & ab$MDR<3, "R", "MR"))
#figura 2-ab length
ab %>% 
  drop_na(MDR) %>% 
  ggplot(aes(x=as.factor(MDR)))+geom_bar(aes(fill=MR))+
  labs(x="numero di resistenze al panel di antibiotici", 
       y="numero ceppi")
  


#PREVALENZA DI WILD ANIMALS PORTATORI DI R E MR - ALL DATA 671 campioni..####
##....
#dati aggregati per campione per calcolare la prevalenza di animali che hanno
#almeno un ceppo resistente ad almeno un antibiotico
#steps: 1.trasformo i risultati dei singoli AB in 1 e 0 (1=R, 0=S)
#       2.sommo per ogni ceppo i valori e ottengo il numero di resistenze per ceppo
#       3.creo una variabile R che mi indica se il ceppo ha almeno una resistenza
#       4.aggrego il dataset per la variabile IDcamp e sommo i valori di R , valori =>1
#         indicano che nel campione c'era almeno un ceppo con almeno una resistenza
#       5. dal database originale preparo un dataset escludendo i ceppi senza identificazione
#       6. Unisco i due dataframe, che devono avere le stesse dimensioni, usando la variabile
#          IDgruppo comune e creo una variabile chiamata RSelv che ha valore uguale a S se
#          Res è 0, oppure R se Res è >=1. Il nuovo dataframe si chiama Prev è contiene i dati
#          per analizzare la prevalenza di animali portatori di ceppi resistenti... 

#step1
Psel<-AMR %>% 
  # filter(x=="TRUE") %>%
  filter(identificazione!="Non identificabile") %>% 
  dplyr::select(-15,-18,-19)
Psel[,13:19]<-apply(Psel[,13:19], 2 , funz)
#step2
Psel<-Psel %>% 
  mutate(MDR = rowSums(.[13:19]))###antibiogram length###
#step3
Psel$R<- ifelse(Psel$MDR==0, 0, 1) 
#step4
Psel<-Psel %>% 
  drop_na(R) %>% 
  group_by(IDcamp)%>% 
  summarise(Res=sum(R))
#step5
pAMR<-AMR %>% 
  filter(x=="TRUE") %>% 
  filter(identificazione!="Non identificabile")
#step6
Prev<-pAMR %>% 
  left_join(Psel) %>% 
  mutate(RSelv=ifelse(Res==0,"S","R"))
## prevalenza resistenza in specie...####
options(digits=2)
mr<-Prev%>%
  select(Specieagg, RSelv)%>%
  group_by(Specieagg, RSelv) %>%
  #drop_na(RSelv) %>%
  tally() %>%
  pivot_wider(names_from = RSelv, values_from = n, values_fill = list(n = 0)) %>% 
  data.frame() %>% 
  mutate(N=rowSums(.[2:3],na.rm = TRUE)) 
  


options(digits=2)
Rhpd <- binom.bayes(
  x = mr$R, n = mr$N, type = "highest", conf.level = 0.95, tol = 1e-9)
# Rhpd<- cbind("Specieagg"=mr[, 1], Rhpd[,6:8]) %>% 
#   arrange(desc(mean))

Rhpd<- cbind("Specieagg"=mr[, 1], Rhpd) 

R <- Rhpd %>% 
  arrange(desc(mean)) %>% 
  mutate(Specieagg = unique(factor(Specieagg)))

R %>% 
  select("Gruppo Specie" = Specieagg, -method, "R"=x, "N"=n, "Prevalenza"=mean, "inf-HPD"=lower, "sup-HPD"=upper, -shape1, -shape2, -sig) %>% 
  kable("latex", booktabs = T,
        caption = "Stime della prevalenza di campioni resistenti suddivisi per gruppo-specie: R= numero di campioni resistenti, N= numero campioni esaminati, Prevalenza = media della distribuzione beta, inf-HPD= valore inferiore dell'intervallo HPD, sup-HPD = valore superiore dell'intervallo HPD") %>% 
  kable_styling()
  

z <- binom.bayes.densityplot(R)
z+facet_wrap(~Specieagg)+xlab("Prevalenza")

#grafico bayesian density (figura 3)
# prev <-Prev %>% 
#   mutate(prev=ifelse(RSelv=="S", 0, 1)) 
# 
# modn <- stan_glm(prev~ 1, data=prev,family=binomial(link="logit"))
# modp<-stan_glm(prev~ Specieagg, data=prev,family=binomial(link="logit"))
# 
# t<-emmeans(modp, ~Specieagg)
# 
# t %>% as.data.frame() %>% 
#   mutate(emmean=invlogit(emmean),
#              lower.HPD = invlogit(lower.HPD),
#              upper.HPD = invlogit(upper.HPD)) %>% 
#   select("Gruppo-Specie" = Specieagg, "Prevalenza"=emmean, "HPD-inf"= lower.HPD, "HPD-sup"= upper.HPD ) %>% 
#   arrange(desc(Prevalenza)) %>% 
#   kable("latex" ) %>% 
#   kable_styling()



# mod<-glm(prev~ Specieagg, data=prev,family=binomial(link="logit"))


# p<-gather_emmeans_draws(t)

# p %>% 
# mutate("prev"=invlogit(.value))%>% 
#   group_by(Specieagg) %>% 
#   summarise(m=mean(prev),
#             sd=sd(prev))
#   ggplot(aes(x = prev, y=Specieagg,fill = Specieagg)) +
#   geom_density_ridges(panel_scaling=FALSE)+
#   theme_ridges()+
#   scale_fill_brewer(palette = 7) +
#   theme_ridges() + theme(legend.position = "NULL")+labs(x="Prevalenza",
#                                                         y="")

#prevalenza multi-resistenza###################

mPsel<-AMR %>% 
  filter(identificazione!="Non identificabile") %>% 
  dplyr::select(-15,-18,-19)
mPsel[,13:19]<-apply(mPsel[,13:19], 2 , funz)
#step2
mPsel<-mPsel %>% 
  mutate(MDR = rowSums(.[13:19]))###antibiogram length###
#step3
mPsel$MR<- ifelse(mPsel$MDR==0 |mPsel$MDR<=2, 0, 1) 
mPsel$R<- ifelse(mPsel$MDR==0, 0, 1)

mPsel<-mPsel %>% 
  drop_na(R) %>% 
  drop_na(MR) %>% 
  group_by(IDcamp)%>% 
  summarise(Res=sum(R), 
            MRes=sum(MR))
#step5
pAMR<-AMR %>% 
  filter(x=="TRUE") %>% 
  filter(identificazione!="Non identificabile")
#step6
mPrev<-pAMR %>% 
  left_join(mPsel) %>% 
  mutate(MRSelv=ifelse(MRes==0,"Sr","MR"))

binom.bayes(
  x = 92, n = 670)

options(digits=2)
Mr<-mPrev%>%
  select(Specieagg, MRSelv)%>%
  group_by(Specieagg, MRSelv) %>%
  drop_na(MRSelv) %>%
  tally() %>%
  pivot_wider(names_from = MRSelv, values_from = n, values_fill = list(n = 0)) %>% 
  data.frame() %>% 
  mutate(N=rowSums(.[2:3],na.rm = TRUE)) 
options(digits=2)
MRhpd <- binom.bayes(
  x = Mr$MR, n = Mr$N, type = "highest", conf.level = 0.95, tol = 1e-9)
MRhpd<- cbind("Specieagg"=mr[, 1], MRhpd)

MR <- MRhpd %>% 
  arrange(desc(mean)) %>% 
  mutate(Specieagg = unique(factor(Specieagg)))

MR %>% 
  select(-method, "MR"=x, "N"=n, "Prevalenza"=mean, "inf-HPD"=lower, "sup-HPD"=upper, -shape1, -shape2, -sig) %>% 
  kable("latex", caption = "Stime della prevalenza di campioni multi-resistenti suddivisi per gruppo-specie: MR= numero di campioni resistenti, N= numero campioni esaminati, Prevalenza = media della distribuzione beta, inf-HPD= valore inferiore dell'intervallo HPD, sup-HPD = valore superiore dell'intervallo HPD)",
        booktabs = T) %>% 
  kable_styling()


z <- binom.bayes.densityplot(MR)
z+facet_wrap(~Specieagg)+xlab("Prevalenza")
#bayesian density
#grafico bayesian density (figura 3)
# mprev <-mPrev %>% 
#   mutate(prev=ifelse(MRSelv=="Sr", 0, 1)) 
# 
# modp<-stan_glm(prev~ Specieagg, data=mprev,family=binomial(link="logit"))
# 
# t<-emmeans(modp, ~Specieagg)
# p<-gather_emmeans_draws(t)
# 
# p %>% 
#   mutate("prev"=logit2prob(.value))%>% 
#   ggplot(aes(x = prev, y=Specieagg,fill = Specieagg)) +
#   geom_density_ridges(panel_scaling=TRUE)+
#   theme_ridges()+
#   scale_fill_brewer(palette = 7) +
#   theme_ridges() + theme(legend.position = "NULL")+labs(x="Prevalenza MR Intervalli di Credibilità Bayesiani 95%",y="")
#                      
#PREVALENZA R E MR IN WILDLIFE BY TERRITORIO E SPECIE####
#USO DATASET ComAMRsel 640 RIGHE
  #-prevalenza  R by URB                                                    
RbyT<-ComAMRsel %>% 
  mutate(urb=ifelse(urb==1, "densPop alta",
                    ifelse(urb==2,"densPop media", "densPop scarsa" ))) %>% 
  drop_na(Res) %>% 
  select(Specieagg, montano, Res, MRes) %>% 
  group_by(Specieagg, montano, Res) %>% 
  summarise(n=n()) %>% 
  pivot_wider(names_from = Res, values_from = n, values_fill = list(n=0)) %>% 
  mutate(N=S+R) %>% 
  data.frame()
  hpd <- binom.bayes(
  x = RbyT$R, n = RbyT$N, type = "highest", conf.level = 0.95, tol = 1e-9)
  
  
  Rhpd<- cbind("Specieagg"=RbyT[, 1], hpd[,6:8])
 
Rhpd<-cbind(RbyT, Rhpd[,-1])
  
#Bayesian multilevel model R~(1|comune)+ Specie+ pascolo+ urbanizzazione####
#codici nel file bayesmod.R
                                                                          

#CARATTERIZZAZIONE FENOTIPICA DELL'ANTIBIOTICO-RESISTENZA DEGLI ISOLATI BATTERICI####

#Antibiotico-resistenza byAB <- tabella 4####
z <-AMR %>%
  filter(identificazione!="Non identificabile") %>% 
  select(Specieagg,identificazione, 13,14,16,17,20:22) %>% 
  pivot_longer(cols=3:9, names_to = "antibiotico") %>% 
  group_by(antibiotico,value) %>% 
  drop_na(value) %>% 
  tally() %>% 
  pivot_wider(names_from = value,values_from = n) %>% 
  mutate("N"=R+S, 
         "prop" = round(100*(R/N),2)) %>% 
  arrange(desc(prop))

  
options(digits=2)
Rhpd <- binom.bayes(
  x = z$R, n = z$N, type = "highest", conf.level = 0.95, tol = 1e-9)
# Rhpd<- cbind("Specieagg"=mr[, 1], Rhpd[,6:8]) %>% 
#   arrange(desc(mean))

Rhpd<- cbind("Antibiotico"=z[, 1], Rhpd[]) 

R <- Rhpd[-8,] %>% 
  arrange(desc(mean)) %>% 
  mutate(Antibiotico = unique(factor(antibiotico)))

R %>% 
  select(Antibiotico,-method, "R"=x, "N"=n, "Prevalenza"=mean, "inf-HPD"=lower, "sup-HPD"=upper, -shape1, -shape2, -sig, -antibiotico) %>% 
  kable("latex", caption = "Stime bayesiane della prevalenza di ceppi resistenti ai differenti antibiotici", booktabs = TRUE) %>% 
  kable_styling()


pz <- binom.bayes.densityplot(R, fill.central = "steelblue", fill.lower = "steelblue",
                              alpha = 1.2) +facet_wrap(~Antibiotico)


#--Bayesian posterior prevalence of seven antibiotic-resistence phenotype
#--figura 5



dt<-AMR %>%
  select(Specieagg,identificazione, 13,14,16,17,20:22) %>% 
  na.omit() 
dt[,3:9]<-apply(dt[,3:9], 2 , funz)
AB<-data.frame(dt[,3:9])
my_lms <- lapply(1:7, function(x) stan_glm(AB[,x] ~ Specieagg, data=dt,
                                           family=binomial(link="logit")))
t<- lapply(1:7, function(x) emmeans(my_lms[[x]], ~Specieagg))
#t2<-lapply(1:7, function(x) contrast(t[[x]], method="eff"))
z<- lapply(1:7, function(x) gather_emmeans_draws(t[[x]]))

bigdf<-do.call(rbind, z)
ab<-c(rep("COL", 36000), rep("CFT", 36000), rep("KAN", 36000), rep("ENR",36000),
      rep("GEN", 36000), rep("TET",36000), rep("AMP",36000))

bigdf<-cbind(bigdf, "ab"=ab)
bigdf <- bigdf %>% 
  mutate(ab = factor(ab, levels = c("AMP", "TET","CFT", "COL", "ENR", "KAN", "GEN")))  
bigfg<- bigdf %>% 
  ggplot(aes(x = logit2prob(.value), y=Specieagg,fill = Specieagg)) +geom_density_ridges(panel_scaling=TRUE)+
  theme_ridges()+facet_wrap(~ab)+
  scale_fill_brewer(palette = "Blues") +
  theme_ridges() + theme(legend.position = "NULL")+labs(x="Prevalenza", y="")
#tabella 4

bigdf %>% 
  group_by(Specieagg,ab) %>% 
  summarise(prev=round(mean(logit2prob(.value)),2)) %>% 
  pivot_wider(names_from = ab, values_from = "prev") %>% 
  kable("latex", booktabs = T, caption = "Profilo di resistenza dei ceppi ai diversi antibiotici per gruppo-specie di provevienza del campione di feci") %>% 
  kable_styling()



#Antibiotico-resistenza by genere <-tabella 5####
tot<-AMR %>%
  select(identificazione, 13,14,16,17,20:22) %>% 
  pivot_longer(cols=2:8, names_to = "antibiotico") %>% 
  group_by(identificazione,antibiotico) %>% 
  tally(name="tot")

res<-AMR %>% 
  select(identificazione, 13,14,16,17,20:22) %>% 
  pivot_longer(cols=2:8, names_to = "antibiotico") %>% 
  group_by(identificazione, antibiotico) %>% 
  filter(value=='R') %>% 
  tally(name="res") 

tot %>% full_join(res)%>% 
  replace_na(list(res=0)) %>% 
  filter(identificazione!="Non identificabile") %>% 
  mutate("%R"=(res/tot)*100) %>% 
  select(-res) %>% 
  pivot_wider(names_from = antibiotico, values_from = `%R`) %>% 
  arrange(desc(tot)) %>% 
  select("Genere" = identificazione, "N.ceppi" = tot, AMP, TET, CFT, COL, ENR, KAN, GEN, ) %>% 
  kable("latex", digits = 2, booktabs= T, caption = "Profilo di antibiotico-resistenza tra i  diversi generi dei ceppi isolati") %>% 
  kable_styling()



#Profilo di multiresistenza####
 amr %>% 
  filter(profilo!="SUSC") %>% 
  group_by(profilo) %>% 
  dplyr::summarise(n=n()) %>% 
  arrange(n) %>% 
  #top_n(10, n) %>% 
  mutate(profilo = factor(profilo, unique(profilo))) %>% 
  #ggplot(aes(x=profilo, y=n))+geom_bar(stat = "identity")+coord_flip()
  ggplot(aes(x=profilo, y=n, label=n))+
  geom_segment( aes(x=profilo, xend=profilo, y=0, yend=n), color="grey")+
  geom_point( aes(x=profilo, y=n), size=8.4, color="steelblue" )+
  geom_text(color="white", size=4)+
  coord_flip()+
  theme_ipsum_rc()+
  labs(y="n.ceppi",x="")


#prevalenza ceppi ABr e MAbr per gruppo-specie di provenienza del campione




####Biodiversità<---fig.6####
  
amr %>% 
    group_by(Specieagg,profilo) %>% 
    dplyr::summarise(n=n()) %>% 
    ggplot( aes(Specieagg,profilo), label=n) + 
    geom_tile(aes(fill = n)) + 
    geom_text(aes(label = n), size=4) +
    scale_fill_gradient(low = "gray", high = "red")+
    #scale_fill_gradient(low = "lightgrey",high = "steelblue")+
    scale_x_discrete(expand = c(0, 0)) + theme_ipsum_rc()+
    scale_y_discrete(expand = c(0, 0)) + labs(x="Gruppo Specie")+
    theme(legend.position = "bottom",axis.ticks = element_blank(),axis.text.x = element_text(angle = 90, hjust = 1,size=8),axis.text.y = element_text(size=8))

 profili<-amr %>% 
    group_by(Specieagg,profilo) %>% 
    dplyr::summarise(n=n()) %>% 
    pivot_wider(names_from = profilo, values_from = n, values_fill=list(n = 0)) %>%
    data.frame()
 
  renyis<-renyi(profili[-1], hill=TRUE)
  
  # tabella valori renyis <-tabella 6
  specie<-levels(profili$Specieagg)
  
  tab<-renyis
  row.names(tab)<-specie
  tab%>% 
    kable("latex", digits = 2, booktabs = T, caption = "Valori di entropia di Renyi standardizzati tra i diversi gruppi specie" ) %>% 
    kable_styling()

 ###fig.7#<-graifico di Renyis standardizzato
  renyis<-renyis-renyis[,1]
   renyis %>% 
    mutate(Specie=levels(profili$Specieagg)) %>% 
    pivot_longer(cols=1:11, names_to="alpha",values_to = "indici" ) %>% 
    mutate(alpha=factor(alpha, levels=c("0", "0.25", "0.5","1", "2", "4", "8", "16","32","64","Inf"))) %>% 
    ggplot(aes(x=alpha, y=indici, color=Specie, group=Specie))+
    geom_line()+theme_bw()+
    labs(y="Rényi entropy", x="Rényi scale (alpha)")+
    theme(legend.position="bottom",legend.text=element_text(size=7),
          legend.title = element_blank())#+
    #scale_color_manual(values = brewer.pal(12, "Set1"))
  
   
   
   
   
   
   
   
   
#####META-ANALISI PREVALENZE####
   amrbib <- read_excel("meta.xlsx")
   amrbib<-amrbib %>% 
     filter(articolo!="8") %>% 
     filter(articolo!="10")
   
   options(digits=2)
   resbinom <- binom.bayes(
     x = amrbib$nresitenti, n = amrbib$nisolati,
     type = "highest", conf.level = 0.95, tol = 1e-9)
   
   
   metares<-cbind(amrbib, resbinom[,6:8])
   
 
   metares %>% 
     ggplot( aes(y=mean,ymin=lower, ymax=upper, x=articolo))+
     geom_point(color="blue", size=2)+geom_linerange(color="blue", size=.8)+
     coord_flip()+
     theme_ipsum_rc(axis_title_just = "mc")+
     facet_wrap(~specie)+
     labs(x="", y="Prevalenza")
 
   
  
######################################################
  
  
  # #biodiversità    
  # coli<-amr %>% 
  #   mutate(genere=ifelse(identificazione!= "E.coli", "Enterobacteriacee", "E.coli")) %>% 
  #   filter(genere=="E.coli") %>% 
  #   group_by(Specieagg,profilo) %>% 
  #   dplyr::summarise(n=n()) %>% 
  #   pivot_wider(names_from = profilo, values_from = n, values_fill=list(n = 0)) %>%
  #   data.frame() 
  # 
  # coli.renyis<-exp(renyi(coli[,-1]))
  # 
  # 
  # 
  # entb<-amr %>% 
  #   mutate(genere=ifelse(identificazione!= "E.coli", "Enterobacteriacee", "E.coli")) %>% 
  #   filter(genere=="Enterobacteriacee") %>% 
  #   group_by(Specieagg,profilo) %>% 
  #   dplyr::summarise(n=n()) %>% 
  #   pivot_wider(names_from = profilo, values_from = n, values_fill=list(n = 0)) %>%
  #   data.frame()
  # 
  # entb.renyis<-exp(renyi(entb[,-1]))
  # 
  # genere<-rbind(coli.renyis,entb.renyis)
  # 
  # 
  # genere %>% 
  #   mutate(Specie=levels(profili$Specieagg)) %>% 
  #   mutate(genere=c(rep("E.coli",9), rep("Altre Enterobacteriacee", 9))) %>% 
  #   pivot_longer(cols=1:11, names_to="alpha",values_to = "indici" ) %>% 
  #   mutate(alpha=factor(alpha, levels=c("0", "0.25", "0.5","1", "2", "4", "8", "16","32","64","Inf"))) %>% 
  #   ggplot(aes(x=alpha, y=indici, color=Specie, group=Specie))+
  #   geom_point()+geom_line()+facet_grid(~genere)
  # 
  # 
  

###################################################################




###Prevalenza ceppi multiresistenti per specie (Bayes)
# d2<-d %>% 
#   fastDummies::dummy_columns("MR")
# 
# AB<-data.frame(d2[,24:26])
# my_lms <- lapply(1:3, function(x) stan_glm(AB[,x] ~ Specieagg, data=d2,
#                                            family=binomial(link="logit")))
# 
# t<- lapply(1:3, function(x) emmeans(my_lms[[x]], ~Specieagg))
# t2<-lapply(1:3, function(x) contrast(t[[x]], method="eff"))
# z<- lapply(1:3, function(x) gather_emmeans_draws(t2[[x]]))
# 
# bigdf<-do.call(rbind, z)
# 
# MR<-c(rep("Multiresistenti", 36000),rep("Resistenti", 36000), rep("Suscettibili", 36000))
# 
# bigdf<-cbind(bigdf, "MR"=MR)
# 
# bigfg<-bigdf %>% 
#   mutate(MR=factor(MR, levels=c("Suscettibili", "Resistenti", "Multiresistenti"))) %>% 
#   mutate("prev"=.value) %>% 
#   ggplot(aes(x = prev, y=contrast,fill = contrast)) +geom_density_ridges(panel_scaling=TRUE)+
#   theme_ridges()+facet_wrap(~MR)+
#   scale_fill_brewer(palette = 7) +
#   theme_ridges() + theme(legend.position = "NULL")+labs(x="Differenze prevalenza intra-specie vs Prevalenza media e Intervalli di Credibilità Bayesiani 95%",
#                                                         y="")
# 
# 
# 
# 
# 
# 







### PROFILO RESISTENZE WILDLIFE





# funz<-function(x){
#   
#   abs(as.numeric(as.factor(x))-2)
# }

##tabella ceppi S/R/MR by specie fauna
# d %>% 
#   mutate(Specieagg=forcats::fct_explicit_na(Specieagg)) %>% 
#   mutate(MR=factor(MR, levels=c("S","R","MR"), ordered=TRUE)) %>% 
#   drop_na(MR) %>% 
#   group_by(Specieagg, MR) %>% 
#   tally()%>% 
#   #mutate(prop=100*prop.table(n)) %>% 
#   #arrange(desc(n))
#   pivot_wider(names_from = MR, values_from = n, values_fill = list(n=0)) %>% 
#   kable("latex") %>% 
#   kable_styling()



# mr<-d%>%
#   select(Specieagg, MR) %>% 
#   group_by(Specieagg, MR) %>% 
#   drop_na(MR) %>% 
#   tally() %>% 
#   pivot_wider(names_from = MR, values_from = n) %>% 
#   mutate(N=rowSums(.[2:4],na.rm = TRUE)) %>% 
#   data.frame()
# options(digits=2)
# Shpd <- binom.bayes(
#   x = mr$S, n = mr$N, type = "highest", conf.level = 0.95, tol = 1e-9)
# Shpd<- cbind("Specieagg"=mr[, 1], Shpd[,6:8], rep("S",9))
# names(Shpd)[2:5]<-c("m", "low", "hig", "gruppo")
# 
# Rhpd <- binom.bayes(
#   x = mr$R, n = mr$N, type = "highest", conf.level = 0.95, tol = 1e-9)
# Rhpd<- cbind("Specieagg"=mr[, 1], Rhpd[,6:8], rep("R",9))
# names(Rhpd)[2:5]<-c("m", "low", "hig", "gruppo")
# 
# mRhpd <- binom.bayes(
#   x = mr$MR, n = mr$N, type = "highest", conf.level = 0.95, tol = 1e-9)
# mRhpd<- cbind("Specieagg"=mr[, 1], mRhpd[,6:8], rep("MR",9))
# names(mRhpd)[2:5]<-c("m", "low", "hig", "gruppo")
# 
# MR<-rbind(Shpd, Rhpd, mRhpd)
# MR %>% 
#   pivot_longer(cols=2:4, names_to = "par")
# MR %>% 
#   ggplot( aes(y=m,ymin=low, ymax=hig, x=Specieagg))+
#   geom_point(color="blue", size=2)+geom_linerange(color="blue", size=.8)+
#   facet_wrap(~gruppo)+coord_flip()+
#   theme_ipsum_rc(axis_title_just = "mc")+
#   labs(x="", y="Prevalenza")


#Analisi delle corrispondenze
# mr<-mr %>% 
#   column_to_rownames(var="Specieagg") %>% 
#   select(-4)
# 
# dt <- as.table(as.matrix(mr[-4]))
# res.ca <- CA(mr, graph = FALSE)
# fviz_ca_biplot(res.ca, repel = TRUE)

#######PROFILI MULTIRESISTENZA
# amr <- AMR %>%
#   drop_na(Specieagg) %>%
#   filter(identificazione!="Non identificabile") %>%
#   dplyr::select(-15,-18,-19)
# amr$COL<-ifelse(amr$COL=='R', 'COL',0)
# amr$CFT<-ifelse(amr$CFT=='R', 'CFT',0)
# #amr$tilmicosina<-ifelse(amr$tilmicosina=='R', 'TIL',0)
# amr$KAN<-ifelse(amr$KAN=='R', 'KAN',0)
# amr$ENR<-ifelse(amr$ENR=='R', 'ENR',0)
# #amr$oxacillina<-ifelse(amr$oxacillina=='R', 'OXA',0)
# #amr$eritromicina<-ifelse(amr$eritromicina=='R', 'ERT',0)
# amr$GEN<-ifelse(amr$GEN=='R', 'GEN',0)
# amr$TET<-ifelse(amr$TET=='R', 'TET',0)
# amr$AMP<-ifelse(amr$AMP=='R', 'AMP',0)
# 
# #write.table(amr, file="amrxx.csv")
# 
# amr[,13:19]<-amr[,13:19] != 0
# nomi_abb<-toupper(abbreviate(names(amr)[13:19]))
# X<-  apply(amr[, 13:19], 1, function(x) nomi_abb[x])
# XX<-lapply(X, paste, collapse="-")
# 
# amr$profilo<-unlist(XX)



#png("fig3.png", height = 550, width = 600)
# amr %>% 
#   filter(!profilo  %in% c("NA-NA-NA-NA-NA-NA-NA")) %>% 
#   mutate( profilo= ifelse(profilo=="", "SUSC", profilo))%>% 
# amr %>%   
#   group_by(profilo) %>% 
#   dplyr::summarise(n=n()) %>% 
#   arrange(n) %>% 
#   #top_n(10, n) %>% 
#   mutate(profilo = factor(profilo, unique(profilo))) %>% 
#   #ggplot(aes(x=profilo, y=n))+geom_bar(stat = "identity")+coord_flip()
#   ggplot(aes(x=profilo, y=n, label=n))+
#   geom_segment( aes(x=profilo, xend=profilo, y=0, yend=n), color="grey")+
#   geom_point( aes(x=profilo, y=n), size=8.4, color="lightblue" )+
#   geom_text(color="black", size=4)+
#   coord_flip()+
#   theme_ipsum_rc(axis_title_just = "mc")+
#   labs(y="n.ceppi",x="Profilo di resistenza/multiresistenza")
 






# dat <- read.csv("spcdat.csv", header=TRUE, row.names=1)
# exp(renyi(dat))
# dat2 <- t(dat[4:8,])
# out <- iNEXT(dat2, q=c(0,1,2), datatype="abundance")
# 
# 
# profili2<-t(data.frame(profili[,-1], row.names = profili[,1]))
# out <- iNEXT(profili2, q=c(0,1,2), datatype="abundance")
# ggiNEXT(out, type=3, facet.var = "order")

# alpha<-diversityvariables(profili[,-1], y=NULL, digits=2)
# alpha<-data.frame(do.call(rbind, alpha))

#renyiplot(renyi(profili[,-1]))

#diversityresult(prof, y=NULL, digits=2, index="richness", method="s")


# plot(renyiaccum(prof))
# 
# Renyi.1 <- renyiresult(prof, method = "s")
# 
# abdist <- vegdist(prof, method = "bray")

#renyiplot(renyi(profili[-1]))



# ARINDEX

# funz<-function(x){
#   
#   abs(as.numeric(as.factor(x))-2)
# }

d2<-d %>% 
  dplyr::select(-15,-18,-19)



d2[,13:19]<-apply(d2[,13:19], 2 , funz)



d2$res<-rowSums(d2[,13:19], na.rm = T)

d2<-d2 %>% 
  mutate(nab=7) %>% 
  mutate(ARi=res/nab)

###Distribuzione ARi per isolato

d2 %>%
  arrange(ARi) %>% 
  mutate(IDceppo = factor(IDceppo, unique(IDceppo))) %>% 
  top_n(300, ARi) %>% 
  ggplot(aes(x=IDceppo, y=ARi))+
  geom_segment( aes(x=IDceppo, xend=IDceppo, y=0, yend=ARi), color="grey")+
  geom_point( aes(x=IDceppo, y=ARi), size=1, color="black" )+
  coord_flip()

d2<-d2 %>% 
  mutate(AR0.20= ifelse(ARi<0.20, "BPS", "APS"))

table(d2$AR0.20)

####ARi x Genere###

png("fig5.png", height = 550, width = 600)
d2 %>% 
  filter(!(identificazione=="Non identificabile")) %>% 
  group_by(identificazione) %>% 
  dplyr::summarise(Somma=sum(res),
                   nceppi=n(),
                   nab=7) %>% 
  mutate(ARi=round(Somma/(nceppi*nab),2)) %>% 
  arrange(ARi) %>% 
  mutate(identificazione = factor(identificazione, unique(identificazione))) %>% 
  ggplot(aes(x=identificazione, y=ARi, label=ARi))+
  geom_segment( aes(x=identificazione, xend=identificazione, y=0, yend=ARi), color="black")+
  geom_point( aes(x=identificazione, y=ARi), size=8.3, color="lightblue" )+
  geom_text(color="black", size=4)+
  coord_flip()+
  theme_ipsum_rc(axis_title_just = "mc")+
  labs(y="ARindex",x="Genere ceppi isolati")+
  geom_hline(yintercept =0.20, col="red")
dev.off()

####ARi x Specie###

ARispec<-d2 %>% 
  group_by(Specieagg) %>% 
  dplyr::summarise(Somma=sum(res),
                   nceppi=n(),
                   nab=7) %>% 
  mutate(ARi=Somma/(nceppi*nab))


plot(density(d2$ARi))


#######################

esbl <- read_excel("RELAZIONE FINALE/esbl.xlsx")

esbl %>% 
  drop_na() %>% 
  kable("latex", caption = "Caratterizzazione genotipica dei geni di resistenza dei 47 ceppi resistenti a Ceftiofur",
        booktabs = TRUE, longtable = T) %>% 
  kable_styling(latex_options = c("repeat_header"))


###Grafico

# AMR$x<-!duplicated(AMR$IDcamp)#<-crea una variabile che identifica i singoli campioni di feci
# AMR %>% 
#   filter(x=="TRUE") %>% #filtra i campioni non duplicati 
#   group_by(Specieagg) %>% 
#   tally() %>% 
#   arrange(desc(n)) %>% 
#   mutate(prop=100*prop.table(n)) %>% 
#   ggplot(aes(x=n, y=fct_reorder(Specieagg, n), label=paste(round(prop, 1),"%")))+
#   # geom_segment( aes(x=Specieagg, xend=Specieagg, y=0, yend=n), color="grey")+
#   geom_point(size=15, color="grey")+
#   geom_text(color="black", size=4)+
#   labs(x="N.campioni")+
#   theme_ipsum_rc(axis_title_just = "mc")+
#   theme(
#     legend.position = "none",
#     panel.border = element_blank(),
#     panel.spacing = unit(0.1, "lines"),
#     strip.text.x = element_text(size = 10, face="bold"),
#     axis.text.y=element_text(size=10),
#     axis.title.x = element_text(size = 14),
#     plot.title = element_text(color = "blue",  face = "bold", size=13.2),
#     plot.caption = element_text(color = "blue", face = "italic", size=10),
#     plot.subtitle = element_text(size=10)
#     
#     
#   ) +
#   
#   labs(y="",x="N.campioni",
#        title="Ruolo della fauna selvatica nella diffusione e mantenimento dell'antibiotico-resistenza (PRC2016020-IZSLER)-risultati preliminari", 
#        subtitle = "Distribuzione di 729 campioni di feci per specie di provenienza",
#        caption="
#        Cervidi: Capriolo, Cervo; 
#        
#        sCaprine: Camoscio,Stambecco, Muflone; 
#        
#        U.acquatici: Cigno, Gabbiano, Cormorano, Germano; 
#        
#        Altri Vol: Piccione, Starna, Fagiano")

##Distribuzione dei ceppi e dei ceppi  per Specie di provenienza delle feci




# AMR %>%  
#   group_by(Specieagg, identificazione) %>% 
#   filter(identificazione!="Non identificabile") %>% 
#   tally() %>%   arrange(desc(n)) %>% 
#   pivot_wider(names_from = Specieagg, values_from = n) %>% 
#   adorn_totals(where = c("row","col")) %>% 
#   kable("latex" ) %>% 
#   kable_styling()


 
##########Grafico con la ditribuzione dei ceppi 
#isolati per specie di provenieinza delle feci#
 
# AMR %>%  
#   group_by(Specieagg) %>% 
#   tally() %>% 
#   arrange(desc(n)) %>% 
#   mutate(prop=100*prop.table(n)) %>% 
#   ggplot(aes(x=n, y=fct_reorder(Specieagg, n), label=paste(round(prop, 1),"%")))+
#   # geom_segment( aes(x=Specieagg, xend=Specieagg, y=0, yend=n), color="grey")+
#   geom_point(size=15, color="grey")+
#   geom_text(color="black", size=4)+
#   labs(x="N.campioni")+
#   theme_ipsum_rc(axis_title_just = "mc")+
#   theme(
#     legend.position = "none",
#     panel.border = element_blank(),
#     panel.spacing = unit(0.1, "lines"),
#     strip.text.x = element_text(size = 10, face="bold"),
#     axis.text.y=element_text(size=10),
#     axis.title.x = element_text(size = 16),
#     plot.title = element_text(color = "blue",  face = "bold"),
#     plot.caption = element_text(color = "blue", face = "italic", size=10),
#     plot.subtitle = element_text(size=10)
#     
#     
#   ) +
#   
#   labs(y="",x="N.campioni",
#        title="Ruolo della fauna selvatica nella diffusione e mantenimento dell'antibiotico-resistenza (PRC2016020-IZSLER)-risultati preliminari", 
#        subtitle = "Distribuzione dei 941 ceppi batterici isolati per specie di provenienza delle feci",
#        caption="
#        Cervidi: Capriolo, Cervo; 
#        
#        sCaprine: Camoscio,Stambecco, Muflone; 
#        
#        U.acquatici: Cigno, Gabbiano, Cormorano, Germano;
#        
#        Altri Vol: Piccione, Starna, Fagiano")

######mappa della provenienza dei campioni di feci 


####MD 
# funz<-function(x){
#   
#   abs(as.numeric(as.factor(x))-2)
# }

# d<-d %>% 
#   select(-tilmicosina,-oxacillina, -eritromicina)

# amr[,13:19]<-apply(amr[,13:19], 2 , funz)
# 
# amr<-amr %>% 
#   mutate(MDR = rowSums(.[13:19])) 

#amr<-amr[-416,]
######DESCRITTIVA DEI COMUNI

# d<-d %>% 
#   #filter(Specieagg=="") %>% 
#   drop_na(identificazione) %>% 
#   group_by(comune)%>% 
#   dplyr::summarise("n"=n())#,
#   #                  MAR=sum(MDR)/(n*7)) %>% 
#   as.data.frame()
# 
# 
# com<-subset(comuni, comuni@data$NOME_COM %in% d$comune)
# 
# com@data<-merge(com@data, d, by.x = "NOME_COM", by.y = "comune")

# com@data$MAR[ which(com@data$MAR == 0)] = NA


####titolo
#
# # mytext=paste("N.campioni: ", round(com@data$n,2),  sep="") %>%
#   lapply(htmltools::HTML)
# 
# tag.map.title <- tags$style(HTML("
#   .leaflet-control.map-title { 
#     transform: translate(-50%,20%);
#     position: fixed !important;
#     left: 50%;
#     text-align: center;
#     padding-left: 10px; 
#     padding-right: 10px; 
#     background: rgba(255,255,255,0.75);
#     font-weight: bold;
#     font-size: 12px;
#   }
# "))

# title <- tags$div(
#   tag.map.title, HTML("Ruolo della fauna selvatica nella diffusione e mantenimento 
#   dell'antibiotico-resistenza (PRC2016020-IZSLER)-risultati preliminari")
# )  


###############PROFILI DI RESISTENZA 
# AMR %>%
#   select(Specieagg,identificazione, 13,14,16,17,20:22) %>% 
#   pivot_longer(cols=3:9, names_to = "antibiotico") %>% 
#   group_by(antibiotico,value) %>% 
#   drop_na(value) %>% 
#   tally() %>% 
#   pivot_wider(names_from = value,values_from = n) %>% 
#   mutate("%Res"=round(100*(R/(R+S)),2)) %>% 
#   arrange(desc(`%Res`)) %>% 
#   kable("latex") %>% 
#   kable_styling()

########tabella profili di resistenza ceppo/antibiotico 
# tot<-AMR %>%
#   select(identificazione, 13,14,16,17,20:22) %>% 
#   pivot_longer(cols=2:8, names_to = "antibiotico") %>% 
#   group_by(identificazione,antibiotico) %>% 
#   tally(name="tot")
# 
# res<-AMR %>% 
#    select(identificazione, 13,14,16,17,20:22) %>% 
#    pivot_longer(cols=2:8, names_to = "antibiotico") %>% 
#    group_by(identificazione, antibiotico) %>% 
#    filter(value=='R') %>% 
#    tally(name="res") 
# 
# tot %>% full_join(res)%>% 
#     replace_na(list(res=0)) %>% 
#   filter(identificazione!="Non identificabile") %>% 
#     mutate("%R"=(res/tot)*100) %>% 
#     select(-res) %>% 
#   pivot_wider(names_from = antibiotico, values_from = `%R`) %>% 
# kable("latex", digits = 2) %>% 
#   kable_styling()
#   





# 
# x<-AMR %>%
#   select(identificazione,22) %>% 
#   filter(identificazione!="Non identificabile") %>% 
#   group_by(identificazione, ampicillina) %>%
#   drop_na(ampicillina) %>% 
#   tally() %>% 
#   drop_na(n) %>% 
#   pivot_wider(names_from = ampicillina,values_from = n,
#               values_fill = list(n = 0)) %>%
#   mutate("%Res"=round(100*(R/(R+S)),2)) %>% 
#   arrange(desc(`%Res`)) %>% 
#   select(-R, -S) %>% 
#   pivot_wider(names_from = identificazione, values_from = `%Res`,values_fill = list(`%Res` = 0)) %>% 
#   kable( ) %>% 
#   kable_styling()
#   
#   
#   
#   
#   pivot_longer(cols=3:9, names_to = "antibiotico") %>% 
#   group_by(antibiotico,value) %>% 
#   drop_na(value) %>% 
#   tally() %>% 
#   pivot_wider(names_from = value,values_from = n)

######MULTIRESISTENZE SRMR 

# funz<-function(x){
#   
#   abs(as.numeric(as.factor(x))-2)
# }
# 
# d2<-AMR %>% 
#   dplyr::select(-15,-18,-19)
# 
# 
# 
# d2[,13:19]<-apply(d2[,13:19], 2 , funz)
# 
# 
# 
# d2<-d2 %>% 
#   mutate(MDR = rowSums(.[13:19])) 
# 
# d2$R<- ifelse(d2$MDR==0, 0, 1)
# 
# d2$MR<-ifelse(d2$MDR==0, "S", 
#               ifelse(d2$MDR>=1 & d2$MDR<3, "R", "MR"))
# 
# d2<-d2 %>% 
#   mutate(MR=factor(MR, levels=c("S","R","MR"), ordered=TRUE)) %>% 
#   mutate(Specieagg=factor(Specieagg, 
#                           levels=c("CERVIDI","sCAPRINAE","CARNIVORI","CINGHIALE",
#                                    "LEPRE","CORVIDI" ,"RAPACI","UCCELLI ACQUATICI",
#                                   "ALTRI VOLATILI")))
# 
# 
# 
# png("fig2.png", height = 550, width = 500)
# d2 %>%
#   drop_na(MR) %>% 
#   group_by(Specieagg, MR) %>% 
#   tally()%>% 
#   #mutate(prop=100*prop.table(n)) %>% 
#   arrange(desc(n)) %>% 
#   ggplot(aes(x=MR, y=n, label=n))+
#   geom_segment( aes(x=MR, xend=MR, y=0, yend=n), color="grey") +
#   geom_point( aes(x=MR, y=n, color=Specieagg), size=5 ) +
#   geom_text(color="black", size=3)+
#   coord_flip()+
#   theme_ipsum_rc(axis_title_just = "mc") +
#   theme(
#     legend.position = "none",
#     panel.border = element_blank(),
#     panel.spacing = unit(0.1, "lines"),
#     strip.text.x = element_text(size = 10, face="bold"),
#     axis.text.y=element_text(size=10),
#     axis.title.x = element_text(size = 16),
#     plot.title = element_text(color = "blue",  face = "bold"),
#     plot.caption = element_text(color = "blue", face = "italic", size=7),
#     plot.subtitle = element_text(size=10)
#     
#     
#   ) +
#   
#   labs(y="n.ceppi",x="")+
#        #title="Ruolo della fauna selvatica nella diffusione e mantenimento dell'antibiotico-resistenza (PRC2016020-IZSLER)-risultati preliminari", 
#        #subtitle = "Distribuzione di 923 ceppi di Enterobatteriacee, per specie e grado di resistenza ad un pannello di antibiotici (COLISTINA, CEFTIOFUR,KANAMICINA,ENROFLOXACIN, TETRACICLINA, AMPICILLINA)",
#        #caption="S= Suscettibile, R= Resistente fino a due antibiotici, MR=Multiresistente
#        
#        #Cervidi: Capriolo, Cervo; 
#        #sCaprine: Camoscio,Stambecco, Muflone; 
#        #U.acquatici: Cigno, Gabbiano, Cormorano, Germano; 
#       # Altri Vol: Piccione, Starna, Fagiano")+
#   ggforce::facet_col(~Specieagg,  scales="free_y", space="free")
# dev.off()
# 
# 
# 
# d2 %>%
#   drop_na(MR) %>% 
#   filter(identificazione=="E.coli") %>% 
#   group_by(Specieagg, MR) %>% 
#   tally() %>% 
#   #mutate(prop=100*prop.table(n)) %>% 
#   arrange(desc(n)) %>% 
#   ggplot(aes(x=MR, y=n, label=n))+
#   geom_segment( aes(x=MR, xend=MR, y=0, yend=n), color="grey") +
#   geom_point( aes(x=MR, y=n, color=Specieagg), size=5 ) +
#   geom_text(color="black", size=3)+
#   coord_flip()+
#   theme_ipsum_rc(axis_title_just = "mc") +
#   theme(
#     legend.position = "none",
#     panel.border = element_blank(),
#     panel.spacing = unit(0.1, "lines"),
#     strip.text.x = element_text(size = 10, face = "bold"),
#     axis.text.y=element_text(size=10),
#     axis.title.x = element_text(size = 16),
#     plot.title = element_text(color = "blue",  face = "bold"),
#     plot.caption = element_text(color = "blue", face = "italic", size=7),
#     plot.subtitle = element_text(size=10)
#     
#     
#   ) +
#   
#   labs(y="n.ceppi",x="",
#        title="Ruolo della fauna selvatica nella diffusione e mantenimento dell'antibiotico-resistenza (PRC2016020-IZSLER)-risultati preliminari", 
#        subtitle = "Distribuzione di 615 ceppi di E.coli, per specie e grado di resistenza ad un pannello di antibiotici (COLISTINA, CEFTIOFUR,KANAMICINA,ENROFLOXACIN, TETRACICLINA, AMPICILLINA)",
#        caption="S= Suscettibile, R= Resistente fino a due antibiotici, MR=Multiresistente
#        
#        Cervidi: Capriolo, Cervo; 
#        sCaprine: Camoscio,Stambecco, Muflone; 
#        U.acquatici: Cigno, Gabbiano, Cormorano, Germano; 
#        Altri Vol: Piccione, Starna, Fagiano")+
#   ggforce::facet_col(~Specieagg,  scales="free_y", space="free")
# 



# mr<-d%>%
#   select(Specieagg, MR) %>% 
#   group_by(Specieagg, MR) %>% 
#   drop_na(MR) %>% 
#   tally() %>% 
# pivot_wider(names_from = MR, values_from = n) %>% 
#   mutate(N=S+R+MR) %>% 
#   data.frame()# %>% 
  #pivot_longer(cols = 2:4, names_to = "gruppo")
# mod<-brm(data = mr, family = binomial,
#     value | trials(N) ~ 1 + gruppo ,
#     prior = c(prior(normal(0, 10), class = Intercept),
#               prior(normal(0, 10), class = b)),
#     iter = 2500, warmup = 500, cores = 2, chains = 2,
#     seed = 10) 



# s<-32
# n<-363
# 
#   shape1<-s+1
#   shape2<-n-(s+1)
#   x <- seq(0,1,length=500)
#   #xp<-seq(from=0, to=1, by=0.01)
#   beta=dbeta(x,shape1 = shape1, shape2 = shape2)
#   df <- data.frame(x,beta)
#   ggplot(df, aes(x=x, y=beta))+
#     geom_line(col="blue")#+#theme_bw(16, "serif")+
#     #scale_y_continuous(expand=c(0, 0))+
#    # labs(x="Probability", y="Density",
#    #      title=paste("Beta distribution (n=",input$n2,";","s=", input$s2,")"))+
#    # theme(plot.title = element_text(size = rel(1), vjust = 1.5))
# options(digits=2)
# Shpd <- binom.bayes(
#   x = mr$S, n = mr$N, type = "highest", conf.level = 0.95, tol = 1e-9)
# Shpd<- cbind("Specieagg"=mr[, 1], Shpd[,6:8], rep("S",9))
# names(Shpd)[2:5]<-c("m", "low", "hig", "gruppo")

# x<-binom.bayes.densityplot(Shpd, 
# fill.central = "steelblue",
# fill.lower = "lightgray",
# fill.upper = "lightgray")+
# theme_ipsum_rc(axis_title_just = "mc")

# Rhpd <- binom.bayes(
#   x = mr$R, n = mr$N, type = "highest", conf.level = 0.95, tol = 1e-9)
# Rhpd<- cbind("Specieagg"=mr[, 1], Rhpd[,6:8], rep("R",9))
# names(Rhpd)[2:5]<-c("m", "low", "hig", "gruppo")
# 
# mRhpd <- binom.bayes(
#   x = mr$MR, n = mr$N, type = "highest", conf.level = 0.95, tol = 1e-9)
# mRhpd<- cbind("Specieagg"=mr[, 1], mRhpd[,6:8], rep("MR",9))
# names(mRhpd)[2:5]<-c("m", "low", "hig", "gruppo")
# 
# MR<-rbind(Shpd, Rhpd, mRhpd)
# 
# MR %>% 
#   pivot_longer(cols=2:4, names_to = "par") #%>% 
  
# MR %>% 
#  ggplot(aes(x=m, y=Specieagg))+geom_point()+
#   geom_segment(aes(x=low, 
#                    xend=hig, 
#                    y=Specieagg,
#                    yend=Specieagg))+
#  facet_wrap(~gruppo)


# MR %>% 
#   #arrange(m) %>% 
# ggplot( aes(y=m,ymin=low, ymax=hig, x=Specieagg))+
#   geom_point(color="blue", size=2)+geom_linerange(color="blue", size=.8)+
#   facet_wrap(~gruppo)+coord_flip()+
#   theme_ipsum_rc(axis_title_just = "mc")+
#   labs(x="", y="Prevalenza")


 
####CORRESPONDENCE ANALYSIS #
 
# mr<-mr %>% 
#   column_to_rownames(var="Specieagg") %>% 
#   select(-4)
# 
#   
# dt <- as.table(as.matrix(mr[-4]))
# res.ca <- CA(mr, graph = FALSE)
# fviz_ca_biplot(res.ca, repel = TRUE)


# balloonplot(t(dt), main =" ", xlab ="", ylab="",
#             label = FALSE, show.margins = FALSE)
# fviz_screeplot(res.ca, addlabels = TRUE, ylim = c(0, 50))
# fviz_screeplot(res.ca) +
#   geom_hline(yintercept=33.33, linetype=2, color="red")
# 
# fviz_ca_biplot(res.ca, 
#                map ="rowprincipal", arrow = c(TRUE, TRUE),
#                repel = TRUE)
# fviz_ca_biplot(res.ca, map ="colgreen", arrow = c(TRUE, FALSE),
#                repel = TRUE)
 
 


#######PROFILI MULTIRESISTENZA 

# amr <- d
# amr<-amr %>% 
#   dplyr::select(-c(15,18:19))
# amr$colistina<-ifelse(amr$colistina=='R', 'COL',0)
# amr$ceftiofur<-ifelse(amr$ceftiofur=='R', 'CFT',0)
# #amr$tilmicosina<-ifelse(amr$tilmicosina=='R', 'TIL',0)
# amr$kanamicina<-ifelse(amr$kanamicina=='R', 'KAN',0)
# amr$enrofloxacin<-ifelse(amr$enrofloxacin=='R', 'ENR',0)
# #amr$oxacillina<-ifelse(amr$oxacillina=='R', 'OXA',0)
# #amr$eritromicina<-ifelse(amr$eritromicina=='R', 'ERT',0)
# amr$gentamicina<-ifelse(amr$gentamicina=='R', 'GEN',0)
# amr$tetraciclina<-ifelse(amr$tetraciclina=='R', 'TET',0)
# amr$ampicillina<-ifelse(amr$ampicillina=='R', 'AMP',0)
# 
# #write.table(amr, file="amrxx.csv")
# 
# amr[,13:19]<-amr[,13:19] != 0
# nomi_abb<-toupper(abbreviate(names(amr)[13:19]))
# X<-  apply(amr[, 13:19], 1, function(x) nomi_abb[x])
# XX<-lapply(X, paste, collapse="-")
# 
# amr$profilo<-unlist(XX)
# 
# 
# 
# png("fig3.png", height = 550, width = 600)
# amr %>% 
#   filter(!profilo  %in% c("NA-NA-NA-NA-NA-NA-NA", "" )) %>% 
#   #filter(identificazione=="E.coli") %>% 
#   group_by(profilo) %>% 
#   dplyr::summarise(n=n()) %>% 
#   arrange(n) %>% 
#   top_n(10, n) %>% 
#   mutate(profilo = factor(profilo, unique(profilo))) %>% 
#   #ggplot(aes(x=profilo, y=n))+geom_bar(stat = "identity")+coord_flip()
#   ggplot(aes(x=profilo, y=n, label=n))+
#   geom_segment( aes(x=profilo, xend=profilo, y=0, yend=n), color="grey")+
#   geom_point( aes(x=profilo, y=n), size=8.4, color="lightblue" )+
#   geom_text(color="black", size=4)+
#   coord_flip()+
#   theme_ipsum_rc(axis_title_just = "mc")+
#   labs(y="n.ceppi",x="Profilo di resistenza/multiresistenza")
# dev.off()

 






###### Bayesian regression of Isolate ARindex 

##clean dataframe 

#d2<-d2 %>% 
  #dplyr::select(1,2,3,7,12,13:19,20:39)

###ARindex è una proporzione derivata da conteggi ( numero di resistenze / numero di test ab eseguiti)
# quindi possono essere analizzati come modelli di regression logistica per dati aggregati...

###aggregate bayes logistic regression brms###



# mod0 <-brm(data=d2, family = binomial,
#             res|trials(nab)~1)
# 
# mod1 <-brm(data=d2, family = binomial,res|trials(nab)~1+Specieagg)
# mod1 <-brm(data=d2, family = binomial,res|trials(nab)~)
# 
#   mod2 <-brm(data=d2, family = binomial,res|trials(nab)~Specieagg+urb+hapasc)
# 
#   pp<-brms::pp_check(mod0)
# 
# ###betaregression 
# 
# beta0<-brm(data=d2, family=zero_one_inflated_beta(),
#           ARi~1)
#   
# beta1<-brm(data=d2, family=zero_one_inflated_beta(),
#           ARi~Specieagg)
# 
# beta2<-brm(data=d2, family=zero_one_inflated_beta(),
#           ARi~Specieagg+urb)
# 
# pp<-brms::pp_check(mod0)
# 
# library(GGally)
# ggpairs(d2[,13:30])


