##i dataset utilizzati sono: 
# AMR <-fonte originale dei dati imputati dal borsista (978 records)
# AMR_com origina da AMR dopo inputazione del nominativo dei comuni mancanti in AMR (911 records)
# AMR_istat origina da AMR_com dopo join con i dati dei censimenti istat (878 records)

###Dataset AMR/AMR_com/AMR_istat####
AMR<- read_excel("AMR2x.xlsx", 
                  sheet = "AMR")
AMR <- AMR %>% filter(IDcamp!= "596")
AMR<-AMR %>%

mutate(Specieagg=factor(Specieagg,
                          levels=c("CERVIDI","sCAPRINAE","CARNIVORI","CINGHIALE",
                                   "LEPRE","CORVIDI" ,"RAPACI","UCCELLI ACQUATICI",
                                   "ALTRI VOLATILI"))) %>% 
mutate(Specieagg=recode(Specieagg,"sCAPRINAE"="BOVIDI", "CINGHIALE"="SUIDI"))


names(AMR)[c(13:22)]<-c("COL", "CFT", "til","KAN",
                     "ENR", "ox", "er", "GEN", "TET", "AMP")

AMR$x<-!duplicated(AMR$IDcamp)
AMR$na <- ifelse(is.na(AMR$identificazione ),  "cancellare",
                 ifelse(AMR$identificazione== "Non identificabile", "cancellare", "tenere"))




AMR_com <- read_excel("~/Library/Mobile Documents/com~apple~CloudDocs/gitProject/AMR/ANALYSIS/dati/AMR_dati completi dei comuni.xlsx")#carica il dataset finale

AMR_com <- AMR_com %>% filter(IDcamp!= "596")



AMR_com<-AMR_com %>% 
  mutate(Specieagg=factor(Specieagg,
                        levels=c("CERVIDI","sCAPRINAE","CARNIVORI","CINGHIALE",
                                 "LEPRE","CORVIDI" ,"RAPACI","UCCELLI ACQUATICI",
                                 "ALTRI VOLATILI"))) %>% 
  mutate(Specieagg=recode(Specieagg,"sCAPRINAE"="BOVIDI", "CINGHIALE"="SUIDI"))
AMR_com$x<-!duplicated(AMR_com$IDcamp)
AMR_com <-AMR_com %>%
  filter(identificazione!="Non identificabile")
classcom <- read_excel("dati/classcom.xlsx")
classcom$comune<-casefold(classcom$comune, upper=TRUE)
class2<-subset(classcom, classcom$comune%in%AMR_com$comune)
d<-merge(AMR_com, class2, by.x = "comune")
pascolo <- read_excel("dati/pascolo.xlsx")
pascolo$comune<-casefold(pascolo$comune, upper=TRUE)
pascolo2<-subset(pascolo, pascolo$comune%in%d$comune)
d<-merge(d, pascolo2, by.x = "comune", all.x = TRUE)
alt <- read_excel("dati/alt.xlsx")
alt$comune<-casefold(alt$comune, upper=TRUE)
alt2<-subset(alt, alt$comune%in%d$comune)
d<-merge(d, alt2, by.x = "comune", all.x = TRUE)
denspop <- read_excel("dati/denspop.xlsx")
denspop$comune<-casefold(denspop$comune, upper=TRUE)
dens2<-subset(denspop, denspop$comune%in%d$comune)
AMR_istat<-merge(d, dens2, by.x = "comune", all.x = TRUE)
names(AMR_istat)[c(13:22)]<-c("COL", "CFT", "til","KAN",
                        "ENR", "ox", "er", "GEN", "TET", "AMP")

#Dataset prevalenze R e MR e caratterizzazione territoriale dei comuni####
mPsel<-AMR %>% 
  filter(identificazione!="Non identificabile") %>% 
  dplyr::select(-15,-18,-19)
mPsel[,13:19]<-apply(mPsel[,13:19], 2 , funz)
mPsel<-mPsel %>% 
  mutate(MDR = rowSums(.[13:19]))###antibiogram length###
mPsel$MR<- ifelse(mPsel$MDR==0 |mPsel$MDR<=2, 0, 1) 
mPsel$R<- ifelse(mPsel$MDR==0, 0, 1)
mPsel<-mPsel %>% 
  drop_na(R) %>% 
  drop_na(MR) %>% 
  group_by(IDcamp)%>% 
  summarise(Res=sum(R), 
            MRes=sum(MR)) 
ComAMRsel<-AMR_istat %>% 
  filter(x=="TRUE") %>% 
  select(1, 2, 6, 7, 25:41) %>% 
  left_join(mPsel, by="IDcamp") %>% 
  mutate(Res=ifelse(Res==0,"S","R"), 
         MRes=ifelse(MRes==0,"No", "Si"))

#Dataset con i profili di resistenza####
amr <- AMR %>% 
  drop_na(Specieagg) %>% 
  filter(identificazione!="Non identificabile") %>% 
  dplyr::select(-15,-18,-19) 
amr$COL<-ifelse(amr$COL=='R', 'COL',0)
amr$CFT<-ifelse(amr$CFT=='R', 'CFT',0)
amr$KAN<-ifelse(amr$KAN=='R', 'KAN',0)
amr$ENR<-ifelse(amr$ENR=='R', 'ENR',0)
amr$GEN<-ifelse(amr$GEN=='R', 'GEN',0)
amr$TET<-ifelse(amr$TET=='R', 'TET',0)
amr$AMP<-ifelse(amr$AMP=='R', 'AMP',0)
 amr[,13:19]<-amr[,13:19] != 0
nomi_abb<-toupper(abbreviate(names(amr)[13:19]))
X<-  apply(amr[, 13:19], 1, function(x) nomi_abb[x])
XX<-lapply(X, paste, collapse="-")
amr$profilo<-unlist(XX)
amr<-amr %>% 
  filter(!profilo  %in% c("NA-NA-NA-NA-NA-NA-NA")) %>% 
  mutate( profilo= ifelse(profilo=="", "SUSC", profilo))

rm(X,XX)


rm(alt, alt2,classcom,class2, dens2, denspop, pascolo, pascolo2)







