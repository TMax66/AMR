#preparo un set di dati per singolo campione di feci ( e quindi di specie di fauna da 
#cui proviene classificandolo con due nuove colonne R se il campione ha almeno un ceppo di 
#enterobatteriacee resistente ad almeno 1 ab e la colonna MR se il campione ha almeno un
#ceppo di enterob resistente a 3 o più ab del panel....)
# parto dal dataset AMR 911 righe... e filtro i ceppi che non sono risultati identificabili...

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
#Ora seleziono prendo il dataset d, cioè il dataset con le informazioni per singolo comune e lo 
# linko al precedente usando Idcamp come chiave

ComAMRsel<-AMR_istat %>% 
  filter(x=="TRUE") %>% 
  select(1, 2, 6, 7, 25:41) %>% 
  left_join(mPsel, by="IDcamp") %>% 
  mutate(Res=ifelse(Res==0,"S","R"), 
         MRes=ifelse(MRes==0,"No", "Si"))

