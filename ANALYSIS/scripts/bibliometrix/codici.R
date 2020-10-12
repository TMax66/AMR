library(bibliometrix)
library(tidyverse)
library(Matrix)
library(stringr)
library(igraph)
library(FactoMineR)
library(factoextra)
library(topicmodels)
library(tidytext)
library(tm)
library(ggthemes)
library(DT)
library(knitr)
library(kableExtra)

files<-c("amr1.bib","amr2.bib")
amr<- convert2df(files, dbsource = "wos", format = "bibtex")
results <-biblioAnalysis(amr, sep = ";")
S<-summary(results, k = 10, pause = FALSE)

amr<-metaTagExtraction(amr, Field = "CR_AU", sep = ";", aff.disamb = TRUE)


s<-as.data.frame(S[["AnnualProduction"]])
names(s)<-c("Anno", "Articoli")

library(hrbrthemes)
s %>% 
  filter(Anno!="2020") %>% 
ggplot( aes(x=as.numeric(as.character(Anno)), y=Articoli,group=1))+geom_point()+geom_line()+
  geom_smooth()+labs(y="N.articoli", x="Anno di pubblicazione")+
  geom_label(
    label="Tasso di crescita annuo % = 6.27", 
    y=75,
    x=13,
    # label.padding = unit(0.55, "lines"), # Rectangle size around label
    # label.size = 0.35,
    # color = "black",
    # fill="white"
  )+theme_ipsum_rc()

  ###Country del primo Autore####
CO<-as.data.frame(table(results[["CO"]]))

CO<-as.data.frame(table(results[[""]]))


CO %>% 
  arrange(Freq) %>% 
  top_n(10) %>% 
  mutate(Stato = factor(Var1, unique(Var1))) %>% 
  ggplot(aes(x=Stato, y=Freq))+
  geom_segment( aes(x=Stato, xend=Stato, y=0, yend=Freq), color="black")+  
  geom_point( aes(x=Stato, y=Freq),shape=21,color="darkblue" )+
  coord_flip()

####Journal####
source<-as.data.frame(results[["Sources"]])
source %>% 
  arrange(Freq) %>% 
  top_n(10) %>% 
  mutate(Journal = factor(SO, unique(SO))) %>% 
  ggplot(aes(x=Journal, y=Freq))+
  geom_bar(stat = "identity", fill="steelblue")+
  geom_text(aes(label=Freq), size=5,   hjust = -0.5)+
  labs(x="N.articoli pubblicati")+
  coord_flip()


####trend numero pubblicazioni per juournal top 10###
amr %>% 
  select(SO, PY) %>% 
  filter(SO %in% c("APPLIED AND ENVIRONMENTAL MICROBIOLOGY","JOURNAL OF WILDLIFE DISEASES",
                   "FRONTIERS IN MICROBIOLOGY","PLOS ONE","ZOONOSES AND PUBLIC HEALTH",
                   "VETERINARY MICROBIOLOGY","SCIENCE OF THE TOTAL ENVIRONMENT","MICROBIAL DRUG RESISTANCE",
                   "AVIAN PATHOLOGY","JOURNAL OF APPLIED MICROBIOLOGY" )) %>% 
  #filter(PY<2020) %>% 
  group_by(SO, PY) %>% 
  summarise(freq=n()) %>% 
  mutate(Journal=factor(SO, levels=c("APPLIED AND ENVIRONMENTAL MICROBIOLOGY","JOURNAL OF WILDLIFE DISEASES",
                                     "FRONTIERS IN MICROBIOLOGY","PLOS ONE","ZOONOSES AND PUBLIC HEALTH",
                                     "VETERINARY MICROBIOLOGY","SCIENCE OF THE TOTAL ENVIRONMENT","MICROBIAL DRUG RESISTANCE",
                                     "AVIAN PATHOLOGY","JOURNAL OF APPLIED MICROBIOLOGY" 
                                     ))) %>% 
  #arrange(desc(PY, freq)) %>% 
  
  #mutate(SO = factor(SO, unique(SO))) %>% 
  ggplot(aes(x = PY, y = freq)) + 
  geom_smooth(se = FALSE)+
  geom_line(alpha=0.2)+geom_point(alpha=0.2)+
  facet_wrap(facets = vars(Journal))+
  labs(x="Anno di pubblicazione", y="N.articoli pubblicati")
  


amr %>% 
  select("wos.categ"=SC) %>% 
  group_by(wos.categ) %>% 
  summarise(freq=n()) %>% 
  top_n(10) %>% 
  arrange(freq) %>% 
  mutate(wos.categ = factor(wos.categ, unique(wos.categ))) %>% 
  ggplot(aes(x=wos.categ, y=freq))+
  geom_bar(stat = "identity", fill="steelblue")+
  geom_text(aes(label=freq), size=5,   hjust = -0.5)+
  labs(x="N.articoli pubblicati")+
  coord_flip()
  


#######NETWORK ANALYSIS co-citation#####
NetMatrix <- biblioNetwork(amr, analysis = "collaboration", network = "countries", sep = ";")

net=networkPlot(NetMatrix, n = 50, 
                Title = "Co-Citation Network", type = "fruchterman", 
                size.cex=TRUE, size=20, remove.multiple=FALSE, 
                labelsize=0.7,edgesize =50,edges.min=0.3)
netStat<-networkStat(NetMatrix)
summary(netStat, k=10)


######NETWORK ANALYSIS of KEY WORDS####
NetMatrix <- biblioNetwork(amr, analysis = "co-occurrences", network = "author_keywords", sep = ";")

net=networkPlot(NetMatrix, n = 50, Title = "Keyword Co-occurrences", type = "kamada", size=T)

# histResults <- histNetwork(amr, min.citations=quantile(amr$TC,0.75), sep = ";")
# 
# options(width = 130)
# net <- histPlot(histResults, n=10, size = 5, labelsize = 3)




#####CITATION ANALYSIS#####
cit<-citations(amr, field = "article", sep = ";")

cit$Cited
y=cit$Year
s=cit$Source
c=cit$Cited

CIT<-data.frame(y,s,c)

CIT %>% 
  group_by(CR) %>% 
  summarise(cit=sum(Freq))


##########TEXTMINING ABSTRACTS###########

abstract<-amr %>% 
  select(AB, PY)

