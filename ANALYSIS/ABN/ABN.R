library(abn)
#library(rstan)
library(Rgraphviz)
library(tidyverse)
#library(googlesheets4)
library(binom)
#library(googledrive)
library(readxl)

df<-AMR <- read_excel("~/gitProgetti/AMR-PRC2016020/ANALYSIS/AMR2.xlsx", 
                   sheet = "AMR")
rm(AMR)



#### vengono escluse le righe relative ai ceppi a cui non si è giunti ad una identificazione 
### di genere. 
df<-df %>% 
  filter(!is.na(identificazione)) %>% 
  select(Specieagg,identificazione, c(13,14,16,17,20:22)) 

df<-na.omit(df)
df<-df %>% 
  filter(identificazione=="E.coli") %>% 
  select(-identificazione)%>% 
  data.frame()

df<-as.data.frame(lapply (df, factor))
df<-as.data.frame(model.matrix(~ . + 0, data=df[, c(1:8)], contrasts.arg = lapply(df[, c(1:8)], contrasts, contrasts=FALSE)))

names(df)[1:9]<-c("Altrivol","Carnivori","Cervidi","Cinghiale",
                  "Corvidi","Lepre","Rapaci","sCaprinae","uccAcqu")

df<-as.data.frame(lapply (df, factor))

df<-df %>% 
  select(1:9, colistinaR,ceftiofurR,kanamicinaR,enrofloxacinR,gentamicinaR,tetraciclinaR,ampicillinaR)


write.table(df, file="dati.csv")


ban<-matrix(rep(0, dim(df)[2]^2), ncol=dim(df)[2])
colnames(ban)<-rownames(ban)<-names(df)

ban[1,2]<-1
ban[2,1]<-1
ban[1,3]<-1
ban[3,1]<-1
ban[1,4]<-1
ban[4,1]<-1
ban[1,5]<-1
ban[5,1]<-1
ban[1,6]<-1
ban[6,1]<-1
ban[1,7]<-1
ban[7,1]<-1
ban[1,8]<-1
ban[8,1]<-1
ban[1,9]<-1
ban[9,1]<-1

ban[2,3]<-1
ban[3,2]<-1
ban[2,4]<-1
ban[4,2]<-1
ban[2,5]<-1
ban[5,2]<-1
ban[2,6]<-1
ban[6,2]<-1
ban[2,7]<-1
ban[7,2]<-1
ban[2,8]<-1
ban[8,2]<-1
ban[2,9]<-1
ban[9,2]<-1

ban[3,4]<-1
ban[4,3]<-1
ban[3,5]<-1
ban[5,3]<-1
ban[3,6]<-1
ban[6,3]<-1
ban[3,7]<-1
ban[7,3]<-1
ban[3,8]<-1
ban[8,3]<-1
ban[3,9]<-1
ban[9,3]<-1

ban[4,5]<-1
ban[5,4]<-1
ban[4,6]<-1
ban[6,4]<-1
ban[4,7]<-1
ban[7,4]<-1
ban[4,8]<-1
ban[8,4]<-1
ban[4,9]<-1
ban[9,4]<-1

ban[5,6]<-1
ban[6,5]<-1
ban[5,7]<-1
ban[7,5]<-1
ban[5,8]<-1
ban[8,5]<-1
ban[5,9]<-1
ban[9,5]<-1

ban[6,7]<-1
ban[7,6]<-1
ban[6,8]<-1
ban[8,6]<-1
ban[6,9]<-1
ban[9,6]<-1

ban[7,8]<-1
ban[8,7]<-1
ban[7,9]<-1
ban[9,7]<-1

ban[8,9]<-1
ban[9,8]<-1

ban[1,10]<-1
ban[1,11]<-1
ban[1,12]<-1
ban[1,13]<-1
ban[1,14]<-1
ban[1,15]<-1
ban[1,16]<-1

ban[2,10]<-1
ban[2,11]<-1
ban[2,12]<-1
ban[2,13]<-1
ban[2,14]<-1
ban[2,15]<-1
ban[2,16]<-1

ban[3,10]<-1
ban[3,11]<-1
ban[3,12]<-1
ban[3,13]<-1
ban[3,14]<-1
ban[3,15]<-1
ban[3,16]<-1

ban[4,10]<-1
ban[4,11]<-1
ban[4,12]<-1
ban[4,13]<-1
ban[4,14]<-1
ban[4,15]<-1
ban[4,16]<-1

ban[5,10]<-1
ban[5,11]<-1
ban[5,12]<-1
ban[5,13]<-1
ban[5,14]<-1
ban[5,15]<-1
ban[5,16]<-1

ban[6,10]<-1
ban[6,11]<-1
ban[6,12]<-1
ban[6,13]<-1
ban[6,14]<-1
ban[6,15]<-1
ban[6,16]<-1


ban[7,10]<-1
ban[7,11]<-1
ban[7,12]<-1
ban[7,13]<-1
ban[7,14]<-1
ban[7,15]<-1
ban[7,16]<-1

ban[8,10]<-1
ban[8,11]<-1
ban[8,12]<-1
ban[8,13]<-1
ban[8,14]<-1
ban[8,15]<-1
ban[8,16]<-1

ban[9,10]<-1
ban[9,11]<-1
ban[9,12]<-1
ban[9,13]<-1
ban[9,14]<-1
ban[9,15]<-1
ban[9,16]<-1


retain<-matrix(rep(0, dim(df)[2]^2), ncol=dim(df)[2])
colnames(retain)<-rownames(retain)<-names(df)


mydists<-list(   
               Altrivol="binomial",
               Carnivori="binomial",
               Cervidi="binomial",
               Cinghiale="binomial",
               Corvidi="binomial",
               Lepre="binomial",
               Rapaci ="binomial",
               sCaprinae="binomial",
               uccAcqu   ="binomial",
               colistinaR ="binomial",
                
               ceftiofurR ="binomial",  
                
               kanamicinaR  ="binomial",
                
               enrofloxacinR="binomial",
                
               gentamicinaR ="binomial",
               
               tetraciclinaR ="binomial",
                
               ampicillinaR ="binomial"
                )
####identifico il numero di parents che minimizza la loglikelihood#####
#max.par<-... #inserire il numero di max.parents reiterare da 1 a 8 e fare grafico

#mycache<-buildscorecache(data.df = df,data.dists = mydists,
                         #dag.banned = ban,dag.retained = retain, max.parents = max.par)

#mp.dag <- mostprobable( score.cache=mycache)

#fabn <- fitabn( dag.m=mp.dag,data.df=mydat,
                #data.dists=mydists, create.graph = TRUE)

#p1<-fabn$mlik
#p2<-fabn$mlik
#p3<-fabn$mlik
#p4<-fabn$mlik
#p5<-fabn$mlik
#p6<-fabn$mlik
#p7<-fabn$mlik

#mlik<-data.frame(p1,p2,p3,p4,p5, p6, p7)
#mlik %>% 
 # gather(key="Parents", value=mlik, p1,p2,p3,p4,p5,p6,p7) %>% 
  #ggplot(aes(x=Parents, y=mlik, group = 1))+geom_point()+geom_line()
#########################################################################
#####scelgo il dag con max.par=4###################################

max.par<-4

mycache<-buildscorecache(data.df = df,data.dists = mydists,
                         dag.banned = ban,dag.retained = retain, max.parents = max.par)

mp.dag <- mostprobable( score.cache=mycache)

fabn <- fitabn( dag.m=mp.dag,data.df=mydat,
                data.dists=mydists, create.graph = TRUE)


tographviz(dag.m=mp.dag$dag, data.df= df, data.dists=mydists,
           outfile="graph.dot", directed=FALSE)

###su Linux##
system("dot -Tpdf -o graph.pdf graph.dot")
system("evince graph.pdf")
####

marg.f <- fitabn( dag.m=mp.dag,data.df=mydat,data.dists=mydists,
                  compute.fixed=TRUE,n.grid=1000)



marnew <- marg.f$marginals[[1]]
for( i in 2: length(marg.f$marginals)){
  marnew <- c(marnew, marg.f$marginals[[i]])}

print( names(marnew))

m <- marnew

Av.p <- cbind( m[["Altrivol|(Intercept)"]])
Cr.p <- cbind( m[["Carnivori|(Intercept)"]])
Cv.p <- cbind( m[["Cervidi|(Intercept)"]])
Cn.p <- cbind( m[["Cinghiale|(Intercept)"]])
Co.p <- cbind( m[["Corvidi|(Intercept)"]]) 
Le.p <- cbind( m[["Lepre|(Intercept)"]])
Ra.p <- cbind( m[["Rapaci|(Intercept)"]])
sCa.p<- cbind( m[["sCaprinae|(Intercept)"]])
uAq.p<- cbind( m[["uccAcqu|(Intercept)"]])

COL.p <- cbind(m[["colistinaR|(Intercept)"]],
               m[["colistinaR|tetraciclinaR"]],
               m[["colistinaR|ampicillinaR"]],
               m[["colistinaR|Cinghiale"]],
               m[["colistinaR|sCaprinae"]])


CFT.p <- cbind(m[["ceftiofurR|(Intercept)"]],
               m[["ceftiofurR|Rapaci"]],
               m[["ceftiofurR|tetraciclinaR"]],
               m[["ceftiofurR|uccAcqu"]],
               m[["ceftiofurR|kanamicinaR"]])

KAN.p <- cbind(m[[ "kanamicinaR|(Intercept)"]],
               m[["kanamicinaR|gentamicinaR"]])


ENR.p <- cbind(m[["enrofloxacinR|(Intercept)"]],
               m[["enrofloxacinR|Corvidi"]],
               m[["enrofloxacinR|Rapaci"]],
               m[["enrofloxacinR|kanamicinaR"]],
               m[["enrofloxacinR|ampicillinaR"]])


GEN.p <- cbind(m[["gentamicinaR|(Intercept)"]])


TET.p <- cbind(m[["tetraciclinaR|(Intercept)"]],
               m[["tetraciclinaR|kanamicinaR"]])

AMP.p <- cbind(m[["ampicillinaR|(Intercept)" ]],
               m[["ampicillinaR|Cervidi"]],
               m[["ampicillinaR|ceftiofurR"]],
               m[["ampicillinaR|tetraciclinaR"]])


dump( c( "Av.p", "Cr.p", "Cv.p", "Cn.p", "Co.p","Le.p", "Ra.p","sCa.p","uAq.p",
         "COL.p", "CFT.p", "KAN.p", "ENR.p", "GEN.p", "TET.p", "AMP.p"),
      file="amr_post_params.R")

system("jags jags_amr_script.R")


#######################codice per eseguire un solo bootstrap####
orig.data<-df

#names(orig.data) <-names(boot.data)

library( coda)
boot.data <- read.coda("out1chain1.txt","out1index.txt")
boot.data <- as.data.frame(boot.data)
names(orig.data) <-names(boot.data)
for( j in 1:dim(orig.data)[2]){if(is.factor(orig.data[,j]))
{ boot.data[,j]<- as.factor(boot.data[,j])
levels(boot.data[,j])<- levels(orig.data[,j])}}

ban <- matrix( rep(0,dim(orig.data)[2]^2),
               ncol=dim(orig.data)[2])

ban[1,2]<-1
ban[2,1]<-1
ban[1,3]<-1
ban[3,1]<-1
ban[1,4]<-1
ban[4,1]<-1
ban[1,5]<-1
ban[5,1]<-1
ban[1,6]<-1
ban[6,1]<-1
ban[1,7]<-1
ban[7,1]<-1
ban[1,8]<-1
ban[8,1]<-1
ban[1,9]<-1
ban[9,1]<-1

ban[2,3]<-1
ban[3,2]<-1
ban[2,4]<-1
ban[4,2]<-1
ban[2,5]<-1
ban[5,2]<-1
ban[2,6]<-1
ban[6,2]<-1
ban[2,7]<-1
ban[7,2]<-1
ban[2,8]<-1
ban[8,2]<-1
ban[2,9]<-1
ban[9,2]<-1

ban[3,4]<-1
ban[4,3]<-1
ban[3,5]<-1
ban[5,3]<-1
ban[3,6]<-1
ban[6,3]<-1
ban[3,7]<-1
ban[7,3]<-1
ban[3,8]<-1
ban[8,3]<-1
ban[3,9]<-1
ban[9,3]<-1

ban[4,5]<-1
ban[5,4]<-1
ban[4,6]<-1
ban[6,4]<-1
ban[4,7]<-1
ban[7,4]<-1
ban[4,8]<-1
ban[8,4]<-1
ban[4,9]<-1
ban[9,4]<-1

ban[5,6]<-1
ban[6,5]<-1
ban[5,7]<-1
ban[7,5]<-1
ban[5,8]<-1
ban[8,5]<-1
ban[5,9]<-1
ban[9,5]<-1

ban[6,7]<-1
ban[7,6]<-1
ban[6,8]<-1
ban[8,6]<-1
ban[6,9]<-1
ban[9,6]<-1

ban[7,8]<-1
ban[8,7]<-1
ban[7,9]<-1
ban[9,7]<-1

ban[8,9]<-1
ban[9,8]<-1

ban[1,10]<-1
ban[1,11]<-1
ban[1,12]<-1
ban[1,13]<-1
ban[1,14]<-1
ban[1,15]<-1
ban[1,16]<-1

ban[2,10]<-1
ban[2,11]<-1
ban[2,12]<-1
ban[2,13]<-1
ban[2,14]<-1
ban[2,15]<-1
ban[2,16]<-1

ban[3,10]<-1
ban[3,11]<-1
ban[3,12]<-1
ban[3,13]<-1
ban[3,14]<-1
ban[3,15]<-1
ban[3,16]<-1

ban[4,10]<-1
ban[4,11]<-1
ban[4,12]<-1
ban[4,13]<-1
ban[4,14]<-1
ban[4,15]<-1
ban[4,16]<-1

ban[5,10]<-1
ban[5,11]<-1
ban[5,12]<-1
ban[5,13]<-1
ban[5,14]<-1
ban[5,15]<-1
ban[5,16]<-1

ban[6,10]<-1
ban[6,11]<-1
ban[6,12]<-1
ban[6,13]<-1
ban[6,14]<-1
ban[6,15]<-1
ban[6,16]<-1


ban[7,10]<-1
ban[7,11]<-1
ban[7,12]<-1
ban[7,13]<-1
ban[7,14]<-1
ban[7,15]<-1
ban[7,16]<-1

ban[8,10]<-1
ban[8,11]<-1
ban[8,12]<-1
ban[8,13]<-1
ban[8,14]<-1
ban[8,15]<-1
ban[8,16]<-1

ban[9,10]<-1
ban[9,11]<-1
ban[9,12]<-1
ban[9,13]<-1
ban[9,14]<-1
ban[9,15]<-1
ban[9,16]<-1

colnames( ban) <- rownames(ban) <- names(orig.data)



retain <- matrix( rep(0,dim(orig.data)[2]^2),
                  ncol=dim(orig.data)[2])
colnames( retain) <- rownames(retain) <- names(orig.data)


mydists<-list(   
  Av="binomial", Cr="binomial",Cv="binomial",
  Cn="binomial",Co="binomial",Le="binomial",
  Ra ="binomial",sCa="binomial",uAq ="binomial",
  COL ="binomial",TET ="binomial",CFT ="binomial",
  AMP ="binomial", KAN  ="binomial",
  GEN ="binomial", ENR="binomial"
)

max.par <- 4

boot1.cache <- buildscorecache( data.df=boot.data,
                                data.dists=mydists, max.parents=max.par,
                                dag.banned=ban, dag.retained=retain)

boot1.mp <- mostprobable( score.cache=boot1.cache)

###############################################################################

############codice per eseguire 5 bootstrap#####


index<-1
orig.data<-df
max.par<-4;#parent limit for original data
start<-seq(1,1000,by=5);
stop<-seq(5,1000,by=5);


ban <- matrix( rep(0,dim(orig.data)[2]^2),
               ncol=dim(orig.data)[2])

ban[1,2]<-1
ban[2,1]<-1
ban[1,3]<-1
ban[3,1]<-1
ban[1,4]<-1
ban[4,1]<-1
ban[1,5]<-1
ban[5,1]<-1
ban[1,6]<-1
ban[6,1]<-1
ban[1,7]<-1
ban[7,1]<-1
ban[1,8]<-1
ban[8,1]<-1
ban[1,9]<-1
ban[9,1]<-1

ban[2,3]<-1
ban[3,2]<-1
ban[2,4]<-1
ban[4,2]<-1
ban[2,5]<-1
ban[5,2]<-1
ban[2,6]<-1
ban[6,2]<-1
ban[2,7]<-1
ban[7,2]<-1
ban[2,8]<-1
ban[8,2]<-1
ban[2,9]<-1
ban[9,2]<-1

ban[3,4]<-1
ban[4,3]<-1
ban[3,5]<-1
ban[5,3]<-1
ban[3,6]<-1
ban[6,3]<-1
ban[3,7]<-1
ban[7,3]<-1
ban[3,8]<-1
ban[8,3]<-1
ban[3,9]<-1
ban[9,3]<-1

ban[4,5]<-1
ban[5,4]<-1
ban[4,6]<-1
ban[6,4]<-1
ban[4,7]<-1
ban[7,4]<-1
ban[4,8]<-1
ban[8,4]<-1
ban[4,9]<-1
ban[9,4]<-1

ban[5,6]<-1
ban[6,5]<-1
ban[5,7]<-1
ban[7,5]<-1
ban[5,8]<-1
ban[8,5]<-1
ban[5,9]<-1
ban[9,5]<-1

ban[6,7]<-1
ban[7,6]<-1
ban[6,8]<-1
ban[8,6]<-1
ban[6,9]<-1
ban[9,6]<-1

ban[7,8]<-1
ban[8,7]<-1
ban[7,9]<-1
ban[9,7]<-1

ban[8,9]<-1
ban[9,8]<-1

ban[1,10]<-1
ban[1,11]<-1
ban[1,12]<-1
ban[1,13]<-1
ban[1,14]<-1
ban[1,15]<-1
ban[1,16]<-1

ban[2,10]<-1
ban[2,11]<-1
ban[2,12]<-1
ban[2,13]<-1
ban[2,14]<-1
ban[2,15]<-1
ban[2,16]<-1

ban[3,10]<-1
ban[3,11]<-1
ban[3,12]<-1
ban[3,13]<-1
ban[3,14]<-1
ban[3,15]<-1
ban[3,16]<-1

ban[4,10]<-1
ban[4,11]<-1
ban[4,12]<-1
ban[4,13]<-1
ban[4,14]<-1
ban[4,15]<-1
ban[4,16]<-1

ban[5,10]<-1
ban[5,11]<-1
ban[5,12]<-1
ban[5,13]<-1
ban[5,14]<-1
ban[5,15]<-1
ban[5,16]<-1

ban[6,10]<-1
ban[6,11]<-1
ban[6,12]<-1
ban[6,13]<-1
ban[6,14]<-1
ban[6,15]<-1
ban[6,16]<-1


ban[7,10]<-1
ban[7,11]<-1
ban[7,12]<-1
ban[7,13]<-1
ban[7,14]<-1
ban[7,15]<-1
ban[7,16]<-1

ban[8,10]<-1
ban[8,11]<-1
ban[8,12]<-1
ban[8,13]<-1
ban[8,14]<-1
ban[8,15]<-1
ban[8,16]<-1

ban[9,10]<-1
ban[9,11]<-1
ban[9,12]<-1
ban[9,13]<-1
ban[9,14]<-1
ban[9,15]<-1
ban[9,16]<-1

colnames( ban) <- rownames(ban) <- names(orig.data)



retain <- matrix( rep(0,dim(orig.data)[2]^2),
                  ncol=dim(orig.data)[2])
colnames( retain) <- rownames(retain) <- names(orig.data)


mydists<-list(   
  Av="binomial", Cr="binomial",Cv="binomial",
  Cn="binomial",Co="binomial",Le="binomial",
  Ra ="binomial",sCa="binomial",uAq ="binomial",
  COL ="binomial",TET ="binomial",CFT ="binomial",
  AMP ="binomial", KAN  ="binomial",
  GEN ="binomial", ENR="binomial"
)

dags<-list();
#########################################
for(i in start[index]:stop[index]){ #MASTER LOOP - each interation creates a bootstrap sample and finds mostprobable model
  #########################################
  #create bootstrap data
  #1. create parameter file with unique random seed
  init.file<-paste("init_",i,sep="");#tempfile(pattern=paste("_",index,"_",sep=""),tmpdir=getwd());#file to hold jags seed
  cat(paste("\".RNG.name\" <-\"base::Mersenne-Twister\"","\n",sep=""),file=init.file,append=FALSE);
  cat(paste("\".RNG.seed\" <- ",i,"\n",sep=""),file=init.file,append=TRUE);#note i is unique
  #2. create script file with unique output file name
  run.file<-paste("script_",i,sep="");#tempfile(pattern=paste("_",index,"_",sep=""),tmpdir=getwd());#file to hold jags seed
  
  #this is needed verbatim     
  cat("model in model.bug
      data in post_params.R
      compile, nchains(1)
      ",file=run.file);
  cat(paste("parameters in ",init.file,"\n",sep=""),file=run.file,append=TRUE);
  cat("initialize
      update 100000
      monitor Av, thin(10)
      monitor Cr, thin(10)
      monitor Cv, thin(10)
      monitor Cn, thin(10)
      monitor Co, thin(10)
      monitor Le, thin(10)
      monitor Ra, thin(10)
      monitor sCa, thin(10)
      monitor vAq, thin(10)
      monitor COL, thin(10)
      monitor CFT, thin(10)
      monitor KAN, thin(10)
      monitor ENR, thin(10)
      monitor GEN, thin(10)
      monitor TET, thin(10)
      monitor AMP, thin(10)
      update 6140, by(1000)
      ",file=run.file,append=TRUE);
   out.file<-paste("out",i,sep="");
   cat(paste("coda *, stem(\"",out.file,"\")\n",sep=""),file=run.file,append=TRUE);
   
   #3. run the MCMC sampler
   #system(paste("/home/vetadm/flewis/bin/jags ",run.file,sep=""));
   system(paste("jags ",run.file,sep=""));
   #4. read in mcmc data and convert to format suitable for mostprobable
   boot.data<-read.coda(paste(out.file,"chain1.txt",sep=""),paste(out.file,"index.txt",sep=""));
   boot.data<-as.data.frame(boot.data);
   for(j in 1:dim(orig.data)[2]){if(is.factor(orig.data[,j])){boot.data[,j]<-as.factor(boot.data[,j]);
   levels(boot.data[,j])<-levels(orig.data[,j]);}}
   
   #5. run the MostProb search on the bootstrap data
   boot1.cache<-buildscorecache(data.df=boot.data,data.dists=mydists, max.parents=max.par,centre=TRUE);
   dags[[i]]<-mostprobable(score.cache=boot1.cache);
   unlink(c(init.file,run.file,out.file,paste(out.file,"chain1.txt",sep=""),paste(out.file,"index.txt",sep="")));#tidy up
}

save(dags,file=paste("mp",index,".RData",sep=""));








#######esempio###############

###per far correre jags servono 4 file: 

#1) BUG model definition (*_model.bug)
#2) distribuzioni marginali ottenute con fitabn()---(*_post_params.R)
#3) script che fa correre le simulazioni MCMC (jags_*_script.R)
#4) file con  random number seed (inits.R)

#### per far correre la simulazione seguire le seguenti istruzioni:
#raccogliere i 4 file sopra in una cartella
#nella linea di comando di R digitare jags_*_script.R ( * nell'esempio sta per pig... sostituire con altro nome per i propri dati)


orig.data <- pigs.vienna[,-11]

system("jags jags_pigs_script.R")


library( coda)
boot.data <- read.coda("out1chain1.txt","out1index.txt")
boot.data <- as.data.frame(boot.data)
for( j in 1:dim(orig.data)[2]){if(is.factor(orig.data[,j]))
{ boot.data[,j]<- as.factor(boot.data[,j])
levels(boot.data[,j])<- levels(orig.data[,j])}}


ban <- matrix( rep(0,dim(orig.data)[2]^2),
               ncol=dim(orig.data)[2])
colnames( ban) <- rownames(ban) <- names(orig.data)
retain <- matrix( rep(0,dim(orig.data)[2]^2),
                  ncol=dim(orig.data)[2])
colnames( retain) <- rownames(retain) <- names(orig.data)
mydists <- list( PC="binomial", PT="binomial", MS="binomial",
                 HS="binomial", TAIL="binomial",
                 Abscess="binomial", Pyaemia="binomial",
                 EPcat="binomial", PDcat="binomial",
                 plbinary="binomial")




max.par <- 3

boot1.cache <- buildscorecache( data.df=boot.data,
                                data.dists=mydists, max.parents=max.par,
                                dag.banned=ban, dag.retained=retain)

boot1.mp <- mostprobable( score.cache=boot1.cache)

datadir <- tempdir()
save( boot1.mp,file=paste( datadir,"boot1run.RData",sep=''))

mydata <- pigs.vienna[,-11]
N <- 10240;
# Write out manually, clearer than using rep()
mydag <- matrix(c(
  #PC PT MS HS TAIL Abscess Pyaemia EPcat PDcat plbinary
  0, 1, 0, 0, 0, 0, 0, 0, 0, 0,
  0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
  0, 0, 0, 1, 0, 0, 0, 0, 0, 0,
  0, 0, 0, 0, 0, 0, 0, 1, 0, 0,
  0, 0, 0, 0, 0, 0, 0, 0, 1, 0,
  0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
  0, 0, 0, 0, 1, 0, 0, 0, 0, 0,
  0, 0, 0, 0, 0, 0, 0, 0, 0, 1,
  0, 0, 1, 0, 0, 0, 0, 1, 0, 1,
  1, 1, 0, 0, 0, 1, 0, 0, 0, 0),
  byrow=TRUE,ncol=10)
colnames(mydag) <- rownames(mydag) <- names(mydata)
sum( mydag) 

mydists <- list( PC="binomial", PT="binomial", MS="binomial",
                 HS="binomial", TAIL="binomial",
                 Abscess="binomial", Pyaemia="binomial",
                 EPcat="binomial", PDcat="binomial",
                 plbinary="binomial")
# Use fitabn to check mydag is correct (no typos mlik = -44684.64)
print( fitabn(dag.m=mydag,data.df=mydata,data.dists=mydists)$mlik)

bestdag <- mydag

boot.dags <- list()
these <- grep("mp10Kboot\\d+.RData", dir())
num <- 1system.file(’bootstrapping_example’,package=’abn’)
for( i in dir()[these]){# Load each file
  load(i) # Provides dags, a list
  tmp <- dags[which(unlist(lapply(dags,sum))>0)]
  # Get valid entries in dags but as a list
  for( j in 1:length(tmp)){
    # For each entry copy into boot.dags, and increment counter
    boot.dags[[num]]<- tmp[[j]]; num <- num+1 }
  rm( dags)
}

if( FALSE){
  scores <- rep(0,length(boot.dags))
  for(i in 1:length(boot.dags)){
    scores[i] <- fitabn(dag.m=boot.dags[[i]],data.df=mydata,
                        data.dists=mydists)$mlik
  }
  scores.b <- scores[-which(scores< -N)]
  orig.score <- fitabn(dag.m=bestdag,data.df=mydata,
                       data.dists=mydists)$mlik
  plot(density(scores.b,from=min(scores.b),to=max(scores.b)))
  abline(v=orig.score,lwd=2,col="blue")
}


boot.dags.trim <- boot.dags
for( i in 1:length(boot.dags)){
  boot.dags.trim[[i]] <- boot.dags.trim[[i]]*bestdag }
arc.freq <- lapply(boot.dags.trim,sum)
arc.freq <- table(unlist(arc.freq))
library( Cairo)
CairoPNG("PigsFreqBootRes.png",pointsize=10,width=720,height=700)
par(las=1, mar=c(6.1,6.1,4.1,2.1))
barplot( arc.freq,ylab="",xlab="",col="skyblue",
         names.arg=names(arc.freq), ylim=c(0,2500))
par( las=1)
mtext( "Number of arcs in bootstrap DAG",1,line=3,cex=1.5)
par( las=3)
mtext( "Frequency out of 10240",2,line=4,cex=1.5)
dev.off()


total.dag <- matrix(rep(0,dim(bestdag)[2]^2),ncol=dim(bestdag)[2])
colnames(total.dag) <- rownames(total.dag)<- colnames(bestdag)
# Get the support for each arc, total.dag:
for( i in 1:length(boot.dags)){
  if(sum(boot.dags[[i]])>0){total.dag <- total.dag+boot.dags[[i]]}}
total.dag <- total.dag*bestdag # We only want arcs in the best DAG
total.dag
##################################################################





































########con i dati delle specie e dell'area########################
rm(list=ls())

dati<-gs_title("AMR")
df <-gs_read(dati, ws="AMR" )
df<-df %>% 
  filter(!is.na(identificazione)) %>% 
  select(area,Specieagg,identificazione, c(13:22)) 
  
df<-na.omit(df)
df<-df %>% 
  filter(identificazione=="E.coli") %>% 
  select(-identificazione)%>% 
  data.frame()

df<-as.data.frame(lapply (df, factor))
df<-as.data.frame(model.matrix(~ . + 0, data=df[, c(1:12)], contrasts.arg = lapply(df[, c(1:12)], contrasts, contrasts=FALSE)))

names(df)[1:22]<-c("Alessandria", "Binago","Brescia","Lecco-Como",
             "Pavia", "PianuraBG", "PrealpiBG","SondrioAV",
             "SondrioMV","SondrioBV", "Borlezza","Brembana","Seriana",
             "Altrivol","Carnivori","Cervidi","Cinghiale",
             "Corvidi","Lepre","Rapaci","sCaprinae","uccAcqu")
df<-df %>% 
  select(-Alessandria, -Brescia, -`Lecco-Como`)
df<-as.data.frame(lapply (df, factor))

ban<-matrix(rep(0, dim(df)[2]^2), ncol=dim(df)[2])
colnames(ban)<-rownames(ban)<-names(df)

ban[1,2]<-1
ban[2,1]<-1
ban[1,3]<-1
ban[3,1]<-1
ban[1,4]<-1
ban[4,1]<-1
ban[1,5]<-1
ban[5,1]<-1
ban[1,6]<-1
ban[6,1]<-1
ban[1,7]<-1
ban[7,1]<-1
ban[1,8]<-1
ban[8,1]<-1
ban[1,9]<-1
ban[9,1]<-1
ban[1,10]<-1
ban[10,1]<-1

ban[2,3]<-1
ban[3,2]<-1
ban[2,4]<-1
ban[4,2]<-1
ban[2,5]<-1
ban[5,2]<-1
ban[2,6]<-1
ban[6,2]<-1
ban[2,7]<-1
ban[7,2]<-1
ban[2,8]<-1
ban[8,2]<-1
ban[2,9]<-1
ban[9,2]<-1
ban[2,10]<-1
ban[10,2]<-1

ban[3,4]<-1
ban[4,3]<-1
ban[3,5]<-1
ban[5,3]<-1
ban[3,6]<-1
ban[6,3]<-1
ban[3,7]<-1
ban[7,3]<-1
ban[3,8]<-1
ban[8,3]<-1
ban[3,9]<-1
ban[9,3]<-1
ban[3,10]<-1
ban[10,3]<-1

ban[4,5]<-1
ban[5,4]<-1
ban[4,6]<-1
ban[6,4]<-1
ban[4,7]<-1
ban[7,4]<-1
ban[4,8]<-1
ban[8,4]<-1
ban[4,9]<-1
ban[9,4]<-1
ban[4,10]<-1
ban[10,4]<-1

ban[5,6]<-1
ban[6,5]<-1
ban[5,7]<-1
ban[7,5]<-1
ban[5,8]<-1
ban[8,5]<-1
ban[5,9]<-1
ban[9,5]<-1
ban[5,10]<-1
ban[10,5]<-1

ban[6,7]<-1
ban[7,6]<-1
ban[6,8]<-1
ban[8,6]<-1
ban[6,9]<-1
ban[9,6]<-1
ban[6,10]<-1
ban[10,6]<-1

ban[7,8]<-1
ban[8,7]<-1
ban[7,9]<-1
ban[9,7]<-1
ban[7,10]<-1
ban[10,7]<-1

ban[8,9]<-1
ban[9,8]<-1
ban[8,10]<-1
ban[10,8]<-1

ban[9,10]<-1
ban[10,9]<-1


ban[1,11]<-1
ban[2,11]<-1
ban[3,11]<-1
ban[4,11]<-1
ban[5,11]<-1
ban[6,11]<-1
ban[7,11]<-1
ban[8,11]<-1
ban[9,11]<-1
ban[10,11]<-1

ban[11,1]<-1
ban[12,1]<-1
ban[13,1]<-1
ban[14,1]<-1
ban[15,1]<-1
ban[16,1]<-1
ban[17,1]<-1
ban[18,1]<-1
ban[19,1]<-1

ban[1,12]<-1
ban[2,12]<-1
ban[3,12]<-1
ban[4,12]<-1
ban[5,12]<-1
ban[6,12]<-1
ban[7,12]<-1
ban[8,12]<-1
ban[9,12]<-1
ban[10,12]<-1

ban[11,2]<-1
ban[12,2]<-1
ban[13,2]<-1
ban[14,2]<-1
ban[15,2]<-1
ban[16,2]<-1
ban[17,2]<-1
ban[18,2]<-1
ban[19,2]<-1


ban[1,13]<-1
ban[2,13]<-1
ban[3,13]<-1
ban[4,13]<-1
ban[5,13]<-1
ban[6,13]<-1
ban[7,13]<-1
ban[8,13]<-1
ban[9,13]<-1
ban[10,13]<-1

ban[11,3]<-1
ban[12,3]<-1
ban[13,3]<-1
ban[14,3]<-1
ban[15,3]<-1
ban[16,3]<-1
ban[17,3]<-1
ban[18,3]<-1
ban[19,3]<-1


ban[1,14]<-1
ban[2,14]<-1
ban[3,14]<-1
ban[4,14]<-1
ban[5,14]<-1
ban[6,14]<-1
ban[7,14]<-1
ban[8,14]<-1
ban[9,14]<-1
ban[10,14]<-1

ban[11,4]<-1
ban[12,4]<-1
ban[13,4]<-1
ban[14,4]<-1
ban[15,4]<-1
ban[16,4]<-1
ban[17,4]<-1
ban[18,4]<-1
ban[19,4]<-1


ban[1,15]<-1
ban[2,15]<-1
ban[3,15]<-1
ban[4,15]<-1
ban[5,15]<-1
ban[6,15]<-1
ban[7,15]<-1
ban[8,15]<-1
ban[9,15]<-1
ban[10,15]<-1

ban[11,5]<-1
ban[12,5]<-1
ban[13,5]<-1
ban[14,5]<-1
ban[15,5]<-1
ban[16,5]<-1
ban[17,5]<-1
ban[18,5]<-1
ban[19,5]<-1

ban[1,16]<-1
ban[2,16]<-1
ban[3,16]<-1
ban[4,16]<-1
ban[5,16]<-1
ban[6,16]<-1
ban[7,16]<-1
ban[8,16]<-1
ban[9,16]<-1
ban[10,16]<-1

ban[11,6]<-1
ban[12,6]<-1
ban[13,6]<-1
ban[14,6]<-1
ban[15,6]<-1
ban[16,6]<-1
ban[17,6]<-1
ban[18,6]<-1
ban[19,6]<-1



ban[1,17]<-1
ban[2,17]<-1
ban[3,17]<-1
ban[4,17]<-1
ban[5,17]<-1
ban[6,17]<-1
ban[7,17]<-1
ban[8,17]<-1
ban[9,17]<-1
ban[10,17]<-1

ban[11,7]<-1
ban[12,7]<-1
ban[13,7]<-1
ban[14,7]<-1
ban[15,7]<-1
ban[16,7]<-1
ban[17,7]<-1
ban[18,7]<-1
ban[19,7]<-1

ban[1,18]<-1
ban[2,18]<-1
ban[3,18]<-1
ban[4,18]<-1
ban[5,18]<-1
ban[6,18]<-1
ban[7,18]<-1
ban[8,18]<-1
ban[9,18]<-1
ban[10,18]<-1

ban[11,8]<-1
ban[12,8]<-1
ban[13,8]<-1
ban[14,8]<-1
ban[15,8]<-1
ban[16,8]<-1
ban[17,8]<-1
ban[18,8]<-1
ban[19,8]<-1

ban[1,19]<-1
ban[2,19]<-1
ban[3,19]<-1
ban[4,19]<-1
ban[5,19]<-1
ban[6,19]<-1
ban[7,19]<-1
ban[8,19]<-1
ban[9,19]<-1
ban[10,19]<-1

ban[11,9]<-1
ban[12,9]<-1
ban[13,9]<-1
ban[14,9]<-1
ban[15,9]<-1
ban[16,9]<-1
ban[17,9]<-1
ban[18,9]<-1
ban[19,9]<-1

retain<-matrix(rep(0, dim(df)[2]^2), ncol=dim(df)[2])
colnames(retain)<-rownames(retain)<-names(df)


mydists<-list( Binago="binomial",     
               Pavia="binomial",      
               PianuraBG="binomial",   
               PrealpiBG ="binomial",   
               SondrioAV="binomial",
               SondrioMV="binomial", 
               SondrioBV="binomial",  
               Borlezza="binomial",     
               Brembana="binomial",    
               Seriana="binomial",      
               Altrivol="binomial",   
               Carnivori="binomial",    
               Cervidi="binomial", 
               Cinghiale="binomial",  
               Corvidi="binomial",    
               Lepre="binomial",      
               Rapaci ="binomial",      
               sCaprinae="binomial",  
               uccAcqu   ="binomial",   
               colistinaR ="binomial",
               colistinaS ="binomial",  
               ceftiofurR ="binomial",  
               ceftiofurS ="binomial",  
               tilmicosinaR ="binomial",
               tilmicosinaS ="binomial",
               kanamicinaR  ="binomial",
               kanamicinaS ="binomial", 
               enrofloxacinR="binomial",
               enrofloxacinS="binomial",
               oxacillinaR ="binomial",
               oxacillinaS ="binomial",  
               eritromicinaR ="binomial",
               eritromicinaS ="binomial",
               gentamicinaR ="binomial",
               gentamicinaS ="binomial",
               tetraciclinaR ="binomial",
               tetraciclinaS ="binomial",
               ampicillinaR ="binomial",
               ampicillinaS ="binomial")
max.par<-1

mycache<-buildscorecache(data.df = df,data.dists = mydists,
                         dag.banned = ban,dag.retained = retain, max.parents = max.par)




































































































# # options(gargle_oauth_cache = ".secrets")
# # gargle::gargle_oauth_cache()
# # drive_auth()
# # list.files(".secrets/")#<---questo codice fa solo vedere il file presente nella cartella .secrets creata dal codice
# 
# options(
#   gargle_oauth_cache = ".secrets",
#   gargle_oauth_email = TRUE
# )
# drive_auth()
# sheets_auth(token = drive_token())
# mydrive<-drive_find(type = "spreadsheet")
# id<-mydrive[1,2]
# dati<-read_sheet(id$id)
# #saveRDS(dati, file="AMR.Rda")
# 
# df<-readRDS(file="AMR.Rda")

