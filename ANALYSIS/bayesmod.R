#bayes model per R e RS predittori, denspopkmq, altitudine, pascolo.....

ggcorr(ComAMRsel[, c(5,9,10,11,12,13,14,15,16,17,18,19,21)], geom = "text")

#standardizzo i predittori...trasformo in integer le variabili categoriche

x<-ComAMRsel %>% 
      filter(!Specieagg %in% c("LEPRE", "UCCELLI ACQUATICI","ALTRI VOLATILI")) %>%
      droplevels() %>% 
      rename(densPop=`denpop(abkmq)`) %>% 
      mutate(
        hapasc = replace_na(hapasc, 0),
        capi=replace_na(capi, 0),
        aziende=replace_na(aziende,0),
        densPop=standardize(densPop),
             altitudinesd=standardize(std), 
             superficecom=standardize(sup), 
             gruppoS=as.integer(Specieagg),
             pascolo=standardize(hapasc),
             capiS=standardize(capi),
             aziendeS=standardize(aziende), 
        R=abs(as.integer(as.numeric(as.factor(Res)))-2)
             ) %>% 
      drop_na(R) %>%
      drop_na(std) %>% 
      drop_na(densPop)  
     
  
library(dagitty)
    
WildRes<-dagitty( "dag{    
                     
             Specie->R
             Pascolo->R
             Urbanizzazione->R
             Urbanizzazione->Specie
             Urbanizzazione->Pascolo
             
                     }")
    
    
    plot(graphLayout(WildRes))
    
   impliedConditionalIndependencies(WildRes)    
    
    
#############BAYES MULTILEVEL MODEL WITH BRMS#######
library(brms)
myprior <-  c(set_prior("normal(0, 0.5)", class = "Intercept"),
              set_prior("cauchy(0, 1)", class = "sd"))


mod1 <- brm(R ~ 1 + (1|comune) + Specieagg + pascolo + densPop,
            data = x, family = bernoulli, chains = 4, iter = 4000, warmup = 1000, cores = 4)

mod2 <- brm(R ~ 1 + (1|comune) + Specieagg + pascolo + densPop + Specieagg*pascolo,
            data = x, family = bernoulli, chains = 4, iter = 4000, warmup = 1000, cores = 4)

mod3 <- brm(R ~ 1 + (1|comune) +  Specieagg + pascolo + densPop + Specieagg*pascolo + Specieagg*densPop, 
            data = x, family = bernoulli, chains = 4, iter = 4000, warmup = 1000, cores = 4)

loo(mod1, mod2, mod3)

kfm <- kfold(mod1, K=10)
kfm2 <- kfold(mod2,K=10)
kfm3 <- kfold(mod3, K=10)

options(digits = 2)
kf <- loo_compare(kfm, kfm2, kfm3)

kable(kf, "latex") 
  

library(see)
library(bayestestR)

plot(p_direction(mod))

library()
   
#######BAYES MODEL WITH RETHINKING PACKG#############   
    lista_dati<-list(
      WildlifeR=x$R,
      Pascolo=x$pascolo,
      GruppoSpecie=x$gruppoS,
      Urbanizzazione=x$densPop
    )
    
    
    mod1<-ulam(
      alist(
          WildlifeR~bernoulli(p),
          logit(p)<-Sp[GruppoSpecie]+bP*Pascolo+bU*Urbanizzazione,
          Sp[GruppoSpecie]~dnorm(0,0.5), 
          bP~dlnorm(0, 0.1),
          bU~dlnorm(0,0.1)
        ),data=lista_dati , chains = 5, iter=10000, cores=4, 
        log_lik = TRUE, control=list(max_treedepth=13, adapt_delta=0.99))  
      
    mod2<-ulam(
      alist(
        WildlifeR~bernoulli(p),
        logit(p)<-Sp[GruppoSpecie]+bP*Pascolo,
        Sp[GruppoSpecie]~dnorm(0,0.5), 
        bP~dlnorm(0, 0.1)
      ),data=lista_dati , chains = 5, iter=10000, cores=4,  
      log_lik = TRUE, control=list(max_treedepth=13, adapt_delta=0.99))  
    
      mod3<-ulam(
      alist(
        WildlifeR~bernoulli(p),
        logit(p)<-Sp[GruppoSpecie]+bU*Urbanizzazione,
        Sp[GruppoSpecie]~dnorm(0,0.5), 
        bU~dlnorm(0, 0.1)
      ),data=lista_dati , chains = 5, iter=10000,  cores=4, 
      log_lik = TRUE, control=list(max_treedepth=13, adapt_delta=0.99))  
    
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                         ###Interazione#####
    mod2Int<-
      ulam(
        alist(
          WildlifeR~bernoulli(p),
          logit(p)<-Sp[GruppoSpecie]+(bp+bPSp*GruppoSpecie)*Pascolo,
          Sp[GruppoSpecie]~dnorm(0,0.5), 
          bp~dlnorm(0, 0.1),
          bPSp~dlnorm(0,0.1)
        ),data=lista_dati , chains = 5, iter=10000,  cores=4, 
        log_lik = TRUE, control=list(max_treedepth=13, adapt_delta=0.99)) 
      
    mod3Int<-
      ulam(
        alist(
          WildlifeR~bernoulli(p),
          logit(p)<-Sp[GruppoSpecie]+(bp+bPSp*GruppoSpecie)*Pascolo+bU*Urbanizzazione,
          Sp[GruppoSpecie]~dnorm(0,0.5),
          bU~dlnorm(0,0.1),
          bp~dlnorm(0, 0.1),
          bPSp~dlnorm(0,0.1)
        ),data=lista_dati , chains = 5, iter=10000,  cores=4, 
        log_lik = TRUE, control=list(max_treedepth=13, adapt_delta=0.99)) 
    
    
    
  ####multilevels#####  
      lista_datiML<-list(
      WildlifeR=x$R,
      Pascolo=x$pascolo,
      GruppoSpecie=x$gruppoS,
      Urbanizzazione=x$densPop,
      comune=factor(x$comune)
    )
    

    MLInt<-
      ulam(
        alist(
          WildlifeR~bernoulli(p),
          logit(p)<-a[comune]*sigma_g +Sp[GruppoSpecie]+(bp+bPSp*GruppoSpecie)*Pascolo+bU*Urbanizzazione,
          Sp[GruppoSpecie]~dnorm(0,0.5), 
          bU~dlnorm(0,0.1),
          bp~dlnorm(0, 0.1),
          bPSp~dlnorm(0,0.1),
          ##parte del multilevel model 
          
          a[comune]~dnorm(0,0.5),##adaptive prior
          sigma_g~dexp(7)#hyoerprior
        ),data=lista_datiML , chains = 5, iter=10000,  cores=4, 
        log_lik = TRUE, control=list(max_treedepth=13, adapt_delta=0.99)) 
    
    
 
    load("modelli.RData")
    load("mod2Int.Rdata")
    load("mod3Int.Rdata")
    load("MLInt.Rdata")
       
    mods<-compare(mod1, mod2, mod3, mod2Int,mod3Int, MLInt)
    mods2<-compare(mod1, mod2, mod3, mod2Int,mod3Int, MLInt, func = PSIS)
   
     mods %>% 
      kable("latex") %>% 
      kable_styling()
     
     mods2 %>% 
       kable("latex") %>% 
       kable_styling()
     
    
     plot(precis(MLInt, depth = 2, pars=c("Sp","bU", "bp", "bPSp")))
     
     posterior<-extract.samples(MLInt) %>% 
      data.frame() %>% 
       select(1:9) %>% 
       pivot_longer(1:9, names_to="predittori", values_to = "logit") %>% 
       mutate(predittori=recode(predittori, Sp.6="RAPACI", Sp.5="CORVIDI",
                                 Sp.4="SUIDI", Sp.3= "CARNIVORI", Sp.2="BOVIDI",
                                 Sp.1="CERVIDI", bU="Urbanizzazione", bp="Pascolo",
                                 bPSp="Pascolo*Specie")) %>% 
       mutate(predittori=factor(predittori, levels=c("Urbanizzazione",
           "Pascolo","Pascolo*Specie","CERVIDI","BOVIDI","SUIDI","CARNIVORI",
           "CORVIDI","RAPACI"))) %>% 
       ggplot(aes(x = logit, y=predittori, fill=predittori)) +
       geom_density_ridges(panel_scaling=TRUE)+
       theme_ridges()+
       scale_fill_brewer(palette = 7) +
       theme_ridges() + theme(legend.position = "NULL")+
       labs(x="Coefficienti di regressione (Stime a Posteriori Bayesiane)",y="")
     
     
        mutate(Species = recode(Species, setosa = "SETOSA",
                               versicolor = "VERSICOLOR",
                               virginica = "VIRGINICA"))   
       
       
       
    
     fitted<-link(MLInt)
     
     WRes.mean<-data.frame("Prevalence"=apply(fitted, 2, mean))
     WRes.PI<-data.frame(t(apply(fitted, 2, PI, prob=0.97)))

     Prediction<-cbind(x, WRes.mean, WRes.PI)
     
     Prediction %>% 
       ggplot(aes(x=pascolo, y=Prevalence))+geom_point()+
       facet_wrap(~Specieagg)
Prediction %>% 
  ggplot(aes(y=Res, x=pascolo))+geom_point()+
  geom_line(WRes.mean, aes(y=Prevalence))

Prediction %>% 
  ggplot(aes(x=pascolo, y=R))+
  geom_point(aes(y=Prevalence))+facet_grid(~Specieagg)
#####################################################################