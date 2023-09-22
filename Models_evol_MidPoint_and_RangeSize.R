# pack<- c('ape', 'geiger', 'phytools', 'picante', 'tidyverse', 'tidyverse', 'tidyr', 'dplyr')
# installed_packages <- pack %in% rownames(installed.packages())
# if (any(installed_packages == FALSE)) {
#   install.packages(pack[!installed_packages])
# }

library(tidyr)
library(dplyr)
library(ape)
library(geiger)
library(phytools)
library(picante)
library(tidyverse)

dat<-read.csv("data/Ranges/Spatial_metadata.csv")
mp<-read.csv("data/hig_low_midpoint_by_spp_all.csv")
traits<-read.csv("data/Traits/Trait_data.csv")

dat <-dplyr::select(dat, c(Binomial.1.2, Order.1.2, Family.1.2, Genus.1.2, Number.Cells.Present.Natural.Range))
t<- select(traits, c(Binomial.1.2, Terrestrial, Marine, Freshwater,  Aerial))
mp<- select(mp, c( spp,  mp_lat))
colnames(mp)<- c("Binomial.1.2", "mp_lat")

dat<-merge(dat, t, by = "Binomial.1.2", all=T)
dat<-merge(dat, mp, by = "Binomial.1.2", all=T)


#Removing aquatic species
n <- dat %>%
  filter(Marine == 0 & Freshwater == 0 & Terrestrial == 1 | Aerial == 1)

dat <-select(n, c(Binomial.1.2, Order.1.2, Family.1.2, Genus.1.2, Number.Cells.Present.Natural.Range, mp_lat))

table(is.na(dat$mp_lat))
dat <- dat %>% drop_na()
table(is.na(dat$mp_lat)) 

dat$mp_lat<-abs(dat$mp_lat)

tr<-read.nexus("data/Phylogenies/Complete_phylogeny.nex")

prep<-function(order, dat, tr){
  x<-subset(dat, Order.1.2==order, select=c("Binomial.1.2", "mp_lat"))
  xx<-x[,2]
  names(xx)<-x[,1]
  res<-treedata(tr, xx)$data
}

#Applying the function
rode<-prep("Rodentia", dat, tr[[1]])
chiro<-prep("Chiroptera", dat, tr[[1]])
euli<-prep("Eulipotyphla", dat, tr[[1]])
prima<-prep("Primates", dat, tr[[1]])
ceta<-prep("Cetartiodactyla", dat, tr[[1]])
car<-prep("Carnivora", dat, tr[[1]])
dipro<-prep("Diprotodontia", dat, tr[[1]])
dide<-prep("Didelphimorphia", dat, tr[[1]])
lago<-prep("Lagomorpha", dat, tr[[1]])
afro<-prep("Afrosoricida", dat, tr[[1]])
dasy<-prep("Dasyuromorphia", dat, tr[[1]])

hist(afro)




####Brownian Motion
fitc_MA_BM<-function(x,tr, order){
  res<-data.frame(matrix(nrow=1000,ncol=6))
  colnames(res) <- c('sigsq','z0','lnL', 'k', 'aic','aicc')
  for (i in 1:50) {
    y<-treedata(tr[[i]],x, warnings = FALSE)
    m1<-fitContinuous(y$phy, y$data, model="BM")
    res[i,]<-c(m1$opt$sigsq, m1$opt$z0, m1$opt$lnL, m1$opt$k, m1$opt$aic, m1$opt$aicc)
    write.csv(res,order)
  }
  res
}

####Lambda
fitc_MA_Lambda<-function(x,tr, order){
  res<-data.frame(matrix(nrow=1000,ncol=6))
  colnames(res) <- c('sigsq','z0','lnL', 'k', 'aic','aicc')
  for (i in 1:50) {
    y<-treedata(tr[[i]],x, warnings = FALSE)
    m1<-fitContinuous(y$phy, y$data, model="lambda")
    res[i,]<-c(m1$opt$sigsq, m1$opt$z0, m1$opt$lnL, m1$opt$k, m1$opt$aic, m1$opt$aicc)
    write.csv(res, order)
  }
  res
}

###Kappa
fitc_MA_Kappa<-function(x,tr, order){
  res<-data.frame(matrix(nrow=50,ncol=6))
  colnames(res) <- c('sigsq','z0','lnL', 'k', 'aic','aicc')
  for (i in 1:1000) {
    y<-treedata(tr[[i]],x, warnings = FALSE)
    m1<-fitContinuous(y$phy, y$data, model="kappa")
    res[i,]<-c(m1$opt$sigsq, m1$opt$z0, m1$opt$lnL, m1$opt$k, m1$opt$aic, m1$opt$aicc)
    write.csv(res, order)
  }
  res
}

fitc_MA_BM(ceta, tr, "mp_Cetartiodactyla_BM_abs.csv")
fitc_MA_BM(lago, tr, "mp_Lagomorpha_BM_abs.csv")
fitc_MA_BM(rode, tr, "mp_Rodentia_BM_abs.csv")
fitc_MA_BM(chiro, tr, "mp_Chiroptera_BM_abs.csv")
fitc_MA_BM(euli, tr, "mp_Eulipotyphla_BM_abs.csv")
fitc_MA_BM(prima, tr, "mp_Primates_BM_abs.csv")
fitc_MA_BM(car, tr, "mp_Carnivora_BM_abs.csv")
fitc_MA_BM(dipro, tr, "mp_Diprotodontia_BM_abs.csv")
fitc_MA_BM(dide, tr, "mp_Didelphimorphia_BM_abs.csv")
fitc_MA_BM(afro, tr, "mp_Afrosoricida_BM_abs.csv")
fitc_MA_BM(dasy, tr, "mp_Dasyuromorphia_BM_abs.csv")

fitc_MA_Lambda(ceta, tr, "mp_Cetartiodactyla_Lambda_abs.csv")
fitc_MA_Lambda(lago, tr, "mp_Lagomorpha_Lambda_abs.csv")
fitc_MA_Lambda(rode, tr, "mp_Rodentia_Lambda_abs.csv")
fitc_MA_Lambda(chiro, tr, "mp_Chiroptera_Lambda_abs.csv")
fitc_MA_Lambda(euli, tr, "mp_Eulipotyphla_Lambda_abs.csv")
fitc_MA_Lambda(prima, tr, "mp_Primates_Lambda_abs.csv")
fitc_MA_Lambda(car, tr, "mp_Carnivora_Lambda_abs.csv")
fitc_MA_Lambda(dipro, tr, "mp_Diprotodontia_Lambda_abs.csv")
fitc_MA_Lambda(dide, tr, "mp_Didelphimorphia_Lambda_abs.csv")
fitc_MA_Lambda(afro, tr, "mp_Afrosoricida_Lambda_abs.csv")
fitc_MA_Lambda(dasy, tr, "mp_Dasyuromorphia_Lambda_abs.csv")
# #
fitc_MA_Kappa(ceta, tr, "mp_Cetartiodactyla_Kappa_abs.csv")
fitc_MA_Kappa(lago, tr, "mp_Lagomorpha_Kappa_abs.csv")
fitc_MA_Kappa(rode, tr, "mp_Rodentia_Kappa_abs.csv")
fitc_MA_Kappa(chiro, tr, "mp_Chiroptera_Kappa_abs.csv")
fitc_MA_Kappa(euli, tr, "mp_Eulipotyphla_Kappa_abs.csv")
fitc_MA_Kappa(prima, tr, "mp_Primates_Kappa_abs.csv")
fitc_MA_Kappa(car, tr, "mp_Carnivora_Kappa_abs.csv")
fitc_MA_Kappa(dipro, tr, "mp_Diprotodontia_Kappa_abs.csv")
fitc_MA_Kappa(dide, tr, "mp_Didelphimorphia_Kappa_abs.csv")
fitc_MA_Kappa(afro, tr, "mp_Afrosoricida_Kappa_abs.csv")
fitc_MA_Kappa(dasy, tr, "mp_Dasyuromorphia_Kappa_abs.csv")


#### For all species
all<-data.frame(dat$mp_lat)
rownames(all)<-dat$Binomial.1.2
all$dat.mp_lat<- abs(all$dat.mp_lat)
colnames(all)<- "midp"
table(is.na(all$midp))

all<-treedata(tr[[1]], all)$data

###BM
res_BM<-data.frame(matrix(nrow=1000,ncol=6))
colnames(res_BM) <- c('sigsq','z0','lnL', 'k', 'aic','aicc')
for (i in 1:1000) {
  y<-treedata(tr[[i]], all, warnings = FALSE)
  m1<-fitContinuous(y$phy, y$data, model="BM")
  res_BM[i,]<-c(m1$opt$sigsq, m1$opt$z0, m1$opt$lnL, m1$opt$k, m1$opt$aic, m1$opt$aicc)
  write.csv(res_BM, "mp_zall_spp_BM_abs-ok.csv")
}
# 
# ###Lambda
res_Lambda<-data.frame(matrix(nrow=1000,ncol=6))
colnames(res_Lambda) <- c('sigsq','z0','lnL', 'k', 'aic','aicc')
for (i in 1:1000) {
  y<-treedata(tr[[i]], all, warnings = FALSE)
  m1<-fitContinuous(y$phy, y$data, model="lambda")
  res_Lambda[i,]<-c(m1$opt$sigsq, m1$opt$z0, m1$opt$lnL, m1$opt$k, m1$opt$aic, m1$opt$aicc)
  write.csv(res_Lambda, "mp_zall_spp_Lambda_abs.csv")
}
# 
###Kappa
res_Kappa<-data.frame(matrix(nrow=1000,ncol=6))
colnames(res_Kappa) <- c('sigsq','z0','lnL', 'k', 'aic','aicc')
for (i in 1:1000) {
  y<-treedata(tr[[i]],all, warnings = FALSE)
  m1<-fitContinuous(y$phy, y$data, model="kappa")
  res_Kappa[i,]<-c(m1$opt$sigsq, m1$opt$z0, m1$opt$lnL, m1$opt$k, m1$opt$aic, m1$opt$aicc)
  write.csv(res_Kappa, "mp_zall_spp_Kappa.csv")
}


rm(list=ls())
#####################
# Now all over again for the range size

dat<-read.csv("data/Ranges/Spatial_metadata.csv")
traits<-read.csv("data/Traits/Trait_data.csv")

dat <-select(dat, c(Binomial.1.2, Order.1.2, Family.1.2, Genus.1.2, Number.Cells.Present.Natural.Range))
t<- select(traits, c(Binomial.1.2, Terrestrial, Marine, Freshwater,  Aerial))
dat<-merge(dat, t, by = "Binomial.1.2", all=T)
head(dat)

#Removing aquatic species
n <- dat %>%
  filter(Marine == 0 & Freshwater == 0 & Terrestrial == 1 | Aerial == 1)

dat <-select(n, c(Binomial.1.2, Order.1.2, Family.1.2, Genus.1.2, Number.Cells.Present.Natural.Range))

dat$Number.Cells.Present.Natural.Range<-log((dat$Number.Cells.Present.Natural.Range+1)*96.5)

tr<-read.nexus("data/Phylogenies/Complete_phylogeny.nex")

prep<-function(order, dat, tr){
  x<-subset(dat, Order.1.2==order, select=c("Binomial.1.2", "Number.Cells.Present.Natural.Range"))
  xx<-x[,2]
  names(xx)<-x[,1]
  res<-treedata(tr, xx)$data
}

rode<-prep("Rodentia", dat, tr[[1]])
chiro<-prep("Chiroptera", dat, tr[[1]])
euli<-prep("Eulipotyphla", dat, tr[[1]])
prima<-prep("Primates", dat, tr[[1]])
ceta<-prep("Cetartiodactyla", dat, tr[[1]])
car<-prep("Carnivora", dat, tr[[1]])
dipro<-prep("Diprotodontia", dat, tr[[1]])
dide<-prep("Didelphimorphia", dat, tr[[1]])
lago<-prep("Lagomorpha", dat, tr[[1]])
afro<-prep("Afrosoricida", dat, tr[[1]])
dasy<-prep("Dasyuromorphia", dat, tr[[1]])

# median(afro)
# median(car)
# median(ceta)
# median(chiro)
# median(dasy)
# median(dide)
# median(dipro)
# median(euli)
# median(lago)
# median(prima)
# median(rode)

####Brownian Motion
fitc_MA_BM<-function(x,tr, order){
  res<-data.frame(matrix(nrow=1000,ncol=6))
  colnames(res) <- c('sigsq','z0','lnL', 'k', 'aic','aicc')
  for (i in 1:1000) {
    y<-treedata(tr[[i]],x, warnings = FALSE)
    m1<-fitContinuous(y$phy, y$data, model="BM")
    res[i,]<-c(m1$opt$sigsq, m1$opt$z0, m1$opt$lnL, m1$opt$k, m1$opt$aic, m1$opt$aicc)
    write.csv(res,order)
  }
  res
}

####Lambda
fitc_MA_Lambda<-function(x,tr, order){
  res<-data.frame(matrix(nrow=1000,ncol=6))
  colnames(res) <- c('sigsq','z0','lnL', 'k', 'aic','aicc')
  for (i in 1:1000) {
    y<-treedata(tr[[i]],x, warnings = FALSE)
    m1<-fitContinuous(y$phy, y$data, model="lambda")
    res[i,]<-c(m1$opt$sigsq, m1$opt$z0, m1$opt$lnL, m1$opt$k, m1$opt$aic, m1$opt$aicc)
    write.csv(res, order)
  }
  res
}

####Kappa
fitc_MA_Kappa<-function(x,tr, order){
  res<-data.frame(matrix(nrow=1000,ncol=6))
  colnames(res) <- c('sigsq','z0','lnL', 'k', 'aic','aicc')
  for (i in 1:1000) {
    y<-treedata(tr[[i]],x, warnings = FALSE)
    m1<-fitContinuous(y$phy, y$data, model="kappa")
    res[i,]<-c(m1$opt$sigsq, m1$opt$z0, m1$opt$lnL, m1$opt$k, m1$opt$aic, m1$opt$aicc)
    write.csv(res, order)
  }
  res
}


fitc_MA_BM(ceta, tr, "rs_Cetartiodactyla_BM.csv")
fitc_MA_BM(lago, tr, "rs_Lagomorpha_BM.csv")
fitc_MA_BM(rode, tr, "rs_Rodentia_BM.csv")
fitc_MA_BM(chiro, tr, "rs_Chiroptera_BM.csv")
fitc_MA_BM(euli, tr, "rs_Eulipotyphla_BM.csv")
fitc_MA_BM(prima, tr, "rs_Primates_BM.csv")
fitc_MA_BM(car, tr, "rs_Carnivora_BM.csv")
fitc_MA_BM(dipro, tr, "rs_Diprotodontia_BM.csv")
fitc_MA_BM(dide, tr, "rs_Didelphimorphia_BM.csv")
fitc_MA_BM(afro, tr, "rs_Afrosoricida_BM.csv")
fitc_MA_BM(dasy, tr, "rs_Dasyuromorphia_BM.csv")

fitc_MA_Lambda(ceta, tr, "rs_Cetartiodactyla_Lambda.csv")
fitc_MA_Lambda(lago, tr, "rs_Lagomorpha_Lambda.csv")
fitc_MA_Lambda(rode, tr, "rs_Rodentia_Lambda.csv")
fitc_MA_Lambda(chiro, tr, "rs_Chiroptera_Lambda.csv")
fitc_MA_Lambda(euli, tr, "rs_Eulipotyphla_Lambda.csv")
fitc_MA_Lambda(prima, tr, "rs_Primates_Lambda.csv")
fitc_MA_Lambda(car, tr, "rs_Carnivora_Lambda.csv")
fitc_MA_Lambda(dipro, tr, "rs_Diprotodontia_Lambda.csv")
fitc_MA_Lambda(dide, tr, "rs_Didelphimorphia_Lambda.csv")
fitc_MA_Lambda(afro, tr, "rs_Afrosoricida_Lambda.csv")
fitc_MA_Lambda(dasy, tr, "rs_Dasyuromorphia_Lambda.csv")

fitc_MA_Kappa(ceta, tr, "rs_Cetartiodactyla_Kappa.csv")
fitc_MA_Kappa(lago, tr, "rs_Lagomorpha_Kappa.csv")
fitc_MA_Kappa(rode, tr, "rs_Rodentia_Kappa.csv")
fitc_MA_Kappa(chiro, tr, "rs_Chiroptera_Kappa.csv")
fitc_MA_Kappa(euli, tr, "rs_Eulipotyphla_Kappa.csv")
fitc_MA_Kappa(prima, tr, "rs_Primates_Kappa.csv")
fitc_MA_Kappa(car, tr, "rs_Carnivora_Kappa.csv")
fitc_MA_Kappa(dipro, tr, "rs_Diprotodontia_Kappa.csv")
fitc_MA_Kappa(dide, tr, "rs_Didelphimorphia_Kappa.csv")
fitc_MA_Kappa(afro, tr, "rs_Afrosoricida_Kappa.csv")
fitc_MA_Kappa(dasy, tr, "rs_Dasyuromorphia_Kappa.csv")

###For all species
all<-data.frame(dat$Number.Cells.Present.Natural.Range)
rownames(all)<-dat$Binomial.1.2
colnames(all)<- "range"
all<-treedata(tr[[1]], all)$data

###BM
res_BM<-data.frame(matrix(nrow=1000,ncol=6))
colnames(res_BM) <- c('sigsq','z0','lnL', 'k', 'aic','aicc')
for (i in 1:1000) {
  y<-treedata(tr[[i]], all, warnings = FALSE)
  
  m1<-fitContinuous(y$phy, y$data, model="BM")
  res_BM[i,]<-c(m1$opt$sigsq, m1$opt$z0, m1$opt$lnL, m1$opt$k, m1$opt$aic, m1$opt$aicc)
  write.csv(res_BM, "rs_tables/rs_zall_spp_BM.csv")
}

###Lambda
res_Lambda<-data.frame(matrix(nrow=1000,ncol=6))
colnames(res_Lambda) <- c('sigsq','z0','lnL', 'k', 'aic','aicc')
for (i in 1:1000) {
  y<-treedata(tr[[i]],all, warnings = FALSE)
  m1<-fitContinuous(y$phy, y$data, model="lambda")
  res_Lambda[i,]<-c(m1$opt$sigsq, m1$opt$z0, m1$opt$lnL, m1$opt$k, m1$opt$aic, m1$opt$aicc)
  write.csv(res_Lambda, "rs_tables/rs_zall_spp_Lambda.csv")
}

###Kappa
res_Kappa<-data.frame(matrix(nrow=1000,ncol=6))
colnames(res_Kappa) <- c('sigsq','z0','lnL', 'k', 'aic','aicc')
for (i in 1:1000) {
  y<-treedata(tr[[i]],all, warnings = FALSE)
  m1<-fitContinuous(y$phy, y$data, model="kappa")
  res_Kappa[i,]<-c(m1$opt$sigsq, m1$opt$z0, m1$opt$lnL, m1$opt$k, m1$opt$aic, m1$opt$aicc)
  write.csv(res_Kappa, "rs_tables/rs_zall_spp_Kappa.csv")}




###############    Second part of the code     ###############

### After all the fits were calculate and stored, we need to calculate the model averaging of each model for the 11 orders and for all species using the AIC and the number of parameters (k) in each model

#### First for Range Size

library(dplyr)
library(geiger)
rm(list=ls())


org_aic<- function(order){
  filenames <- list.files(".", pattern = order)
  all_files <- data.frame(Reduce(cbind, lapply(filenames, read.csv)))
  tb<-select(all_files, aic, k, aic.1, k.1, aic.2, k.2)
  colnames(tb)<-c("BM_aic", "BM_k", "Kappa_aic", "Kappa_k", "Lambda_aic", "Lambda_k")
  tb}

all<-org_aic("zall_spp")
afr<-org_aic("Afrosoricida")
car<-org_aic("Carnivora")
cet<-org_aic("Cetartiodactyla")
chi<-org_aic("Chiroptera")
das<-org_aic("Dasyuromorphia")
did<-org_aic("Didelphimorphia")
dip<-org_aic("Diprotodontia")
eul<-org_aic("Eulipotyphla")
lag<-org_aic("Lagomorpha")
pri<-org_aic("Primates")
rod<-org_aic("Rodentia")

df_mod_rs <- data.frame(matrix(ncol = 6, nrow = 12))
colnames(df_mod_rs)<- c("order", "BM_mod_aver", "Lambda_mod_aver", "Kappa_mod_aver", "WN", "spp_n")

df_mod_rs$order<-c("All_spp","Afrosoricida","Carnivora", "Cetartiodactyla","Chiroptera","Dasyuromorphia","Didelphimorphia", "Diprotodontia","Eulipotyphla", "Lagomorpha", "Primates","Rodentia")


###Calculating the model averaging
mod_averg<- function(ord, n){

ord$rel_lik_BM <- exp((min(ord$BM_aic) - ord$BM_aic)/2)
sum_rel_lik_BM <- sum(ord$rel_lik_BM)
ord$weight_BM  <- ord$rel_lik_BM/sum_rel_lik_BM
df_mod_rs[n,2] <- sum(ord$weight_BM * ord$BM_aic) + 2 * sum(ord$weight_BM * (ord$BM_k + 1)/(nrow(ord) - min(ord$BM_k) - 2))


ord$rel_lik_Lambda <- exp((min(ord$Lambda_aic) - ord$Lambda_aic)/2)
sum_rel_lik_Lambda<- sum(ord$rel_lik_Lambda)
ord$weight_Lambda <- ord$rel_lik_Lambda/sum_rel_lik_Lambda
df_mod_rs[n,3] <- sum(ord$weight_Lambda * ord$Lambda_aic) + 2 * sum(ord$weight_Lambda * (ord$Lambda_k + 1)/(nrow(ord) - min(ord$Lambda_k) - 2))

ord$rel_lik_Kappa <- exp((min(ord$Kappa_aic) - ord$Kappa_aic)/2)
sum_rel_lik_Kappa<- sum(ord$rel_lik_Kappa)
ord$weight_Kappa <- ord$rel_lik_Kappa/sum_rel_lik_Kappa
df_mod_rs[n,4] <- sum(ord$weight_Kappa * ord$Kappa_aic) + 2 * sum(ord$weight_Kappa * (ord$Kappa_k + 1)/(nrow(ord) - min(ord$Kappa_k) - 2))

return(df_mod_rs)
}

df_mod_rs<-mod_averg(all, 1)
df_mod_rs<-mod_averg(afr, 2)
df_mod_rs<-mod_averg(car, 3)
df_mod_rs<-mod_averg(cet, 4)
df_mod_rs<-mod_averg(chi, 5)
df_mod_rs<-mod_averg(das, 6)
df_mod_rs<-mod_averg(did, 7)
df_mod_rs<-mod_averg(dip, 8)
df_mod_rs<-mod_averg(eul, 9)
df_mod_rs<-mod_averg(lag, 10)
df_mod_rs<-mod_averg(pri, 11)
df_mod_rs<-mod_averg(rod, 12)



#Run the 10:66 lines to open original data!#####

### Now we need to calculate the fit of each data on White Noise model, that is how good a non-phylogenetic model explains our data.

y_list <- list(df_mod_rs$order)[[1]]

for (i in 2:12){
x<-subset(dat, Order.1.2==y_list[i], select=c("Binomial.1.2", "Number.Cells.Present.Natural.Range"))
x$Number.Cells.Present.Natural.Range<-log((x$Number.Cells.Present.Natural.Range+1)*96.5)
xx<-x[,2]
names(xx)<-x[,1]
y<-treedata(tr[[1]], xx, warnings = FALSE)
df_mod_rs[i,5]<-fitContinuous(y$phy, y$data, model="white")$opt$aic }


###For all species

x<- select(dat, c("Binomial.1.2", "Number.Cells.Present.Natural.Range"))
x$Number.Cells.Present.Natural.Range<-log((x$Number.Cells.Present.Natural.Range+1)*96.5)
xx<-x[,2]
names(xx)<-x[,1]
all<-treedata(tr[[1]], xx, warnings = FALSE)
df_mod_rs[1,5]<-fitContinuous(all$phy, all$data, model="white")$opt$aic

df_mod_rs$spp_n[1]<- length(all$data)
df_mod_rs$spp_n[2]<-length(afro)
df_mod_rs$spp_n[3]<-length(car)
df_mod_rs$spp_n[4]<-length(ceta)
df_mod_rs$spp_n[5]<-length(chiro)
df_mod_rs$spp_n[6]<-length(dasy)
df_mod_rs$spp_n[7]<-length(dide)
df_mod_rs$spp_n[8]<-length(dipro)
df_mod_rs$spp_n[9]<-length(euli)
df_mod_rs$spp_n[10]<-length(lago)
df_mod_rs$spp_n[11]<-length(prima)
df_mod_rs$spp_n[12]<-length(rode)

write.csv(df_mod_rs, "Models_AIC-avr_RangeSize.csv")

dir()
##### Now all over again for the latitudinal midpoint
rm(list=ls())

folder<-"."

org_aic<- function(order){
  filenames <- list.files(folder, pattern = order)
  all_files <- data.frame(Reduce(cbind, lapply(filenames, read.csv)))
  tb<-select(all_files, aic, k, aic.1, k.1, aic.2, k.2)
  colnames(tb)<-c("BM_aic", "BM_k", "Kappa_aic", "Kappa_k", "Lambda_aic", "Lambda_k")
  tb}

all<-org_aic("zall_spp")
afr<-org_aic("Afrosoricida")
car<-org_aic("Carnivora")
cet<-org_aic("Cetartiodactyla")
chi<-org_aic("Chiroptera")
das<-org_aic("Dasyuromorphia")
did<-org_aic("Didelphimorphia")
dip<-org_aic("Diprotodontia")
eul<-org_aic("Eulipotyphla")
lag<-org_aic("Lagomorpha")
pri<-org_aic("Primates")
rod<-org_aic("Rodentia")

df_mod_mp <- data.frame(matrix(ncol = 5, nrow = 12))
colnames(df_mod_mp)<- c("order", "BM_mod_aver", "Lambda_mod_aver", "Kappa_mod_aver", "WN")

df_mod_mp$order<-c("All_spp","Afrosoricida","Carnivora", "Cetartiodactyla","Chiroptera","Dasyuromorphia","Didelphimorphia", "Diprotodontia","Eulipotyphla", "Lagomorpha", "Primates","Rodentia")


###Calculating the model averaging
mod_averg<- function(ord, n){
  
  ord$rel_lik_BM <- exp((min(ord$BM_aic) - ord$BM_aic)/2)
  sum_rel_lik_BM <- sum(ord$rel_lik_BM)
  ord$weight_BM  <- ord$rel_lik_BM/sum_rel_lik_BM
  df_mod_mp[n,2] <- sum(ord$weight_BM * ord$BM_aic) + 2 * sum(ord$weight_BM * (ord$BM_k + 1)/(nrow(ord) - min(ord$BM_k) - 2))
  
  
  ord$rel_lik_Lambda <- exp((min(ord$Lambda_aic) - ord$Lambda_aic)/2)
  sum_rel_lik_Lambda<- sum(ord$rel_lik_Lambda)
  ord$weight_Lambda <- ord$rel_lik_Lambda/sum_rel_lik_Lambda
  df_mod_mp[n,3] <- sum(ord$weight_Lambda * ord$Lambda_aic) + 2 * sum(ord$weight_Lambda * (ord$Lambda_k + 1)/(nrow(ord) - min(ord$Lambda_k) - 2))
  
  ord$rel_lik_Kappa <- exp((min(ord$Kappa_aic) - ord$Kappa_aic)/2)
  sum_rel_lik_Kappa<- sum(ord$rel_lik_Kappa)
  ord$weight_Kappa <- ord$rel_lik_Kappa/sum_rel_lik_Kappa
  df_mod_mp[n,4] <- sum(ord$weight_Kappa * ord$Kappa_aic) + 2 * sum(ord$weight_Kappa * (ord$Kappa_k + 1)/(nrow(ord) - min(ord$Kappa_k) - 2))
  
  return(df_mod_mp)
}

df_mod_mp<-mod_averg(all, 1)
df_mod_mp<-mod_averg(afr, 2)
df_mod_mp<-mod_averg(car, 3)
df_mod_mp<-mod_averg(cet, 4)
df_mod_mp<-mod_averg(chi, 5)
df_mod_mp<-mod_averg(das, 6)
df_mod_mp<-mod_averg(did, 7)
df_mod_mp<-mod_averg(dip, 8)
df_mod_mp<-mod_averg(eul, 9)
df_mod_mp<-mod_averg(lag, 10)
df_mod_mp<-mod_averg(pri, 11)
df_mod_mp<-mod_averg(rod, 12)


### Now we need to calculate the fit of each data on White Noise model, that is how good a non-phylogenetic model explains our data.

#Run the 9:44 lines to open original data
y_list <- list(df_mod_mp$order)[[1]]

for (i in 2:12){
  x<-subset(dat, Order.1.2==y_list[i], select=c("Binomial.1.2", "mp_lat"))
  xx<-x[,2]
  names(xx)<-x[,1]
  y<-treedata(tr[[1]], xx, warnings = FALSE)
  df_mod_mp[i,5]<-fitContinuous(y$phy, y$data, model="white")$opt$aic }


###For all species

x<- select(dat, c("Binomial.1.2", "mp_lat"))
xx<-x[,2]
names(xx)<-x[,1]
all<-treedata(tr[[1]], xx, warnings = FALSE)
df_mod_mp[1,5]<-fitContinuous(all$phy, all$data, model="white")$opt$aic

df_mod_mp$spp_n[1]<- length(all$data)
df_mod_mp$spp_n[2]<-length(afro)
df_mod_mp$spp_n[3]<-length(car)
df_mod_mp$spp_n[4]<-length(ceta)
df_mod_mp$spp_n[5]<-length(chiro)
df_mod_mp$spp_n[6]<-length(dasy)
df_mod_mp$spp_n[7]<-length(dide)
df_mod_mp$spp_n[8]<-length(dipro)
df_mod_mp$spp_n[9]<-length(euli)
df_mod_mp$spp_n[10]<-length(lago)
df_mod_mp$spp_n[11]<-length(prima)
df_mod_mp$spp_n[12]<-length(rode)

write.csv(df_mod_mp, "Models_AIC-avr_MidPoint_abs.csv")
