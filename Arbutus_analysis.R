
 # devtools::install_github("pennell-lab-ubc/arbutus")

### Calculate the ARBUTUS statistics for all species and for each order

library(ape)
library(geiger)
library(arbutus)
library(dplyr)
library(tidyverse)
library(phytools)

#### For Range Size

dat<-read.csv("data/Ranges/Spatial_metadata.csv")
traits<-read.csv("data/Traits/Trait_data.csv")

dat <-dplyr::select(dat, c(Binomial.1.2, Order.1.2, Family.1.2, Genus.1.2, Number.Cells.Present.Natural.Range))
t<- dplyr::select(traits, c(Binomial.1.2, Terrestrial, Marine, Freshwater,  Aerial))
dat<-merge(dat, t, by = "Binomial.1.2", all=T)

#Removing aquatic species
n <- dat %>%
  filter(Marine == 0 & Freshwater == 0 & Terrestrial == 1 | Aerial == 1)

dat <-dplyr::select(n, c(Binomial.1.2, Order.1.2, Family.1.2, Genus.1.2, Number.Cells.Present.Natural.Range))

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

# setwd("Arbutus/tables_arb_range")
slope_S.sar<-function(ordem, tr, tab_name){
  res<-data.frame(matrix(nrow =1000, ncol = 2))   
  colnames(res) <- c("slope", "pvalue")
  for (i in 1:1000) {
    phy<-treedata(tr[[1]],ceta, warnings = FALSE)$phy
    range<-as.data.frame(treedata(tr[[1]],ceta, warnings = FALSE)$data)
    range$nam<-rownames(range)
    a<-range[ order(match(range$nam, phy$tip.label)), ]
    data<-a[,"V1"]
    names(data)<-a$nam
    unit.tree.phy <- make_unit_tree(phy, data=data)
    res$slope[i]<-pic_stat_sasr(unit.tree.phy)
    obs <- calculate_pic_stat(unit.tree.phy, stats=NULL) #calculate default test stats on observed data
    simdat <- simulate_char_unit(unit.tree.phy, nsim=2) #simulate data on unit.tree
    sim <- calculate_pic_stat(simdat, stats=NULL) #calculate default test stats on simulated data
    resp <- compare_pic_stat(obs, sim) #compare simulated to observed test statistics
    res$pvalue<-pvalue_arbutus(resp)[4]
    write.csv(res, tab_name)}
}

slope_S.sar(ceta, tr, "Cetartiodactyla_Ssar_range_teste.csv")
slope_S.sar(lago, tr, "Lagomorpha_Ssar_range.csv")
slope_S.sar(rode, tr, "Rodentia_Ssar_range.csv")
slope_S.sar(chiro, tr, "Chiroptera_Ssar_range.csv")
slope_S.sar(euli, tr, "Eulipotyphla_Ssar_range.csv")
slope_S.sar(prima, tr, "Primates_Ssar_range.csv")
slope_S.sar(car, tr, "Carnivora_Ssar_range.csv")
slope_S.sar(dipro, tr, "Diprotodontia_Ssar_range.csv")
slope_S.sar(dide, tr, "Didelphimorphia_Ssar_range.csv")
slope_S.sar(afro, tr, "Afrosoricida_Ssar_range.csv")
slope_S.sar(dasy, tr, "Dasyuromorphia_Ssar_range.csv")

#### for all species (range size)
all<-data.frame(dat$Number.Cells.Present.Natural.Range)
rownames(all)<-dat$Binomial.1.2
colnames(all)<- "V1"
all<-treedata(tr[[1]], all)$data
# slope_S.sar(all, tr, "zallspp_Ssar_range.csv")


#### For midpoint

dat<-read.csv("data/Ranges/Spatial_metadata.csv")
mp<-read.csv("data/hig_low_midpoint_by_spp_all.csv")
traits<-read.csv("data/Traits/Trait_data.csv")

dat <-dplyr::select(dat, c(Binomial.1.2, Order.1.2, Family.1.2, Genus.1.2, Number.Cells.Present.Natural.Range))
t<- dplyr::select(traits, c(Binomial.1.2, Terrestrial, Marine, Freshwater,  Aerial))
mp<- dplyr::select(mp, c( spp,  mp_lat))
colnames(mp)<- c("Binomial.1.2", "mp_lat")

dat<-merge(dat, t, by = "Binomial.1.2", all=T)
dat<-merge(dat, mp, by = "Binomial.1.2", all=T)

#Removing aquatic species
n <- dat %>%
  filter(Marine == 0 & Freshwater == 0 & Terrestrial == 1 | Aerial == 1)

dat <-dplyr::select(n, c(Binomial.1.2, Order.1.2, Family.1.2, Genus.1.2, Number.Cells.Present.Natural.Range, mp_lat))
# dat$mp_lat<-abs(dat$mp_lat)

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

setwd("Arbutus/tables_arb_midpoint_abs")
slope_S.sar<-function(ordem, tr, tab_name){
  res<-data.frame(matrix(nrow =1000, ncol = 2))   
  colnames(res) <- c("slope", "pvalue")
  for (i in 625:1000) {
    phy<-treedata(tr[[i]],ordem, warnings = FALSE)$phy
    range<-as.data.frame(treedata(tr[[i]],ordem, warnings = FALSE)$data)
    range$nam<-rownames(range)
    a<-range[ order(match(range$nam, phy$tip.label)), ]
    data<-a[,"V1"]
    names(data)<-a$nam
    unit.tree.phy <- make_unit_tree(phy, data=data)
    res$slope[i]<-pic_stat_sasr(unit.tree.phy)
    obs <- calculate_pic_stat(unit.tree.phy, stats=NULL) #calculate default test stats on observed data
    simdat <- simulate_char_unit(unit.tree.phy, nsim=100) #simulate data on unit.tree
    sim <- calculate_pic_stat(simdat, stats=NULL) #calculate default test stats on simulated data
    resp <- compare_pic_stat(obs, sim) #compare simulated to observed test statistics
    res$pvalue[i]<-pvalue_arbutus(resp)[4]
    write.csv(res, tab_name)}
}
slope_S.sar(ceta, tr, "Cetartiodactyla_Ssar_mp_abs.csv")
slope_S.sar(lago, tr, "Lagomorpha_Ssar_mp_abs.csv")
slope_S.sar(rode, tr, "Rodentia_Ssar_mp_abs.csv")
slope_S.sar(chiro, tr, "Chiroptera_Ssar_mp_abs.csv")
slope_S.sar(euli, tr, "Eulipotyphla_Ssar_mp_abs.csv")
slope_S.sar(prima, tr, "Primates_Ssar_mp.csv")
slope_S.sar(car, tr, "Carnivora_Ssar_mp.csv")
slope_S.sar(dipro, tr, "Diprotodontia_Ssar_mp.csv")
slope_S.sar(dide, tr, "Didelphimorphia_Ssar_mp.csv")
slope_S.sar(afro, tr, "Afrosoricida_Ssar_mp_final.csv")
slope_S.sar(dasy, tr, "Dasyuromorphia_Ssar_mp.csv")

all<-data.frame(dat$mp_lat)
rownames(all)<-dat$Binomial.1.2
colnames(all)<- "V1"
table(is.na(all$midp))
all <- all %>% drop_na()
table(is.na(all$midp)) 
slope_S.sar(all, tr, "zall_Ssar_mp_abs.csv")

