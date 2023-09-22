#### Now we will perform a simple, but informative analisys. To test whether the rate of evolution of range size has a correlation with the ancestral midpoint of latitude we calculate the rate of each genera and use the average area size between its species in a PGLs.
#Again, to consider the phylogenetic uncertain, we use the average model to calculate the best correlation model. 
#Our mainly hypothesis is that the genus with major range sizes will present a faster rate of evolution.???

#First we break the phylogeny at genus level
# data from: https://datadryad.org/resource/doi:10.5061/dryad.bp26v20
# rm(list=ls())

# install.packages("dplyr")
# install.packages("geiger")
# install.packages("phytools")
# install.packages("tidyverse")
# install.packages("ape")
# install.packages("nlme")
# install.packages("caper")
library(geiger)
library(phytools)
library(tidyverse)
library(ape)
library(nlme)
library(caper)
library(dplyr)
library(ggplot2)
library(RColorBrewer)
library(gridExtra)
library(showtext)

dat<-read.csv("data/Ranges/Spatial_metadata.csv")
mp<-read.csv("data/hig_low_midpoint_by_spp_all.csv")
traits<-read.csv("data/Traits/Trait_data.csv")

dat <-dplyr::select(dat, c(Binomial.1.2, Order.1.2, Family.1.2, Genus.1.2, Number.Cells.Present.Natural.Range))

t<- dplyr::select(traits, c(Binomial.1.2, Terrestrial, Marine, Freshwater,  Aerial))
mp<- dplyr::select(mp, c(spp,  mp_lat))
colnames(mp)<- c("Binomial.1.2", "mp_lat")

dat<-merge(dat, t, by = "Binomial.1.2", all=T)
dat<-merge(dat, mp, by = "Binomial.1.2", all=T)


#Removing aquatic species
n <- dat %>%
  filter(Marine == 0 & Freshwater == 0 & Terrestrial == 1 | Aerial == 1)

dat <-dplyr::select(n, c(Binomial.1.2, Order.1.2, Family.1.2, Genus.1.2, Number.Cells.Present.Natural.Range, mp_lat))

#Function to keep only one species by genus in the phylogeny
keep.tip <- function(phy, tip)
{
  if (!inherits(phy, "phylo"))
    stop("object \"phy\" is not of class \"phylo\"")
  Ntip <- length(phy$tip.label)
  ## convert to indices if strings passed in
  if (is.character(tip)) {
    idx <- match(tip, phy$tip.label)
    ## stop on bad tip names
    ## alternative is to warn but proceed. not sure what stance is
    if (anyNA(idx)) {
      um <- c("umatched tip labels:\n", paste(tip[is.na(idx)], collapse = " "))
      stop(um)
    }
    tip <- idx
  } else {
    # check that passed in indices are all valid
    out.of.range <- tip > Ntip
    if (any(out.of.range)) {
      warning("some tip numbers were larger than the number of tips: they were ignored")
      tip <- tip[!out.of.range]
    }
  }
  ## get complement tip indices to drop
  toDrop <- setdiff(1:Ntip, tip)
  drop.tip(phy, toDrop)
}

tr<-read.nexus("data/Phylogenies/Complete_phylogeny.nex")

# Code steps:
# Prune the 1000 trees for the same genus;
# Calculate (range size) sigma for the 1000 trees of each genus (with >3 spp);
# Calculate the model averaging between 1000 sigmas;
# Calculate ancestral midpoint of each genus- average bt the 1000 values;
# Save the genus name, the average sigma of the 1000 trees and the average midpoint ancestor value in a same table.

#separating species into their particular genera
nam<-tr$tip.label
gen<-character()
for (i in 1:length(nam$tip.label)){
  gen[i]<-strsplit(nam$tip.label[i], "_")[[1]][1]
  spp<-data.frame(cbind(nam$tip.label, gen))
  spp<-spp[duplicated(spp$gen) | duplicated(spp$gen, fromLast = TRUE), ]}
#write.csv(spp,"spp.csv")

#Creating a unique list of genera
keep_gen<-unique(spp$gen) 
#write.table(keep_gen, "keep_gen.csv")


#Transforming negative latitudes
# dat$mp_lat_abs<-abs(dat$mp_lat)

library(parallel)
library(doParallel)

cl <- makeCluster(detectCores()-1)
registerDoParallel(cl)
clusterExport(cl, varlist = c("keep.tip","tr", "spp", "dat"))
clusterEvalQ(cl, library("geiger"))
clusterEvalQ(cl, library("ape"))
clusterEvalQ(cl, library("phytools"))


each_tree<-function(i, k_gen){
  g<-toString(k_gen)
  esp<-subset(spp, gen==g)
  tip<-as.character(esp$V1)
  gen_tr<-keep.tip(tr[[i]], tip) ### i
  x<-subset(dat, select=c("Binomial.1.2", "mp_lat"))
  xx<-x[,2]
  names(xx)<-x[,1]
  dat_gen<-treedata(gen_tr, xx)$data
  fit<-fitContinuous(gen_tr, dat_gen, model="BM")
  fit$opt$z0}


tab_final<-data.frame(matrix(nrow =775, ncol = 3))
colnames(tab_final) <- c("genus", "n_spp", "mp_anc")
for(j in 1:775){
  try({
    k_gen<-keep_gen[j]
    g<-toString(k_gen)  
    esp<-subset(spp, gen==g)
    res<-parLapply(cl, 1:1000, each_tree, k_gen=g)
    tab_final$genus[j]<-g                           #### j
    tab_final$mp_anc[j]<- mean(unlist(res), na.rm = T)   #### j
    tab_final$n_spp[j]<- dim(esp)[1]})                  #### j
  write.csv(tab_final, "ancestral_state_mp_neglat.csv")}

stopCluster(cl)


##Calculating the log likelihood and rate of evolution of range size for each genus with each tree.

cl <- makeCluster(detectCores()-1)
registerDoParallel(cl)
clusterExport(cl, varlist = c("keep.tip","tr", "spp", "dat"))
clusterEvalQ(cl, library("geiger"))
clusterEvalQ(cl, library("ape"))
clusterEvalQ(cl, library("phytools"))

tab_final_rate<-data.frame(matrix(nrow =775, ncol = 3))
colnames(tab_final_rate) <- c("genus", "n_spp", "sigma")

each_tr_rate<-function(t, k_gen){
  g<-toString(k_gen)
  esp<-subset(spp, gen==g)
  tip<-as.character(esp$V1)
  gen_tr<-keep.tip(tr[[t]], tip) 
  x<-subset(dat, select=c("Binomial.1.2", "Number.Cells.Present.Natural.Range"))
  x$Number.Cells.Present.Natural.Range<- log(x$Number.Cells.Present.Natural.Range)
  xx<-x[,2]
  names(xx)<-x[,1]
  dat_gen<-treedata(gen_tr, xx)$data
  rate<-fitContinuous(gen_tr, dat_gen, model="lambda")
  rate$opt$sigsq}
  
for(j in 1:775){
  try({
    k_gen<-keep_gen[j]
    g<-toString(k_gen)
    esp<-subset(spp, gen==g)
    res<-parLapply(cl, 1:1000, each_tr_rate, k_gen=g)
      tab_final_rate$genus[j]<-g                             #### j
      tab_final_rate$sigma[j]<- mean(unlist(res), na.rm = T)   #### j
      tab_final_rate$n_spp[j]<- dim(esp)[1]                  #### j
      write.csv(tab_final_rate, "rate_evol_range-size-gen_comp_phy.csv")})}

stopCluster(cl)


#### Testing the relation (using a PGLS) between the ancestral state of midPoint and the rate of evolution for each genus with more than 3 and 5 species.

anc_MP<-na.omit(read.csv("PGLS_lat-range/ancestral_state_mp_neglat.csv"))
rate_RS<-na.omit(read.csv("PGLS_lat-range/rate_evol_range-size-gen_comp_phy.csv"))
rate_RS$sigma<-as.numeric(rate_RS$sigma)
anc_MP$mp_anc<-abs(anc_MP$mp_anc)
dat<-merge(rate_RS, anc_MP, by="genus", all.x = T)

dat<-  dat %>% 
  filter(n_spp.x>2) 

######
#Bulding a genus level (1000) phylogenies of Mammals
#tr: 1000 phylogenetic trees

keep.tip <- function(phy, tip)
{
  if (!inherits(phy, "phylo"))
    stop("object \"phy\" is not of class \"phylo\"")
  Ntip <- length(phy$tip.label)
  ## convert to indices if strings passed in
  if (is.character(tip)) {
    idx <- match(tip, phy$tip.label)
    ## stop on bad tip names
    ## alternative is to warn but proceed. not sure what stance is
    if (anyNA(idx)) {
      um <- c("umatched tip labels:\n", paste(tip[is.na(idx)], collapse = " "))
      stop(um)
    }
    tip <- idx
  } else {
    # check that passed in indices are all valid
    out.of.range <- tip > Ntip
    if (any(out.of.range)) {
      warning("some tip numbers were larger than the number of tips: they were ignored")
      tip <- tip[!out.of.range]
    }
  }
  ## get complement tip indices to drop
  toDrop <- setdiff(1:Ntip, tip)
  drop.tip(phy, toDrop)
}

#Function to prune species of all 1000 trees and keep genus.
gen_tr_mil<-function(tr)
{
  tree_gen<-as.multiPhylo(tr[[1]])
  ##loop to apply for all 1000 phylogenies
  for (j in 1:1000){
    #Get a list of all species and genus names, keep in a list just one specie per genus level trees:
    nam<-tr[[j]]$tip.label
    gen<-character()
    for (i in 1:5831)
      gen[i]<-strsplit(tr[[j]]$tip.label[[i]], "_")[[1]][1]
    df1<-data.frame(nam)
    df2<-data.frame(gen)
    df<-data.frame(df2$gen, df1$nam)
    # prune the tree of all but one member of each genus:
    keep_sp<-distinct(df, df$df2.gen, .keep_all= TRUE)
    spp_to_keep<-as.character(keep_sp$df1.nam)
    tree_gen[[j]]<-keep.tip(tr[[j]],spp_to_keep)
    #rename our tips to be the genus names only:
    tree_gen[[j]]$tip.label<-sapply(strsplit(tree_gen[[j]]$tip.label,"_"),function(x) x[1])}
  tree_gen}

Mil_Phy_genus_Mammals<-gen_tr_mil(tr)
writeNexus(Mil_Phy_genus_Mammals, file="data/1000_tr_gen_complete.nex")

#### Calculate a pgls abline for each tree
tree<- read.nexus("PGLS_lat-range/1000_tr_gen_complete.nex")

data<-dplyr::select(datt, genus, sigma, mp_anc)

df<-  matrix(ncol = 2, nrow = 0)
colnames(df) <- c("generos", "names")
df<- data.frame(df)

results<-data.frame(matrix(ncol = 4, nrow = 1000))
colnames(results)<-c("intercept", "abline", "p_value", "r.squared")

### PGLS with absolute latitudes

for (i in 1:1000){
  comp.data<-comparative.data(tree[[i]], data, names.col="genus", vcv.dim=2)
  mod<-pgls(sigma~mp_anc, data=comp.data) 
  results$intercept[i]<-round(summary(mod)$coefficients[[1]], 3)
  results$abline[i]<-round(summary(mod)$coefficients[[2]], 3)
  results$p_value[i]<-round(summary(mod)$coefficients[[8]], 3)
  results$r.squared[i]<-round(summary(mod)$adj.r.squared, 3)
write.csv(results, "PGLS_lat-range/results_lat_abs.csv")
}

results<-read.csv("PGLS_lat-range/results_abs.csv") #results_lat_abs
showtext_opts(dpi = 1000)
showtext_auto(enable = TRUE)

p<- ggplot(results, aes(x=p_value))+ 
  geom_histogram(bins=50) +
  geom_vline(aes(xintercept = 0.05, col='red'))+
  ylab("Frequency") + 
  xlab("p-value")+
  theme_bw()+
  theme(panel.background = element_blank(), axis.title.x = element_text(size=10),
        axis.title.y = element_text(size=10))+
  theme(legend.position = "none")


p
b<-results$p_value<0.05
table(b) #67% 


#### Figure 5

ord<-read.csv("data/Ranges/Spatial_metadata.csv")
ord<- ord %>% 
  dplyr::select("Genus.1.2", "Order.1.2") %>% 
  rename(genus = Genus.1.2) %>% 
  distinct()

dados<-merge(datt, ord, by="genus")

table(dados$Order.1.2)

mo<-list("Primates",
         "Chiroptera",
         "Carnivora",
         "Cetartiodactyla",
         "Rodentia",
         "Diprotodontia",
         "Eulipotyphla")

replace_names <- function(name, excluded_names) {
  ifelse(name %in% excluded_names, name, "Other orders")
}
dados$Order.1.2 <- replace_names(dados$Order.1.2, mo)
head(dados)

Order.1.2 <- c("Rodentia", "Carnivora", "Cetartiodactyla", "Chiroptera", "Diprotodontia", "Eulipotyphla", "Primates", "Other orders")
colors <- c("#f37e46","#58016d", "#3d79b3", "#2fceca", "#a40215", "#8FD744",  "#FDE724", "white")
df_colors <- data.frame(Order.1.2, colors)
dados <- merge(dados, df_colors, by = "Order.1.2", all.x = TRUE)
dados$Order.1.2<-  gsub("Rodentia", "aRodentia", dados$Order.1.2)

dados<-dados[order(dados$Order.1.2), ]


p1<-ggplot(dados, aes(mp_anc, sigma, fill = colors)) +
  geom_point(size = 2, alpha = 0.75, shape = 21, color = "black")+
  scale_fill_identity() +
  xlab("Genus ancestral latudinal midpoint") + 
  ylab("Genus evolutionaty rate of range size (σ)")+
  theme_bw()+
  theme(panel.background = element_blank(), axis.title.x = element_text(size=10),
        axis.title.y = element_text(size=10))

g<-grid.arrange(p,p1, ncol = 2, nrow = 1)

g <- grid.arrange(p, p1, ncol = 2, widths = c(3, 3))

ggsave("Fig_5.tiff", g, width = 10, height = 5, units = "in", dpi = 1000)

library(ggplot2)
slopes<-read.csv("PGLS_lat-range/results.csv")

# Create an empty data frame
empty_df <- data.frame(x = numeric(0), y = numeric(0))

# Generate the empty plot with specified limits
ylim_values <- c(-0.006, 0, 0.005)  
pl <- ggplot(data = empty_df, aes(x = x, y = y)) +
  coord_cartesian(xlim = c(0.005, 0.1), ylim = c(-0.006, 0.005))+
  theme_bw() + 
  scale_y_continuous(breaks = ylim_values, labels = ylim_values)+
 theme(axis.line = element_line(colour = "black"))+
  xlab("")+
  ylab("")

for (i in 1:nrow(slopes)) {
  pl <- pl + geom_abline(intercept = 0, slope = slopes$abline[i], color="gray35", linewidth=0.8, alpha = 0.50)}

ggsave("ablines_pgls.tiff", pl, width = 5, height = 5, units = "in", dpi = 1000)

min(slopes$abline)
# ggsave("arranged_plots2.tiff", u, width = 10, height = 5, units = "in", dpi = 800)









empty_df <- data.frame(x = numeric(0), y = numeric(0))

# Create an empty plot with specified limits
p <- ggplot(data = empty_df, aes(x = x, y = y)) +
  coord_cartesian(xlim = c(0.005, 0.1), ylim = c(-0.006, 0.005))+
  theme_bw() + 
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))
for (i in 1:1000) {
  p+ geom_abline(intercept=0, slope = slopes$abline[i], col = alpha("gray47", 0.2))}


for (i in 1:1000) {
  abline(a=0, b = slopes$abline[i], col = alpha("gray47", 0.2))}
dev.off()

