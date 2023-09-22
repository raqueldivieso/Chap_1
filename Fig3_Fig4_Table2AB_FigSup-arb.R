#Figure 3
# install.packages("scales")
library(scales)
library(ggplot2)
library(dplyr)
library(tidyverse)
library(hrbrthemes)
library(viridis)
library(forcats)

setwd("/Arbutus/tables_arb_range_log")
## Combining tables

filenames <- list.files(".",  pattern = ".csv")
all_files <- Reduce(cbind, lapply(filenames, read.csv))
all_files<- data.frame(all_files)

Data <- subset(all_files, select = c(slope, slope.1, slope.2, slope.3, slope.4, slope.5, slope.6, slope.7, slope.8, slope.9, slope.10, slope.11))

# write.csv(all_files, "Slopes_ordem.csv")

Data<-rename(Data, All_spp=slope, Afrosoricida=slope.1, Carnivora=slope.2, Cetartiodactyla=slope.3, Chiroptera=slope.4, Dasyuromorphia=slope.5, Didelphimorphia=slope.6, Diprotodontia=slope.7, Eulipotyphla=slope.8, Lagomorpha=slope.9, Primates=slope.10, Rodentia=slope.11  )

tiff("Figura_3_rangesize6_teste.tiff",  width=10, height=12, units="in", res=800)
layout(matrix(1:12, ncol=3, nrow = 4, byrow=TRUE))
par(mar = c(3, 3, 2, 2))

#All
plot(1, type='n',xlim=c(0.8, 30),ylim=c(-20,30), xlab=' ', ylab="", main=NULL, cex.axis=1.5)

for (i in 1:1000) {
  abline(a=0, b = Data$All_spp[i], col = alpha("#0087b3", 0.2))}
abline(a=0, b = mean(Data$All_spp), col="black", lwd = 1.8)

#Afrosoricida
plot(1, type='n',xlim=c(0.8, 30),ylim=c(-20,30), xlab='', ylab="", main=NULL, cex.axis=1.5)

for (i in 1:1000) {
  abline(a=0, b = Data$Afrosoricida[i], col = alpha("#0087b3", 0.2))}
abline(a=0, b = mean(Data$Afrosoricida), col="black", lwd = 1.8)


#Carnivora
plot(1,type='n',xlim=c(0.8, 30),ylim=c(-20,30),xlab=' ', ylab='    ', main=NULL, cex.axis=1.5)

for (i in 1:1000) {
  abline(a=0, b = Data$Carnivora[i], col = alpha("#0087b3", 0.2))}
abline(a=0, b = mean(Data$Carnivora), col="black", lwd = 1.8)


#Cetartiodactyla
plot(1,type='n',xlim=c(0.8, 30),ylim=c(-20,30),xlab=' ', ylab='    ', main=NULL, cex.axis=1.5)

for (i in 1:1000) {
  abline(a=0, b = Data$Cetartiodactyla[i], col = alpha("#0087b3", 0.2))}
abline(a=0, b = mean(Data$Cetartiodactyla), col="black", lwd = 1.8)

#Chiroptera
plot(1,type='n',xlim=c(0.8, 30),ylim=c(-20,30),xlab=' ', ylab='    ', main=NULL, cex.axis=1.5)

for (i in 1:1000) {
  abline(a=0, b = Data$Chiroptera[i], col = alpha("#0087b3", 0.2))}
abline(a=0, b = mean(Data$Chiroptera), col="black", lwd = 1.8)

#Dasyuromorphia
plot(1,type='n',xlim=c(0.8, 30),ylim=c(-20,30),xlab=' ', ylab='    ', main=NULL, cex.axis=1.5)

for (i in 1:1000) {
  abline(a=0, b = Data$Dasyuromorphia[i], col = alpha("#0087b3", 0.2))}
abline(a=0, b = mean(Data$Dasyuromorphia), col="black", lwd = 1.8)


#Didelphimorphia
plot(1,type='n',xlim=c(0.8, 30),ylim=c(-20,30),xlab=' ', ylab='   ', main=NULL, cex.axis=1.5)

for (i in 1:1000) {
  abline(a=0, b = Data$Didelphimorphia[i], col = alpha("#0087b3", 0.2))}
abline(a=0, b = mean(Data$Didelphimorphia), col="black", lwd = 1.8)


#Diprotodontia
plot(1,type='n',xlim=c(0.8, 30),ylim=c(-20,30),xlab=' ', ylab='   ', main=NULL, cex.axis=1.5)

for (i in 1:1000) {
  abline(a=0, b = Data$Diprotodontia[i], col = alpha("#0087b3", 0.2))}
abline(a=0, b = mean(Data$Diprotodontia), col="black", lwd = 1.8)

#Eulipotyphla
plot(1,type='n',xlim=c(0.8, 30),ylim=c(-20,30),xlab=' ', ylab='   ', main=NULL, cex.axis=1.5)

for (i in 1:1000) {
  abline(a=0, b = Data$Eulipotyphla[i], col = alpha("#0087b3", 0.2))}
abline(a=0, b = mean(Data$Eulipotyphla), col="black", lwd = 1.8)

#Lagomorpha
plot(1,type='n',xlim=c(0.8, 30),ylim=c(-20,30),xlab=' ', ylab='   ', main=NULL, cex.axis=1.5)

for (i in 1:1000) {
  abline(a=0, b = Data$Lagomorpha[i], col = alpha("#0087b3", 0.2))}
abline(a=0, b = mean(Data$Lagomorpha), col="black", lwd = 1.8)

#Primates
plot(1,type='n',xlim=c(0.8, 30),ylim=c(-20,30),xlab=' ', ylab='   ', main=NULL, cex.axis=1.5)

for (i in 1:1000) {
  abline(a=0, b = Data$Primates[i], col = alpha("#0087b3", 0.2))}
abline(a=0, b = mean(Data$Primates), col="black", lwd = 1.8)


#Rodentia
plot(1,type='n',xlim=c(0.8, 30),ylim=c(-20,30),xlab=' ', ylab='    ', main=NULL, cex.axis=1.8)

for (i in 1:1000) {
  abline(a=0, b = Data$Rodentia[i], col = alpha("#0087b3", 0.2))}
abline(a=0, b = mean(Data$Rodentia), col="black", lwd = 1.8)

dev.off()

###Now for latitudinal midpoint

setwd("C:/Users/raque/OneDrive - ufpr.br/Área de Trabalho/1_cap_final/Arbutus/tables_arb_midpoint_abs")

filenames <- list.files(".",  pattern = ".csv")
all_files <- Reduce(cbind, lapply(filenames, read.csv))
all_files<- data.frame(all_files)

Data <- subset(all_files, select = c(slope, slope.1, slope.2, slope.3, slope.4, slope.5, slope.6, slope.7, slope.8, slope.9, slope.10, slope.11))

Data <- dplyr::select(all_files, slope, slope.1, slope.2, slope.3, slope.4, slope.5, slope.6, slope.7, slope.8, slope.9, slope.10, slope.11)

write.csv(all_files, "Slopes_ordem.csv")

Data<-rename(Data, All_spp=slope, Afrosoricida=slope.1, Carnivora=slope.2, Cetartiodactyla=slope.3, Chiroptera=slope.4, Dasyuromorphia=slope.5, Didelphimorphia=slope.6, Diprotodontia=slope.7, Eulipotyphla=slope.8, Lagomorpha=slope.9, Primates=slope.10, Rodentia=slope.11  )

# colnames(Data)<-c("All_spp", "Afrosoricida", "Carnivora", "Cetartiodactyla", "Chiroptera", "Dasyuromorphia", "Didelphimorphia", "Diprotodontia", "Eulipotyphla", "Lagomorpha", "Primates","Rodentia")
# # byrow=TRUE

# pdf("Figura_2_midpoint.pdf", width=10, height=12)
# dev.off()
tiff("Figura_4_mid_point_abs.tiff",  width=10, height=12, units="in", res=800)
layout(matrix(1:12, ncol=3, nrow = 4, byrow=TRUE))
par(mar = c(3, 3, 2, 2))
# par(mar = c(bottom, left, top, right))

#All
plot(1, type='n',xlim=c(1, 30),ylim=c(-2,5), xlab=' ', ylab="", main=NULL, cex.axis=1.5)

for (i in 1:1000) {
  abline(a=0, b = Data$All_spp[i], col = alpha("#cd5555", 0.2))}
abline(a=0, b = mean(Data$All_spp), col="black", lwd = 1.8)


#Afrosoricida
plot(1, type='n', xlim=c(1, 30),ylim=c(-2,5), xlab='', ylab="", main=NULL, cex.axis=1.5)

for (i in 1:1000) {
  abline(a=0, b = Data$Afrosoricida[i], col = alpha("#cd5555", 0.2))}
abline(a=0, b = mean(Data$Afrosoricida), col="black", lwd = 1.8)


#Carnivora
plot(1, type='n',xlim=c(1, 30),ylim=c(-2,5),xlab=' ', ylab='    ', main=NULL, cex.axis=1.5)


for (i in 1:1000) {
  abline(a=0, b = Data$Carnivora[i], col = alpha("#cd5555", 0.2))}
abline(a=0, b = mean(Data$Carnivora), col="black", lwd = 1.8)


#Cetartiodactyla
plot(1,type='n',xlim=c(1, 30),ylim=c(-2,5),xlab=' ', ylab='    ', main=NULL, cex.axis=1.5)

for (i in 1:1000) {
  abline(a=0, b = Data$Cetartiodactyla[i], col = alpha("#cd5555", 0.2))}
abline(a=0, b = mean(Data$Cetartiodactyla), col="black", lwd = 1.8)

#Chiroptera
plot(1,type='n',xlim=c(1, 30),ylim=c(-2,5),xlab=' ', ylab='    ', main=NULL, cex.axis=1.5)

for (i in 1:1000) {
  abline(a=0, b = Data$Chiroptera[i], col = alpha("#cd5555", 0.2))}
abline(a=0, b = mean(Data$Chiroptera), col="black", lwd = 1.8)

#Dasyuromorphia
plot(1,type='n',xlim=c(1, 30),ylim=c(-2,5),xlab=' ', ylab='    ', main=NULL, cex.axis=1.5)

for (i in 1:1000) {
  abline(a=0, b = Data$Dasyuromorphia[i], col = alpha("#cd5555", 0.2))}
abline(a=0, b = mean(Data$Dasyuromorphia), col="black", lwd = 1.8)


#Didelphimorphia
plot(1,type='n',xlim=c(1, 30),ylim=c(-2,5),xlab=' ', ylab='   ', main=NULL, cex.axis=1.5)

for (i in 1:1000) {
  abline(a=0, b = Data$Didelphimorphia[i], col = alpha("#cd5555", 0.2))}
abline(a=0, b = mean(Data$Didelphimorphia), col="black", lwd = 1.8)


#Diprotodontia
plot(1,type='n',xlim=c(1, 30),ylim=c(-2,5),xlab=' ', ylab='   ', main=NULL, cex.axis=1.5)

for (i in 1:1000) {
  abline(a=0, b = Data$Diprotodontia[i], col = alpha("#cd5555", 0.2))}
abline(a=0, b = mean(Data$Diprotodontia), col="black", lwd = 1.8)

#Eulipotyphla
plot(1,type='n',xlim=c(1, 30),ylim=c(-2,5),xlab=' ', ylab='   ', main=NULL, cex.axis=1.5)

for (i in 1:1000) {
  abline(a=0, b = Data$Eulipotyphla[i], col = alpha("#cd5555", 0.2))}
abline(a=0, b = mean(Data$Eulipotyphla), col="black", lwd = 1.8)

#Lagomorpha
plot(1,type='n',xlim=c(1, 30),ylim=c(-2,5),xlab=' ', ylab='   ', main=NULL, cex.axis=1.5)

for (i in 1:1000) {
  abline(a=0, b = Data$Lagomorpha[i], col = alpha("#cd5555", 0.2))}
abline(a=0, b = mean(Data$Lagomorpha), col="black", lwd = 1.8)

#Primates
plot(1,type='n',xlim=c(1, 30),ylim=c(-2,5),xlab=' ', ylab='   ', main=NULL, cex.axis=1.5)

for (i in 1:1000) {
  abline(a=0, b = Data$Primates[i], col = alpha("#cd5555", 0.2))}
abline(a=0, b = mean(Data$Primates), col="black", lwd = 1.8)


#Rodentia
plot(1,type='n',xlim=c(1, 30),ylim=c(-2,5), xlab=' ', ylab='    ', main=NULL, cex.axis=1.5)

for (i in 1:1000) {
  abline(a=0, b = Data$Rodentia[i], col = alpha("#cd5555", 0.2))}
abline(a=0, b = mean(Data$Rodentia), col="black", lwd = 1.8)

dev.off()

### Tab 2- Range Size (log)
#### Calculating the significance between all 1000 p-values:
rm(list=ls())
setwd("C:/Users/raque/OneDrive - ufpr.br/Área de Trabalho/1_cap_final/Arbutus/tables_arb_range_kmlog")
filenames <- list.files(".",  pattern = ".csv")
all_files <- Reduce(cbind, lapply(filenames, read.csv))
all_files<- data.frame(all_files)

Data <- subset(all_files, select = -c(X, X.1, X.2, X.3, X.4, X.5, X.6, X.7, X.8, X.9, X.10, X.11))

colnames(Data)<-c("All_spp_sars", "All_spp_p", "Afrosoricida_sars","Afrosoricida_p", "Carnivora_sars","Carnivora_p", "Cetartiodactyla_sars","Cetartiodactyla_p", "Chiroptera_sars", "Chiroptera_p", "Dasyuromorphia_sars", "Dasyuromorphia_p", "Didelphimorphia_sars", "Didelphimorphia_p", "Diprotodontia_sars","Diprotodontia_p", "Eulipotyphla_sars","Eulipotyphla_p", "Lagomorpha_sars", "Lagomorpha_p", "Primates_sars", "Primates_p", "Rodentia_sars", "Rodentia_p")

tab2<-data.frame(matrix(nrow =12, ncol = 3))
colnames(tab2) <- c("order","Significant_pvalues", "Mean_slope")
tab2$order<-c("All_spp", "Afrosoricida", "Carnivora", "Cetartiodactyla", "Chiroptera", "Dasyuromorphia", "Didelphimorphia", "Diprotodontia", "Eulipotyphla", "Lagomorpha", "Primates","Rodentia")

for (i in 1:12){
  
  x<-select(Data, paste(tab2$order[i], "sars", sep="_"))[,1]
  y<-select(Data, paste(tab2$order[i], "p", sep="_"))[,1]
  adjusted_p_values <- p.adjust(y, method = "bonferroni", n=1000)
  tab2$Significant_pvalues[i]<- sum(adjusted_p_values <= 0.05)
  tab2$Mean_slope[i]<- mean(x)
}

write.csv(tab2, "C:/Users/raque/OneDrive - ufpr.br/Área de Trabalho/1_cap_final/Arbutus/Tab2A_rangesize.csv")

###Tab 2B- Lat MidPoint (abs)
#### Calculating the significance between all 1000 p-values:
rm(list=ls())
setwd("C:/Users/raque/OneDrive - ufpr.br/Área de Trabalho/1_cap_final/Arbutus/tables_arb_midpoint_abs")
filenames <- list.files(".",  pattern = ".csv")
all_files <- Reduce(cbind, lapply(filenames, read.csv))
all_files<- data.frame(all_files)

 write.csv(Data, "arb_res_mpabs.csv")
Data <- subset(all_files, select = -c(X, X.1, X.2, X.3, X.4, X.5, X.6, X.7, X.8, X.9, X.10, X.11))

head(Data)

#once the p.adjust function did not run with all zeros, we need to set a p-value extreme low to place zeros velues:
Data<-
  Data %>%
  mutate(across(slope:pvalue.11, ~ replace(., . ==  0 , 0.000001)))
hist(Data$slope)

colnames(Data)<-c("All_spp_sars", "All_spp_p", "Afrosoricida_sars","Afrosoricida_p", "Carnivora_sars","Carnivora_p", "Cetartiodactyla_sars","Cetartiodactyla_p", "Chiroptera_sars", "Chiroptera_p", "Dasyuromorphia_sars", "Dasyuromorphia_p", "Didelphimorphia_sars", "Didelphimorphia_p", "Diprotodontia_sars","Diprotodontia_p", "Eulipotyphla_sars","Eulipotyphla_p", "Lagomorpha_sars", "Lagomorpha_p", "Primates_sars", "Primates_p", "Rodentia_sars", "Rodentia_p")

tab2<-data.frame(matrix(nrow =12, ncol = 3))
colnames(tab2) <- c("order","Significant_pvalues", "Mean_slope")
tab2$order<-c("All_spp", "Afrosoricida", "Carnivora", "Cetartiodactyla", "Chiroptera", "Dasyuromorphia", "Didelphimorphia", "Diprotodontia", "Eulipotyphla", "Lagomorpha", "Primates","Rodentia")

for (i in 1:12){
  x<-select(Data, paste(tab2$order[6], "sars", sep="_"))[,1]
  y<-select(Data, paste(tab2$order[5], "p", sep="_"))[,1]
  adjusted_p_values <- p.adjust(y, method = "bonferroni", n=1000)
  tab2$Significant_pvalues[i]<- sum(adjusted_p_values <= 0.05)
  tab2$Mean_slope[i]<- mean(x)
}
write.csv(tab2, "C:/Users/raque/OneDrive - ufpr.br/Área de Trabalho/1_cap_final/Arbutus/Tab2B_midpoint_abs.csv")


###P-values histograms - Range size
setwd("C:/Users/raque/OneDrive - ufpr.br/Área de Trabalho/1_cap_final/Arbutus/tables_arb_range_kmlog")
filenames <- list.files(".",  pattern = ".csv")
all_files <- Reduce(cbind, lapply(filenames, read.csv))
all_files<- data.frame(all_files)
Data <- subset(all_files, select = c(pvalue , pvalue.1, pvalue.2, pvalue.3, pvalue.4, pvalue.5, pvalue.6, pvalue.7, pvalue.8, pvalue.9, pvalue.10, pvalue.11))

colnames(Data)<-c("Aall_spp", "Afrosoricida", "Carnivora", "Cetartiodactyla", "Chiroptera", "Dasyuromorphia", "Didelphimorphia", "Diprotodontia", "Eulipotyphla", "Lagomorpha", "Primates","Rodentia")
# 
# Data<-rename(Data, All_spp=slope, Afrosoricida=slope.1, Carnivora=slope.2, Cetartiodactyla=slope.3, Chiroptera=slope.4, Dasyuromorphia=slope.5, Didelphimorphia=slope.6, Diprotodontia=slope.7, Eulipotyphla=slope.8, Lagomorpha=slope.9, Primates=slope.10, Rodentia=slope.11  )
# 
# 

Data <- Data %>%
  gather(key = "order", value = "pvalue", everything())

# plot
rangesize<-ggplot(Data, aes(x = pvalue)) +
  geom_histogram() +
  facet_wrap(~ order, scales = "free", ncol = 3) +
  lims(x=c(-0.02, 0.75))+
  geom_vline(aes(xintercept = 0.05, colour ="red"),linewidth=0.8)+
  labs(x = "p-value", y = "frequency") +
  theme_bw()+
  theme(axis.title=element_text(size=17), strip.text = element_text(size=17), legend.position = "none")

tiff("C:/Users/raque/OneDrive - ufpr.br/Área de Trabalho/1_cap_final/Arbutus/Figura_Sup_Mat_rangesize_l.tiff",  width=10, height=12, units="in", res=800)
rangesize
dev.off()


setwd("C:/Users/raque/OneDrive - ufpr.br/Área de Trabalho/1_cap_final/Arbutus/tables_arb_midpoint_abs")
filenames <- list.files(".",  pattern = ".csv")
all_files <- Reduce(cbind, lapply(filenames, read.csv))
all_files<- data.frame(all_files)
Data <- subset(all_files, select = c(pvalue , pvalue.1, pvalue.2, pvalue.3, pvalue.4, pvalue.5, pvalue.6, pvalue.7, pvalue.8, pvalue.9, pvalue.10, pvalue.11))
colnames(Data)<-c("Aall_spp", "Afrosoricida", "Carnivora", "Cetartiodactyla", "Chiroptera", "Dasyuromorphia", "Didelphimorphia", "Diprotodontia", "Eulipotyphla", "Lagomorpha", "Primates","Rodentia")

Data <- Data %>%
  gather(key = "order", value = "pvalue", everything())

# plot
midpoint<-ggplot(Data, aes(x = pvalue)) +
  geom_histogram() +
  facet_wrap(~ order, scales = "free", ncol=3) +
  geom_vline(aes(xintercept = 0.05, colour ="red"),linewidth=0.8)+
  labs(x = "p-value", y = "frequency") +
  theme_bw()+
  theme(axis.title=element_text(size=17), strip.text = element_text(size=17), legend.position = "none")
# tiff(midpoint, "midpoint_arbutus_hist.tiff")

tiff("C:/Users/raque/OneDrive - ufpr.br/Área de Trabalho/1_cap_final/Arbutus/Figura_Sup_Mat_midpoint_abs.tiff",  width=10, height=12, units="in", res=800)
midpoint
dev.off()

library(terra)


