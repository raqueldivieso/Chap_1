library(dplyr)
library(ggplot2)
library(tidyverse)
library(gridExtra)

### Figure 2 - AIC values distribution for Latitudinal Mid-Point
rm(list=ls())

setwd("C:/Users/raque/OneDrive - ufpr.br/Área de Trabalho/1_cap_final")
dir()
ala<-read.csv("Models_evol/Models_AIC-avr_MidPoint_abs.csv")

# dataframe organization:

# First, let's combine all AIC values for each model in a dataframe by order and one for all species. All tables needed are in a folder named "tables".
setwd("Models_evol/mp_tables")
dir()

folder<-"C:/Users/raque/OneDrive - ufpr.br/Área de Trabalho/1_cap_final/Models_evol/mp_tables"
#
org_lnl<- function(order){
  filenames <- list.files(folder, pattern = order)
  all_files <- data.frame(Reduce(cbind, lapply(filenames, read.csv)))
  tb<-select(all_files, aic, aic.1, aic.2)
  colnames(tb)<-c("BM", "Kappa", "Lambda")
  write.csv(tb, as.character(paste(order, ".csv", sep = ""))) }


org_lnl("zall_spp")
org_lnl("Afrosoricida")
org_lnl("Carnivora")
org_lnl("Cetartiodactyla")
org_lnl("Chiroptera")
org_lnl("Dasyuromorphia")
org_lnl("Didelphimorphia")
org_lnl("Diprotodontia")
org_lnl("Eulipotyphla")
org_lnl("Lagomorpha")
org_lnl("Primates")
org_lnl("Rodentia")

transp_tab<-function(csv_ord){
  {
    tab_ord<-read.csv(csv_ord, header=T)
    list <- 1:1000
    BM <-rep("BM",length(list))
    BM<-as.data.frame(BM)
    Lambda<- rep("Lambda",length(list))
    Lambda<-as.data.frame(Lambda)
    Kappa <- rep("Kappa",length(list))
    Kappa<-as.data.frame(Kappa)
    BM<-data.frame(BM$BM, tab_ord$BM)
    Lambda<-data.frame(Lambda$Lambda, tab_ord$Lambda)
    Kappa<-data.frame(Kappa$Kappa, tab_ord$Kappa)
    nam<-c("Model", "estimate")
    colnames(BM)=nam
    colnames(Lambda)=nam
    colnames(Kappa)=nam
    ord_ok<-rbind(BM, Lambda, Kappa)}
  ord_ok}

setwd("C:/Users/raque/OneDrive - ufpr.br/Área de Trabalho/1_cap_final/Models_evol/mp_tables")
all_ok<-transp_tab("zall_spp.csv")
afro_ok<-transp_tab("Afrosoricida.csv")
car_ok<-transp_tab("Carnivora.csv")
ceta_ok<-transp_tab("Cetartiodactyla.csv")
chiro_ok<-transp_tab("Chiroptera.csv")
dasy_ok<-transp_tab("Dasyuromorphia.csv")
didel_ok<-transp_tab("Didelphimorphia.csv")
dipro_ok<-transp_tab("Diprotodontia.csv")
euli_ok<-transp_tab("Eulipotyphla.csv")
lago_ok<-transp_tab("Lagomorpha.csv")
prima_ok<-transp_tab("Primates.csv")
rode_ok<-transp_tab("Rodentia.csv")

by_n_a <- function(x,n) {
  seq(min(x), max(x), by=n)}


all_g<-ggplot(all_ok, aes(x=estimate)) +   
  geom_histogram(data=subset(all_ok, Model=="BM"), color = "#890276", fill="#890276", alpha=0.65, linewidth=0.2, breaks = by_n_a(all_ok$estimate, 1300))+
  geom_histogram(data=subset(all_ok, Model=="Lambda"), color= "#fea108", fill="#fea108", alpha=0.65, linewidth=0.2, breaks = by_n_a(all_ok$estimate, 1300))+
  geom_histogram(data=subset(all_ok, Model=="Kappa"), color="#287c89", fill="#287c89", alpha=0.65, linewidth=0.2, breaks = by_n_a(all_ok$estimate, 1300))+
  guides(colour = "none")+        #BM          #kappa       #lambda
  labs(x="", y="")+ 
  theme_minimal()+
  theme(axis.line = element_line(colour = "grey"),
        panel.border = element_blank(),
        legend.position = "none")+
  geom_hline(yintercept = -0, col="gray98", linewidth=0.45) +
  geom_vline(xintercept = ala$WN[1], linetype="dashed", linewidth=0.40)+
  geom_vline(xintercept = ala$Kappa_mod_aver[1], col="#287c89",linewidth=0.45)+
  geom_vline(xintercept = ala$BM_mod_aver[1], col="#890276",linewidth=0.45)+
  geom_vline(xintercept = ala$Lambda_mod_aver[1], col="#fea108",linewidth=0.45)
all_g

afro_g<-ggplot(afro_ok, aes(x=estimate)) +   
  geom_histogram(data=subset(afro_ok, Model=="BM"), color = "#890276", fill="#890276", alpha=0.65, linewidth=0.2, breaks = by_n_a(afro_ok$estimate, 13.2))+
  geom_histogram(data=subset(afro_ok, Model=="Lambda"), color= "#fea108", fill="#fea108", alpha=0.65, linewidth=0.2, breaks = by_n_a(afro_ok$estimate, 13.2))+
  geom_histogram(data=subset(afro_ok, Model=="Kappa"), color="#287c89", fill="#287c89", alpha=0.65, linewidth=0.2, breaks = by_n_a(afro_ok$estimate, 13.2))+
  guides(colour = "none")+        #BM          #kappa       #lambda
  labs(x="", y="")+ 
  theme_minimal()+
  theme(axis.line = element_line(colour = "grey"),
        panel.border = element_blank(),
        legend.position = "none")+
  geom_hline(yintercept = -0, col="gray98", linewidth=0.45) +
  geom_vline(xintercept = ala$WN[2], linetype="dashed", linewidth=0.40)+
  geom_vline(xintercept = ala$Kappa_mod_aver[2], col="#287c89",linewidth=0.45)+
  geom_vline(xintercept = ala$BM_mod_aver[2], col="#890276",linewidth=0.45)+
  geom_vline(xintercept = ala$Lambda_mod_aver[2], col="#fea108",linewidth=0.45)
afro_g


car_g<-ggplot(car_ok, aes(x=estimate)) +   
  geom_histogram(data=subset(car_ok, Model=="Lambda"), color= "#fea108", fill="#fea108", alpha=0.65, linewidth=0.2, breaks = by_n_a(car_ok$estimate, 42))+
  geom_histogram(data=subset(car_ok, Model=="BM"), color = "#890276", fill="#890276", alpha=0.65, linewidth=0.2, breaks = by_n_a(car_ok$estimate, 42))+
  geom_histogram(data=subset(car_ok, Model=="Kappa"), color="#287c89", fill="#287c89", alpha=0.65, linewidth=0.2, breaks = by_n_a(car_ok$estimate, 42))+
  guides(colour = "none")+        #BM          #kappa       #lambda
  labs(x="", y="")+ 
  theme_minimal()+
  theme(axis.line = element_line(colour = "grey"),
        panel.border = element_blank(),
        legend.position = "none")+
  geom_hline(yintercept = -0, col="gray98", linewidth=0.45) +
  geom_vline(xintercept = ala$WN[3], linetype="dashed", linewidth=0.40)+
  geom_vline(xintercept = ala$BM_mod_aver[3], col="#890276",linewidth=0.45)+
  geom_vline(xintercept = ala$Lambda_mod_aver[3], col="#fea108",linewidth=0.45)+
  geom_vline(xintercept = ala$Kappa_mod_aver[3], col="#287c89",linewidth=0.45)
car_g

ceta_g<-ggplot(ceta_ok, aes(x=estimate)) +   
  geom_histogram(data=subset(ceta_ok, Model=="Lambda"), color= "#fea108", fill="#fea108", alpha=0.65, linewidth=0.2, breaks = by_n_a(ceta_ok$estimate, 50))+
  geom_histogram(data=subset(ceta_ok, Model=="BM"), color = "#890276", fill="#890276", alpha=0.65, linewidth=0.2, breaks = by_n_a(ceta_ok$estimate, 50))+
  geom_histogram(data=subset(ceta_ok, Model=="Kappa"), color="#287c89", fill="#287c89", alpha=0.65, linewidth=0.2, breaks = by_n_a(ceta_ok$estimate, 50))+
  guides(colour = "none")+        #BM          #kappa       #lambda
  labs(x="", y="")+ 
  theme_minimal()+
  theme(axis.line = element_line(colour = "grey"),
        panel.border = element_blank(),
        legend.position = "none")+
  geom_hline(yintercept = -0, col="gray98", linewidth=0.45) +
  geom_vline(xintercept = ala$WN[4], linetype="dashed", linewidth=0.40)+
  geom_vline(xintercept = ala$BM_mod_aver[4], col="#890276",linewidth=0.45)+
  geom_vline(xintercept = ala$Lambda_mod_aver[4], col="#fea108",linewidth=0.45)+
  geom_vline(xintercept = ala$Kappa_mod_aver[4], col="#287c89",linewidth=0.45)
ceta_g

chiro_g<-ggplot(chiro_ok, aes(x=estimate)) +   
  geom_histogram(data=subset(chiro_ok, Model=="BM"), color = "#890276", fill="#890276", alpha=0.65, linewidth=0.2, breaks = by_n_a(chiro_ok$estimate, 150))+
  geom_histogram(data=subset(chiro_ok, Model=="Lambda"), color= "#fea108", fill="#fea108", alpha=0.65, linewidth=0.2, breaks = by_n_a(chiro_ok$estimate, 150))+
  geom_histogram(data=subset(chiro_ok, Model=="Kappa"), color="#287c89", fill="#287c89", alpha=0.65, linewidth=0.2, breaks = by_n_a(chiro_ok$estimate, 150))+
  guides(colour = "none")+        #BM          #kappa       #lambda
  labs(x="", y="")+ 
  theme_minimal()+
  theme(axis.line = element_line(colour = "grey"),
        panel.border = element_blank(),
        legend.position = "none")+
  geom_hline(yintercept = -0, col="gray98", linewidth=0.45) +
  geom_vline(xintercept = ala$WN[5], linetype="dashed", linewidth=0.40)+
  geom_vline(xintercept = ala$Kappa_mod_aver[5], col="#287c89",linewidth=0.45)+
  geom_vline(xintercept = ala$BM_mod_aver[5]+70, col="#890276",linewidth=0.45)+
  geom_vline(xintercept = ala$Lambda_mod_aver[5], col="#fea108",linewidth=0.45)
chiro_g

dasy_g<-ggplot(dasy_ok, aes(x=estimate)) +   
  geom_histogram(data=subset(dasy_ok, Model=="BM"), color = "#890276", fill="#890276", alpha=0.65, linewidth=0.2, breaks = by_n_a(dasy_ok$estimate, 12))+
  geom_histogram(data=subset(dasy_ok, Model=="Lambda"), color= "#fea108", fill="#fea108", alpha=0.65, linewidth=0.2, breaks = by_n_a(dasy_ok$estimate, 12))+
  geom_histogram(data=subset(dasy_ok, Model=="Kappa"), color="#287c89", fill="#287c89", alpha=0.65, linewidth=0.2, breaks = by_n_a(dasy_ok$estimate, 12))+
  guides(colour = "none")+        #BM          #kappa       #lambda
  labs(x="", y="")+ 
  theme_minimal()+
  theme(axis.line = element_line(colour = "grey"),
        panel.border = element_blank(),
        legend.position = "none")+
  geom_hline(yintercept = -0, col="gray98", linewidth=0.45) +
  geom_vline(xintercept = ala$WN[6], linetype="dashed", linewidth=0.40)+
  geom_vline(xintercept = ala$Kappa_mod_aver[6], col="#287c89",linewidth=0.45)+
  geom_vline(xintercept = ala$BM_mod_aver[6], col="#890276",linewidth=0.45)+
  geom_vline(xintercept = ala$Lambda_mod_aver[6], col="#fea108",linewidth=0.45)
dasy_g


###
didel_g<-ggplot(didel_ok, aes(x=estimate)) +   
  geom_histogram(data=subset(didel_ok, Model=="BM"), color = "#890276", fill="#890276", alpha=0.65, linewidth=0.2, breaks = by_n_a(didel_ok$estimate, 11.5))+
  geom_histogram(data=subset(didel_ok, Model=="Lambda"), color= "#fea108", fill="#fea108", alpha=0.65, linewidth=0.2, breaks = by_n_a(didel_ok$estimate, 11.5))+
  geom_histogram(data=subset(didel_ok, Model=="Kappa"), color="#287c89", fill="#287c89", alpha=0.65, linewidth=0.2, breaks = by_n_a(didel_ok$estimate, 11.5))+
  guides(colour = "none")+        #BM          #kappa       #lambda
  labs(x="", y="")+ 
  theme_minimal()+
  theme(axis.line = element_line(colour = "grey"),
        panel.border = element_blank(),
        legend.position = "none")+
  geom_hline(yintercept = -0, col="gray98", linewidth=0.45) +
  geom_vline(xintercept = ala$WN[7], linetype="dashed", linewidth=0.40)+
  geom_vline(xintercept = ala$Kappa_mod_aver[7], col="#287c89",linewidth=0.45)+
  geom_vline(xintercept = ala$BM_mod_aver[7], col="#890276",linewidth=0.45)+
  geom_vline(xintercept = ala$Lambda_mod_aver[7], col="#fea108",linewidth=0.45)
didel_g

dipro_g<-ggplot(dipro_ok, aes(x=estimate)) +   
  geom_histogram(data=subset(dipro_ok, Model=="BM"), color = "#890276", fill="#890276", alpha=0.65, linewidth=0.2, breaks = by_n_a(dipro_ok$estimate, 42))+
  geom_histogram(data=subset(dipro_ok, Model=="Lambda"), color= "#fea108", fill="#fea108", alpha=0.65, linewidth=0.2, breaks = by_n_a(dipro_ok$estimate, 42))+
  geom_histogram(data=subset(dipro_ok, Model=="Kappa"), color="#287c89", fill="#287c89", alpha=0.65, linewidth=0.2, breaks = by_n_a(dipro_ok$estimate, 42))+
  guides(colour = "none")+        #BM          #kappa       #lambda
  labs(x="", y="")+ 
  theme_minimal()+
  theme(axis.line = element_line(colour = "grey"),
        panel.border = element_blank(),
        legend.position = "none")+
  geom_hline(yintercept = -0, col="gray98", linewidth=0.45) +
  geom_vline(xintercept = ala$WN[8], linetype="dashed", linewidth=0.40)+
  geom_vline(xintercept = ala$Kappa_mod_aver[8], col="#287c89",linewidth=0.45)+
  geom_vline(xintercept = ala$BM_mod_aver[8], col="#890276",linewidth=0.45)+
  geom_vline(xintercept = ala$Lambda_mod_aver[8], col="#fea108",linewidth=0.45)
dipro_g


euli_g<-ggplot(euli_ok, aes(x=estimate)) +   
  geom_histogram(data=subset(euli_ok, Model=="BM"), color = "#890276", fill="#890276", alpha=0.65, linewidth=0.2, breaks = by_n_a(euli_ok$estimate, 120))+
  geom_histogram(data=subset(euli_ok, Model=="Lambda"), color= "#fea108", fill="#fea108", alpha=0.65, linewidth=0.2, breaks = by_n_a(euli_ok$estimate, 120))+
  geom_histogram(data=subset(euli_ok, Model=="Kappa"), color="#287c89", fill="#287c89", alpha=0.65, linewidth=0.2, breaks = by_n_a(euli_ok$estimate, 120))+
  guides(colour = "none")+        #BM          #kappa       #lambda
  labs(x="", y="")+ 
  theme_minimal()+
  theme(axis.line = element_line(colour = "grey"),
        panel.border = element_blank(),
        legend.position = "none")+
  geom_hline(yintercept = -0, col="gray98", linewidth=0.45) +
  geom_vline(xintercept = ala$WN[9], linetype="dashed", linewidth=0.40)+
  geom_vline(xintercept = ala$Kappa_mod_aver[9], col="#287c89",linewidth=0.45)+
  geom_vline(xintercept = ala$BM_mod_aver[9], col="#890276",linewidth=0.45)+
  geom_vline(xintercept = ala$Lambda_mod_aver[9], col="#fea108",linewidth=0.45)
euli_g

lago_g<-ggplot(lago_ok, aes(x=estimate)) +   
  geom_histogram(data=subset(lago_ok, Model=="BM"), color = "#890276", fill="#890276", alpha=0.65, linewidth=0.2, breaks = by_n_a(lago_ok$estimate, 13))+
  geom_histogram(data=subset(lago_ok, Model=="Lambda"), color= "#fea108", fill="#fea108", alpha=0.65, linewidth=0.2, breaks = by_n_a(lago_ok$estimate, 13))+
  geom_histogram(data=subset(lago_ok, Model=="Kappa"), color="#287c89", fill="#287c89", alpha=0.65, linewidth=0.2, breaks = by_n_a(lago_ok$estimate, 13))+
  guides(colour = "none")+        #BM          #kappa       #lambda
  labs(x="", y="")+ 
  theme_minimal()+
  theme(axis.line = element_line(colour = "grey"),
        panel.border = element_blank(),
        legend.position = "none")+
  geom_hline(yintercept = -0, col="gray98", linewidth=0.45) +
  geom_vline(xintercept = ala$WN[10], linetype="dashed", linewidth=0.40)+
  geom_vline(xintercept = ala$Kappa_mod_aver[10], col="#287c89",linewidth=0.45)+
  geom_vline(xintercept = ala$BM_mod_aver[10], col="#890276",linewidth=0.45)+
  geom_vline(xintercept = ala$Lambda_mod_aver[10], col="#fea108",linewidth=0.45)
lago_g

prima_g<-ggplot(prima_ok, aes(x=estimate)) +   
  geom_histogram(data=subset(prima_ok, Model=="BM"), color = "#890276", fill="#890276", alpha=0.65, linewidth=0.2, breaks = by_n_a(prima_ok$estimate, 120))+
  geom_histogram(data=subset(prima_ok, Model=="Lambda"), color= "#fea108", fill="#fea108", alpha=0.65, linewidth=0.2, breaks = by_n_a(prima_ok$estimate, 120))+
  geom_histogram(data=subset(prima_ok, Model=="Kappa"), color="#287c89", fill="#287c89", alpha=0.65, linewidth=0.2, breaks = by_n_a(prima_ok$estimate, 120))+
  guides(colour = "none")+        #BM          #kappa       #lambda
  labs(x="", y="")+ 
  theme_minimal()+
  theme(axis.line = element_line(colour = "grey"),
        panel.border = element_blank(),
        legend.position = "none")+
  geom_hline(yintercept = -0, col="gray98", linewidth=0.45) +
  geom_vline(xintercept = ala$WN[11], linetype="dashed", linewidth=0.40)+
  geom_vline(xintercept = ala$Kappa_mod_aver[11]-5, col="#287c89",linewidth=0.45)+
  geom_vline(xintercept = ala$BM_mod_aver[11], col="#890276",linewidth=0.45)+
  geom_vline(xintercept = ala$Lambda_mod_aver[11], col="#fea108",linewidth=0.45)
prima_g


rode_g<-ggplot(rode_ok, aes(x=estimate)) +   
  geom_histogram(data=subset(rode_ok, Model=="BM"), color = "#890276", fill="#890276", alpha=0.65, linewidth=0.2, breaks = by_n_a(rode_ok$estimate, 660))+
  geom_histogram(data=subset(rode_ok, Model=="Lambda"), color= "#fea108", fill="#fea108", alpha=0.65, linewidth=0.2, breaks = by_n_a(rode_ok$estimate, 660))+
  geom_histogram(data=subset(rode_ok, Model=="Kappa"), color="#287c89", fill="#287c89", alpha=0.65, linewidth=0.2, breaks = by_n_a(rode_ok$estimate, 660))+
  guides(colour = "none")+        #BM          #kappa       #lambda
  labs(x="", y="")+ 
  theme_minimal()+
  theme(axis.line = element_line(colour = "grey"),
        panel.border = element_blank(),
        legend.position = "none")+
  geom_hline(yintercept = -0, col="gray98", linewidth=0.45) +
  geom_vline(xintercept = ala$WN[12], linetype="dashed", linewidth=0.40)+
  geom_vline(xintercept = ala$Kappa_mod_aver[12], col="#287c89",linewidth=0.45)+
  geom_vline(xintercept = ala$BM_mod_aver[12], col="#890276",linewidth=0.45)+
  geom_vline(xintercept = ala$Lambda_mod_aver[12], col="#fea108",linewidth=0.45)
rode_g


tiff("C:/Users/raque/OneDrive - ufpr.br/Área de Trabalho/1_cap_final/Models_evol/Fig1_AIC_MP_transp_abs3.tiff",  width=9, height=7, units="in", res=800)
grid.arrange(all_g, afro_g, car_g, ceta_g, chiro_g, dasy_g, didel_g, dipro_g, euli_g, lago_g, prima_g, rode_g,  nrow = 4) 
dev.off()


