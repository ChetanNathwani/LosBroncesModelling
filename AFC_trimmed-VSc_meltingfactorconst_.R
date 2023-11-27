
#several Kds Fraction rows

library(praise)
library(cowplot)
praise()
library(tidyverse)
library(naniar)
library(dplyr)
library(IsoplotR) 
library(forcats)
library(ggpubr)
library(reshape2)


rm(list = ls())

Input <- read.csv("Input_VSc.csv") #input file for kds and C
Input <- Input %>% remove_rownames %>% column_to_rownames(var="X")
glimpse(Input)
tail(Input)


#best working

#variables

#fractionation increments
X_incr <- seq(1, 0.2, by=-0.025) 


#number of iterations
IterationMC <- c(1500000) #number
Run <- c(1:IterationMC) #counting for formatting

r_ratio <- (runif(IterationMC, min = 0, max = 0.6)) #range of assimilation



#C0 melt fixed or MC
C0_Ba <- runif(IterationMC, min= Input["Ba", "C0_med"], max=Input["Ba", "C0_med"])  #original melt concentration Ba
C0_Th <- runif(IterationMC,min= Input["Th", "C0_med"], max=Input["Th", "C0_med"])  #original melt concentration Th
C0_Y <- runif(IterationMC, min= Input["Y", "C0_med"], max=Input["Y", "C0_med"])  #original melt concentration Y
C0_Sr <- runif(IterationMC, min= C0_Y*(Input["Sr/Y", "C0_med"]), max=C0_Y*(Input["Sr/Y", "C0_med"]))  #original melt concentration Sr
C0_La <- runif(IterationMC, min= Input["La", "C0_med"], max=Input["La", "C0_med"])  #original melt concentration La
C0_Yb <- runif(IterationMC,min= Input["Yb", "C0_med"], max=Input["Yb", "C0_med"])  #original melt concentration Yb
C0_Dy <- runif(IterationMC, min= Input["Dy", "C0_med"], max=Input["Dy", "C0_med"])  #original melt concentration Dy
C0_V <- runif(IterationMC, min= Input["V", "C0_med"], max=Input["V", "C0_med"])  #original melt concentration V
C0_Sc <- runif(IterationMC, min= Input["Sc", "C0_med"], max=Input["Sc", "C0_med"])  #original melt concentration Dy


#Ca - concentration of assimilant
melting<- sample(0:1,IterationMC, replace=T)#deep or shallow (15kbar or <15kbar, Qian and Herrman, 2011)

Ca_Ba_o <- runif(IterationMC, min= Input["Ba", "Ca_min"], max=Input["Ba", "Ca_max"]) #assimilant concentration Ba
Ca_Th_o <- runif(IterationMC, min= Input["Th", "Ca_min"], max=Input["Th", "Ca_max"])  #assimilant concentration Th
Ca_Sr_o <- runif(IterationMC, min= Input["Sr", "Ca_min"], max=Input["Sr", "Ca_max"]) #assimilant concentration Sr
Ca_Y_o <- runif(IterationMC, min= Input["Y", "Ca_min"], max=Input["Y", "Ca_max"]) #assimilant concentration Y
Ca_La_o <- runif(IterationMC, min= Input["La", "Ca_min"], max=Input["La", "Ca_max"])  #assimilant concentration La
Ca_Yb_o <- runif(IterationMC, min= Input["Yb", "Ca_min"], max=Input["Yb", "Ca_max"]) #assimilant concentration Yb
Ca_Dy_o <- runif(IterationMC, min= Input["Dy", "Ca_min"], max=Input["Dy", "Ca_max"]) #assimilant concentration Dy
Ca_Sc_o <- runif(IterationMC, min= Input["Sc", "Ca_min"], max=Input["Sc", "Ca_max"]) #assimilant concentration Sc
Ca_V_o <- runif(IterationMC, min= Input["V", "Ca_min"], max=Input["V", "Ca_max"]) #assimilant concentration V

#melting coefficient Qian and Herrmann
Ca_Ba <- 1.95*Ca_Ba_o #assimilant concentration Ba
Ca_Th <- 2.07*Ca_Th_o  #assimilant concentration Th
Ca_Sr <- 1.37*Ca_Sr_o #assimilant concentration Sr
Ca_Y <- 0.74*Ca_Y_o #assimilant concentration Y
Ca_La <- 1.73*Ca_La_o  #assimilant concentration La
Ca_Yb <- 0.66*Ca_Yb_o #assimilant concentration Yb
Ca_Dy <- 0.71*Ca_Dy_o #assimilant concentration Dy
Ca_Sc <- 0.43*Ca_Sc_o #assimilant concentration Dy
Ca_V <- 0.23*Ca_V_o #assimilant concentration Dy

#Modal mineral abundance fixed or MC
Xi_ApaMC <- runif(IterationMC, min=0.00, max=0.06) # Apatite
Xi_MgtMC <- runif(IterationMC, min=0.00, max=0.06) # magneite
Xi_GrtMC <- sample(0:1,IterationMC, replace=T)*runif(IterationMC, min=0.00, max=0.00) # garnet - bimodal decision if grt or not and then for the ones with garnet random number
Xi_RutMC <- runif(IterationMC, min=0.00, max=0.06) # rutile
Xi_AmpMC <- runif(IterationMC, min=0.0, max=0.72) #Amphibole
Xi_PlgMC <- runif(IterationMC, min=0.0, max= 1-Xi_ApaMC-
                    Xi_RutMC-Xi_GrtMC-Xi_MgtMC-Xi_AmpMC) # Cpx
Xi_CpxMC<- runif(IterationMC, min=0.0, max=1-Xi_ApaMC-
                    Xi_RutMC-Xi_GrtMC-Xi_MgtMC-Xi_AmpMC-Xi_PlgMC) # Opx
 Xi_OpxMC<- 1-Xi_ApaMC-
  Xi_RutMC-Xi_GrtMC-Xi_MgtMC-Xi_AmpMC-Xi_PlgMC-Xi_CpxMC #plg calculated from the other ones to reach 100


#Partition coefficients fixed or MC



#Barium
KiBa_AmpMC <- runif(IterationMC, min= Input["Ba", "Ki_Amp_min"], max=Input["Ba", "Ki_Amp_max"]) #Partition coefficient of Sr in Olivine
KiBa_CpxMC <- runif(IterationMC, min= Input["Ba", "Ki_Cpx_min"], max=Input["Ba", "Ki_Cpx_max"]) #Partition coefficient of Sr in cpx
KiBa_OpxMC <- runif(IterationMC, min= Input["Ba", "Ki_Opx_min"], max=Input["Ba", "Ki_Opx_max"]) #Partition coefficient of Sr in cpx
KiBa_ApaMC <- runif(IterationMC, min= Input["Ba", "Ki_Apa_min"], max=Input["Ba", "Ki_Apa_max"]) #Partition coefficient of Sr in cpx
KiBa_MgtMC <- runif(IterationMC, min= Input["Ba", "Ki_Mgt_min"], max=Input["Ba", "Ki_Mgt_max"]) #Partition coefficient of Sr in cpx
KiBa_GrtMC <- runif(IterationMC, min= Input["Ba", "Ki_Grt_min"], max=Input["Ba", "Ki_Grt_max"]) #Partition coefficient of Sr in cpx
KiBa_RutMC <- runif(IterationMC, min= Input["Ba", "Ki_Rut_min"], max=Input["Ba", "Ki_Rut_max"]) #Partition coefficient of Sr in cpx
KiBa_PlgMC <- runif(IterationMC, min= Input["Ba", "Ki_Plg_min"], max=Input["Ba", "Ki_Plg_max"]) #Partition coefficient of Sr in plg
D_BaMC <- (Xi_AmpMC*KiBa_AmpMC+
             Xi_CpxMC*KiBa_CpxMC+
             Xi_OpxMC*KiBa_OpxMC+
             Xi_ApaMC*KiBa_ApaMC+
             Xi_MgtMC*KiBa_MgtMC+
             Xi_GrtMC*KiBa_GrtMC+
             Xi_RutMC*KiBa_RutMC+
             Xi_PlgMC*KiBa_PlgMC) #Bulk partition coefficients



#Thorium # calculated with Ba/Th ratio from Ba
KiTh_AmpMC <- runif(IterationMC, min= KiBa_AmpMC / Input["Ba/Th", "Ki_Amp_max"], max=KiBa_AmpMC / Input["Ba/Th", "Ki_Amp_min"]) #Partition coefficient of Sr in Olivine
KiTh_CpxMC <- runif(IterationMC, min= KiBa_CpxMC / Input["Ba/Th", "Ki_Cpx_max"], max=KiBa_CpxMC / Input["Ba/Th", "Ki_Cpx_min"]) #Partition coefficient of Sr in cpx
KiTh_OpxMC <- runif(IterationMC, min= KiBa_OpxMC / Input["Ba/Th", "Ki_Opx_max"], max=KiBa_OpxMC / Input["Ba/Th", "Ki_Opx_min"]) #Partition coefficient of Sr in cpx
KiTh_ApaMC <- runif(IterationMC, min= Input["Th", "Ki_Apa_min"], max=Input["Th", "Ki_Apa_max"]) #Partition coefficient of Sr in cpx
KiTh_MgtMC <- runif(IterationMC, min= Input["Th", "Ki_Mgt_min"], max=Input["Th", "Ki_Mgt_max"]) #Partition coefficient of Sr in cpx
KiTh_GrtMC <- runif(IterationMC, min= KiBa_GrtMC / Input["Ba/Th", "Ki_Grt_max"], max=KiBa_GrtMC / Input["Ba/Th", "Ki_Grt_min"]) #Partition coefficient of Sr in cpx
KiTh_RutMC <- runif(IterationMC, min= KiBa_RutMC / Input["Ba/Th", "Ki_Rut_max"], max=KiBa_RutMC / Input["Ba/Th", "Ki_Rut_min"])#Partition coefficient of Sr in cpx
KiTh_PlgMC <- runif(IterationMC, min= KiBa_PlgMC / Input["Ba/Th", "Ki_Plg_max"], max=KiBa_PlgMC / Input["Ba/Th", "Ki_Plg_min"]) #Partition coefficient of Sr in plg
D_ThMC <- (Xi_AmpMC*KiTh_AmpMC+
             Xi_CpxMC*KiTh_CpxMC+
             Xi_OpxMC*KiTh_OpxMC+
             Xi_ApaMC*KiTh_ApaMC+
             Xi_MgtMC*KiTh_MgtMC+
             Xi_GrtMC*KiTh_GrtMC+
             Xi_RutMC*KiTh_RutMC+
             Xi_PlgMC*KiTh_PlgMC) #Bulk partition coefficients


#Srontium
KiSr_AmpMC <- runif(IterationMC, min= Input["Sr", "Ki_Amp_min"], max=Input["Sr", "Ki_Amp_max"]) #Partition coefficient of Sr in Olivine
KiSr_CpxMC <- runif(IterationMC, min= Input["Sr", "Ki_Cpx_min"], max=Input["Sr", "Ki_Cpx_max"]) #Partition coefficient of Sr in cpx
KiSr_OpxMC <- runif(IterationMC, min= Input["Sr", "Ki_Opx_min"], max=Input["Sr", "Ki_Opx_max"]) #Partition coefficient of Sr in cpx
KiSr_ApaMC <- runif(IterationMC, min= Input["Sr", "Ki_Apa_min"], max=Input["Sr", "Ki_Apa_max"]) #Partition coefficient of Sr in cpx
KiSr_MgtMC <- runif(IterationMC, min= Input["Sr", "Ki_Mgt_min"], max=Input["Sr", "Ki_Mgt_max"]) #Partition coefficient of Sr in cpx
KiSr_GrtMC <- runif(IterationMC, min= Input["Sr", "Ki_Grt_min"], max=Input["Sr", "Ki_Grt_max"])#Partition coefficient of Sr in cpx
KiSr_RutMC <- runif(IterationMC, min= Input["Sr", "Ki_Rut_min"], max=Input["Sr", "Ki_Rut_max"]) #Partition coefficient of Sr in cpx
KiSr_PlgMC <- runif(IterationMC, min= Input["Sr", "Ki_Plg_min"], max=Input["Sr", "Ki_Plg_max"]) #Partition coefficient of Sr in plg
D_SrMC <- (Xi_AmpMC*KiSr_AmpMC+
             Xi_CpxMC*KiSr_CpxMC+
             Xi_OpxMC*KiSr_OpxMC+
             Xi_ApaMC*KiSr_ApaMC+
             Xi_MgtMC*KiSr_MgtMC+
             Xi_GrtMC*KiSr_GrtMC+
             Xi_RutMC*KiSr_RutMC+
             Xi_PlgMC*KiSr_PlgMC) #Bulk partition coefficients

#Yttrium
KiY_AmpMC <- runif(IterationMC, min= KiSr_AmpMC / Input["Sr/Y", "Ki_Amp_max"], max=KiSr_AmpMC / Input["Sr/Y", "Ki_Amp_min"]) #Partition coefficient of Sr in Olivine
KiY_CpxMC <- runif(IterationMC, min= KiSr_CpxMC / Input["Sr/Y", "Ki_Cpx_max"], max=KiSr_CpxMC / Input["Sr/Y", "Ki_Cpx_min"]) #Partition coefficient of Sr in cpx
KiY_OpxMC <- runif(IterationMC, min= KiSr_OpxMC / Input["Sr/Y", "Ki_Opx_max"], max=KiSr_OpxMC / Input["Sr/Y", "Ki_Opx_min"]) #Partition coefficient of Sr in cpx
KiY_ApaMC <- runif(IterationMC, min= KiSr_ApaMC / Input["Sr/Y", "Ki_Apa_max"], max=KiSr_ApaMC / Input["Sr/Y", "Ki_Apa_min"]) #Partition coefficient of Sr in cpx
KiY_MgtMC <- runif(IterationMC, min= KiSr_MgtMC / Input["Sr/Y", "Ki_Mgt_max"], max=KiSr_MgtMC / Input["Sr/Y", "Ki_Mgt_min"]) #Partition coefficient of Sr in cpx
KiY_GrtMC <- runif(IterationMC, min= KiSr_GrtMC / Input["Sr/Y", "Ki_Grt_max"], max=KiSr_GrtMC / Input["Sr/Y", "Ki_Grt_min"]) #Partition coefficient of Sr in cpx
KiY_RutMC <- runif(IterationMC, min= KiSr_RutMC / Input["Sr/Y", "Ki_Rut_max"], max=KiSr_RutMC / Input["Sr/Y", "Ki_Rut_min"]) #Partition coefficient of Sr in cpx
KiY_PlgMC <- runif(IterationMC, min= KiSr_PlgMC / Input["Sr/Y", "Ki_Plg_max"], max=KiSr_PlgMC / Input["Sr/Y", "Ki_Plg_min"]) #Partition coefficient of Sr in plg
D_YMC <- (Xi_AmpMC*KiY_AmpMC+
             Xi_CpxMC*KiY_CpxMC+
             Xi_OpxMC*KiY_OpxMC+
             Xi_ApaMC*KiY_ApaMC+
             Xi_MgtMC*KiY_MgtMC+
             Xi_GrtMC*KiY_GrtMC+
             Xi_RutMC*KiY_RutMC+
             Xi_PlgMC*KiY_PlgMC) #Bulk partition coefficients

#Lanthanum
KiLa_AmpMC <- runif(IterationMC, min= Input["La", "Ki_Amp_min"], max=Input["La", "Ki_Amp_max"]) #Partition coefficient of Sr in Olivine
KiLa_CpxMC <- runif(IterationMC, min= Input["La", "Ki_Cpx_min"], max=Input["La", "Ki_Cpx_max"]) #Partition coefficient of Sr in cpx
KiLa_OpxMC <- runif(IterationMC, min= Input["La", "Ki_Opx_min"], max=Input["La", "Ki_Opx_max"]) #Partition coefficient of Sr in cpx
KiLa_ApaMC <- runif(IterationMC, min= Input["La", "Ki_Apa_min"], max=Input["La", "Ki_Apa_max"]) #Partition coefficient of Sr in cpx
KiLa_MgtMC <- runif(IterationMC, min= Input["La", "Ki_Mgt_min"], max=Input["La", "Ki_Mgt_max"]) #Partition coefficient of Sr in cpx
KiLa_GrtMC <- runif(IterationMC, min= Input["La", "Ki_Grt_min"], max=Input["La", "Ki_Grt_max"]) #Partition coefficient of Sr in cpx
KiLa_RutMC <- runif(IterationMC, min= Input["La", "Ki_Rut_min"], max=Input["La", "Ki_Rut_max"]) #Partition coefficient of Sr in cpx
KiLa_PlgMC <- runif(IterationMC, min= Input["La", "Ki_Plg_min"], max=Input["La", "Ki_Plg_max"]) #Partition coefficient of Sr in cpx
D_LaMC <- (Xi_AmpMC*KiLa_AmpMC+
            Xi_CpxMC*KiLa_CpxMC+
            Xi_OpxMC*KiLa_OpxMC+
            Xi_ApaMC*KiLa_ApaMC+
            Xi_MgtMC*KiLa_MgtMC+
            Xi_GrtMC*KiLa_GrtMC+
            Xi_RutMC*KiLa_RutMC+
            Xi_PlgMC*KiLa_PlgMC) #Bulk partition coefficients


#Yterbium
KiYb_AmpMC <- runif(IterationMC, min= Input["Yb", "Ki_Amp_min"], max=Input["Yb", "Ki_Amp_max"]) #Partition coefficient of Sr in Olivine
KiYb_CpxMC <- runif(IterationMC, min= Input["Yb", "Ki_Cpx_min"], max=Input["Yb", "Ki_Cpx_max"]) #Partition coefficient of Sr in cpx
KiYb_OpxMC <- runif(IterationMC, min= Input["Yb", "Ki_Opx_min"], max=Input["Yb", "Ki_Opx_max"]) #Partition coefficient of Sr in cpx
KiYb_ApaMC <- runif(IterationMC, min= Input["Yb", "Ki_Apa_min"], max=Input["Yb", "Ki_Apa_max"]) #Partition coefficient of Sr in cpx
KiYb_MgtMC <- runif(IterationMC, min= Input["Yb", "Ki_Mgt_min"], max=Input["Yb", "Ki_Mgt_max"]) #Partition coefficient of Sr in cpx
KiYb_GrtMC <- runif(IterationMC, min= Input["Yb", "Ki_Grt_min"], max=Input["Yb", "Ki_Grt_max"]) #Partition coefficient of Sr in cpx
KiYb_RutMC <- runif(IterationMC, min= Input["Yb", "Ki_Rut_min"], max=Input["Yb", "Ki_Rut_max"]) #Partition coefficient of Sr in cpx
KiYb_PlgMC <- runif(IterationMC, min= Input["Yb", "Ki_Plg_min"], max=Input["Yb", "Ki_Plg_max"]) #Partition coefficient of Sr in plg
D_YbMC <- (Xi_AmpMC*KiYb_AmpMC+
             Xi_CpxMC*KiYb_CpxMC+
             Xi_OpxMC*KiYb_OpxMC+
             Xi_ApaMC*KiYb_ApaMC+
             Xi_MgtMC*KiYb_MgtMC+
             Xi_GrtMC*KiYb_GrtMC+
             Xi_RutMC*KiYb_RutMC+
             Xi_PlgMC*KiYb_PlgMC) #Bulk partition coefficients



#Dysprosium
KiDy_AmpMC <- runif(IterationMC, min= Input["Dy", "Ki_Amp_min"], max=Input["Dy", "Ki_Amp_max"]) #Partition coefficient of Sr in Olivine
KiDy_CpxMC <- runif(IterationMC, min= Input["Dy", "Ki_Cpx_min"], max=Input["Dy", "Ki_Cpx_max"]) #Partition coefficient of Sr in cpx
KiDy_OpxMC <- runif(IterationMC, min= Input["Dy", "Ki_Opx_min"], max=Input["Dy", "Ki_Opx_max"]) #Partition coefficient of Sr in cpx
KiDy_ApaMC <- runif(IterationMC, min= Input["Dy", "Ki_Apa_min"], max=Input["Dy", "Ki_Apa_max"]) #Partition coefficient of Sr in cpx
KiDy_MgtMC <- runif(IterationMC, min= Input["Dy", "Ki_Mgt_min"], max=Input["Dy", "Ki_Mgt_max"]) #Partition coefficient of Sr in cpx
KiDy_GrtMC <- runif(IterationMC, min= Input["Dy", "Ki_Grt_min"], max=Input["Dy", "Ki_Grt_max"]) #Partition coefficient of Sr in cpx
KiDy_RutMC <- runif(IterationMC, min= Input["Dy", "Ki_Rut_min"], max=Input["Dy", "Ki_Rut_max"]) #Partition coefficient of Sr in cpx
KiDy_PlgMC <- runif(IterationMC, min= Input["Dy", "Ki_Plg_min"], max=Input["Dy", "Ki_Plg_max"]) #Partition coefficient of Sr in plg
D_DyMC <- (Xi_AmpMC*KiDy_AmpMC+
             Xi_CpxMC*KiDy_CpxMC+
             Xi_OpxMC*KiDy_OpxMC+
             Xi_ApaMC*KiDy_ApaMC+
             Xi_MgtMC*KiDy_MgtMC+
             Xi_GrtMC*KiDy_GrtMC+
             Xi_RutMC*KiDy_RutMC+
             Xi_PlgMC*KiDy_PlgMC) #Bulk partition coefficients

#Scandium
KiSc_AmpMC <- runif(IterationMC, min= Input["Sc", "Ki_Amp_min"], max=Input["Sc", "Ki_Amp_max"]) #Partition coefficient of Sr in Olivine
KiSc_CpxMC <- runif(IterationMC, min= Input["Sc", "Ki_Cpx_min"], max=Input["Sc", "Ki_Cpx_max"]) #Partition coefficient of Sr in cpx
KiSc_OpxMC <- runif(IterationMC, min= Input["Sc", "Ki_Opx_min"], max=Input["Sc", "Ki_Opx_max"]) #Partition coefficient of Sr in cpx
KiSc_ApaMC <- runif(IterationMC, min= Input["Sc", "Ki_Apa_min"], max=Input["Sc", "Ki_Apa_max"]) #Partition coefficient of Sr in cpx
KiSc_MgtMC <- runif(IterationMC, min= Input["Sc", "Ki_Mgt_min"], max=Input["Sc", "Ki_Mgt_max"]) #Partition coefficient of Sr in cpx
KiSc_GrtMC <- runif(IterationMC, min= Input["Sc", "Ki_Grt_min"], max=Input["Sc", "Ki_Grt_max"]) #Partition coefficient of Sr in cpx
KiSc_RutMC <- runif(IterationMC, min= Input["Sc", "Ki_Rut_min"], max=Input["Sc", "Ki_Rut_max"]) #Partition coefficient of Sr in cpx
KiSc_PlgMC <- runif(IterationMC, min= Input["Sc", "Ki_Plg_min"], max=Input["Sc", "Ki_Plg_max"]) #Partition coefficient of Sr in plg
D_ScMC <- (Xi_AmpMC*KiSc_AmpMC+
             Xi_CpxMC*KiSc_CpxMC+
             Xi_OpxMC*KiSc_OpxMC+
             Xi_ApaMC*KiSc_ApaMC+
             Xi_MgtMC*KiSc_MgtMC+
             Xi_GrtMC*KiSc_GrtMC+
             Xi_RutMC*KiSc_RutMC+
             Xi_PlgMC*KiSc_PlgMC) #Bulk partition coefficients

#Vanadium
KiV_AmpMC <- runif(IterationMC, min= KiSc_AmpMC * Input["V/Sc", "Ki_Amp_min"], max=KiSc_AmpMC * Input["V/Sc", "Ki_Amp_max"]) #Partition coefficient of Sr in Olivine
KiV_CpxMC <- runif(IterationMC, min= KiSc_CpxMC * Input["V/Sc", "Ki_Cpx_min"], max=KiSc_CpxMC * Input["V/Sc", "Ki_Cpx_max"]) #Partition coefficient of Sr in cpx
KiV_OpxMC <- runif(IterationMC, min= KiSc_OpxMC * Input["V/Sc", "Ki_Opx_min"], max=KiSc_OpxMC * Input["V/Sc", "Ki_Opx_max"]) #Partition coefficient of Sr in cpx
KiV_ApaMC <- runif(IterationMC, min= KiSc_ApaMC * Input["V/Sc", "Ki_Apa_min"], max=KiSc_ApaMC * Input["V/Sc", "Ki_Apa_max"]) #Partition coefficient of Sr in cpx
KiV_MgtMC <- runif(IterationMC, min= KiSc_MgtMC * Input["V/Sc", "Ki_Mgt_min"], max=KiSc_MgtMC * Input["V/Sc", "Ki_Mgt_max"]) #Partition coefficient of Sr in cpx
KiV_GrtMC <- runif(IterationMC, min= KiSc_GrtMC * Input["V/Sc", "Ki_Grt_min"], max=KiSc_GrtMC * Input["V/Sc", "Ki_Grt_max"]) #Partition coefficient of Sr in cpx
KiV_RutMC <- runif(IterationMC, min= KiSc_RutMC * Input["V/Sc", "Ki_Rut_min"], max=KiSc_RutMC * Input["V/Sc", "Ki_Rut_max"]) #Partition coefficient of Sr in cpx
KiV_PlgMC <- runif(IterationMC, min= KiSc_PlgMC * Input["V/Sc", "Ki_Plg_min"], max=KiSc_PlgMC * Input["V/Sc", "Ki_Plg_max"]) #Partition coefficient of Sr in plg
D_VMC <- (Xi_AmpMC*KiY_AmpMC+
            Xi_CpxMC*KiY_CpxMC+
            Xi_OpxMC*KiY_OpxMC+
            Xi_ApaMC*KiY_ApaMC+
            Xi_MgtMC*KiY_MgtMC+
            Xi_GrtMC*KiY_GrtMC+
            Xi_RutMC*KiY_RutMC+
            Xi_PlgMC*KiY_PlgMC) #Bulk partition coefficients

#Columns for column names and formatting
Info.mx <- matrix(c(Run, r_ratio,
                   Xi_AmpMC, Xi_CpxMC, Xi_OpxMC,Xi_ApaMC, Xi_MgtMC, Xi_GrtMC, Xi_RutMC, Xi_PlgMC, 
                   KiBa_AmpMC, KiBa_CpxMC, KiBa_OpxMC, KiBa_ApaMC, KiBa_MgtMC, KiBa_GrtMC, KiBa_RutMC,KiBa_PlgMC,
                   KiTh_AmpMC, KiTh_CpxMC, KiTh_OpxMC, KiTh_ApaMC, KiTh_MgtMC, KiTh_GrtMC, KiTh_RutMC, KiTh_PlgMC,
                   KiSr_AmpMC, KiSr_CpxMC, KiSr_OpxMC, KiSr_ApaMC, KiSr_MgtMC, KiSr_GrtMC, KiSr_RutMC, KiSr_PlgMC,
                   KiY_AmpMC, KiY_CpxMC, KiY_OpxMC, KiY_ApaMC, KiY_MgtMC, KiY_GrtMC, KiY_RutMC, KiY_PlgMC,
                   KiLa_AmpMC, KiLa_CpxMC, KiLa_OpxMC, KiLa_ApaMC, KiLa_MgtMC, KiLa_GrtMC, KiLa_RutMC, KiLa_PlgMC,
                   KiYb_AmpMC, KiYb_CpxMC, KiYb_OpxMC, KiYb_ApaMC, KiYb_MgtMC, KiYb_GrtMC, KiYb_RutMC, KiYb_PlgMC,
                   KiDy_AmpMC, KiDy_CpxMC, KiDy_OpxMC, KiDy_ApaMC, KiDy_MgtMC, KiDy_GrtMC, KiDy_RutMC, KiDy_PlgMC,
                KiSc_AmpMC, KiSc_CpxMC, KiSc_OpxMC, KiSc_ApaMC, KiSc_MgtMC, KiSc_GrtMC, KiSc_RutMC, KiSc_PlgMC,
                KiV_AmpMC, KiV_CpxMC, KiV_OpxMC, KiV_ApaMC, KiV_MgtMC, KiV_GrtMC, KiV_RutMC, KiV_PlgMC,
                   Ca_Ba_o, 
                   Ca_Th_o, 
                   Ca_Sr_o,  
                   Ca_Y_o, 
                   Ca_La_o, 
                   Ca_Yb_o, 
                   Ca_Dy_o, 
                Ca_Sc_o, 
                Ca_V_o), ncol = 91) #columns for the Sr calculations
colnames(Info.mx) <- c("Run", "R_Assim",
                      "Xi_AmpMC", "Xi_CpxMC", "Xi_OpxMC","Xi_ApaMC", "Xi_MgtMC", "Xi_GrtMC", "Xi_RutMC", "Xi_PlgMC", 
                      "KiBa_AmpMC", "KiBa_CpxMC", "KiBa_OpxMC", "KiBa_ApaMC", "KiBa_MgtMC", "KiBa_GrtMC", "KiBa_RutMC","KiBa_PlgMC",
                      "KiTh_AmpMC", "KiTh_CpxMC", "KiTh_OpxMC", "KiTh_ApaMC", "KiTh_MgtMC", "KiTh_GrtMC", "KiTh_RutMC", "KiTh_PlgMC",
                      "KiSr_AmpMC", "KiSr_CpxMC", "KiSr_OpxMC", "KiSr_ApaMC", "KiSr_MgtMC", "KiSr_GrtMC", "KiSr_RutMC", "KiSr_PlgMC",
                      "KiY_AmpMC", "KiY_CpxMC", "KiY_OpxMC", "KiY_ApaMC", "KiY_MgtMC", "KiY_GrtMC", "KiY_RutMC", "KiY_PlgMC",
                      "KiLa_AmpMC", "KiLa_CpxMC", "KiLa_OpxMC", "KiLa_ApaMC", "KiLa_MgtMC", "KiLa_GrtMC", "KiLa_RutMC", "KiLa_PlgMC",
                      "KiYb_AmpMC", "KiYb_CpxMC", "KiYb_OpxMC", "KiYb_ApaMC", "KiYb_MgtMC", "KiYb_GrtMC", "KiYb_RutMC", "KiYb_PlgMC",
                      "KiDy_AmpMC", "KiDy_CpxMC", "KiDy_OpxMC", "KiDy_ApaMC", "KiDy_MgtMC", "KiDy_GrtMC", "KiDy_RutMC", "KiDy_PlgMC",
                    "KiSc_AmpMC", "KiSc_CpxMC", "KiSc_OpxMC", "KiSc_ApaMC", "KiSc_MgtMC", "KiSc_GrtMC", "KiSc_RutMC", "KiSc_PlgMC",
                    "KiV_AmpMC", "KiV_CpxMC", "KiV_OpxMC", "KiV_ApaMC", "KiV_MgtMC", "KiV_GrtMC", "KiV_RutMC", "KiV_PlgMC",
                    "Ca_Ba", 
                      "Ca_Th", 
                      "Ca_Sr", 
                      "Ca_Y", 
                      "Ca_La", 
                      "Ca_Yb", 
                      "Ca_Dy", 
                    "Ca_Sc", 
                    "Ca_V")
Info.df <- as.data.frame((Info.mx))


#z-component - fixed for each run. if whole fractionation trends for different r_ratio need other formula
z_Ba <-((r_ratio+D_BaMC-1)/(r_ratio-1))
z_Th <-((r_ratio+D_ThMC-1)/(r_ratio-1))
z_Sr <-((r_ratio+D_SrMC-1)/(r_ratio-1))
z_Y <-((r_ratio+D_YMC-1)/(r_ratio-1))
z_La <-((r_ratio+D_LaMC-1)/(r_ratio-1))
z_Yb <-((r_ratio+D_YbMC-1)/(r_ratio-1))
z_Dy <-((r_ratio+D_DyMC-1)/(r_ratio-1))
z_Sc <-((r_ratio+D_ScMC-1)/(r_ratio-1))
z_V <-((r_ratio+D_VMC-1)/(r_ratio-1))

#Clear work space
rm(list = ls()[grepl("Ki", ls())])
rm(list = ls()[grepl("Xi", ls())])

#MC modelling
#function applying the calculation to all C0s and bulk partition coefficients against all increments
AFC_D_BaMC<-outer(1:length(D_BaMC),1:length(X_incr),function(i,j) C0_Ba[i]* ((X_incr[j]^(-z_Ba[i]))+
                                                                               ((r_ratio[i]/(r_ratio[i]-1))*Ca_Ba[i]/(z_Ba*C0_Ba)*
                                                                                  (1-(X_incr[j]^(-z_Ba[i])))))) 
colnames(AFC_D_BaMC) <- X_incr #name increments
AFC_BaMC <- head(cbind(Run, AFC_D_BaMC), length(AFC_D_BaMC)) #add run information
AFC_BaMC.df <- as.data.frame((AFC_BaMC)) #make into data frame
AFC_BaMC.df <- melt(AFC_BaMC.df,id.vars = c(1),variable.name = "Fraction", value.name = "Ba_ppm") #transfer into long format
AFC_BaMC.df <- transform(AFC_BaMC.df, Fraction = as.numeric(as.character(Fraction)))
AFC_BaMC.df <- AFC_BaMC.df%>%
  mutate(ID_Ba=Run+Fraction)
Ba_data <- data.matrix(AFC_BaMC.df, rownames.force = NA)

rm(list = ls()[grepl("AFC", ls())]) #deletes eveythin called AFC
rm(list = ls()[grepl("Ba$", ls())]) #deletes everything ending with Ba

#same for Th as for Ba
AFC_D_ThMC<-outer(1:length(D_ThMC), 1:length(X_incr),function(i,j) C0_Th[i]* ((X_incr[j]^(-z_Th[i]))+
                                                                                ((r_ratio[i]/(r_ratio[i]-1))*Ca_Th[i]/(z_Th*C0_Th)*
                                                                                   (1-(X_incr[j]^(-z_Th[i])))))) 
colnames(AFC_D_ThMC) <- X_incr
AFC_ThMC <- head(cbind(Run, AFC_D_ThMC), length(AFC_D_ThMC)) #add run information
AFC_ThMC.df <- as.data.frame((AFC_ThMC)) #make into data frame
AFC_ThMC.df <- melt(AFC_ThMC.df,id.vars = c(1),variable.name = "FractionT_Th", value.name = "Th_ppm")%>%
  rename(RunsTh = Run )#transfer into long format T_important for later formatting
AFC_ThMC.df <- transform(AFC_ThMC.df, FractionT_Th = as.numeric(as.character(FractionT_Th))) #into numerics to be able to convert into matrix
AFC_ThMC.df <- AFC_ThMC.df%>%
  mutate(ID_Th=RunsTh+FractionT_Th)%>% 
  select(-starts_with("Runs"))%>%
  select(-starts_with("FractionT"))
Th_data <- data.matrix(AFC_ThMC.df, rownames.force = NA)

rm(list = ls()[grepl("AFC", ls())])
rm(list = ls()[grepl("Th$", ls())])


#same for Sr as for Ba
AFC_D_SrMC<-outer(1:length(D_SrMC), 1:length(X_incr),function(i,j) C0_Sr[i]* ((X_incr[j]^(-z_Sr[i]))+
                                                                                ((r_ratio[i]/(r_ratio[i]-1))*Ca_Sr[i]/(z_Sr*C0_Sr)*
                                                                                   (1-(X_incr[j]^(-z_Sr[i])))))) 
colnames(AFC_D_SrMC) <- X_incr
AFC_SrMC <- head(cbind(Run, AFC_D_SrMC), length(AFC_D_SrMC)) #add run information
AFC_SrMC.df <- as.data.frame((AFC_SrMC)) #make into data frame
AFC_SrMC.df <- melt(AFC_SrMC.df,id.vars = c(1),variable.name = "FractionT_Sr", value.name = "Sr_ppm") %>%
  rename(RunsSr = Run )#transfer into long format
AFC_SrMC.df <- transform(AFC_SrMC.df, FractionT_Sr = as.numeric(as.character(FractionT_Sr)))
AFC_SrMC.df <- AFC_SrMC.df%>%
  mutate(ID_Sr=RunsSr+FractionT_Sr)%>% 
  select(-starts_with("Runs"))%>%
  select(-starts_with("FractionT"))
Sr_data <- data.matrix(AFC_SrMC.df, rownames.force = NA)

rm(list = ls()[grepl("AFC", ls())])
rm(list = ls()[grepl("Sr$", ls())])


#same for Y as for Ba
AFC_D_YMC<-outer(1:length(D_YMC), 1:length(X_incr),function(i,j) C0_Y[i]* ((X_incr[j]^(-z_Y[i]))+
                                                                             ((r_ratio[i]/(r_ratio[i]-1))*Ca_Y[i]/(z_Y*C0_Y)*
                                                                                (1-(X_incr[j]^(-z_Y[i])))))) 
colnames(AFC_D_YMC) <- X_incr
AFC_YMC <- head(cbind(Run, AFC_D_YMC), length(AFC_D_YMC)) #add run information
AFC_YMC.df <- as.data.frame((AFC_YMC)) #make into data frame
AFC_YMC.df <- melt(AFC_YMC.df,id.vars = c(1),variable.name = "FractionT_Y", value.name = "Y_ppm")%>%
  rename(RunsY = Run ) #transfer into long format
AFC_YMC.df <- transform(AFC_YMC.df, FractionT_Y = as.numeric(as.character(FractionT_Y)))
AFC_YMC.df <- AFC_YMC.df%>%
  mutate(ID_Y=RunsY+FractionT_Y)%>% 
  select(-starts_with("Runs"))%>%
  select(-starts_with("FractionT"))
Y_data <- data.matrix(AFC_YMC.df, rownames.force = NA)


rm(list = ls()[grepl("AFC", ls())])
rm(list = ls()[grepl("Y$", ls())])



#same for La as for Ba
AFC_D_LaMC<-outer(1:length(D_LaMC), 1:length(X_incr),function(i,j) C0_La[i]* ((X_incr[j]^(-z_La[i]))+
                                                                                ((r_ratio[i]/(r_ratio[i]-1))*Ca_La[i]/(z_La*C0_La)*
                                                                                   (1-(X_incr[j]^(-z_La[i])))))) 
colnames(AFC_D_LaMC) <- X_incr
AFC_LaMC <- head(cbind(Run, AFC_D_LaMC), length(AFC_D_LaMC)) #add run information
AFC_LaMC.df <- as.data.frame((AFC_LaMC)) #make into data frame
AFC_LaMC.df <- melt(AFC_LaMC.df,id.vars = c(1),variable.name = "FractionT_La", value.name = "La_ppm")%>%
  rename(RunsLa = Run ) #transfer into long format
AFC_LaMC.df <- transform(AFC_LaMC.df, FractionT_La = as.numeric(as.character(FractionT_La)))
AFC_LaMC.df <- AFC_LaMC.df%>%
  mutate(ID_La=RunsLa+FractionT_La)%>% 
  select(-starts_with("Runs"))%>%
  select(-starts_with("FractionT"))
La_data <- data.matrix(AFC_LaMC.df, rownames.force = NA)

rm(list = ls()[grepl("AFC", ls())])
rm(list = ls()[grepl("La$", ls())])

#same for Yb as for Ba
AFC_D_YbMC<-outer(1:length(D_YbMC), 1:length(X_incr),function(i,j) C0_Yb[i]* ((X_incr[j]^(-z_Yb[i]))+
                                                                                ((r_ratio[i]/(r_ratio[i]-1))*Ca_Yb[i]/(z_Yb*C0_Yb)*
                                                                                   (1-(X_incr[j]^(-z_Yb[i])))))) 
colnames(AFC_D_YbMC) <- X_incr
AFC_YbMC <- head(cbind(Run, AFC_D_YbMC), length(AFC_D_YbMC)) #add run information
AFC_YbMC.df <- as.data.frame((AFC_YbMC)) #make into data frame
AFC_YbMC.df <- melt(AFC_YbMC.df,id.vars = c(1),variable.name = "FractionT_Yb", value.name = "Yb_ppm")%>%
  rename(RunsYb = Run ) #transfer into long format
AFC_YbMC.df <- transform(AFC_YbMC.df, FractionT_Yb = as.numeric(as.character(FractionT_Yb)))
AFC_YbMC.df <- AFC_YbMC.df%>%
  mutate(ID_Yb=RunsYb+FractionT_Yb)%>% 
  select(-starts_with("Runs"))%>%
  select(-starts_with("FractionT"))
Yb_data <- data.matrix(AFC_YbMC.df, rownames.force = NA)

rm(list = ls()[grepl("AFC", ls())])
rm(list = ls()[grepl("Yb$", ls())])

#same for Dy as for Sr
AFC_D_DyMC<-outer(1:length(D_DyMC), 1:length(X_incr),function(i,j) C0_Dy[i]* ((X_incr[j]^(-z_Dy[i]))+
                                                                                ((r_ratio[i]/(r_ratio[i]-1))*Ca_Dy[i]/(z_Dy*C0_Dy)*
                                                                                   (1-(X_incr[j]^(-z_Dy[i])))))) 
colnames(AFC_D_DyMC) <- X_incr
AFC_DyMC <- head(cbind(Run, AFC_D_DyMC), length(AFC_D_DyMC)) #add run information
AFC_DyMC.df <- as.data.frame((AFC_DyMC)) #make into data frame
AFC_DyMC.df <- melt(AFC_DyMC.df,id.vars = c(1),variable.name = "FractionT_Dy", value.name = "Dy_ppm")%>%
  rename(RunsDy = Run ) #transfer into long format
AFC_DyMC.df <- transform(AFC_DyMC.df, FractionT_Dy = as.numeric(as.character(FractionT_Dy)))
AFC_DyMC.df <- AFC_DyMC.df%>%
  mutate(ID_Dy=RunsDy+FractionT_Dy)%>% 
  select(-starts_with("Runs"))%>%
  select(-starts_with("FractionT"))
Dy_data <- data.matrix(AFC_DyMC.df, rownames.force = NA)

rm(list = ls()[grepl("AFC", ls())])
rm(list = ls()[grepl("Dy$", ls())])#deletes everything ending with Dy

#same for Sc as for Ba
AFC_D_ScMC<-outer(1:length(D_ScMC), 1:length(X_incr),function(i,j) C0_Sc[i]* ((X_incr[j]^(-z_Sc[i]))+
                                                                                ((r_ratio[i]/(r_ratio[i]-1))*Ca_Sc[i]/(z_Sc*C0_Sc)*
                                                                                   (1-(X_incr[j]^(-z_Sc[i])))))) 
colnames(AFC_D_ScMC) <- X_incr
AFC_ScMC <- head(cbind(Run, AFC_D_ScMC), length(AFC_D_ScMC)) #add run information
AFC_ScMC.df <- as.data.frame((AFC_ScMC)) #make into data frame
AFC_ScMC.df <- melt(AFC_ScMC.df,id.vars = c(1),variable.name = "FractionT_Sc", value.name = "Sc_ppm")%>%
  rename(RunsSc = Run ) #transfer into long format
AFC_ScMC.df <- transform(AFC_ScMC.df, FractionT_Sc = as.numeric(as.character(FractionT_Sc)))
AFC_ScMC.df <- AFC_ScMC.df%>%
  mutate(ID_Sc=RunsSc+FractionT_Sc)%>% 
  select(-starts_with("Runs"))%>%
  select(-starts_with("FractionT"))
Sc_data <- data.matrix(AFC_ScMC.df, rownames.force = NA)

rm(list = ls()[grepl("AFC", ls())])
rm(list = ls()[grepl("Sc$", ls())])

#same for V as for Ba
AFC_D_VMC<-outer(1:length(D_VMC), 1:length(X_incr),function(i,j) C0_V[i]* ((X_incr[j]^(-z_V[i]))+
                                                                                ((r_ratio[i]/(r_ratio[i]-1))*Ca_V[i]/(z_V*C0_V)*
                                                                                   (1-(X_incr[j]^(-z_V[i])))))) 
colnames(AFC_D_VMC) <- X_incr
AFC_VMC <- head(cbind(Run, AFC_D_VMC), length(AFC_D_VMC)) #add run information
AFC_VMC.df <- as.data.frame((AFC_VMC)) #make into data frame
AFC_VMC.df <- melt(AFC_VMC.df,id.vars = c(1),variable.name = "FractionT_V", value.name = "V_ppm")%>%
  rename(RunsV = Run ) #transfer into long format
AFC_VMC.df <- transform(AFC_VMC.df, FractionT_V = as.numeric(as.character(FractionT_V)))
AFC_VMC.df <- AFC_VMC.df%>%
  mutate(ID_V=RunsV+FractionT_V)%>% 
  select(-starts_with("Runs"))%>%
  select(-starts_with("FractionT"))
V_data <- data.matrix(AFC_VMC.df, rownames.force = NA)

rm(list = ls()[grepl("AFC", ls())])
rm(list = ls()[grepl("V$", ls())])


rm(list = ls()[grepl("MC$", ls())])#deletes everything ending with MC
rm(list = ls()[grepl("^D_", ls())]) #deletes everything starting with D
rm(list = ls()[grepl("^Ca", ls())]) #deletes everything starting with D
rm(list = ls()[grepl(".mx", ls())]) #deletes everything starting with D
rm(list = ls()[grepl("melting", ls())]) #deletes everything starting with D
rm(list = ls()[grepl("Run", ls())]) #deletes everything starting with D
rm(list = ls()[grepl("r_ratio", ls())]) #deletes everything starting with D
rm


#combine


AFC_tot.ls<- do.call(cbind, lapply(ls(pattern = "_data"), get))
rm(list = ls()[grepl("data$", ls())])#deletes everything ending with MC
AFC_tot.df <- as.data.frame((AFC_tot.ls))
rm(list = ls()[grepl("AFC_tot.ls", ls())])
AFC_tot.df <- AFC_tot.df%>%
  mutate(Test = ID_Ba-ID_Th+(ID_Ba-ID_Sr)+(ID_Ba-ID_Y)+(ID_Ba-ID_La)+(ID_Ba-ID_Dy)+(ID_Ba-ID_Yb) 
         +(ID_Ba-ID_Sc)+(ID_Ba-ID_V))



#test if merging succesful
sum(AFC_tot.df$Test)
AFC_tot.df <- AFC_tot.df %>% 
  select(-starts_with("ID"))%>%
  mutate(SrY=Sr_ppm/Y_ppm)%>%
  mutate(BaTh=Ba_ppm/Th_ppm)%>%
  mutate(LaYb=La_ppm/Yb_ppm)%>%
  mutate(DyYb=Dy_ppm/Yb_ppm)%>%
  mutate(ThLa=Th_ppm/La_ppm)%>%
  mutate(VSc=V_ppm/Sc_ppm)
  



