##################################### DATA LOADING #####################################
########################################################################################
rm(list=ls()) # empty the memory

if(!require("ggplot2")) install.packages("ggplot2")
if(!require("ggExtra")) install.packages("ggExtra")
if(!require("gridExtra")) install.packages("gridExtra")
if(!require("reshape2")) install.packages("reshape2")
if(!require("MuMIn")) install.packages("MuMIn")
if(!require("afex")) install.packages("afex")
if(!require("emmeans")) install.packages("emmeans")
if(!require("effects")) install.packages("effects")
if(!require("lme4")) install.packages("lme4")
if(!require("lemon")) install.packages("lemon")

library(lsmeans)
library(doBy)
library(ggpubr)
library(DescTools)
library(viridis)
library(MASS)
library(plotrix)
library(bayestestR)
library(dplyr)
library(coin)
library(nnet)

#load ECOS data
file <- file.choose() 
file_name <- basename(file)
file_dir <- dirname(file)
setwd(file_dir)
load(file_name)

ECOS_dupl <- ECOS

#########DATA CURATION############

#discard participants who are not students
n <- array(c(0), dim = c(length(ECOS_dupl$record_id),1))

for (i in 1:(length(ECOS_dupl$record_id))){
  if (is.na(ECOS_dupl$uni[i])) {c <- 1; n[i] <- c}  
  else if (ECOS_dupl$uni[i] == 0){c <- 1; n[i] <- c}
  else {c <- 0}
}

ECOS_dupl2 <-data.frame(cbind(ECOS_dupl,n))

ECOS_dupl2 <- subset(ECOS_dupl2, n == 0, colnames(ECOS_dupl2)) #keep students


#keep only completed fulfillments
ECOS_dupl2 <- subset(ECOS_dupl2, domande_generali_complete != 0, colnames(ECOS_dupl2))

#keep only italian native speakers
ECOS_dupl2 <- subset(ECOS_dupl2, mlingua == 1, colnames(ECOS_dupl2))

#ignore participants with uncompleted questionnaires
ECOS_dupl2 <- subset(ECOS_dupl2, bdi_complete != 0, colnames(ECOS_dupl2)) 
ECOS_dupl2 <- subset(ECOS_dupl2, ehq21_complete != 0, colnames(ECOS_dupl2))
ECOS_dupl2 <- subset(ECOS_dupl2, ocir_complete != 0, colnames(ECOS_dupl2))
ECOS_dupl2 <- subset(ECOS_dupl2, bai_complete != 0, colnames(ECOS_dupl2))
ECOS_dupl2 <- subset(ECOS_dupl2, edi_iii_complete != 0, colnames(ECOS_dupl2))

#ignore participants older than 30
ECOS_dupl2 <- subset(ECOS_dupl2, eta <= 30, colnames(ECOS_dupl2))

#Keep first cross-sectional for EHQ time-zero model building
#ECOS_dupl2 <- subset(ECOS_dupl2, redcap_event_name == "primo_trasversale_arm_1", colnames(ECOS_dupl2))

#OR: keep only those who completed questionnaires twice
ECOS_dupl2 <- ECOS_dupl2[ECOS_dupl2$record_id %in% ECOS_dupl2$record_id[duplicated(ECOS_dupl2$record_id)],]

#keep only psychology students
#ECOS_dupl2 <- subset(ECOS_dupl2, scluni == "A6", colnames(ECOS_dupl2))


############################

#reorder
ECOS_dupl2 <- ECOS_dupl2 %>% arrange(record_id)

#sum "cognitive"/"somatic" subscales of BDI-II

################################
#cognitive - items 1-3, 5-9, 14

bdicog <- array(c(0), dim = c(length(ECOS_dupl2$record_id),1))

for (i in 1:(length(ECOS_dupl2$record_id))){
  bdicog[i] <- (ECOS_dupl2$bdi1[i] + ECOS_dupl2$bdi2[i] + ECOS_dupl2$bdi3[i] +
                  ECOS_dupl2$bdi5[i] + ECOS_dupl2$bdi6[i] + ECOS_dupl2$bdi7[i] + 
                  ECOS_dupl2$bdi8[i] + ECOS_dupl2$bdi9[i] + ECOS_dupl2$bdi14[i])
}

ECOS_dupl2 <-data.frame(cbind(ECOS_dupl2,bdicog))

#somatic - items 4, 10-13, 15-21
bdisom <- array(c(0), dim = c(length(ECOS_dupl2$record_id),1))

for (i in 1:(length(ECOS_dupl2$record_id))){
  bdisom[i] <- (ECOS_dupl2$bdi4[i] + ECOS_dupl2$bdi10[i] + ECOS_dupl2$bdi11[i] +
                  ECOS_dupl2$bdi12[i] + ECOS_dupl2$bdi13[i] + ECOS_dupl2$bdi15[i] + 
                  ECOS_dupl2$bdi16[i] + ECOS_dupl2$bdi17[i] + ECOS_dupl2$bdi18[i] + 
                  ECOS_dupl2$bdi19[i] + ECOS_dupl2$bdi20[i] + ECOS_dupl2$bdi21[i])
}

ECOS_dupl2 <-data.frame(cbind(ECOS_dupl2,bdisom))

#BDI total

bditot <- array(c(0), dim = c(length(ECOS_dupl2$record_id),1))

for (i in 1:length(ECOS_dupl2$record_id)){
  bditot[i] <- (ECOS_dupl2$bdicog[i] + ECOS_dupl2$bdisom[i])
}

ECOS_dupl2 <-data.frame(cbind(ECOS_dupl2,bditot))


#sum BAI

baitot <- array(c(0), dim = c(length(ECOS_dupl2$record_id),1))

for (i in 1:(length(ECOS_dupl2$record_id))){
  baitot[i] <- (ECOS_dupl2$bai1[i] + ECOS_dupl2$bai2[i] + ECOS_dupl2$bai3[i] +
                  ECOS_dupl2$bai4[i] + ECOS_dupl2$bai5[i] + ECOS_dupl2$bai6[i] +
                  ECOS_dupl2$bai7[i] + ECOS_dupl2$bai8[i] + ECOS_dupl2$bai9[i] +
                  ECOS_dupl2$bai10[i] + ECOS_dupl2$bai11[i] + ECOS_dupl2$bai12[i] +
                  ECOS_dupl2$bai13[i] + ECOS_dupl2$bai14[i] + ECOS_dupl2$bai15[i] +
                  ECOS_dupl2$bai16[i] + ECOS_dupl2$bai17[i] + ECOS_dupl2$bai18[i] +
                  ECOS_dupl2$bai19[i] + ECOS_dupl2$bai20[i] + ECOS_dupl2$bai21[i])
}

ECOS_dupl2 <-data.frame(cbind(ECOS_dupl2,baitot))

#sum OCI-R
#hoarding 1-7-13
hoard <- array(c(0), dim = c(length(ECOS_dupl2$record_id),1))

for (i in 1:(length(ECOS_dupl2$record_id))){
  hoard[i] <- (ECOS_dupl2$ocir1[i] + ECOS_dupl2$ocir7[i] + ECOS_dupl2$ocir13[i])
}

ECOS_dupl2 <-data.frame(cbind(ECOS_dupl2,hoard))

#checking 2-8-14
checking <- array(c(0), dim = c(length(ECOS_dupl2$record_id),1))

for (i in 1:(length(ECOS_dupl2$record_id))){
  checking[i] <- (ECOS_dupl2$ocir2[i] + ECOS_dupl2$ocir8[i] + ECOS_dupl2$ocir14[i])
}

ECOS_dupl2 <-data.frame(cbind(ECOS_dupl2,checking))

#ordering 3-9-15

ordering <- array(c(0), dim = c(length(ECOS_dupl2$record_id),1))

for (i in 1:(length(ECOS_dupl2$record_id))){
  ordering[i] <- (ECOS_dupl2$ocir3[i] + ECOS_dupl2$ocir9[i] + ECOS_dupl2$ocir15[i])
}

ECOS_dupl2 <-data.frame(cbind(ECOS_dupl2,ordering))

#mental neutralizing 4-10-16

mental <- array(c(0), dim = c(length(ECOS_dupl2$record_id),1))

for (i in 1:(length(ECOS_dupl2$record_id))){
  mental[i] <- (ECOS_dupl2$ocir4[i] + ECOS_dupl2$ocir10[i] + ECOS_dupl2$ocir16[i])
}

ECOS_dupl2 <-data.frame(cbind(ECOS_dupl2,mental))

#washing 5-11-17

wash <- array(c(0), dim = c(length(ECOS_dupl2$record_id),1))

for (i in 1:(length(ECOS_dupl2$record_id))){
  wash[i] <- (ECOS_dupl2$ocir5[i] + ECOS_dupl2$ocir11[i] + ECOS_dupl2$ocir17[i])
}

ECOS_dupl2 <-data.frame(cbind(ECOS_dupl2,wash))

#obsessing 6-12-18


obsess <- array(c(0), dim = c(length(ECOS_dupl2$record_id),1))

for (i in 1:(length(ECOS_dupl2$record_id))){
  obsess[i] <- (ECOS_dupl2$ocir6[i] + ECOS_dupl2$ocir12[i] + ECOS_dupl2$ocir18[i])
}

ECOS_dupl2 <-data.frame(cbind(ECOS_dupl2,obsess))

#ocitot


ocitot <- array(c(0), dim = c(length(ECOS_dupl2$record_id),1))

for (i in 1:length(ECOS_dupl2$record_id)){
  ocitot[i] <- (ECOS_dupl2$obsess[i] + ECOS_dupl2$wash[i] + ECOS_dupl2$mental[i] +
                  ECOS_dupl2$ordering[i] + ECOS_dupl2$checking[i] + ECOS_dupl2$hoard[i])
}

ECOS_dupl2 <-data.frame(cbind(ECOS_dupl2,ocitot))


#ehq score
#convictions, items 1, 3, 5, 11, 21.
EHQconvictions <- array(c(0), dim = c(length(ECOS_dupl2$record_id),1))

for (i in 1:(length(ECOS_dupl2$record_id))){
  EHQconvictions[i] <- (ECOS_dupl2$ehq1[i] + ECOS_dupl2$ehq3[i] + ECOS_dupl2$ehq5[i] +
                       ECOS_dupl2$ehq11[i] + ECOS_dupl2$ehq21[i])
}

ECOS_dupl2 <-data.frame(cbind(ECOS_dupl2,EHQconvictions))

#problems 2, 4, 6, 7, 8, 10, 13, 14, 16, 17, 18, 20.
EHQproblems <- array(c(0), dim = c(length(ECOS_dupl2$record_id),1))

for (i in 1:(length(ECOS_dupl2$record_id))){
  EHQproblems[i] <- (ECOS_dupl2$ehq2[i] + ECOS_dupl2$ehq4[i] + ECOS_dupl2$ehq6[i] +
                          ECOS_dupl2$ehq7[i] + ECOS_dupl2$ehq8[i] + ECOS_dupl2$ehq10[i] +
                       ECOS_dupl2$ehq13[i] + ECOS_dupl2$ehq14[i] + ECOS_dupl2$ehq16[i] +
                       ECOS_dupl2$ehq17[i] + ECOS_dupl2$ehq18[i] + ECOS_dupl2$ehq20[i])
}

ECOS_dupl2 <-data.frame(cbind(ECOS_dupl2,EHQproblems))

#emotions 9, 12, 15, 19.
EHQemotions <- array(c(0), dim = c(length(ECOS_dupl2$record_id),1))

for (i in 1:(length(ECOS_dupl2$record_id))){
  EHQemotions[i] <- (ECOS_dupl2$ehq9[i] + ECOS_dupl2$ehq12[i] + ECOS_dupl2$ehq15[i] +
                          ECOS_dupl2$ehq19[i])
}

ECOS_dupl2 <-data.frame(cbind(ECOS_dupl2,EHQemotions))

#ehq total
EHQtotal <- array(c(0), dim = c(length(ECOS_dupl2$record_id),1))

for (i in 1:(length(ECOS_dupl2$record_id))){
  EHQtotal[i] <- (EHQconvictions[i]+ EHQemotions[i] + EHQproblems[i])
}

ECOS_dupl2 <- data.frame(cbind(ECOS_dupl2, EHQtotal))

#edi iii score
edialone <- ECOS_dupl2[, c(149:239)]
ECOS_dupl2 <- ECOS_dupl2[, -c(149:239)]

edialone_inv <- edialone[,c("ediq1", "ediq12", "ediq19", "ediq31", "ediq55", "ediq62", "ediq37",
                            "ediq42", "ediq50", "ediq20", "ediq80", "ediq91", "ediq15", "ediq23",
                            "ediq57", "ediq69", "ediq73", "ediq17", "ediq30", "ediq76", "ediq89",
                            "ediq26", "ediq22", "ediq39", "ediq58")]
edialone_rig <- edialone[,-c(1,12,19,31,55,62,37,42,50,20,80,91,15,23,26,22,39,58,57,69,73,17,30,76,89)]

edialone_rig.mx <- as.matrix(edialone_rig)
edialone_rig.mx[which(edialone_rig == "A")] <- 4
edialone_rig.mx[which(edialone_rig == "B")] <- 3
edialone_rig.mx[which(edialone_rig == "C")] <- 2
edialone_rig.mx[which(edialone_rig == "D")] <- 1
edialone_rig.mx[which(edialone_rig == "E")] <- 0
edialone_rig.mx[which(edialone_rig == "F")] <- 0

edialone_rig<- as.data.frame(edialone_rig.mx)
  

edialone_inv.mx <- as.matrix(edialone_inv)
edialone_inv.mx[which(edialone_inv == "A")] <- 0
edialone_inv.mx[which(edialone_inv == "B")] <- 0
edialone_inv.mx[which(edialone_inv == "C")] <- 1
edialone_inv.mx[which(edialone_inv == "D")] <- 2
edialone_inv.mx[which(edialone_inv == "E")] <- 3
edialone_inv.mx[which(edialone_inv == "F")] <- 4

edialone_inv<- as.data.frame(edialone_inv.mx)


edialone <- cbind(edialone_rig,edialone_inv)

ECOS_dupl2 <- as.data.frame(cbind(ECOS_dupl2,edialone))

#############edi_iii subscales
#thinness impulse items 1-7-11-16-52-32-49

thinImp <- array(c(0), dim = c(length(ECOS_dupl2$record_id),1))

for (i in 1:(length(ECOS_dupl2$record_id))){
  thinImp[i] <- (as.numeric(ECOS_dupl2$ediq1[i]) + as.numeric(ECOS_dupl2$ediq7[i]) + as.numeric(ECOS_dupl2$ediq11[i]) +
                   as.numeric(ECOS_dupl2$ediq16[i]) + as.numeric(ECOS_dupl2$ediq52[i]) + as.numeric(ECOS_dupl2$ediq32[i]) +
                   as.numeric(ECOS_dupl2$ediq49[i]))
}

ECOS_dupl2 <-data.frame(cbind(ECOS_dupl2,thinImp))

#bulimia 4-5-28-38-46-53-61-64


bulimia <- array(c(0), dim = c(length(ECOS_dupl2$record_id),1))

for (i in 1:(length(ECOS_dupl2$record_id))){
  bulimia[i] <- (as.numeric(ECOS_dupl2$ediq4[i]) + as.numeric(ECOS_dupl2$ediq5[i]) + as.numeric(ECOS_dupl2$ediq28[i]) +
                   as.numeric(ECOS_dupl2$ediq38[i]) + as.numeric(ECOS_dupl2$ediq46[i]) + as.numeric(ECOS_dupl2$ediq53[i]) +
                   as.numeric(ECOS_dupl2$ediq61[i])+ as.numeric(ECOS_dupl2$ediq64[i]))
}

ECOS_dupl2 <-data.frame(cbind(ECOS_dupl2,bulimia))

#body dissatisfaction 2-9-12-19-31-45-47-55-59-62

bodyDis <- array(c(0), dim = c(length(ECOS_dupl2$record_id),1))

for (i in 1:(length(ECOS_dupl2$record_id))){
  bodyDis[i] <- (as.numeric(ECOS_dupl2$ediq2[i]) + as.numeric(ECOS_dupl2$ediq9[i]) + as.numeric(ECOS_dupl2$ediq12[i]) +
                   as.numeric(ECOS_dupl2$ediq19[i]) + as.numeric(ECOS_dupl2$ediq31[i]) + as.numeric(ECOS_dupl2$ediq45[i]) +
                   as.numeric(ECOS_dupl2$ediq47[i])+ as.numeric(ECOS_dupl2$ediq55[i]) + as.numeric(ECOS_dupl2$ediq59[i])+
                   as.numeric(ECOS_dupl2$ediq62[i]))
}

ECOS_dupl2 <-data.frame(cbind(ECOS_dupl2,bodyDis))

#low self-esteem 10-27-37-41-42-50

selfEst <- array(c(0), dim = c(length(ECOS_dupl2$record_id),1))

for (i in 1:(length(ECOS_dupl2$record_id))){
  selfEst[i] <- (as.numeric(ECOS_dupl2$ediq10[i]) + as.numeric(ECOS_dupl2$ediq27[i]) + as.numeric(ECOS_dupl2$ediq37[i]) +
                   as.numeric(ECOS_dupl2$ediq41[i]) + as.numeric(ECOS_dupl2$ediq42[i]) + as.numeric(ECOS_dupl2$ediq50[i]))
}

ECOS_dupl2 <-data.frame(cbind(ECOS_dupl2,selfEst))

#personal alienation 18-20-24-56-80-84-91

alienPer <- array(c(0), dim = c(length(ECOS_dupl2$record_id),1))

for (i in 1:(length(ECOS_dupl2$record_id))){
  alienPer[i] <- (as.numeric(ECOS_dupl2$ediq18[i]) + as.numeric(ECOS_dupl2$ediq20[i]) + as.numeric(ECOS_dupl2$ediq24[i]) +
                   as.numeric(ECOS_dupl2$ediq56[i]) + as.numeric(ECOS_dupl2$ediq80[i]) + as.numeric(ECOS_dupl2$ediq84[i])+
                    as.numeric(ECOS_dupl2$ediq91[i]))
}

ECOS_dupl2 <-data.frame(cbind(ECOS_dupl2,alienPer))

#interpersonal insecurity 15-23-34-57-69-73-87

inseInter <- array(c(0), dim = c(length(ECOS_dupl2$record_id),1))

for (i in 1:(length(ECOS_dupl2$record_id))){
  inseInter[i] <- (as.numeric(ECOS_dupl2$ediq15[i]) + as.numeric(ECOS_dupl2$ediq23[i]) + as.numeric(ECOS_dupl2$ediq34[i]) +
                    as.numeric(ECOS_dupl2$ediq57[i]) + as.numeric(ECOS_dupl2$ediq69[i]) + as.numeric(ECOS_dupl2$ediq73[i])+
                    as.numeric(ECOS_dupl2$ediq87[i]))
}

ECOS_dupl2 <-data.frame(cbind(ECOS_dupl2,inseInter))

#interpersonal alienation 17-30-54-65-74-76-89

alienInt <- array(c(0), dim = c(length(ECOS_dupl2$record_id),1))

for (i in 1:(length(ECOS_dupl2$record_id))){
  alienInt[i] <- (as.numeric(ECOS_dupl2$ediq17[i]) + as.numeric(ECOS_dupl2$ediq30[i]) + as.numeric(ECOS_dupl2$ediq54[i]) +
                    as.numeric(ECOS_dupl2$ediq65[i]) + as.numeric(ECOS_dupl2$ediq74[i]) + as.numeric(ECOS_dupl2$ediq76[i])+
                    as.numeric(ECOS_dupl2$ediq89[i]))
}

ECOS_dupl2 <-data.frame(cbind(ECOS_dupl2,alienInt))

#enterocept deficit 8-21-26-33-40-44-51-60-77

defEntero <- array(c(0), dim = c(length(ECOS_dupl2$record_id),1))

for (i in 1:(length(ECOS_dupl2$record_id))){
  defEntero[i] <- (as.numeric(ECOS_dupl2$ediq8[i]) + as.numeric(ECOS_dupl2$ediq21[i]) + as.numeric(ECOS_dupl2$ediq26[i]) +
                    as.numeric(ECOS_dupl2$ediq33[i]) + as.numeric(ECOS_dupl2$ediq40[i]) + as.numeric(ECOS_dupl2$ediq44[i])+
                    as.numeric(ECOS_dupl2$ediq51[i]) + as.numeric(ECOS_dupl2$ediq60[i]) + as.numeric(ECOS_dupl2$ediq77[i]) )
}

ECOS_dupl2 <-data.frame(cbind(ECOS_dupl2,defEntero))

#emotional disregulation 67-70-72-79-81-83-85-90

disregEm <- array(c(0), dim = c(length(ECOS_dupl2$record_id),1))

for (i in 1:(length(ECOS_dupl2$record_id))){
  disregEm[i] <- (as.numeric(ECOS_dupl2$ediq67[i]) + as.numeric(ECOS_dupl2$ediq70[i]) + as.numeric(ECOS_dupl2$ediq72[i]) +
                     as.numeric(ECOS_dupl2$ediq79[i]) + as.numeric(ECOS_dupl2$ediq81[i]) + as.numeric(ECOS_dupl2$ediq83[i])+
                     as.numeric(ECOS_dupl2$ediq85[i]) + as.numeric(ECOS_dupl2$ediq90[i]))
}

ECOS_dupl2 <-data.frame(cbind(ECOS_dupl2,disregEm))

#perfectionism 13-29-36-43-52-63

perfect <- array(c(0), dim = c(length(ECOS_dupl2$record_id),1))

for (i in 1:(length(ECOS_dupl2$record_id))){
  perfect[i] <- (as.numeric(ECOS_dupl2$ediq13[i]) + as.numeric(ECOS_dupl2$ediq29[i]) + as.numeric(ECOS_dupl2$ediq36[i]) +
                    as.numeric(ECOS_dupl2$ediq43[i]) + as.numeric(ECOS_dupl2$ediq52[i]) + as.numeric(ECOS_dupl2$ediq63[i]))
}

ECOS_dupl2 <-data.frame(cbind(ECOS_dupl2,perfect))

#ascetism 66-68-75-78-82-86-88

ascetism <- array(c(0), dim = c(length(ECOS_dupl2$record_id),1))

for (i in 1:(length(ECOS_dupl2$record_id))){
  ascetism[i] <- (as.numeric(ECOS_dupl2$ediq66[i]) + as.numeric(ECOS_dupl2$ediq68[i]) + as.numeric(ECOS_dupl2$ediq75[i]) +
                   as.numeric(ECOS_dupl2$ediq78[i]) + as.numeric(ECOS_dupl2$ediq82[i]) + as.numeric(ECOS_dupl2$ediq86[i]) +
                    as.numeric(ECOS_dupl2$ediq88[i]))
}

ECOS_dupl2 <-data.frame(cbind(ECOS_dupl2,ascetism))

#Fear of maturity 3-6-14-22-35-39-48-58

fearMat <- array(c(0), dim = c(length(ECOS_dupl2$record_id),1))

for (i in 1:(length(ECOS_dupl2$record_id))){
  fearMat[i] <- (as.numeric(ECOS_dupl2$ediq3[i]) + as.numeric(ECOS_dupl2$ediq6[i]) + as.numeric(ECOS_dupl2$ediq14[i]) +
                    as.numeric(ECOS_dupl2$ediq22[i]) + as.numeric(ECOS_dupl2$ediq35[i]) + as.numeric(ECOS_dupl2$ediq39[i]) +
                    as.numeric(ECOS_dupl2$ediq48[i]) + as.numeric(ECOS_dupl2$ediq58[i]))
}

ECOS_dupl2 <-data.frame(cbind(ECOS_dupl2,fearMat))

#editot

EDI3TOT <- array(c(0), dim = c(length(ECOS_dupl2$record_id),1))

for (i in 1:(length(ECOS_dupl2$record_id))){
  EDI3TOT[i] <- (ECOS_dupl2$fearMat[i]+ ECOS_dupl2$ascetism[i] + ECOS_dupl2$perfect[i] + 
                   ECOS_dupl2$alienInt[i] + ECOS_dupl2$alienPer[i] +
                   ECOS_dupl2$inseInter[i] + ECOS_dupl2$defEntero[i] + ECOS_dupl2$disregEm[i] + 
                   ECOS_dupl2$selfEst[i] + 
                   ECOS_dupl2$bodyDis[i] +
                   ECOS_dupl2$bulimia[i] + ECOS_dupl2$thinImp[i])
}

ECOS_dupl2 <- data.frame(cbind(ECOS_dupl2, EDI3TOT))

#ED_risk


ED_risk <- array(c(0), dim = c(length(ECOS_dupl2$record_id),1))

for (i in 1:(length(ECOS_dupl2$record_id))){
  ED_risk[i] <- (ECOS_dupl2$bodyDis[i] + ECOS_dupl2$bulimia[i] + ECOS_dupl2$thinImp[i])
}

ECOS_dupl2 <- data.frame(cbind(ECOS_dupl2, ED_risk))

#inadequate

Inadeq <- array(c(0), dim = c(length(ECOS_dupl2$record_id),1))

for (i in 1:(length(ECOS_dupl2$record_id))){
  Inadeq[i] <- (ECOS_dupl2$selfEst[i] + ECOS_dupl2$alienPer[i])
}

ECOS_dupl2 <- data.frame(cbind(ECOS_dupl2, Inadeq))

#InterpersProblems

InterProb <- array(c(0), dim = c(length(ECOS_dupl2$record_id),1))

for (i in 1:(length(ECOS_dupl2$record_id))){
  InterProb[i] <- (ECOS_dupl2$inseInter[i] + ECOS_dupl2$alienInt[i])
}

ECOS_dupl2 <- data.frame(cbind(ECOS_dupl2, InterProb))

#AffectiveProblems

affect <- array(c(0), dim = c(length(ECOS_dupl2$record_id),1))

for (i in 1:(length(ECOS_dupl2$record_id))){
  affect[i] <- (ECOS_dupl2$disregEm[i] + ECOS_dupl2$defEntero[i])
}

ECOS_dupl2 <- data.frame(cbind(ECOS_dupl2, affect))

#Overcontrol

overctrl <- array(c(0), dim = c(length(ECOS_dupl2$record_id),1))

for (i in 1:(length(ECOS_dupl2$record_id))){
  overctrl[i] <- (ECOS_dupl2$perfect[i] + ECOS_dupl2$ascetism[i])
}

ECOS_dupl2 <- data.frame(cbind(ECOS_dupl2, overctrl))

#generalDisadapt

gDisadapt <- array(c(0), dim = c(length(ECOS_dupl2$record_id),1))


for (i in 1:(length(ECOS_dupl2$record_id))){
  gDisadapt[i] <- (ECOS_dupl2$overctrl[i] + ECOS_dupl2$affect[i] + ECOS_dupl2$InterProb[i] +
                     ECOS_dupl2$Inadeq[i] + fearMat[i])
}

ECOS_dupl2 <- data.frame(cbind(ECOS_dupl2, gDisadapt))

#mark those that are during - after Lockdown
after <- array(c(0), dim = c(length(ECOS_dupl2$record_id),1))
for (i in 1:(length(ECOS_dupl2$record_id))){
  if (ECOS_dupl2$record_id[i] < 337){after[i] <- 0}
  else if (ECOS_dupl2$record_id[i] > 337){after[i] <- 1}
}

ECOS_dupl2 <- data.frame(cbind(ECOS_dupl2, after))
##############

#rename some variables
#ECOS_dupl2 <- ECOS_dupl2 %>% arrange(record_id, redcap_repeat_instance)

#ECOS_dupl2 <- ECOS_dupl2[-(1:2),]

levels(ECOS_dupl2$redcap_event_name)[levels(ECOS_dupl2$redcap_event_name)=="primo_trasversale_arm_1"] <- "Baseline"

levels(ECOS_dupl2$redcap_event_name)[levels(ECOS_dupl2$redcap_event_name)=="secondo_trasversal_arm_1"] <- "6 months later"

ECOS_dupl2$sex <- as.factor(ECOS_dupl2$sex)
ECOS_dupl2$distvit___7 <- as.factor(ECOS_dupl2$distvit___7)

levels(ECOS_dupl2$sex)[levels(ECOS_dupl2$sex)=="0"] <- "Females"

levels(ECOS_dupl2$sex)[levels(ECOS_dupl2$sex)=="1"] <- "Males"


levels(ECOS_dupl2$distvit___7)[levels(ECOS_dupl2$distvit___7)== "0"] <- "Disorder"

levels(ECOS_dupl2$distvit___7)[levels(ECOS_dupl2$distvit___7)== "1"] <- "No Disorder"

ECOS_dupl2 <- ECOS_dupl2[-c(1:2),]

# for the sake of simplicity, let's define "6 months later" as "During lockdown" irregardless
# of realt time windows --> we'll define after/during lockdown periods later on
levels(ECOS_dupl2$redcap_event_name)[levels(ECOS_dupl2$redcap_event_name)=="Baseline"] <- "Before Lockdown"

levels(ECOS_dupl2$redcap_event_name)[levels(ECOS_dupl2$redcap_event_name)=="6 months later"] <- "During Lockdown"

######################TEST FOR CHANGES IN BDI SCORE#######################??
## a % deltaBDI (dBDI) is the result of 
#[(BDI Score during Lockdown - before Lockdown)/(BDI Score before Lockdown + 1)]
#that "+1" accounts for BDI-2 scores before Lockdown = 0 (the % delta would otherwise be Inf)

dBDI <- with(ECOS_dupl2,
             ((bditot[redcap_event_name == "During Lockdown"]) - (bditot[redcap_event_name == "Before Lockdown"])))

######################TEST FOR CHANGES IN EDI SCORE#######################
## a % deltaEDI (dEDI) is the result of
#[(EDI Score during Lockdown - before Lockdown)/(EDI Score before Lockdown)]

dEDI <- with(ECOS_dupl2,
             (((ED_risk[redcap_event_name == "During Lockdown"] - (ED_risk[redcap_event_name == "Before Lockdown"])))))


dDisadapt <- with(ECOS_dupl2,
                  (((gDisadapt[redcap_event_name == "During Lockdown"] - (gDisadapt[redcap_event_name == "Before Lockdown"])))))


dEHQTOT <- with(ECOS_dupl2,
                (((EHQtotal[redcap_event_name == "During Lockdown"] - (EHQtotal[redcap_event_name == "Before Lockdown"])))))


docitot <- with(ECOS_dupl2,
                (((ocitot[redcap_event_name == "During Lockdown"] - (ocitot[redcap_event_name == "Before Lockdown"])))))



dBAI <- with(ECOS_dupl2,
             (((baitot[redcap_event_name == "During Lockdown"] - (baitot[redcap_event_name == "Before Lockdown"])))))

# ECOS_dupl2$peconom <- as.numeric(ECOS_dupl2$peconom)
# 
# diff_economic <- array(c(0), dim = c(length(ECOS_dupl2$record_id),1))
# for (i in 1:(length(ECOS_dupl2$record_id))){
#   if (ECOS_dupl2$redcap_event_name[i] == "Before Lockdown") {diff_economic[i] <- ECOS_dupl2$peconom[i-1] - ECOS_dupl2$peconom[i]}
#   else if (ECOS_dupl2$redcap_event_name[i] == "During Lockdown") {diff_economic[i] <- ECOS_dupl2$peconom[i] - ECOS_dupl2$peconom[i]}
# }
# 
# ECOS_dupl2 <- data.frame(cbind(ECOS_dupl2, diff_economic))


#############NUMBER OF PEOPLE EXPERIENCING CLINICAL INCREASE##########
# define clinical categories as
# < 25th percentile - mild or no symptoms
# > 66th - severe symptomatology
# between those two ranges - moderate symptomatology
clinicalEDRISK <- array(c(0), dim = c(length(ECOS_dupl2$record_id),1))
for (i in 1:(length(ECOS_dupl2$record_id))){
  
  if (ECOS_dupl2$ED_risk[i] <12) {clinicalEDRISK[i] <- 0}
  else if (ECOS_dupl2$ED_risk[i] >= 12 & ECOS_dupl2$ED_risk[i] <= 36) {clinicalEDRISK[i] <- 1}
  else if (ECOS_dupl2$ED_risk[i] > 36 ) {clinicalEDRISK[i] <- 2}
}

ECOS_dupl2 <- data.frame(cbind(ECOS_dupl2, clinicalEDRISK))

# define change in clinical category
# treat categories as number for a second:
# subtract the previous category (before Lockdown) to the current class (during Lockdown)
# if no change, the result = 0
# if change in upper category:
# from <25th to 25-66th range = 1, as well as
# from 25-66th range to > 66 = 1
# from < 25th directly to > 66th = 2
# the other way (-2 or -1) if symptoms improve

# 
# diff_clinicalEDRISK <- array(c(0), dim = c(length(ECOS_dupl2$record_id),1))
# for (i in 1:(length(ECOS_dupl2$record_id))){
#   if (ECOS_dupl2$redcap_event_name[i] == "Before Lockdown") {diff_clinicalEDRISK[i] <- clinicalEDRISK[i-1] - clinicalEDRISK[i]}
#   else if (ECOS_dupl2$redcap_event_name[i] == "During Lockdown") {diff_clinicalEDRISK[i] <- clinicalEDRISK[i] - clinicalEDRISK[i]}
# }
# 
# ECOS_dupl2 <- data.frame(cbind(ECOS_dupl2, diff_clinicalEDRISK))
##############GRAPHS AND STATISTICS###############################
ECOS_dupl2$record_id <- as.factor(ECOS_dupl2$record_id)
ECOS_dupl2$redcap_event_name <- as.factor(ECOS_dupl2$redcap_event_name) 
ECOS_dupl2$reside <- as.factor(ECOS_dupl2$reside)
ECOS_dupl2$sex <- as.factor(ECOS_dupl2$sex)
is.numeric(ECOS_dupl2$eta)
ECOS_dupl2$schlunianni <- as.factor(ECOS_dupl2$schlunianni)
is.numeric(ECOS_dupl2$bmi)
is.factor(ECOS_dupl2$dietan)
ECOS_dupl2$distvit___7 <- as.factor(ECOS_dupl2$distvit___7)
ECOS_dupl2$distvit___6 <- as.factor(ECOS_dupl2$distvit___6)
ECOS_dupl2$distvit___5 <- as.factor(ECOS_dupl2$distvit___5)
ECOS_dupl2$distvit___4 <- as.factor(ECOS_dupl2$distvit___4)
ECOS_dupl2$distvit___3 <- as.factor(ECOS_dupl2$distvit___3)
ECOS_dupl2$distvit___2 <- as.factor(ECOS_dupl2$distvit___2)
ECOS_dupl2$distvit___1 <- as.factor(ECOS_dupl2$distvit___1)
ECOS_dupl2$peconom <- as.factor(ECOS_dupl2$peconom)
ECOS_dupl2$famdist <- as.factor(ECOS_dupl2$famdist)
ECOS_dupl2$intolal <- as.factor(ECOS_dupl2$intolal)
ECOS_dupl2$evitacib <- as.factor(ECOS_dupl2$evitacib)
ECOS_dupl2$talim <- as.factor(ECOS_dupl2$talim)

for (i in 1:(length(ECOS_dupl2$record_id))){
  if (ECOS_dupl2$bmi[i] > 100){ECOS_dupl2$bmi[i] <- ECOS_dupl2$bmi[i]/10000}
}



anydiet <- array(c(0), dim = c(length(ECOS_dupl2$record_id),1))
for (i in 1:(length(ECOS_dupl2$record_id))){
  if (ECOS_dupl2$dietan[i] == "A0"){anydiet[i] <- 0}
  else if (ECOS_dupl2$dietan[i] != "A0"){anydiet[i] <- 1}
}

ECOS_dupl2 <- data.frame(cbind(ECOS_dupl2, anydiet))

anydisease <- array(c(0), dim = c(length(ECOS_dupl2$record_id),1))
for (i in 1:(length(ECOS_dupl2$record_id))){
  if (ECOS_dupl2$mmet[i] == "0" & ECOS_dupl2$mgi[i] == "0"& ECOS_dupl2$msn[i] == "0"){anydisease[i] <- 0}
  else if (ECOS_dupl2$mmet[i] == "1" | ECOS_dupl2$mgi[i] == "1" | ECOS_dupl2$msn[i] == "1"){anydisease[i] <- 1}
}

ECOS_dupl2 <- data.frame(cbind(ECOS_dupl2, anydisease))


anyED <- array(c(0), dim = c(length(ECOS_dupl2$record_id),1))
for (i in 1:(length(ECOS_dupl2$record_id))){
  if (ECOS_dupl2$distvit___1[i] == "0" & ECOS_dupl2$distvit___2[i] == "0"& ECOS_dupl2$distvit___3[i] == "0"){anyED[i] <- 0}
  else if (ECOS_dupl2$distvit___1[i] == "1" | ECOS_dupl2$distvit___2[i] == "1" | ECOS_dupl2$distvit___3[i] == "1"){anyED[i] <- 1}
}

ECOS_dupl2 <- data.frame(cbind(ECOS_dupl2, anyED))


ECOS_dupl2$bditot <- as.numeric(ECOS_dupl2$bditot)
ECOS_dupl2$EDI3TOT <- as.numeric(ECOS_dupl2$EDI3TOT)

classBDI <- array(c(0), dim = c(length(ECOS_dupl2$record_id),1))
for (i in 1:(length(ECOS_dupl2$record_id))){
  if (ECOS_dupl2$bditot[i] <= 9 & ECOS_dupl2$sex[i] == "Females" | ECOS_dupl2$bditot[i] <= 7 & ECOS_dupl2$sex[i] == "Males"){classBDI[i] <- 1}
  else if (ECOS_dupl2$bditot[i] > 9 & ECOS_dupl2$bditot[i] <= 17 & ECOS_dupl2$sex[i] == "Females" | ECOS_dupl2$bditot[i] > 7 & ECOS_dupl2$bditot[i] <= 13 & ECOS_dupl2$sex[i] == "Males"){classBDI[i] <- 2}
  else if (ECOS_dupl2$bditot[i] > 17 & ECOS_dupl2$sex[i] == "Females" | ECOS_dupl2$bditot[i] > 13 & ECOS_dupl2$sex[i] == "Males" ){classBDI[i] <- 3}
}

ECOS_dupl2 <- data.frame(cbind(ECOS_dupl2, classBDI))

classEDI <- array(c(0), dim = c(length(ECOS_dupl2$record_id),1))
for (i in 1:(length(ECOS_dupl2$record_id))){
  if (ECOS_dupl2$EDI3TOT[i] <= 178 & ECOS_dupl2$sex[i] == "Females" | ECOS_dupl2$EDI3TOT[i] <= 160 & ECOS_dupl2$sex[i] == "Males" ){classEDI[i] <- 1}
  else if (ECOS_dupl2$EDI3TOT[i] > 178 & ECOS_dupl2$EDI3TOT[i] <= 226 & ECOS_dupl2$sex[i] == "Females" | ECOS_dupl2$EDI3TOT[i] > 160 & ECOS_dupl2$EDI3TOT[i] <= 182 & ECOS_dupl2$sex[i] == "Males"){classEDI[i] <- 2}
  else if (ECOS_dupl2$EDI3TOT[i] > 226 & ECOS_dupl2$sex[i] == "Females" | ECOS_dupl2$EDI3TOT[i] > 182 & ECOS_dupl2$sex[i] == "Males"){classEDI[i] <- 3}
}

ECOS_dupl2 <- data.frame(cbind(ECOS_dupl2, classEDI))


#############NUMBER OF PEOPLE EXPERIENCING CLINICAL INCREASE##########
# define clinical categories as
# < 25th percentile - mild or no symptoms
# > 66th - severe symptomatology
# between those two ranges - moderate symptomatology
clinicalDISADAPT <- array(c(0), dim = c(length(ECOS_dupl2$record_id),1))
for (i in 1:(length(ECOS_dupl2$record_id))){
  
  if (ECOS_dupl2$gDisadapt[i] <37) {clinicalDISADAPT[i] <- 0}
  else if (ECOS_dupl2$gDisadapt[i] >= 37 & ECOS_dupl2$gDisadapt[i] <= 80) {clinicalDISADAPT[i] <- 1}
  else if (ECOS_dupl2$gDisadapt[i] > 80 ) {clinicalDISADAPT[i] <- 2}
}

ECOS_dupl2 <- data.frame(cbind(ECOS_dupl2, clinicalDISADAPT))

# define change in clinical category
# treat categories as number for a second:
# subtract the previous category (before Lockdown) to the current class (during Lockdown)
# if no change, the result = 0
# if change in upper category:
# from <25th to 25-66th range = 1, as well as
# from 25-66th range to > 66 = 1
# from < 25th directly to > 66th = 2
# the other way (-2 or -1) if symptoms improve

# 
# diff_clinicalDISADAPT <- array(c(0), dim = c(length(ECOS_dupl2$record_id),1))
# for (i in 1:(length(ECOS_dupl2$record_id))){
#   if (ECOS_dupl2$redcap_event_name[i] == "Before Lockdown") {diff_clinicalDISADAPT[i] <- clinicalDISADAPT[i-1] - clinicalDISADAPT[i]}
#   else if (ECOS_dupl2$redcap_event_name[i] == "During Lockdown") {diff_clinicalDISADAPT[i] <- clinicalDISADAPT[i] - clinicalDISADAPT[i]}
# }


#############NUMBER OF PEOPLE EXPERIENCING CLINICAL INCREASE##########
# define clinical categories as
# < 90th percentile (score is stratified according to sex) - mild or no symptoms
# > 95th - severe symptomatology
# between those two ranges - moderate symptomatology
clinicalBDI <- array(c(0), dim = c(length(ECOS_dupl2$record_id),1))
for (i in 1:(length(ECOS_dupl2$record_id))){
  
  if (ECOS_dupl2$bditot[i] >=11 & ECOS_dupl2$bditot[i] <= 13 & ECOS_dupl2$sex[i] == "Males") {clinicalBDI[i] <- 0}
  else if (ECOS_dupl2$bditot[i] <= 18 & ECOS_dupl2$bditot[i] >=14 & ECOS_dupl2$sex[i] == "Males") {clinicalBDI[i] <- 1}
  else if (ECOS_dupl2$bditot[i] >= 19 & ECOS_dupl2$sex[i] == "Males") {clinicalBDI[i] <- 2}
  
  else if (ECOS_dupl2$bditot[i] >= 14 & ECOS_dupl2$bditot[i] <=16 & ECOS_dupl2$sex[i] == "Females") {clinicalBDI[i] <- 0}
  else if (ECOS_dupl2$bditot[i] <= 19 & ECOS_dupl2$bditot[i] >= 17 & ECOS_dupl2$sex[i] == "Females") {clinicalBDI[i] <- 1}
  else if (ECOS_dupl2$bditot[i] >= 20 & ECOS_dupl2$sex[i] == "Females") {clinicalBDI[i] <- 2}
}

ECOS_dupl2 <- data.frame(cbind(ECOS_dupl2, clinicalBDI))

diff_clinicalBDI <- array(c(0), dim = c(length(ECOS_dupl2$record_id),1))
for (i in 1:(length(ECOS_dupl2$record_id))){
  if (ECOS_dupl2$redcap_event_name[i] == "Before Lockdown") {diff_clinicalBDI[i] <- clinicalBDI[i-1] - clinicalBDI[i]}
  else if (ECOS_dupl2$redcap_event_name[i] == "During Lockdown") {diff_clinicalBDI[i] <- clinicalBDI[i] - clinicalBDI[i]}
}

ECOS_dupl2 <- data.frame(cbind(ECOS_dupl2, diff_clinicalBDI))

# define records that were completed after or during lockdown
events <- array(c(0), dim = c(length(ECOS_dupl2$record_id),1))
for (i in 1:(length(ECOS_dupl2$record_id))){
  if (ECOS_dupl2$redcap_event_name[i] == "Before Lockdown"){events[i] <- "Before Lockdown"}
  else if ((ECOS_dupl2$after[i] == 1) & (ECOS_dupl2$redcap_event_name[i] == "During Lockdown")) {events[i] <- "After Lockdown"}
  else if ((ECOS_dupl2$after[i] == 0) & (ECOS_dupl2$redcap_event_name[i] == "During Lockdown")) {events[i] <- "During Lockdown"}
}

ECOS_dupl2 <- data.frame(cbind(ECOS_dupl2, events))

ECOS_dupl2$redcap_event_name <- ECOS_dupl2$events

# define change in clinical category
# treat categories as number for a second:
# subtract the previous category (before Lockdown) to the current class (during Lockdown)
# if no change, the result = 0
# if change in upper category:
# from <90th to 90-95th range = 1, as well as
# from 90-95th range to > 95 = 1
# from < 90th directly to > 95th = 2
# the other way (-2 or -1) if symptoms improve

#ECOS_dupl_after <- ECOS_dupl2[323:716,] #rows -> RESULTS IN ROW 325 TO 718
#ECOS_dupl_before <- ECOS_dupl2[-(323:716),] #rows

#levels(ECOS_dupl_after$redcap_event_name)[levels(ECOS_dupl_after$redcap_event_name)=="During Lockdown"] <- "After Lockdown"

#ECOS_dupl2 <- rbind(ECOS_dupl_before, ECOS_dupl_after)

#ECOS_dupl2$record_id <- as.numeric(ECOS_dupl2$record_id)

#ECOS_dupl_BF <- subset(ECOS_dupl2, record_id > 336, colnames(ECOS_dupl2))

# 
# group_by(ECOS_dupl_BF, redcap_event_name) %>%
#   summarise(
#     count = n(),
#     mean = mean(EDI3TOT, na.rm = TRUE),
#     sd = sd(EDI3TOT, na.rm = TRUE),
#   )  
# 
# ECOS_dupl_BF2 <- subset(ECOS_dupl_BF, redcap_event_name == "Before Lockdown", colnames(ECOS_dupl_BF))
# ECOS_dupl_BF3 <- subset(ECOS_dupl_BF, redcap_event_name != "Before Lockdown", colnames(ECOS_dupl_BF))
# 
# ECOS_dupl_BF2 <- cbind(ECOS_dupl_BF2, ECOS_dupl_BF3$bditot, ECOS_dupl_BF3$baitot, ECOS_dupl_BF3$ocitot,
#                       ECOS_dupl_BF3$ED_risk, ECOS_dupl_BF3$gDisadapt, ECOS_dupl_BF3$EDI3TOT)
# 
# cor.test(ECOS_dupl_BF2$bditot, ECOS_dupl_BF2$`ECOS_dupl_BF3$bditot`, method = "pearson")


# group_by(ECOS_dupl2, redcap_event_name, sex, distvit___7) %>%
#   summarise(
#     count = n(),
#     median = median(ED_risk, na.rm = TRUE),
#     quantile1st = quantile(ED_risk, probs = 0.25, na.rm = TRUE),
#     quantile3rd = quantile(ED_risk, probs= 0.75, na.rm = TRUE)
#   )  
# 
# 
# group_by(ECOS_dupl2, redcap_event_name, sex, distvit___7) %>%
#   summarise(
#     count = n(),
#     median = median(gDisadapt, na.rm = TRUE),
#     quantile1st = quantile(gDisadapt, probs = 0.25, na.rm = TRUE),
#     quantile3rd = quantile(gDisadapt, probs= 0.75, na.rm = TRUE)
#   )  
# 
# 
# group_by(ECOS_dupl2, redcap_event_name, sex,distvit___7) %>%
#   summarise(
#     count = n(),
#     median = median(bditot, na.rm = TRUE),
#     quantile1st = quantile(bditot, probs = 0.25, na.rm = TRUE),
#     quantile3rd = quantile(bditot, probs= 0.75, na.rm = TRUE)
#   )  
# 
# group_by(ECOS_dupl2, redcap_event_name, sex,distvit___7) %>%
#   summarise(
#     count = n(),
#     median = median(ocitot, na.rm = TRUE),
#     quantile1st = quantile(ocitot, probs = 0.25, na.rm = TRUE),
#     quantile3rd = quantile(ocitot, probs= 0.75, na.rm = TRUE)
#   )  
# 
# group_by(ECOS_dupl2, redcap_event_name, sex, distvit___7) %>%
#   summarise(
#     count = n(),
#     median = median(baitot, na.rm = TRUE),
#     quantile1st = quantile(baitot, probs = 0.25, na.rm = TRUE),
#     quantile3rd = quantile(baitot, probs= 0.75, na.rm = TRUE)
#   )  
# 

#  per calcolare la differenza tra gli score, innanzitutto dobbiamo avere che il
# numero di record di ecos_dupl2 matchi quello di osservazioni delle differenze tra le scale:
# abbiamo infatti che ecos_dupl2 ha 718 records ma dBDI ne ha 358 (la metà).
# quindi selezioniamo solo i recordi il cui redcap_event_name non sia "Before Lockdown"
# così potremo calcolarci le differenze negli score a seconda del periodo di interesse

ECOS_dupl2_half <- subset(ECOS_dupl2, redcap_event_name != "Before Lockdown", colnames(ECOS_dupl2))

ECOS_dupl2_half <- data.frame(cbind(ECOS_dupl2_half, dBDI, dBAI, dDisadapt, dEDI, dEHQTOT, docitot))

group_by(ECOS_dupl2_half, redcap_event_name, sex, distvit___7) %>%
  summarise(
    count = n(),
    median = median(dEDI, na.rm = TRUE),
    quantile1st = quantile(dEDI, probs = 0.25, na.rm = TRUE),
    quantile3rd = quantile(dEDI, probs= 0.75, na.rm = TRUE)
  )



ECOS_dupl2$anydiet <- as.factor(ECOS_dupl2$anydiet)
ECOS_dupl2$anyED <- as.factor(ECOS_dupl2$anyED)
ECOS_dupl2$anydisease <- as.factor(ECOS_dupl2$anydisease)
ECOS_dupl2$classBDI <- as.factor(ECOS_dupl2$classBDI)
ECOS_dupl2$classEDI <- as.factor(ECOS_dupl2$classEDI)

table(ECOS_dupl2$anydiet, ECOS_dupl2$redcap_event_name)
table(ECOS_dupl2$classBDI, ECOS_dupl2$redcap_event_name)
table(ECOS_dupl2$classEDI, ECOS_dupl2$redcap_event_name)

#Graph settings
l <- 3 # side of images
sa <- 20 # size of text
sl <- 2 # size lines
st <- 0.15 # length ticks


######################BDI################

# ggplot(ECOS_dupl2, aes(x=sex, y=EHQtotal, fill=anyED)) + 
#   #facet_wrap(~sex, ncol = 2) + # Multi plot windcows by ~ "" , scale="free"
#   geom_violin(trim=TRUE, scale = "width") + # trim=F (trim the tails of the violins)
#   #stat_boxplot(geom = "errorbar", width = 0.5) +  
#   geom_boxplot(width=0.4, lwd=1 ,outlier.size=2, outlier.shape=16, outlier.colour="black") + # ,color="white"
#   #scale_color_manual(values=c("gray20","gray60","gray90")) +
#   #geom_jitter(shape=16, position=position_jitter(0.15), size=1.5,
#              # alpha=0.5) + 
#   #stat_summary(fun.data=median_iqr, geom="pointrange", size = 1,col="gray90") +
#   
#   scale_color_manual(values= c("gray20","gray20")) + # Use custom color palettes (conditioncol)
#   # stat_summary(fun.y=mean, geom="point", shape=18, size=3, col="goldenrod4") + # Add mean
#   theme_bw() + # or minimal/grey/classic
#   theme(panel.background = element_rect(fill = "transparent", colour = NA), # or theme_blank()
#         plot.background = element_rect(fill = "transparent", colour = NA),
#         strip.background = element_blank(),
#         strip.text.x = element_text(size =20),
#         axis.text=element_text(size=25),
#         #axis.line.x = element_blank(),
#         #axis.ticks.x = element_blank(),
#         axis.line = element_line(size=sl, colour="black"),
#         axis.title.y = element_text(size=30, face="plain",colour="black", hjust= 0.35, margin = margin(t = 0, r = 5, b = 0, l = 0)),
#         axis.title.x = element_text(size=30, face="plain",colour="black", margin = margin(t = 5, r = 0, b = 0, l = 0)),
#         axis.ticks = element_line(size = sl, colour="black"),
#         axis.ticks.length = unit(st,"cm"),
#         axis.text.x = element_text(angle = 45, vjust = 1, size = 25, hjust = 1,family="sans",face = "plain"),
#         #axis.text.y = element_text(size = sa,family="sans",face = "plain", colour="black", hjust = 1), #hjust = 0.5
#         #axis.text.y = element_blank(),
#         panel.grid.major.x = element_blank(),
#         panel.grid.major.y = element_blank(),
#         panel.grid = element_blank(),
#         panel.border = element_blank(),
#         legend.position="none") +
#   theme(aspect.ratio=2) +
#   coord_capped_cart(bottom="left",left="top", gap=0.5) +
#   expand_limits(x= 0, y=0) +
#   #scale_x_discrete(expand = c(0,0.8)) +
#   scale_y_continuous(breaks = seq(0,50,10))+
#   labs(y = "BDI-2 Score") +
#   labs(x = "")
# 
# ################################
# #output table
# #event_name = before/during Lockdown
# #eta = age
# #consider age during Lockdown
# ECOS_dupl3 <- subset(ECOS_dupl2, redcap_event_name != "Before Lockdown", colnames(ECOS_dupl2))
# 
# group_by(ECOS_dupl3) %>%
#   summarise(
#     count = n(),
#     mean = mean(eta, na.rm = TRUE),
#     sd = sd(eta, na.rm = TRUE)
#   )
# 
# 
# group_by(ECOS_dupl3, sex) %>%
#   summarise(
#     count = n(),
#     mean = mean(eta, na.rm = TRUE),
#     sd = sd(eta, na.rm = TRUE)
#   )
# 
# 
# #distvit___7 if == 0 --> medical diagnosis of (at least) a mental disorder
# #distvit___7 elseif == 1 --> negative history for mental disorders
# group_by(ECOS_dupl3, sex,distvit___7) %>%
#   summarise(
#     count = n(),
#     mean = mean(eta, na.rm = TRUE),
#     sd = sd(eta, na.rm = TRUE)
#   )
# 
# 

#generalised linear mixed models for variable control in BDI score
#(1|record_id) is the random effect accounting for correlation of data
#of paired samples

ECOS_dupl2$clinicalEDRISK <- as.factor(ECOS_dupl2$clinicalEDRISK)

m0 <- glmer(EHQtotal ~ 1 + (1|eta),  family = "poisson", ECOS_dupl2)
m1 <- glmer(EHQtotal ~ sex + (1|eta),  family = "poisson", ECOS_dupl2)
m2 <- glmer(EHQtotal ~ sex + anyED + (1|eta),  family = "poisson", ECOS_dupl2)
m3 <- glmer(EHQtotal ~ sex + anyED + distvit___7 + (1|eta),  family = "poisson", ECOS_dupl2)
m4 <- glmer(EHQtotal ~ sex + anyED + anydiet + (1|eta),  family = "poisson", ECOS_dupl2)
m5 <- glmer(EHQtotal ~ sex + anyED + anydiet + anydisease + (1|eta),  family = "poisson", ECOS_dupl2)
m6 <- glmer(EHQtotal ~ sex + anyED + anydiet + anydisease + classEDI + (1|eta),  family = "poisson", ECOS_dupl2)
m7 <- glmer(EHQtotal ~ sex + anyED + anydiet + anydisease + clinicalEDRISK + (1|eta),  family = "poisson", ECOS_dupl2)

# bmi, peso, famdist non aggiungono
# meglio ed risk che edi tot e meglio ed risk di gdisadapt
# meglio ancora se dividi ed risk per categorie

anova(m6,m7)
plot(allEffects(m7))
summary(m7)
r.squaredGLMM(m7)

# dopo aver tenuto in considerazione tutti gli altri fattori (anyED, anyDisease) il sesso non ? un fattore
# importante

m8 <- glmer(EHQtotal ~ anyED + anydiet + anydisease + clinicalEDRISK + (1|eta),  family = "poisson", ECOS_dupl2)
anova(m7,m8)
summary(m8)
r.squaredGLMM(m8)
plot(allEffects(m8))


m9 <- glmer(EHQtotal ~ distvit___1 + anydiet + anydisease + clinicalEDRISK + (1|eta),  family = "poisson", ECOS_dupl2)
anova(m8,m9)
summary(m9)

#anoressia nervosa ? l'unico disturbo che altera il EHQTOT

# per singoli disturbi????

e8 <- glmer(EHQconvictions ~ anydiet + (1|eta),  family = "poisson", ECOS_dupl2)
e9 <- glmer(EHQconvictions ~ anydiet + bmi + (1|eta),  family = "poisson", ECOS_dupl2)
e10 <- glmer(EHQconvictions ~ anydiet + bmi +(1|eta),  family = "poisson", ECOS_dupl2) 

# no anydisease/ no anyED e neanche i singoli 

anova(e9,e10)
summary(e10)
r.squaredGLMM(e10)
plot(allEffects(e10))

########################################

e8 <- glmer(EHQemotions ~ anydiet + clinicalEDRISK + (1|eta),  family = "poisson", ECOS_dupl2)
e9 <- glmer(EHQemotions ~ anydiet + clinicalEDRISK + ocitot + (1|eta),  family = "poisson", ECOS_dupl2)
#ocitot si ma quanto ? rilevante? Categorie di oci?

anova(e8,e9)
summary(e9)
r.squaredGLMM(e9)
plot(allEffects(e9))


# 
# m2 <- glmer(bditot ~ sex:redcap_event_name + sex + (1|record_id)  + (1|eta),  family = "poisson", ECOS_dupl2)
# anova(m1,m2)
# plot(allEffects(m2))
# summary(m2)
# 
# #check if sex, per se, is not significant twice
# #sexModel <- glmer(bditot ~ sex + (1|record_id) + (1|eta),  family = "poisson", ECOS_dupl2)
# #anova(m0,sexModel)
# 
# m3 <- glmer(bditot ~ distvit___7:redcap_event_name + distvit___7 + (1|record_id) + (1|sex) + (1|eta),  family = "poisson", ECOS_dupl2)
# 
# anova(m1,m3)
# plot(allEffects(m3))
# summary(m3)
# 
# 
# m4 <- glmer(bditot ~ distvit___7:redcap_event_name:sex + (1|record_id) + (1|eta),  family = "poisson", ECOS_dupl2)
# 
# anova(m1,m4)
# plot(allEffects(m4))
# summary(m4)

# 
# ECOS_test <- subset(ECOS_dupl2, redcap_event_name == "Before Lockdown", colnames(ECOS_dupl2))
# ECOS_test$after <- as.factor(ECOS_test$after)
# ntest0 <- glmer(bditot ~ 1 + (1|record_id) + (1|eta),  family = "poisson", ECOS_test)
# ntest1 <- glmer(bditot ~ after + (1|record_id) + (1|eta),  family = "poisson", ECOS_test)
# 
# anova(ntest0,ntest1)
# plot(allEffects(ntest1))
# summary(ntest1)

levels(ECOS_dupl2$anyED)[levels(ECOS_dupl2$anyED)=="1"] <- "Any ED"
levels(ECOS_dupl2$anyED)[levels(ECOS_dupl2$anyED)=="0"] <- "No ED"

ggplot(ECOS_dupl2, aes(x=anyED, y=EHQemotions)) + 
  facet_grid(~sex) + # Multi plot windcows by ~ "" , scale="free"
  geom_violin(trim=TRUE, scale = "width", fill = "gray") + # trim=F (trim the tails of the violins)
  scale_fill_manual(values=c("gray50","gray50")) +
  #stat_boxplot(geom = "errorbar", width = 0.5) +  
  geom_boxplot(width=0.4, lwd=1 ,outlier.size=2.5, outlier.shape=8, outlier.colour="black",position = position_dodge(width=0)) + # ,color="white"
  #geom_jitter(shape=16, position=position_jitter(0.15), size=1.5,
  # alpha=0.5) + 
  stat_summary(fun =mean, geom="point", size = 1,col="red") +

#  scale_color_manual(values= c("gray20","gray20","gray20")) + # Use custom color palettes (conditioncol)
  # stat_summary(fun.y=mean, geom="point", shape=18, size=3, col="goldenrod4") + # Add mean
  theme_bw() + # or minimal/grey/classic
  theme(panel.background = element_rect(fill = "transparent", colour = NA), # or theme_blank()
        plot.background = element_rect(fill = "transparent", colour = NA),
        strip.background = element_blank(),
        strip.text.x = element_text(size =30),
        strip.text.y = element_text(size=30),
        axis.text=element_text(size=35),
        #axis.line.x = element_blank(),
        #axis.ticks.x = element_blank(),
        axis.line = element_line(size=sl, colour="black"),
        axis.title.y = element_text(size=45, face="plain",colour="black", hjust= 0.5, vjust = 2, margin = margin(t = 0, r = 5, b = 0, l = 0)),
        axis.title.x = element_text(size=30, face="plain",colour="black", margin = margin(t = 5, r = 0, b = 0, l = 0)),
        axis.ticks = element_line(size = sl, colour="black"),
        axis.ticks.length = unit(st,"cm"),
        axis.text.x = element_text(angle = 45, vjust = 1, size = 30, hjust = 1,family="sans",face = "plain", colour="black"),
        axis.text.y = element_text(size = 40,family="sans",face = "plain", colour="black", hjust = 1), #hjust = 0.5
        #axis.text.y = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid = element_blank(),
        panel.border = element_blank(),
        legend.position="none") +
  theme(aspect.ratio=2) +
  coord_capped_cart(bottom="left",left="top", gap=0.5) +
  expand_limits(x= 0, y=0) +
  #scale_x_discrete(expand = c(0,0.8)) +
  scale_y_continuous(breaks = seq(0,40,10))+
  ylim(0,20)+
  labs(y = "EHQ Feelings") +
  labs(x = "")


#Plot differences in BDI scores

diffplot <- subset(ECOS_dupl2, redcap_event_name == "During Lockdown" | redcap_event_name == "After Lockdown", colnames(ECOS_dupl2)) 

diffplot <- data.frame(cbind(diffplot, dBDI, dBAI, docitot, dEHQTOT, dEDI, dDisadapt))

diffplot$record_id <- as.factor(diffplot$record_id)

ggplot(diffplot, aes(x=redcap_event_name, y=dBDI)) + 
  facet_grid(sex~distvit___7) + # Multi plot windcows by ~ "" , scale="free"
  geom_violin(trim=TRUE, scale = "width", fill = "gray") + # trim=F (trim the tails of the violins)
  scale_fill_manual(values=c("gray50","gray50")) +
  #stat_boxplot(geom = "errorbar", width = 0.5) +  
  geom_boxplot(width=0.4, lwd=1 ,outlier.size=2.5, outlier.shape=8, outlier.colour="black",position = position_dodge(width=0)) + # ,color="white"
  #scale_fill_manual(values=c("gray10","gray10")) +
  #geom_jitter(shape=16, position=position_jitter(0.15), size=1.5,
  # alpha=0.5) + 
  stat_summary(fun.y =mean, geom="point", size = 1,col="red") +
  geom_hline(yintercept=0, linetype="dashed", 
             color = "gray20", size=1) +
  
  #  scale_color_manual(values= c("gray20","gray20","gray20")) + # Use custom color palettes (conditioncol)
  # stat_summary(fun.y=mean, geom="point", shape=18, size=3, col="goldenrod4") + # Add mean
  theme_bw() + # or minimal/grey/classic
  theme(panel.background = element_rect(fill = "transparent", colour = NA), # or theme_blank()
        plot.background = element_rect(fill = "transparent", colour = NA),
        strip.background = element_blank(),
        strip.text.x = element_text(size =30),
        strip.text.y = element_text(size=30),
        axis.text=element_text(size=35),
        #axis.line.x = element_blank(),
        #axis.ticks.x = element_blank(),
        axis.line = element_line(size=sl, colour="black"),
        axis.title.y = element_text(size=45, face="plain",colour="black", hjust= 0.5, margin = margin(t = 0, r = 5, b = 0, l = 0)),
        axis.title.x = element_text(size=30, face="plain",colour="black", margin = margin(t = 5, r = 0, b = 0, l = 0)),
        axis.ticks = element_line(size = sl, colour="black"),
        axis.ticks.length = unit(st,"cm"),
        axis.text.x = element_text(angle = 45, vjust = 1, size = 30, hjust = 1,family="sans",face = "plain"),
        #axis.text.y = element_text(size = sa,family="sans",face = "plain", colour="black", hjust = 1), #hjust = 0.5
        #axis.text.y = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid = element_blank(),
        panel.border = element_blank(),
        legend.position="none") +
  theme(aspect.ratio=2) +
  coord_capped_cart(bottom="left",left="top", gap=0.5) +
  expand_limits(x= 0, y=0) +
  #scale_x_discrete(expand = c(0,0.8)) +
  scale_y_continuous(breaks = seq(-30,30,15))+
  labs(y = "Difference BDI-2 Score") +
  labs(x = "")

#################################

#linear mixed models for variable control in % delta BDI score
group_by(diffplot, redcap_event_name) %>%
  summarise(
    count = n(),
    median = median(dBDI, na.rm = TRUE),
    quantile1st = quantile(dBDI, probs = 0.25, na.rm = TRUE),
    quantile3rd = quantile(dBDI, probs= 0.75, na.rm = TRUE)
  )

group_by(diffplot, redcap_event_name, sex) %>%
  summarise(
    count = n(),
    median = median(dBDI, na.rm = TRUE),
    quantile1st = quantile(dBDI, probs = 0.25, na.rm = TRUE),
    quantile3rd = quantile(dBDI, probs= 0.75, na.rm = TRUE)
  )

group_by(diffplot, redcap_event_name, sex, distvit___7) %>%
  summarise(
    count = n(),
    median = median(dBDI, na.rm = TRUE),
    quantile1st = quantile(dBDI, probs = 0.25, na.rm = TRUE),
    quantile3rd = quantile(dBDI, probs= 0.75, na.rm = TRUE)
  )


#######################
#bdi9 suicidality

ECOS_dupl2$suic <- as.factor(ECOS_dupl2$bdi9)

table(ECOS_dupl2$suic, ECOS_dupl2$redcap_event_name)
#####################################################################################
#####CHECK FOR EXPLANATORY VARIABLES OF BDI SCORE DURING Lockdown AND dBDIbditot#####

corbdiplotBEFORE <- subset(ECOS_dupl2, redcap_event_name == "Before Lockdown", colnames(ECOS_dupl2)) 
corbdiplotNOW <- subset(ECOS_dupl2, redcap_event_name != "Before Lockdown", colnames(ECOS_dupl2)) 

corbdiplotNOW$BDILockdown <- corbdiplotNOW$bditot

corbdiplot <- data.frame(cbind(corbdiplotBEFORE, corbdiplotNOW$BDILockdown))

corbdiplot$BDILockdown <- corbdiplot$corbdiplotNOW.BDILockdown

corbdiplot <- data.frame(cbind(corbdiplot, dBDI))

ggscatter(corbdiplot, x = "bditot", y = "dBDI", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.coef.size = 6, cor.method = "kendall", color = "sex", palette = c("gray20", "gray20"),
          xlab = "BDI-2 Score Before Lockdown", ylab = "Difference BDI-2 Score", add.params = list(color = "black",
                                                                                           fill = "gray70"),
          ggtheme = theme(panel.background = element_rect(fill = "transparent", colour = NA), # or theme_blank()
                          plot.background = element_rect(fill = "transparent", colour = NA),
                          strip.background = element_blank(),
                          strip.text.x = element_text(size =20),
                          axis.text=element_text(size=25),
                          #axis.line.x = element_blank(),
                          #axis.ticks.x = element_blank(),
                          axis.line = element_line(size=sl, colour="gray60"),
                          axis.title.y = element_text(size=30, face="plain",colour="black", margin = margin(t = 0, r = 5, b = 0, l = 0)),
                          axis.title.x = element_text(size=30, face="plain",colour="black", margin = margin(t = 5, r = 0, b = 0, l = 0)),
                          axis.ticks = element_line(size = sl, colour="black"),
                          axis.ticks.length = unit(st,"cm"),
                          axis.text.x = element_text(vjust = 1, size = 25, hjust = 1,family="sans",face = "plain"),
                          #axis.text.y = element_text(size = sa,family="sans",face = "plain", colour="black", hjust = 1), #hjust = 0.5
                          #axis.text.y = element_blank(),
                          panel.grid.major.x = element_blank(),
                          panel.grid.major.y = element_blank(),
                          panel.grid = element_blank(),
                          panel.border = element_blank(),
                          legend.position="none"))


#regression: does bdi before Lockdown explains delta, at least partially?
n0 <- lmer(dBDI ~ 1 + (1|sex), corbdiplotNOW)
n1 <- lmer(dBDI ~ redcap_event_name + (1|sex), corbdiplotNOW)

anova(n0,n1)
summary(n1)
plot(allEffects(n1))

n0 <- lmer(dBDI ~ sex:distvit___7 + (1|eta), corbdiplotNOW)
n1 <- lmer(dBDI ~ redcap_event_name:sex:distvit___7 + (1|eta), corbdiplotNOW)

anova(n0,n1)
summary(n1)
plot(allEffects(n1))


m0 <- lmer(dBDI ~ 1 + (1|sex), corbdiplot)
m1 <- lmer(dBDI ~ bditot + (1|sex), corbdiplot)

anova(m0,m1)
summary(m1)
plot(allEffects(m1))


##############################################
#predict bdi increase with data from 6 months ago
#variables already stored as factors

m0 <- lmer(dBDI ~  + (1|sex), corbdiplot)
m1 <- lmer(dBDI ~ distvit___7 + (1|sex) , corbdiplot)

plot(allEffects(m1))
anova(m0,m1)

m0 <- lmer(dBDI ~ 1  + (1|sex), corbdiplot)
m1 <- lmer(dBDI ~ peconom  + (1|sex), corbdiplot)

anova(m0,m1)
plot(allEffects(m1))

m0 <- lmer(dBDI ~ 1  + (1|sex), corbdiplot)
m1 <- lmer(dBDI ~ famdist  + (1|sex), corbdiplot)

anova(m0,m1)
plot(allEffects(m1))

#age
m0 <- lmer(dBDI ~ 1 + (1|sex), corbdiplot)
m1 <- lmer(dBDI ~ eta + (1|sex), corbdiplot)

anova(m0,m1)
summary(m1)
plot(allEffects(m1))


#geographical residence
m0 <- lmer(dBDI ~ 1 + (1|sex), corbdiplot)
m1 <- lmer(dBDI ~ reside + (1|sex), corbdiplot)

anova(m0,m1)

m0 <- lmer(dBDI ~ 1   + (1|eta), corbdiplot)
m1 <- lmer(dBDI ~ sex  + (1|eta), corbdiplot)

anova(m0,m1)


clinicalchange_t <- subset(ECOS_dupl2, after == 0, colnames(ECOS_dupl2))

clinicalchange_t <- subset(clinicalchange_t, redcap_event_name == "Before Lockdown", colnames(clinicalchange_t))

table(clinicalchange_t$clinicalBDI, clinicalchange_t$diff_clinicalBDI, clinicalchange_t$sex, clinicalchange_t$distvit___7)

clinicalchange <- subset(ECOS_dupl2, redcap_event_name != "Before Lockdown", colnames(ECOS_dupl2))
clinicalchange <- data.frame(cbind(clinicalchange,dBDI))

clinicalchange <- subset(clinicalchange, redcap_event_name == "During Lockdown", colnames(clinicalchange))

clinicalchange$clinicalBDI <- as.factor(clinicalchange$clinicalBDI)
clinicalchange <- clinicalchange[,-(289)] #col of diff_clinical bdi
clinicalchange$diff_clinicalBDI <- clinicalchange_t$diff_clinicalBDI
clinicalchange$diff_clinicalBDI <- as.factor(clinicalchange$diff_clinicalBDI)

#we are interested in those whose symptoms get worse

levels(clinicalchange$clinicalBDI)[levels(clinicalchange$clinicalBDI)=="0"] <- "Mild"
levels(clinicalchange$clinicalBDI)[levels(clinicalchange$clinicalBDI)=="1"] <- "Moderate"
levels(clinicalchange$clinicalBDI)[levels(clinicalchange$clinicalBDI)=="2"] <- "Severe"

#ECOS_dupl_before <- subset(ECOS_dupl_before, redcap_event_name == "Before Lockdown", colnames(ECOS_dupl_before))
#clinicalchange <- data.frame(cbind(clinicalchange, ECOS_dupl_before$dBDI))

#clinicalchange <- data.frame(cbind(clinicalchange, corbdiplot$BDILockdown))

#using nnet package to run multinomial regression
m0 <- multinom(clinicalchange$diff_clinicalBDI ~ 1, clinicalchange)
summary(m0)

m1 <- multinom(clinicalchange$diff_clinicalBDI ~ clinicalchange$dBDI, clinicalchange)
summary(m1)

#######
P <- predict(m1, newdata = clinicalchange$dBDI,  "probs")

pp <- data.frame(cbind(P, clinicalchange$dBDI))


#summarise the probabilities per clinical category
# -2
group_by(pp) %>%
  summarise(
    count = n(),
    median = median(X.2, na.rm = TRUE),
    quantile1st = quantile(X.2, probs = 0.25, na.rm = TRUE),
    quantile3rd = quantile(X.2, probs= 0.75, na.rm = TRUE)
  )

#-1
group_by(pp) %>%
  summarise(
    count = n(),
    median = median(X.1, na.rm = TRUE),
    quantile1st = quantile(X.1, probs = 0.25, na.rm = TRUE),
    quantile3rd = quantile(X.1, probs= 0.75, na.rm = TRUE)
  )

#0
group_by(pp) %>%
  summarise(
    count = n(),
    median = median(X0, na.rm = TRUE),
    quantile1st = quantile(X0, probs = 0.25, na.rm = TRUE),
    quantile3rd = quantile(X0, probs= 0.75, na.rm = TRUE)
  )

#+1
group_by(pp) %>%
  summarise(
    count = n(),
    median = median(X1, na.rm = TRUE),
    quantile1st = quantile(X1, probs = 0.25, na.rm = TRUE),
    quantile3rd = quantile(X1, probs= 0.75, na.rm = TRUE)
  )

#+2
group_by(pp) %>%
  summarise(
    count = n(),
    median = median(X2, na.rm = TRUE),
    quantile1st = quantile(X2, probs = 0.25, na.rm = TRUE),
    quantile3rd = quantile(X2, probs= 0.75, na.rm = TRUE)
  )

#dBDI
group_by(pp) %>%
  summarise(
    count = n(),
    median = median(V6, na.rm = TRUE),
    quantile1st = quantile(V6, probs = 0.25, na.rm = TRUE),
    quantile3rd = quantile(V6, probs= 0.75, na.rm = TRUE)
  )

ggplot(pp, aes(x=V6, y=X0)) + 
  #facet_wrap(V8 ~ V7, ncol = 2) + # Multi plot windcows by ~ "" , scale="free"
  #geom_violin(trim=TRUE, scale = "width") + # trim=F (trim the tails of the violins)
  #geom_hline(aes(yintercept = meanmale), color = "deepskyblue3",size = 1)+
  #geom_hline(aes(yintercept = meanfemale), color = "deeppink2", size = 1)+
  #geom_hline(aes(yintercept = 0), color = "gray30", size = 1, linetype = "dashed")+
  #geom_boxplot(width=0.85, lwd=1 ,outlier.size=0.5, outlier.shape=16, outlier.colour="transparent",position = position_dodge(width=0)) + # ,color="white"
  scale_color_manual(values=c("deeppink2","deepskyblue3")) +
  annotate("rect", xmin = -4, xmax = 5, ymin=0, ymax=0.95, color = "gray90", fill = "gray90") +
  geom_jitter(shape=16, position=position_jitter(0.15), size=3,
              alpha=0.5, color = "gray10") +
  geom_line()+
  #stat_summary(fun.data=median_mad, geom="pointrange", size = 1,col="gray2") +
  
  #  scale_color_manual(values= c("gray20","gray20","gray20")) + # Use custom color palettes (conditioncol)
  #stat_summary(fun.y=mean, geom="pointrange", shape=18, size=3, col="goldenrod4") + # Add mean
  theme_bw() + # or minimal/grey/classic
  theme(panel.background = element_rect(fill = "transparent", colour = NA), # or theme_blank()
        plot.background = element_rect(fill = "transparent", colour = NA),
        strip.background = element_blank(),
        #strip.text.x = element_text(size =20),
        axis.text=element_text(size=25),
        #axis.line.x = element_blank(),
        #axis.ticks.x = element_blank(),
        axis.line = element_line(size=sl, colour="black"),
        axis.title.y = element_text(size=30, face="plain",colour="black", margin = margin(t = 0, r = 5, b = 0, l = 0)),
        axis.title.x = element_text(size=30, face="plain",colour="black", margin = margin(t = 5, r = 0, b = 0, l = 0), hjust = 0.5),
        axis.ticks.y = element_line(size = sl, colour="black"),
        axis.ticks.x = element_blank(),
        axis.ticks.length = unit(st,"cm"),
        axis.text.x = element_text(angle = 45,face="plain",colour="black", margin = margin(t = 0, r = 5, b = 0, l = 0), vjust = 1, hjust =1),
        #axis.text.y = element_text(size = sa,family="sans",face = "plain", colour="black", hjust = 1), #hjust = 0.5
        #axis.text.y = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid = element_blank(),
        panel.border = element_blank(),
        legend.position="none") +
  theme(aspect.ratio=1) +
  coord_capped_cart(bottom="left",left="top", gap=0.5) +
  scale_x_continuous(breaks = c(-20,0,20)) +
  ylim(0,1)+
  labs(y = "P(clinical stability)") +
  #annotate("rect", xmin = 661, xmax = 990, ymin=0, ymax=2.5, color = "orangered1", fill = "orangered1") +
  labs(x = "Difference BDI-2 Score")


ggplot(pp, aes(x=V6, y=X0)) + 
 # facet_wrap(V8~ V7, ncol = 2) + # Multi plot windcows by ~ "" , scale="free"
  #geom_violin(trim=TRUE, scale = "width") + # trim=F (trim the tails of the violins)
  #geom_hline(aes(yintercept = meanmale), color = "deepskyblue3",size = 1)+
  #geom_hline(aes(yintercept = meanfemale), color = "deeppink2", size = 1)+
  #geom_hline(aes(yintercept = 0), color = "gray30", size = 1, linetype = "dashed")+
  #geom_boxplot(width=0.85, lwd=1 ,outlier.size=0.5, outlier.shape=16, outlier.colour="transparent",position = position_dodge(width=0)) + # ,color="white"
  scale_color_manual(values=c("deeppink2","deepskyblue3")) +
  annotate("rect", xmin = -4, xmax = 5, ymin=0, ymax=0.95, color = "gray90", fill = "gray90") +
  geom_line(aes(x = V6, y = X1))+
  geom_line(aes(x=V6, y =X2)) +
  geom_jitter(aes(x= V6, y= X1), shape=1, position=position_jitter(0.15), size=3,
              alpha=0.5, color = "gray20") +
  geom_jitter(aes(x= V6, y= X2), shape=2, position=position_jitter(0.15), size=3,
              alpha=0.5, color = "gray20") +
  #stat_summary(fun.data=median_mad, geom="pointrange", size = 1,col="gray2") +
  
  #  scale_color_manual(values= c("gray20","gray20","gray20")) + # Use custom color palettes (conditioncol)
  #stat_summary(fun.y=mean, geom="pointrange", shape=18, size=3, col="goldenrod4") + # Add mean
  theme_bw() + # or minimal/grey/classic
  theme(panel.background = element_rect(fill = "transparent", colour = NA), # or theme_blank()
        plot.background = element_rect(fill = "transparent", colour = NA),
        strip.background = element_blank(),
        #strip.text.x = element_text(size =20),
        axis.text=element_text(size=25),
        #axis.line.x = element_blank(),
        #axis.ticks.x = element_blank(),
        axis.line = element_line(size=sl, colour="black"),
        axis.title.y = element_text(size=30, face="plain",colour="black", margin = margin(t = 0, r = 5, b = 0, l = 0)),
        axis.title.x = element_text(size=30, face="plain",colour="black", margin = margin(t = 5, r = 0, b = 0, l = 0), hjust = 0.5),
        axis.ticks.y = element_line(size = sl, colour="black"),
        axis.ticks.x = element_blank(),
        axis.ticks.length = unit(st,"cm"),
        axis.text.x = element_text(angle = 45,face="plain",colour="black", margin = margin(t = 0, r = 5, b = 0, l = 0), vjust = 1, hjust =1),
        #axis.text.y = element_text(size = sa,family="sans",face = "plain", colour="black", hjust = 1), #hjust = 0.5
        #axis.text.y = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid = element_blank(),
        panel.border = element_blank(),
        legend.position="none") +
  theme(aspect.ratio=1) +
  coord_capped_cart(bottom="left",left="top", gap=0.5) +
  scale_x_continuous(breaks = c(-20,0,20)) +
  ylim(0,1)+
  labs(y = "P(clinical change)") +
  #annotate("rect", xmin = 661, xmax = 990, ymin=0, ymax=2.5, color = "orangered1", fill = "orangered1") +
  labs(x = "Difference BDI-2 Score")

##############################################

#EDI 3
#############################ED_RISK############################

ggplot(ECOS_dupl2, aes(x=redcap_event_name, y=ED_risk)) + 
  facet_grid(sex~distvit___7) + # Multi plot windcows by ~ "" , scale="free"
  geom_violin(trim=TRUE, scale = "width", fill = "gray") + # trim=F (trim the tails of the violins)
  scale_fill_manual(values=c("gray50","gray50")) +
  #stat_boxplot(geom = "errorbar", width = 0.5) +  
  geom_boxplot(width=0.4, lwd=1 ,outlier.size=2, outlier.shape=16, outlier.colour="black",position = position_dodge(width=0)) + # ,color="white"
  scale_fill_manual(values=c("gray10","gray10")) +
  #geom_jitter(shape=16, position=position_jitter(0.15), size=1.5,
  # alpha=0.5) + 
  stat_summary(fun.y =mean, geom="point", size = 1,col="red") +
  
  #  scale_color_manual(values= c("gray20","gray20","gray20")) + # Use custom color palettes (conditioncol)
  # stat_summary(fun.y=mean, geom="point", shape=18, size=3, col="goldenrod4") + # Add mean
  theme_bw() + # or minimal/grey/classic
  theme(panel.background = element_rect(fill = "transparent", colour = NA), # or theme_blank()
        plot.background = element_rect(fill = "transparent", colour = NA),
        strip.background = element_blank(),
        strip.text.x = element_text(size =30),
        strip.text.y = element_text(size=30),
        axis.text=element_text(size=35),
        #axis.line.x = element_blank(),
        #axis.ticks.x = element_blank(),
        axis.line = element_line(size=sl, colour="black"),
        axis.title.y = element_text(size=45, face="plain",colour="black", hjust= 0.5, vjust = 2, margin = margin(t = 0, r = 5, b = 0, l = 0)),
        axis.title.x = element_text(size=30, face="plain",colour="black", margin = margin(t = 5, r = 0, b = 0, l = 0)),
        axis.ticks = element_line(size = sl, colour="black"),
        axis.ticks.length = unit(st,"cm"),
        axis.text.x = element_text(angle = 45, vjust = 1, size = 30, hjust = 1,family="sans",face = "plain"),
        #axis.text.y = element_text(size = sa,family="sans",face = "plain", colour="black", hjust = 1), #hjust = 0.5
        #axis.text.y = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid = element_blank(),
        panel.border = element_blank(),
        legend.position="none") +
  theme(aspect.ratio=2) +
  coord_capped_cart(bottom="left",left="top", gap=0.5) +
  expand_limits(x= 0, y=0) +
  #scale_x_discrete(expand = c(0,0.8)) +
  #scale_y_continuous(breaks = seq(0,50,10))+
  labs(y = "ED Risk Score") +
  labs(x = "")


m0 <- glmer(ED_risk ~ 1 + (1|record_id) + (1|eta),  family = "poisson", ECOS_dupl2)
m1 <- glmer(ED_risk ~ redcap_event_name + (1|record_id) + (1|eta),  family = "poisson", ECOS_dupl2)

anova(m0,m1)
plot(allEffects(m1))
summary(m1)

m2 <- glmer(ED_risk ~ redcap_event_name:sex + sex + (1|record_id)  + (1|eta),  family = "poisson", ECOS_dupl2)
anova(m1,m2)
plot(allEffects(m2))
summary(m2)

#check if sex, per se, is not significant twice
#sexModel <- glmer(bditot ~ sex + (1|record_id)  + (1|eta),  family = "poisson", ECOS_dupl2)
#anova(m0,sexModel)

m3 <- glmer(ED_risk ~ redcap_event_name:distvit___7 + distvit___7 + (1|record_id)+ (1|sex) + (1|eta),  family = "poisson", ECOS_dupl2)


anova(m1,m3)
plot(allEffects(m3))
summary(m3)

m4 <- glmer(ED_risk ~ redcap_event_name:anydiet + anydiet + (1|record_id)+ (1|sex) + (1|eta), family = "poisson", ECOS_dupl2)
anova(m1,m4)
plot(allEffects(m4))
summary(m4)

m5 <- glmer(ED_risk ~ redcap_event_name:anydisease + anydisease + (1|record_id)+ (1|sex) + (1|eta), family = "poisson", ECOS_dupl2)
anova(m1,m5)
plot(allEffects(m5))
summary(m5)

m6 <- glmer(ED_risk ~ redcap_event_name:anyED + anyED + (1|record_id) + (1|eta) + (1|sex), family = "poisson", ECOS_dupl2)
anova(m1,m6)
plot(allEffects(m6))
summary(m6)

###############################GENERAL DISADAPT#################################################

ggplot(ECOS_dupl2, aes(x=redcap_event_name, y=gDisadapt)) + 
  facet_grid(sex~distvit___7) + # Multi plot windcows by ~ "" , scale="free"
  geom_violin(trim=TRUE, scale = "width", fill = "gray") + # trim=F (trim the tails of the violins)
  scale_fill_manual(values=c("gray50","gray50")) +
  #stat_boxplot(geom = "errorbar", width = 0.5) +  
  geom_boxplot(width=0.4, lwd=1 ,outlier.size=2, outlier.shape=16, outlier.colour="black",position = position_dodge(width=0)) + # ,color="white"
  scale_fill_manual(values=c("gray10","gray10")) +
  #geom_jitter(shape=16, position=position_jitter(0.15), size=1.5,
  # alpha=0.5) + 
  stat_summary(fun.y =mean, geom="point", size = 1,col="red") +
  
  #  scale_color_manual(values= c("gray20","gray20","gray20")) + # Use custom color palettes (conditioncol)
  # stat_summary(fun.y=mean, geom="point", shape=18, size=3, col="goldenrod4") + # Add mean
  theme_bw() + # or minimal/grey/classic
  theme(panel.background = element_rect(fill = "transparent", colour = NA), # or theme_blank()
        plot.background = element_rect(fill = "transparent", colour = NA),
        strip.background = element_blank(),
        strip.text.x = element_text(size =30),
        strip.text.y = element_text(size=30),
        axis.text=element_text(size=35),
        #axis.line.x = element_blank(),
        #axis.ticks.x = element_blank(),
        axis.line = element_line(size=sl, colour="black"),
        axis.title.y = element_text(size=45, face="plain",colour="black", hjust= 0.5, vjust = 2, margin = margin(t = 0, r = 5, b = 0, l = 0)),
        axis.title.x = element_text(size=30, face="plain",colour="black", margin = margin(t = 5, r = 0, b = 0, l = 0)),
        axis.ticks = element_line(size = sl, colour="black"),
        axis.ticks.length = unit(st,"cm"),
        axis.text.x = element_text(angle = 45, vjust = 1, size = 30, hjust = 1,family="sans",face = "plain"),
        #axis.text.y = element_text(size = sa,family="sans",face = "plain", colour="black", hjust = 1), #hjust = 0.5
        #axis.text.y = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid = element_blank(),
        panel.border = element_blank(),
        legend.position="none") +
  theme(aspect.ratio=2) +
  coord_capped_cart(bottom="left",left="top", gap=0.5) +
  expand_limits(x= 0, y=0) +
  #scale_x_discrete(expand = c(0,0.8)) +
  #scale_y_continuous(breaks = seq(0,50,10))+
  labs(y = "Maladjustment Score") +
  labs(x = "")

m0 <- glmer(gDisadapt ~ 1 + (1|record_id) + (1|eta),  family = "poisson", ECOS_dupl2)
m1 <- glmer(gDisadapt ~ redcap_event_name + (1|record_id) + (1|eta),  family = "poisson", ECOS_dupl2)

anova(m0,m1)
plot(allEffects(m1))
summary(m1)

m2 <- glmer(gDisadapt ~ redcap_event_name:sex + sex + (1|record_id)  + (1|eta),  family = "poisson", ECOS_dupl2)
anova(m1,m2)
plot(allEffects(m2))
summary(m2)

#check if sex, per se, is not significant twice
#sexModel <- glmer(bditot ~ sex + (1|record_id)  + (1|eta),  family = "poisson", ECOS_dupl2)
#anova(m0,sexModel)

m3 <- glmer(gDisadapt ~ redcap_event_name:distvit___7 + distvit___7 + (1|sex)+ (1|record_id) + (1|eta),  family = "poisson", ECOS_dupl2)

anova(m1,m3)
plot(allEffects(m3))
summary(m3)


m6 <- glmer(gDisadapt ~ redcap_event_name:anyED + anyED + (1|record_id) + (1|eta) + (1|sex),  family = "poisson", ECOS_dupl2)

anova(m1,m6)
plot(allEffects(m6))
summary(m6)

#Plot differences in EDI scores

diffplotNOW <- subset(ECOS_dupl2, redcap_event_name != "Before Lockdown", colnames(ECOS_dupl2)) 

diffplotNOW <- data.frame(cbind(diffplotNOW, dEDI))

diffplotNOW$record_id <- as.factor(diffplotNOW$record_id)


ggplot(diffplotNOW, aes(x=redcap_event_name, y=dEDI)) + 
  facet_grid(sex~distvit___7) + # Multi plot windcows by ~ "" , scale="free"
  geom_violin(trim=TRUE, scale = "width", fill = "gray") + # trim=F (trim the tails of the violins)
  scale_fill_manual(values=c("gray50","gray50")) +
  #stat_boxplot(geom = "errorbar", width = 0.5) +  
  geom_boxplot(width=0.4, lwd=1 ,outlier.size=2, outlier.shape=16, outlier.colour="black",position = position_dodge(width=0)) + # ,color="white"
  scale_fill_manual(values=c("gray10","gray10")) +
  #geom_jitter(shape=16, position=position_jitter(0.15), size=1.5,
  # alpha=0.5) + 
  stat_summary(fun.y =mean, geom="point", size = 1,col="red") +
  geom_hline(yintercept=0, linetype="dashed", 
             color = "gray20", size=1) +
  
  #  scale_color_manual(values= c("gray20","gray20","gray20")) + # Use custom color palettes (conditioncol)
  # stat_summary(fun.y=mean, geom="point", shape=18, size=3, col="goldenrod4") + # Add mean
  theme_bw() + # or minimal/grey/classic
  theme(panel.background = element_rect(fill = "transparent", colour = NA), # or theme_blank()
        plot.background = element_rect(fill = "transparent", colour = NA),
        strip.background = element_blank(),
        strip.text.x = element_text(size =30),
        strip.text.y = element_text(size=30),
        axis.text=element_text(size=35),
        #axis.line.x = element_blank(),
        #axis.ticks.x = element_blank(),
        axis.line = element_line(size=sl, colour="black"),
        axis.title.y = element_text(size=45, face="plain",colour="black", hjust= 0.5, vjust = 2, margin = margin(t = 0, r = 5, b = 0, l = 0)),
        axis.title.x = element_text(size=30, face="plain",colour="black", margin = margin(t = 5, r = 0, b = 0, l = 0)),
        axis.ticks = element_line(size = sl, colour="black"),
        axis.ticks.length = unit(st,"cm"),
        axis.text.x = element_text(angle = 45, vjust = 1, size = 30, hjust = 1,family="sans",face = "plain"),
        #axis.text.y = element_text(size = sa,family="sans",face = "plain", colour="black", hjust = 1), #hjust = 0.5
        #axis.text.y = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid = element_blank(),
        panel.border = element_blank(),
        legend.position="none") +
  theme(aspect.ratio=2) +
  coord_capped_cart(bottom="left",left="top", gap=0.5) +
  expand_limits(x= 0, y=0) +
  #scale_x_discrete(expand = c(0,0.8)) +
  #scale_y_continuous(breaks = seq(0,50,10))+
  labs(y = "Difference ED RISK Score") +
  labs(x = "")


#linear mixed models for variable control in % delta BDI score
group_by(diffplotNOW, redcap_event_name ) %>%
  summarise(
    count = n(),
    median = median(dEDI, na.rm = TRUE),
    quantile1st = quantile(dEDI, probs = 0.25, na.rm = TRUE),
    quantile3rd = quantile(dEDI, probs= 0.75, na.rm = TRUE)
    
  )

#linear mixed models for variable control in % delta BDI score
group_by(diffplotNOW, redcap_event_name, sex) %>%
  summarise(
    count = n(),
    median = median(dEDI, na.rm = TRUE),
    quantile1st = quantile(dEDI, probs = 0.25, na.rm = TRUE),
    quantile3rd = quantile(dEDI, probs= 0.75, na.rm = TRUE)
  )

group_by(diffplotNOW, redcap_event_name, sex, distvit___7) %>%
  summarise(
    count = n(),
    median = median(dEDI, na.rm = TRUE),
    quantile1st = quantile(dEDI, probs = 0.25, na.rm = TRUE),
    quantile3rd = quantile(dEDI, probs= 0.75, na.rm = TRUE)
  )

diffplotNOW[2,17] = 26.21 #change a value of bmi
diffplotNOW[135,17] = 16.84


m0 <- lmer(dEDI ~ 1 +(1|eta), diffplotNOW)
m1 <- lmer(dEDI ~ sex + (1|eta), diffplotNOW)
anova(m0,m1)
plot(allEffects(m1))
summary(m1)

m0 <- lmer(dEDI ~ 1 +(1|sex), diffplotNOW)
m1 <- lmer(dEDI ~ eta +(1|sex), diffplotNOW)
anova(m0,m1)
plot(allEffects(m1))
summary(m1)

m0 <- lmer(dEDI ~ 1 +(1|eta), diffplotNOW)
m1 <- lmer(dEDI ~ reside + (1|eta), diffplotNOW)
anova(m0,m1)
plot(allEffects(m1))
summary(m1)

m0 <- lmer(dEDI ~ 1 +(1|eta), diffplotNOW)
m1 <- lmer(dEDI ~ intolal + (1|eta), diffplotNOW)
anova(m0,m1)
plot(allEffects(m1))
summary(m1)

m0 <- lmer(dEDI ~ 1 +(1|eta), diffplotNOW)
m1 <- lmer(dEDI ~ talim + (1|eta), diffplotNOW)
anova(m0,m1)
plot(allEffects(m1))
summary(m1)

table(ECOS_dupl2$talim, ECOS_dupl2$redcap_event_name)

m0 <- lmer(dEDI ~ 1 +(1|eta), diffplotNOW)
m1 <- lmer(dEDI ~ evitacib + (1|eta), diffplotNOW)
anova(m0,m1)
plot(allEffects(m1))
summary(m1)


m0 <- lmer(dEDI ~ 1 +(1|eta), diffplotNOW)
m1 <- lmer(dEDI ~ distvit___7 + (1|eta), diffplotNOW)
anova(m0,m1)
plot(allEffects(m1))
summary(m1)

m0 <- lmer(dEDI ~ 1 +(1|eta), diffplotNOW)
m1 <- lmer(dEDI ~ dietan + (1|eta), diffplotNOW)
anova(m0,m1)
plot(allEffects(m1))
summary(m1)


m0 <- lmer(dEDI ~ 1 +(1|eta), diffplotNOW)
m1 <- lmer(dEDI ~ anydiet + (1|eta), diffplotNOW)
anova(m0,m1)
plot(allEffects(m1))
summary(m1)

m0 <- lmer(dEDI ~ 1 +(1|eta), diffplotNOW)
m1 <- lmer(dEDI ~ anydisease + (1|eta), diffplotNOW)
anova(m0,m1)
plot(allEffects(m1))
summary(m1)

m0 <- lmer(dEDI ~ 1 +(1|eta), diffplotNOW)
m1 <- lmer(dEDI ~ peconom + (1|eta), diffplotNOW)
anova(m0,m1)
plot(allEffects(m1))
summary(m1)

m0 <- lmer(dEDI ~ 1 +(1|eta), diffplotNOW)
m1 <- lmer(dEDI ~ anyED + (1|eta), diffplotNOW)
anova(m0,m1)
plot(allEffects(m1))
summary(m1)



diffplot <- subset(ECOS_dupl2, redcap_event_name == "Before Lockdown", colnames(ECOS_dupl2)) 

diffplot <- data.frame(cbind(diffplot, dEDI))

m0 <- lmer(dEDI ~ 1 +(1|eta), diffplot)
m1 <- lmer(dEDI ~ peconom + (1|eta), diffplot)
anova(m0,m1)
plot(allEffects(m1))
summary(m1)

m0 <- lmer(dEDI ~ 1 +(1|eta), diffplot)
m1 <- lmer(dEDI ~ ED_risk + (1|eta), diffplot)
anova(m0,m1)
plot(allEffects(m1))
summary(m1)

library("ggpubr")
ggscatter(diffplot, x = "ED_risk", y = "dEDI", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson")


diffplotNOW <- subset(ECOS_dupl2, redcap_event_name == "During Lockdown", colnames(ECOS_dupl2)) 

diffplotNOW <- data.frame(cbind(diffplotNOW, dEDI))

#Plot differences in BDI scores

diffplotNOW <- data.frame(cbind(diffplotNOW, dDisadapt))


ggplot(diffplotNOW, aes(x=sex, y=dDisadapt, group = sex, color=sex)) + 
  #facet_wrap(~ distvit___7, ncol = 2) + # Multi plot windcows by ~ "" , scale="free"
  #geom_violin(trim=TRUE, scale = "width") + # trim=F (trim the tails of the violins)
  #geom_hline(aes(yintercept = meanmale), color = "deepskyblue3",size = 1)+
  #geom_hline(aes(yintercept = meanfemale), color = "deeppink2", size = 1)+
  geom_hline(aes(yintercept = 0), color = "gray30", size = 1, linetype = "dashed")+
  #geom_boxplot(width=0.85, lwd=1 ,outlier.size=0.5, outlier.shape=16, outlier.colour="transparent",position = position_dodge(width=0)) + # ,color="white"
  scale_color_manual(values=c("deeppink2","deepskyblue3")) +
  geom_jitter(shape=16, position=position_jitter(0.15), size=1.5,
              alpha=0.5) +
  
  stat_summary(fun.data=median_iqr, geom="pointrange", size = 1,col="gray2") +
  
  #  scale_color_manual(values= c("gray20","gray20","gray20")) + # Use custom color palettes (conditioncol)
  #stat_summary(fun.y=mean, geom="pointrange", shape=18, size=3, col="goldenrod4") + # Add mean
  theme_bw() + # or minimal/grey/classic
  theme(panel.background = element_rect(fill = "transparent", colour = NA), # or theme_blank()
        plot.background = element_rect(fill = "transparent", colour = NA),
        strip.background = element_blank(),
        #strip.text.x = element_text(size =20),
        axis.text=element_text(size=25),
        #axis.line.x = element_blank(),
        #axis.ticks.x = element_blank(),
        axis.line = element_line(size=sl, colour="black"),
        axis.title.y = element_text(size=30, face="plain",colour="black", margin = margin(t = 0, r = 5, b = 0, l = 0)),
        axis.title.x = element_text(size=30, face="plain",colour="black", margin = margin(t = 5, r = 0, b = 0, l = 0)),
        axis.ticks.y = element_line(size = sl, colour="black"),
        axis.ticks.x = element_blank(),
        axis.ticks.length = unit(st,"cm"),
        axis.text.x = element_text(angle = 45,face="plain",colour="black", margin = margin(t = 0, r = 5, b = 0, l = 0), vjust = 1, hjust =1),
        #axis.text.y = element_text(size = sa,family="sans",face = "plain", colour="black", hjust = 1), #hjust = 0.5
        #axis.text.y = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid = element_blank(),
        panel.border = element_blank(),
        legend.position="none") +
  theme(aspect.ratio=2) +
  coord_capped_cart(bottom="left",left="top", gap=0.5) +
 # scale_y_continuous(breaks = seq(-1,1)) +
  labs(y = "% ??Disadapt Score") +
  labs(x = "")


m0 <- lmer(dDisadapt ~ 1 +(1|eta), diffplotNOW)
m1 <- lmer(dDisadapt ~ sex + (1|eta), diffplotNOW)
anova(m0,m1)
plot(allEffects(m1))
summary(m1)

m0 <- lmer(dDisadapt ~ 1 +(1|sex), diffplotNOW)
m1 <- lmer(dDisadapt ~ eta +(1|sex), diffplotNOW)
anova(m0,m1)
plot(allEffects(m1))
summary(m1)

m0 <- lmer(dDisadapt ~ 1 +(1|eta), diffplotNOW)
m1 <- lmer(dDisadapt ~ reside + (1|eta), diffplotNOW)
anova(m0,m1)

m0 <- lmer(dDisadapt ~ 1 +(1|eta), diffplotNOW)
m1 <- lmer(dDisadapt ~ intolal + (1|eta), diffplotNOW)
anova(m0,m1)


m0 <- lmer(dDisadapt ~ 1 +(1|eta), diffplotNOW)
m1 <- lmer(dDisadapt ~ talim + (1|eta), diffplotNOW)
anova(m0,m1)

m0 <- lmer(dDisadapt ~ 1 +(1|eta), diffplotNOW)
m1 <- lmer(dDisadapt ~ evitacib + (1|eta), diffplotNOW)
anova(m0,m1)

m0 <- lmer(dDisadapt ~ 1 +(1|eta), diffplotNOW)
m1 <- lmer(dDisadapt ~ schlunianni + (1|eta), diffplotNOW)
anova(m0,m1)

m0 <- lmer(dDisadapt ~ 1 +(1|eta), diffplotNOW)
m1 <- lmer(dDisadapt ~ distvit___7 + (1|eta), diffplotNOW)
anova(m0,m1)

m0 <- lmer(dDisadapt ~ 1 +(1|eta), diffplotNOW)
m1 <- lmer(dDisadapt ~ dietan + (1|eta), diffplotNOW)
anova(m0,m1)

m0 <- lmer(dDisadapt ~ 1 +(1|eta), diffplotNOW)
m1 <- lmer(dDisadapt ~ anydiet + (1|eta), diffplotNOW)
anova(m0,m1)

m0 <- lmer(dDisadapt ~ 1 +(1|eta), diffplotNOW)
m1 <- lmer(dDisadapt ~ anydisease + (1|eta), diffplotNOW)
anova(m0,m1)

m0 <- lmer(dDisadapt ~ 1 +(1|eta), diffplotNOW)
m1 <- lmer(dDisadapt ~ peconom + (1|eta), diffplotNOW)
anova(m0,m1)


m0 <- lmer(dDisadapt ~ 1 +(1|eta), diffplot)
m1 <- lmer(dDisadapt ~ gDisadapt + (1|eta), diffplot)
anova(m0,m1)
plot(allEffects(m1))
summary(m1)

m0 <- lmer(dDisadapt ~ 1 +(1|eta), diffplot)
m1 <- lmer(dDisadapt ~ anyED + (1|eta), diffplot)
anova(m0,m1)
plot(allEffects(m1))
summary(m1)



#BAI


ggplot(ECOS_dupl2, aes(x=redcap_event_name, y=baitot)) + 
  facet_grid(sex~distvit___7) + # Multi plot windcows by ~ "" , scale="free"
  geom_violin(trim=TRUE, scale = "width", fill = "gray") + # trim=F (trim the tails of the violins)
  scale_fill_manual(values=c("gray50","gray50")) +
  #stat_boxplot(geom = "errorbar", width = 0.5) +  
  geom_boxplot(width=0.4, lwd=1 ,outlier.size=2, outlier.shape=16, outlier.colour="black",position = position_dodge(width=0)) + # ,color="white"
  scale_fill_manual(values=c("gray10","gray10")) +
  #geom_jitter(shape=16, position=position_jitter(0.15), size=1.5,
  # alpha=0.5) + 
  stat_summary(fun.y =mean, geom="point", size = 1,col="red") +
  
  #  scale_color_manual(values= c("gray20","gray20","gray20")) + # Use custom color palettes (conditioncol)
  # stat_summary(fun.y=mean, geom="point", shape=18, size=3, col="goldenrod4") + # Add mean
  theme_bw() + # or minimal/grey/classic
  theme(panel.background = element_rect(fill = "transparent", colour = NA), # or theme_blank()
        plot.background = element_rect(fill = "transparent", colour = NA),
        strip.background = element_blank(),
        strip.text.x = element_text(size =30),
        strip.text.y = element_text(size=30),
        axis.text=element_text(size=35),
        #axis.line.x = element_blank(),
        #axis.ticks.x = element_blank(),
        axis.line = element_line(size=sl, colour="black"),
        axis.title.y = element_text(size=45, face="plain",colour="black", hjust= 0.5, vjust = 2, margin = margin(t = 0, r = 5, b = 0, l = 0)),
        axis.title.x = element_text(size=30, face="plain",colour="black", margin = margin(t = 5, r = 0, b = 0, l = 0)),
        axis.ticks = element_line(size = sl, colour="black"),
        axis.ticks.length = unit(st,"cm"),
        axis.text.x = element_text(angle = 45, vjust = 1, size = 30, hjust = 1,family="sans",face = "plain"),
        #axis.text.y = element_text(size = sa,family="sans",face = "plain", colour="black", hjust = 1), #hjust = 0.5
        #axis.text.y = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid = element_blank(),
        panel.border = element_blank(),
        legend.position="none") +
  theme(aspect.ratio=2) +
  coord_capped_cart(bottom="left",left="top", gap=0.5) +
  expand_limits(x= 0, y=0) +
  #scale_x_discrete(expand = c(0,0.8)) +
  #scale_y_continuous(breaks = seq(0,50,10))+
  labs(y = "BAI Score") +
  labs(x = "")



m0 <- glmer(baitot ~ 1 + (1|record_id) + (1|eta),  family = "poisson", ECOS_dupl2)
m1 <- glmer(baitot ~ redcap_event_name + (1|record_id) + (1|eta),  family = "poisson", ECOS_dupl2)

anova(m0,m1)
plot(allEffects(m1))
summary(m1)

m2 <- glmer(baitot ~ redcap_event_name:sex + sex + (1|record_id)  + (1|eta),  family = "poisson", ECOS_dupl2)
anova(m1,m2)
plot(allEffects(m2))
summary(m2)



m3 <- glmer(baitot ~ redcap_event_name:distvit___7 + distvit___7 + (1|record_id)  + (1|eta) +(1|sex),  family = "poisson", ECOS_dupl2)
anova(m1,m3)
plot(allEffects(m3))
summary(m3)



ggplot(diffplot, aes(x=redcap_event_name, y=dBAI)) + 
  facet_grid(sex~distvit___7) + # Multi plot windcows by ~ "" , scale="free"
  geom_violin(trim=TRUE, scale = "width", fill = "gray") + # trim=F (trim the tails of the violins)
  scale_fill_manual(values=c("gray50","gray50")) +
  #stat_boxplot(geom = "errorbar", width = 0.5) +  
  geom_boxplot(width=0.4, lwd=1 ,outlier.size=2.5, outlier.shape=8, outlier.colour="black",position = position_dodge(width=0)) + # ,color="white"
  #scale_fill_manual(values=c("gray10","gray10")) +
  #geom_jitter(shape=16, position=position_jitter(0.15), size=1.5,
  # alpha=0.5) + 
  stat_summary(fun.y =mean, geom="point", size = 1,col="red") +
  geom_hline(yintercept=0, linetype="dashed", 
             color = "gray20", size=1) +
  
  #  scale_color_manual(values= c("gray20","gray20","gray20")) + # Use custom color palettes (conditioncol)
  # stat_summary(fun.y=mean, geom="point", shape=18, size=3, col="goldenrod4") + # Add mean
  theme_bw() + # or minimal/grey/classic
  theme(panel.background = element_rect(fill = "transparent", colour = NA), # or theme_blank()
        plot.background = element_rect(fill = "transparent", colour = NA),
        strip.background = element_blank(),
        strip.text.x = element_text(size =30),
        strip.text.y = element_text(size=30),
        axis.text=element_text(size=35),
        #axis.line.x = element_blank(),
        #axis.ticks.x = element_blank(),
        axis.line = element_line(size=sl, colour="black"),
        axis.title.y = element_text(size=45, face="plain",colour="black", hjust= 0.5, margin = margin(t = 0, r = 5, b = 0, l = 0)),
        axis.title.x = element_text(size=30, face="plain",colour="black", margin = margin(t = 5, r = 0, b = 0, l = 0)),
        axis.ticks = element_line(size = sl, colour="black"),
        axis.ticks.length = unit(st,"cm"),
        axis.text.x = element_text(angle = 45, vjust = 1, size = 30, hjust = 1,family="sans",face = "plain"),
        #axis.text.y = element_text(size = sa,family="sans",face = "plain", colour="black", hjust = 1), #hjust = 0.5
        #axis.text.y = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid = element_blank(),
        panel.border = element_blank(),
        legend.position="none") +
  theme(aspect.ratio=2) +
  coord_capped_cart(bottom="left",left="top", gap=0.5) +
  expand_limits(x= 0, y=0) +
  #scale_x_discrete(expand = c(0,0.8)) +
  scale_y_continuous(breaks = seq(-30,30,15))+
  labs(y = "Difference BAI Score") +
  labs(x = "")


#ocitot 


ggplot(ECOS_dupl2, aes(x=redcap_event_name, y=ocitot)) + 
  facet_grid(sex~distvit___7) + # Multi plot windcows by ~ "" , scale="free"
  geom_violin(trim=TRUE, scale = "width", fill = "gray") + # trim=F (trim the tails of the violins)
  scale_fill_manual(values=c("gray50","gray50")) +
  #stat_boxplot(geom = "errorbar", width = 0.5) +  
  geom_boxplot(width=0.4, lwd=1 ,outlier.size=2, outlier.shape=16, outlier.colour="black",position = position_dodge(width=0)) + # ,color="white"
  scale_fill_manual(values=c("gray10","gray10")) +
  #geom_jitter(shape=16, position=position_jitter(0.15), size=1.5,
  # alpha=0.5) + 
  stat_summary(fun.y =mean, geom="point", size = 1,col="red") +
  
  #  scale_color_manual(values= c("gray20","gray20","gray20")) + # Use custom color palettes (conditioncol)
  # stat_summary(fun.y=mean, geom="point", shape=18, size=3, col="goldenrod4") + # Add mean
  theme_bw() + # or minimal/grey/classic
  theme(panel.background = element_rect(fill = "transparent", colour = NA), # or theme_blank()
        plot.background = element_rect(fill = "transparent", colour = NA),
        strip.background = element_blank(),
        strip.text.x = element_text(size =30),
        strip.text.y = element_text(size=30),
        axis.text=element_text(size=35),
        #axis.line.x = element_blank(),
        #axis.ticks.x = element_blank(),
        axis.line = element_line(size=sl, colour="black"),
        axis.title.y = element_text(size=45, face="plain",colour="black", hjust= 0.5, vjust = 2, margin = margin(t = 0, r = 5, b = 0, l = 0)),
        axis.title.x = element_text(size=30, face="plain",colour="black", margin = margin(t = 5, r = 0, b = 0, l = 0)),
        axis.ticks = element_line(size = sl, colour="black"),
        axis.ticks.length = unit(st,"cm"),
        axis.text.x = element_text(angle = 45, vjust = 1, size = 30, hjust = 1,family="sans",face = "plain"),
        #axis.text.y = element_text(size = sa,family="sans",face = "plain", colour="black", hjust = 1), #hjust = 0.5
        #axis.text.y = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid = element_blank(),
        panel.border = element_blank(),
        legend.position="none") +
  theme(aspect.ratio=2) +
  coord_capped_cart(bottom="left",left="top", gap=0.5) +
  expand_limits(x= 0, y=0) +
  #scale_x_discrete(expand = c(0,0.8)) +
  #scale_y_continuous(breaks = seq(0,50,10))+
  labs(y = "OCI-R Score") +
  labs(x = "")


m0 <- glmer(ocitot ~ 1 + (1|record_id) + (1|eta),  family = "poisson", ECOS_dupl2)
m1 <- glmer(ocitot ~ redcap_event_name + (1|record_id) + (1|eta),  family = "poisson", ECOS_dupl2)

anova(m0,m1)
plot(allEffects(m1))
summary(m1)

m2 <- glmer(ocitot ~ redcap_event_name:sex + sex + (1|record_id)  + (1|eta),  family = "poisson", ECOS_dupl2)
anova(m1,m2)
plot(allEffects(m2))
summary(m2)



m3 <- glmer(ocitot ~ redcap_event_name:distvit___7 + distvit___7 + (1|record_id) + (1|sex)  + (1|eta),  family = "poisson", ECOS_dupl2)
anova(m1,m3)
plot(allEffects(m3))
summary(m3)


ggplot(diffplot, aes(x=redcap_event_name, y=docitot)) + 
  facet_grid(sex~distvit___7) + # Multi plot windcows by ~ "" , scale="free"
  geom_violin(trim=TRUE, scale = "width", fill = "gray") + # trim=F (trim the tails of the violins)
  scale_fill_manual(values=c("gray50","gray50")) +
  #stat_boxplot(geom = "errorbar", width = 0.5) +  
  geom_boxplot(width=0.4, lwd=1 ,outlier.size=2.5, outlier.shape=8, outlier.colour="black",position = position_dodge(width=0)) + # ,color="white"
  #scale_fill_manual(values=c("gray10","gray10")) +
  #geom_jitter(shape=16, position=position_jitter(0.15), size=1.5,
  # alpha=0.5) + 
  stat_summary(fun.y =mean, geom="point", size = 1,col="red") +
  geom_hline(yintercept=0, linetype="dashed", 
             color = "gray20", size=1) +
  
  #  scale_color_manual(values= c("gray20","gray20","gray20")) + # Use custom color palettes (conditioncol)
  # stat_summary(fun.y=mean, geom="point", shape=18, size=3, col="goldenrod4") + # Add mean
  theme_bw() + # or minimal/grey/classic
  theme(panel.background = element_rect(fill = "transparent", colour = NA), # or theme_blank()
        plot.background = element_rect(fill = "transparent", colour = NA),
        strip.background = element_blank(),
        strip.text.x = element_text(size =30),
        strip.text.y = element_text(size=30),
        axis.text=element_text(size=35),
        #axis.line.x = element_blank(),
        #axis.ticks.x = element_blank(),
        axis.line = element_line(size=sl, colour="black"),
        axis.title.y = element_text(size=45, face="plain",colour="black", hjust= 0.5, margin = margin(t = 0, r = 5, b = 0, l = 0)),
        axis.title.x = element_text(size=30, face="plain",colour="black", margin = margin(t = 5, r = 0, b = 0, l = 0)),
        axis.ticks = element_line(size = sl, colour="black"),
        axis.ticks.length = unit(st,"cm"),
        axis.text.x = element_text(angle = 45, vjust = 1, size = 30, hjust = 1,family="sans",face = "plain"),
        #axis.text.y = element_text(size = sa,family="sans",face = "plain", colour="black", hjust = 1), #hjust = 0.5
        #axis.text.y = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid = element_blank(),
        panel.border = element_blank(),
        legend.position="none") +
  theme(aspect.ratio=2) +
  coord_capped_cart(bottom="left",left="top", gap=0.5) +
  expand_limits(x= 0, y=0) +
  #scale_x_discrete(expand = c(0,0.8)) +
  scale_y_continuous(breaks = c(-20,0,20))+
  labs(y = "Difference OCI-R Score") +
  labs(x = "")




ggplot(ECOS_dupl2, aes(x=redcap_event_name, y=EHQtotal)) + 
  facet_grid(sex~distvit___7) + # Multi plot windcows by ~ "" , scale="free"
  geom_violin(trim=TRUE, scale = "width", fill = "gray") + # trim=F (trim the tails of the violins)
  scale_fill_manual(values=c("gray50","gray50")) +
  #stat_boxplot(geom = "errorbar", width = 0.5) +  
  geom_boxplot(width=0.4, lwd=1 ,outlier.size=2, outlier.shape=16, outlier.colour="black",position = position_dodge(width=0)) + # ,color="white"
  scale_fill_manual(values=c("gray10","gray10")) +
  #geom_jitter(shape=16, position=position_jitter(0.15), size=1.5,
  # alpha=0.5) + 
  stat_summary(fun.y =mean, geom="point", size = 1,col="red") +
  
  #  scale_color_manual(values= c("gray20","gray20","gray20")) + # Use custom color palettes (conditioncol)
  # stat_summary(fun.y=mean, geom="point", shape=18, size=3, col="goldenrod4") + # Add mean
  theme_bw() + # or minimal/grey/classic
  theme(panel.background = element_rect(fill = "transparent", colour = NA), # or theme_blank()
        plot.background = element_rect(fill = "transparent", colour = NA),
        strip.background = element_blank(),
        strip.text.x = element_text(size =30),
        strip.text.y = element_text(size=30),
        axis.text=element_text(size=35),
        #axis.line.x = element_blank(),
        #axis.ticks.x = element_blank(),
        axis.line = element_line(size=sl, colour="black"),
        axis.title.y = element_text(size=45, face="plain",colour="black", hjust= 0.5, vjust = 2, margin = margin(t = 0, r = 5, b = 0, l = 0)),
        axis.title.x = element_text(size=30, face="plain",colour="black", margin = margin(t = 5, r = 0, b = 0, l = 0)),
        axis.ticks = element_line(size = sl, colour="black"),
        axis.ticks.length = unit(st,"cm"),
        axis.text.x = element_text(angle = 45, vjust = 1, size = 30, hjust = 1,family="sans",face = "plain"),
        #axis.text.y = element_text(size = sa,family="sans",face = "plain", colour="black", hjust = 1), #hjust = 0.5
        #axis.text.y = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid = element_blank(),
        panel.border = element_blank(),
        legend.position="none") +
  theme(aspect.ratio=2) +
  coord_capped_cart(bottom="left",left="top", gap=0.5) +
  expand_limits(x= 0, y=0) +
  #scale_x_discrete(expand = c(0,0.8)) +
  #scale_y_continuous(breaks = seq(0,50,10))+
  labs(y = "EHQ Score") +
  labs(x = "")


m0 <- glmer(EHQtotal ~ 1 + (1|record_id) + (1|eta),  family = "poisson", ECOS_dupl2)
m1 <- glmer(EHQtotal ~ redcap_event_name + (1|record_id) + (1|eta),  family = "poisson", ECOS_dupl2)

anova(m0,m1)
plot(allEffects(m1))
summary(m1)

m2 <- glmer(EHQtotal ~ redcap_event_name:sex + sex + (1|record_id)  + (1|eta),  family = "poisson", ECOS_dupl2)
anova(m1,m2)
plot(allEffects(m2))
summary(m2)



m3 <- glmer(EHQtotal ~ redcap_event_name:distvit___7 + distvit___7 + (1|record_id)  + (1|eta) (1|sex),  family = "poisson", ECOS_dupl2)
anova(m1,m3)
plot(allEffects(m3))
summary(m3)


m0 <- glmer(ED_risk ~ 1 + (1|record_id) + (1|eta),  family = "poisson", ECOS_dupl2)
m1 <- glmer(ED_risk ~ redcap_event_name + (1|record_id) + (1|eta),  family = "poisson", ECOS_dupl2)
m2 <- glmer(ED_risk ~ redcap_event_name:sex + sex + (1|record_id) + (1|eta),  family = "poisson", ECOS_dupl2)
m3 <- glmer(ED_risk ~ redcap_event_name:distvit___3 + distvit___3 + (1|record_id) + (1|sex) + (1|eta),  family = "poisson", ECOS_dupl2)
anova(m0,m1,m2,m3)
plot(allEffects(m3))
summary(m3)


table(ECOS_dupl2$diff_economic)

diffplot <- subset(ECOS_dupl2, redcap_event_name == "Before Lockdown", colnames(ECOS_dupl2)) 

diffplot <- data.frame(cbind(diffplot, dDisadapt, dBDI,dEDI,docitot, dBAI, dEHQTOT))


ggscatter(diffplot, x = "peconom", y = "dBDI", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.coef.size = 6, cor.method = "kendall", color = "sex", palette = c("gray40", "gray40"),
          xlab = "Age", ylab = "% ??ED Risk Score", add.params = list(color = "black",
                                                                                fill = "gray70"),
          ggtheme = theme(panel.background = element_rect(fill = "transparent", colour = NA), # or theme_blank()
                          plot.background = element_rect(fill = "transparent", colour = NA),
                          strip.background = element_blank(),
                          strip.text.x = element_text(size =20),
                          axis.text=element_text(size=25),
                          #axis.line.x = element_blank(),
                          #axis.ticks.x = element_blank(),
                          axis.line = element_line(size=sl, colour="gray60"),
                          axis.title.y = element_text(size=30, face="plain",colour="black", margin = margin(t = 0, r = 5, b = 0, l = 0)),
                          axis.title.x = element_text(size=30, face="plain",colour="black", margin = margin(t = 5, r = 0, b = 0, l = 0)),
                          axis.ticks = element_line(size = sl, colour="black"),
                          axis.ticks.length = unit(st,"cm"),
                          axis.text.x = element_text(vjust = 1, size = 25, hjust = 1,family="sans",face = "plain"),
                          #axis.text.y = element_text(size = sa,family="sans",face = "plain", colour="black", hjust = 1), #hjust = 0.5
                          #axis.text.y = element_blank(),
                          panel.grid.major.x = element_blank(),
                          panel.grid.major.y = element_blank(),
                          panel.grid = element_blank(),
                          panel.border = element_blank(),
                          legend.position="none"))


ggscatter(diffplot, x = "dEDI", y = "dBDI", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.coef.size = 6, cor.method = "kendall", color = "sex", palette = c("gray40", "gray40"),
          xlab = "% ??ED Risk Score", ylab = "% ??BDI-2 Score", add.params = list(color = "black",
                                                                                            fill = "gray70"),
          ggtheme = theme(panel.background = element_rect(fill = "transparent", colour = NA), # or theme_blank()
                          plot.background = element_rect(fill = "transparent", colour = NA),
                          strip.background = element_blank(),
                          strip.text.x = element_text(size =20),
                          axis.text=element_text(size=25),
                          #axis.line.x = element_blank(),
                          #axis.ticks.x = element_blank(),
                          axis.line = element_line(size=sl, colour="gray60"),
                          axis.title.y = element_text(size=30, face="plain",colour="black", margin = margin(t = 0, r = 5, b = 0, l = 0)),
                          axis.title.x = element_text(size=30, face="plain",colour="black", margin = margin(t = 5, r = 0, b = 0, l = 0)),
                          axis.ticks = element_line(size = sl, colour="black"),
                          axis.ticks.length = unit(st,"cm"),
                          axis.text.x = element_text(vjust = 1, size = 25, hjust = 1,family="sans",face = "plain"),
                          #axis.text.y = element_text(size = sa,family="sans",face = "plain", colour="black", hjust = 1), #hjust = 0.5
                          #axis.text.y = element_blank(),
                          panel.grid.major.x = element_blank(),
                          panel.grid.major.y = element_blank(),
                          panel.grid = element_blank(),
                          panel.border = element_blank(),
                          legend.position="none"))


ggscatter(diffplot, x = "dDisadapt", y = "dBDI", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.coef.size = 6, cor.method = "kendall", color = "sex", palette = c("gray40", "gray40"),
          xlab = "% ??Disadapt Score", ylab = "% ??BDI-2 Score", add.params = list(color = "black",
                                                                                fill = "gray70"),
          ggtheme = theme(panel.background = element_rect(fill = "transparent", colour = NA), # or theme_blank()
                          plot.background = element_rect(fill = "transparent", colour = NA),
                          strip.background = element_blank(),
                          strip.text.x = element_text(size =20),
                          axis.text=element_text(size=25),
                          #axis.line.x = element_blank(),
                          #axis.ticks.x = element_blank(),
                          axis.line = element_line(size=sl, colour="gray60"),
                          axis.title.y = element_text(size=30, face="plain",colour="black", margin = margin(t = 0, r = 5, b = 0, l = 0)),
                          axis.title.x = element_text(size=30, face="plain",colour="black", margin = margin(t = 5, r = 0, b = 0, l = 0)),
                          axis.ticks = element_line(size = sl, colour="black"),
                          axis.ticks.length = unit(st,"cm"),
                          axis.text.x = element_text(vjust = 1, size = 25, hjust = 1,family="sans",face = "plain"),
                          #axis.text.y = element_text(size = sa,family="sans",face = "plain", colour="black", hjust = 1), #hjust = 0.5
                          #axis.text.y = element_blank(),
                          panel.grid.major.x = element_blank(),
                          panel.grid.major.y = element_blank(),
                          panel.grid = element_blank(),
                          panel.border = element_blank(),
                          legend.position="none"))

ggscatter(diffplot, x = "dDisadapt", y = "dEDI", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.coef.size = 6, cor.method = "kendall", color = "sex", palette = c("gray40", "gray40"),
          xlab = "% ??Disadapt Score", ylab = "% ??ED Risk Score", add.params = list(color = "black",
                                                                                 fill = "gray70"),
          ggtheme = theme(panel.background = element_rect(fill = "transparent", colour = NA), # or theme_blank()
                          plot.background = element_rect(fill = "transparent", colour = NA),
                          strip.background = element_blank(),
                          strip.text.x = element_text(size =20),
                          axis.text=element_text(size=25),
                          #axis.line.x = element_blank(),
                          #axis.ticks.x = element_blank(),
                          axis.line = element_line(size=sl, colour="gray60"),
                          axis.title.y = element_text(size=30, face="plain",colour="black", margin = margin(t = 0, r = 5, b = 0, l = 0)),
                          axis.title.x = element_text(size=30, face="plain",colour="black", margin = margin(t = 5, r = 0, b = 0, l = 0)),
                          axis.ticks = element_line(size = sl, colour="black"),
                          axis.ticks.length = unit(st,"cm"),
                          axis.text.x = element_text(vjust = 1, size = 25, hjust = 1,family="sans",face = "plain"),
                          #axis.text.y = element_text(size = sa,family="sans",face = "plain", colour="black", hjust = 1), #hjust = 0.5
                          #axis.text.y = element_blank(),
                          panel.grid.major.x = element_blank(),
                          panel.grid.major.y = element_blank(),
                          panel.grid = element_blank(),
                          panel.border = element_blank(),
                          legend.position="none"))


ggscatter(diffplot, x = "dDisadapt", y = "dEHQTOT", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.coef.size = 6, cor.method = "kendall", color = "sex", palette = c("gray40", "gray40"),
          xlab = "% ??Disadapt Score", ylab = "% ??EHQ Score", add.params = list(color = "black",
                                                                                   fill = "gray70"),
          ggtheme = theme(panel.background = element_rect(fill = "transparent", colour = NA), # or theme_blank()
                          plot.background = element_rect(fill = "transparent", colour = NA),
                          strip.background = element_blank(),
                          strip.text.x = element_text(size =20),
                          axis.text=element_text(size=25),
                          #axis.line.x = element_blank(),
                          #axis.ticks.x = element_blank(),
                          axis.line = element_line(size=sl, colour="gray60"),
                          axis.title.y = element_text(size=30, face="plain",colour="black", margin = margin(t = 0, r = 5, b = 0, l = 0)),
                          axis.title.x = element_text(size=30, face="plain",colour="black", margin = margin(t = 5, r = 0, b = 0, l = 0)),
                          axis.ticks = element_line(size = sl, colour="black"),
                          axis.ticks.length = unit(st,"cm"),
                          axis.text.x = element_text(vjust = 1, size = 25, hjust = 1,family="sans",face = "plain"),
                          #axis.text.y = element_text(size = sa,family="sans",face = "plain", colour="black", hjust = 1), #hjust = 0.5
                          #axis.text.y = element_blank(),
                          panel.grid.major.x = element_blank(),
                          panel.grid.major.y = element_blank(),
                          panel.grid = element_blank(),
                          panel.border = element_blank(),
                          legend.position="none"))

ggscatter(diffplot, x = "dEDI", y = "dEHQTOT", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.coef.size = 6, cor.method = "kendall", color = "sex", palette = c("gray40", "gray40"),
          xlab = "% ??Disadapt Score", ylab = "% ??EHQ Score", add.params = list(color = "black",
                                                                               fill = "gray70"),
          ggtheme = theme(panel.background = element_rect(fill = "transparent", colour = NA), # or theme_blank()
                          plot.background = element_rect(fill = "transparent", colour = NA),
                          strip.background = element_blank(),
                          strip.text.x = element_text(size =20),
                          axis.text=element_text(size=25),
                          #axis.line.x = element_blank(),
                          #axis.ticks.x = element_blank(),
                          axis.line = element_line(size=sl, colour="gray60"),
                          axis.title.y = element_text(size=30, face="plain",colour="black", margin = margin(t = 0, r = 5, b = 0, l = 0)),
                          axis.title.x = element_text(size=30, face="plain",colour="black", margin = margin(t = 5, r = 0, b = 0, l = 0)),
                          axis.ticks = element_line(size = sl, colour="black"),
                          axis.ticks.length = unit(st,"cm"),
                          axis.text.x = element_text(vjust = 1, size = 25, hjust = 1,family="sans",face = "plain"),
                          #axis.text.y = element_text(size = sa,family="sans",face = "plain", colour="black", hjust = 1), #hjust = 0.5
                          #axis.text.y = element_blank(),
                          panel.grid.major.x = element_blank(),
                          panel.grid.major.y = element_blank(),
                          panel.grid = element_blank(),
                          panel.border = element_blank(),
                          legend.position="none"))

ggscatter(diffplot, x = "dDisadapt", y = "docitot", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.coef.size = 6, cor.method = "kendall", color = "sex", palette = c("gray40", "gray40"),
          xlab = "% ??Disadapt Score", ylab = "% ??OCI-R Score", add.params = list(color = "black",
                                                                               fill = "gray70"),
          ggtheme = theme(panel.background = element_rect(fill = "transparent", colour = NA), # or theme_blank()
                          plot.background = element_rect(fill = "transparent", colour = NA),
                          strip.background = element_blank(),
                          strip.text.x = element_text(size =20),
                          axis.text=element_text(size=25),
                          #axis.line.x = element_blank(),
                          #axis.ticks.x = element_blank(),
                          axis.line = element_line(size=sl, colour="gray60"),
                          axis.title.y = element_text(size=30, face="plain",colour="black", margin = margin(t = 0, r = 5, b = 0, l = 0)),
                          axis.title.x = element_text(size=30, face="plain",colour="black", margin = margin(t = 5, r = 0, b = 0, l = 0)),
                          axis.ticks = element_line(size = sl, colour="black"),
                          axis.ticks.length = unit(st,"cm"),
                          axis.text.x = element_text(vjust = 1, size = 25, hjust = 1,family="sans",face = "plain"),
                          #axis.text.y = element_text(size = sa,family="sans",face = "plain", colour="black", hjust = 1), #hjust = 0.5
                          #axis.text.y = element_blank(),
                          panel.grid.major.x = element_blank(),
                          panel.grid.major.y = element_blank(),
                          panel.grid = element_blank(),
                          panel.border = element_blank(),
                          legend.position="none"))

ggscatter(diffplot, x = "dEDI", y = "docitot", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.coef.size = 6, cor.method = "kendall", color = "sex", palette = c("gray40", "gray40"),
          xlab = "% ??ED Risk Score", ylab = "% ??OCI-R Score", add.params = list(color = "black",
                                                                               fill = "gray70"),
          ggtheme = theme(panel.background = element_rect(fill = "transparent", colour = NA), # or theme_blank()
                          plot.background = element_rect(fill = "transparent", colour = NA),
                          strip.background = element_blank(),
                          strip.text.x = element_text(size =20),
                          axis.text=element_text(size=25),
                          #axis.line.x = element_blank(),
                          #axis.ticks.x = element_blank(),
                          axis.line = element_line(size=sl, colour="gray60"),
                          axis.title.y = element_text(size=30, face="plain",colour="black", margin = margin(t = 0, r = 5, b = 0, l = 0)),
                          axis.title.x = element_text(size=30, face="plain",colour="black", margin = margin(t = 5, r = 0, b = 0, l = 0)),
                          axis.ticks = element_line(size = sl, colour="black"),
                          axis.ticks.length = unit(st,"cm"),
                          axis.text.x = element_text(vjust = 1, size = 25, hjust = 1,family="sans",face = "plain"),
                          #axis.text.y = element_text(size = sa,family="sans",face = "plain", colour="black", hjust = 1), #hjust = 0.5
                          #axis.text.y = element_blank(),
                          panel.grid.major.x = element_blank(),
                          panel.grid.major.y = element_blank(),
                          panel.grid = element_blank(),
                          panel.border = element_blank(),
                          legend.position="none"))


ggscatter(diffplot, x = "dEDI", y = "dBAI", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.coef.size = 6, cor.method = "kendall", color = "sex", palette = c("gray40", "gray40"),
          xlab = "% ??ED Risk Score", ylab = "% ??BAI Score", add.params = list(color = "black",
                                                                                fill = "gray70"),
          ggtheme = theme(panel.background = element_rect(fill = "transparent", colour = NA), # or theme_blank()
                          plot.background = element_rect(fill = "transparent", colour = NA),
                          strip.background = element_blank(),
                          strip.text.x = element_text(size =20),
                          axis.text=element_text(size=25),
                          #axis.line.x = element_blank(),
                          #axis.ticks.x = element_blank(),
                          axis.line = element_line(size=sl, colour="gray60"),
                          axis.title.y = element_text(size=30, face="plain",colour="black", margin = margin(t = 0, r = 5, b = 0, l = 0)),
                          axis.title.x = element_text(size=30, face="plain",colour="black", margin = margin(t = 5, r = 0, b = 0, l = 0)),
                          axis.ticks = element_line(size = sl, colour="black"),
                          axis.ticks.length = unit(st,"cm"),
                          axis.text.x = element_text(vjust = 1, size = 25, hjust = 1,family="sans",face = "plain"),
                          #axis.text.y = element_text(size = sa,family="sans",face = "plain", colour="black", hjust = 1), #hjust = 0.5
                          #axis.text.y = element_blank(),
                          panel.grid.major.x = element_blank(),
                          panel.grid.major.y = element_blank(),
                          panel.grid = element_blank(),
                          panel.border = element_blank(),
                          legend.position="none"))

ggscatter(diffplot, x = "dEDI", y = "dBAI", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.coef.size = 6, cor.method = "kendall", color = "sex", palette = c("gray40", "gray40"),
          xlab = "% ??ED Risk Score", ylab = "% ??BAI Score", add.params = list(color = "black",
                                                                              fill = "gray70"),
          ggtheme = theme(panel.background = element_rect(fill = "transparent", colour = NA), # or theme_blank()
                          plot.background = element_rect(fill = "transparent", colour = NA),
                          strip.background = element_blank(),
                          strip.text.x = element_text(size =20),
                          axis.text=element_text(size=25),
                          #axis.line.x = element_blank(),
                          #axis.ticks.x = element_blank(),
                          axis.line = element_line(size=sl, colour="gray60"),
                          axis.title.y = element_text(size=30, face="plain",colour="black", margin = margin(t = 0, r = 5, b = 0, l = 0)),
                          axis.title.x = element_text(size=30, face="plain",colour="black", margin = margin(t = 5, r = 0, b = 0, l = 0)),
                          axis.ticks = element_line(size = sl, colour="black"),
                          axis.ticks.length = unit(st,"cm"),
                          axis.text.x = element_text(vjust = 1, size = 25, hjust = 1,family="sans",face = "plain"),
                          #axis.text.y = element_text(size = sa,family="sans",face = "plain", colour="black", hjust = 1), #hjust = 0.5
                          #axis.text.y = element_blank(),
                          panel.grid.major.x = element_blank(),
                          panel.grid.major.y = element_blank(),
                          panel.grid = element_blank(),
                          panel.border = element_blank(),
                          legend.position="none"))


ggscatter(diffplot, x = "dDisadapt", y = "dBAI", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.coef.size = 6, cor.method = "kendall", color = "sex", palette = c("gray40", "gray40"),
          xlab = "% ??Disadapt Score", ylab = "% ??BAI Score", add.params = list(color = "black",
                                                                              fill = "gray70"),
          ggtheme = theme(panel.background = element_rect(fill = "transparent", colour = NA), # or theme_blank()
                          plot.background = element_rect(fill = "transparent", colour = NA),
                          strip.background = element_blank(),
                          strip.text.x = element_text(size =20),
                          axis.text=element_text(size=25),
                          #axis.line.x = element_blank(),
                          #axis.ticks.x = element_blank(),
                          axis.line = element_line(size=sl, colour="gray60"),
                          axis.title.y = element_text(size=30, face="plain",colour="black", margin = margin(t = 0, r = 5, b = 0, l = 0)),
                          axis.title.x = element_text(size=30, face="plain",colour="black", margin = margin(t = 5, r = 0, b = 0, l = 0)),
                          axis.ticks = element_line(size = sl, colour="black"),
                          axis.ticks.length = unit(st,"cm"),
                          axis.text.x = element_text(vjust = 1, size = 25, hjust = 1,family="sans",face = "plain"),
                          #axis.text.y = element_text(size = sa,family="sans",face = "plain", colour="black", hjust = 1), #hjust = 0.5
                          #axis.text.y = element_blank(),
                          panel.grid.major.x = element_blank(),
                          panel.grid.major.y = element_blank(),
                          panel.grid = element_blank(),
                          panel.border = element_blank(),
                          legend.position="none"))
##################CLINICAL CATEGORIES OF RISK###############?
#economic
clinicalchange <- subset(ECOS_dupl2, redcap_event_name == "Before Lockdown", colnames(ECOS_dupl2))

clinicalchange$diff_economic <- as.factor(clinicalchange$diff_economic)

#we are interested in those whose symptoms get worse
levels(clinicalchange$peconom)[levels(clinicalchange$peconom)=="1"] <- "No Worry"
levels(clinicalchange$peconom)[levels(clinicalchange$peconom)=="2"] <- "Slight"
levels(clinicalchange$peconom)[levels(clinicalchange$peconom)=="3"] <- "Moderate"
levels(clinicalchange$peconom)[levels(clinicalchange$peconom)=="4"] <- "Severe"
levels(clinicalchange$peconom)[levels(clinicalchange$peconom)=="5"] <- "Very Severe"

#little update

table(clinicalchange$peconom, clinicalchange$diff_economic)

m0 <- lmer(dDisadapt ~ 1 + (1|eta), clinicalchange)
m1 <- lmer(dDisadapt ~ diff_economic + (1|eta), clinicalchange)
anova(m0,m1)
plot(allEffects(m1))
summary(m1)


m0 <- lmer(dEDI ~ 1 + (1|eta), clinicalchange)
m1 <- lmer(dEDI ~ diff_economic + (1|eta), clinicalchange)
anova(m0,m1)
plot(allEffects(m1))
summary(m1)



clinicalchange <- subset(ECOS_dupl2, redcap_event_name == "Before Lockdown", colnames(ECOS_dupl2))

clinicalchange$clinicalEDRISK <- as.factor(clinicalchange$clinicalEDRISK)
clinicalchange$diff_clinicalEDRISK <- as.factor(clinicalchange$diff_clinicalEDRISK)

#we are interested in those whose symptoms get worse

levels(clinicalchange$clinicalEDRISK)[levels(clinicalchange$clinicalEDRISK)=="0"] <- "Mild"
levels(clinicalchange$clinicalEDRISK)[levels(clinicalchange$clinicalEDRISK)=="1"] <- "Typical"
levels(clinicalchange$clinicalEDRISK)[levels(clinicalchange$clinicalEDRISK)=="2"] <- "Severe"

#little update

table(clinicalchange$clinicalEDRISK, clinicalchange$diff_clinicalEDRISK)

clinicalchange <- data.frame(cbind(clinicalchange, diffplot$dEDI))

EDRISKLockdown <- subset(ECOS_dupl2, redcap_event_name == "During Lockdown", colnames(ECOS_dupl2))
EDRISKLockdown$EDRISKLockdown <- EDRISKLockdown$clinicalEDRISK

clinicalchange <- data.frame(cbind(clinicalchange, EDRISKLockdown$EDRISKLockdown))
clinicalchange$dEDI <- clinicalchange$diffplot.dEDI
clinicalchange$EDRISKLockdown <- clinicalchange$EDRISKLockdown.EDRISKLockdown


#using nnet package to run multinomial regression
m0 <- multinom(diff_clinicalEDRISK ~ 1, clinicalchange)
summary(m0)

m1 <- multinom(diff_clinicalEDRISK ~ dEDI, clinicalchange)
summary(m1)

#######
P <- predict(m1, newdata = dEDI,  "probs")

pp <- data.frame(cbind(P, clinicalchange$dEDI))
#summarise the probabilities per clinical category


#-1
group_by(pp) %>%
  summarise(
    count = n(),
    median = median(X.1, na.rm = TRUE),
    quantile1st = quantile(X.1, probs = 0.25, na.rm = TRUE),
    quantile3rd = quantile(X.1, probs= 0.75, na.rm = TRUE)
  )

#0
group_by(pp) %>%
  summarise(
    count = n(),
    median = median(X0, na.rm = TRUE),
    quantile1st = quantile(X0, probs = 0.25, na.rm = TRUE),
    quantile3rd = quantile(X0, probs= 0.75, na.rm = TRUE)
  )

#+1
group_by(pp) %>%
  summarise(
    count = n(),
    median = median(X1, na.rm = TRUE),
    quantile1st = quantile(X1, probs = 0.25, na.rm = TRUE),
    quantile3rd = quantile(X1, probs= 0.75, na.rm = TRUE)
  )


#dEDI
group_by(pp) %>%
  summarise(
    count = n(),
    median = median(V4, na.rm = TRUE),
    quantile1st = quantile(V4, probs = 0.25, na.rm = TRUE),
    quantile3rd = quantile(V4, probs= 0.75, na.rm = TRUE)
  )

ggplot(pp, aes(x=V4, y=X0)) + 
  #facet_wrap(~ V5, ncol = 2) + # Multi plot windcows by ~ "" , scale="free"
  #geom_violin(trim=TRUE, scale = "width") + # trim=F (trim the tails of the violins)
  #geom_hline(aes(yintercept = meanmale), color = "deepskyblue3",size = 1)+
  #geom_hline(aes(yintercept = meanfemale), color = "deeppink2", size = 1)+
  #geom_hline(aes(yintercept = 0), color = "gray30", size = 1, linetype = "dashed")+
  #geom_boxplot(width=0.85, lwd=1 ,outlier.size=0.5, outlier.shape=16, outlier.colour="transparent",position = position_dodge(width=0)) + # ,color="white"
  scale_color_manual(values=c("deeppink2","deepskyblue3")) +
  annotate("segment", x = -0.081, xend = -0.081, y=0, yend=1, color = "goldenrod2", size =1, linetype = "dashed") +
  annotate("segment", x = 0.13, xend = 0.13, y=0, yend=1, color = "goldenrod2", size =1, linetype = "dashed") +
  geom_jitter(shape=16, position=position_jitter(0.15), size=3,
              alpha=0.5, color = "gray70") +
  geom_line()+
  geom_line(aes(x = V4, y = X1))+
  geom_jitter(aes(x= V4, y= X1), shape=17, position=position_jitter(0.15), size=3,
              alpha=0.5, color = "gray0") +
  #stat_summary(fun.data=median_mad, geom="pointrange", size = 1,col="gray2") +
  
  #  scale_color_manual(values= c("gray20","gray20","gray20")) + # Use custom color palettes (conditioncol)
  #stat_summary(fun.y=mean, geom="pointrange", shape=18, size=3, col="goldenrod4") + # Add mean
  theme_bw() + # or minimal/grey/classic
  theme(panel.background = element_rect(fill = "transparent", colour = NA), # or theme_blank()
        plot.background = element_rect(fill = "transparent", colour = NA),
        strip.background = element_blank(),
        #strip.text.x = element_text(size =20),
        axis.text=element_text(size=25),
        #axis.line.x = element_blank(),
        #axis.ticks.x = element_blank(),
        axis.line = element_line(size=sl, colour="black"),
        axis.title.y = element_text(size=30, face="plain",colour="black", margin = margin(t = 0, r = 5, b = 0, l = 0)),
        axis.title.x = element_text(size=30, face="plain",colour="black", margin = margin(t = 5, r = 0, b = 0, l = 0), hjust = 0.5),
        axis.ticks.y = element_line(size = sl, colour="black"),
        axis.ticks.x = element_blank(),
        axis.ticks.length = unit(st,"cm"),
        axis.text.x = element_text(angle = 45,face="plain",colour="black", margin = margin(t = 0, r = 5, b = 0, l = 0), vjust = 1, hjust =1),
        #axis.text.y = element_text(size = sa,family="sans",face = "plain", colour="black", hjust = 1), #hjust = 0.5
        #axis.text.y = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid = element_blank(),
        panel.border = element_blank(),
        legend.position="none") +
  theme(aspect.ratio=1) +
  coord_capped_cart(bottom="left",left="top", gap=0.5) +
  #scale_x_continuous(breaks = c(0,2,4,6,8,10,12,14)) +
  ylim(0,1)+
  xlim(-0.5,1)+
  labs(y = "P(clinical change)") +
  labs(x = "% ??ED Risk")




ECOS_dupl2 <- data.frame(cbind(ECOS_dupl2, diff_clinicalDISADAPT))

clinicalchange <- subset(ECOS_dupl2, redcap_event_name == "Before Lockdown", colnames(ECOS_dupl2))

clinicalchange$clinicalDISADAPT <- as.factor(clinicalchange$clinicalDISADAPT)
clinicalchange$diff_clinicalDISADAPT <- as.factor(clinicalchange$diff_clinicalDISADAPT)

#we are interested in those whose symptoms get worse

levels(clinicalchange$clinicalDISADAPT)[levels(clinicalchange$clinicalDISADAPT)=="0"] <- "Mild"
levels(clinicalchange$clinicalDISADAPT)[levels(clinicalchange$clinicalDISADAPT)=="1"] <- "Typical"
levels(clinicalchange$clinicalDISADAPT)[levels(clinicalchange$clinicalDISADAPT)=="2"] <- "Severe"

#little update

table(clinicalchange$clinicalDISADAPT, clinicalchange$diff_clinicalDISADAPT)

clinicalchange <- data.frame(cbind(clinicalchange, diffplot$dDisadapt))

DISADAPTLockdown <- subset(ECOS_dupl2, redcap_event_name == "During Lockdown", colnames(ECOS_dupl2))
DISADAPTLockdown$DISADAPTLockdown <- DISADAPTLockdown$clinicalDISADAPT

clinicalchange <- data.frame(cbind(clinicalchange, DISADAPTLockdown$DISADAPTLockdown))
clinicalchange$dEDI <- clinicalchange$diffplot.dDisadapt
clinicalchange$DISADAPTLockdown <- clinicalchange$DISADAPTLockdown.DISADAPTLockdown


#using nnet package to run multinomial regression
m0 <- multinom(diff_clinicalEDRISK ~ 1, clinicalchange)
summary(m0)

m1 <- multinom(diff_clinicalEDRISK ~ dEDI, clinicalchange)
summary(m1)

#######
P <- predict(m1, newdata = dDisadapt,  "probs")

pp <- data.frame(cbind(P, clinicalchange$dEDI))

#summarise the probabilities per clinical category