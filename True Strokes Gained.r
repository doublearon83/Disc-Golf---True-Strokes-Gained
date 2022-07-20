#import data, using Dynamic Discs Open as example
DDO2021_MPO<-read.csv("DDO2021dataMPO.csv",header=T)

#Do some data cleaning
DDO2021_MPO<-DDO2021_MPO[-1]
DDO2021_MPO$tournament<-factor(DDO2021_MPO$tournament)

#Use regression to calculated expected strokes gained
#for "average" touring pro, rating = 1020

#strokes gained tee-to-green
lmstg<-lm(SGTG~Rating,data=DDO2021_MPO)
sgtgr<-coef(lmstg)[1]+1020*coef(lmstg)[2]

#strokes gained putting  
lmsp<-lm(SGP~Rating, data=DDO2021_MPO)
sgpr<-coef(lmsp)[1]+1020*coef(lmsp)[2]

#calculating true strokes gained
DDO2021_MPO$tSGTG<-DDO2021_MPO$SGTG-sgtgr
DDO2021_MPO$tSGP<-DDO2021_MPO$SGP-sgpr

#total true strokes gained
DDO2021_MPO$tottSG<-DDO2021_MPO$tSGTG+DDO2021_MPO$tSGP

#true strokes gained per round
DDO2021_MPO$tSGTG_round<-(DDO2021_MPO$SGTG-sgtgr)/DDO2021_MPO$NR
DDO2021_MPO$tSGP_round<-(DDO2021_MPO$SGP-sgpr)/DDO2021_MPO$NR