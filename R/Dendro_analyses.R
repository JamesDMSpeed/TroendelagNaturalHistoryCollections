#Dendro

library(segmented)
library(mgcv)
library(gratia)
library(visreg)


dendrodat<-read.csv("Data/Dendro/Dendro.csv",header=T,sep=";",dec=",")
names(dendrodat)[1]<-"Year"

dendrodat<-dendrodat[dendrodat$Year>=1900,]


plot(dendrodat$Year,dendrodat$Pinus_syl_StdCrn,type='b')
plot(dendrodat$Year,dendrodat$Picea_abies_StdCrn,type='b')

#Linear regressions
lmPinsyl<-lm(Pinus_syl_StdCrn~Year,data=dendrodat)
summary(lmPinsyl)
lmPicea<-lm(Picea_abies_StdCrn~Year,data=dendrodat)
summary(lmPicea)

segPinus<-segmented(lmPinsyl,seg.Z=~Year,npsi = 2,data=dendrodat)
segPinus
segPicea<-segmented(lmPicea,seg.Z=~Year,npsi = 2,data=dendrodat)
segPinus
pscore.test(lmPinsyl,seg.Z=~Year,n.break = 2)
pscore.test(lmPicea,seg.Z=~Year,n.break = 2)


gamPicea<-gam(Picea_abies_StdCrn~s(Year),data=dendrodat)
gamPinus<-gam(Pinus_syl_StdCrn~s(Year),data=dendrodat)

dPinusdt<-derivatives(gamPinus)
dPiceadt<-derivatives(gamPicea)

par(mfcol=c(3,2))
par(mar=c(1,5,3,1))
plot(dendrodat$Year,dendrodat$Pinus_syl_StdCrn,type='b',main="Pinus",las=1,ylab="Detrended growth")
abline(v=segPinus$psi[,2],lty=2,col=2)
par(mar=c(1,5,1,1))
visreg(gamPinus)
par(mar=c(5,5,1,1))
plot(dPinusdt$data,dPinusdt$derivative,type='l',xlab="Year",las=1,ylab="Derivative")
lines(dPinusdt$data,dPinusdt$upper,type='l',col=grey(0.5))
lines(dPinusdt$data,dPinusdt$lower,type='l',col=grey(0.5))
abline(h=0)

par(mar=c(1,5,3,1))
plot(dendrodat$Year,dendrodat$Picea_abies_StdCrn,type='b',main="Picea",las=1,ylab="Detrended growth")
#abline(v=segPicea$psi[,2],lty=2)
par(mar=c(1,5,1,1))
visreg(gamPicea)
par(mar=c(5,5,1,1))
plot(dPiceadt$data,dPiceadt$derivative,type='l',las=1,xlab="Year",ylab="Derivative")
lines(dPiceadt$data,dPiceadt$upper,type='l',col=grey(0.5))
lines(dPiceadt$data,dPiceadt$lower,type='l',col=grey(0.5))
abline(h=0)

cor.test(dendrodat$Picea_abies_StdCrn,dendrodat$Year)
cor.test(dendrodat$Pinus_syl_StdCrn,dendrodat$Year)

#Climate data
climdata<-read.csv("Data/ClimateData_forAnalyses.csv",header=T)

dendroclim<-merge(climdata,dendrodat,by='Year')

par(mfrow=c(1,1))
par(mar=c(5,5,3,1))
plot(dendroclim$Annual,dendroclim$Pinus_syl_StdCrn,xlab=expression("Annual Temperature ("*~degree*C*")"),las=1,
     ylab="Detrended growth",main="Pinus")
lm1<-lm(Pinus_syl_StdCrn~Annual,data=dendroclim)
summary(lm1)
abline(lm1)
