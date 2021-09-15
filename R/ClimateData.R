#Climate data for Trøndelag

#Monthly homogenised means downloaded from seklima.no (Norsk Klimaservicesenter [klimaservicesenter.no])
#All stations in Trøndelag

#Packages
library(data.table)#fread
library(segmented)#Segmented regression
library(mgcv)#GAMs
library(gratia)#Gam derivatives
library(visreg)#Gam plots

#--

climdat1<-fread('Data/tableA-Trd.csv',header=T,sep=";",dec=",")
climdat2<-fread('Data/tableTrd-end.csv',header=T,sep=";",dec=",")

climdat<-rbind(climdat1,climdat2)
head(climdat)

climdat$Month<-as.numeric(substr(climdat$`Tid(norsk normaltid)`,1,2))
climdat$Year<-substr(climdat$`Tid(norsk normaltid)`,4,7)

climdat1900<-climdat[climdat$Year>=1900 & climdat$Year<2021,]
climdat1900summer<-climdat1900[climdat1900$Month>=6 & climdat1900$Month<=8,]


#Annual temperature 
yearavg<-tapply(climdat1900$`Homogenisert middeltemperatur (mnd)`,list(climdat1900$Year),mean)
#Summer temperature
summeravg<-tapply(climdat1900summer$`Homogenisert middeltemperatur (mnd)`,list(climdat1900summer$Year),mean)

#par(mfrow=c(2,1))
#par(mar=c(5,5,1,1))
plot(1900:2020,yearavg,type='b',xlab="Year",ylab=expression("Annual Temperature ("*~degree*C*")"),las=1,pch=16,cex=0.5)
lines(1900:2020,frollmean(yearavg,5),col=2,lwd=2)
plot(1900:2020,summeravg,type='b',xlab="Year",ylab=expression("Summer Temperature ("*~degree*C*")"),las=1,pch=16,cex=0.5)
lines(1900:2020,frollmean(summeravg,5),col=2,lwd=2)

#Statistical analysis of trends
timeseriesdf<-data.frame(Year=1900:2020,Annual=yearavg,Summer=summeravg)

write.csv(timeseriesdf,"Data/ClimateData_forAnalyses.csv")

#Linear Models
lmAnnual<-lm(Annual~Year,data=timeseriesdf)
summary(lmAnnual)
plot(1900:2020,yearavg,type='b',xlab="Year",ylab=expression("Annual Temperature ("*~degree*C*")"),las=1,pch=16,cex=0.5)
lines(1900:2020,frollmean(yearavg,5),col=2,lwd=2)
abline(lmAnnual)
text(1910, 7,paste("R2 = ",round(summary(lmAnnual)$r.squared,2)))

lmSummer<-lm(Summer~Year,data=timeseriesdf)
summary(lmSummer)
plot(1900:2020,summeravg,type='b',xlab="Year",ylab=expression("Summer Temperature ("*~degree*C*")"),las=1,pch=16,cex=0.5)
lines(1900:2020,frollmean(summeravg,5),col=2,lwd=2)
abline(lmSummer)
text(1910, 15,paste("R2 = ",round(summary(lmSummer)$r.squared,2)))

#Segmented regression
segAnnual<-segmented(lmAnnual,seg.Z=~Year,npsi=2)
segAnnual
segSummer<-segmented(lmSummer,seg.Z=~Year,nspi=2)
segSummer
davies.test(lmAnnual,seg.Z = ~Year,k=100)
davies.test(lmSummer,seg.Z = ~Year,k=100)
#No significant break points in the temperture records

#With hypothesis of 2 break points
pscore.test(lmAnnual,seg.Z=~Year,k=100,n.break=2,alternative = "less")
pscore.test(lmSummer,seg.Z=~Year,k=100,n.break=2,alternative = "less")

par(mfrow=c(2,1))
plot(1900:2020,yearavg,type='b',xlab="Year",ylab=expression("Annual Temperature ("*~degree*C*")"),las=1,pch=16,cex=0.5,cex.lab=0.8)
#lines(1900:2020,frollmean(yearavg,5),col=2,lwd=2)
abline(lmAnnual)
abline(v=c(1946,1979),lty=2)
text(1910, 6,paste("R2 = ",round(summary(lmAnnual)$r.squared,2)))
plot(1900:2020,summeravg,type='b',xlab="Year",ylab=expression("Summer Temperature ("*~degree*C*")"),las=1,pch=16,cex=0.5,cex.lab=0.8)
#lines(1900:2020,frollmean(summeravg,5),col=2,lwd=2)
abline(lmSummer)
text(1910, 15,paste("R2 = ",round(summary(lmSummer)$r.squared,2)))


#GAMs
#Annual
gam1<-gam(Annual ~ s(Year) , data = timeseriesdf, method = "REML")
summary(gam1)
plot(gam1,residuals=TRUE)
points(1900:2020,yearavg)#,type='b',xlab="Year",ylab=expression("Annual Temperature ("*~degree*C*")"),las=1,pch=16,cex=0.5)

acf(resid(gam1))


dTdt<-derivatives(gam1)
plot(dTdt$data,dTdt$derivative,type='l')
lines(dTdt$data,dTdt$upper,type='l',col=grey(0.5))
lines(dTdt$data,dTdt$lower,type='l',col=grey(0.5))


par(mfrow=c(3,1))
par(mar=c(1,5,1,1))
plot(1900:2020,yearavg,type='b',
     xlab="",ylab=expression("Annual Temperature ("*~degree*C*")"),las=1,pch=16,cex=0.5)
abline(v=c(1946,1979),lty=2)
plot(segAnnual,add=T)
#lines(1900:2020,frollmean(yearavg,5),col=2,lwd=2)
visreg(gam1,residuals=TRUE,xlab="")
abline(v=c(1946,1979),lty=2)
par(mar=c(5,5,1,1))
plot(dTdt$data,dTdt$derivative,type='l',xlab="Year",ylim=c(-0.06,0.12),las=1)
lines(dTdt$data,dTdt$upper,type='l',col=grey(0.5))
lines(dTdt$data,dTdt$lower,type='l',col=grey(0.5))
abline(h=0)
abline(v=c(1946,1979),lty=2)


#Summer
gamS<-gam(Summer ~ s(Year) , data = timeseriesdf, method = "REML")
summary(gamS)
plot(gamS,residuals=TRUE)
points(1900:2020,yearavg)#,type='b',xlab="Year",ylab=expression("Annual Temperature ("*~degree*C*")"),las=1,pch=16,cex=0.5)

acf(resid(gamS))


dSTdt<-derivatives(gamS)
plot(dSTdt$data,dSTdt$derivative,type='l')
lines(dSTdt$data,dSTdt$upper,type='l',col=grey(0.5))
lines(dSTdt$data,dSTdt$lower,type='l',col=grey(0.5))


par(mfrow=c(3,1))
par(mar=c(1,5,1,1))
plot(1900:2020,summeravg,type='b',
     xlab="",ylab=expression("Summer Temperature ("*~degree*C*")"),las=1,pch=16,cex=0.5)
visreg(gamS,residuals=TRUE,xlab="")
par(mar=c(5,5,1,1))
plot(dSTdt$data,dSTdt$derivative,type='l',xlab="Year",ylim=c(-0.06,0.12))
lines(dSTdt$data,dSTdt$upper,type='l',col=grey(0.5))
lines(dSTdt$data,dSTdt$lower,type='l',col=grey(0.5))
abline(h=0)
