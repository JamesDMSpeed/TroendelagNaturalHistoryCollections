#Climate data for Trøndelag

#Monthly homogenised means downloaded from seklima.no (Norsk Klimaservicesenter [klimaservicesenter.no])
#All stations in Trøndelag

#Packages
library(data.table)#fread
library(segmented)#Segmented regression


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
segAnnual<-segmented(lmAnnual,seg.Z=~Year)
segAnnual
segSummer<-segmented(lmSummer,seg.Z=~Year)
segSummer
davies.test(lmAnnual,seg.Z = ~Year,k=100)
davies.test(lmSummer,seg.Z = ~Year,k=100)
#No significant break points in the temperture records
