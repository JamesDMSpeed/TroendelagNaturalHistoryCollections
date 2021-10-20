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



##Oceanic data
library(ncdf4)
ncbud = nc_open("Data/bud.nc")

#Temperature data at different depths and times

temp<-ncvar_get(ncbud,"TEMP_ADJUSTED")
time<-ncvar_get(ncbud,"TIME") #Days since 1950.01.01
depth<-ncvar_get(ncbud,"DEPH")#Depth in m 

seadate<-as.Date(time, origin = '1950-01-01')
seayear<-format(seadate,format="%Y")
seamonth<-format(seadate,format="%m")
table(seayear,seamonth)

#Series at  depth
temp1m<-temp[1,]
temp5m<-temp[2,]
temp200m<-temp[11,]
plot(seadate,temp1m,type='l')

#Annual averages
countrecordsperyr<-(tapply(temp5m,seayear,length))

yearseq<-data.frame(Year=seq(min(seayear),max(seayear),by=1))

temp1mAnn<-data.frame(tapply(temp1m,seayear,max))#Max temp as variable records
names(temp1mAnn)[1]<-"Temp1m"
temp1mAnn$Year<-rownames(temp1mAnn)
temp1mdf<-merge.data.frame(temp1mAnn,yearseq,all.y=T)
plot(temp1mdf$Year,temp1mdf$Temp1m,type='b')


temp5mAnn<-data.frame(tapply(temp5m,seayear,max))#Max temp as variable records
names(temp5mAnn)[1]<-"Temp5m"
temp5mAnn$Year<-rownames(temp5mAnn)
temp5mdf<-merge.data.frame(temp5mAnn,yearseq,all.y=T)
plot(temp5mdf$Year,temp5mdf$Temp5m,type='b')


temp200mAnn<-data.frame(tapply(temp200m,seayear,max))#Max temp as variable records
names(temp200mAnn)[1]<-"Temp200m"
temp200mAnn$Year<-rownames(temp200mAnn)
temp200mdf<-merge.data.frame(temp200mAnn,yearseq,all.y=T)
plot(temp200mdf$Year,temp200mdf$Temp200m,type='b',main="Max annual temp at 200m")

write.csv(temp200mAnn,"Data/MarineMaxTemp200m.csv")
