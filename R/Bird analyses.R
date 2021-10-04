#Bird data
#Magne & Peter
#Number of territories per km2

#Species
#Løvsanger Willow warbler
#Bjørkefink Brambling
#Gråtrost Fieldfare
#Trepiplerke Tree pipit
#Blåstrupe Bluetroat
#Sivspurv Reed bunting
#Rødvingetrost Redwing
#Gråsisik Redpoll

#Drop fieldfare, redboll and brambling

#Read in data
#Birds
birds<-read.csv("Data/Birds/Budalen pr 2018.csv",header = T,sep=";",dec=",")

#Climate data
climdata<-read.csv("Data/ClimateData_forAnalyses.csv",header=T)


birdclim<-merge(birds,climdata,by.x='year',by.y = "Year")


#Plot some time series

par(mfrow=c(2,4))
par(mar=c(3,3,1,1))
plot(birds$year,birds$lovsange,type='b',las=1,main="Willow warbler")
plot(birds$year,birds$bjorkefi,type='b',las=1,main="Brambling")
plot(birds$year,birds$graatros,type='b',las=1,main="Fieldfare")
plot(birds$year,birds$trepiple,type='b',las=1,main="Tree pipit")
plot(birds$year,birds$blaastru,type='b',las=1,main="Bluethroat")
plot(birds$year,birds$sivspurv,type='b',las=1,main="Reed bunting")
plot(birds$year,birds$rodvinge,type='b',las=1,main="Redwing")
plot(birds$year,birds$graasisi,type='b',las=1,main="Redpoll")


#Linear models. ~year
lmlovsange<-with(birds,lm(lovsange~year))
summary(lmlovsange)
lmbjorkefi<-with(birds,lm(bjorkefi~year))
summary(lmbjorkefi)
lmgraatros<-with(birds,lm(graatros~year))
summary(lmgraatros)
lmtrepiple<-with(birds,lm(trepiple~year))
summary(lmtrepiple)
lmblaastru<-with(birds,lm(blaastru~year))
summary(lmblaastru)
lmsivspurv<-with(birds,lm(sivspurv~year))
summary(lmsivspurv)
lmrodvinge<-with(birds,lm(rodvinge~year))
summary(lmrodvinge)
lmgraasisi<-with(birds,lm(graasisi~year))
summary(lmgraasisi)

#Segmented models
seglovsange<-segmented(lmlovsange,seg.Z=~year,npsi = 2,data=birds)
seglovsange
pscore.test(lmlovsange,seg.Z=~year,n.break = 2)
plot(birds$year,birds$lovsange,type='b',las=1,main="Willow warbler")
plot(seglovsange,add=T,col=2)
abline(v=seglovsange$psi[,2],lty=2,col=2)

segbjorkefi<-segmented(lmbjorkefi,seg.Z=~year,npsi = 2,data=birds)
segbjorkefi
pscore.test(lmbjorkefi,seg.Z=~year,n.break = 2)
plot(birds$year,birds$bjorkefi,type='b',las=1,main="Brambling")
#plot(segbjorkefi,add=T)
#abline(v=segbjorkefi$psi[,2],lty=2)

seggraatros<-segmented(lmgraatros,seg.Z=~year,npsi = 2,data=birds)
seggraatros
pscore.test(lmgraatros,seg.Z=~year,n.break = 2)
plot(birds$year,birds$graatros,type='b',las=1,main="Fieldfare")
plot(seggraatros,add=T,col=2)
abline(v=seggraatros$psi[,2],lty=2,col=2)

segtrepiple<-segmented(lmtrepiple,seg.Z=~year,npsi = 2,data=birds)
segtrepiple
pscore.test(lmtrepiple,seg.Z=~year,n.break = 2)
plot(birds$year,birds$trepiple,type='b',las=1,main="Tree pipit")
#plot(segtrepiple,add=T)
#abline(v=segtrepiple$psi[,2],lty=2)

segblaastru<-segmented(lmblaastru,seg.Z=~year,npsi = 2,data=birds)
segblaastru
pscore.test(lmblaastru,seg.Z=~year,n.break = 2)
plot(birds$year,birds$blaastru,type='b',las=1,main="Blue throat")
#plot(segblaastru,add=T)
#abline(v=segblaastru$psi[,2],lty=2)

segsivspurv<-segmented(lmsivspurv,seg.Z=~year,npsi = 2,data=birds)
segsivspurv
pscore.test(lmsivspurv,seg.Z=~year,n.break = 2)
plot(birds$year,birds$sivspurv,type='b',las=1,main="Reed bunting")
#plot(segsivspurv,add=T)
#abline(v=segsivspurv$psi[,2],lty=2)

segrodvinge<-segmented(lmrodvinge,seg.Z=~year,npsi = 2,data=birds)
segrodvinge
pscore.test(lmrodvinge,seg.Z=~year,n.break = 2)
plot(birds$year,birds$rodvinge,type='b',las=1,main="Redwing")
#plot(segrodvinge,add=T)
#abline(v=segrodvinge$psi[,2],lty=2)

seggraasisi<-segmented(lmgraasisi,seg.Z=~year,npsi = 2,data=birds)
seggraasisi
pscore.test(lmgraasisi,seg.Z=~year,n.break = 2)
plot(birds$year,birds$graasisi,type='b',las=1,main="Redpoll")
#plot(seggraasisi,add=T)
#abline(v=seggraasisi$psi[,2],lty=2)


#Collect breakpoitns in a df

#Temporal correlations
birdcoryeardf<-data.frame(species=names(birds)[3:10],r=rep(NA,times=8),n=rep(length(birds$lovsange[!is.na(birds$lovsange)]),times=8))
for(i in 1:8){
birdcoryeardf$r[i]<-cor(birds$year,birds[,2+i],use="pairwise.complete.obs")
}

#Quick metaanalysis
esbirdyr<-escalc(measure="COR",ri=birdcoryeardf$r,ni=birdcoryeardf$n)
rmabirdyr<-rma(esbirdyr[c(1,4,5,6,7),])
forest(rmabirdyr,slab=birdcoryeardf$species[c(1,4,5,6,7)],main="Birds - Year")


#Temperature correlations
birdcortempdf<-data.frame(species=names(birdclim)[3:10],r=rep(NA,times=8),n=rep(length(birdclim$lovsange[!is.na(birds$lovsange)]),times=8))
for(i in 1:8){
  birdcortempdf$r[i]<-cor(birdclim$Annual,birdclim[,2+i],use="pairwise.complete.obs")
}
esbirdtemp<-escalc(measure="COR",ri=birdcortempdf$r,ni=birdcortempdf$n)
rmabirdtemp<-rma(esbirdtemp[c(1,4,5,6,7),],measure = "COR")
forest(rmabirdtemp,slab=birdcortempdf$species[c(1,4,5,6,7)],main="Birds - Temperature")

par(mfrow=c(2,1))
par(mar=c(4,1,3,1))
forest(rmabirdyr,slab=birdcoryeardf$species[c(1,4,5,6,7)],main="Birds - Year")
forest(rmabirdtemp,slab=birdcortempdf$species[c(1,4,5,6,7)],main="Birds - Temperature")


write.csv(birdcoryeardf,"BirdYearCor.csv")
write.csv(birdcortempdf,"BirdTempCor.csv")

#Temperature models
par(mfrow=c(2,1))
par(mar=c(4,5,2,1))
plot(birdclim$Annual,birdclim$lovsange,xlab=expression("Annual Temperature ("*~degree*C*")"),las=1,
     ylab="Terrorities km2",main="Willow warbler")
lmWWt<-lm(lovsange~Annual,data=birdclim)
summary(lmWWt)
#abline(lm1)
cor.test(birdclim$lovsange,birdclim$Annual,data=birdclim)


plot(birdclim$Annual,birdclim$bjorkefi,xlab=expression("Annual Temperature ("*~degree*C*")"),las=1,
     ylab="Terrorities km2",main="Brambling")
lmBbt<-lm(bjorkefi~Annual,data=birdclim)
summary(lmBbt)
#abline(lm1)

par(mfrow=c(2,4))
par(mar=c(3,3,1,1))
plot(birdclim$Annual,birdclim$lovsange,type='p',las=1,main="Willow warbler")
plot(birdclim$Annual,birdclim$bjorkefi,type='p',las=1,main="Brambling")
plot(birdclim$Annual,birdclim$graatros,type='p',las=1,main="Fieldfare")
plot(birdclim$Annual,birdclim$trepiple,type='p',las=1,main="Tree pipit")
plot(birdclim$Annual,birdclim$blaastru,type='p',las=1,main="Bluethroat")
plot(birdclim$Annual,birdclim$sivspurv,type='p',las=1,main="Reed bunting")
plot(birdclim$Annual,birdclim$rodvinge,type='p',las=1,main="Redwing")
plot(birdclim$Annual,birdclim$graasisi,type='p',las=1,main="Redpoll")

par(mfrow=c(2,4))
par(mar=c(3,3,1,1))
plot(birdclim$Summer,birdclim$lovsange,type='p',las=1,main="Willow warbler")
plot(birdclim$Summer,birdclim$bjorkefi,type='p',las=1,main="Brambling")
plot(birdclim$Summer,birdclim$graatros,type='p',las=1,main="Fieldfare")
plot(birdclim$Summer,birdclim$trepiple,type='p',las=1,main="Tree pipit")
plot(birdclim$Summer,birdclim$blaastru,type='p',las=1,main="Bluethroat")
plot(birdclim$Summer,birdclim$sivspurv,type='p',las=1,main="Reed bunting")
plot(birdclim$Summer,birdclim$rodvinge,type='p',las=1,main="Redwing")
plot(birdclim$Summer,birdclim$graasisi,type='p',las=1,main="Redpoll")
