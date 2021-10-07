#Plant phenology anlaysis

#Libraries
library(readxl)#Reading direct from Excel
library(ggplot2)#plots
library(mgcv)#GAMs
library(gratia)#Gam derivatives
library(visreg)#Gam plots
library(metafor)


#Read in data

plantphen <- read_excel("Data/Plants/Phenology_herb_sheets_for_analysis.xlsx")
plantphendat1<-read.csv("Data/Plants/PlantPhenology.csv",header=T,dec=',',sep=';')

#Climate data
climdata<-read.csv("Data/ClimateData_forAnalyses.csv",header=T)



#FIx spp name
names(plantphendat1)[1]<-"Scientific.name"

#Look at dates samples were made
plantphendat1$sampledDate<-as.Date(plantphendat1$eventDate_ed,format="%d.%m.%Y")
plantphendat1$sampledDate
summary(plantphendat1$sampledDate)

hist(plantphendat1$sampledDate,breaks="years")

plantphendat1$Year<-as.numeric(format(plantphendat1$sampledDate,'%Y'))
plantphendat1$DayofYear<-as.numeric(format(plantphendat1$sampledDate,"%j"))

hist(plantphendat1$DayofYear)

#Merge climate and phenology data
plantphendat<-merge(plantphendat1,climdata,by.x='Year',by.y = "Year",all.x=T,all.y=F)


ggplot(plantphendat,aes(x=DayofYear))+geom_histogram()+
  facet_grid(Scientific.name ~.)


ggplot(plantphendat,aes(x=Year))+geom_histogram()+
  facet_grid(Scientific.name ~.)

#Look at species
levels(as.factor(plantphendat$Scientific.name))

par(mfrow=c(6,4))
par(mar=c(1,1,1,1))
for(i in 1:24){
with(plantphendat[plantphendat$Scientific.name==levels(plantphendat$Scientific.name)[i],],plot(DayofYear,Flowering.intensity,main=levels(plantphendat$Scientific.name)[i]))
}

ggplot(plantphendat,aes(x=DayofYear,y=Flowering.intensity))+geom_point(aes(color=Year))+#geom_smooth(colour="black")+
  facet_grid(Scientific.name~.)+
  theme(strip.text.y.right = element_text(angle = 0))

#lmQ<-with(plantphendat[plantphendat$Scientific.name==levels(plantphendat$Scientific.name)[1],],lm(Flowering.intensity~I(Year^2)+Year))
#summary(lmQ)

table(plantphendat$DayofYear,plantphendat$Year)
table(plantphendat$Scientific.name,plantphendat$Year)

image(as.matrix(table(plantphendat$DayofYear,plantphendat$Year)))


#DF - species, year, doy, floweringintensity, senecscene intensity
with(plantphendat,tapply(Flowering.intensity,list(Scientific.name,Year),max))


#Looking at Residuals from a GAM fit. Vs year
gam1<-gam(Flowering.intensity ~ s(DayofYear) , 
          data = plantphendat[plantphendat$Scientific.name==levels(plantphendat$Scientific.name)[2],],
          method = "REML")

with(plantphendat[plantphendat$Scientific.name==levels(plantphendat$Scientific.name)[2],],
     plot(DayofYear,Flowering.intensity,main=levels(plantphendat$Scientific.name)[2]))
plot(gam1,add=T)
visreg(gam1,add=T)

#Quad reg not better
lmQ1<-with(plantphendat[plantphendat$Scientific.name==levels(plantphendat$Scientific.name)[2],],
           lm(Flowering.intensity~I(DayofYear^2)+DayofYear))
anova(lmQ1)
newdat<-data.frame(DayofYear=25:308)
newdat$p<-predict(lmQ1,newdat)
lines(newdat$DayofYear,newdat$p)

plot(plantphendat$Year[plantphendat$Scientific.name==levels(plantphendat$Scientific.name)[2]
     & !is.na(plantphendat$Flowering.intensity)],
     residuals(gam1))
abline(h=0)

cor.test(plantphendat$Year[plantphendat$Scientific.name==levels(plantphendat$Scientific.name)[2]
                       & !is.na(plantphendat$Flowering.intensity)],
     residuals(gam1))
#Problem here is that the 

dgam1<-derivatives(gam1)
par(mfrow=c(2,1))
par(mar=c(1,1,1,1))
visreg(gam1)
abline(h=0)
plot(dgam1$data,dgam1$derivative,type='l')
abline(h=0)


plot(plantphendat$Year[plantphendat$Scientific.name==levels(plantphendat$Scientific.name)[2]
                       & !is.na(plantphendat$Flowering.intensity)&plantphendat$DayofYear<140],
     residuals(gam1)[plantphendat<140])



#THen find the earliest date with max flowering? 

sp2<-plantphendat[plantphendat$Scientific.name==levels(plantphendat$Scientific.name)[2],]

plot(sp2$DayofYear,sp2$Flowering.intensity)

quantile(sp2$Flowering.intensity,na.rm=T)


#Looping through some correlations
plantphenCordf<-data.frame(Species=levels(as.factor(plantphendat$Scientific.name)),
                           R=rep(NA,times=length(levels(as.factor(plantphendat$Scientific.name)))),
                           N=rep(NA,times=length(levels(as.factor(plantphendat$Scientific.name)))))
plantphenRegdf<-data.frame(Species=levels(as.factor(plantphendat$Scientific.name)),
                           b=rep(NA,times=length(levels(as.factor(plantphendat$Scientific.name)))),
                           se=rep(NA,times=length(levels(as.factor(plantphendat$Scientific.name)))))


plantphenTempCordf<-data.frame(Species=levels(as.factor(plantphendat$Scientific.name)),
                           R=rep(NA,times=length(levels(as.factor(plantphendat$Scientific.name)))),
                           N=rep(NA,times=length(levels(as.factor(plantphendat$Scientific.name)))))
plantphenTempRegdf<-data.frame(Species=levels(as.factor(plantphendat$Scientific.name)),
                               b=rep(NA,times=length(levels(as.factor(plantphendat$Scientific.name)))),
                               se=rep(NA,times=length(levels(as.factor(plantphendat$Scientific.name)))))


speciesyrdf<-list()

for(k in 1:length(levels(plantphendat$Scientific.name))){
spk<-plantphendat[plantphendat$Scientific.name==levels(plantphendat$Scientific.name)[k],]

levels(as.factor(spk$Year))     
spkdf<-data.frame(Year=levels(as.factor(spk$Year)),DoY=rep(NA,times=length(levels(as.factor(spk$Year)))))
for(i in 1:length(levels(as.factor(spk$Year)))){
spki<-spk[spk$Year==levels(as.factor(spk$Year))[i],]
spkiMax<-spki[spki$Flowering.intensity>=quantile(spk$Flowering.intensity,na.rm=T)[4],]
#spkdf$Year<-as.numeric(levels(as.factor(spk$Year))[i])
spkdf$DoY[i]<-as.numeric(min(spkiMax$DayofYear))
}
spkdf$DoY[spkdf$DoY=="Inf"]<-NA
spkdf$Year<-as.numeric(as.character(spkdf$Year))
spkdfClim<-merge(spkdf,climdata,by='Year',all.x=T,all.y=F)
plot(spkdf$Year,spkdf$DoY,main=levels(as.factor(plantphendat$Scientific.name))[k],xlab="Year",ylab="Day of Year",las=1)
plot(spkdfClim$Annual,spkdfClim$DoY,main=levels(as.factor(plantphendat$Scientific.name))[k],xlab="Annual temperature",ylab="Day of Year",las=1)
speciesyrdf[[k]]<-spkdf
print(levels(as.factor(plantphendat$Scientific.name))[k])
print(quantile(spk$Flowering.intensity,na.rm=T)[4])
print(cor.test(spkdf$Year,spkdf$DoY))
plantphenCordf$R[plantphenCordf$Species==spk$Scientific.name[1]]<-cor(spkdf$Year,spkdf$DoY,use='pairwise')
plantphenCordf$N[plantphenCordf$Species==spk$Scientific.name[1]]<-length(na.omit(spkdf$DoY))
plantphenTempCordf$R[plantphenTempCordf$Species==spk$Scientific.name[1]]<-cor(spkdfClim$Annual,spkdfClim$DoY,use='pairwise')
plantphenTempCordf$N[plantphenTempCordf$Species==spk$Scientific.name[1]]<-length(na.omit(spkdfClim$DoY))
plantphenRegdf$b[plantphenCordf$Species==spk$Scientific.name[1]]<-summary(lm(spkdf$DoY~spkdf$Year))$coefficients[2,1]
plantphenRegdf$se[plantphenCordf$Species==spk$Scientific.name[1]]<-summary(lm(spkdf$DoY~spkdf$Year))$coefficients[2,2]
plantphenTempRegdf$b[plantphenRegdf$Species==spk$Scientific.name[1]]<-summary(lm(spkdfClim$DoY~spkdfClim$Annual))$coefficients[2,1]
plantphenTempRegdf$se[plantphenRegdf$Species==spk$Scientific.name[1]]<-summary(lm(spkdfClim$DoY~spkdfClim$Annual))$coefficients[2,2]
}

plantphenCordf
plantphenTempCordf
plantphenRegdf
plantphenTempRegdf
speciesyrdf

#Meta analyses -> NB only species with >= 5 years of records
esplantphenyr<-escalc(measure="COR",ri=plantphenCordf$R[plantphenCordf$N>=5],ni=plantphenCordf$N[plantphenCordf$N>=5])
rmaplantphenyr<-rma(esplantphenyr)
forest(rmaplantphenyr,slab=plantphenCordf$Species[plantphenCordf$N>=5],main="Plant phenology - Year",sub="Earliest DoY with 75% quartile species flowering intensity")

esplantphentemp<-escalc(measure="COR",ri=plantphenTempCordf$R[plantphenTempCordf$N>=5],ni=plantphenTempCordf$N[plantphenTempCordf$N>=5])
rmaplantphentemp<-rma(esplantphentemp)
forest(rmaplantphentemp,slab=plantphenTempCordf$Species[plantphenTempCordf$N>=5],main="Plant phenology - Temperature",sub="Earliest DoY with 75% quartile species flowering intensity")

rmaplantphenReg<-rma(yi=plantphenRegdf$b[plantphenCordf$N>=5],sei=plantphenRegdf$se[plantphenCordf$N>=5],measure="GEN")
forest(rmaplantphenReg,slab=plantphenRegdf$Species[plantphenCordf$N>=5],main="Plant phenology - Year",xlab="Regression Slope",sub="Change in earliest DoY of flowering intensity at 75% quantile of species maximum")

rmaplantphenTempReg<-rma(yi=plantphenTempRegdf$b[plantphenCordf$N>=5],sei=plantphenTempRegdf$se[plantphenCordf$N>=5],measure="GEN")
forest(rmaplantphenTempReg,slab=plantphenTempRegdf$Species[plantphenCordf$N>=5],main="Plant phenology - Temperature",xlab="Regression Slope",sub="Change in earliest DoY of flowering intensity at 75% quantile of species maximum")


segspeciesdf<-data.frame(Species=plantphenCordf$Species,P=rep(NA,times=24),
                         Bp1=rep(NA,times=24),Bp1se=rep(NA,times=24),Bp2=rep(NA,times=24),Bp2se=rep(NA,times=24))

#Segmented regressions
#for(i in which(plantphenTempCordf$N>=5)){
for (i in 1:24){
    print(levels(as.factor(plantphenCordf$Species))[i])
with(speciesyrdf[[i]],plot(Year,DoY,main=levels(as.factor(plantphenCordf$Species))[i]))
lm1<-with(speciesyrdf[[i]],lm(DoY~Year))
abline(lm1)
summary(lm1)     
if(plantphenCordf$N[i]>=5)
seglm1<-segmented(lm1,seg.Z=~Year,nspsi=2, data=speciesyrdf[[i]])
seglm1
plot(seglm1,add=T)
ps1<-(pscore.test(lm1,seg.Z=~Year,n.break = 2))
print(ps1)
print(seglm1$psi)
segspeciesdf$P[i]<-ps1$p.value
segspeciesdf[i,3]<-ifelse(is.null(seglm1$psi),rep(NA,times=2),
                            ifelse(nrow(seglm1$psi==1),seglm1$psi[2],seglm1$psi[1,2]))
segspeciesdf[i,4]<-ifelse(is.null(seglm1$psi),rep(NA,times=2),
                          ifelse(nrow(seglm1$psi==1),seglm1$psi[3],seglm1$psi[1,3]))
segspeciesdf[i,5]<-ifelse(is.null(seglm1$psi),rep(NA,times=2),
                          ifelse(nrow(seglm1$psi==1),rep(NA,times=2),seglm1$psi[2,2]))
segspeciesdf[i,6]<-ifelse(is.null(seglm1$psi),rep(NA,times=2),
                          ifelse(nrow(seglm1$psi==1),rep(NA,times=2),seglm1$psi[2,3]))

}

segspeciesdf
segspeciesdf[segspeciesdf$P<0.05,]

write.csv(plantphenCordf,"Data/Plants/PlantYearCor.csv")
write.csv(plantphenTempCordf,"Data/Plants/PlantTempCor.csv")
write.csv(segspeciesdf,"Data/Plants/PlantYearBreakpoints.csv")
write.csv(plantphenRegdf,"Data/Plants/PlantYearReg.csv")
write.csv(plantphenTempRegdf,"Data/Plants/PlantTempReg.csv")


par(mfrow=c(2,1))
par(mar=c(3,3,1,1))
with(speciesyrdf[[7]],plot(Year,DoY,main=levels(as.factor(plantphenCordf$Species))[7]))
lm1<-with(speciesyrdf[[7]],lm(DoY~Year))
abline(lm1)
summary(lm1)     
seglm1<-segmented(lm1,seg.Z=~Year,nspsi=2, data=speciesyrdf[[7]])
seglm1
plot(seglm1,add=T,col=2)
abline(v=seglm1$psi[[2]],col=2)

with(speciesyrdf[[21]],plot(Year,DoY,main=levels(as.factor(plantphenCordf$Species))[21]))
lm1<-with(speciesyrdf[[21]],lm(DoY~Year))
abline(lm1)
summary(lm1)     
seglm1<-segmented(lm1,seg.Z=~Year,nspsi=2, data=speciesyrdf[[21]])
seglm1
plot(seglm1,add=T,col=2)
abline(v=seglm1$psi[[2]],col=2)

