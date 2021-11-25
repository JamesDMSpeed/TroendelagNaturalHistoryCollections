#Summary data


rm(list=ls())

library(metafor)
library(ggplot2)

# Study duration and effect size ------------------------------------------



#For associations between duration and es
#Read in correlation effect sizes

#Plants
plantefcoryear1<-read.csv("Data/Plants/PlantYearCor.csv",header=T)
plantefcoryear<-plantefcoryear1[plantefcoryear1$N>=5,]#Drop species with <5 years
plantefcoryear$EcoEffect<-rep("Phenology",times=nrow(plantefcoryear))
plantefcoryear$Kingdom<-rep("Plantae",times=nrow(plantefcoryear))

plantefcortemp<-read.csv("Data/Plants/PlantTempCor.csv",header=T)
plantefcortemp<-plantefcortemp[plantefcortemp$N>=5,]#Drop species with <5 temps
plantefcortemp$EcoEffect<-rep("Phenology",times=nrow(plantefcortemp))
plantefcortemp$Kingdom<-rep("Plantae",times=nrow(plantefcortemp))

#Birds
birdefcoryear<-read.csv("Data/Birds/BirdYearCor.csv",header=T)
birdefcoryear$Duration<-rep(51,times=nrow(birdefcoryear))
birdefcoryear$species<-c("Willow warbler","Brambling","Fieldfare","Tree pipit","Bluethroat","Reed bunting","Redwing","Redpoll")
birdefcoryear<-birdefcoryear[!birdefcoryear$species%in%c("Fieldfare","Brambling","Redpoll"),]
names(birdefcoryear)[2:4]<-c("Species","R","N")
birdefcoryear$EcoEffect<-rep("Abundance",times=nrow(birdefcoryear))
birdefcoryear$Kingdom<-rep("Animalia",times=nrow(birdefcoryear))

birdefcortemp<-read.csv("Data/Birds/BirdTempCor.csv",header=T)
birdefcortemp$Duration<-rep(51,times=nrow(birdefcortemp))
birdefcortemp$species<-c("Willow warbler","Brambling","Fieldfare","Tree pipit","Bluethroat","Reed bunting","Redwing","Redpoll")
birdefcortemp<-birdefcortemp[!birdefcortemp$species%in%c("Fieldfare","Brambling","Redpoll"),]
names(birdefcortemp)[2:4]<-c("Species","R","N")
birdefcortemp$EcoEffect<-rep("Abundance",times=nrow(birdefcortemp))
birdefcortemp$Kingdom<-rep("Animalia",times=nrow(birdefcortemp))

#Dendro
dendrocoryear<-read.csv("Data/Dendro/dendrocoryear",header=T)
dendrocoryear$Duration<-rep(463,times=2)
dendrocoryear$EcoEffect<-rep("Growth",times=2)
dendrocoryear$Kingdom<-rep("Plantae",times=2)

dendrocortemp<-read.csv("Data/Dendro/dendrocortemp",header=T)
dendrocortemp$Duration<-rep(463,times=2)
dendrocortemp$EcoEffect<-rep("Growth",times=2)
dendrocortemp$Kingdom<-rep("Plantae",times=2)

#Marine inverts
marineq10coryear<-read.csv("Data/Marine invertebrates/marineeq10coryear")
marineq90coryear<-read.csv("Data/Marine invertebrates/marineeq90coryear")

marinecoryear<-rbind(marineq10coryear,marineq90coryear)
marineq90coryear$EcoEffect<-c(rep("Distribution",times=9))
marineq90coryear$Kingdom<-rep("Animalia",times=9)
marinecoryear$EcoEffect<-c(rep("Distribution_advance",times=9),rep("Distribution-tail",times=9))
marinecoryear$Kingdom<-rep("Animalia",times=9)

#Terrestrial distributions
indspp<-read.csv("Data/SOD/regslopes_individualSpecies.csv",header=T)
indspp10<-indspp[indspp$N>=10,]
names(indspp10)[c(2,3,8)]<-c("Species","Kingdom","R")
indspp10$EcoEffect<-rep("Distribution",times=nrow(indspp10))
indsppleading<-indspp10[,c(1:2,8,6,7,18,3)]
indsppleadingreg<-indspp10[,c(1:4,10:11,14:15)]

#Jonsvannet inverts
jonsinvMaxAb<-read.csv("Data/Inverts/jonsinvMaxAbCordf")
jonsinvMaxAb$EcoEffect<-rep("Abundance",times=nrow(jonsinvMaxAb))
jonsinvMaxAb$Kingdom<-rep("Animalia",times=nrow(jonsinvMaxAb))

#Atna inverts
atnarich<-read.csv("Data/Inverts/AtnaCorYear.csv")
atnarich$EcoEffect<-rep("Diversity",times=nrow(atnarich))
atnarich$Kingdom<-rep("Animalia",times=nrow(atnarich))
names(atnarich)[names(atnarich)=="Elevation"]<-"Species"
names(atnarich)[3:4]<-c("R","N")


#Bind together
allcorefyear<-rbind(plantefcoryear,birdefcoryear,marineq90coryear,indsppleading,jonsinvMaxAb,atnarich)
#allcoreftemp<-rbind(plantefcortemp,birdefcortemp,dendrocortemp)
allcorefyear$R2<-allcorefyear$R*allcorefyear$R

#Plot study duration against R2
durplot<-ggplot(data=allcorefyear,aes(x=Duration,y=R2))
durplot+geom_point(aes(colour=EcoEffect,shape=Kingdom))+
  labs(y = bquote(~R^2))


lm1<-with(allcorefyear,lm(I(R^2) ~Duration*Kingdom))
summary(lm1)
anova(lm1)

lm2<-with(allcorefyear,lm(I(R^2) ~Duration+Kingdom))
anova(lm2)

lm2<-with(allcorefyear,lm(I(R^2) ~Duration*EcoEffect))
anova(lm2)

lm2<-with(allcorefyear,lm(I(R^2) ~Kingdom+EcoEffect))
anova(lm2)
summary(lm2)


##Duration vs effect size with standardised regression coefficients
#Birds
birdefregscaleyear<-read.csv("Data/Birds/BirdYearScaleReg.csv",header=T)
birdefregscaleyear$Duration<-rep(51,times=nrow(birdefregscaleyear))
birdefregscaleyear$species<-c("Willow warbler","Brambling","Fieldfare","Tree pipit","Bluethroat","Reed bunting","Redwing","Redpoll")
birdefregscaleyear<-birdefregscaleyear[!birdefregscaleyear$species%in%c("Fieldfare","Brambling","Redpoll"),]
names(birdefregscaleyear)[2:4]<-c("Species","b","se")
birdefregscaleyear$EcoEffect<-rep("Abundance",times=nrow(birdefregscaleyear))
birdefregscaleyear$Kingdom<-rep("Animalia",times=nrow(birdefregscaleyear))

birdefregscaletemp<-read.csv("Data/Birds/BirdTempScaleReg.csv",header=T)
birdefregscaletemp$Duration<-rep(51,times=nrow(birdefregscaletemp))
birdefregscaletemp$species<-c("Willow warbler","Brambling","Fieldfare","Tree pipit","Bluethroat","Reed bunting","Redwing","Redpoll")
birdefregscaletemp<-birdefregscaletemp[!birdefregscaletemp$species%in%c("Fieldfare","Brambling","Redpoll"),]
names(birdefregscaletemp)[2:4]<-c("Species","b","se")
birdefregscaletemp$EcoEffect<-rep("Abundance",times=nrow(birdefregscaletemp))
birdefregscaletemp$Kingdom<-rep("Animalia",times=nrow(birdefregscaletemp))



#Plants
plantefregscaleyear<-read.csv("Data/Plants/PlantYearScaleReg.csv",header=T)
plantefregscaleyear<-plantefregscaleyear[plantefcoryear1$N>=5,]#Drop species with <5 years
plantefregscaleyear$Duration<-plantefcoryear$Duration
plantefregscaleyear$EcoEffect<-rep("Phenology",times=nrow(plantefregscaleyear))
plantefregscaleyear$Kingdom<-rep("Plantae",times=nrow(plantefregscaleyear))

plantefregscaletemp<-read.csv("Data/Plants/PlantTempScaleReg.csv",header=T)
plantefregscaletemp<-plantefregscaletemp[plantefcoryear1$N>=5,]#Drop species with <5 temps
plantefregscaletemp$Duration<-plantefcortemp$Duration
plantefregscaletemp$EcoEffect<-rep("Phenology",times=nrow(plantefregscaletemp))
plantefregscaletemp$Kingdom<-rep("Plantae",times=nrow(plantefregscaletemp))



#Jonsvan
jonsinvMaxAbScale<-read.csv("Data/Inverts/jonsinvMaxAbRegScaledf")
jonsinvMaxAbScale$Duration<-jonsinvMaxAb$Duration
jonsinvMaxAbScale$EcoEffect<-rep("Abundance",times=nrow(jonsinvMaxAb))
jonsinvMaxAbScale$Kingdom<-rep("Animalia",times=nrow(jonsinvMaxAb))

jonsinvMaxAbScaleTemp<-read.csv("Data/Inverts/jonsinvMaxAbTempRegScaledf")
jonsinvMaxAbScaleTemp$Duration<-jonsinvMaxAb$Duration
jonsinvMaxAbScaleTemp$EcoEffect<-rep("Abundance",times=nrow(jonsinvMaxAb))
jonsinvMaxAbScaleTemp$Kingdom<-rep("Animalia",times=nrow(jonsinvMaxAb))


#Atna
#Atna inverts
atnarichscale<-read.csv("Data/Inverts/AtnaRegYearScale.csv")
atnarichscale$Duration<-atnarich$Duration
atnarichscale$EcoEffect<-rep("Diversity",times=nrow(atnarich))
atnarichscale$Kingdom<-rep("Animalia",times=nrow(atnarich))
names(atnarichscale)[names(atnarichscale)=="Elevation"]<-"Species"

atnarichScaleTemp<-read.csv("Data/Inverts/AtnaRegTempScale.csv")
atnarichScaleTemp$Duration<-atnarich$Duration
atnarichScaleTemp$EcoEffect<-rep("Diversity",times=nrow(atnarich))
atnarichScaleTemp$Kingdom<-rep("Animalia",times=nrow(atnarich))
names(atnarichScaleTemp)[names(atnarichScaleTemp)=="Elevation"]<-"Species"


#Terrestrial distributions
indspp_scale<-read.csv("Data/SOD/regslopes_individualSpecies_scaled.csv",header=T)
indsppscale10<-indspp_scale[indspp_scale$N>=10,]
names(indsppscale10)[c(2,3,8)]<-c("Species","Kingdom","R")
indsppscale10$EcoEffect<-rep("Distribution",times=nrow(indsppscale10))
indsppscaleleading<-indsppscale10[,c(1:2,8,6,7,18,3)]
indsppscaleleadingreg<-indsppscale10[,c(1:4,7,10:11,18)]
names(indsppscaleleadingreg)[6:7]<-c("b","se")

indspp_scale_temp<-read.csv("Data/SOD/regslopes_individualSpecies_temp_scaled.csv",header=T)
indspp_scale_temp$Duration<-indspp_scale$Duration
indsppscale_temp10<-indspp_scale_temp[indspp_scale$N>=10,]
names(indsppscale_temp10)[c(2,3,10)]<-c("Species","Kingdom","R")
indsppscale_temp10$EcoEffect<-rep("Distribution",times=nrow(indsppscale_temp10))
#indsppscale_templeading<-indsppscale_temp10[,c(1:2,8,6,7,18,3)]
indsppscale_templeadingreg<-indsppscale_temp10[,c(1:4,7,8:9,11)]
names(indsppscale_templeadingreg)[6:7]<-c("b","se")



#Marine dists
marineq90regScaleyear<-read.csv("Data/Marine invertebrates/marineeq90regyear_scale")
marineq90regScaleyear$Duration<-marineq90coryear$Duration
marineq90regScaleyear$Kingdom<-rep("Animalia",times=9)
marineq90regScaleyear$EcoEffect<-c(rep("Distribution",times=9))


#Bind together
allcorefyearScale<-rbind(plantefregscaleyear,birdefregscaleyear,jonsinvMaxAbScale,atnarichscale,indsppscaleleadingreg[,c(1:2,6:7,5,3,8)],marineq90regScaleyear)#,marineq90coryear,indsppleading,jonsinvMaxAb,atnarich)
#Not marine for temperatures
allcoreftempScale<-rbind(plantefregscaletemp,birdefregscaletemp,jonsinvMaxAbScaleTemp,atnarichScaleTemp,indsppscale_templeadingreg[,c(1:2,6:7,5,8,3)])#,marineq90regScaleyear)#,marineq90coryear,indsppleading,jonsinvMaxAb,atnarich)


allcorefyearScale$ResponseType<-allcorefyearScale$EcoEffect
allcoreftempScale$ResponseType<-allcoreftempScale$EcoEffect

#Plot study duration against ES

lmD2<-with(allcorefyearScale,lm(log(abs(b))~Duration))
summary(lmD2)
anova(lmD2)
#newdat<-data.frame(Duration=1:250)
#p1<-predict(lmD2,newdat,se.fit=T,interval=c("confidence"),type="response")
#dat1<-data.frame(cbind(p1$fit,Duration=newdat$Duration))
#with(allcorefyearScale,plot(Duration,abs(b),col=as.factor(EcoEffect),pch=levels(as.factor(Kingdom))))
#lines(newdat$Duration,log(newdat$pred1))
lmDT<-with(allcoreftempScale,lm(log(abs(b))~Duration))
summary(lmDT)
anova(lmDT)

durplot2<-ggplot(data=allcorefyearScale[order(allcorefyearScale$EcoEffect),],aes(x=Duration,y=(abs(b))))

tiff("Figures/DurationsES_scaled.tif",width=8,height=6,res=150,units="in")
durplot2+geom_point(aes(colour=ResponseType,shape=Kingdom))+
  labs(y = "Absolute scaled effect size (log)")+
#  geom_line(data = dat1, aes(y = fit,x=Duration))+
  scale_y_continuous(trans="log10")+
#  stat_smooth(data=allcorefyearScale[allcorefyearScale$EcoEffect=="Phenology",],method='lm',aes(colour=ResponseType),alpha=0.5)+
#  stat_smooth(data=allcorefyearScale[allcorefyearScale$EcoEffect=="Distribution",],method='lm',aes(colour=ResponseType),alpha=1)+
  stat_smooth(method='lm')
dev.off()

durplot2_temp<-ggplot(data=allcoreftempScale[order(allcoreftempScale$EcoEffect),],aes(x=Duration,y=(abs(b))))
durplot2_temp+geom_point(aes(colour=ResponseType,shape=Kingdom))+
  labs(y = "Absolute scaled effect size (log)")+
  #  geom_line(data = dat1, aes(y = fit,x=Duration))+
     scale_y_continuous(trans="log10")+
  #  stat_smooth(data=allcorefyearScale[allcorefyearScale$EcoEffect=="Phenology",],method='lm',aes(colour=ResponseType),alpha=0.5)+
  #  stat_smooth(data=allcorefyearScale[allcorefyearScale$EcoEffect=="Distribution",],method='lm',aes(colour=ResponseType),alpha=1)+
  stat_smooth(method='lm')

#Joint ggplot
allscaleEFboth<-rbind(allcorefyearScale,allcoreftempScale)
allscaleEFboth$Predictor<-c(rep("Year",times=nrow(allcorefyearScale)),rep("Temperature",times=nrow(allcoreftempScale)))

tiff("Figures/DurationsESBoth_scaled.tif",width=8,height=7,res=150,units="in")
durplotBoth<-ggplot(data=allscaleEFboth[order(allscaleEFboth$EcoEffect),],aes(x=Duration,y=(abs(b))))
durplotBoth+geom_point(aes(colour=ResponseType,shape=Kingdom))+
  labs(y = "Absolute scaled effect size (log)", x= "Duration (years)")+
  #  geom_line(data = dat1, aes(y = fit,x=Duration))+
  scale_y_continuous(trans="log10")+
    #  stat_smooth(data=allcorefyearScale[allcorefyearScale$EcoEffect=="Phenology",],method='lm',aes(colour=ResponseType),alpha=0.5)+
  #  stat_smooth(data=allcorefyearScale[allcorefyearScale$EcoEffect=="Distribution",],method='lm',aes(colour=ResponseType),alpha=1)+
  stat_smooth(method='lm')+
  #facet_grid(rows=vars(Predictor),as.table = F)+
  facet_wrap(vars(Predictor),nrow=2,strip.position = "top",as.table = F)+
  theme_bw()
dev.off()
                            
                            
                            
#Is there a releationship if we only look at dists
lmDD<-with(allcorefyearScale[allcorefyearScale$EcoEffect=="Distribution",]
                 ,lm(log(abs(b))~Duration))
summary(lmDD)
anova(lmDD)

lmDDT<-with(allcoreftempScale[allcoreftempScale$EcoEffect=="Distribution",]
                  ,lm(log(abs(b))~Duration))
summary(lmDDT)
anova(lmDDT)


#Is there a releationship if we only look at phenology
lmDP<-with(allcorefyearScale[allcorefyearScale$EcoEffect=="Phenology",]
                 ,lm(log(abs(b))~Duration))
summary(lmDP)
anova(lmDP)

lmDPT<-with(allcoreftempScale[allcoreftempScale$EcoEffect=="Phenology",]
           ,lm(log(abs(b))~Duration))
summary(lmDPT)
anova(lmDPT)


#Temperature duration
allcoreftempScale$DurationTruncated<-allcoreftempScale$Duration
allcoreftempScale$DurationTruncated[allcoreftempScale$Duration>120]<-120

lmTempES<-with(allcoreftempScale,lm((abs(b))~DurationTruncated))
summary(lmTempES)

durplotTemp2<-ggplot(data=allcoreftempScale[order(allcoreftempScale$EcoEffect),],aes(x=DurationTruncated,y=(abs(b))))

#tiff("Figures/DurationsES_scaled_temp.tif",width=8,height=6,res=150,units="in")
durplotTemp2+geom_point(aes(colour=EcoEffect,shape=Kingdom))+
  labs(y = "Absolute scaled effect size")+
  #geom_line()+
  stat_smooth(method='lm')#+
  #scale_y_continuous(trans="log10")
#dev.off()




# Main forest plots -------------------------------------------------------


#For main forest plots
#Read in regression effect sizes

#Plants
plantefregyear<-read.csv("Data/Plants/PlantYearReg.csv",header=T)
plantefregtemp<-read.csv("Data/Plants/PlantTempReg.csv",header=T)

plantefregyear<-plantefregyear[plantefregyear$Species%in%plantefcoryear$Species,]#Drop species with <5 years
plantefregyear
plantefregyear$Kingdom<-rep('Plantae',times=nrow(plantefregyear))
plantefregyear$EcoEffect<-rep('Phenology',times=nrow(plantefregyear))

plantefregtemp<-plantefregtemp[plantefregtemp$Species%in%plantefcortemp$Species,]#Drop species with <5 temps
plantefregtemp
plantefregtemp$Kingdom<-rep('Plantae',times=nrow(plantefregtemp))
plantefregtemp$EcoEffect<-rep('Phenology',times=nrow(plantefregtemp))


#Birds
birdefregyear<-read.csv("Data/Birds/BirdYearReg.csv",header=T)
birdefregyear$Duration<-rep(51,times=nrow(birdefregyear))
birdefregyear$Species<-c("Willow warbler","Brambling","Fieldfare","Tree pipit","Bluethroat","Reed bunting","Redwing","Redpoll")
birdefregyear<-birdefregyear[!birdefregyear$Species%in%c("Fieldfare","Brambling","Redpoll"),]
birdefregyear$Kingdom<-rep("Animalia",times=nrow(birdefregyear))
birdefregyear$EcoEffect<-rep("Abundance",times=nrow(birdefregyear))


birdefregtemp<-read.csv("Data/Birds/BirdTempReg.csv",header=T)
birdefregtemp$Duration<-rep(51,times=nrow(birdefregtemp))
birdefregtemp$Species<-c("Willow warbler","Brambling","Fieldfare","Tree pipit","Bluethroat","Reed bunting","Redwing","Redpoll")
birdefregtemp<-birdefregtemp[!birdefregtemp$Species%in%c("Fieldfare","Brambling","Redpoll"),]
birdefregtemp$Kingdom<-rep("Animalia",times=nrow(birdefregtemp))
birdefregtemp$EcoEffect<-rep("Abundance",times=nrow(birdefregtemp))

#Marine inverts

#marineq10regyear<-read.csv("Data/Marine invertebrates/marineeq10regryear")
marineq90regyear<-read.csv("Data/Marine invertebrates/marineeq90regyear")
marineq90regyear$Kingdom<-rep("Animalia",times=nrow(marineq90regyear))
marineq90regyear$EcoEffect<-rep("Distribution",times=nrow(marineq90regyear))

marineq90regtemp<-read.csv("Data/Marine invertebrates/marineeq90regtemp")
marineq90regtemp$Kingdom<-rep("Animalia",times=nrow(marineq90regtemp))
marineq90regtemp$EcoEffect<-rep("Distribution",times=nrow(marineq90regtemp))


#SODs
indsppleadingreg
animalleadingreg<-indsppleadingreg[indsppleadingreg$Kingdom=="Animalia",]
plantleadingreg<-indsppleadingreg[indsppleadingreg$Kingdom=="Plantae",]
fungileadingreg<-indsppleadingreg[indsppleadingreg$Kingdom=="Fungi",]

#Jonsvannet
jonsinvMaxAbRegdf<-read.csv('Data/Inverts/jonsinvMaxAbRegdf')
jonsinvMaxAbTempRegdf<-read.csv('Data/Inverts/jonsinvMaxAbTempRegdf')

jonsinvMaxAbRegdf$Kingdom<-rep("Animalia",times=nrow(jonsinvMaxAbRegdf))
jonsinvMaxAbRegdf$EcoEffect<-rep("Abundance",times=nrow(jonsinvMaxAbRegdf))
jonsinvMaxAbTempRegdf$Kingdom<-rep("Animalia",times=nrow(jonsinvMaxAbTempRegdf))
jonsinvMaxAbTempRegdf$EcoEffect<-rep("Abundance",times=nrow(jonsinvMaxAbTempRegdf))


#Atna inverts
atnarichRegdf<-read.csv("Data/Inverts/AtnaRegYear.csv")
atnarichRegdf$Kingdom<-rep("Animalia",times=nrow(atnarichRegdf))
atnarichRegdf$EcoEffect<-rep("Diversity",times=nrow(atnarichRegdf))
atnarichTempRegdf<-read.csv("Data/Inverts/AtnaRegTemp.csv")
atnarichTempRegdf$Kingdom<-rep("Animalia",times=nrow(atnarichTempRegdf))
atnarichTempRegdf$EcoEffect<-rep("Diversity",times=nrow(atnarichTempRegdf))


#Unweighted fixed effects models for each

#Year

birdyearRMA<-rma(escalc(yi=birdefregyear$b,sei=birdefregyear$se,measure="GEN"),method="FE",weighted=F)
plantyearRMA<-rma(escalc(yi=plantefregyear$b,sei=plantefregyear$se,measure="GEN"),method="FE",weighted=F)
marinedistyearRMA<-rma(escalc(yi=marineq90regyear$b,sei=marineq90regyear$se,measure="GEN"),method="FE",weighted=F)
animaldistyearRMA<-rma(escalc(yi=animalleadingreg$regslope_year_leading,sei=animalleadingreg$se_year_leading,measure="GEN"),method="FE",weighted=F)
plantdistyearRMA<-rma(escalc(yi=plantleadingreg$regslope_year_leading,sei=plantleadingreg$regslope_year_leading,measure="GEN"),method="FE",weighted=F)
fungidistyearRMA<-rma(escalc(yi=fungileadingreg$regslope_year_leading,sei=fungileadingreg$se_year_leading,measure="GEN"),method="FE",weighted=F)
jonsinvyearRMA<-rma(escalc(yi=jonsinvMaxAbRegdf$b,sei=jonsinvMaxAbRegdf$se,measure="GEN"),method="FE",weighted=F)
atnayearRMA<-rma(escalc(yi=atnarichRegdf$b,sei=atnarichRegdf$se,measure="GEN"),method="FE",weighted =F)

forest(birdyearRMA,slab=c("Willow warbler","Tree pipet","Bluethroat","Reed bunting","Redwing"),xlab="Regression slope",main="Breeding bird abundance: year",mlab="Model estimate",top=1)
forest(plantyearRMA,slab=plantefregyear$Species,main="Plant phenology: year",xlab="Regression slope",mlab="Model estimate",top=1)
forest(marinedistyearRMA,slab=marineq90regyear$Species,main="Marine inverterate distibution: year",xlab="Regression slope",mlab="Model estimate",top=1)
forest(plantdistyearRMA,slab=plantleadingreg$Species,main="Plant distributions: year",xlab="Regression slope",mlab="Model estimate",top=1)
forest(animaldistyearRMA,slab=animalleadingreg$Species,main="Terrestrial animal distributions: year",xlab="Regression slope",mlab="Model estimate",top=1)
forest(fungidistyearRMA,slab=fungileadingreg$Species,main="Fungi distributions: year",xlab="Regression slope",mlab="Model estimate",top=1)
forest(jonsinvyearRMA,slab=jonsinvMaxAbRegdf$Species,main="Limnic zooplankton abundnace: year",xlab="Regression slope",mlab="Model estimate",top=1)
forest(atnayearRMA,slab=atnarichRegdf$Elevation,main="Mayfly and stonefly species richness: year",xlab="Regression slope",mlab="Model estimate",top=1)


#Bind up 
yearlist<-list(birdyearRMA,plantyearRMA,marinedistyearRMA,animaldistyearRMA,plantdistyearRMA,fungidistyearRMA,jonsinvyearRMA,atnayearRMA)
yearrmadf<-data.frame(dataset=c("Breeding bird abundance","Plant phenology","Marine invertebrates","Animalia","Plantae","Fungi","Benthic invertebrate abundance","Mayfly and stone fly diversity"),
  est=rep(NA,times=length(yearlist)),
                      se=rep(NA,times=length(yearlist)),
                      ci.lb=rep(NA,times=length(yearlist)),
                      ci.ub=rep(NA,times=length(yearlist)))
for(i in 1:length(yearlist)){
  yearrmadf$est[i]<-yearlist[[i]]$beta
  yearrmadf$se[i]<-yearlist[[i]]$se
  yearrmadf$ci.lb[i]<-yearlist[[i]]$ci.lb
  yearrmadf$ci.ub[i]<-yearlist[[i]]$ci.ub
}
yearrmadf
round(yearrmadf[,2:5],3)

#temperature

birdtempRMA<-rma(escalc(yi=birdefregtemp$b,sei=birdefregtemp$se,measure="GEN"),method="FE",weighted=F)
planttempRMA<-rma(escalc(yi=plantefregtemp$b,sei=plantefregtemp$se,measure="GEN"),method="FE",weighted=F)
#No effect size for marine invert temperatures
#marinedisttempRMA<-rma(escalc(yi=marineq90regtemp$b,sei=marineq90regtemp$se,measure="GEN"),method="FE",weighted=F)
animaldisttempRMA<-rma(escalc(yi=animalleadingreg$regslope_temp_leading,sei=animalleadingreg$se_temp_leading,measure="GEN"),method="FE",weighted=F)
plantdisttempRMA<-rma(escalc(yi=plantleadingreg$regslope_temp_leading,sei=plantleadingreg$regslope_temp_leading,measure="GEN"),method="FE",weighted=F)
fungidisttempRMA<-rma(escalc(yi=fungileadingreg$regslope_temp_leading,sei=fungileadingreg$se_temp_leading,measure="GEN"),method="FE",weighted=F)
jonsinvtempRMA<-rma(escalc(yi=jonsinvMaxAbTempRegdf$b,sei=jonsinvMaxAbTempRegdf$se,measure="GEN"),method="FE",weighted=F)
atnatempRMA<-rma(escalc(yi=atnarichTempRegdf$b,sei=atnarichTempRegdf$se,measure="GEN"),method="FE",weighted =F)

forest(birdtempRMA,slab=c("Willow warbler","Tree pipet","Bluethroat","Reed bunting","Redwing"),xlab="Regression slope",main="Breeding bird abundance: temperature",mlab="Model estimate",top=1)
forest(planttempRMA,slab=plantefregtemp$Species,main="Plant phenology: temperature",xlab="Regression slope",mlab="Model estimate",top=1)
#forest(marinedisttempRMA,slab=marineq90regtemp$Species,main="Marine inverterate distibution: temperature",xlab="Regression slope",mlab="Model estimate",top=1)
forest(plantdisttempRMA,slab=plantleadingreg$Species,main="Plant distributions: temperature",xlab="Regression slope",mlab="Model estimate",top=1)
forest(animaldisttempRMA,slab=animalleadingreg$Species,main="Terrestrial animal distributions: temperature",xlab="Regression slope",mlab="Model estimate",top=1)
forest(fungidisttempRMA,slab=fungileadingreg$Species,main="Fungi distributions: temperature",xlab="Regression slope",mlab="Model estimate",top=1)
forest(jonsinvtempRMA,slab=jonsinvMaxAbTempRegdf$Species,main="Limnic zooplankton abundnace: temperature",xlab="Regression slope",mlab="Model estimate",top=1)
forest(atnatempRMA,slab=atnarichTempRegdf$Elevation,main="Mayfly and stonefly species richness: temperature",xlab="Regression slope",mlab="Model estimate",top=1)


#Bind up 
templist<-list(birdtempRMA,planttempRMA,animaldisttempRMA,plantdisttempRMA,fungidisttempRMA,jonsinvtempRMA,atnatempRMA)
temprmadf<-data.frame(dataset=c("Breeding bird abundance","Plant phenology","Animalia","Plantae","Fungi","Benthic invertebrate abundance","Mayfly and stone fly diversity"),
                      est=rep(NA,times=length(templist)),
                      se=rep(NA,times=length(templist)),
                      ci.lb=rep(NA,times=length(templist)),
                      ci.ub=rep(NA,times=length(templist)))
for(i in 1:length(templist)){
  temprmadf$est[i]<-templist[[i]]$beta
  temprmadf$se[i]<-templist[[i]]$se
  temprmadf$ci.lb[i]<-templist[[i]]$ci.lb
  temprmadf$ci.ub[i]<-templist[[i]]$ci.ub
}
temprmadf
round(temprmadf[,2:5],3)





tiff("Figures/BirdForests.tif",width=10,height=6,units="in",res=150)
par(mfrow=c(1,2))
forest(birdyearRMA,slab=c("Willow warbler","Tree pipet","Bluethroat","Reed bunting","Redwing"),xlab="Change in terrortories per year",main="Breeding bird abundance: year",mlab="Model estimate",top=1)
forest(birdtempRMA,slab=c("Willow warbler","Tree pipet","Bluethroat","Reed bunting","Redwing"),xlab=expression("Change in terrortories per "^degree*C),main="Breeding bird abundance: temperature",mlab="Model estimate",top=1)
dev.off()

tiff("Figures/PlantPhenForests.tif",width=10,height=6,units="in",res=150)
par(mfrow=c(1,2))
forest(plantyearRMA,slab=plantefregyear$Species,main="Plant phenology: year",xlab="Change in earliest date of peak flowering per year",mlab="Model estimate",top=1)
forest(planttempRMA,slab=plantefregtemp$Species,main="Plant phenology: temperature",xlab=expression("Change in earliest date of peak flowering per "^degree*C),mlab="Model estimate",top=1)
dev.off()

tiff("Figures/BenthicInverts.tif",width=10,height=6,units="in",res=150)
par(mfrow=c(1,2))
forest(jonsinvyearRMA,slab=jonsinvMaxAbRegdf$Species,main="Limnic zooplankton abundnace: year",xlab="Change in invertebrate abundance per year",mlab="Model estimate",top=1)
forest(jonsinvtempRMA,slab=jonsinvMaxAbTempRegdf$Species,main="Limnic zooplankton abundnace: temperature",xlab=expression("Change in invertebrate abundance per "^degree*C),mlab="Model estimate",top=1)
dev.off()

tiff("Figures/MayfliesStoneFlies_SR.tif",width=10,height=6,units="in",res=150)
par(mfrow=c(1,2))
forest(atnayearRMA,slab=atnarichRegdf$Elevation,main="Mayfly and stonefly species richness: year",xlab="Change in species richness per year",mlab="Model estimate",top=1)
forest(atnatempRMA,slab=atnarichTempRegdf$Elevation,main="Mayfly and stonefly species richness: temperature",xlab=expression("Change in species richness per "^degree*C),mlab="Model estimate",top=1)
dev.off()


tiff("Figures/Distributions.tif",width=10,height=6,units="in",res=150)
par(mfrow=c(1,2))
with(yearrmadf[3:6,],forest(rma(yi=est,sei=se,measure="GEN",weighted=F,method="FE"),slab=dataset,main="Species distributions: year",xlab=expression(paste("Change in latitude"^degree, "per year")),mlab="Model estimate",top=1))
with(temprmadf[3:5,],forest(rma(yi=est,sei=se,measure="GEN",weighted=F,method="FE"),slab=dataset,main="Species distributions: temperature",xlab=expression(paste("Change in latitude"^degree, "per "^degree*C)),mlab="Model estimate",top=1,ylim = c(-1.5,5.0)))
dev.off()


distrmatemp<-with(temprmadf[3:5,],(rma(yi=est,sei=se,measure="GEN",weighted=F,method="FE")))
distrmayear<-with(yearrmadf[3:6,],(rma(yi=est,sei=se,measure="GEN",weighted=F,method="FE")))



#Trying to make a single big plot
#5 x 2
plantyearRMA$k.all#22
birdyearRMA$k.all#5
jonsinvyearRMA$k.all#16
atnayearRMA$k.all#4
distrmayear$k.all#4

rowsps<-c(4/51,8/51,24/51,29/51)

rowsps<-c(6/51,12/51,26/51,32/51)

#Colours to match ggplots
cols<-scales::hue_pal()(4)


tiff("Figures/BigForestTest.tif",height=20,width=8,units="in",res=150)
{
par(fig=c(0,0.6,0,rowsps[1]))
par(mar=c(4,1,2,1))
with(yearrmadf[3:6,],forest(rma(yi=est,sei=se,measure="GEN",weighted=F,method="FE"),efac=c(2,2,3),slab=dataset,main="",
                              xlab=expression(paste("Change in latitude (", degree,"N year"^-1,")")),mlab="Overall",top=1,psize=0.1,col=cols[2],digits=3,xlim=c(-0.025,0.035)))
points(yearrmadf$est[3:6],4:1,pch=c(16,16,15,17),cex=1.5,col=cols[2])
mtext("Latitudinal distributions",side=3,adj=0,line=1.5,font=2)
par(fig=c(0.6,1,0,rowsps[1]),new=T)
with(temprmadf[3:5,],forest(rma(yi=est,sei=se,measure="GEN",weighted=F,method="FE"),xlim=c(-0.2,0.6),efac=c(2,2,3),slab=NA,main="",xlab=expression(paste("Change in latitude (", degree,"N ",degree,"C"^-1,")")),mlab="",top=1,ylim = c(-1.5,5.0),col=cols[2],psize = 0.1))
points(temprmadf$est[3:5],3:1,pch=c(16,15,17),cex=1.5,col=cols[2])

par(fig=c(0,0.6,rowsps[1],rowsps[2]),new=T)
forest(atnayearRMA,efac=c(2,2,3),slab=atnarichRegdf$Elevation,main="",xlab=expression(paste("Change in richness (species year"^-1,")")),mlab="Overall",top=1,col=cols[3],psize=0.1)
points(atnarichRegdf$b,4:1,col=cols[3],pch=16,cex=1.5)
mtext("Mayfly and stonefly diversity",side=3,adj=0,line=1.5,font=2)
par(fig=c(0.6,1,rowsps[1],rowsps[2]),new=T)
forest(atnatempRMA,efac=c(2,2,3),slab=NA,xlim=c(-1,4),main="",xlab=expression(paste("Change in richness (species ",degree,"C"^-1,")")),mlab="",top=1,col=cols[3],psize=0.1)
points(atnarichTempRegdf$b,4:1,col=cols[3],pch=16,cex=1.5)


par(fig=c(0,0.6,rowsps[2],rowsps[3]),new=T)
forest(jonsinvyearRMA,efac=c(2,2,2),slab=jonsinvMaxAbRegdf$Species,digits=0,main="",xlab=expression(paste("Change in abundance (inds. m"^-3," year"^-1,")")),mlab="Overall",top=1,col=cols[1],psize=0.1)
points(jonsinvMaxAbRegdf$b,16:1,col=cols[1],pch=16,cex=1.5)
mtext("Limnic zooplankton abundance",side=3,adj=0,line=1.1,font=2)
par(fig=c(0.6,1,rowsps[2],rowsps[3]),new=T)
forest(jonsinvtempRMA,efac=c(2,2,2),slab=NA,xlim=c(-8000,300000),digits=0,main="",xlab=expression(paste("Change in abundance (inds. m"^-3,~degree,"C"^-1,")")),mlab="",top=1,col=cols[1],psize=0.1)
points(jonsinvMaxAbTempRegdf$b,16:1,col=cols[1],pch=16,cex=1.5)


par(fig=c(0,0.6,rowsps[3],rowsps[4]),new=T)
#forest(birdyearRMA,slab=c("Willow warbler","Tree pipet","Bluethroat","Reed bunting","Redwing"),xlab="Change in terrortories per year",main="",mlab="Breeding bird abundance",top=1)
forest(birdyearRMA,xlim=c(-3,1.8),efac=c(3,3,4),slab=c("Phylloscopus trochilus","Anthus trivialis","Luscinia svecica","Emberiza schoeniclus","Turdus iliacus"),xlab=expression(paste("Change in terrortories (km"^-2," year"^-1,")")),main="",mlab="Overall",top=1,col=cols[1],psize=0.1)
points(birdefregyear$b,5:1,col=cols[1],pch=16,cex=1.5)
mtext("Breeding bird abundance",side=3,adj=0,line=1.5,font=2)
par(fig=c(0.6,1,rowsps[3],rowsps[4]),new=T)
forest(birdtempRMA,efac=c(3,3,4),xlim=c(-10,20),slab=NA,xlab=expression(paste("Change in terrortories  (km"^-2,~degree, "C"^-1,")")),main="",mlab="",top=1,col=cols[1],psize=0.1)
points(birdefregtemp$b,5:1,col=cols[1],pch=16,cex=1.5)

par(fig=c(0,0.6,rowsps[4],1),new=T)
forest(plantyearRMA,efac=c(2,2,2),slab=plantefregyear$Species,main="",xlab=expression(paste("Change in peak flowering (days year"^-1,")")),mlab="Overall",top=1,psize=0.1,col=cols[4])
points(plantefregyear$b,22:1,col=cols[4],pch=15,cex=1.5)
mtext("Plant flowering phenology",side=3,adj=0,font=2)
par(fig=c(0.6,1,rowsps[4],1),new=T)
forest(planttempRMA,xlim=c(-20,40),efac=c(2,2,2),slab=NA,main="",xlab=expression(paste("Change in peak flowering (days ",degree,"C"^-1,")")),mlab="",top=1,col=cols[4],psize=0.1)
points(plantefregtemp$b,22:1,col=cols[4],pch=15,cex=1.5)
dev.off()
}
#efac for big polygon
#Points -> colour to match fig4

# Breakpoints -------------------------------------------------------------
bpsDist<-read.csv("Data/SOD/breakpoints_SOD.csv",header=T,sep=";")
bpsPlantphen<-read.csv("Data/Plants/PlantYearBreakpoints.csv")

#Histogram only >80 yrs
h1<-hist(c(bpsDist$bp1_lead_year[bpsDist$Duration>80],bpsDist$bp2_lead_year[bpsDist$Duration>80]),breaks=(1900:2020),ylim=c(0,6),xlab="Year",main="")
hist(bpsPlantphen$Bp1,add=T,col=3,breaks=h1$breaks)
abline(v=c(1946,1979),lwd=2,lty=2)


#Histogram all
h1<-hist(c(bpsDist$bp1_lead_year,bpsDist$bp2_lead_year),breaks=(1900:2020),ylim=c(0,15),xlab="Year",main="")
hist(bpsPlantphen$Bp1,add=T,col=3,breaks=h1$breaks)
abline(v=c(1946,1979),lwd=2,lty=2)

#Stacked barplot
Yeardf<-data.frame(Year=1900:2020)
distbps80<-c(bpsDist$bp1_lead_year[bpsDist$Duration>=80],bpsDist$bp2_lead_year[bpsDist$Duration>80])
distbps80peryear<-data.frame(tapply(distbps80,round(distbps80),length))
distbps80peryear$Year<-rownames(distbps80peryear)
names(distbps80peryear)[1]<-"BPs"
ppbps<-bpsPlantphen$Bp1
ppbpsperyear<-data.frame(tapply(ppbps,round(ppbps),length))
ppbpsperyear$Year<-rownames(ppbpsperyear)
names(ppbpsperyear)[1]<-"BPs"
m1<-rbind(distbps80peryear,ppbpsperyear)
m1$Type<-c(rep("Distribution",nrow(distbps80peryear)),rep("Phenology",times=nrow(ppbpsperyear)))

m2<-merge(Yeardf,m1,all.x=T,by="Year")
sum(m2$BPs,na.rm=T)#Number of breakpoints
#mat1<-tapply(m1$BPs,list(m1$Type,m1$Year),sum)
       
#cbp<-c(1946,1979)
#b2<-barplot(cbp)
#b_bp<-barplot(mat1,beside=F) 
#abline(v=b2)

tiff("Figures/Breakpoints.tif",width=8,height=6,units="in",res=150)
ggplot(data=m2,aes(x=Year,y=BPs,fill=Type))+geom_bar(stat="sum",position="stack",show.legend =  c("x"=T,"y"=T,size=F))+
  geom_vline(xintercept = c(1946.5,1979.5), color = "black",size=1.5,linetype="longdash")+
  ylab("Frequency")+
  theme_bw()+
  scale_fill_manual("legend", values = c("Phenology" = "darkgreen", "Distribution" = "orange"))
  #scale_fill_manual("legend", values = c("PP" = "gray85", "Dist" = "gray35"))
dev.off()


#Test fo normality and multimodality
library(diptest)#Dip test
library(truncnorm)#Truncated normal distributions for simulating bimodal distribution
allbks<-c(distbps80,ppbps)
shapiro.test(allbks)#Non normal
dip.test(allbks)#Multimodal

#Uniform distribution
randunif1<-runif(length(!is.na(allbks)),min=1900,max=2020)
#Normal (truncated at 1900 and 2020)
randnorm1<-rtruncnorm(length(!is.na(allbks)),mean(allbks,na.rm=T),sd(allbks,na.rm=T),a=1900,b=2020)
ks.test(allbks,randunif1)#Not uniform


#Bimodal #Truncated. #SD/4 
bimodal <- c(rtruncnorm(round(length(!is.na(allbks))/2), mean=1946, sd=sd(allbks,na.rm=T)/3,a=1900,b=2020),
          rtruncnorm(round(length(!is.na(allbks))/2), mean=1979, sd=sd(allbks,na.rm=T)/3),a=1900,b=2020)
hist(bimodal)

ks.test(allbks,bimodal)

tiff("Figures/ECDFs.tif",width=8,height=6,units="in",res=150)
plot(ecdf(allbks),xlab="Year",main="Empirical cumulative distribution function")#, xlim = c(1900:2020))
plot(ecdf(randunif1), add = TRUE, lty = "dashed",col=grey(0.8))
plot(ecdf(randnorm1), add = TRUE, lty = "dashed",col=grey(0.6))
plot(ecdf(bimodal),add=TRUE,col=grey(0.4))
legend("topl",lwd=2,col=c(1,grey(0.8),grey(0.6),grey(0.4)),c("Distribution of breakpoints","Uniform distribution","Normal distribution","Bimodal distribution"))
abline(v=c(1946,1979))
dev.off()

summary(allbks)
