#Summary data


rm(list=ls())



# Study duration and effect size ------------------------------------------



#For associations between duration and es
#Read in correlation effect sizes

#Plants
plantefcoryear<-read.csv("Data/Plants/PlantYearCor.csv",header=T)
plantefcoryear<-plantefcoryear[plantefcoryear$N>=5,]#Drop species with <5 years
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


marineq10coryear<-read.csv("Data/Marine invertebrates/marineeq10coryear")
marineq90coryear<-read.csv("Data/Marine invertebrates/marineeq90coryear")

marinecoryear<-rbind(marineq10coryear,marineq90coryear)
marinecoryear$EcoEffect<-c(rep("Distribution_advance",times=9),rep("Distribution-tail",times=9))
marinecoryear$Kingdom<-rep("Animalia",times=9)

#Bind together
allcorefyear<-rbind(plantefcoryear,birdefcoryear,dendrocoryear,marinecoryear)
allcoreftemp<-rbind(plantefcortemp,birdefcortemp,dendrocortemp)


#Plot study duration against R2
par(mfrow=c(2,1))
with(allcorefyear,plot(Duration,(R*R),ylab="R2",main="Temporal effect size",col=as.factor(allcoreftemp$EcoEffect),pch=(allcorefyear$EcoEffect)))
with(allcoreftemp,plot(Duration,(R*R),ylab="R2",main="Temperature effect size",col=as.factor(allcoreftemp$EcoEffect)))#But temperature data only 100yrs
#rect(0,0,100,1,col=1,border=F,density=10)
abline(v=120)


# Main forest plots -------------------------------------------------------


#For main forest plots
#Read in regression effect sizes

#Plants
plantefregyear<-read.csv("Data/Plants/PlantYearReg.csv",header=T)
plantefregyear<-plantefregyear[plantefregyear$Species%in%plantefcoryear$Species,]#Drop species with <5 years

#Birds
birdefregyear<-read.csv("Data/Birds/BirdYearReg.csv",header=T)
birdefregyear$Duration<-rep(51,times=nrow(birdefregyear))
birdefregyear$Species<-c("Willow warbler","Brambling","Fieldfare","Tree pipit","Bluethroat","Reed bunting","Redwing","Redpoll")
birdefregyear<-birdefregyear[!birdefregyear$Species%in%c("Fieldfare","Brambling","Redpoll"),]
names(birdefregyear)[2:4]<-c("Species","R","N")

#Marine inverts

marineq10regyear<-read.csv("Data/Marine invertebrates/marineeq10regryear")
marineq90regyear<-read.csv("Data/Marine invertebrates/marineeq90regyear")





#Unweighted fixed effects models for each
esbirdyear<-escalc(measure="GEN",b=allregefyear$R,se=allregefyear$N)
allregefrma<-rma(esallregyear,measure = "reg",method="FE")
forest(allregefrma,main="All year")



