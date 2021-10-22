#Jonsvannet inverts

library(readxl)
library(metafor)

#Read in data
jonsinv<-read_xls("Data/Inverts/All_Sites_Final.xls",sheet="All_sites")
#Climate data
climdata<-read.csv("Data/ClimateData_forAnalyses.csv",header=T)


#Get dates, year, day of year
jonsinv$eventDate<-as.Date(jonsinv$eventDate,format="%d.%m.%Y")
jonsinv$Year<-as.numeric(format(jonsinv$eventDate,"%Y"))
jonsinv$DayofYear<-as.numeric(format(jonsinv$eventDate,"%j"))

#Set spp as a factor
jonsinv$verbatimScientificName<-as.factor(jonsinv$verbatimScientificName)

#Individuals per sample
jonsinv$countpersample<-jonsinv$individualCount/jonsinv$`x (# of samples)`

#Merge with climate
jonsinvclim<-merge(jonsinv,climdata,by.x="Year",by.y = "Year",all.x = T,all.y = F)



#For each species and year, maximum count
tapply(jonsinvclim$countpersample,list(jonsinvclim$Year,jonsinvclim$verbatimScientificName),max)


#Looping through some correlations
jonsinvCordf<-data.frame(Species=levels(as.factor(jonsinvclim$verbatimScientificName)),
                           R=rep(NA,times=length(levels(as.factor(jonsinvclim$verbatimScientificName)))),
                           N=rep(NA,times=length(levels(as.factor(jonsinvclim$verbatimScientificName)))),
                           Duration=rep(NA,times=length(levels(as.factor(jonsinvclim$verbatimScientificName)))))
jonsinvRegdf<-data.frame(Species=levels(as.factor(jonsinvclim$verbatimScientificName)),
                           b=rep(NA,times=length(levels(as.factor(jonsinvclim$verbatimScientificName)))),
                           se=rep(NA,times=length(levels(as.factor(jonsinvclim$verbatimScientificName)))))


jonsinvTempCordf<-data.frame(Species=levels(as.factor(jonsinvclim$verbatimScientificName)),
                               R=rep(NA,times=length(levels(as.factor(jonsinvclim$verbatimScientificName)))),
                               N=rep(NA,times=length(levels(as.factor(jonsinvclim$verbatimScientificName)))))
jonsinvTempRegdf<-data.frame(Species=levels(as.factor(jonsinvclim$verbatimScientificName)),
                               b=rep(NA,times=length(levels(as.factor(jonsinvclim$verbatimScientificName)))),
                               se=rep(NA,times=length(levels(as.factor(jonsinvclim$verbatimScientificName)))))

jonsinvMaxAbCordf<-jonsinvCordf
jonsinvMaxAbRegdf<-jonsinvRegdf
jonsinvMaxAbTempCordf<-jonsinvTempCordf
jonsinvMaxAbTempRegdf<-jonsinvTempRegdf

jonsvanspeciesyrdf<-list()

for(k in 1:length(levels(jonsinvclim$verbatimScientificName))){
  dfInv<-jonsinvclim[jonsinvclim$verbatimScientificName==levels(jonsinvclim$verbatimScientificName)[k],]
  dfInvY<-data.frame(Year=as.numeric(as.character(levels(as.factor(dfInv$Year)))),MaxAb=rep(NA,times=length(levels(as.factor(dfInv$Year)))),DoY=rep(NA,times=length(levels(as.factor(dfInv$Year)))))
  
  for(i in 1:length(levels(as.factor(dfInv$Year)))){
    dfInvYi<-dfInv[dfInv$Year==levels(as.factor(dfInv$Year))[i],]
    dfInvY$MaxAb[i]<-max(dfInvYi$countpersample)
    dfInvY$DoY[i]<-dfInvYi$DayofYear[which.max(dfInvYi$countpersample)]
  }
 
 dfInvYClim<-merge(dfInvY,climdata,by='Year',all.x=T,all.y=F)
  plot(dfInvY$Year,dfInvY$DoY,main=levels(as.factor(jonsinvclim$verbatimScientificName))[k],xlab="Year",ylab="Day of Year",las=1)
  plot(dfInvYClim$Annual,dfInvYClim$DoY,main=levels(as.factor(jonsinvclim$verbatimScientificName))[k],xlab="Annual temperature",ylab="Day of Year",las=1)
  jonsvanspeciesyrdf[[k]]<-dfInvYclim
  
  print(cor.test(dfInvY$Year,dfInvY$DoY))
  jonsinvCordf$R[jonsinvCordf$Species==levels(jonsinvclim$verbatimScientificName)[k]]<-cor(dfInvY$Year,dfInvY$DoY,use='pairwise')
  jonsinvCordf$N[jonsinvCordf$Species==levels(jonsinvclim$verbatimScientificName)[k]]<-length(na.omit(dfInvY$DoY))
  jonsinvCordf$Duration[jonsinvCordf$Species==levels(jonsinvclim$verbatimScientificName)[k]]<-max(dfInvY$Year)-min(dfInvY$Year)
  jonsinvTempCordf$R[jonsinvTempCordf$Species==levels(jonsinvclim$verbatimScientificName)[k]]<-cor(dfInvYClim$Annual,dfInvYClim$DoY,use='pairwise')
  jonsinvTempCordf$N[jonsinvTempCordf$Species==levels(jonsinvclim$verbatimScientificName)[k]]<-length(na.omit(dfInvYClim$DoY))
  jonsinvRegdf$b[jonsinvCordf$Species==levels(jonsinvclim$verbatimScientificName)[k]]<-summary(lm(dfInvY$DoY~dfInvY$Year))$coefficients[2,1]
  jonsinvRegdf$se[jonsinvCordf$Species==levels(jonsinvclim$verbatimScientificName)[k]]<-summary(lm(dfInvY$DoY~dfInvY$Year))$coefficients[2,2]
  jonsinvTempRegdf$b[jonsinvRegdf$Species==levels(jonsinvclim$verbatimScientificName)[k]]<-summary(lm(dfInvYClim$DoY~dfInvYClim$Annual))$coefficients[2,1]
  jonsinvTempRegdf$se[jonsinvRegdf$Species==levels(jonsinvclim$verbatimScientificName)[k]]<-summary(lm(dfInvYClim$DoY~dfInvYClim$Annual))$coefficients[2,2]

  jonsinvMaxAbCordf$R[jonsinvMaxAbCordf$Species==levels(jonsinvclim$verbatimScientificName)[k]]<-cor(dfInvY$Year,dfInvY$MaxAb,use='pairwise')
  jonsinvMaxAbCordf$N[jonsinvMaxAbCordf$Species==levels(jonsinvclim$verbatimScientificName)[k]]<-length(na.omit(dfInvY$MaxAb))
  jonsinvMaxAbCordf$Duration[jonsinvMaxAbCordf$Species==levels(jonsinvclim$verbatimScientificName)[k]]<-max(dfInvY$Year)-min(dfInvY$Year)
  jonsinvMaxAbTempCordf$R[jonsinvMaxAbTempCordf$Species==levels(jonsinvclim$verbatimScientificName)[k]]<-cor(dfInvYClim$Annual,dfInvYClim$MaxAb,use='pairwise')
  jonsinvMaxAbTempCordf$N[jonsinvMaxAbTempCordf$Species==levels(jonsinvclim$verbatimScientificName)[k]]<-length(na.omit(dfInvYClim$MaxAb))
  jonsinvMaxAbRegdf$b[jonsinvMaxAbCordf$Species==levels(jonsinvclim$verbatimScientificName)[k]]<-summary(lm(dfInvY$MaxAb~dfInvY$Year))$coefficients[2,1]
  jonsinvMaxAbRegdf$se[jonsinvMaxAbCordf$Species==levels(jonsinvclim$verbatimScientificName)[k]]<-summary(lm(dfInvY$MaxAb~dfInvY$Year))$coefficients[2,2]
  jonsinvMaxAbTempRegdf$b[jonsinvMaxAbRegdf$Species==levels(jonsinvclim$verbatimScientificName)[k]]<-summary(lm(dfInvYClim$MaxAb~dfInvYClim$Annual))$coefficients[2,1]
  jonsinvMaxAbTempRegdf$se[jonsinvMaxAbRegdf$Species==levels(jonsinvclim$verbatimScientificName)[k]]<-summary(lm(dfInvYClim$MaxAb~dfInvYClim$Annual))$coefficients[2,2]
  
  
  }

jonsinvCordf
jonsinvRegdf
jonsinvTempRegdf
jonsinvMaxAbCordf


write.csv(jonsinvMaxAbCordf,"Data/Inverts/jonsinvMaxAbCordf")
write.csv(jonsinvCordf,"Data/Inverts/jonsinvCordf")
write.csv(jonsinvRegdf,"Data/Inverts/jonsinvRegdf")
write.csv(jonsinvTempRegdf,"Data/Inverts/jonsinvTempRegdf")
write.csv(jonsinvMaxAbRegdf,"Data/Inverts/jonsinvMaxAbRegdf")
write.csv(jonsinvMaxAbTempRegdf,"Data/Inverts/jonsinvMaxAbTempRegdf")


par(mfrow=c(2,2))
jonsinves<-escalc(yi=jonsinvRegdf$b,sei=jonsinvRegdf$se,measure="GEN")
jonsinvrma<-rma(jonsinves,method="FE",weighted=F)
forest(jonsinvrma,main="Jonsvannet invertebrate phenology \n Day of year of maximum abundnace",xlab="Regression slope on year",slab=jonsinvRegdf$Species)

jonsinvMaxAbes<-escalc(yi=jonsinvMaxAbRegdf$b,sei=jonsinvMaxAbRegdf$se,measure="GEN")
jonsinvMaxAbrma<-rma(jonsinvMaxAbes,method="FE",weighted=F)
forest(jonsinvMaxAbrma,main="Jonsvannet invertebrate abundance \n Individuals per sample",xlab="Regression slope on year",slab=jonsinvMaxAbRegdf$Species)

jonsinvtempes<-escalc(yi=jonsinvTempRegdf$b,sei=jonsinvTempRegdf$se,measure="GEN")
jonsinvtemprma<-rma(jonsinvtempes,method="FE",weighted=F)
forest(jonsinvtemprma,main="Jonsvannet invertebrate phenology \n Day of year of maximum abundnace",xlab="Regression slope on temperature",slab=jonsinvTempRegdf$Species)

jonsinvMaxAbtempes<-escalc(yi=jonsinvMaxAbTempRegdf$b,sei=jonsinvMaxAbTempRegdf$se,measure="GEN")
jonsinvMaxAbtemprma<-rma(jonsinvMaxAbtempes,method="FE",weighted=F)
forest(jonsinvMaxAbtemprma,main="Jonsvannet invertebrate abundance \n Individuals per sample",xlab="Regression slope on temperature",slab=jonsinvMaxAbTempRegdf$Species)

