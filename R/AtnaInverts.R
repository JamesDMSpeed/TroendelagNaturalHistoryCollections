#Atna invertebrates

atnadat<-read.csv("Data/Inverts/Richness_Ephemeroptera.csv",header=T,sep=";")

#Climate data
climdata<-read.csv("Data/ClimateData_forAnalyses.csv",header=T)

#Max species richness per station
stationdf<-data.frame(with(atnadat,tapply(Species.Richness,list(Year,Elevation..m.),max,na.rm=T)))
stationdf$Year<-as.numeric(rownames(stationdf))

with(stationdf,plot(Year,X380,type='b',ylim=c(0,10),ylab="Species richness",las=1))
with(stationdf,lines(Year,X710,type='b',col='red'))
with(stationdf,lines(Year,X1060,type='b',col='blue'))
legend('topl',lty=1,col=c(1,'red','blue'),c('380m','710m','1060m'),ncol=3)

with(stationdf,cor.test(Year,X380))
with(stationdf,cor.test(Year,X710))
with(stationdf,cor.test(Year,X1060))


#Merge with climate data
stationdfclim<-merge(stationdf,climdata,all.x=T,all.y=F,by="Year")

#Dataframe with temporal correlations
atnacorYeardf<-data.frame(Elevation=c('X380','X710','X1060'),
                                      r=c(with(stationdf,cor(Year,X380)),
                                          with(stationdf,cor(Year,X710)),
                                          with(stationdf,cor(Year,X1060,use='pairwise'))),
                                      n=c(length(stationdf$X380[!is.na(stationdf$X380)]),
                                      length(stationdf$X710[!is.na(stationdf$X710)]),
                                             length(stationdf$X1060[!is.na(stationdf$X1060)])))
atnacorYeardf

#Dataframe with temporal regression correlations
atnaregYeardf<-data.frame(Elevation=c('X380','X710','X1060'),
                          b=c(with(stationdf,summary(lm(X380~Year))$coefficients[2,1]),
                              with(stationdf,summary(lm(X710~Year))$coefficients[2,1]),
                              with(stationdf,summary(lm(X1060~Year))$coefficients[2,1])),
                          se=c(with(stationdf,summary(lm(X380~Year))$coefficients[2,2]),
                              with(stationdf,summary(lm(X710~Year))$coefficients[2,2]),
                              with(stationdf,summary(lm(X1060~Year))$coefficients[2,2])))
atnaregYeardf



#Dataframe with temperature correlactions
atnacorTempdf<-data.frame(Elevation=c('X380','X710','X1060'),
                          r=c(with(stationdfclim,cor(Annual,X380)),
                              with(stationdfclim,cor(Annual,X710)),
                              with(stationdfclim,cor(Annual,X1060,use='pairwise'))),
                          n=c(length(stationdfclim$X380[!is.na(stationdfclim$X380)]),
                              length(stationdfclim$X710[!is.na(stationdfclim$X710)]),
                              length(stationdfclim$X1060[!is.na(stationdfclim$X1060)])))
atnacorTempdf

#Dataframe with temperature regression correlations
atnaregAnnualdf<-data.frame(Elevation=c('X380','X710','X1060'),
                          b=c(with(stationdfclim,summary(lm(X380~Annual))$coefficients[2,1]),
                              with(stationdfclim,summary(lm(X710~Annual))$coefficients[2,1]),
                              with(stationdfclim,summary(lm(X1060~Annual))$coefficients[2,1])),
                          se=c(with(stationdfclim,summary(lm(X380~Annual))$coefficients[2,2]),
                               with(stationdfclim,summary(lm(X710~Annual))$coefficients[2,2]),
                               with(stationdfclim,summary(lm(X1060~Annual))$coefficients[2,2])))
atnaregAnnualdf


#Meta analyses - correlations
par(mfrow=c(2,1))
esatnacoryr<-escalc(measure="COR",ri=atnacorYeardf$r,ni=atnacorYeardf$n)
rmaatnacoryr<-rma(esatnacoryr)
forest(rmaatnacoryr,slab=atnacorYeardf$Elevation,main="Atna mayflies - Year")

esatnacortemp<-escalc(measure="COR",ri=atnacorTempdf$r,ni=atnacorTempdf$n)
rmaatnacortemp<-rma(esatnacortemp)
forest(rmaatnacortemp,slab=atnacorYeardf$Elevation,main="Atna mayflies - Annual temperature")

#Meta-analysis - regressions

rmaatnaregyr<-rma(yi=atnaregYeardf$b,sei=atnaregYeardf$se,measure="GEN")
forest(rmaatnaregyr,slab=atnaregYeardf$Elevation,main="Atna mayflies - Year",xlab="Regression slopes")

rmaatnaregtemp<-rma(yi=atnaregAnnualdf$b,sei=atnaregAnnualdf$se,measure="GEN")
forest(rmaatnaregtemp,slab=atnaregYeardf$Elevation,main="Atna mayflies - Annual temperature",xlab="Regression slopes")

write.csv(atnacorYeardf,"Data/Inverts/AtnaCorYear.csv")
write.csv(atnaregYeardf,"Data/Inverts/AtnaRegYear.csv")
write.csv(atnacorTempdf,"Data/Inverts/AtnaCorTemp.csv")
write.csv(atnaregAnnualdf,"Data/Inverts/AtnaRegTemp.csv")
