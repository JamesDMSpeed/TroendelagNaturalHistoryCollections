#Atna invertebrates

#atnadat<-read.csv("Data/Inverts/Richness_Ephemeroptera_Plecoptera_Updated.csv",header=T,sep=";")
atnadat<-read.csv("Data/Inverts/Richness_Ephemeroptera_Plecoptera_Allgenera.csv",header=T,sep=";")
#olddat<-read.csv("Data/Inverts/Richness_Ephemeroptera.csv",header=T,sep=";")
#Climate data
climdata<-read.csv("Data/ClimateData_forAnalyses.csv",header=T)

#Remove last years from 2018
atnadat<-atnadat[atnadat$Year<2018,]

#Code elevation
levels(atnadat$ï..Site)
atnadat$Elevation<-rep(NA,times=nrow(atnadat))
atnadat$Elevation[atnadat$ï..Site=='DÃ¸rÃ¥lseter']<-1060
atnadat$Elevation[atnadat$ï..Site=='Skranglehaugen']<-1120
atnadat$Elevation[atnadat$ï..Site=='Vollen']<-710
atnadat$Elevation[atnadat$ï..Site=='Solbakken']<-380


#Max species richness per station
stationdf<-data.frame(with(atnadat,tapply(Total.Species.Richness,list(Year,Elevation),max,na.rm=T)))
stationdf$Year<-as.numeric(rownames(stationdf))

with(stationdf,plot(Year,X380,type='b',ylim=c(0,15),ylab="Species richness",las=1))
with(stationdf,lines(Year,X710,type='b',col='red'))
with(stationdf,lines(Year,X1060,type='b',col='blue'))
with(stationdf,lines(Year,X1120,type='b',col='green'))
abline(v=2002.5)
legend('topl',lty=1,col=c(1,'red','blue','green'),c('380m','710m','1060m',"1120"),ncol=3)

with(stationdf,cor.test(Year,X380))
with(stationdf,cor.test(Year,X710))
with(stationdf,cor.test(Year,X1060))

stationdf$timeperiod<-stationdf$Year
stationdf$timeperiod[stationdf$Year<=2002]<-'old'
stationdf$timeperiod[stationdf$Year>2002]<-'new'

t.test(stationdf$X380~stationdf$timeperiod)
t.test(stationdf$X710~stationdf$timeperiod)
t.test(stationdf$X1060~stationdf$timeperiod)
t.test(stationdf$X1120~stationdf$timeperiod)

lmST<-with(stationdf,lm(X710~Year+timeperiod))
summary(lmST)


#Merge with climate data
stationdfclim<-merge(stationdf,climdata,all.x=T,all.y=F,by="Year")

lmSTemp<-with(stationdfclim[stationdfclim$timeperiod=='old',],lm(X1060~Annual))
lmSTemp<-with(stationdfclim,lm(X1120~Annual+timeperiod))
summary(lmSTemp)


#Dataframe with temporal correlations
atnacorYeardf<-data.frame(Elevation=c('380m','710m','1060m',"1120m"),
                                      r=c(with(stationdf,cor(Year,X380)),
                                          with(stationdf,cor(Year,X710)),
                                          with(stationdf,cor(Year,X1060,use='pairwise')),
                                          with(stationdf,cor(Year,X1120,use='pairwise'))),
                                      n=c(length(stationdf$X380[!is.na(stationdf$X380)]),
                                      length(stationdf$X710[!is.na(stationdf$X710)]),
                                             length(stationdf$X1060[!is.na(stationdf$X1060)]),
                                      length(stationdf$X1120[!is.na(stationdf$X1120)])),
                                      Duration=c(max(stationdf$Year[!is.na(stationdf$X380)])-min(stationdf$Year[!is.na(stationdf$X380)]),
                                                 max(stationdf$Year[!is.na(stationdf$X710)])-min(stationdf$Year[!is.na(stationdf$X710)]),
                                                 max(stationdf$Year[!is.na(stationdf$X1060)])-min(stationdf$Year[!is.na(stationdf$X1060)]),
                                                 max(stationdf$Year[!is.na(stationdf$X1120)])-min(stationdf$Year[!is.na(stationdf$X1120)])))
atnacorYeardf

#Dataframe with temporal regression correlations
# atnaregYeardf<-data.frame(Elevation=c('380m','710m','1060m',"1120m"),
#                           b=c(with(stationdf,summary(lm(X380~Year))$coefficients[2,1]),
#                               with(stationdf,summary(lm(X710~Year))$coefficients[2,1]),
#                               with(stationdf,summary(lm(X1060~Year))$coefficients[2,1]),
#                               with(stationdf,summary(lm(X1120~Year))$coefficients[2,1])),
#                           se=c(with(stationdf,summary(lm(X380~Year))$coefficients[2,2]),
#                               with(stationdf,summary(lm(X710~Year))$coefficients[2,2]),
#                               with(stationdf,summary(lm(X1060~Year))$coefficients[2,2]),
#                               with(stationdf,summary(lm(X1120~Year))$coefficients[2,2])))

atnaregYeardf<-data.frame(Elevation=c('380m','710m','1060m',"1120m"),
                          b=c(with(stationdf,summary(lm(X380~Year+timeperiod))$coefficients[2,1]),
                              with(stationdf,summary(lm(X710~Year+timeperiod))$coefficients[2,1]),
                              with(stationdf,summary(lm(X1060~Year+timeperiod))$coefficients[2,1]),
                              with(stationdf,summary(lm(X1120~Year+timeperiod))$coefficients[2,1])),
                          se=c(with(stationdf,summary(lm(X380~Year+timeperiod))$coefficients[2,2]),
                               with(stationdf,summary(lm(X710~Year+timeperiod))$coefficients[2,2]),
                               with(stationdf,summary(lm(X1060~Year+timeperiod))$coefficients[2,2]),
                               with(stationdf,summary(lm(X1120~Year+timeperiod))$coefficients[2,2])))

forest(rma(escalc(yi=atnaregYeardf$b,sei=atnaregYeardf$se,measure="GEN")))
atnaregYeardf

#Dataframe with temporal regression correlations
atnaregYearScaledf<-data.frame(Elevation=c('380m','710m','1060m',"1120m"),
                          b=c(with(stationdf,summary(lm(scale(X380)~Year))$coefficients[2,1]),
                              with(stationdf,summary(lm(scale(X710)~Year))$coefficients[2,1]),
                              with(stationdf,summary(lm(scale(X1060)~Year))$coefficients[2,1]),
                              with(stationdf,summary(lm(scale(X1120)~Year))$coefficients[2,1])),
                          se=c(with(stationdf,summary(lm(scale(X380)~Year))$coefficients[2,2]),
                               with(stationdf,summary(lm(scale(X710)~Year))$coefficients[2,2]),
                               with(stationdf,summary(lm(scale(X1060)~Year))$coefficients[2,2]),
                               with(stationdf,summary(lm(scale(X1120)~Year))$coefficients[2,2])))

atnaregYearScaledf<-data.frame(Elevation=c('380m','710m','1060m',"1120m"),
                               b=c(with(stationdf,summary(lm(scale(X380)~Year+timeperiod))$coefficients[2,1]),
                                   with(stationdf,summary(lm(scale(X710)~Year+timeperiod))$coefficients[2,1]),
                                   with(stationdf,summary(lm(scale(X1060)~Year+timeperiod))$coefficients[2,1]),
                                   with(stationdf,summary(lm(scale(X1120)~Year+timeperiod))$coefficients[2,1])),
                               se=c(with(stationdf,summary(lm(scale(X380)~Year+timeperiod))$coefficients[2,2]),
                                    with(stationdf,summary(lm(scale(X710)~Year+timeperiod))$coefficients[2,2]),
                                    with(stationdf,summary(lm(scale(X1060)~Year+timeperiod))$coefficients[2,2]),
                                    with(stationdf,summary(lm(scale(X1120)~Year+timeperiod))$coefficients[2,2])))


atnaregYearScaledf



#Dataframe with temperature correlactions
atnacorTempdf<-data.frame(Elevation=c('380m','710m','1060m','1120m'),
                          r=c(with(stationdfclim,cor(Annual,X380)),
                              with(stationdfclim,cor(Annual,X710)),
                              with(stationdfclim,cor(Annual,X1060,use='pairwise')),
                              with(stationdfclim,cor(Annual,X1120,use='pairwise'))),
                          n=c(length(stationdfclim$X380[!is.na(stationdfclim$X380)]),
                              length(stationdfclim$X710[!is.na(stationdfclim$X710)]),
                              length(stationdfclim$X1060[!is.na(stationdfclim$X1060)]),
                              length(stationdfclim$X1120[!is.na(stationdfclim$X1120)])))
atnacorTempdf

#Dataframe with temperature regression correlations
# atnaregAnnualdf<-data.frame(Elevation=c('380m','710m','1060m','1120m'),
#                           b=c(with(stationdfclim,summary(lm(X380~Annual))$coefficients[2,1]),
#                               with(stationdfclim,summary(lm(X710~Annual))$coefficients[2,1]),
#                               with(stationdfclim,summary(lm(X1060~Annual))$coefficients[2,1]),
#                               with(stationdfclim,summary(lm(X1120~Annual))$coefficients[2,1])),
#                           se=c(with(stationdfclim,summary(lm(X380~Annual))$coefficients[2,2]),
#                                with(stationdfclim,summary(lm(X710~Annual))$coefficients[2,2]),
#                                with(stationdfclim,summary(lm(X1060~Annual))$coefficients[2,2]),
#                                with(stationdfclim,summary(lm(X1120~Annual))$coefficients[2,2])))

atnaregAnnualdf<-data.frame(Elevation=c('380m','710m','1060m','1120m'),
                            b=c(with(stationdfclim,summary(lm(X380~Annual+timeperiod))$coefficients[2,1]),
                                with(stationdfclim,summary(lm(X710~Annual+timeperiod))$coefficients[2,1]),
                                with(stationdfclim,summary(lm(X1060~Annual+timeperiod))$coefficients[2,1]),
                                with(stationdfclim,summary(lm(X1120~Annual+timeperiod))$coefficients[2,1])),
                            se=c(with(stationdfclim,summary(lm(X380~Annual+timeperiod))$coefficients[2,2]),
                                 with(stationdfclim,summary(lm(X710~Annual+timeperiod))$coefficients[2,2]),
                                 with(stationdfclim,summary(lm(X1060~Annual+timeperiod))$coefficients[2,2]),
                                 with(stationdfclim,summary(lm(X1120~Annual+timeperiod))$coefficients[2,2])))

atnaregAnnualdf

forest(rma(escalc(yi=atnaregAnnualdf$b,sei=atnaregAnnualdf$se,measure="GEN")))


#Dataframe with scaled temperature regression correlations
# atnaregAnnualScaledf<-data.frame(Elevation=c('380m','710m','1060m','1120m'),
#                             b=c(with(stationdfclim,summary(lm(scale(X380)~Annual))$coefficients[2,1]),
#                                 with(stationdfclim,summary(lm(scale(X710)~Annual))$coefficients[2,1]),
#                                 with(stationdfclim,summary(lm(scale(X1060)~Annual))$coefficients[2,1]),
#                                 with(stationdfclim,summary(lm(scale(X1120)~Annual))$coefficients[2,1])),
#                             se=c(with(stationdfclim,summary(lm(scale(X380)~Annual))$coefficients[2,2]),
#                                  with(stationdfclim,summary(lm(scale(X710)~Annual))$coefficients[2,2]),
#                                  with(stationdfclim,summary(lm(scale(X1060)~Annual))$coefficients[2,2]),
#                                  with(stationdfclim,summary(lm(X1120~Annual))$coefficients[2,2])))
atnaregAnnualScaledf<-data.frame(Elevation=c('380m','710m','1060m','1120m'),
                                 b=c(with(stationdfclim,summary(lm(scale(X380)~Annual+timeperiod))$coefficients[2,1]),
                                     with(stationdfclim,summary(lm(scale(X710)~Annual+timeperiod))$coefficients[2,1]),
                                     with(stationdfclim,summary(lm(scale(X1060)~Annual+timeperiod))$coefficients[2,1]),
                                     with(stationdfclim,summary(lm(scale(X1120)~Annual+timeperiod))$coefficients[2,1])),
                                 se=c(with(stationdfclim,summary(lm(scale(X380)~Annual+timeperiod))$coefficients[2,2]),
                                      with(stationdfclim,summary(lm(scale(X710)~Annual+timeperiod))$coefficients[2,2]),
                                      with(stationdfclim,summary(lm(scale(X1060)~Annual+timeperiod))$coefficients[2,2]),
                                      with(stationdfclim,summary(lm(X1120~Annual+timeperiod))$coefficients[2,2])))

atnaregAnnualScaledf



#Meta analyses - correlations
par(mfrow=c(2,1))
esatnacoryr<-escalc(measure="COR",ri=atnacorYeardf$r,ni=atnacorYeardf$n)
rmaatnacoryr<-rma(esatnacoryr)
forest(rmaatnacoryr,slab=atnacorYeardf$Elevation,main="Atna mayflies - Year")

esatnacortemp<-escalc(measure="COR",ri=atnacorTempdf$r,ni=atnacorTempdf$n)
rmaatnacortemp<-rma(esatnacortemp)
forest(rmaatnacortemp,slab=atnacorYeardf$Elevation,main="Atna mayflies - Annual temperature")

#Meta-analysis - regressions

rmaatnaregyr<-rma(yi=atnaregYeardf$b,sei=atnaregYeardf$se,measure="GEN",weighted="F",method="FE")
forest(rmaatnaregyr,slab=atnaregYeardf$Elevation,main="Atna mayflies & stoneflies - Year",xlab="Regression slopes")

rmaatnaregtemp<-rma(yi=atnaregAnnualdf$b,sei=atnaregAnnualdf$se,measure="GEN",weighted="F",method="FE")
forest(rmaatnaregtemp,slab=atnaregYeardf$Elevation,main="Atna mayflies & stoneflies - Annual temperature",xlab="Regression slopes")

write.csv(atnacorYeardf,"Data/Inverts/AtnaCorYear.csv")
write.csv(atnaregYeardf,"Data/Inverts/AtnaRegYear.csv")
write.csv(atnaregYearScaledf,"Data/Inverts/AtnaRegYearScale.csv")
write.csv(atnaregAnnualScaledf,"Data/Inverts/AtnaRegTempScale.csv")

write.csv(atnacorTempdf,"Data/Inverts/AtnaCorTemp.csv")
write.csv(atnaregAnnualdf,"Data/Inverts/AtnaRegTemp.csv")

