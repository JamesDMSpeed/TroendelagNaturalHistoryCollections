#Species occurrence data
library(metafor)


#Read in data

leadingedgeyear<-read.csv("Data/SOD/SpeciesOccurrenceLeadEdgeYearReg.csv",header=T)
leadingedgetemp<-read.csv("Data/SOD/SpeciesOccurrenceLeadEdgeTempReg.csv",header=T)
tailedgeyear<-read.csv("Data/SOD/SpeciesOccurrenceTailEdgeYearReg.csv",header=T)
tailedgetemp<-read.csv("Data/SOD/SpeciesOccurrenceTailEdgeTempReg.csv",header=T)

#Forest plots - skipping the total spp
par(mfrow=c(1,2))
leadingedgeyear_rma<-rma(yi=leadingedgeyear$b[6:21],sei=leadingedgeyear$se[6:21],measure="GEN")
leadingedgeyear_rma
forest(leadingedgeyear_rma,slab=leadingedgeyear$species[6:21],main="Leading edge - year",xlab="Regression slope")

leadingedgetemp_rma<-rma(yi=leadingedgetemp$b[6:21],sei=leadingedgetemp$se[6:21],measure="GEN")
leadingedgetemp_rma
forest(leadingedgetemp_rma,slab=leadingedgetemp$species[6:21],main="Leading edge - temperature",xlab="Regression slope")

tailedgeyear_rma<-rma(yi=tailedgeyear$b[6:21],sei=tailedgeyear$se[6:21],measure="GEN")
tailedgeyear_rma
forest(tailedgeyear_rma,slab=tailedgeyear$species[6:21],main="Tail edge - year",xlab="Regression slope")

tailedgetemp_rma<-rma(yi=tailedgetemp$b[6:21],sei=tailedgetemp$se[6:21],measure="GEN")
tailedgetemp_rma
forest(tailedgetemp_rma,slab=tailedgetemp$species[6:21],main="Tail edge - temperature",xlab="Regression slope")


alldistsyr<-rbind(leadingedgeyear,tailedgeyear)
alldistsyr$edge<-c(rep("leading",times=nrow(leadingedgeyear)),rep('tailing',times=nrow(tailedgeyear)))
alldiststemp<-rbind(leadingedgetemp,tailedgetemp)
alldiststemp$edge<-c(rep("leading",times=nrow(leadingedgetemp)),rep('tailing',times=nrow(tailedgetemp)))

write.csv(alldistsyr,"Data/SOD/SODYearReg.csv")
write.csv(alldiststemp,"Data/SOD/SODTempReg.csv")


#Individual species regression slopes
indspp<-read.csv("Data/SOD/regslopes_individualSpecies.csv",header=T)
indspp10<-indspp[indspp$N>=10,]

esleading<-escalc(yi=indspp10$regslope_year_leading,sei=indspp10$se_year_leading,measure="GEN")
rmaleading<-rma(esleading,method="FE",weighted=T,mods=~indspp10$kingdom-1)
rmaleading
forest(rmaleading,slab=indspp10$species)


#Table for individual spp
sppdistyr_scaled<-read.csv("Data/SOD/regslopes_individualSpecies_scaled.csv",header=T)
sppdisttemp_scaled<-read.csv("Data/SOD/regslopes_individualSpecies_temp_scaled.csv",header=T)

sppdistyr_scaled2<-sppdistyr_scaled[,c(2:4,6,10,11)]
sppdisttemp_scaled2<-sppdisttemp_scaled[,c(2,8:9)]

names(sppdisttemp_scaled2)[2:3]<-c("b_temperature","se_temperature")
names(sppdistyr_scaled2)[5:6]<-c("b_year","se_year")

combineddf<-merge(sppdistyr_scaled2,sppdisttemp_scaled2,by="species")
