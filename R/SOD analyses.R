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
