#Marine invertebrates
library(metafor)

#Read in data

marineinverts<-read.csv("Data/Marine invertebrates/marineIVlatcull.csv",header=T)


#Effect sizes for q10 and q90s

marineq90coryear<-data.frame(Species=levels(marineinverts$species_sp),R=(rep(NA,times=9)),N=rep(NA,times=9),Duration=rep(NA,times=9))
marineq90regyear<-data.frame(Species=levels(marineinverts$species_sp),b=(rep(NA,times=9)),se=rep(NA,times=9))
marineq10coryear<-data.frame(Species=levels(marineinverts$species_sp),R=(rep(NA,times=9)),N=rep(NA,times=9),Duration=rep(NA,times=9))
marineq10regyear<-data.frame(Species=levels(marineinverts$species_sp),b=(rep(NA,times=9)),se=rep(NA,times=9))
marinemedcoryear<-data.frame(Species=levels(marineinverts$species_sp),R=(rep(NA,times=9)),N=rep(NA,times=9),Duration=rep(NA,times=9))
marinemedregyear<-data.frame(Species=levels(marineinverts$species_sp),b=(rep(NA,times=9)),se=rep(NA,times=9))



for(i in 1:9){
marineq90coryear$R[i]<-with(marineinverts[marineinverts$species_sp==levels(marineinverts$species_sp)[i],],cor(year,q90))
marineq90coryear$N[i]<-with(marineinverts[marineinverts$species_sp==levels(marineinverts$species_sp)[i],],length(year))
marineq90coryear$Duration[i]<-with(marineinverts[marineinverts$species_sp==levels(marineinverts$species_sp)[i],],max(year)-min(year))
lmI<-with(marineinverts[marineinverts$species_sp==levels(marineinverts$species_sp)[i],],lm(q90~year))
marineq90regyear$b[i]<-summary(lmI)$coefficients[2,1]
marineq90regyear$se[i]<-summary(lmI)$coefficients[2,2]

marineq10coryear$R[i]<-with(marineinverts[marineinverts$species_sp==levels(marineinverts$species_sp)[i],],cor(year,q10))
marineq10coryear$N[i]<-with(marineinverts[marineinverts$species_sp==levels(marineinverts$species_sp)[i],],length(year))
marineq10coryear$Duration[i]<-with(marineinverts[marineinverts$species_sp==levels(marineinverts$species_sp)[i],],max(year)-min(year))
lmI<-with(marineinverts[marineinverts$species_sp==levels(marineinverts$species_sp)[i],],lm(q10~year))
marineq10regyear$b[i]<-summary(lmI)$coefficients[2,1]
marineq10regyear$se[i]<-summary(lmI)$coefficients[2,2]

marinemedcoryear$R[i]<-with(marineinverts[marineinverts$species_sp==levels(marineinverts$species_sp)[i],],cor(year,median))
marinemedcoryear$N[i]<-with(marineinverts[marineinverts$species_sp==levels(marineinverts$species_sp)[i],],length(year))
marinemedcoryear$Duration[i]<-with(marineinverts[marineinverts$species_sp==levels(marineinverts$species_sp)[i],],max(year)-min(year))
lmI<-with(marineinverts[marineinverts$species_sp==levels(marineinverts$species_sp)[i],],lm(median~year))
marinemedregyear$b[i]<-summary(lmI)$coefficients[2,1]
marinemedregyear$se[i]<-summary(lmI)$coefficients[2,2]

}

marineq90coryear
marineq90regyear
marineq10coryear
marineq10regyear
marinemedcoryear
marinemedregyear


marineq90year_rma<-rma(yi=marineq90regyear$b,sei=marineq90regyear$se,measure="GEN",weighted=F,method="FE")
marineq90year_rma
forest(marineq90year_rma,slab=marineq90regyear$Species,main="Leading edge - year",xlab="Regression slope")

marineq10year_rma<-rma(yi=marineq10regyear$b,sei=marineq10regyear$se,measure="GEN",weighted=F,method="FE")
marineq10year_rma
forest(marineq10year_rma,slab=marineq10regyear$Species,main="Tailing edge - year",xlab="Regression slope")

marinemedyear_rma<-rma(yi=marinemedregyear$b,sei=marinemedregyear$se,measure="GEN",weighted=F,method="FE")
marinemedyear_rma
forest(marinemedyear_rma,slab=marinemedregyear$Species,main="Median - year",xlab="Regression slope")


write.csv(marineq10coryear,"Data/Marine invertebrates/marineeq10coryear")
write.csv(marineq10regyear,"Data/Marine invertebrates/marineeq10regryear")
write.csv(marineq90coryear,"Data/Marine invertebrates/marineeq90coryear")
write.csv(marineq90regyear,"Data/Marine invertebrates/marineeq90regyear")

