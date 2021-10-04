#Plant phenology anlaysis
library(readxl)


#Read in data

plantphen <- read_excel("Data/Plants/Phenology_herb_sheets_for_analysis.xlsx")
plantphendat<-read.csv("Data/Plants/PlantPhenology.csv",header=T,dec=',',sep=';')


#Look at dates samples were made
plantphendat$sampledDate<-as.Date(plantphendat$eventDate_ed,format="%d.%m.%Y")
plantphendat$sampledDate
summary(plantphendat$sampledDate)

hist(plantphendat$sampledDate,breaks="years")

plantphendat$Year<-format(plantphendat$sampledDate,'%Y')
plantphendat$DayofYear<-as.numeric(format(plantphendat$sampledDate,"%j"))

hist(plantphendat$DayofYear)
