setwd("/Users/glennd/Documents/TrondelagChange")
# install.packages("/Users/glennd/Documents/TrondelagChange/wicket_0.4.0.tar.gz", repos = NULL, type="source")
##--- PACKAGES ---#### 
library(rgbif)
library(stringr) # string manipulations (not needed, may also be done by base R)
library(plyr)  
library(dplyr) # for data-wrangling
library(rio)   # for importing/unzipping data
library(tidyr)
library(tidyverse)
library(wicket) # check WKT strings
library(tibble)
library(data.table) 
library(sf)      
library(ggplot2)
library(ggpubr)
library(ggExtra)
library(gridExtra)
library(mapview)
library(raster)   
library(rgeos)   
library(maptools)   
library(rgdal) 
library(taxize)
library(segmented)

##--- 1. GBIF DATA DOWNLOAD ---####
# Most of this is code adapted from the GBIF workshop during the OIKOS 2018 conference
# OBS! The following should only be done once, otherwise the data can be loaded directly
{
  # Make R ask for you login credentials:
  options(gbif_user=rstudioapi::askForPassword("my gbif username"))
  options(gbif_email=rstudioapi::askForPassword("my registred gbif e-mail"))
  options(gbif_pwd=rstudioapi::askForPassword("my gbif password"))
  
  # GBIF download key - be aware of the exact syntax of the occ_dowload. The geometry has to be counter-clockwise.
  # We can debate the exact predicates in the key, e.g. the 'institutionCode' might be worth removing, and then doing
  # the filtering later on. Likewise, the used bbox might be a bit overwhelming - this is up for debate/further cropping.
  ### OBS! This retrieves all data - if you wish to subset data into specific taxa, insert pred_in("taxonomic_level", c("taxa1","taxa2"))
  download_key <- occ_download(
    pred("hasGeospatialIssue", FALSE),
    pred("hasCoordinate", TRUE),
    pred("country", "NO"),
    pred_in("institutionCode", c("ntnu-vm","NTNU-VM","trh","TRH")),
    pred_within("POLYGON((8.102385 62.255654, 14.327401 62.255654, 14.327401 65.471311, 8.102385 65.471311, 8.102385 62.255654))")
  ) %>% 
    occ_download_meta
  
  # Define function to retrieve the requested data
  download_GBIF_API <- function(download_key,n_try,Sys.sleep_duration,destfile_name){
    start_time <- Sys.time()
    n_try_count <- 1
    
    download_url <- paste("http://api.gbif.org/v1/occurrence/download/request/",
                          download_key[1],sep="")
    
    try_download <- try(download.file(url=download_url,destfile=destfile_name,
                                      quiet=TRUE, mode="wb"),silent = TRUE)
    
    while (inherits(try_download, "try-error") & n_try_count < n_try) {   
      Sys.sleep(Sys.sleep_duration)
      n_try_count <- n_try_count+1
      try_download <- try(download.file(url=download_url,destfile=destfile_name,
                                        quiet=TRUE),silent = TRUE)
      print(paste("trying... Download link not ready. Time elapsed (min):",
                  round(as.numeric(paste(difftime(Sys.time(),start_time, units = "mins"))),2)))
    }
  }
  
  # Call function,  create and unzip zip-file for download - IGNORE THE WARNING, IT'S FINE!
  download_GBIF_API(download_key=download_key,destfile_name="tmp.zip",n_try=50,Sys.sleep_duration=180)   # Try 50 times, wait 3 minutes in between each try
  archive_files <- unzip("tmp.zip", files = "NULL", list = T) 
  
  # Get the occurrence.txt file in as a dataframe (using import from rio)
  occurrence <- import(unzip("tmp.zip", files="occurrence.txt"),header=T,sep="\t")
  
  # paste("GBIF Occurrence Download", download_key[2], "accessed via GBIF.org on", Sys.Date())
}

### Once the steps above have been performed, use this for future retrieval
library(bit64)
occurrence_troendelag <- fread("occurrence.txt")

##--- 1.1 Data cleaning ---####
# Double-check that the most important columns seem correct - several columns are logical, and carries no immediately important information -
# these can be removed for better overview
occurrence_troendelag <- occurrence_troendelag[,-c(2,4:9,11:26,28:36,38:39,41,43:57,63,67,73,76,78,82:83,85:86,89:92,94:95,97,
                                                   100:102,106,109,115:116,119:120,136:138,142:144,146:147,149:167,172:176,178:182,
                                                   184:190,197,201:203,205:206,214:215,228,234,239,248:249)]
str(occurrence_troendelag)

# Make the data spatial
occurrence_troendelag <- st_as_sf(occurrence_troendelag, coords = c("decimalLongitude", "decimalLatitude"), crs = 4326)

# Get map of norway for plotting
norway <- st_as_sf(getData("GADM", country="NO", level=0), crs=crs(occurrence_troendelag))
# Create the used bbox for illustration/plotting
troendelag <- data.frame( lon = c(8.102385, 14.327401, 14.327401, 8.102385, 8.102385),
                          lat = c(62.255654, 62.255654, 65.471311, 65.471311, 62.255654) ) %>%
  st_as_sf(coords = c("lon", "lat"), crs = 4326) %>%
  summarise(geometry = st_combine(geometry)) %>%
  st_cast("POLYGON")

plot(st_geometry(norway), border="gray35", col="gray80", axes=T, xlim=c(8,12), ylim=c(63,65.5))
plot(st_geometry(troendelag), border="red", lwd=2, axes=T, add=T)
plot(st_geometry(occurrence_troendelag), pch=".", cex=0.25, col="black", add=T)

# Check the distribution of presence/absence records
table(occurrence_troendelag$occurrenceStatus)   

# In the following, only summarise the PRESENT data:
occ_present <- occurrence_troendelag[occurrence_troendelag$occurrenceStatus == "PRESENT",]

# From previous experience, the species names reported as 'species' may not correspond to the names reported as genus+species.
# For that reason, create a new column coercing genus and specificEpithet and infraSpecificEpithet# Create a column incl subspecies and one without and compare
occ_present <- occ_present %>%
  dplyr::mutate(species_sp = str_trim(paste(genus, specificEpithet, sep=" ")))
length(unique(occ_present$species))
length(unique(occ_present$species_sp))

# As we will be doing temporal analyses later, we will also need to only include records with temporal information:
occ_present <- occ_present %>%  
  filter(!is.na(year))         
str(occ_present)
unique(occ_present$datasetName)
## Now just getting ocean taxa - all contained in the "Marine invertebrate collection NTNU University Museum" dataset
unique(occ_present_ocean$datasetName)

occ_present_ocean <- subset(occ_present, datasetName == "Marine invertebrate collection NTNU University Museum")
str(occ_present_ocean)

##--- 1.2 Temporal data cleaning ####
# We further have to filter data based on a minimum number of records for each species (either in total or as a minimum no. records pr year)
# Summarise how many times pr year each species occurs
summ <- as.data.frame(table(st_drop_geometry(occ_present_ocean)$species_sp, st_drop_geometry(occ_present_ocean)$year))
names(summ) <- c("species_sp","year","freq")
summ$species_sp <- as.character(summ$species_sp)
summ$year <- as.integer(as.character(summ$year))

# An initial suggestion for the minimum no. records pr year is 5 - this means that all records for which the species in question
# is registered <5 times within said year will be removed
occ_present_ocean <- anti_join(occ_present_ocean, summ[summ$freq<5,c("species_sp","year")], by=c("species_sp","year"="year"))  

# Filter records from after 1900, as this is the temporal range for the used climate data
occ_present_ocean <- occ_present_ocean %>%
  filter(year>=1900)  

summ2 <- st_drop_geometry(occ_present_ocean) %>%
  group_by(species_sp, kingdom,phylum,class,order,family) %>%
  summarise(minyear = min(year),
            maxyear = max(year),
            rangeyear = max(year) - min(year) +1)  # +1 as the range cannot be 0
# Remove species with less than 50 years of data
summ2 <- summ2 %>%
  filter(rangeyear >=50)  

unique(summ2$species_sp)
# Subset dataset to only include the species above:
occ_present_ocean <- right_join(occ_present_ocean, summ2[,c("species_sp","minyear","maxyear","rangeyear")], by=c("species_sp")) 
str(occ_present_ocean)
## lets plot the data now
troendelag <- data.frame( lon = c(8.102385, 14.327401, 14.327401, 8.102385, 8.102385),
                          lat = c(62.255654, 62.255654, 65.471311, 65.471311, 62.255654) ) %>%
  st_as_sf(coords = c("lon", "lat"), crs = 4326) %>%
  summarise(geometry = st_combine(geometry)) %>%
  st_cast("POLYGON")

plot(st_geometry(norway), border="gray35", col="gray80", axes=T, xlim=c(9,12), ylim=c(63.25,65.5))
plot(st_geometry(troendelag), border="red", lwd=2, axes=T, add=T)
plot(st_geometry(occ_present_ocean), pch=".", cex=0.5, col="black", add=T)


##--- 2. SUMMARY & EXPORT        ---####
# Summarise the latitudinal data for each species for each year; I have used the 10th and 90th percentile as the values
# of interest - edit these to whatever is needed
unique(occ_present_ocean$species_sp)

## Turns out most marine invertebrate records are taxonomically unidentifed
subs <- subset(occ_present_ocean, species_sp =="")
unique(occ_present_ocean$species_sp)
occ_present_ocean <- subset(occ_present_ocean, species_sp !="" | species_sp !="Abra")
lat <- st_drop_geometry(occ_present_ocean) %>%
  group_by(species_sp, year, kingdom,phylum,class,order,family) %>%
  summarise(max = max(lat),
            range = max(lat) - min(lat),
            mean = mean(lat),
            median = median(lat),
            q10 = quantile(lat, c(0.10)),
            q90 = quantile(lat, c(0.90)))
## error about sf format...
## I am not used to sf objects and dont have the time right now.. I am just extracting lats and longs in columns and working 
# from there
joint <- st_coordinates(occ_present_ocean)
joint <- as.data.frame(joint)
posi <- st_drop_geometry(occ_present_ocean)

## lat and longs stored in columns latt and long respectively
posi$long <- joint$X
posi$latt <- joint$Y

str(posi)
colnames(posi)
unique(posi$catalogNumber)

lat <- posi  %>%
  group_by(species_sp, year, kingdom,phylum,class,order,family) %>%
  summarise(max = max(latt),
            range = max(latt) - min(latt),
            mean = mean(latt),
            median = median(latt),
            q10 = quantile(latt, c(0.10)),
            q90 = quantile(latt, c(0.90)),
            uniqlat = n_distinct(latt),
            total = n())
unique(lat$species_sp)
hist(lat$range, breaks =20)

## 122 species, but lots of species records in particular years are just from one place. i.e. - range = 0
## these could be relevant for aggregate patterns, but not for individual species summary stats ..
## making sure a range of locations were sampled for each year by removing records with a latitudinal range <0.4 (arbitrary)
## change to whatever is appropriate
latrange <- subset(lat, range > 0.4)
latrange <- droplevels(latrange)
unique(latrange$species_sp)
## down to 104 species
unique(lat$species_sp)
str(latrange)
ggplot(latrange, aes(x=year, y=q10, color=species_sp)) +
  geom_point(,show.legend = FALSE) + 
  geom_smooth(method=lm,show.legend = FALSE, se=F)
## need to select longer time series again
## lots of species now with only a few years so need to resummarise and select species with decent span (50+ years)
summ3 <- latrange %>%
  group_by(species_sp, kingdom,phylum,class,order,family) %>%
  summarise(minyear = min(year),
            maxyear = max(year),
            rangeyear = max(year) - min(year) +1) 

summ3 <- summ3 %>%
  filter(rangeyear >=50) 
str(summ3)

latrangefil <- right_join(latrange, summ3[,c("species_sp","minyear","maxyear","rangeyear")], by=c("species_sp")) 
str(latrangefil)
unique(latrangefil$species_sp)

## Checking records with longer time series again:
ggplot(latrangefil, aes(x=year, y=q90, color=species_sp)) +
  geom_point(,show.legend = FALSE) + 
  geom_smooth(method=lm,show.legend = FALSE, se=F)
unique(latrangefil$species_sp)
## this lands us with just 35 species (three of which are only genera IDs and need discarding anyway)
## still lots of species with only a few points (i.e. 50 years span but one one point on either end..)
## Another problem is that many species are widespread (i.e in the Med, tropics and subtropics.. )
## therefore using 'coldphilic' definition of Tanyas may be more problemtic with marine stuff because this definition is based on
## distributions in Norway.. there are so few species now, so I went through manually and only retained species that
## were restricted to northern (boreal) latitudes (not in med)

latcull <- subset(latrangefil, species_sp == "Calocaris macandreae" | species_sp =="Margarites groenlandicus"|
                    species_sp == "Musculus niger" | species_sp =="Neptunea despecta"|
                    species_sp == "Nuculana pernula" | species_sp =="Ophiopholis aculeata"|
                    species_sp == "Stenosemus albus" | species_sp =="Testudinalia testudinalis"|
                    species_sp =="Velutina velutina")
ggplot(latcull, aes(x=year, y=q90, color=species_sp)) +
  geom_point(,show.legend = FALSE) + 
  geom_smooth(method=lm, se=F)

### plotting filtered data
ggarrange(
  latcull %>%
    ggplot(aes(x=year, y=q10)) +
    geom_point(size=.1) +
    geom_smooth(size=.5, method = "lm") +
    labs(title="",x="Year",y="10th percentile of registered latitudes") +
    theme_minimal() +
    theme(legend.position = "bottom"),
  latcull %>%
    ggplot(aes(x=year, y=q90)) +
    geom_point(size=.1) +
    geom_smooth(size=.5, method = "lm") +
    labs(title="",x="Year",y="90th percentile of registered latitudes") +
    theme_minimal() +
    theme(legend.position = "bottom"),
  latcull %>%
    ggplot(aes(x=year, y=median)) +
    geom_point(size=.1) +
    geom_smooth(size=.5, method = "lm") +
    labs(title="",x="Year",y="median of registered latitudes") +
    theme_minimal() +
    theme(legend.position = "bottom"),
  latcull %>%
    ggplot(aes(x=year, y=mean)) +
    geom_point(size=.1) +
    geom_smooth(size=.5, method = "lm") +
    labs(title="",x="Year",y="mean of registered latitudes") +
    theme_minimal() +
    theme(legend.position = "bottom"),
  ncol=2, nrow=2, common.legend = T)

# Compile and export table(s) for meta-analysis
## For species with records over 50 years in temporal duration with >5 records per year
write.csv(lat, file="lat.csv")

## For species with records over 50 years in temporal duration, with >5 records per year sampled at a range of locations
## at least 0.4 degrees latitude apart in each year
write.csv(latcull, file="latcull.csv")

## Aggregates of the above two datasets by year
exp <- lat %>% 
  group_by(year) %>% summarise(q90_cold_all = mean(q90, na.rm=T),
                                           n90_cold_all = n(),
                                           q10_cold_all = mean(q10, na.rm=T),
                                           n10_cold_all = n())
write.csv(exp, file="lat_aggregate.csv")
exp1 <- latcull %>% 
  group_by( year) %>% summarise(q90_cold_all = mean(q90, na.rm=T),
                               n90_cold_all = n(),
                               q10_cold_all = mean(q10, na.rm=T),
                               n10_cold_all = n())
write.csv(exp1, file="latcull_aggregate.csv")