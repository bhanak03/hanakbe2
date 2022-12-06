#download and install packages
install.packages("nngeo")
library(nngeo)

#bring in data sets
setwd("C:/GitHub/hanakbe2/Final Project")
metaldata.csv <- read.csv("MetalDataXR.csv", header=T)
locationdata.csv <- read.csv("LocationDataXR.csv", header=T)
salamanderdata.csv <- read.csv("SalamanderDataXR.csv", header = T)


#merge metal data and location
?merge
metaldata.csv <- metaldata.csv[complete.cases(metaldata.csv),]
locmet<- merge(locationdata.csv,metaldata.csv, by.x= "MonitoringLocationIdentifier", by.y= "MonitoringLocationIdentifier")


#create specific object
salamander_so <- st_as_sf(salamanderdata.csv,coords = c("Lat", "Long"))
location_so <- st_as_sf(locmet, coords = c("LatitudeMeasure","LongitudeMeasure"))


#match location sites
matched = st_join(salamander_so, location_so, join = nngeo::st_nn, maxdist = 5000, k = 1, progress = FALSE)
matched
df <- matched


#remove NAs
salloc <- na.omit(df)


#run RDA
library(spdep)
library(adespatial)
library(vegan)
library(readxl)

colnames(locmet)
colnames(salloc)

pjor <- subset(matched, matched$Species=="PJOR")

pjorIron <- subset(matched, matched$Species=="PJOR" & matched$CharacteristicName == "Iron")

mod<- lm(pjor$Number~pjor$ResultMeasureValue+pjor$CharacteristicName)
anova(mod)
boxplot(pjorIron$Number~as.factor(pjorIron$ResultMeasureValue))


mod <- lm(as.numeric(as.character(matched$Number))~Species+ResultMeasureValue+CharacteristicName, data=matched)
anova(mod)
summary(mod)
