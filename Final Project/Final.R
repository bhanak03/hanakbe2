#READ ME- this code doesn't run with ctrl A + ctrl  + enter because some of the stats tests do not have enough information to run, but they will run by
#going line by line.

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


#create specific object - spatial object
salamander_so <- st_as_sf(salamanderdata.csv,coords = c("Lat", "Long"))
location_so <- st_as_sf(locmet, coords = c("LatitudeMeasure","LongitudeMeasure"))


#match location sites
matched = st_join(salamander_so, location_so, join = nngeo::st_nn, maxdist = 5000, k = 1, progress = FALSE)
matched
df <- matched


#remove NAs (this wound up being irrelevant for what I needed)
salloc <- na.omit(df)


#run lm
library(spdep)
library(adespatial)
library(vegan)
library(readxl)

colnames(locmet)
colnames(salloc)


#PJOR(Plethodon jordani) species
pjor <- subset(matched, matched$Species=="PJOR")
mod<- lm(pjor$Number~pjor$ResultMeasureValue+pjor$CharacteristicName)
anova(mod)
summary(mod)
#iron, zinc, strontium, manganese significant

#iron subset, test (also you wrote this so I didn't count it towards my graphs)
pjorIron <- subset(matched, matched$Species=="PJOR" & matched$CharacteristicName == "Iron")
plot(pjorIron$Number~as.factor(pjorIron$ResultMeasureValue))

#DOCO (Desmognathus ocoee) species
doco <- subset(matched, matched$Species=="DOCO")
mod <- lm(doco$Number~doco$ResultMeasureValue+doco$CharacteristicName)
anova(mod)
summary(mod)
#zinc, manganese, and iron significant

#EWIL (Eurycea wilderae) species
ewil <- subset(matched, matched$Species=="EWIL")
mod <- lm(ewil$Number~ewil$ResultMeasureValue+ewil$CharacteristicName)
anova(mod)
summary(mod)
#iron, zinc, manganese, and iron significant

#PTEY (Plethodon teyahalee) species
ptey <- subset(matched, matched$Species=="PTEY")
mod <- lm(ptey$Number~ptey$ResultMeasureValue+ptey$CharacteristicName)
anova(mod)
summary(mod)
#strontium and lead significant 

#DIMI (Desmognathus imitator) species
dimi <- subset(matched, matched$Species=="DIMI")
mod <- lm(dimi$Number~dimi$ResultMeasureValue+dimi$CharacteristicName)
anova(mod)
summary(mod)
#iron significant 

#DMON (Desmognathus monticola) species
dmon <- subset(matched, matched$Species=="DMON")
mod <- lm(dmon$Number~dmon$ResultMeasureValue+dmon$CharacteristicName)
anova(mod)
summary(mod)
#no significance

#DCON (Desmognathus conanti) species
dcon <- subset(matched, matched$Species=="DCON")
mod <- lm(dcon$Number~dcon$ResultMeasureValue+dcon$CharacteristicName)
anova(mod)
summary(mod)
#only one unique value, cannot run lm

#DQUA (Desmognathus quadramaculatus) species
dqua <- subset(matched, matched$Species=="DQUA")
mod <- lm(dqua$Number~dqua$ResultMeasureValue+dqua$CharacteristicName)
anova(mod)
summary(mod)
#perfect fit, summary unreliable

#DSAN (Desmognathus santeelah) species
dsan <- subset(matched, matched$Species=="DSAN")
mod <- lm(dsan$Number~dsan$ResultMeasureValue+dsan$CharacteristicName)
anova(mod)
summary(mod)
#no significance

#DWRI (Desmognathus wrighti) species
dwri <- subset(matched, matched$Species=="DWRI")
mod <- lm(dwri$Number~dwri$ResultMeasureValue+dwri$CharacteristicName)
anova(mod)
summary(mod)
#iron, manganese, and strontium significant 

#GPOR (Gyrinophilus porphyriticus) species
gpor <- subset(matched, matched$Species=="GPOR")
mod <- lm(gpor$Number~gpor$ResultMeasureValue+gpor$CharacteristicName)
anova(mod)
summary(mod)
#perfect fit, summary unreliable

#NVIR (Notopthalmus viridescenes) species
nvir <- subset(matched, matched$Species=="NVIR")
mod <- lm(nvir$Number~nvir$ResultMeasureValue+nvir$CharacteristicName)
anova(mod)
summary(mod)
#only one unique value, cannot run lm

#PGLU (Plethodon glutinosus) species
pglu <- subset(matched, matched$Species=="PGLU")
mod <- lm(pglu$Number~pglu$ResultMeasureValue+pglu$CharacteristicName)
anova(mod)
summary(mod)
#perfect fit, summary unreliable

#PSER (Plethodon serratus) species
pser <- subset(matched, matched$Species=="PSER")
mod <- lm(pser$Number~pser$ResultMeasureValue+pser$CharacteristicName)
anova(mod)
summary(mod)
#perfect fit, summary unreliable

#All species and metals
mod <- lm(as.numeric(as.character(matched$Number))~Species+ResultMeasureValue+CharacteristicName, data=matched)
anova(mod)
summary(mod)
#strontium, manganese(most significant), and iron significant among all species

#graph of most common species (pjor) and most significant metal (manganese)
pjormang <- subset(matched, matched$Species=="PJOR" & matched$CharacteristicName == "Manganese")
plot(pjormang$Number~as.factor(pjormang$ResultMeasureValue),ylab="",xlab="")
title(main = "Comparison of PJOR Salamander presence and Manganese")
title(xlab = "Manganese (mg/l)", ylab= "PJOR Salamander Number")


#graph of second most common species (ewil) and most significant metal (manganese)
ewilmang <- subset(matched, matched$Species=="EWIL" & matched$CharacteristicName == "Manganese")
plot(ewilmang$Number~as.factor(ewilmang$ResultMeasureValue),ylab="",xlab="")
title(main = "Comparison of EWIL Salamander presence and Manganese")
title(xlab = "Manganese (mg/l)", ylab= "EWIL Salamander Number")


#do gam for "big honker" -matt

library(MASS)
install.packages("MuMIn")
library(MuMIn)
library(mgcv)
install.packages("lme4")
library(lme4)

#GAM model for all species and all metals
gam.mod <- gam(as.numeric(as.character(matched$Number))~Species+ResultMeasureValue+CharacteristicName, family = gaussian, random = list(ID=~ 1), data = matched)
summary(gam.mod)
#strontium, manganese, and iron were sufficient among all species

#GAM pl/ot
plot(gam.mod$residuals, ylim = c(-.1,.1), ylab="Residuals")
title(main = "Residuals of All Salamander Species Compared with All Metals")
AIC(gam.mod)
#crowded (bad) residual model and high AIC :( (i.e not slay)
#AIC only matters in comparison to another model, but you're spot on about the residuals.

# Your code is easy to follow and appropriately commented!

#only a couple things that would improve it:
  #If you remove the ylim from your residual plot it is more informative of a bad fit (though you already knew this)
plot(gam.mod$residuals, ylab="Residuals")
  #If the gam was a poor fit for the data, why not try a different distribution, or different combos of 
      #Species+ResultMeasureValue+CharacteristicName and/or interactive effects to compare models?
  #You did a great job with the linear models walking through the options, it would have made sense to do the same with the GAM.



