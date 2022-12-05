#download and install packages
install.packages("nngeo")
library(nngeo)

#bring in data sets
setwd("C:/GitHub/hanakbe2/Final Project")
metaldata.csv <- read.csv("metal data.csv", header=T)
locationdata.csv <- read.csv("location data.csv", header=T)
salamanderdata.csv <- read.csv("Salamander Data.csv", header = T)
read.csv()

#merge metal data and location
?merge
locmet<- merge(locationdata.csv,metaldata.csv, by= "MonitoringLocationIdentifier", all=T)

#create specific object
salamander_so <- st_as_sf(salamanderdata.csv,coords = c("Lat", "Long"))
location_so <- st_as_sf(locationdata.csv, coords = c("LatitudeMeasure","LongitudeMeasure"))



#to do
#cut out extra files

#match location sites
matched = st_join(salamander_so, location_so, join = nngeo::st_nn, maxdist = 5000, k = 1, progress = FALSE)
matched
