## code to prepare `DATASET` dataset goes here

usethis::use_data(DATASET, overwrite = TRUE)
library(gridExtra)
library(tmaptools)
library(leaflet)
library(readxl)
library(sf)
setwd("~/Desktop/Bachelorarbeit")
sfdata=st_read("Datafolder/sf_data.shp", quiet=T)

Germany=st_read("Datafolder/Germany/Germany.shp",quiet=T)
Rivers=st_read("Datafolder/GIS DATA/data/germany-waterways-shape/waterways.shp", quiet=T)
grdc= read_excel("Datafolder/grdc_03_2021/catalogue/GRDC_Stations_Germany.xlsx")
Germany=st_read("Datafolder/Germany/Germany.shp",quiet=T)
Rivers=st_read("Datafolder/GIS DATA/data/germany-waterways-shape/waterways.shp", quiet=T)
#Darstellung

#character to double
grdc$r_height_yr=as.numeric(grdc$r_height_yr)
## Warning: NAs durch Umwandlung erzeugt
grdc$r_volume_yr=as.numeric(grdc$r_volume_yr)
## Warning: NAs durch Umwandlung erzeugt
grdc$lta_discharge=as.numeric(grdc$lta_discharge)
## Warning: NAs durch Umwandlung erzeugt
#es sind NAs in der Tabelle

#geostat.
st_grdc=st_as_sf(grdc, coords=c("long","lat"), crs=4326 )
#wie finde ich perfekten crs code? WGS84..
setwd("~/Desktop/Bachelorarbeit/Paket erstellen/dischanalyst")
grdc_germany=st_grdc

