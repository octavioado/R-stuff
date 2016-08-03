# Precipitation analysis
# Data from Planet OS
library(jsonlite)
library(ggplot2)

burl<-'http://api.planetos.com/v1/datasets/'
apik<- '4a80ef4d4b56437f835f1e820aaaa150'
datasetnames<-'noaa_gfs_global_sflux_0.12d'
lon<- -122.005152
lat<- 37.383252
var<-'Temperature_surface'

# precipitation data

psd <-paste0(burl,datasetnames,'/point?lon=',lon,'&lat=',lat,'&apikey=',apik,'&var=',var,'&reftime_end=2016-08-01T08:00:00&count=100')

pd  <- fromJSON(psd)
temp<-