# Precipitation analysis
# Data from Planet OS
library(jsonlite)
library(ggplot2)

burl<-'http://api.planetos.com/v1/datasets/'
apik<- ''
datasetnames<-'noaa_gfs_global_sflux_0.12d'
lon<- -122.005152
lat<- 37.383252
var<-'Temperature_surface'

# precipitation data

psd <-paste0(burl,datasetnames,'/point?lon=',lon,'&lat=',lat,'&apikey=',apik,'&var=',var,'&reftime_end=2016-08-01T08:00:00&count=1000')

pd  <- fromJSON(psd)
temp<-pd$entries$data
tim<-pd$entries$axes$time


#Visualization

ggplot(temp, aes(x=Temperature_surface))+ geom_histogram(stat = 'bin',binwidth = 3)
ggplot(temp, aes(y=Temperature_surface, x=seq(1:1000)))+ geom_point(aes(color=Temperature_surface)) +scale_color_continuous(name='Kelvin', low = 'blue', high = 'red')
