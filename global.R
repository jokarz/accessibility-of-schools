#Preloading
packages = c('rgdal', 'spatstat', 'raster', 'tidyverse', 'sf', 'KernSmooth',
             'sp', 'geojsonio', 'leaflet', 'dplyr', 'SpatialAcc', 'rgeos', 
             'RColorBrewer', 'spdplyr', 'tools', 'DT') 

for (p in packages){
  if(!require(p, character.only = T)){ 
    install.packages(p)
  }
  library(p,character.only = T) 
}

isopath = 'data/isodata'
csvpath = 'data/csvdata'
matrixpath = 'data/matrixdata'
spatialpath = 'data/spatialdata'
objpath = 'data/objdata'

schIcon <- makeIcon(
  iconUrl = "www/schicon.png",
  iconWidth = 30, iconHeight = 36,
  iconAnchorX = 15, iconAnchorY = 36,
  popupAnchorX = 1, popupAnchorY = -35
)

residentialdata = readRDS(file=paste0(objpath, '/', 'all.residential.rds'))
postaldata = readRDS(file=paste0(objpath, '/', 'all.postal.rds'))
schdata = readRDS(file=paste0(objpath, '/', 'all.school.rds'))
kernelspatial = readRDS(file=paste0(objpath, '/', 'kernel.residential.rds'))

