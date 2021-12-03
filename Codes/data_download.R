
library(rgee)

library(sf)
library(getPass)
library(dplyr)
#ee_clean_pyenv()

#rgee::ee_install()

ee_Initialize(email = getPass("Enter your gmail address"), drive = TRUE)

## get the bounmdary of Kilifi
# read the Kilifi shapefile and convert it to EE objectr
roi <- sf::st_read("/Users/rragankonywa/Desktop/Muyodi/plan A/polygon project Area/PA.shp")
names(roi)[2] <- "area"
roi_ee <- sf_as_ee(roi)
bands <- c("B1", "B2", "B3", "B4", "B5", "B6", "B7", "B8", "B8A", "B9","B11", "B12")
# Filter the GEE database and get images for specified dates and clip each to kilifi
object <- ee$ImageCollection("COPERNICUS/S2_SR")$
  filterDate("2020-01-01", "2020-12-31")$
  filterBounds(roi_ee)$
  select(bands)$
  filter(ee$Filter()$lte("CLOUDY_PIXEL_PERCENTAGE", 20))$
  map(function(image){
    return (image$clip(roi_ee))
  })

visParams = list(
  min = 0,
  max = 3000,
  bands = c('B4', 'B3', 'B2')
)

length_ee(object)

ee_print(object$first())

Map$centerObject(roi_ee)
Map$addLayer(index_ee_ic(object,9),visParams, "rgb")
Map$addLayer(roi_ee,visParams,"roi")



batchDownloadR_ee <- function(image_collection, bands,resolution, roi, progress=NULL, folder){
  for(i in 1:length_ee(image_collection)){
    image <- index_ee_ic(image_collection, i)
    title_name <- image$get('system:index')$getInfo()
    
    extents = ee$Image$pixelLonLat()$updateMask(image$select(c("B1"))$mask())$
      reduceRegion(ee$Reducer$minMax(), roi, resolution)
    
    bounds = ee$Geometry$Rectangle(extents$values(c("longitude_min", "latitude_min", "longitude_max", "latitude_max")))
    
    
    downConfig = list(scale = resolution, maxPixels = 10000000000000 , 
                      region = bounds, 
                      fileFormat = 'GeoTIFF', 
                      driveFolder = folder)
    
    task = ee$batch$Export$image(image,title_name, 
                                 downConfig)
    
    task$start()
    ee_monitoring()
  }
  if(progress==T){
    task$start()
    ee_monitoring()
  } else{
    task$start()
    # ee_monitoring()
  }
  
}

# bands to download


source("/Users/rragankonywa/OneDrive/UniWurzburg/EAGLES/my_functions/R_gee_functions/index_ee_ic.R")


batchDownloadR_ee(image_collection = object,bands = bands,resolution = 10,roi = roi_ee,progress = T,folder="Muyodi_images_2020")

ee_print(object$first())


