
#library(parallel)
library(future.apply)
library(progressr)
#https://cran.r-project.org/web/packages/progressr/vignettes/progressr-intro.html

#daymet climate data information
#https://daymet.ornl.gov/overview

#MODIS GPP data information
#https://lpdaac.usgs.gov/products/myd17a2hv061/

#import daymet precipitation data -----
library(daymetr)

#two ecoregions

#northern_mixed_prairies
#shortgrass_steppe
region_name = 'northern_mixed_prairies' 

#set year

#2003-2020
year_value = '2020'

#run script to access, format, and save data
source('daymet_ppt_import_script.R')

look <- raster(filename)
plot(look)


look.2 <- raster("./../../Data/Climate/Ecoregion/northern_mixed_prairies/Precipitation/Period/14/Precip_2012_14.tif")
plot(look.2)


#-------------------------------------------------------------------------------
#import MODIS GPP data -----

#load library for API:
library(MODISTools)

#two ecoregions:
#northern_mixed_prairies
#shortgrass_steppe

region_name = 'northern_mixed_prairies'

#set year
#2003-2020

year_value = '2008'

#runs script to access, format, and save data

source('gpp_import_script.R')

look <- raster(filename)
summary(look)
plot(look)

look.2 <- raster("./../../Data/GPP/Ecoregion/northern_mixed_prairies/MODIS_GPP/Period/8/GPP_2019_8.tif")
plot(look.2)

#-------------------------------------------------------------------------------
#import daymet temp data -----
library(daymetr)

#two ecoregions

#northern_mixed_prairies
#region_name = 'shortgrass_steppe'
region_name = 'northern_mixed_prairies' 

#set year

#2003-2020
#year_value = '2004'

years <- seq(2019,2020,1)

for(j in years){

year_value = j
  
#run script to access, format, and save data
source('daymet_temp_import_script.R')
  
}

