
#library(parallel)
library(future.apply)
library(progressr)
#https://cran.r-project.org/web/packages/progressr/vignettes/progressr-intro.html

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
get_daymet <- function(i){
  
  temp_lat <- sgs.1000[i, ] %>% pull(y)
  temp_lon <- sgs.1000[i, ] %>% pull(x)
  
  
  test <- download_daymet(
    lat = temp_lat,
    lon = temp_lon,
    start = year_value,
    end = year_value
  ) %>% 
    #--- just get the data part ---#
    .$data #%>% 
  # #--- convert to tibble (not strictly necessary) ---#
  # as_tibble() %>% 
  # #--- assign site_id so you know which record is for which site_id ---#
  # mutate(site_id = temp_site) %>% 
  # #--- get date from day of the year ---#
  # mutate(date = as.Date(paste(year, yday, sep = "-"), "%Y-%j"))
  
  test<- test %>%
    dplyr::filter(yday > 57) %>%
    dplyr::filter(yday < 297) 
  
  
  # summary(test)
  # head(test)
  
  #subset to precip
  test<-test[c(1,2,4)]
  
  #get 16-day sums of precip for each year for the given pixel
  #year_test <- unique(test$year)
  
  summed_precip <- get_16_day_sums(test)
  
  #add period ID
  summed_precip$period <- rownames(summed_precip)
  summed_precip$period <- as.numeric(summed_precip$period) + 1
  
  #add in year and coordinate columns
  summed_precip$year <- year_value
  summed_precip$x<-temp_lon
  summed_precip$y<-temp_lat
  
  return(summed_precip)
  
}
source('daymet_ppt_import_script.R')

look <- raster(filename)
plot(look)


look.2 <- raster("./../../Data/Climate/Ecoregion/northern_mixed_prairies/Precipitation/Period/14/Precip_2012_14.tif")
plot(look.2)


#-------------------------------------------------------------------------------
#import MODIS GPP data -----

#load library for API:
library(MODISTools)

#Data source:
#https://lpdaac.usgs.gov/products/myd17a2hv061/

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
