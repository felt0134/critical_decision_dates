
#import and save to file 1 km MODIS GPP

library(MODISTools)

#import and subset
rangeland_npp_covariates <- readRDS('./../../Data/Herbaceous_NPP_1986_2015_V2.rds')
head(rangeland_npp_covariates)
mean_production<-aggregate(npp~ x + y + region,mean,data=rangeland_npp_covariates)
sgs<-subset(mean_production,region==c('shortgrass_steppe'))
#head(sgs)
sgs_raster <- rasterFromXYZ(sgs[c(1,2,4)])
plot(sgs_raster)

#increase pixel resolution
#check math
#http://uop.whoi.edu/UOPinstruments/frodo/aer/julian-day-table.html
# 1/7 #want to pixel size to be 1/7 of original
# .0625*0.145
# 0.0090625*111 #concert to km. This equals about 1 km
# 1/0.145 #convert so its a factor
sgs_raster <- raster::disaggregate(sgs_raster,fact=7) #convert to ~1 km resolution

#convert back to df
sgs <- data.frame(rasterToPoints(sgs_raster))

#LOOP PREP
summed_GPP_list <- list()
year_list <- list()
period_values <- c(1:15)

#set this up first
sgs.1000<-sgs[(1:50),]

#test_function <- get_rainfall_period(period_id=2)

id_values <- c(1:14)
year_values <- c(2003:2020)
year_id_list_2 <- list()

Ecoregion = 'SGS'
#Ecoregion='NMP'

#all i is doing is allowing you to eventually subset by each year (1986-2019)
for(i in year_values){

  aea.proj <- "+proj=longlat +datum=WGS84"
  
  # PERIOD 1
  
  test_function <- get_modis_gpp_period(period_id=1)
  #test_function$year <- as.numeric(test_function$year)
  #str(test_function)
  test_function <-subset(test_function,year==i)  
  
  year_id_list_2[[i]] <- test_function
  
  year_id_df <- data.frame(year_id_list_2[[i]])
  year_id_df <- year_id_df[c(4,5,1)]
  
  year_id_raster <- rasterFromXYZ(year_id_df)
  crs(year_id_raster) <- aea.proj
  filename <- paste0('./../../Data/GPP/Ecoregion/',Ecoregion,'/MODIS_GPP/Period/1/MODIS_GPP_',i,'_1.tif')
  writeRaster(year_id_raster,filename,overwrite=TRUE)
  
}



