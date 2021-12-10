
#Author: A Felton
#This script imports 16-day summed estimates of growing season rainfall
#across the two rangeland ecoregions

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
#https://www.usna.edu/Users/oceano/pguth/md_help/html/approx_equivalents.htm
# 1/7 #want to pixel size to be 1/7 of original
# .0625*0.145
# 0.0090625*111 #concert to km. This equals about 1 km
# 1/0.145 #convert so its a factor
sgs_raster <- raster::disaggregate(sgs_raster,fact=7) #convert to ~1 km resolution

#convert back to df
sgs <- data.frame(rasterToPoints(sgs_raster))


#import daymet precipitation data ------

#https://daymet.ornl.gov
#https://cran.r-project.org/web/packages/daymetr/index.html


summed_precip_list <- list()
year_list <- list()
period_values <- c(1:15)

#set this up first
sgs.1000<-sgs[(1:50),]

#test_function <- get_rainfall_period(period_id=2)

id_values <- c(1:14)
year_values <- c(2000:2020)
year_id_list_2 <- list()

Ecoregion = 'SGS'
#Ecoregion='NMP'

#all i is doing is allowing you to eventually subset by each year (1986-2019)
for(i in year_values){
  
  aea.proj <- "+proj=longlat +datum=WGS84"
  
  # PERIOD 1
  
  test_function <- get_rainfall_period(period_id=1)
  #test_function$year <- as.numeric(test_function$year)
  #str(test_function)
  test_function <-subset(test_function,year==i)  
  
  year_id_list_2[[i]] <- test_function
  
  year_id_df <- data.frame(year_id_list_2[[i]])
  year_id_df <- year_id_df[c(4,5,1)]
  
  year_id_raster <- rasterFromXYZ(year_id_df)
  crs(year_id_raster) <- aea.proj
  filename <- paste0('./../../Data/Climate/Ecoregion/',Ecoregion,'/Precipitation/Period/1/Precip_',i,'_1.tif')
  writeRaster(year_id_raster,filename,overwrite=TRUE)
  
}
  #PERIOD 2
  
  
  test_function <- get_rainfall_period(period_id=2)
  #test_function$year <- as.numeric(test_function$year)
  #str(test_function)
  test_function <-subset(test_function,year==i)  
  
  year_id_list_2[[i]] <- test_function
  
  year_id_df <- data.frame(year_id_list_2[[i]])
  year_id_df <- year_id_df[c(4,5,1)]
  
  year_id_raster <- rasterFromXYZ(year_id_df)
  crs(year_id_raster) <- aea.proj
  filename <- paste0('./../../Data/Climate/Ecoregion/',Ecoregion,'/Precipitation/Period/2/Precip_',i,'_2.tif')
  writeRaster(year_id_raster,filename,overwrite=TRUE)
  
  
  #PERIOD 3
  
  
  
  test_function <- get_rainfall_period(period_id=3)
  #test_function$year <- as.numeric(test_function$year)
  #str(test_function)
  test_function <-subset(test_function,year==i)  
  
  year_id_list_2[[i]] <- test_function
  
  year_id_df <- data.frame(year_id_list_2[[i]])
  year_id_df <- year_id_df[c(4,5,1)]
  
  year_id_raster <- rasterFromXYZ(year_id_df)
  crs(year_id_raster) <- aea.proj
  filename <- paste0('./../../Data/Climate/Ecoregion/',Ecoregion,'/Precipitation/Period/3/Precip_',i,'_3.tif')
  writeRaster(year_id_raster,filename,overwrite=TRUE)
  
  
  #PERIOD 4
  
  
  test_function <- get_rainfall_period(period_id=4)
  #test_function$year <- as.numeric(test_function$year)
  #str(test_function)
  test_function <-subset(test_function,year==i)  
  
  year_id_list_2[[i]] <- test_function
  
  year_id_df <- data.frame(year_id_list_2[[i]])
  year_id_df <- year_id_df[c(4,5,1)]
  
  year_id_raster <- rasterFromXYZ(year_id_df)
  crs(year_id_raster) <- aea.proj
  filename <- paste0('./../../Data/Climate/Ecoregion/',Ecoregion,'/Precipitation/Period/4/Precip_',i,'_4.tif')
  writeRaster(year_id_raster,filename,overwrite=TRUE)
  
  
  #PERIOD 5
  
  test_function <- get_rainfall_period(period_id=5)
  #test_function$year <- as.numeric(test_function$year)
  #str(test_function)
  test_function <-subset(test_function,year==i)  
  
  year_id_list_2[[i]] <- test_function
  
  year_id_df <- data.frame(year_id_list_2[[i]])
  year_id_df <- year_id_df[c(4,5,1)]
  
  year_id_raster <- rasterFromXYZ(year_id_df)
  crs(year_id_raster) <- aea.proj
  filename <- paste0('./../../Data/Climate/Ecoregion/',Ecoregion,'/Precipitation/Period/5/Precip_',i,'_5.tif')
  writeRaster(year_id_raster,filename,overwrite=TRUE)
  
  
  #PERIOD 6
  
  test_function <- get_rainfall_period(period_id=6)
  #test_function$year <- as.numeric(test_function$year)
  #str(test_function)
  test_function <-subset(test_function,year==i)  
  
  year_id_list_2[[i]] <- test_function
  
  year_id_df <- data.frame(year_id_list_2[[i]])
  year_id_df <- year_id_df[c(4,5,1)]

  year_id_raster <- rasterFromXYZ(year_id_df)
  crs(year_id_raster) <- aea.proj
  filename <- paste0('./../../Data/Climate/Ecoregion/',Ecoregion,'/Precipitation/Period/6/Precip_',i,'_6.tif')
  writeRaster(year_id_raster,filename,overwrite=TRUE)
  
  
  #PERIOD 7
  
  
  test_function <- get_rainfall_period(period_id=7)
  #test_function$year <- as.numeric(test_function$year)
  #str(test_function)
  test_function <-subset(test_function,year==i)  
  
  year_id_list_2[[i]] <- test_function
  
  year_id_df <- data.frame(year_id_list_2[[i]])
  year_id_df <- year_id_df[c(4,5,1)]
  
  year_id_raster <- rasterFromXYZ(year_id_df)
  crs(year_id_raster) <- aea.proj
  filename <- paste0('./../../Data/Climate/Ecoregion/',Ecoregion,'/Precipitation/Period/7/Precip_',i,'_7.tif')
  writeRaster(year_id_raster,filename,overwrite=TRUE)
  
  
  #PERIOD 8
  
  
  test_function <- get_rainfall_period(period_id=8)
  #test_function$year <- as.numeric(test_function$year)
  #str(test_function)
  test_function <-subset(test_function,year==i)  
  
  year_id_list_2[[i]] <- test_function
  
  year_id_df <- data.frame(year_id_list_2[[i]])
  year_id_df <- year_id_df[c(4,5,1)]
  
  year_id_raster <- rasterFromXYZ(year_id_df)
  crs(year_id_raster) <- aea.proj
  filename <- paste0('./../../Data/Climate/Ecoregion/',Ecoregion,'/Precipitation/Period/8/Precip_',i,'_8.tif')
  writeRaster(year_id_raster,filename,overwrite=TRUE)
  
  
  #PERIOD 9
  
  
  test_function <- get_rainfall_period(period_id=9)
  #test_function$year <- as.numeric(test_function$year)
  #str(test_function)
  test_function <-subset(test_function,year==i)  
  
  year_id_list_2[[i]] <- test_function
  
  year_id_df <- data.frame(year_id_list_2[[i]])
  year_id_df <- year_id_df[c(4,5,1)]
  
  year_id_raster <- rasterFromXYZ(year_id_df)
  crs(year_id_raster) <- aea.proj
  filename <- paste0('./../../Data/Climate/Ecoregion/',Ecoregion,'/Precipitation/Period/9/Precip_',i,'_9.tif')
  writeRaster(year_id_raster,filename,overwrite=TRUE)
  
  
  #PERIOD 10
  
  
  test_function <- get_rainfall_period(period_id=10)
  #test_function$year <- as.numeric(test_function$year)
  #str(test_function)
  test_function <-subset(test_function,year==i)  
  
  year_id_list_2[[i]] <- test_function
  
  year_id_df <- data.frame(year_id_list_2[[i]])
  year_id_df <- year_id_df[c(4,5,1)]
  
  year_id_raster <- rasterFromXYZ(year_id_df)
  crs(year_id_raster) <- aea.proj
  filename <- paste0('./../../Data/Climate/Ecoregion/',Ecoregion,'/Precipitation/Period/10/Precip_',i,'_10.tif')
  writeRaster(year_id_raster,filename,overwrite=TRUE)
  
  
  #PERIOD 11
  
  
  test_function <- get_rainfall_period(period_id=11)
  #test_function$year <- as.numeric(test_function$year)
  #str(test_function)
  test_function <-subset(test_function,year==i)  
  
  year_id_list_2[[i]] <- test_function
  
  year_id_df <- data.frame(year_id_list_2[[i]])
  year_id_df <- year_id_df[c(4,5,1)]
  
  year_id_raster <- rasterFromXYZ(year_id_df)
  crs(year_id_raster) <- aea.proj
  filename <- paste0('./../../Data/Climate/Ecoregion/',Ecoregion,'/Precipitation/Period/11/Precip_',i,'_11.tif')
  writeRaster(year_id_raster,filename,overwrite=TRUE)
  
  
  
  #PERIOD 12
  
  
  test_function <- get_rainfall_period(period_id=12)
  #test_function$year <- as.numeric(test_function$year)
  #str(test_function)
  test_function <-subset(test_function,year==i)  
  
  year_id_list_2[[i]] <- test_function
  
  year_id_df <- data.frame(year_id_list_2[[i]])
  year_id_df <- year_id_df[c(4,5,1)]
  
  year_id_raster <- rasterFromXYZ(year_id_df)
  crs(year_id_raster) <- aea.proj
  filename <- paste0('./../../Data/Climate/Ecoregion/',Ecoregion,'/Precipitation/Period/12/Precip_',i,'_12.tif')
  writeRaster(year_id_raster,filename,overwrite=TRUE)
  
  
  #PERIOD 13
  
  
  test_function <- get_rainfall_period(period_id=13)
  #test_function$year <- as.numeric(test_function$year)
  #str(test_function)
  test_function <-subset(test_function,year==i)  
  
  year_id_list_2[[i]] <- test_function
  
  year_id_df <- data.frame(year_id_list_2[[i]])
  year_id_df <- year_id_df[c(4,5,1)]
  
  year_id_raster <- rasterFromXYZ(year_id_df)
  crs(year_id_raster) <- aea.proj
  filename <- paste0('./../../Data/Climate/Ecoregion/',Ecoregion,'/Precipitation/Period/13/Precip_',i,'_13.tif')
  writeRaster(year_id_raster,filename,overwrite=TRUE)
  
  

#PERIOD 14


test_function <- get_rainfall_period(period_id=14)
#test_function$year <- as.numeric(test_function$year)
#str(test_function)
test_function <-subset(test_function,year==i)  

year_id_list_2[[i]] <- test_function

year_id_df <- data.frame(year_id_list_2[[i]])
year_id_df <- year_id_df[c(4,5,1)]

year_id_raster <- rasterFromXYZ(year_id_df)
crs(year_id_raster) <- aea.proj
filename <- paste0('./../../Data/Climate/Ecoregion/',Ecoregion,'/Precipitation/Period/14/Precip_',i,'_14.tif')
writeRaster(year_id_raster,filename,overwrite=TRUE)

#PERIOD 14


test_function <- get_rainfall_period(period_id=15)
#test_function$year <- as.numeric(test_function$year)
#str(test_function)
test_function <-subset(test_function,year==i)  

year_id_list_2[[i]] <- test_function

year_id_df <- data.frame(year_id_list_2[[i]])
year_id_df <- year_id_df[c(4,5,1)]

year_id_raster <- rasterFromXYZ(year_id_df)
crs(year_id_raster) <- aea.proj
filename <- paste0('./../../Data/Climate/Ecoregion/',Ecoregion,'/Precipitation/Period/15/Precip_',i,'_15.tif')
writeRaster(year_id_raster,filename,overwrite=TRUE)


}


#take a look
# test.look <- raster(filename)
# plot(test.look)
