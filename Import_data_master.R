

library(parallel)
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
#get_modis_gpp_period_2 <- function(period_id,start_val,end_val){
  
  summed_gpp_list <- list()
  
  for(i in 1:nrow(sgs.1000)){
    
    #test.df<-mean_production[i]
    lon.val<-sgs$x[i]
    lat.val<-sgs$y[i]
    
    # lon.val<-sgs.1000$x
    # lat.val<-sgs.1000$y
    
    #get GPP data
    site_gpp <- mt_subset(product = "MYD17A2H",
                          lat = lat.val,
                          lon =  lon.val,
                          band = 'Gpp_500m',
                          start = start_val,
                          end = end_val,
                          km_lr = 5,
                          km_ab = 5,
                          site_name = "SGS",
                          internal = TRUE,
                          progress = TRUE)
    
    #filter out bad values, get day of year, take median value for coordinate, and rescale GPP units to g/m^2
    site_gpp_2  <- site_gpp  %>%
      #filter(value <= X) %>% if there is a threshold value to filter by
      group_by(calendar_date) %>% 
      summarize(doy = as.numeric(format(as.Date(calendar_date)[1],"%j")),
                gpp_mean = median(value * as.double(scale))) %>% 
      filter(doy > 60) %>%
      filter(doy < 300)
    
    #get gpp in grams
    site_gpp_2$gpp_mean <- site_gpp_2$gpp_mean*1000
    
    #get year column
    site_gpp_2$year <- substr(site_gpp_2$calendar_date, 1, 4)
    
    #filter out years with incomplete data
    site_length = aggregate(doy~year,length,data=site_gpp_2)
    colnames(site_length) = c('year','length')
    site_gpp_2 = merge(site_gpp_2,site_length,by='year')
    site_gpp_2 = site_gpp_2 %>%
      dplyr::filter(length > 29)
    
    site_gpp_3 <- get_16_day_sums_gpp(site_gpp_2)
    site_gpp_3$period <- as.numeric(rownames(site_gpp_3))
    site_gpp_3$period = (site_gpp_3$period +1)
    
    site_gpp_3$x <- lon.val
    site_gpp_3$y <- lat.val
    site_gpp_3 <- site_gpp_3[c(3,4,1,2)]
    
    summed_gpp_list[[i]] <- site_gpp_3
    
    
  }
  
  summed_gpp_df <- do.call('rbind',summed_gpp_list)
  
  return(summed_gpp_df) 
  
}
get_modis_gpp_period_3 <- function(i){
  
  
  temp_lat <- sgs.1000[i, ] %>% pull(y)
  temp_lon <- sgs.1000[i, ] %>% pull(x)
  
  #get GPP data
  site_gpp <- mt_subset(product = "MYD17A2H",
                        lat = temp_lat,
                        lon =  temp_lon,
                        band = 'Gpp_500m',
                        start = start_date,
                        end = end_date,
                        km_lr = 5,
                        km_ab = 5,
                        site_name = Ecoregion,
                        internal = TRUE,
                        progress = TRUE)
  
  #filter out bad values, get day of year, take median value for coordinate, and rescale GPP units to g/m^2
  site_gpp_2  <- site_gpp  %>%
    #filter(value <= X) %>% if there is a threshold value to filter by
    group_by(calendar_date) %>% 
    summarize(doy = as.numeric(format(as.Date(calendar_date)[1],"%j")),
              gpp_mean = median(value * as.double(scale))) %>% 
    filter(doy > 60) %>%
    filter(doy < 300)
  
  #get gpp in grams
  site_gpp_2$gpp_mean <- site_gpp_2$gpp_mean*1000
  
  #get year column
  site_gpp_2$year <- substr(site_gpp_2$calendar_date, 1, 4)
  
  #filter out years with incomplete data
  site_length = aggregate(doy~year,length,data=site_gpp_2)
  colnames(site_length) = c('year','length')
  site_gpp_2 = merge(site_gpp_2,site_length,by='year')
  site_gpp_2 = site_gpp_2 %>%
    dplyr::filter(length > 29)
  
  site_gpp_3 <- get_16_day_sums_gpp(site_gpp_2)
  site_gpp_3$period <- as.numeric(rownames(site_gpp_3))
  site_gpp_3$period = (site_gpp_3$period +1)
  
  site_gpp_3$x <- temp_lat
  site_gpp_3$y <- temp_lon
  site_gpp_3 <- site_gpp_3[c(3,4,1,2)]
  
  
  
  return(site_gpp_3) 
  
}
source('gpp_import_script.R')

look <- raster(filename)
summary(look)
plot(look)

look.2 <- raster("./../../Data/GPP/Ecoregion/northern_mixed_prairies/MODIS_GPP/Period/8/GPP_2019_8.tif")
plot(look.2)
