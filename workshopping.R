
#https://cran.r-project.org/web/packages/MODISTools/vignettes/modistools-vignette.html

#try accessing productivity data -----

#install.packages('MODISTools')
library(MODISTools)
mt_products()
get_modis_gpp_period_2 <- function(period_id,start_val,end_val){
  
  summed_gpp_list <- list()
  
  for(i in 1:nrow(sgs.1000)){
    
    #test.df<-mean_production[i]
    # lon.val<-sgs$x[i]
    # lat.val<-sgs$y[i]
    
    lon.val<-sgs.1000$x
    lat.val<-sgs.1000$y
    
    #get GPP data
    site_gpp <- mt_subset(product = "MYD17A2H",
                          lat = lat.val,
                          lon =  lon.val,
                          band = 'Gpp_500m',
                          start = start_date,
                          end = end_date,
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

test <- get_modis_gpp_period_2(period_id = 1, start_val = start_date,end_val = end_date)

#try to combine precip and GPP ------


#get precip
summed_precip_list <- list()
for(i in 1:nrow(sgs.1000)){
  
  #test.df<-mean_production[i]
  lon.val<-sgs$x[i]
  lat.val<-sgs$y[i]
  
  #?download_daymet
  test<-download_daymet(site = "Daymet", lat = lat.val, lon = lon.val,
                        start = 2000, end = 2020,
                        path = tempdir(), internal = TRUE, silent = FALSE, force = FALSE,
                        simplify = FALSE)
  
  #end to most recent year
  #end = as.numeric(format(Sys.time(), "%Y")) - 2
  
  test<-test$data
  
  #March  to October 
  test<- test %>%
    dplyr::filter(yday > 57) %>%
    dplyr::filter(yday < 297) 
  
  test$x<-sgs$x[i]
  test$y<-sgs$y[i]
  
  # summary(test)
  # head(test)
  
  #subset to precip
  test<-test[c(1,2,4,10,11)]
  #summed_precip <- get_16_day_sums(test)
  
  
  #get 16-day sums of precip for each year for the given pixel
  year_test <- unique(test$year)
  
  # 
  for(j in year_test){
    
    #get 16-day sums
    test.year <- subset(test,year==j)
    summed_precip <- get_16_day_sums(test.year)
    
    #add period ID
    summed_precip$period <- rownames(summed_precip)
    summed_precip$period <- as.numeric(summed_precip$period) + 1
    
    #add in year and coordinate columns
    summed_precip$year <- j
    summed_precip$x<-sgs$x[i]
    summed_precip$y<-sgs$y[i]
    
    summed_precip_list[[j]] <- summed_precip
    
    
  }
  
  }

# }
#   

summed_precip_df <- do.call('rbind',summed_precip_list)
head(summed_precip_df)
unique(summed_precip_df$period)

#merge with gpp data
summed_precip_gpp <- merge(site_gpp_3_df,summed_precip_df,by=c('period','year'))

#get means
mean_precip_period <- aggregate(prcp..mm.day.~period,mean,data=summed_precip_gpp)
mean_gpp_period <- aggregate(gpp_mean~period,mean,data=summed_precip_gpp)

#merge the means
summed_precip_gpp <- merge(summed_precip_gpp,mean_precip_period ,by=c('period'))
summed_precip_gpp <- merge(summed_precip_gpp,mean_gpp_period ,by=c('period'))

#get deviations
summed_precip_gpp$gpp_dev <- summed_precip_gpp$gpp_mean.x - summed_precip_gpp$gpp_mean.y
summed_precip_gpp$precip_dev <- summed_precip_gpp$prcp..mm.day..x - summed_precip_gpp$prcp..mm.day..y

summed_precip_gpp_ag <- aggregate(gpp_dev~precip_dev+period,mean,data=summed_precip_gpp)

str(summed_precip_gpp)

ggplot(summed_precip_gpp,aes(precip_dev,gpp_dev)) +
  facet_wrap(~period,scales = 'free') +
  geom_point()

#get slopes for each period
summed_precip_gpp_slopes <- summed_precip_gpp %>% group_by(period) %>%
dplyr::do(model = lm(gpp_dev~precip_dev, data = .)) %>%
dplyr::mutate(coef=coef(model)[2]) %>%
dplyr::mutate(r2=summary(model)$r.squared) 

summed_precip_gpp_slopes <- data.frame(summed_precip_gpp_slopes)
summed_precip_gpp_slopes <- summed_precip_gpp_slopes[c(1,3,4)]
summary(summed_precip_gpp_slopes)

summed_precip_gpp_slopes$period = as.numeric(rownames(summed_precip_gpp_slopes))

summary(summed_precip_gpp_slopes)
ggplot(summed_precip_gpp_slopes,aes(period,coef)) +
  geom_point() +
  geom_smooth(span = 0.3, method = "loess") 

#loess regression
str(summed_precip_gpp_slopes)
loessMod_test1 <- loess(coef ~ period, data=summed_precip_gpp_slopes, span=0.50)
smoothed50 <- predict(loessMod_test1) 
#period 4 and 5 maximum sensitivity = ~0.23

plot(summed_precip_gpp_slopes$coef,x=summed_precip_gpp_slopes$period, type="l")
lines(smoothed50, x=summed_precip_gpp_slopes$period, col="red")

# doy_toy = data.frame(period=c(1:20))
# predict(loessMod_test1,doy_toy)


#spring
summed_precip_gpp_spring  = summed_precip_gpp %>%
  dplyr::filter(period < 6)
plot(gpp_dev~precip_dev,data=summed_precip_gpp_spring)
summary(lm(gpp_dev~precip_dev,data=summed_precip_gpp_spring))

#summer
summed_precip_gpp_summer  = summed_precip_gpp %>%
  dplyr::filter(period < 11) %>%
  dplyr::filter(period > 5)

plot(gpp_dev~precip_dev,data=summed_precip_gpp_summer)
summary(lm(gpp_dev~precip_dev,data=summed_precip_gpp_summer))

#fall
summed_precip_gpp_fall  = summed_precip_gpp %>%
  dplyr::filter(period > 11) 

plot(gpp_dev~precip_dev,data=summed_precip_gpp_fall)
summary(lm(gpp_dev~precip_dev,data=summed_precip_gpp_fall))



#extra------

#   year_list[[i]] <- summed_precip_df
#   
#   summed_precip_df_2 <- data.frame(do.call('rbind',year_list))
#   period_df <- subset(summed_precip_df_2,period==period_id)
#   # #period_year_df <- subset(period_df,year==year_id)
#   # 
#   # 
#   
#   period_list<-period_df
# }







# daymet----


rangeland_npp_covariates <- readRDS('./../../Data/Herbaceous_NPP_1986_2015_V2.rds')
head(rangeland_npp_covariates)
mean_production<-aggregate(npp~ x + y + region,mean,data=rangeland_npp_covariates)
sgs<-subset(mean_production,region==c('shortgrass_steppe'))
head(sgs)
sgs_raster <- rasterFromXYZ(sgs[c(1,2,4)])
plot(sgs_raster)


sgs.1000<-sgs[(1),]

for(i in 1:nrow(sgs.1000)){
  
  #test.df<-mean_production[i]
  lon.val<-sgs$x[i]
  lat.val<-sgs$y[i]
  
  #?download_daymet
  test<-download_daymet(site = "Daymet", lat = sgs.1000$y, lon = sgs.1000$x,
                        start = 2005, end = 2006,
                        path = tempdir(), internal = TRUE, silent = FALSE, force = FALSE,
                        simplify = FALSE)
  
  #end to most recent year
  #end = as.numeric(format(Sys.time(), "%Y")) - 2
  
  test<-test$data
  
  #Feb x to October X
  test<- test %>%
    dplyr::filter(yday > 57) %>%
    dplyr::filter(yday < 297) 
  
  test$x<-sgs$x[i]
  test$y<-sgs$y[i]
  
  # summary(test)
  # head(test)
  
  #subset to precip
  test<-test[c(1,2,4,10,11)]

#increase pixel resolution
#check math
# 1/7 #want to pixel size to be 1/7 of original
# .0625*0.145
# 0.0090625*111 #concert to km. This equals about 1 km
# 1/0.145 #convert so its a factor
sgs_raster <- raster::disaggregate(sgs_raster,fact=6.9)
plot(sgs_raster_2)


df <- download_daymet(site = "Oak Ridge National Laboratories",
                      lat = 36.0133,
                      lon = -84.2625,
                      start = 2000,
                      end = 2010,
                      internal = TRUE,
                      simplify = TRUE) # return tidy data !!


# combining gpp and ppt ------


gpp_list <- list()
ppt_list <- list()

for(i in period_list){
  
  
  
  for(j in year_list){
    
    filename_GPP <- paste0('./../../Data/GPP/Ecoregion/',Ecoregion,'/MODIS_GPP/Period/',i,'/GPP_',j,'_',i,'.tif')
    gpp_raster <- raster(filename_GPP)
    gpp_df <- data.frame(rasterToPoints(gpp_raster))
    gpp_df$year <- j
    gpp_df$period <- i
    colnames(gpp_df) <- c('x','y','gpp','year','period')
    
    filename_PPT <- paste0('./../../Data/Climate/Ecoregion/',Ecoregion,'/Precipitation/Period/',i,'/Precip_',j,'_',i,'.tif')
    ppt_raster <- raster(filename_PPT)
    ppt_df <- data.frame(rasterToPoints(ppt_raster))
    ppt_df$year <- j
    ppt_df$period <- i
    colnames(ppt_df) <- c('x','y','ppt','year','period')
    
    # gpp_list[[j]] <- gpp_df
    # ppt_list[[j]] <- ppt_df
    
  }
  
  gpp_list[[j]] <- gpp_df
  ppt_list[[j]] <- ppt_df
  
}


gpp_list_df <- do.call('rbind',gpp_list)
ppt_list_df <- do.call('rbind',ppt_list)

  
#getting day of 90% productivity across all years ----



#get data and track progress
library(future.apply)
library(progressr)

#get typical day by which 90% of growth has occurred
with_progress({
  p <- progressor(along = id_list)
  gpp_90_list <- future_lapply(id_list, function(i) {
    Sys.sleep(0.1)
    p(sprintf("i=%g", i))
    get_90_gpp(i)
  })
})

gpp_90_df <- do.call('rbind',gpp_90_list)
gpp_90_df$ecoregion <- Ecoregion

#get typical day by which 90% of growth has occurred during years of drought
with_progress({
  p <- progressor(along = id_list)
  gpp_90_drought_list <- future_lapply(id_list, function(i) {
    Sys.sleep(0.1)
    p(sprintf("i=%g", i))
    get_90_gpp_drought(i)
  })
})

gpp_90_drought_df <- do.call('rbind',gpp_90_drought_list)
gpp_90_drought_df$ecoregion <- Ecoregion


#now we have both, so combine them into one
gpp_90_drought_df_2 <- merge(gpp_90_df,gpp_90_drought_df)

filename <- paste0('./../../Data/CDD/day_of_90/day_90_',Ecoregion,'.csv')
write.csv(gpp_90_drought_df_2,filename)
  



# temperature import daymet -----

rangeland_npp_covariates <- readRDS('./../../Data/Herbaceous_NPP_1986_2015_V2.rds')
head(rangeland_npp_covariates)
mean_production<-aggregate(npp~ x + y + region,mean,data=rangeland_npp_covariates)
sgs<-subset(mean_production,region==c('shortgrass_steppe'))


sgs.1000<-sgs[(1),]
year_value

get_daymet_temp <- function(i){
  
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
  test_tmax<-test[c(1,2,7)]
  test_tmin<-test[c(1,2,8)]
  
  #get 16-day sums of precip for each year for the given pixel
  #year_test <- unique(test$year)
  
  mean_tmax <- get_16_day_averages_temp(test_tmax)
  colnames(mean_tmax) <- 'tmax_c'
  mean_tmin <- get_16_day_averages_temp(test_tmin)
  colnames(mean_tmin) <- 'tmin_c'
  
  mean_t <- cbind(mean_tmax,mean_tmin)
  
  mean_t$tmean_c <- (mean_t$tmax_c + mean_t$tmin_c)/2
  
  mean_t <- summarise_all(mean_t,mean)
  
  #add in year and coordinate columns
  mean_t$year <- year_value
  mean_t$x<-temp_lon
  mean_t$y<-temp_lat
  
  return(mean_t)
  
}



# look a temperature trends ------



#list to store outputs in
gpp_list <- list()
years <- seq(2003,2020,1)
Ecoregion = c('shortgrass_steppe')
#run the loop


import_temp <- function(Ecoregion,temp,max_min=T){
  
  #i = 2003
  Ecoregion = 'shortgrass_steppe'
  temp='tmean'
  
  filepath <-dir(paste0('./../../Data/Climate/Ecoregion/',Ecoregion,'/Temperature/',temp,'/'),
                 full.names = T)
  test <- lapply(filepath,format_temp_df,max_min=max_min)
  test <- list_to_df(test)
  test$ecoregion <- Ecoregion
  test$temp_var <- temp
  
  return(test)
  
  
}

test_temp <- import_temp(Ecoregion='shortgrass_steppe',temp='tmean',max_min='F')
head(test_temp)

?lapply
str(test)
test$year <- as.numeric(as.character(test$year))
raster_file <- raster(filepath[1])

#extract year from the name of the raster to later add to dataframe
year_val <- substr(names(raster_file), 6, 9)

#convert to dataframe and add year and period columns
df <- data.frame(rasterToPoints(raster_file))
df$year <- year_val
colnames(df) <- c('x','y','temp','year')


temp_mean_df <- aggregate(temp~year,mean,data=test)
plot(temp~year,data=temp_mean_df)

temp_slope <- test %>%
  group_by(x,y) %>%
  dplyr::do(model = lm(temp~year, data = .)) %>%
  dplyr::mutate(coef=coef(model)[2])

head(temp_slope)

temp_slope <- data.frame(temp_slope[c(1,2,4)])

head(temp_slope)

plot(rasterFromXYZ(temp_slope))

hist(temp_slope$coef)
median(temp_slope$coef)
#0.010 for





