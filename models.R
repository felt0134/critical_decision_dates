
# climate models
#https://jeffreyevans.github.io/spatialEco/reference/subsample.distance.html

#setup ----

period_list <- seq(1, 15, 1) #set periods
period_list <-
  as.character(period_list) #easier when they are characters
year_list <- seq(2003, 2020, 1) #set years
year_list <-
  as.character(year_list) #easier when they are characters


#import climate data -----

#now loop through period and year

Ecoregions <- c('shortgrass_steppe', 'northern_mixed_prairies')

#PRECIPITATION
store_ppt <- list()
for (j in Ecoregions) {
  #create list to store values
  ppt_list <- list()
  
  #loop through each year and period
  for (i in period_list) {
    filepath <-
      dir(
        paste0(
          './../../Data/Climate/Ecoregion/',
          j,
          '/Precipitation/Period/',
          i,
          '/'
        ),
        full.names = T
      )
    test_ppt <- lapply(filepath, format_ppt_df)
    test_ppt <- data.frame(do.call('rbind', test_ppt))
    ppt_list[[i]] <- test_ppt
    
  }
  
  #convert to dataframe
  ppt_df <- do.call('rbind', ppt_list)
  rm(ppt_list, test_ppt)
  
  
  #head(ppt_df)
  
  
  # for later use: get average total growing season PPT for each pixel
  annual_ppt <- aggregate(ppt ~ x + y + year, sum, data = ppt_df)
  annual_ppt <- aggregate(ppt ~ x + y, mean, data = annual_ppt)
  annual_ppt$ecoregion <- j
  
  store_ppt[[j]] <- annual_ppt
  
}

#head(store_ppt[1])

store_ppt_df <- list_to_df(store_ppt)
head(store_ppt_df)


#TEMPERATURE
temp_data_list <- list()
for (i in Ecoregions) {
  #if tmax or tmin, value=T, if tmean, value=F
  temp_data <- import_temp(Ecoregion = i,
                           temp = 'tmax',
                           value = T)
  temp_data_list[[i]] <- temp_data
  
  
  
}

temp_data_list_df <- list_to_df(temp_data_list)
head(temp_data_list_df)

unique(temp_data_list_df$ecoregion)

mean_annual_temp <- aggregate(temp ~ x + y, mean, data = temp_data_list_df)

#plot(rasterFromXYZ(annual_ppt))

#basic regression exploratory: maximum sensitivity -----

#import day of maximum sensitivity 

max_sens_list <- list()
for(i in Ecoregions){

max_sens <- raster(paste0('./../../Data/CDD/maximum_sensitivity/max_sens_day_',i,'.tif'))
max_sens_df <- data.frame(rasterToPoints(max_sens))
colnames(max_sens_df) <- c('x','y','doy')
max_sens_list[[i]] <- max_sens_df

}

max_sens_df_2 <- list_to_df(max_sens_list)
#head(max_sens_df_2)

#PRECIPITATION
max_sens_annual_ppt <- merge(max_sens_df_2,store_ppt_df ,by=c('x','y'))
head(max_sens_annual_ppt)
unique(max_sens_annual_ppt$ecoregion)
summary(max_sens_annual_ppt)

#plots to look at the data
ggplot(max_sens_annual_ppt,aes(ppt,color=ecoregion)) +
  geom_histogram(binwidth = 1)

ggplot(max_sens_annual_ppt,aes(doy,color=ecoregion)) +
  geom_histogram(binwidth = 1) #can see spurious (wrong) estimates at tails

ggplot(max_sens_annual_ppt,aes(x=ppt,y=doy,color=ecoregion)) +
  geom_point(size=1) +
  geom_smooth(method='lm')

#colnames(max_sens_annual_ppt) <- c('x','y','doy','ppt')

#filter out extreme high and low values. Do this than look at plots again
max_sens_annual_ppt <- max_sens_annual_ppt %>%
  dplyr::filter(doy < 297) %>%
  dplyr::filter(doy > 65)

#with mean PPT as predictor 
max_sens_ppt_lm <- lm(doy~ppt,data=max_sens_annual_ppt)
summary(max_sens_ppt_lm)

#max sens with just intercept
max_sens_intercept_lm <- lm(doy~1,data=max_sens_annual_ppt)
summary(max_sens_intercept_lm)

# with ecoregion interaction
max_sens_ppt_ecoregion_lm <- lm(doy~ppt*ecoregion,data=max_sens_annual_ppt)
summary(max_sens_ppt_ecoregion_lm)
AIC(max_sens_ppt_ecoregion_lm)

plot(doy~ppt,data=max_sens_annual_ppt)
AIC(max_sens_ppt_lm,max_sens_intercept_lm,max_sens_ppt_ecoregion_lm)
#335469.2

#look at residuals
max_sens_annual_ppt$resid <- resid(max_sens_ppt_ecoregion_lm)
head(max_sens_annual_ppt)
# residuals_max_sens_model <- rasterFromXYZ(max_sens_annual_ppt[c(1,2,6)])
# residuals_max_sens_model <- resample(residuals_max_sens_model,max_sens_doy_nmp) #need to resample
# plot(residuals_max_sens_model)

#do a variogram
library(gstat)
Vout_annual_1<-variogram(resid~1,loc=~x+y, width=.05,data=max_sens_annual_ppt) ###calculate a variogram using the data. this will take a few annualutes if the full dataset
plot(Vout_annual_1$dist,Vout_annual_1$gamma,xlab='', ylab='Semivariance',main='',cex.lab=1.5, cex.main=2, pch=19, cex=.5)
abline(v=50,col='red')
summary(Vout_annual_1)

library(spatialEco)
d <- max_sens_annual_ppt
#d <- d %>% filter(ecoregion == 'shortgrass_steppe')
coordinates(d) <- ~x+y
plot(d,pch=1,cex=.25)
#crs(d) <- "+proj=longlat +datum=WGS84"
d_subsampled <- subsample.distance(d,size=200,d=5,replacement = F,
                                           latlong = T,echo = F)

plot(d,pch=1,cex=.25)
plot(d_subsampled,pch=1,cex=.5,add=T,col='red')

dm <- spDists(d_subsampled)
diag(dm) <- NA
min(dm,na.rm=T)
row_number <- length(dm[1,])

#turn back into df and run model
d_subsampled <- as.data.frame(d_subsampled)
aggregate(ppt~ecoregion,length,data=d_subsampled)
head(d_subsampled)
max_sens_lm_subsampled <- lm(doy~ppt*ecoregion,data=d_subsampled)
summary(max_sens_lm_subsampled)
hist(d_subsampled$ppt)


#TEMPERATURE 

max_sens_annual_temp <- merge(max_sens_df_2,temp_data_list_df,by=c('x','y'))
head(max_sens_annual_temp)
unique(max_sens_annual_ppt$ecoregion)
summary(max_sens_annual_ppt)

#plots to look at the data
ggplot(max_sens_annual_temp ,aes(temp,color=ecoregion)) +
  geom_histogram(binwidth = 1)

ggplot(max_sens_annual_temp ,aes(doy,color=ecoregion)) +
  geom_histogram(binwidth = 1) #can see spurious (wrong) estimates at tails

ggplot(max_sens_annual_temp ,aes(x=temp,y=doy,color=ecoregion)) +
  geom_point(size=.1,alpha=0.1) +
  geom_smooth(method='lm')

#filter out extreme high and low values. Do this than look at plots again
max_sens_annual_temp <- max_sens_annual_temp %>%
  dplyr::filter(doy < 297) %>%
  dplyr::filter(doy > 65)

#ecoregion model
max_sens_temp_ecoregion_lm <- lm(doy~temp*ecoregion,data=max_sens_annual_temp)
summary(max_sens_temp_ecoregion_lm)

AIC(max_sens_temp_ecoregion_lm)
#mean temp:6116419
#max temp: 6106428
#min temp: 6121498

9528110 - 335469.2




#basic regression exploratory: day of 90% growth -----

#import day of 90 growth
day_90_list <- list()
for(i in Ecoregions){
  
  day_90 <- raster(paste0('./../../Data/CDD/day_of_90/day_90_',i,'.tif'))
  day_90<- data.frame(rasterToPoints(day_90))
  colnames(day_90) <- c('x','y','doy')
  day_90_list[[i]] <- day_90
  
}

day_90_2 <- list_to_df(day_90_list)
head(day_90_2)


day_90_2_annual_ppt <- merge(day_90_2,store_ppt_df ,by=c('x','y'))
head(day_90_2_annual_ppt)
unique(day_90_2_annual_ppt$ecoregion)
summary(day_90_2_annual_ppt)


#make plots
#plots to look at the data
ggplot(day_90_2_annual_ppt,aes(ppt,color=ecoregion)) +
  geom_histogram(binwidth = 1)

ggplot(day_90_2_annual_ppt,aes(doy,color=ecoregion)) +
  geom_histogram(binwidth = 1) #can see spurious (wrong) estimates at tails

ggplot(day_90_2_annual_ppt,aes(x=ppt,y=doy,color=ecoregion)) +
  geom_point(size=1) +
  geom_smooth(method='lm')

#filter out extreme high and low values. Do this than look at plots again
day_90_2_annual_ppt <- day_90_2_annual_ppt%>%
  dplyr::filter(doy < 297) %>%
  dplyr::filter(doy > 65)


#model day of 90% growth as a function of precip and ecoregion

#intercept model
day_90_intercept <- lm(doy~1,day_90_2_annual_ppt)
summary(day_90_intercept)

#ppt model
day_90_ppt_lm <- lm(doy~ppt,day_90_2_annual_ppt)
summary(day_90_ppt_lm)

#ecoregion interaction
day_90_ppt_ecoregion_lm <- lm(doy~ppt*ecoregion,day_90_2_annual_ppt)
summary(day_90_ppt_ecoregion_lm)

AIC(day_90_ppt_ecoregion_lm,day_90_ppt_lm,day_90_intercept)

#now look at how drought impacts day90


day_90_drought_list <- list()
for(i in Ecoregions){
  
  day_90_drought <- raster(paste0('./../../Data/CDD/day_of_90/day_90_drought',i,'.tif'))
  day_90_drought <- data.frame(rasterToPoints(day_90_drought))
  colnames(day_90_drought) <- c('x','y','doy_drought')
  day_90_drought_list[[i]] <- day_90_drought
  
}

day_90_drought_2 <- list_to_df(day_90_drought_list)
head(day_90_drought_2)


day_90_drought_2_annual_ppt <- merge(day_90_drought_2,store_ppt_df ,by=c('x','y'))
head(day_90_drought_2_annual_ppt)

#merge this with average day90
day_90_drought_2_annual_ppt <- merge(day_90_drought_2_annual_ppt,day_90_2_annual_ppt,by=c('x','y',
                                                                                          'ppt','ecoregion'))
head(day_90_drought_2_annual_ppt)


day_90_drought_2_annual_ppt$drought_impact <- day_90_drought_2_annual_ppt$doy_drought -
  day_90_drought_2_annual_ppt$doy

aggregate(drought_impact~ecoregion,median,data=day_90_drought_2_annual_ppt)

head(day_90_drought_2_annual_ppt)

#plot it out
ggplot(day_90_drought_2_annual_ppt,aes(ppt,color=ecoregion)) +
  geom_histogram(binwidth = 1)

ggplot(day_90_drought_2_annual_ppt,aes(drought_impact,color=ecoregion)) +
  geom_histogram(binwidth = 1) #can see spurious (wrong) estimates at tails

ggplot(day_90_drought_2_annual_ppt,aes(x=ppt,y=drought_impact,color=ecoregion)) +
  geom_point(size=1) +
  geom_smooth(method='lm')

#models

#intercept model
day_90_drought_intercept_lm <- lm(drought_impact~1,data=day_90_drought_2_annual_ppt)
summary(day_90_drought_intercept_lm)

#ppt model
day_90_drought_ppt_lm <- lm(drought_impact~ppt,data=day_90_drought_2_annual_ppt)
summary(day_90_drought_ppt_lm)

#ecoregion model
day_90_drought_ppt_ecoregion_lm <- lm(drought_impact~ppt*ecoregion,data=day_90_drought_2_annual_ppt)
summary(day_90_drought_ppt_ecoregion_lm)

AIC(day_90_drought_intercept_lm,day_90_drought_ppt_lm,day_90_drought_ppt_ecoregion_lm)



