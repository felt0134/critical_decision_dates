
#this script loads in the GPP and PPT data, combines them, and performs analyses

#load GPP

# first define the ecoregion and the range of periods and years to loop through
# for processing

#Ecoregion = 'northern_mixed_prairies'
Ecoregion = 'shortgrass_steppe'
period_list <- seq(1,15,1)
period_list <- as.character(period_list) #easier when they are characters
year_list <- seq(2003,2020,1)
year_list <- as.character(year_list)


#first do the GPP import

#function to help with converting each raster into a dataframe/matrix
format_gpp_df <- function(x){
  
  #convert to raster
  raster_file <- raster(x)
  
  #extract year from the name of the raster to later add to dataframe
  year_val <- substr(names(raster_file), 5, 8)
  
  #convert to dataframe and add year and period columns
  df <- data.frame(rasterToPoints(raster_file))
  df$year <- year_val
  df$period <- gsub(paste0("GPP_",year_val,'_'),'', names(raster_file))
  colnames(df) <- c('x','y','gpp','year','period')
  
  #return formatted dataframe
  return(df)
  
  
}

#loop through each year and period combination

#list to store outputs in
gpp_list <- list()

#run the loop
for(i in period_list){

  filepath <-dir(paste0('./../../Data/GPP/Ecoregion/',Ecoregion,'/MODIS_GPP/Period/',i,'/'),full.names = T)
  test <- lapply(filepath,format_gpp_df)
  test <- data.frame(do.call('rbind',test))
  #test <- lapply(test,rasterToPoints)
  gpp_list[[i]] <- test
  
}

#convert list of dataframes to a single dataframe
gpp_df <- do.call('rbind',gpp_list)
rm(gpp_list,test)


#Now import and format precipitation data

#function to convert raster to formatted dataframe
format_ppt_df <- function(x){
  
  #convert to raster
  raster_file <- raster(x)
  
  #extract year from the name of the raster
  year_val <- substr(names(raster_file), 8, 11)
  
  #convert to dataframe and add year and period columns
  df <- data.frame(rasterToPoints(raster_file))
  df$year <- year_val
  df$period <- gsub(paste0("Precip_",year_val,'_'),'', names(raster_file))
  colnames(df) <- c('x','y','ppt','year','period')
  return(df)
  
  
}

#now loop through period and year

#create list to store values
ppt_list <- list()

#loop through each year and period
for(i in period_list){
  
  filepath <-dir(paste0('./../../Data/Climate/Ecoregion/',Ecoregion,'/Precipitation/Period/',i,'/'),full.names = T)
  test_ppt <- lapply(filepath,format_ppt_df)
  test_ppt <- data.frame(do.call('rbind',test_ppt))
  ppt_list[[i]] <- test_ppt
  
}


ppt_df <- do.call('rbind',ppt_list)
rm(ppt_list,test)
#head(ppt_df)

#merge the two dataframes by location, year, and period within each year
ppt_gpp <- merge(gpp_df,ppt_df,by=c('x','y','year','period'))
head(ppt_gpp)

#get means
mean_ppt <- aggregate(ppt~x+y+period,mean,data=ppt_gpp)
mean_gpp <- aggregate(gpp~x+y+period,mean,data=ppt_gpp)
mean_ppt_gpp <- merge(mean_ppt,mean_gpp,by=c('x','y','period'))
colnames(mean_ppt_gpp) <- c('x','y','period','ppt_mean','gpp_mean')
head(mean_ppt_gpp)

#merge means with original data frame and get annual deviations from the mean
mean_ppt_gpp_2 <- merge(mean_ppt_gpp, ppt_gpp,by=c('x','y','period'))
mean_ppt_gpp_2$ppt_dev <- mean_ppt_gpp_2$ppt_mean - mean_ppt_gpp_2$ppt
mean_ppt_gpp_2$gpp_dev <- mean_ppt_gpp_2$gpp_mean - mean_ppt_gpp_2$gpp

head(mean_ppt_gpp_2)

#plot(gpp_dev~ppt_dev,data=mean_ppt_gpp_2)

#create two lists to store slopes as raster and data frames
period_slope_list <- list()
raster_slope_list <- list()

#loop through each period to get GPP-PPT slopes (across years)
for(i in period_list){
  
  #subset to the period
  period_subset <- subset(mean_ppt_gpp_2,period==i)
  
  #get the slope between GPP and PPT deviation for that period
  period_slope <-  period_subset %>% group_by(x, y) %>%
    dplyr::do(model = lm(gpp_dev~ppt_dev, data = .)) %>%
    dplyr::mutate(coef=coef(model)[2])
  
  #convert to a dataframe
  period_slope <- data.frame(period_slope[c(1,2,4)])
  period_slope$period <- i
  
  #convert to a raster
  period_raster <- rasterFromXYZ(period_slope[c(1,2,3)])
  
  #store the outputs in the lists
  period_slope_list[[i]] <- period_slope
  raster_slope_list[[i]] <- period_raster 
  
}

#take a look
plot(raster_slope_list$'5')

#import conversion of period to day of year to map period to DOY
doy_conversion <- read.csv('./../../Data/GPP/period_day_match.csv')
head(doy_conversion)

#convert list of dataframe to a single dataframe
full_period_slope_df <- do.call('rbind',period_slope_list)
head(full_period_slope_df)
full_period_slope_df <- merge(full_period_slope_df,doy_conversion[c(2,3)],by=c('period'))

#produce an ID colum for later merging/subsetting specific coordinates
full_period_slope_df_id <- aggregate(coef ~x+y,mean,data=full_period_slope_df)
full_period_slope_df_id$id <- seq.int(nrow(full_period_slope_df_id))
full_period_slope_df <- merge(full_period_slope_df,
                              full_period_slope_df_id[c(1,2,4)],by=c('x','y'))

#merge with main slope ID


#look at maximum slopes
max_sens <- aggregate(coef~x+y,max,data=full_period_slope_df)
head(max_sens)
max_sens_2 <- merge(max_sens,full_period_slope_df,by=c('x','y','coef'))
head(max_sens_2)
#max_sens_2$period <-as.numeric(max_sens_2$period)
str(max_sens_2)
summary(max_sens_2)
plot(rasterFromXYZ(max_sens_2[c(1,2,5)]))

#max sens
max_sens_3 <- merge(max_sens_2,doy_conversion,by=c('period'))
head(max_sens_3)
plot(rasterFromXYZ(max_sens_3[c(1,2,5)]))

#run loop to get critical decision date for each pixel
id_list <- as.factor(unique(full_period_slope_df$id))
all_dates_list <- list()

for(i in id_list){

#fitting smoothing splines
full_period_slope_df_2_test <- full_period_slope_df %>%
  dplyr::filter(id==5000)
#dim(full_period_slope_df_2_test)

#head(full_period_slope_df_2_test)

#plot(coef~doy,data=full_period_slope_df_2_test)
#https://stat.ethz.ch/R-manual/R-devel/library/stats/html/smooth.spline.html
cars.spl <- with(full_period_slope_df_2_test, smooth.spline(doy, coef))
#lines(cars.spl, col = "blue")

#run model through a sequence of days
doy <- data.frame(seq(from=65,to=297,by=1))
sens_predicted <- data.frame(predict(cars.spl,doy))
colnames(sens_predicted) <- c('day','sensitivity')
#plot(sensitivity~day,data=sens_predicted)

#get day of predicted maximum sensitivity
max_sens <- max(sens_predicted$sensitivity)
max_sens <- subset(sens_predicted,sensitivity==max_sens)
max_sens$period <- 'max_sensitivity'

#get the day at which sensitivity is 25% of maximum

#abline(h=quantile_25)

quantile_25 <- quantile(sens_predicted$sensitivity,probs=0.30)
#quantile_25 <- subset(sens_predicted,sensitivity==quantile_25)

quantile_25_df <- dplyr::filter(sens_predicted, (quantile_25 - 0.005) < 
                                  sensitivity & sensitivity < (quantile_25 + 0.005))

#get the two tails of sensitivity - ending day is the critical decision date
get_starting_day <- subset(quantile_25_df,day == as.numeric(min(quantile_25_df$day)))
get_starting_day$period <- 'start'

get_ending_day <- subset(quantile_25_df,day == as.numeric(max(quantile_25_df$day)))
get_ending_day$period <- 'critical_decision_date'

#combine them
start_end_dates <- rbind(get_starting_day,get_ending_day)
all_dates <- rbind(start_end_dates,max_sens)
all_dates$id <- i

all_dates_list[[i]] <- all_dates


}


#turn into a dataframe
all_dates_df <- do.call('rbind',all_dates_list)
head(all_dates_df)

#subset to critical decision date
cdd <- subset(all_dates_df,period=='critical_decision_date')
cdd <- merge(full_period_slope_df[c(1,2,6)],cdd,by=c('id'))
cdd <- cdd[!duplicated(cdd),]
head(cdd)
plot(rasterFromXYZ(cdd[c(2,3,4)]))

#subset to day of maximum sensitivity 
max_sens <- subset(all_dates_df,period=='max_sensitivity')
max_sens <- merge(full_period_slope_df[c(1,2,6)],max_sens,by=c('id'))
max_sens <- max_sens[!duplicated(max_sens),]
head(max_sens)
plot(rasterFromXYZ(max_sens[c(2,3,4)]))
plot(day~as.numeric(y),data=max_sens)
summary(lm(day~as.numeric(y),data=max_sens))

#thoughts
# 1) Maybe split up the data into those below and those above the period of maximum 
# sensitivity and then get percentiles
# 
# 2) try to turn the dstribution into a nrmal distribution, and then use the SD
# 
# 3) use a lower threshold for sensitivity to be less rigid, or change how the threshold is
# defined.
# Also need to factional in conditional probabilities of rainfall.




#try to fit spline
#https://stat.ethz.ch/R-manual/R-devel/library/stats/html/smooth.spline.html
#https://bmcmedresmethodol.biomedcentral.com/articles/10.1186/s12874-019-0666-3
#https://medium.com/analytics-vidhya/spline-regression-in-r-960ca82aa62c
plot(coef~doy,data=full_period_slope_df_2)
full_period_slope_df_test <- full_period_slope_df_2[c(1:15)]


#try to make a gif------
library(ggthemes)
library(gganimate)


stack_periods <- stack(raster_slope_list)
d <- raster::animate(stack_periods)
saveHTML(raster::animate(stack_periods))
?saveHTML

library(tmap)
anim <- tm_shape(my_stack) + tm_raster() + tm_facets(nrow = 1, ncol = 1)


?animate


mydf <- purrr::map_dfr(
  as.list(stack_periods), 
  ~setNames(as.data.frame(as(., "SpatialPixelsDataFrame")), c('value', 'x', 'y')), 
  .id = 'Period'
)

gg <- ggplot(
  mydf, 
  aes(x = x, y = y, fill = coef)
) +
  #geom_sf(data = borders, fill = "transparent", color = "black", inherit.aes = FALSE) +
  geom_tile() +
  scale_fill_viridis_c() +
  ggthemes::theme_map() +
  transition_time(as.numeric(Period)) #+ labs(title = "Year: {frame_time}")

plot(gganim)

library(animation)
saveGIF(gganim,'period.gif')

plot_list <- list()
for(i in period_list){
  
  period_df <- subset(full_period_slope_df,period==5)
  
  gg <- ggplot(
    period_df, 
    aes(x = x, y = y, fill = value)
  ) +
    #geom_sf(data = borders, fill = "transparent", color = "black", inherit.aes = FALSE) +
    geom_tile() +
    scale_fill_viridis_c() +
    ggthemes::theme_map()
  
  
  
  
}


anim <- ggplot(mydf, aes(x = x, y = y)) +
  geom_raster(aes(fill = value)) +
  transition_manual(Period) +
  scale_fill_viridis_c(limits = c(0, max(mydf$value)))
  file_renderer(dir = ".", prefix = "gganim_plot", overwrite = FALSE)
  
  animate(anim, duration = 5, height = 2, width = 6, units = "in", res = 150)


  
  
  