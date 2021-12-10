# Project Functions

#http://uop.whoi.edu/UOPinstruments/frodo/aer/julian-day-table.html

# Get 16 day sums for precip and GPP ------


get_16_day_sums <- function(x){
  
  #third column (precip) is specifically tailored to the dataset in the loop
  fac <- (seq_len(nrow(x[c(3)]))-1) %/% 16
  summed_precip <- data.frame(apply(x[c(3)], 2, function(v) tapply(v, fac, sum)))
  
  return(summed_precip)
  
}


get_16_day_sums_gpp <- function(x){
  
  #third column (precip) is specifically tailored to the dataset in the loop
  fac <- (seq_len(nrow(x[c(4)]))-1) %/% 2
  summed_gpp <- data.frame(apply(x[c(4)], 2, function(v) tapply(v, fac, sum)))
  
  return(summed_gpp)
  
}

#-------------------------------------------------------------------------------
# import daymet precipitation data -----

get_rainfall_period_2 <- function(year_start,year_end){
  
  summed_precip_list <- list()
  
  for(i in 1:nrow(sgs.1000)){
    
    #test.df<-mean_production[i]
    lon.val<-sgs$x[i]
    lat.val<-sgs$y[i]
    
    #?download_daymet
    test<-download_daymet(site = "Daymet", lat = lat.val, lon = lon.val,
                          start = year_start, end = year_end,
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
    
    #get 16-day sums of precip for each year for the given pixel
    #year_test <- unique(test$year)
    
      summed_precip <- get_16_day_sums(test)
      
      #add period ID
      summed_precip$period <- rownames(summed_precip)
      summed_precip$period <- as.numeric(summed_precip$period) + 1
      
      #add in year and coordinate columns
      summed_precip$year <- year_start
      summed_precip$x<-sgs$x[i]
      summed_precip$y<-sgs$y[i]
      
      summed_precip_list[[i]] <- summed_precip
      
      
    }
    
    summed_precip_df <- do.call('rbind',summed_precip_list)
    #year_list[[i]] <- summed_precip_df
    
    #summed_precip_df_2 <- data.frame(do.call('rbind',year_list))
    #period_df <- subset(summed_precip_df_2,period==period_id)
    # #period_year_df <- subset(period_df,year==year_id)
    # 
    # 
    
    

  return(summed_precip_df)
  
  
}


get_rainfall_period <- function(period_id,year_start,year_end){
  
  summed_precip_list <- list()
  
  for(i in 1:nrow(sgs.1000)){
    
    #test.df<-mean_production[i]
    lon.val<-sgs$x[i]
    lat.val<-sgs$y[i]
    
    #?download_daymet
    test<-download_daymet(site = "Daymet", lat = lat.val, lon = lon.val,
                          start = year_start, end = year_end,
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
    
    #get 16-day sums of precip for each year for the given pixel
    year_test <- unique(test$year)
    year_list <- list()
    
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
    
    summed_precip_df <- do.call('rbind',summed_precip_list)
    year_list[[i]] <- summed_precip_df
    
    summed_precip_df_2 <- data.frame(do.call('rbind',year_list))
    period_df <- subset(summed_precip_df_2,period==period_id)
    # #period_year_df <- subset(period_df,year==year_id)
    # 
    # 
    
    
  }
  
  return(period_df)
  
  
}



#-------------------------------------------------------------------------------
# import MODIS GPP data ------

get_modis_gpp_period <- function(period_id,start_val,end_val){
  
  for(i in 1:nrow(sgs.1000)){
    
    #test.df<-mean_production[i]
    lon.val<-sgs$x[i]
    lat.val<-sgs$y[i]
    
    #get GPP data
    site_gpp <- mt_subset(product = "MYD17A2H",
                          lat = lat.val,
                          lon =  lon.val,
                          band = 'Gpp_500m',
                          start = star_val,
                          end = end_val,
                          km_lr = 5,
                          km_ab = 5,
                          site_name = "SGS",
                          internal = TRUE,
                          progress = TRUE)
    # head(site_gpp)
    # unique(site_gpp$pixel)  
    
    # # create a plot of the data - accounting for the multiplier (scale) component
    # site_gpp <- site_gpp %>%
    #   filter(value <= 100,
    #          lc %in% c("1","5")) %>% # retain everything but fill values
    #   mutate(lc = ifelse(lc == 1, "ENF","DBF")) %>%
    #   group_by(lc, calendar_date) %>% # group by lc and date
    #   summarize(doy = as.numeric(format(as.Date(calendar_date)[1],"%j")),
    #             lai_mean = median(value * as.double(scale)))
    
    #filter out bad values, get day of year, take median value for coordinate, and rescale data
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
    
    head(site_gpp_2)
    unique(site_gpp_2$year)
    
    #create a period column to merge with precip. Run this once using any year.
    # doy.vals <- unique(site_gpp_2$doy)
    # period.vals <- c(1:30)
    # period.df <- data.frame(doy.vals,period.vals)
    # colnames(period.df) <- c('doy','period')
    # head(period.df)
    # site_gpp_coversion <- merge(site_gpp_2,period.df,by=c('doy'))
    # site_gpp_coversion$period = site_gpp_coversion$period/2
    # site_gpp_coversion = site_gpp_coversion[c(1,6)]
    # write.csv(site_gpp_coversion,"./../../Data/GPP/period_day_match.csv")
    #(297-57)/16
    
    #loop through to get cumulative 16-day GPP by period
    year_gpp <- unique(site_gpp_2$year)
    year_gpp_list <- list()
    
    for(i in year_gpp){
      
      site_gpp_3 = subset(site_gpp_2,year==i)
      site_gpp_3 <- get_16_day_sums_gpp(site_gpp_3)
      site_gpp_3$period <- as.numeric(rownames(site_gpp_3))
      site_gpp_3$period = (site_gpp_3$period +1)
      site_gpp_3$year = i
      year_gpp_list[[i]] <- site_gpp_3
      
    }
    
    site_gpp_3_df <- data.frame(do.call('rbind',year_gpp_list))
    #str(site_gpp_3_df)
    
    period_df <- subset(site_gpp_3_df,period==period_id)
    period_df$x <- lon.val
    period_df$y <- lat.val
    
  }
  
  return(period_df) 
  
}
get_modis_gpp_period_2 <- function(period_id,start_val,end_val){
  
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

#-------------------------------------------------------------------------------
# load in precipitation (and potentially production) data for all years for a period -----

get_period_by_year <- function(p,ecoregion){
  
  #values
  #period_list <- c(1:2)
  Ecoregion = ecoregion
  
  #years = file value + 1985
  file_values <- c(1:34)
  
  #ecoregion if else contingency
  if(ecoregion=='SGS'){
    
    df = sgs.1000
    
  }else{df=xxx}
  
  #store data in this list
  store_period_data <- list()
  
  for(i in 1:nrow(df[1,])){
    
    #for(p in period_list){
    
    #p = 1
    Ecoregion = 'SGS'
    
    #period_list <- c(1:14)
    #load directory for a given period
    file_path_period <- paste0('./../../Data/Climate/Ecoregion/',Ecoregion,'/Precipitation/Period/',p)
    dir_period  <- dir(file_path_period, full.names = T)
    
    for(y in file_values){
      
      #y = 1
      #import df
      period_data <- raster(dir_period[y])
      period_data <-data.frame(rasterToPoints(period_data))
      
      period_data <- merge(period_data,df[1,],by=c('x','y'))
      period_data$period <- p
      period_data$year <- y + 2000 #will vary depending on productivity data used
      
      colnames(period_data) <- c('x','y','Precip','region','npp','period','year')
      
      #outputs all years for a period
      store_period_data[[y]] <- period_data
      
      
      
    }
    
    #outputs all periods for a given year
    #store_period_data_2[[p]] <- period_data
    
  }
  
  period_df <- do.call("rbind",store_period_data)
  period_df$ppt_dev <- period_df$Precip - mean(period_df$Precip)
  
  return(period_df)
  
  #store_period_data[[]] <- period_data
  
}
#-------------------------------------------------------------------------------