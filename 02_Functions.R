# Project Functions

#http://uop.whoi.edu/UOPinstruments/frodo/aer/julian-day-table.html

# Get 16 day sums for precip and GPP ------


get_16_day_sums <- function(x) {
  #third column (precip) is specifically tailored to the dataset in the loop
  fac <- (seq_len(nrow(x[c(3)])) - 1) %/% 16
  summed_precip <-
    data.frame(apply(x[c(3)], 2, function(v)
      tapply(v, fac, sum)))
  
  return(summed_precip)
  
}


get_16_day_sums_gpp <- function(x) {
  #third column (precip) is specifically tailored to the dataset in the loop
  fac <- (seq_len(nrow(x[c(4)])) - 1) %/% 2
  summed_gpp <-
    data.frame(apply(x[c(4)], 2, function(v)
      tapply(v, fac, sum)))
  
  return(summed_gpp)
  
}



get_16_day_averages_temp <- function(x) {
  #third column (precip) is specifically tailored to the dataset in the loop
  fac <- (seq_len(nrow(x[c(3)])) - 1) %/% 16
  mean_temp <-
    data.frame(apply(x[c(3)], 2, function(v)
      tapply(v, fac, mean)))
  
  return(mean_temp)
  
}

#-------------------------------------------------------------------------------
# import daymet precipitation data -----

#use this one for full import
get_daymet <- function(i) {
  temp_lat <- sgs.1000[i,] %>% pull(y)
  temp_lon <- sgs.1000[i,] %>% pull(x)
  
  
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
  
  test <- test %>%
    dplyr::filter(yday > 57) %>%
    dplyr::filter(yday < 297)
  
  
  # summary(test)
  # head(test)
  
  #subset to precip
  test <- test[c(1, 2, 4)]
  
  #get 16-day sums of precip for each year for the given pixel
  #year_test <- unique(test$year)
  
  summed_precip <- get_16_day_sums(test)
  
  #add period ID
  summed_precip$period <- rownames(summed_precip)
  summed_precip$period <- as.numeric(summed_precip$period) + 1
  
  #add in year and coordinate columns
  summed_precip$year <- year_value
  summed_precip$x <- temp_lon
  summed_precip$y <- temp_lat
  
  return(summed_precip)
  
}



#-------------------------------------------------------------------------------
# import MODIS GPP data ------

#use this one for full import
get_modis_gpp_period_3 <- function(i) {
  temp_lat <- sgs.1000[i,] %>% pull(y)
  temp_lon <- sgs.1000[i,] %>% pull(x)
  
  #get GPP data
  site_gpp <- mt_subset(
    product = "MYD17A2H",
    lat = temp_lat,
    lon =  temp_lon,
    band = 'Gpp_500m',
    start = start_date,
    end = end_date,
    km_lr = 5,
    km_ab = 5,
    site_name = Ecoregion,
    internal = TRUE,
    progress = TRUE
  )
  
  #filter out bad values, get day of year, take median value for coordinate, and rescale GPP units to g/m^2
  site_gpp_2  <- site_gpp  %>%
    #filter(value <= X) %>% if there is a threshold value to filter by
    group_by(calendar_date) %>%
    summarize(doy = as.numeric(format(as.Date(calendar_date)[1], "%j")),
              gpp_mean = median(value * as.double(scale))) %>%
    filter(doy > 60) %>%
    filter(doy < 300)
  
  #get gpp in grams
  site_gpp_2$gpp_mean <- site_gpp_2$gpp_mean * 1000
  
  #get year column
  site_gpp_2$year <- substr(site_gpp_2$calendar_date, 1, 4)
  
  #filter out years with incomplete data
  site_length = aggregate(doy ~ year, length, data = site_gpp_2)
  colnames(site_length) = c('year', 'length')
  site_gpp_2 = merge(site_gpp_2, site_length, by = 'year')
  site_gpp_2 = site_gpp_2 %>%
    dplyr::filter(length > 29)
  
  site_gpp_3 <- get_16_day_sums_gpp(site_gpp_2)
  site_gpp_3$period <- as.numeric(rownames(site_gpp_3))
  site_gpp_3$period = (site_gpp_3$period + 1)
  
  site_gpp_3$x <- temp_lat
  site_gpp_3$y <- temp_lon
  site_gpp_3 <- site_gpp_3[c(3, 4, 1, 2)]
  
  
  
  return(site_gpp_3)
  
}

#-------------------------------------------------------------------------------
# function to help with converting each GPP raster into a dataframe -----

format_gpp_df <- function(x) {
  #convert to raster
  raster_file <- raster(x)
  
  #extract year from the name of the raster to later add to dataframe
  year_val <- substr(names(raster_file), 5, 8)
  
  #convert to dataframe and add year and period columns
  df <- data.frame(rasterToPoints(raster_file))
  df$year <- year_val
  df$period <-
    gsub(paste0("GPP_", year_val, '_'), '', names(raster_file))
  colnames(df) <- c('x', 'y', 'gpp', 'year', 'period')
  
  #return formatted dataframe
  return(df)
  
  
}


#-------------------------------------------------------------------------------
# function to help with converting each PPT raster into a dataframe ----

format_ppt_df <- function(x) {
  #convert to raster
  raster_file <- raster(x)
  
  #extract year from the name of the raster
  year_val <- substr(names(raster_file), 8, 11)
  
  #convert to dataframe and add year and period columns
  df <- data.frame(rasterToPoints(raster_file))
  df$year <- year_val
  df$period <-
    gsub(paste0("Precip_", year_val, '_'), '', names(raster_file))
  colnames(df) <- c('x', 'y', 'ppt', 'year', 'period')
  return(df)
  
}


#-------------------------------------------------------------------------------
# calculate day of 90% growth (cumulative GPP) for all years ------


get_90_gpp <- function(i) {
  #subset to a given pixel
  growth_id <- gpp_df %>%
    dplyr::filter(id_value == i)
  
  x <- unique(growth_id %>% pull(x))
  y <- unique(growth_id %>% pull(y))
  
  growth_id <- aggregate(gpp ~ doy, mean, data = growth_id)
  
  #for that pixel, get cumulative GPP throughout the year
  growth_id_cmulative <-
    data.frame(growth_id, gpp_2 = cumsum(growth_id$gpp))
  growth_id_cmulative$gpp_3 <-
    100 * (growth_id_cmulative$gpp_2 / max(growth_id_cmulative$gpp_2))
  
  rm(growth_id)
  
  #create spline model of growth curve
  gpp.doy.spl <-
    with(growth_id_cmulative, smooth.spline(doy, gpp_3))
  #lines(gpp.doy.spl, col = "blue")
  
  rm(growth_id_cmulative)
  
  #run model through a sequence of days
  doy <- data.frame(seq(from = 65, to = 297, by = 1))
  gpp_predicted <- data.frame(predict(gpp.doy.spl, doy))
  colnames(gpp_predicted) <- c('day', 'gpp_perc')
  
  #get day of year where roughly 90% of cumulative growth has occurred and turn into dataframe
  gpp_predicted_90 <- gpp_predicted %>%
    dplyr::filter(gpp_perc < 90.1)
  
  rm(gpp_predicted)
  
  doy_90 <-
    max(gpp_predicted_90$day) #this is a rough approximation
  
  doy_90_df <- data.frame(doy_90)
  colnames(doy_90_df) <- c('doy_90')
  doy_90_df$x <- x
  doy_90_df$y <- y
  
  doy_90_df <- doy_90_df[c(2, 3, 1)]
  
  return(doy_90_df)
  
  
  
}

#-------------------------------------------------------------------------------
# calculate day of 90% of growth for drought years ------


get_90_gpp_drought <- function(i) {
  ppt_id <- subset(ppt_gpp, id_value == i)
  gpp_id <- subset(gpp_df, id_value == i)
  
  ppt_id <- aggregate(ppt ~ x + y + id_value + year, sum, data = ppt_id)
  
  x <- unique(ppt_id %>% pull(x))
  y <- unique(ppt_id %>% pull(y))
  
  #identify the quantile of interest
  quantile_25 <- quantile(ppt_id$ppt, probs = 0.25)
  
  #subset to years below this value
  ppt_id  <- ppt_id %>%
    filter(ppt < quantile_25)
  
  ppt_id  <- merge(ppt_id, gpp_id, by = c('x', 'y', 'id_value', 'year'))
  
  ppt_id  <- aggregate(gpp ~ doy, mean, data = ppt_id)
  
  #for that pixel, get cumulative GPP throughout the year
  ppt_id <- data.frame(ppt_id, gpp_2 = cumsum(ppt_id$gpp))
  ppt_id$gpp_3 <- 100 * (ppt_id$gpp_2 / max(ppt_id$gpp_2))
  
  #reduce data size by selecting specific columns
  ppt_id <- ppt_id %>%
    select(c('doy', 'gpp_3'))
  
  #create spline model of growth curve
  gpp.doy.drought.spl <- with(ppt_id, smooth.spline(doy, gpp_3))
  #lines(gpp.doy.spl, col = "blue")
  
  #run model through a sequence of days
  doy <- data.frame(seq(from = 65, to = 297, by = 1))
  gpp_drought_predicted <-
    data.frame(predict(gpp.doy.drought.spl, doy))
  colnames(gpp_drought_predicted) <- c('day', 'gpp_perc')
  
  rm(ppt_id, doy, gpp.doy.drought.spl)
  
  #get day of year where roughly 90% of cumulative growth has occurred and turn into dataframe
  gpp_drought_predicted_90 <- gpp_drought_predicted %>%
    dplyr::filter(gpp_perc < 90.1)
  
  doy_90 <-
    max(gpp_drought_predicted_90$day) #this is a rough approximation
  
  rm(gpp_drought_predicted_90, gpp_drought_predicted)
  
  doy_90_df <- data.frame(doy_90)
  colnames(doy_90_df) <- c('doy_90_drought')
  doy_90_df$x <- x
  doy_90_df$y <- y
  
  doy_90_df <- doy_90_df[c(2, 3, 1)] #re-order to x,y,z
  
  return(doy_90_df)
  
  
}


#-------------------------------------------------------------------------------
# turn a list of the dataframes with same columns into one single dataframe ----


list_to_df <- function(x) {
  df <- data.frame(do.call("rbind", x))
  
  return(df)
  
  
}



#-------------------------------------------------------------------------------
# import daymet temperature data ------


get_daymet_temp <- function(i) {
  temp_lat <- sgs.1000[i,] %>% pull(y)
  temp_lon <- sgs.1000[i,] %>% pull(x)
  
  test <- download_daymet(
    lat = temp_lat,
    lon = temp_lon,
    start = year_value,
    end = year_value
  ) %>%
    #--- just get the data part ---#
    .$data
  
  test <- test %>%
    dplyr::filter(yday > 57) %>%
    dplyr::filter(yday < 297)
  
  #subset to precip
  test_tmax <- test[c(1, 2, 7)]
  test_tmin <- test[c(1, 2, 8)]
  
  #get 16-day sums of precip for each year for the given pixel
  #year_test <- unique(test$year)
  
  mean_tmax <- get_16_day_averages_temp(test_tmax)
  colnames(mean_tmax) <- 'tmax_c'
  mean_tmin <- get_16_day_averages_temp(test_tmin)
  colnames(mean_tmin) <- 'tmin_c'
  
  mean_t <- cbind(mean_tmax, mean_tmin)
  
  mean_t$tmean_c <- (mean_t$tmax_c + mean_t$tmin_c) / 2
  
  mean_t <- summarise_all(mean_t, mean)
  
  #add in year and coordinate columns
  mean_t$year <- year_value
  mean_t$x <- temp_lon
  mean_t$y <- temp_lat
  
  return(mean_t)
  
}

#-------------------------------------------------------------------------------
# import and format temp raster into dataframe ------

#format the temp rasters
format_temp_df <- function(x, max_min) {
  #test out what will be the 'format_temp_df' function
  #convert to raster
  raster_file <- raster(x)
  
  #extract year from the name of the raster to later add to dataframe
  if (max_min == T) {
    year_val <- substr(names(raster_file), 6, 9)
  } else{
    year_val <- substr(names(raster_file), 7, 10)
  }
  
  #convert to dataframe and add year and period columns
  df <- data.frame(rasterToPoints(raster_file))
  df$year <- year_val
  colnames(df) <- c('x', 'y', 'temp', 'year')
  
  #return formatted dataframe
  return(df)
  
  
}

#import the temp rasters from file, using the format function
import_temp <- function(Ecoregion, temp, value) {
  #i = 2003
  
  filepath_2 <-
    dir(
      paste0(
        './../../Data/Climate/Ecoregion/',
        Ecoregion,
        '/Temperature/',
        temp,
        '/'
      ),
      full.names = T
    )
  if (value == T) {
    test <- lapply(filepath_2, format_temp_df, max_min = T)
  } else{
    test <- lapply(filepath_2, format_temp_df, max_min = F)
  }
  
  test <- list_to_df(test)
  test$ecoregion <- Ecoregion
  test$temp_var <- temp
  
  return(test)
  
  
}

#-------------------------------------------------------------------------------



