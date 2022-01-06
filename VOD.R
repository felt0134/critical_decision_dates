

# VOD data


get_vwc <-function(x,y,filepath){
  
  april_vector <-c(x:y)
  april_list<-list()
  
  lat_lon <- './../../Data/VOD/SMAPCenterCoordinates9KM.mat'
  #h5read(lat_lon, read.attributes = TRUE)
  lat_lon<-readMat(lat_lon)
  
  #lat
  lat<-lat_lon[1]
  lat<-as.data.frame(lat)
  
  lat$ID <- rownames(lat)
  lat <- reshape2::melt(lat, id.vars = c("ID"),variable.name = "lat")
  lat <- lat[c(1,3)]
  colnames(lat) <- c('ID','y')
  lat$ID <- rownames(lat)
  #lat$ID <- as.numeric(as.character(lat$ID))
  
  #lon
  lon<-lat_lon[2]
  lon<-as.data.frame(lon)
  
  lon$ID <- rownames(lon)
  lon <- reshape2::melt(lon, id.vars = c("ID"),variable.name = "lon")
  lon <- lon[c(1,3)]
  colnames(lon) <- c('ID','x')
  lon$ID <- rownames(lon)
  #lon$ID <- as.numeric(as.character(lon$ID))
  
  lat_lon_df <- merge(lon,lat,by=c('ID'))
  rm(lat,lon,lat_lon)
  
  #subset to temperate latitudes
  lat_lon_df <- lat_lon_df %>%
    filter(y < 60) %>%
    filter(y > 20)
  
  for(i in april_vector){
    
    # read in tau file 
    tst.2 <-h5read(filepath,"MTDCA_TAU")
    # dim(tst.2) # 1624 3856   91 (days)
    # class(tst.2) # array
    tst.2 <- tst.2[,,c(i)] # subset to day 1
    
    
    #head(tst.2)
    
    tst.2<-as.data.frame(tst.2)
    tst.2 <- reshape2::melt(tst.2,variable.name = "tau")
    # head(tst.2)
    # summary(tst.2)
    # hist(tst.2$value)
    
    tst.2$ID <- rownames(tst.2)
    #head(tst.2)
    tst.2 <- tst.2[c(3,2)]
    #head(tst.2)
    
    #merge with coordinates
    tst.2 <- merge(lat_lon_df,tst.2,by=c('ID'))
    tst.2 <- tst.2[c(2,3,4)]
    
    #plot(lat_lon_df$x,lat_lon_df$y)
    
    tst.2 <- tst.2 %>%
      dplyr::filter(!value=='NaN')
    #head(vwc.coordinates)
    #head(vwc.coord.jan1)
    
    tst.2$day <- i
    
    april_list[[i]]<-tst.2
    
  }
  
  #make to df
  april_df <- do.call('rbind',april_list)
  rm(april_list)
  #head(april_df)
  
  #average across pixels
  # april_ag<-aggregate(vwc~x+y,mean,data=april_df)
  # rm(april_df)
  #head(april_ag)
  
  #return(april_ag)
  return(april_df)
  
  # *kg/mm^2 and mm^m2 (height of water over m^2) workout to be the same, so that conversion is not done.
  
}

library(R.matlab)
library(rhdf5)
test <-get_vwc(x=1,y=14,filepath = './../../Data/VOD/2016/MTDCA_V4_TAU_201604_201606_9km.mat')
head(test)


test_sd <- test %>%
  group_by(x,y) %>%
  summarise(day.sd = sd(day))

test_sd <- data.frame(test_sd)

head(test_sd)
plot(rasterFromXYZ(test_sd))

fix_grid<-function(x){
  
  e<-extent(x[c(1:2)])
  r<-raster(e,ncol=500,nrow=500,crs='+proj=longlat +datum=WGS84')
  r_new<-rasterize(x[,1:2],r,x[3],fun=mean)
  return(r_new)
  
}


test_sd_raster <- fix_grid(test_sd)
plot(test_sd_raster)

ggplot(test_sd,aes(x=x,y=y,fill=day.sd)) +
  geom_raster()

library(sp)
library(rgdal)

coordinates(test_sd)=~x+y
proj4string(test_sd)=CRS("+proj=longlat +datum=WGS84") # set it to lat-long
test_sd = spTransform(test_sd,CRS("+proj=longlat +datum=WGS84"))

  
gridded(test_sd) = TRUE
# At this point you'll get an error if your coordinates don't lie on a nice regular grid.
# Now use the raster package to convert to a raster and set its CRS:
  
r = raster(test_sd)
projection(r) = CRS("+proj=longlat +datum=WGS84")

plot(r)



