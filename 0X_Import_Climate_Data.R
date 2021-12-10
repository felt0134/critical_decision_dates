# get climate data

rangeland_npp_covariates <- readRDS('./../../Data/Herbaceous_NPP_1986_2015_V2.rds')
head(rangeland_npp_covariates)
mean_production<-aggregate(npp~ x + y + region,mean,data=rangeland_npp_covariates)
sgs<-subset(mean_production,region==c('shortgrass_steppe'))
head(sgs[c(1,2,4)])
sgs_raster<-rasterFromXYZ(sgs[c(1,2,4)])
# Update projection
aea.proj <- "+proj=longlat +datum=WGS84"
#sgs_raster<-projectRaster(sgs_raster, crs=aea.proj)
proj4string(sgs_raster) <- CRS("+proj=longlat")
# writeRaster(sgs_raster,'/Users/loaner/Dropbox (Chapman)/Grant_proposals/DOE 2021 extreme precip/2021/Figures/sgs_npp.tif')
# test<-raster('/Users/loaner/Dropbox (Chapman)/Grant_proposals/DOE 2021 extreme precip/2021/Figures/sgs_npp.tif')
# plot(test)
plot(sgs_raster)
rm(mean_production_raster,hot_deserts_above_below,northern_mixed_prairies_above_below,
   semiarid_steppe_above_below,rangeland_npp_covariates,test.df,mean_production,fractional_cover,
   cold_deserts_above_below,california_annuals_above_below)
# ?nrow
# dim(sgs)
sgs.1000<-sgs[(1:1000),]
data.store<-list()
#head(mean_production)

fac <- (seq_len(nrow(m))-1) %/% 16
## Apply sum 
apply(m, 2, function(v) tapply(v, fac, sum))

for(i in 1:nrow(sgs.1000)){
  
  #test.df<-mean_production[i]
  lon.val<-sgs$x[i]
  lat.val<-sgs$y[i]
  
  #?download_daymet
  test<-download_daymet(site = "Daymet", lat = lat.val, lon = lon.val,
                        start = 1986, end = 2019,
                        path = tempdir(), internal = TRUE, silent = FALSE, force = FALSE,
                        simplify = FALSE)
  
  #end to most recent year
  #end = as.numeric(format(Sys.time(), "%Y")) - 2
  
  test<-test$data
  
  #March 1 to October 1
  test<- test %>%
    dplyr::filter(yday > 60) %>%
    dplyr::filter(yday < 285) 
  
  test$x<-sgs$x[i]
  test$y<-sgs$y[i]
  
  # summary(test)
  # head(test)
  
  #subset to precip
  test<-test[c(1,2,4,10,11)]
  
}
  
  #get 16-day sums of precip for each year for the given pixel
  year_test <- unique(test$year)
  
  for(j in year_test){
    
    fac <- (seq_len(nrow(test[c(3)]))-1) %/% 16
    apply(test[c(3)], 2, function(v) tapply(v, fac, sum))
    
  }
  
  fac <- (seq_len(nrow(data.store.df.1986[c(3)]))-1) %/% 16
  apply(data.store.df.1986[c(3)], 2, function(v) tapply(v, fac, sum))
  
  
  
  data.store[[i]] <- test
  
}
#224
# 1-100
data.store.df<- do.call("rbind", data.store)
head(data.store.df)

year.values <- unique(data.store.df$year)

for(i in year.values){
  
  
  
  
}

#prep for mapping
data.store.df$mm<-data.store.df$prcp..mm.day.
head(data.store.df)
deluge.count<-aggregate(mm~x+y,length,data=data.store.df)
head(deluge.count)
plot(rasterFromXYZ(deluge.count))

test<- test %>%
  dplyr::filter()

library(sp)
library(rgdal)
SGS.shape<-readOGR(dsn="/Volumes/GoogleDrive/My Drive/range-resilience/scope/study-area-shapefiles/SGS",layer="SGS")
#plot(SGS.shape)
SGS.shape@bbox <- as.matrix(extent(mean_production_raster))
#plot(SGS.shape)

# Step 2:
SGS.shape.2 <- sp::spTransform(SGS.shape, CRS(aea.proj))

sgs.shape.df<-data.frame(SGS.shape.2)
plot(SGS.shape.2)

head(mean_production)