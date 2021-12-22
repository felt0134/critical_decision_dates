# Figures 
library(scico)

# day of 90-----

#import
day_90_sgs <- raster('./../../Data/CDD/day_of_90/day_90_shortgrass_steppe.tif')
plot(day_90_sgs)

day_90_nmp <- raster('./../../Data/CDD/day_of_90/day_90_northern_mixed_prairies.tif')
plot(day_90_nmp)

# nmp_look <- stack(day_90_nmp,max_sens_doy_nmp)
# plot(nmp_look)

#combine
day_90_sgs_nmp <- raster::merge(day_90_sgs,day_90_nmp,tolerance=1)
plot(day_90_sgs_nmp)
day_90_sgs_nmp <- data.frame(rasterToPoints(day_90_sgs_nmp))

#filter out extreme high and low values
day_90_sgs_nmp <- day_90_sgs_nmp %>%
  dplyr::filter(layer < 297) %>%
  dplyr::filter(layer > 65)

day_90_sgs_nmp_map <- ggplot(day_90_sgs_nmp, aes(x = x, y = y, fill = layer)) + 
  geom_raster() + 
  coord_equal() +
  #geom_sf()
  scale_fill_scico('Day of 90% growth',palette = 'batlow',direction=-1) +
  xlab('') +
  ylab('') +
  theme(
    axis.text.x = element_blank(), #angle=25,hjust=1),
    axis.text.y = element_blank(),
    axis.title.x = element_text(color='black',size=10),
    axis.title.y = element_text(color='black',size=10),
    axis.ticks = element_blank(),
    legend.key = element_blank(),
    #legend.title = element_blank(),
    #legend.text = element_text(size=2),
    #legend.position = c(0.7,0.1),
    #legend.margin =margin(r=5,l=5,t=5,b=5),
    legend.position = c(0.35,0.3),
    #legend.position = 'top',
    strip.background =element_rect(fill="white"),
    strip.text = element_text(size=10),
    panel.background = element_rect(fill=NA),
    panel.border = element_blank(), #make the borders clear in prep for just have two axes
    axis.line.x = element_blank(),
    axis.line.y = element_blank())


#save to file
png(height = 1500,width=2000,res=300,'./../../Figures/day_90_growth_map.png')

print(day_90_sgs_nmp_map)

dev.off()

#next do how drought impacts the 90% day of growth


# max sensitivity ----

#day of maximum sensitivity 

# notes: something weird happens when the two ecoregions are merged as dataframes
# and then plotted/converted to rasters as opposed to merging as raster. but it 
# would be nice to be able to seprate out the ecoregions by color in the bivariate plots.

#import data
max_sens_doy_sgs <- raster('./../../Data/CDD/maximum_sensitivity/max_sens_day_shortgrass_steppe.tif')
plot(max_sens_doy_sgs)
# max_sens_doy_sgs_df <- data.frame(rasterToPoints(max_sens_doy_sgs))
# max_sens_doy_sgs_df$Ecoregion <- 'Shortgrass_steppe'
# colnames(max_sens_doy_sgs_df) <-c('x','y','doy','Ecoregion')

max_sens_doy_nmp <- raster('./../../Data/CDD/maximum_sensitivity/max_sens_day_northern_mixed_prairies.tif')
plot(max_sens_doy_nmp)
# max_sens_doy_nmp_df <- data.frame(rasterToPoints(max_sens_doy_nmp))
# max_sens_doy_nmp_df$Ecoregion <- 'Northern mixed prairies'
# colnames(max_sens_doy_nmp_df) <-c('x','y','doy','Ecoregion')
# 
# #bind them
# max_sens_doy <- rbind(max_sens_doy_nmp_df,max_sens_doy_sgs_df)
# max_sens_doy <- rasterFromXYZ(max_sens_doy[c(1,2,3)])
# plot(max_sens_doy)

#plot(rasterFromXYZ(max_sens_doy))

max_sens_sgs_nmp <- raster::merge(max_sens_doy_sgs,max_sens_doy_nmp,tolerance=1)
plot(max_sens_sgs_nmp)
max_sens_sgs_nmp <- data.frame(rasterToPoints(max_sens_sgs_nmp))

#filter out extreme high and low values
max_sens_sgs_nmp <- max_sens_sgs_nmp %>%
  dplyr::filter(layer < 297) %>%
  dplyr::filter(layer > 65)

max_sens_day_map <- ggplot(max_sens_sgs_nmp, aes(x = x, y = y, fill = layer)) + 
  geom_raster() + 
  coord_equal() +
  #geom_sf()
  scale_fill_scico('Day of maximum sensitivity',palette = 'batlow',direction=-1) +
  xlab('') +
  ylab('') +
  theme(
    axis.text.x = element_blank(), #angle=25,hjust=1),
    axis.text.y = element_blank(),
    axis.title.x = element_text(color='black',size=10),
    axis.title.y = element_text(color='black',size=10),
    axis.ticks = element_blank(),
    legend.key = element_blank(),
    #legend.title = element_blank(),
    #legend.text = element_text(size=2),
    #legend.position = c(0.7,0.1),
    #legend.margin =margin(r=5,l=5,t=5,b=5),
    legend.position = c(0.35,0.3),
    #legend.position = 'top',
    strip.background =element_rect(fill="white"),
    strip.text = element_text(size=10),
    panel.background = element_rect(fill=NA),
    panel.border = element_blank(), #make the borders clear in prep for just have two axes
    axis.line.x = element_blank(),
    axis.line.y = element_blank())


#save to file
png(height = 1500,width=2000,res=300,'./../../Figures/max_sens_day_map.png')

print(max_sens_day_map)

dev.off()



#merge in names
max_sens_sgs_nmp_test <- merge(max_sens_sgs_nmp,max_sens_doy_nmp_df[c(1,2,4)],by=c('x','y'))

#bivariate plot of day of maximum sensitivity and latitude
max_sens_day_bivari <- ggplot(max_sens_sgs_nmp, aes(x = y, y = layer)) + 
  geom_point(alpha=0.25,size=1,pch=21,color='black',fill='white') + 
  #geom_sf()
  xlab('Latitude') +
  ylab('Day of of maximum sensitivity') +
  #geom_smooth(method='lm',color='red') +
  theme(
    axis.text.x = element_text(color='black',size=13), #angle=25,hjust=1),
    axis.text.y = element_text(color='black',size=13),
    axis.title = element_text(color='black',size=16),
    axis.ticks = element_line(color='black'),
    legend.key = element_blank(),
    legend.title = element_blank(),
    legend.text = element_text(size=10),
    legend.position = c(0.15,0.5),
    #legend.position = 'none',
    strip.background =element_rect(fill="white"),
    strip.text = element_text(size=15),
    panel.background = element_rect(fill=NA),
    panel.border = element_blank(), #make the borders clear in prep for just have two axes
    axis.line.x = element_line(colour = "black"),
    axis.line.y = element_line(colour = "black"))

#save to file
png(height = 1700,width=2000,res=300,'./../../Figures/max_sens_latitude.png')

print(max_sens_day_bivari)

dev.off()


#gif of sensitivity -----


g <- ggplot(data = pdsi_df, aes(x = x, y = y, fill = PDSI))  +
  geom_tile() +
  scale_fill_viridis(direction = -1) +
  geom_sf(data = states, fill = "transparent", color = "grey45", inherit.aes = FALSE) +
  geom_sf(data = borders, fill = "transparent", color = "grey33", inherit.aes = FALSE) +
  ggthemes::theme_map() +
  theme(legend.key.width = unit(.70,"cm"), legend.position = c(.10, .10))


g