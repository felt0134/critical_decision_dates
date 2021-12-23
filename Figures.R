# Figures 
library(scico)
#https://www.data-imaginist.com/2018/scico-and-the-colour-conundrum/

#julian day
#http://uop.whoi.edu/UOPinstruments/frodo/aer/julian-day-table.html

# day of 90-----

#import

#sgs
day_90_sgs <- raster('./../../Data/CDD/day_of_90/day_90_shortgrass_steppe.tif')
plot(day_90_sgs)
day_90_sgs_df <- data.frame(rasterToPoints(day_90_sgs))
day_90_sgs_df$Ecoregion <- 'Shortgrass steppe'
colnames(day_90_sgs_df) <- c('x','y','doy','Ecoregion')
median(day_90_sgs_df$doy) #263 = September 20

#nmp
day_90_nmp <- raster('./../../Data/CDD/day_of_90/day_90_northern_mixed_prairies.tif')
plot(day_90_nmp)
day_90_nmp_df <- data.frame(rasterToPoints(day_90_nmp))
day_90_nmp_df$Ecoregion <- 'Northern mixed prairies'
colnames(day_90_nmp_df) <- c('x','y','doy','Ecoregion')
median(day_90_nmp_df$doy) #248 = September 5

day_90_df <- rbind(day_90_nmp_df,day_90_sgs_df)

#filter out extreme high and low values
day_90_df <- day_90_df %>%
  dplyr::filter(doy < 297) %>%
  dplyr::filter(doy > 65)

max_sens_pdf <- ggplot(day_90_df,aes(x=doy,fill=Ecoregion)) +
  #scale_y_continuous(expand = c(0,0),limits = c(0,1.02)) +
  #scale_x_continuous(expand = c(0,0),limits = c(-1.5,0.6)) +
  geom_density(color='black',alpha=0.5,aes(y=..scaled..)) +
  scale_fill_manual(values=c('Northern mixed prairies'='black','Shortgrass steppe'='white')) +
  xlab('Day of 90% growth') +
  ylab('Probability density') +
  theme(
    axis.text.x = element_text(color='black',size=13), #angle=25,hjust=1),
    axis.text.y = element_text(color='black',size=13),
    axis.title = element_text(color='black',size=16),
    axis.ticks = element_line(color='black'),
    legend.key = element_blank(),
    legend.title = element_blank(),
    legend.text = element_text(size=10),
    legend.position = c(0.70,0.25),
    #legend.position = 'none',
    strip.background =element_rect(fill="white"),
    strip.text = element_text(size=15),
    panel.background = element_rect(fill=NA),
    panel.border = element_blank(), #make the borders clear in prep for just have two axes
    axis.line.x = element_line(colour = "black"),
    axis.line.y = element_line(colour = "black"))

#save to file
png(height = 1700,width=2000,res=300,'./../../Figures/day90_distributions.png')

print(max_sens_pdf)

dev.off()


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

#impact of drought on the 90% day of growth

#import

#sgs
day_90_drought_sgs <- raster('./../../Data/CDD/day_of_90/day_90_droughtshortgrass_steppe.tif')
plot(day_90_drought_sgs)
day_90_drought_sgs <- stack(day_90_drought_sgs,day_90_sgs)
day_90_drought_sgs_2 <- day_90_drought_sgs$day_90_droughtshortgrass_steppe - 
  day_90_drought_sgs$day_90_shortgrass_steppe
plot(day_90_drought_sgs_2)
summary(day_90_drought_sgs_2)

plot(layer~y,data=rasterToPoints(day_90_drought_sgs_2))

#nmp
day_90_drought_nmp <- raster('./../../Data/CDD/day_of_90/day_90_droughtnorthern_mixed_prairies.tif')
plot(day_90_drought_nmp)
day_90_drought_nmp <- stack(day_90_drought_nmp,day_90_nmp)
plot(day_90_drought_nmp)
day_90_drought_nmp_2 <- day_90_drought_nmp$day_90_droughtnorthern_mixed_prairies - 
  day_90_drought_nmp$day_90_northern_mixed_prairies
plot(day_90_drought_nmp_2)
summary(day_90_drought_nmp_2)
hist(day_90_drought_nmp_2$layer)

#combine
day_90_drought <- raster::merge(day_90_drought_nmp_2,day_90_drought_sgs_2,tolerance=0.25)
plot(day_90_drought)
summary(day_90_drought)
day_90_drought_df <- data.frame(rasterToPoints(day_90_drought))
str(day_90_drought_df)

#library(RColorBrewer)
#?scale_fill_scico
day_90_sgs_nmp_drought_map <- ggplot(day_90_drought_df, aes(x = x, y = y, fill = layer)) + 
  geom_raster() + 
  coord_equal() +
  #geom_sf()
  scale_fill_scico('Drought impact to day90',palette = 'vik',direction=1,midpoint=0) +
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
    legend.position = c(0.30,0.3),
    #legend.position = 'top',
    strip.background =element_rect(fill="white"),
    strip.text = element_text(size=10),
    panel.background = element_rect(fill=NA),
    panel.border = element_blank(), #make the borders clear in prep for just have two axes
    axis.line.x = element_blank(),
    axis.line.y = element_blank())

#save to file
png(height = 1500,width=2000,res=300,'./../../Figures/day_90_drought_growth_map.png')

print(day_90_sgs_nmp_drought_map)

dev.off()



# max sensitivity ----

#day of maximum sensitivity 

# notes: something weird happens when the two ecoregions are merged as dataframes
# and then plotted/converted to rasters as opposed to merging as raster. but it 
# would be nice to be able to seprate out the ecoregions by color in the bivariate plots.

#import data
max_sens_doy_sgs <- raster('./../../Data/CDD/maximum_sensitivity/max_sens_day_shortgrass_steppe.tif')
plot(max_sens_doy_sgs)
max_sens_doy_sgs_df <- data.frame(rasterToPoints(max_sens_doy_sgs))
max_sens_doy_sgs_df$Ecoregion <- 'Shortgrass steppe'
colnames(max_sens_doy_sgs_df) <-c('x','y','doy','Ecoregion')
median(max_sens_doy_sgs_df$doy) #176 = June 25th

max_sens_doy_nmp <- raster('./../../Data/CDD/maximum_sensitivity/max_sens_day_northern_mixed_prairies.tif')
plot(max_sens_doy_nmp)
max_sens_doy_nmp_df <- data.frame(rasterToPoints(max_sens_doy_nmp))
max_sens_doy_nmp_df$Ecoregion <- 'Northern mixed prairies'
colnames(max_sens_doy_nmp_df) <-c('x','y','doy','Ecoregion')
median(max_sens_doy_nmp_df$doy) #200 = July 19th

#bind them
max_sens_doy <- rbind(max_sens_doy_nmp_df,max_sens_doy_sgs_df)
head(max_sens_doy)

#filter out extreme high and low values
max_sens_doy <- max_sens_doy %>%
  dplyr::filter(doy < 297) %>%
  dplyr::filter(doy > 65)

max_sens_pdf <- ggplot(max_sens_doy,aes(x=doy,fill=Ecoregion)) +
  #scale_y_continuous(expand = c(0,0),limits = c(0,1.02)) +
  #scale_x_continuous(expand = c(0,0),limits = c(-1.5,0.6)) +
  geom_density(color='black',alpha=0.5,aes(y=..scaled..)) +
  scale_fill_manual(values=c('Northern mixed prairies'='black','Shortgrass steppe'='white')) +
  xlab('Day of maximum sensitivity') +
  ylab('Probability density') +
  theme(
    axis.text.x = element_text(color='black',size=13), #angle=25,hjust=1),
    axis.text.y = element_text(color='black',size=13),
    axis.title = element_text(color='black',size=16),
    axis.ticks = element_line(color='black'),
    legend.key = element_blank(),
    legend.title = element_blank(),
    legend.text = element_text(size=10),
    legend.position = c(0.2,0.75),
    #legend.position = 'none',
    strip.background =element_rect(fill="white"),
    strip.text = element_text(size=15),
    panel.background = element_rect(fill=NA),
    panel.border = element_blank(), #make the borders clear in prep for just have two axes
    axis.line.x = element_line(colour = "black"),
    axis.line.y = element_line(colour = "black"))

#save to file
png(height = 1700,width=2000,res=300,'./../../Figures/max_sens_distributions.png')

print(max_sens_pdf)

dev.off()


# max_sens_doy <- rasterFromXYZ(max_sens_doy[c(1,2,3)])
# plot(max_sens_doy)

#plot(rasterFromXYZ(max_sens_doy))

#bind rasters
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