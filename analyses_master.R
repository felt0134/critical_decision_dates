

#master analysis script that calls on other scripts 

#get data and track progress
library(future.apply)
library(progressr)

# generate and save to file estimates of the day of 90% growth and how this
# changes during years of drought



Ecoregion = 'shortgrass_steppe'
#Ecoregion = 'northern_mixed_prairies' 


source('day_of_90.R')
