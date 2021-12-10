#setup workspace

#clearworkspace
rm(list=ls())

pkgs <- c("ncdf4", "raster", "rgdal", "reshape2", "sp",'dplyr','data.table',
          'rstudioapi','daymetr','ggplot2')
lapply(pkgs, library, character.only = TRUE) 

# Set working directory to local directory
current_path <- getActiveDocumentContext()$path
setwd(dirname(current_path))

#load functions
source('02_Functions.R')

