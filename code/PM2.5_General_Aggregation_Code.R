#TITLE: CHAIR-India PM2.5 Aggregation Code
#PURPOSE: Aggregate PM2.5 point data (2008-2018) to Geography
#AUTHOR: Muskaan Khemani (mkhemani@bu.edu)
#DATE: 03-27-2024

############################################################################
## set up environment ##
############################################################################
#set working directory 
setwd('/projectnb/chairgrp/')
dir <- '/projectnb/chairgrp/'
ls()
rm(list=ls())
gc()

#load libraries
library(fst)
library(tidyverse)
library(sf)
library(mapview)
library(raster)
library(dplyr) 

#read in geography, check CRS. In this example, it's the Mumbai Ward SHP
crop_extent <- read_sf("./muskaan/Mumbai_Wards.shp")

#check extent of geography
#mapview(crop_extent)
utm18nCRS <- crs(crop_extent)

############################################################################
## define functions to summarize PM2.5 data ##
############################################################################

# Function to read PM2.5 data for a specific year
read_data <- function(year) {
  #path where your data is stored
  file_path <- paste0("./Ajit_1/Annual_Summaries/files/", year, ".fst")
  return(read_fst(file_path))
}

# Function to aggregate the data for a specific year
process_data <- function(year, crop_extent) {
  AS_data <- read_data(year)
  points <- st_as_sf(AS_data, coords = c("lon", "lat"), crs = utm18nCRS)
  points_joined <- st_join(points, crop_extent, left = FALSE)
  meanx <- points_joined %>% group_by(name) %>% summarize(!!paste0("mean_pm25_", year) := mean(pred_eavg_gp_3, na.rm = TRUE))
  meanxy <- st_drop_geometry(meanx)
  return(meanxy)
}

# Loop through years
years <- 2008:2018
data_list <- list()
for (year in years) {
  data_list[[as.character(year)]] <- process_data(year, crop_extent)
}

# Merge data for all years into one dataframe
final_join <- crop_extent
for (year in years) {
  final_join <- merge(final_join, data_list[[as.character(year)]], by = "name", all.x = TRUE)
}

############################################################################
## export results ##
############################################################################
# Write final data to shapefile
st_write(final_join, "./muskaan/Mumbai_Annual_Summary.shp", overwrite = TRUE, delete_dsn = TRUE)
write_csv(final_join, "./muskaan/Mumbai_Annual_Summary.csv")
