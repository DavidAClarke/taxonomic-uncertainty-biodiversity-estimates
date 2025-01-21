################################################################################
## Script name: 00_main_menu.R
################################################################################

## Load the required libraries 
pkgs <- c("sf","tidyverse","here","terra","raster","sdmpredictors", "usdm", 
          "randomForest", "precrec","lubridate", "parzer", "adehabitatHR", 
          "hypervolume", "caret", "redlistr", "rcartocolor")
lapply(pkgs, require, character.only = T)
cat("\014")
################################################################################
## Load user-defined functions
source("R/01_functions.R")

################################################################################
## File paths
shared_data <- "C:/Users/dcla0021/Documents/postdoc/projects/shared_data/environmental"
#shared_data <- "path/to/data"
res_path <- here("data", "biodiversity", "output", "reps_csv")
sdm_path <- here("data", "biodiversity", "output", "reps_sdm")
model_path <- here("data", "biodiversity", "output", "models")
hypervolume_res_path <- here("data", "biodiversity", "output", "hypervolumes")

################################################################################
## Load cleaned and integrated biodiversity data
bio_data <- read.csv(here("data", "biodiversity", 
                          "output", "merged_new_tax.csv")) %>%
  
  spatially_explicit("decimalLongitude",
                     "decimalLatitude",
                     4326) %>%
  
  st_transform("ESRI:102019") %>% 
  
  dplyr::mutate(Longitude = st_coordinates(.)[,1]) %>%
  
  dplyr::mutate(Latitude = st_coordinates(.)[,2]) %>%
  
  dplyr::mutate(year_grp = as.factor(case_when(year >= 1996 & year < 2008 ~ "G1",
                                               year >= 2008 ~ "G2"))) %>%
  
  tidyr::drop_na(year)
