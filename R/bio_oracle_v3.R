################################################################################
## Script name: bio_oracle_v3.R
## Author: David Clarke
## Copyright (c) David Clarke, 2025
## Email: david_anthony_clarke@hotmail.com
################################################################################

library(biooracler)

shared_data <- "C:/Users/dcla0021/Documents/postdoc/projects/shared_data/environmental"

## Increasing extent of occurrences to ensure capturing all occurrence 
# related environmental information
occs_buff <- st_buffer(st_transform(bio_data, 4326), 20000)

## List all available layers
bo_layers <- biooracler::list_layers()

## Topographic layers
terr_layers <- bo_layers %>% 
  dplyr::filter(str_detect(dataset_id, "terrain"))

my_info <- info_layer(pull(terr_layers[1,1]))
start_time <- my_info$alldata$NC_GLOBAL[which(my_info$alldata$NC_GLOBAL$attribute_name == "time_coverage_start"),]$value
end_time <- my_info$alldata$NC_GLOBAL[which(my_info$alldata$NC_GLOBAL$attribute_name == "time_coverage_end"),]$value

## Download topographic layers
dataset_id <- pull(terr_layers[1,1])
# Underlying functions require each vector in the constraints list to be length 2
time <- c(start_time, end_time)
latitude <- c(as.numeric(st_bbox(occs_buff)[2]), as.numeric(st_bbox(occs_buff)[4]))
longitude <- c(-179.975, 179.975)
constraints <- list(time, latitude,longitude)
names(constraints) <- c("time", "latitude", "longitude")
variables <- c("aspect", "bathymetry_mean", "slope", "terrain_ruggedness_index")

terr_ras <- download_layers(dataset_id, variables, constraints, 
                fmt = "raster", directory = here(shared_data, "bio_oracle_v3", "current"))

# Transform everything to "ESRI:102019" before doing analyses.
terr_ras <- project(terr_ras, y = "ESRI:102019", res = 5500)
terr_ras[terr_ras < -9999] <- NA

lapply(terr_ras, function(i){
  
  writeRaster(i, here(shared_data, "bio_oracle_v3", "current",paste0(names(i),".tif")))
  
})

## Focus on baseline and mean depth layers
scenarios <- c("ssp126","ssp245","ssp370","ssp460","ssp585")

for(s in scenarios){
  
  bl_bo_layers <- bo_layers %>% 
    dplyr::filter(str_detect(dataset_id, glob2rx(paste0("*",s,"*mean*"))))
  
  lapply(bl_bo_layers[["dataset_id"]], function(i){
    
    my_info <- info_layer(i)
    
    start_time <- my_info$alldata$NC_GLOBAL[which(my_info$alldata$NC_GLOBAL$attribute_name == "time_coverage_start"),]$value
    end_time <- my_info$alldata$NC_GLOBAL[which(my_info$alldata$NC_GLOBAL$attribute_name == "time_coverage_end"),]$value
    
    time <- c(start_time, end_time)
    latitude <- c(as.numeric(st_bbox(occs_buff)[2]), as.numeric(st_bbox(occs_buff)[4]))
    longitude <- c(-179.975, 179.975)
    
    constraints <- list(time, latitude,longitude)
    names(constraints) <- c("time", "latitude", "longitude")
    variables <- my_info$variables$variable_name
    
    ras <- download_layers(i, variables, constraints, 
                           fmt = "raster", directory = here(shared_data, "bio_oracle_v3", "future"))
    
    # Transform everything to "ESRI:102019" before doing analyses.
    ras <- project(ras, y = "ESRI:102019", res = 5500)
    ras[ras < -9999] <- NA
    
    lapply(ras, function(j){
      
      writeRaster(j, here(shared_data, "bio_oracle_v3", "future", paste0(names(j),"_",s,".tif")))
      
    })
    
  })
  
  gc()
  
}


