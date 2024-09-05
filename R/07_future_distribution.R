################################################################################
## Script name: 00_future_distribution.R
## Author: David Clarke
## Copyright (c) David Clarke, 2024
## Email: david_anthony_clarke@hotmail.com
################################################################################

## Prepare future data layers
current_benth_preds <- rast(here("data", "environmental", "promach_preds.tif"))

## All future data layers
fut_clim_all_files <- list.files(here(env_data, "sdmpredictors"), 
                                 full.names = T, pattern = ".RCP")
fut_clim_all_stack <- terra::rast(fut_clim_all_files)

## Create buffer around crinoid occurrences
occs_buff <- st_buffer(st_transform(bio_data, 4326), 20000)

## Crop to (buffered) crinoid data
fut_clim_all_stack <- terra::crop(fut_clim_all_stack, occs_buff)

## get names
all_fut_names <- names(fut_clim_all_stack)
for_fut_names <- unique(unlist(regmatches(all_fut_names, gregexpr("(?<=Depth.).*", 
                                                                  all_fut_names, perl = T))))

## find current data variables corresponding with future variables
names(current_benth_preds) <- gsub("Present.Benthic.Mean.Depth.","",names(current_benth_preds))
current_benth_preds_min <- subset(current_benth_preds, for_fut_names)

## Remove highly correlated variables
nocorrvar <- usdm::vifcor(current_benth_preds_min, 0.7)
predictors <- as.character(nocorrvar@results$Variables)
for_fut_preds_min <- subset(current_benth_preds_min,c(predictors))

## Write for future climate variables to disk
writeRaster(for_fut_preds_min, here("data", "environmental", "for_fut_preds_min.tif"))

## Subset the future data layers
l <- list()
for(i in 1:length(predictors)){
  
  l[[i]] <-  grep(predictors[i],names(fut_clim_all_stack), 
                  ignore.case = T, value = T)
}

index <- unique(unlist(l))
fut_clim_all_stack_min <- terra::subset(fut_clim_all_stack, index)

## transform future layers to "ESRI:102019" before doing analyses.
start <- Sys.time()
fut_clim_all_stack_min <- terra::project(fut_clim_all_stack_min, y = "ESRI:102019", res = 9200)
end <- Sys.time()
end-start

## Write future climate variables to disk
writeRaster(fut_clim_all_stack_min, here("data", "environmental", "fut_clim_all_stack_min.tif"))

## Develop current and future SDMs
res_comp <- fut_clim_rf(data = bio_data, 
                       status = "comp", 
                       bias.cor = F,
                       n = 20,
                       preds = for_fut_preds_min,
                       fut_preds = fut_clim_all_stack_min,
                       year = c("2050", "2100"),
                       scenario = c("RCP26", "RCP45", "RCP60", "RCP85"),
                       group = c("G1", "G2"))

res_new <- fut_clim_rf(data = bio_data, 
                       status = "new", 
                       species = "Promachocrinus kerguelensis",
                       bias.cor = F,
                       n = 20,
                       preds = for_fut_preds_min,
                       fut_preds = fut_clim_all_stack_min,
                       year = c("2050", "2100"),
                       scenario = c("RCP26", "RCP45", "RCP60", "RCP85"), 
                       group = c("G1", "G2"))
