################################################################################
## SDM predictors
################################################################################

## Environmental data path
env_data <- "path/to/data"

################################################################################
# There are multiple versions of variables in Bio-ORACLE. Except for bathymetry
# (version 10) I want the latest (version 22). First create these two. 
# Focusing on Bio-ORACLE as it contains benthic related variables whereas 
# MARSPEC is focused on surface and shallow water.
################################################################################

## Bathymetry from version 10
orac_vars_v10 <- get_vars(terrestrial = FALSE, marine = TRUE,
                      dataset = "Bio-ORACLE", ver = 10)
depth <- orac_vars_v10[which(str_detect(orac_vars_v10, "bathy"))]

## Variables from version 22
orac_vars_v22 <- get_vars(terrestrial = FALSE, marine = TRUE,
                          dataset = "Bio-ORACLE", ver = 22)
# remove clouds
clouds <- which(str_detect(orac_vars_v22, "cloud"))
orac_vars_v22 <- orac_vars_v22[-clouds]

## Download data
lapply(orac_vars_v22[1:length(orac_vars_v22)], function(i){

  load_layers(i, datadir = here(env_data, "sdmpredictors"))

  files <- list.files(here(env_data, "sdmpredictors"),
                      full.names = T, pattern = i)

  if(endsWith(files, ".zip")){

    unzip(files, exdir = here(env_data, "sdmpredictors"))

    unlink(files)
  }

})

## Load all layers. Need to crop to my extent before doing correlations
# Choose Mean depth variables
var_files <- list.files(here(env_data, "sdmpredictors"),
                        full.names = T, pattern = ".Benthic.Mean.Depth|Surface")

var_ras <- rast(var_files)

# Use mean bathy if using mean depth variables above
bathy_ras <- rast(here(env_data, "sdmpredictors", "BO_bathymean_lonlat.tif"))
slope <- terrain(bathy_ras, "slope")
ras_vars <- c(bathy_ras,slope, var_ras)

## Increasing extent of occurrences to ensure capturing all occurrence 
# related environmental information
occs_buff <- st_buffer(st_transform(bio_data, 4326), 20000)

## Crop to (buffered) occurrence data
ras_ant <- terra::crop(ras_vars, occs_buff)

## Transform everything to "ESRI:102019" before doing analyses.
ras_ant_proj <- project(ras_ant, y = "ESRI:102019", res = 9200)
writeRaster(ras_ant_proj, here("data", "environmental", "promach_preds_all.tif"))

## Load data and retain benthic variables
ras_ant_proj <- rast(here("data", "environmental", "promach_preds_all.tif"))
nms <- names(ras_ant_proj)
nms_ben <- nms[str_detect(nms, "Surface", negate = T)]
ras_ant_proj_benth <- subset(ras_ant_proj, c(nms_ben))

## Set seed
set.seed(1234)

## Correlations
nocorrvar <- usdm::vifcor(ras_ant_proj_benth, 0.95)
predictors <- as.character(nocorrvar@results$Variables)
env_sub <- subset(ras_ant_proj,c(predictors))

## To limit the collinearity among predictors used for the SDMs, only one 
# predictor within a ‘correlation group’ was used – six predictors overall, 
# i.e. 6 groups (Barbet-Massin and Jetz 2014)

cor_mat <- pearson_correlation_matrix(raster::stack(env_sub))
# Use |0.7| - Dormann et al 2013
cor_groups <- correlation_groups(cor_mat, max_correlation = 0.7)
names(cor_groups) <- paste0("G",LETTERS[1:6])

## Each row represents combination of variables from each group
pred_combs <- expand.grid(cor_groups, KEEP.OUT.ATTRS = F, stringsAsFactors = F)
write.csv(pred_combs, here("data", "environmental", "pred_vars.csv"), row.names = F)
pred_combs <- read.csv(here("data", "environmental", "pred_vars.csv"))

preds_new <- unique(unlist(pred_combs))
env_sub <- subset(ras_ant_proj,c(preds_new))
cor_mat <- pearson_correlation_matrix(raster::stack(ras_ant_proj))
pred_names <- c("Depth", "Silicate (Lt. min)", "Chlorophyll (Mean)",
                "Nitrate (Range)", "Phosphate (Lt. max)", "Phytoplankton (Lt. min)",
                "Salinity (Lt. max)", "Temperature (Range)", "Temperature (Min)",
                "Current velocity (Lt. max)", "Current velocity (Lt. min)",
                "Current velocity (Mean)", "Current velocity (Range)",
                "Dissolved oxygen (Lt. min)", "Dissolved oxygen (Min)",
                "Dissolved oxygen (Range)", "Silicate (Range)")
rownames(cor_mat) <- pred_names
colnames(cor_mat) <- pred_names
cor_plot <- plot_correlation(cor_mat, 
                             palette = rev(c("#2c7bb6", "#abd9e9", "#ffffbf", 
                                             "#fdae61", "#d7191c")))
cor_plot +
  theme(axis.text = element_text(size = 14),
        legend.title = element_text(size = 16),
        legend.text = element_text(size = 14))

writeRaster(env_sub, here("data", "environmental", "promach_preds_min.tif"))
ras_ant_proj <- rast(here("data", "environmental", "promach_preds_min.tif"))

## Create predictor stacks 
pred_list <- c()
for(i in 1:nrow(pred_combs)){
  
  preds <- as.character(pred_combs[i,])
  env_sub <- subset(ras_ant_proj,c(preds))
  pred_list[[i]] <- env_sub
  
}
