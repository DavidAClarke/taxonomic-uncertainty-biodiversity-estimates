################################################################################
## Script name: 01_functions.R
## Author: David Clarke
## Copyright (c) David Clarke, 2024
## Email: david_anthony_clarke@hotmail.com
################################################################################

## Make occurrence data spatially explicit----
spatially_explicit <- function(df, x, y, to_epsg, jit = 0){
  
  df_sf <- df %>%
    
    dplyr::mutate(dplyr::across(all_of(c(x, y)), as.numeric)) %>% # coordinates must be numeric
    
    dplyr::filter(!is.na(!!rlang::sym(x))) %>%
    
    dplyr::filter(!is.na(!!rlang::sym(y))) %>%
    
    sf::st_as_sf(
      coords = c(x, y),
      agr = "constant",
      crs = to_epsg,        # WGS84: 4326
      stringsAsFactors = FALSE,
      remove = TRUE) %>%
    
    sf::st_jitter(amount = jit)
  
  return(df_sf)
}

## Obtain Bio-ORACLE variables----
get_vars <- function(terrestrial, marine, dataset, ver){
  
  datasets <- list_datasets(terrestrial = terrestrial, marine = marine)
  layers <- list_layers(datasets, monthly = F)
  vars <- layers %>% 
    filter(dataset_code == dataset) %>%
    filter(version == ver) %>% 
    dplyr::select(layer_code) %>% 
    pull()
  
}
## For recursive variable selection using `caret`----
dwsrfRFE <-  list(summary = function(data, lev = NULL, model = NULL){
  
  if (is.character(data$obs)) 
    data$obs <- factor(data$obs, levels = lev)
    postResample(data[, "pred"], data[, "obs"])
  
},

fit = function(x, y, first, last, ...){
  
  prNum <- as.numeric(table(y)["1"]) # number of presence records
  spsize <- c("0" = prNum, "1" = prNum)
  
  data <- data.frame(cbind(x,y))
  
  loadNamespace("randomForest")
  randomForest::randomForest(y ~ ., 
                             data = data, 
                             ntree = 1000,
                             sampsize = spsize,
                             replace = TRUE,
                             importance = T)
},

pred = function(object, x) {
  
  tmp <- predict(object, x)
  if (is.factor(object$y)) {
    out <- cbind(data.frame(pred = tmp), 
                 as.data.frame(predict(object, 
                                       x, type = "prob"), stringsAsFactors = TRUE))
  }
  else out <- tmp
  out
  
},

rank = function(object, x, y) {
  
  vimp <- varImp(object)
  if (is.factor(y)) {
    if (all(levels(y) %in% colnames(vimp))) {
      avImp <- apply(vimp[, levels(y), drop = TRUE], 1, 
                     mean)
      vimp$Overall <- avImp
    }
  }
  vimp <- vimp[order(vimp$Overall, decreasing = TRUE), , drop = FALSE]
  if (ncol(x) == 1) {
    vimp$var <- colnames(x)
  }
  else vimp$var <- rownames(vimp)
  vimp
  
},
selectSize = pickSizeBest,
selectVar = pickVars)


## Run random forest SDMs----
run_rf <- function(data, status, species = NULL, n, bias.cor, preds, group = c("G1", "G2")){
  
  
  modEval_data <- data.frame(Species = character(),
                             Model = integer(),
                             Rep = integer(),
                             Bias_cor = logical(),
                             AUC = numeric(),
                             KAPPA = numeric())
  
  # create empty df to store outcomes
  varimp_data <- data.frame(Species = character(),
                            Model = integer(),
                            Rep = integer(),
                            Bias_cor = logical(),
                            Var_name = character(),
                            Acc = numeric(),
                            AccScore = numeric())
  
  pred_dws <- list()
  
  if(status == "genus"){
    
    # species <- "Promachocrinus kerguelensis"
    # sp <- gsub(" ", "_", species)
    
    sp <- "Promachocrinus"
    
    occurrence <- data %>%
      
      dplyr::mutate(x = sf::st_coordinates(.)[,1]) %>%
      
      dplyr::mutate(y = sf::st_coordinates(.)[,2]) %>%
      
      dplyr::select(x,y) %>%
      
      sf::st_drop_geometry() 
    
    if(bias.cor == T){
      
      occurrence <- occurrence %>% 
        
        dismo::gridSample(r = raster::stack(preds[[1]]))
      
    } else {
      
      occurrence <- occurrence
    }
    
    
    write.csv(modEval_data, here("data", "biodiversity", "output", 
                                 paste0(status,"_",sp,"_",bias.cor,"_model_evaluations.csv")), row.names = F)
    write.csv(varimp_data, here("data", "biodiversity", "output", 
                                paste0(status,"_",sp,"_",bias.cor,"_variable_importance.csv")), row.names = F)
  }
  
  if(status == "comp"){
    
    species <- "Promachocrinus kerguelensis"
    sp <- gsub(" ", "_", species)
    
    occurrence <- data %>%
      
      dplyr::filter(scientificName == "Promachocrinus kerguelensis" |
                      scientificName == "Promachocrinus joubini" | 
                      scientificName == "Promachocrinus fragarius" |
                      scientificName == "Promachocrinus unruhi" |
                      scientificName == "Promachocrinus uskglass") %>%
      
      dplyr::mutate(x = sf::st_coordinates(.)[,1]) %>%
      
      dplyr::mutate(y = sf::st_coordinates(.)[,2]) %>%
      
      dplyr::select(x,y) %>%
      
      sf::st_drop_geometry() 
    
    if(bias.cor == T){
      
      occurrence <- occurrence %>% 
        
        dismo::gridSample(r = raster::stack(preds[[1]]))
      
    } else {
      
      occurrence <- occurrence
    }
    
    
    write.csv(modEval_data, here("data", "biodiversity", "output", 
                                 paste0(status,"_",sp,"_",bias.cor,"_model_evaluations.csv")), row.names = F)
    write.csv(varimp_data, here("data", "biodiversity", "output", 
                                paste0(status,"_",sp,"_",bias.cor,"_variable_importance.csv")), row.names = F)
  }
  
  if(status == "new"){
    
    occurrence <- data %>%
      
      dplyr::filter(scientificName == species) %>%
      
      dplyr::mutate(x = sf::st_coordinates(.)[,1]) %>%
      
      dplyr::mutate(y = sf::st_coordinates(.)[,2]) %>%
      
      dplyr::select(x,y) %>%
      
      sf::st_drop_geometry() 
    
    if(bias.cor == T){
      
      occurrence <- occurrence %>% 
        
        dismo::gridSample(r = raster::stack(preds[[1]]))
      
    } else {
      
      occurrence <- occurrence
    }
    
    sp <- gsub(" ", "_", species)
    
    write.csv(modEval_data, here("data", "biodiversity", "output", 
                                 paste0(status,"_",sp,"_",bias.cor,"_model_evaluations.csv")), row.names = F)
    
    write.csv(varimp_data, here("data", "biodiversity", "output", 
                                paste0(status,"_",sp,"_",bias.cor,"_variable_importance.csv")), row.names = F)
  }
  
  cl <- parallel::makeCluster(parallel::detectCores()-4, type='PSOCK')
  doParallel::registerDoParallel(cl)
  
  for(j in 1:length(preds)){
    for(k in 1:n){
      
      message(paste("Running replicate number",k,"of", n,"for model",j, "out of", length(preds)))
      
      bg <- dismo::randomPoints(raster::stack(preds[[j]]), n = 50000) %>% 
        
        as.data.frame()
      
      
      total <- bind_rows(occurrence, bg) %>% # combine occurrence and background
        
        raster::extract(raster::stack(scale(preds[[j]])), .) %>% # extract values of raster covariates
        
        as.data.frame() %>% 
        
        bind_cols(bind_rows(occurrence, bg)) %>%
        
        dplyr::mutate(occ = c(rep(1, nrow(occurrence)), rep(0, nrow(bg)))) %>% # create 0 and 1 response
        
        tidyr::drop_na() %>% # drop the NA values
        
        dplyr::mutate(occ = as.factor(occ))
      
      # Create training testing datasets
      train_index <- createDataPartition(total$occ, p = 0.70, list = FALSE)[,1]
      train <- total[train_index,]
      test <- total[-train_index,]
      
      # Control function
      control <- rfeControl(functions = dwsrfRFE,
                            method = "boot",
                            saveDetails = T,
                            number = 25
      )
      
      
      rfe1 <- rfe(x = train[,1:5], 
                  y = train[,8], 
                  sizes = 5, 
                  rfeControl = control,
                  metric = "Accuracy",
                  maximize = "TRUE")
      
      save(rfe1, file = here("data", "biodiversity", "output", "models", 
                             paste0(status,"_",sp,"_",bias.cor,"_rep_",k,"_model_",j,".RData")))
      
      pred_ras <- predict(raster::stack(scale(preds[[j]])), rfe1$fit, type = "prob", index = 2)
      
      writeRaster(pred_ras, here("data", "biodiversity", "output", "reps_sdm", 
                                 paste0(status,"_",sp,"_",bias.cor,"_rep_",k,"_model_",j,".tif")),
                  overwrite = T)
      
      # Post prediction
      pp <- postResample(predict(rfe1, test[,1:ncol(test)-1]), test[,ncol(test)])
      auc <- pp[1]
      kap <- pp[2]
      
      
      eval_df <- data.frame(Species = sp,
                            Model = j,
                            Rep = k,
                            Bias_cor = bias.cor,
                            AUC = auc,
                            KAPPA = kap)
      row.names(eval_df) <- NULL
      
      write.table(eval_df, here("data", "biodiversity", "output", 
                                paste0(status,"_",sp,"_",bias.cor,"_model_evaluations.csv")), 
                  append = T, col.names = F, sep = ",", row.names = F)
      
      #Variable importance
      var_df <- data.frame(Species = rep(sp, nrow(importance(rfe1$fit))),
                           Model = rep(j, nrow(importance(rfe1$fit))),
                           Rep = rep(k, nrow(importance(rfe1$fit))),
                           Bias_cor = rep(bias.cor, nrow(importance(rfe1$fit))),
                           Var_name = row.names(importance(rfe1$fit)),
                           Acc = importance(rfe1$fit)[,3],
                           AccScore = rank(as.vector(importance(rfe1$fit)[,3])))
      row.names(var_df) <- NULL
      
      write.table(var_df, here("data", "biodiversity", "output", 
                               paste0(status,"_",sp,"_",bias.cor,"_variable_importance.csv")), 
                  append = T, col.names = F, sep = ",", row.names = F)
      
    }
  }
  
  parallel::stopCluster(cl)
}

## Prepare variable importance data for plotting----
accuracy_prep <- function(df, species){
  
  acc_df <- df %>% 
    filter(Species == species) %>%
    group_by(Var_name, Cor_group) %>% 
    mutate(med = median(Acc)) %>% 
    arrange(desc(med)) %>%
    relocate(Species) %>% 
    ungroup() %>% 
    distinct(med = med, .keep_all = T) %>%
    mutate(var_rank = rank(med)) %>%
    arrange(Var_name)
  
  return(acc_df)
  
}

## Determine binary cutoff----
get_cutoff <- function(df, species = NULL, sdm, status){
  
  if(status == "genus"){
    
    cutoff <-  df %>%
      
      #dplyr::filter(scientificName == "Promachocrinus kerguelensis") %>%
      
      dplyr::mutate(x = sf::st_coordinates(.)[,1]) %>%
      
      dplyr::mutate(y = sf::st_coordinates(.)[,2]) %>%
      
      dplyr::select(x,y) %>%
      
      sf::st_drop_geometry() %>%
      
      terra::extract(sdm,.) %>% # extract values of raster covariates
      
      as.data.frame() %>%
      
      dplyr::select(!ID) %>%
      
      rename(suitability = names(sdm)) %>%
      
      pull(suitability) %>%
      
      quantile(probs = seq(0, 1, 0.05), na.rm = T) #This way I can play with different thresholds
  }
  
  if(status == "comp"){
    
    cutoff <-  df %>%
      
      dplyr::filter(scientificName == "Promachocrinus kerguelensis" |
                      scientificName == "Promachocrinus joubini" | 
                      scientificName == "Promachocrinus fragarius" |
                      scientificName == "Promachocrinus unruhi" |
                      scientificName == "Promachocrinus uskglass") %>%
      
      dplyr::mutate(x = sf::st_coordinates(.)[,1]) %>%
      
      dplyr::mutate(y = sf::st_coordinates(.)[,2]) %>%
      
      dplyr::select(x,y) %>%
      
      sf::st_drop_geometry() %>%
      
      terra::extract(sdm,.) %>% # extract values of raster covariates
      
      as.data.frame() %>%
      
      dplyr::select(!ID) %>%
      
      rename(suitability = names(sdm)) %>%
      
      pull(suitability) %>%
      
      quantile(probs = seq(0, 1, 0.05), na.rm = T) #This way I can play with different thresholds
  }
  
  if(status == "new"){
    
    cutoff <-  df %>%
      
      dplyr::filter(scientificName == species) %>%
      
      dplyr::mutate(x = sf::st_coordinates(.)[,1]) %>%
      
      dplyr::mutate(y = sf::st_coordinates(.)[,2]) %>%
      
      dplyr::select(x,y) %>%
      
      sf::st_drop_geometry() %>%
      
      terra::extract(sdm,.) %>% # extract values of raster covariates
      
      as.data.frame() %>%
      
      dplyr::select(!ID) %>%
      
      rename(suitability = names(sdm)) %>%
      
      pull(suitability) %>%
      
      quantile(probs = seq(0, 1, 0.05), na.rm = T)
  }
  
  return(cutoff)
}
## Create range raster----
range_rast <- function(df, species, ras_stack, status, thresh = NULL, bin_cut_num){
  
  r_mean <- app(ras_stack, mean)
  
  if(is.null(thresh)){
    
    bin_cut <- get_cutoff(df = df, species = species,sdm = 
                            r_mean, status = status)
    
    r_bin <- ecospat::ecospat.binary.model(r_mean, bin_cut[bin_cut_num])
    
  } else {
    
    r_bin <- ecospat::ecospat.binary.model(r_mean, thresh)
    
  }
  
  return(r_bin)
  
}

## Metric of Fitzpatrick and Turelli 2006 (adapted from ENMTools::geog.range.overlap())----
## calculate mean of the replicates and then create binary on the mean
range.ov <- function(bin_sdm1, bin_sdm2){
  
  overlap.cells <- sum(terra::values(bin_sdm1 * bin_sdm2) == 
                         1, na.rm = TRUE)
  
  min.cells <- min(sum(terra::values(bin_sdm1) == 1, na.rm = TRUE), 
                   sum(terra::values(bin_sdm2) == 1, na.rm = TRUE))
  
  return(overlap.cells/min.cells)
  
}
## Calculate range size metrics----
range_fun <- function(df, species, ras_stack, status, thresh = NULL, bin_cut_num = 1){
  
  eoo <- c()
  aoo <- c()
  
  for(i in 1:nlyr(ras_stack)){
    
    if(is.null(thresh)){
      
      bin_cut <- get_cutoff(df = df, species = species,sdm = 
                              ras_stack[[i]], status = status)
      
      model_bin <- ecospat::ecospat.binary.model(ras_stack[[i]], bin_cut[bin_cut_num])
      
    } else {
      
      model_bin <- ecospat::ecospat.binary.model(ras_stack[[i]], thresh)
    }
    
    model_bin[model_bin == 0] <- NA
    
    model_EOO <- makeEOO(raster::raster(model_bin))
    
    diffPoly <- st_difference(st_as_sf(model_EOO), coast)
    
    #getAreaEOO(diffPoly) is not working properly, thus use st_area() for EOO area
    AreaEOO <- as.vector(st_area(diffPoly)/1e+6) #/1e+6 to get in km2
    
    AOO <- getAOO(raster(model_bin), grid.size = 4000,min.percent.rule = F)
    
    eoo <- c(eoo, AreaEOO)
    aoo <- c(aoo, AOO)
  }
  return(list(eoo,aoo))
}
## Prepare data for raw data range size metrics----
range_prepare <- function(df, status, species){
  
    if(status == "genus"){
      df_sp <- df %>%
        dplyr::mutate(x = sf::st_coordinates(.)[,1]) %>%
        dplyr::mutate(y = sf::st_coordinates(.)[,2]) %>%
        dplyr::select(x,y) %>%
        as_Spatial()
      }
    if(status == "comp"){
      df_sp <- df %>%
        dplyr::filter(scientificName == "Promachocrinus kerguelensis" |
                        scientificName == "Promachocrinus joubini" | 
                        scientificName == "Promachocrinus fragarius" |
                        scientificName == "Promachocrinus unruhi" |
                        scientificName == "Promachocrinus uskglass") %>%
        dplyr::mutate(x = sf::st_coordinates(.)[,1]) %>%
        dplyr::mutate(y = sf::st_coordinates(.)[,2]) %>%
        dplyr::select(x,y) %>%
        as_Spatial()
      }
    if(status == "new"){
      df_sp <- df %>%
        dplyr::filter(scientificName == species) %>%
        dplyr::mutate(x = sf::st_coordinates(.)[,1]) %>%
        dplyr::mutate(y = sf::st_coordinates(.)[,2]) %>%
        dplyr::select(x,y) %>%
        as_Spatial()
    }
  return(df_sp)
}

## Raw range size metrics----
raw_range <- function(raw_df, shp){
  
  raw_EOO <- makeEOO(raw_df)
  diffPoly <- st_difference(st_as_sf(raw_EOO), shp)
  eoo <- as.vector(st_area(diffPoly)/1e+6)
  
  aoo <- getAOO(raw_df, grid.size = 4000,
                min.percent.rule = F)
  
  return(c(eoo = eoo, aoo = aoo))
  
}

## Calculate hypervolumes----
## Calculate hypervolumes
calc_hypervolume <- function(df, status, model, species = NULL){
  
  if(status == "genus"){
    
    occs <- df %>%
      dplyr::select(Longitude, Latitude) %>%
      raster::extract(x = pred_list_sc[[model]],y = .) %>%
      dplyr::select(-ID) %>%
      drop_na()
    
    hv <- hypervolume_gaussian(occs, name='Promachocrinus')
    
    #write hypervolume and variable importance to disk
    saveRDS(hv, here("data", "biodiversity", "output", "hypervolumes", 
                     paste0("hv_P_m",model, ".rds")))
    varimp <- hypervolume_variable_importance(hv)
    saveRDS(varimp, here("data", "biodiversity", "output", "hypervolumes", 
                         paste0("varimp_hv_P_m",50,".rds")))
    
  }
  
  if(status == "comp"){
    
    occs <- df %>%
      dplyr::filter(scientificName == "Promachocrinus kerguelensis" |
                      scientificName == "Promachocrinus joubini" | 
                      scientificName == "Promachocrinus fragarius" |
                      scientificName == "Promachocrinus unruhi" |
                      scientificName == "Promachocrinus uskglass") %>%
      dplyr::select(Longitude, Latitude) %>%
      raster::extract(x = pred_list_sc[[model]],y = .) %>%
      dplyr::select(-ID) %>%
      drop_na()
    
    hv <- hypervolume_gaussian(occs, name='P. kerguelensis (s.l.)')
    
    #write hypervolume and variable importance to disk
    saveRDS(hv, here("data", "biodiversity", "output", "hypervolumes", 
                     paste0("hv_Pk_comp_m",model,".rds")))
    varimp <- hypervolume_variable_importance(hv)
    saveRDS(varimp, here("data", "biodiversity", "output", "hypervolumes", 
                         paste0("varimp_hv_Pk_comp_m",model,".rds")))
    
  }
  
  if(status == "new"){
    
    occs <- df %>%
      dplyr::filter(scientificName == species) %>%
      dplyr::select(Longitude, Latitude) %>%
      raster::extract(x = pred_list_sc[[model]],y = .) %>%
      dplyr::select(-ID) %>%
      drop_na()
  }
  
  hv <- hypervolume_gaussian(occs, name='P. kerguelensis (s.s.)')
  
  sp <- unlist(str_split(sp, " "))
  
  nm <- paste0(
    stringr::str_extract(sp[1], "^.{1}"),
    stringr::str_extract(sp[2], "^.{2}")
  )
  
  #write hypervolume and variable importance to disk
  saveRDS(hv, here("data", "biodiversity", "output", "hypervolumes", 
                   paste0("hv_",nm,"_m",model,".rds")))
  varimp <- hypervolume_variable_importance(hv)
  saveRDS(varimp, here("data", "biodiversity", "output", "hypervolumes", 
                       paste0("varimp_hv_",nm,"_m",model,".rds")))
  
}

## Hypervolume overlap function----
hyper_overlap <- function(hv1, hv2, hv1_name, hv_name2, record = T, path, 
                          n = 200, cores = 12){
  
  perm_name <- paste0(hv1_name,"_",hv2_name,"_permutation")
  
  comparison_path <- hypervolume_permute(perm_name, 
                                         hv1, hv2, n = n, cores = cores)
  
  overlap <- hypervolume_overlap_test(hv1, hv2, comparison_path, cores = cores)
  
  if(record == T){
    
    saveRDS(Pk_comp_Pko_path, here(path, 
                                   paste0(hv1_name,"_",hv2_name,"_path.rds")))
    
    saveRDS(Pk_comp_Pko_ot, here(path, 
                                 paste0(hv1_name,"_",hv2_name,"_ot.rds")))
  }
  
  return(overlap)
  
}


## Future distribution predictions----
fut_clim_rf <- function(data, status, species = NULL, bias.cor, n, preds, 
                        fut_preds, year, scenario, group = c("G1", "G2")){
  
  pred_dws <- list()
  
  if(status == "genus"){
    
    species <- "Promachocrinus kerguelensis"
    sp <- gsub(" ", "_", species)
    
    occurrence <- data %>%
      
      dplyr::mutate(x = sf::st_coordinates(.)[,1]) %>%
      
      dplyr::mutate(y = sf::st_coordinates(.)[,2]) %>%
      
      dplyr::select(x,y) %>%
      
      sf::st_drop_geometry() 
    
    if(bias.cor == T){
      
      occurrence <- occurrence %>% 
        
        dismo::gridSample(r = raster::stack(preds))
      
    } else {
      
      occurrence <- occurrence
    }
    
    
  }
  
  if(status == "comp"){
    
    species <- "Promachocrinus kerguelensis"
    sp <- gsub(" ", "_", species)
    
    occurrence <- data %>%
      
      dplyr::filter(scientificName == "Promachocrinus kerguelensis" |
                      scientificName == "Promachocrinus joubini" | 
                      scientificName == "Promachocrinus fragarius" |
                      scientificName == "Promachocrinus unruhi" |
                      scientificName == "Promachocrinus uskglass") %>%
      
      dplyr::mutate(x = sf::st_coordinates(.)[,1]) %>%
      
      dplyr::mutate(y = sf::st_coordinates(.)[,2]) %>%
      
      dplyr::select(x,y) %>%
      
      sf::st_drop_geometry() 
    
    if(bias.cor == T){
      
      occurrence <- occurrence %>% 
        
        dismo::gridSample(r = raster::stack(preds[[1]]))
      
    } else {
      
      occurrence <- occurrence
    }
  }
  
  if(status == "new"){
    
    occurrence <- data %>%
      
      dplyr::filter(scientificName == species) %>%
      
      dplyr::mutate(x = sf::st_coordinates(.)[,1]) %>%
      
      dplyr::mutate(y = sf::st_coordinates(.)[,2]) %>%
      
      dplyr::select(x,y) %>%
      
      sf::st_drop_geometry() 
    
    if(bias.cor == T){
      
      occurrence <- occurrence %>% 
        
        dismo::gridSample(r = raster::stack(preds[[1]]))
      
    } else {
      
      occurrence <- occurrence
    }
    
    sp <- gsub(" ", "_", species)
    
    
  }
  for(k in 1:n){
    bg <- dismo::randomPoints(raster::stack(preds), n = 50000) %>% 
      
      as.data.frame()
    
    total <- bind_rows(occurrence, bg) %>% # combine occurrence and background
      
      terra::extract(scale(preds), .) %>% # extract values of raster covariates
      
      as.data.frame() %>% 
      
      dplyr::mutate(occ = c(rep(1, nrow(occurrence)), rep(0, nrow(bg)))) %>% # create 0 and 1 response
      
      tidyr::drop_na() %>% # drop the NA values
      
      dplyr::mutate(occ = as.factor(occ)) %>%
      
      dplyr::select(-ID)
    
    
    train_index <- createDataPartition(total$occ, p = 0.70, list = FALSE)[,1]
    train <- total[train_index,]
    write.csv(train, file = here("data","biodiversity","output",
                                 paste0(status,"_",sp,"_", bias.cor,"_rep_",k,"_for_fut_train.csv")))
    
    test <- total[-train_index,]
    write.csv(test, file = here("data","biodiversity","output",
                                paste0(status,"_",sp,"_", bias.cor,"_rep_",k,"_for_fut_test.csv")))
    
    prNum <- as.numeric(table(train$occ)["1"]) # number of presence records
    spsize <- c("0" = prNum, "1" = prNum)
    
    rf <- randomForest::randomForest(occ ~ ., 
                                     data = train, # model fitting on training set
                                     ntree = 1000,
                                     importance = T,
                                     sampsize = spsize,
                                     replace = TRUE)
    save(rf, file = here("data", "biodiversity", "output", "models", 
                         paste0(status,"_",sp,"_",bias.cor,"_rep_",k,"_RF_Model_for_fut.RData")))
    
    print(postResample(predict(rf, test[,1:ncol(test)-1]), test[,ncol(test)]))
    
    cur_ras <- predict(terra::scale(preds), rf, type = "prob", index = 2)
    writeRaster(cur_ras, here("data", "biodiversity", "output", "reps_sdm", 
                              paste0(status,"_",sp,"_",bias.cor,"_rep_",k,"_for_fut_model.tif")),
                overwrite = T)
    
    for(yr in year){
      for(sc in scenario){
        pattern <- paste0(yr,"AOGCM.",sc)
        fut_preds <- subset(fut_clim_all_stack_min, 
                            grep(pattern, 
                                 names(fut_clim_all_stack_min)))
        names(fut_preds) <- gsub(paste0(pattern,".Benthic.Mean.Depth."),"",names(fut_preds))
        
        
        fut_ras <- predict(terra::scale(fut_preds), rf, type = "prob", index = 2)
        writeRaster(fut_ras, here("data", "biodiversity", "output", "reps_sdm", 
                                  paste0(status,"_",sp,"_",bias.cor,"_rep_",k,"_fut_model_",
                                         yr,sc,".tif")), overwrite = T)
      }
    }
  }
}

## Identity tests----
identity.test <- function(species_1_occ, species_2_occ, bg1, bg2, species_1_name, 
                          species_2_name, env, type, nreps = 99,  
                          low.memory = FALSE, rep.dir = NA, verbose = T, 
                          clamp = TRUE, ...){
  
  # species.1 <- check.bg(species.1, env, nback = nback, bg.source = bg.source, verbose = verbose)
  # species.2 <- check.bg(species.2, env, nback = nback, bg.source = bg.source, verbose = verbose)
  # 
  # identity.precheck(species.1, species.2, env, type, f, nreps)
  
  # Initialize a list to store reps in
  replicate.models <- list()
  
  # Set the output directory when low.memory = TRUE
  # if(low.memory == TRUE){
  #   if(is.na(rep.dir)){
  #     rep.dir <- getwd()
  #   }
  #   
  #   if(substr(rep.dir, nchar(rep.dir), nchar(rep.dir)) != "/"){
  #     rep.dir <- paste0(rep.dir, "/")
  #   }
  #   
  #   if(!dir.exists(rep.dir)){
  #     stop(paste("Specified directory for storing replicates cannot be found!\n\n", getwd()))
  #   }
  # }
  
  # For starters we need to combine species background points so that each model
  # is being built with the same background
  # species.1$background.points <- rbind(species.1$background.points, species.2$background.points)
  # species.2$background.points <- rbind(species.1$background.points, species.2$background.points)
  
  total_1 <- bind_rows(species_1_occ, bg1, bg2) %>% # combine occurrence and background
    terra::extract(env, .) %>% # extract values of raster covariates
    as.data.frame() %>% 
    mutate(occ = c(rep(1, nrow(species_1_occ)), rep(0, nrow(bg1)), rep(0, nrow(bg2)))) %>% # create 0 and 1 response
    tidyr::drop_na() %>%
    mutate(occ = as.factor(occ)) %>%
    dplyr::select(-ID)
  
  total_2 <- bind_rows(species_2_occ, bg1, bg2) %>% # combine occurrence and background
    terra::extract(env, .) %>% # extract values of raster covariates
    as.data.frame() %>% 
    mutate(occ = c(rep(1, nrow(species_2_occ)), rep(0, nrow(bg1)), rep(0, nrow(bg2)))) %>% # create 0 and 1 response
    tidyr::drop_na() %>%
    mutate(occ = as.factor(occ)) %>%
    dplyr::select(-ID)
  
  sample_1 <- sample(c(TRUE, FALSE), nrow(total_1), replace=TRUE, prob=c(0.7,0.3))
  train_1  <- total_1[sample_1, ]
  test_1   <- total_1[!sample_1, ]
  
  train_1 %>% dplyr::select(occ) %>% group_by(occ) %>% count() 
  test_1 %>% dplyr::select(occ) %>% group_by(occ) %>% count() 
  
  sample_2 <- sample(c(TRUE, FALSE), nrow(total_2), replace=TRUE, prob=c(0.7,0.3))
  train_2  <- total_2[sample_2, ]
  test_2   <- total_2[!sample_2, ]
  
  train_2 %>% dplyr::select(occ) %>% group_by(occ) %>% count() 
  test_2 %>% dplyr::select(occ) %>% group_by(occ) %>% count() 
  
  prNum_1 <- as.numeric(table(train_1$occ)["1"]) # number of presence records
  spsize_1 <- c("0" = prNum_1, "1" = prNum_1) # sample size for both classes
  
  prNum_2 <- as.numeric(table(train_2$occ)["1"]) # number of presence records
  spsize_2 <- c("0" = prNum_2, "1" = prNum_2) # sample size for both classes
  
  # combined.presence.points <- rbind(species.1$presence.points, species.2$presence.points)
  combined.presence.points <- rbind(species_1_occ, species_2_occ)
  
  # Clamping layers here so it's not done separately for every replicate
  # and setting replicate clmaping to FALSE
  # if(clamp == TRUE){
  #   # Adding env (skipped for BC otherwise)
  #   this.df <- as.data.frame(raster::extract(env, combined.presence.points, ID = FALSE))
  #   
  #   env <- clamp.env(this.df, env)
  # }
  
  # Build models for empirical data
  message("\nBuilding empirical models...\n")
  # if(type == "glm"){
  #   empirical.species.1.model <- enmtools.glm(species.1, env, f, clamp = FALSE, ...)
  #   empirical.species.2.model <- enmtools.glm(species.2, env, f, clamp = FALSE, ...)
  # }
  # 
  # if(type == "gam"){
  #   empirical.species.1.model <- enmtools.gam(species.1, env, f, clamp = FALSE, ...)
  #   empirical.species.2.model <- enmtools.gam(species.2, env, f, clamp = FALSE, ...)
  # }
  # 
  # if(type == "mx"){
  #   empirical.species.1.model <- enmtools.maxent(species.1, env, clamp = FALSE, ...)
  #   empirical.species.2.model <- enmtools.maxent(species.2, env, clamp = FALSE, ...)
  # }
  # 
  # if(type == "bc"){
  #   empirical.species.1.model <- enmtools.bc(species.1, env, clamp = FALSE, ...)
  #   empirical.species.2.model <- enmtools.bc(species.2, env, clamp = FALSE, ...)
  # }
  # 
  # if(type == "dm"){
  #   empirical.species.1.model <- enmtools.dm(species.1, env, clamp = FALSE, ...)
  #   empirical.species.2.model <- enmtools.dm(species.2, env, clamp = FALSE, ...)
  # }
  
  if(type == "rf"){
    #empirical.species.1.model <- enmtools.rf(species.1, env, clamp = FALSE, ...)
    #empirical.species.2.model <- enmtools.rf(species.2, env, clamp = FALSE, ...)
    # RF with down-sampling
    empirical.species.1.model <- randomForest(occ ~ .,
                                              data = train_1,
                                              ntree = 1000,
                                              sampsize = spsize_1,
                                              replace = TRUE) 
    # predict to raster layers
    pred_dws_1 <- predict(env, empirical.species.1.model, type = "prob", index = 2)
    
    empirical.species.2.model <- randomForest(occ ~ .,
                                              data = train_2,
                                              ntree = 1000,
                                              sampsize = spsize_2,
                                              replace = TRUE) 
    # predict to raster layers
    pred_dws_2 <- predict(env, empirical.species.2.model, type = "prob", index = 2)
  }
  
  
  empirical.overlap <- c(unlist(raster.overlap(pred_dws_1, pred_dws_2)),
                         unlist(env.overlap(empirical.species.1.model, empirical.species.2.model, env = env)[1:3]))
  reps.overlap <- empirical.overlap
  
  message("\nBuilding replicate models...\n")
  
  
  if (requireNamespace("progress", quietly = TRUE)) {
    pb <- progress::progress_bar$new(
      format = " [:bar] :percent eta: :eta",
      total = nreps, clear = FALSE, width= 60)
  }
  
  
  for(i in 1:nreps){
    if(verbose == TRUE){message(paste("\nReplicate", i, "...\n"))}
    
    if (requireNamespace("progress", quietly = TRUE)) {
      pb$tick()
    }
    
    
    combined.presence.points <- combined.presence.points[sample(nrow(combined.presence.points)),]
    #rep.species.1 <- species.1
    #rep.species.2 <- species.2
    #rep.species.1$presence.points <- combined.presence.points[1:nrow(species.1$presence.points),]
    rep.species.1 <- combined.presence.points[1:nrow(species_1_occ),]
    #rep.species.2$presence.points <- combined.presence.points[(nrow(species.1$presence.points) + 1):nrow(combined.presence.points),]
    rep.species.2 <- combined.presence.points[(nrow(species_1_occ) + 1):nrow(combined.presence.points),]
    
    rep_total_1 <- bind_rows(rep.species.1, bg1, bg2) %>% # combine occurrence and background
      terra::extract(env, .) %>% # extract values of raster covariates
      as.data.frame() %>% 
      mutate(occ = c(rep(1, nrow(rep.species.1)), rep(0, nrow(bg1)), rep(0, nrow(bg2)))) %>% # create 0 and 1 response
      tidyr::drop_na() %>%
      mutate(occ = as.factor(occ)) %>%
      dplyr::select(-ID)
    
    rep_total_2 <- bind_rows(rep.species.2, bg1, bg2) %>% # combine occurrence and background
      terra::extract(env, .) %>% # extract values of raster covariates
      as.data.frame() %>% 
      mutate(occ = c(rep(1, nrow(rep.species.2)), rep(0, nrow(bg1)), rep(0, nrow(bg2)))) %>% # create 0 and 1 response
      tidyr::drop_na() %>%
      mutate(occ = as.factor(occ)) %>%
      dplyr::select(-ID)
    
    rep_sample_1 <- sample(c(TRUE, FALSE), nrow(rep_total_1), replace=TRUE, prob=c(0.7,0.3))
    rep_train_1  <- rep_total_1[rep_sample_1, ]
    rep_test_1   <- rep_total_1[!rep_sample_1, ]
    
    rep_train_1 %>% dplyr::select(occ) %>% group_by(occ) %>% count() 
    rep_test_1 %>% dplyr::select(occ) %>% group_by(occ) %>% count() 
    
    rep_sample_2 <- sample(c(TRUE, FALSE), nrow(rep_total_2), replace=TRUE, prob=c(0.7,0.3))
    rep_train_2  <- rep_total_2[rep_sample_2, ]
    rep_test_2   <- rep_total_2[!rep_sample_2, ]
    
    rep_train_2 %>% dplyr::select(occ) %>% group_by(occ) %>% count() 
    rep_test_2 %>% dplyr::select(occ) %>% group_by(occ) %>% count() 
    
    rep_prNum_1 <- as.numeric(table(rep_train_1$occ)["1"]) # number of presence records
    rep_spsize_1 <- c("0" = rep_prNum_1, "1" = rep_prNum_1) # sample size for both classes
    
    rep_prNum_2 <- as.numeric(table(rep_train_2$occ)["1"]) # number of presence records
    rep_spsize_2 <- c("0" = rep_prNum_2, "1" = rep_prNum_2)
    
    # Building models for reps
    # if(type == "glm"){
    #   rep.species.1.model <- enmtools.glm(rep.species.1, env, f, clamp = FALSE)
    #   rep.species.2.model <- enmtools.glm(rep.species.2, env, f, clamp = FALSE)
    # }
    # 
    # if(type == "gam"){
    #   rep.species.1.model <- enmtools.gam(rep.species.1, env, f, clamp = FALSE, ...)
    #   rep.species.2.model <- enmtools.gam(rep.species.2, env, f, clamp = FALSE, ...)
    # }
    # 
    # if(type == "mx"){
    #   rep.species.1.model <- enmtools.maxent(rep.species.1, env, clamp = FALSE, ...)
    #   rep.species.2.model <- enmtools.maxent(rep.species.2, env, clamp = FALSE, ...)
    # }
    # 
    # if(type == "bc"){
    #   rep.species.1.model <- enmtools.bc(rep.species.1, env, clamp = FALSE, ...)
    #   rep.species.2.model <- enmtools.bc(rep.species.2, env, clamp = FALSE, ...)
    # }
    # 
    # if(type == "dm"){
    #   rep.species.1.model <- enmtools.dm(rep.species.1, env, clamp = FALSE, ...)
    #   rep.species.2.model <- enmtools.dm(rep.species.2, env, clamp = FALSE, ...)
    # }
    
    if(type == "rf"){
      #rep.species.1.model <- enmtools.rf(rep.species.1, env, clamp = FALSE, ...)
      #rep.species.2.model <- enmtools.rf(rep.species.2, env, clamp = FALSE, ...)
      rep_rf_dws_1 <- randomForest(occ ~ .,
                                   data = rep_train_1,
                                   ntree = 1000,
                                   sampsize = rep_spsize_1,
                                   replace = TRUE) 
      # predict to raster layers
      rep_pred_dws_1 <- predict(env, rep_rf_dws_1, type = "prob", index = 2)
      
      rep_rf_dws_2 <- randomForest(occ ~ .,
                                   data = rep_train_2,
                                   ntree = 1000,
                                   sampsize = rep_spsize_2,
                                   replace = TRUE) 
      # predict to raster layers
      rep_pred_dws_2 <- predict(env, rep_rf_dws_2, type = "prob", index = 2)
      
    }
    
    reps.overlap <- rbind(reps.overlap, c(unlist(raster.overlap(rep_pred_dws_1, rep_pred_dws_2)),
                                          unlist(env.overlap(rep_rf_dws_1, rep_rf_dws_2, env = env)[1:3])))
    
    # Appending models to replicates list
    if(low.memory == TRUE){
      path.1 <- paste0(rep.dir, species_1_name, ".rep.", i, ".Rda")
      path.2 <- paste0(rep.dir, species_2_name, ".rep.", i, ".Rda")
      save(rep_rf_dws_1, file = path.1)
      save(rep_rf_dws_2, file = path.2)
      replicate.models[[paste0(species_1_name, ".rep.", i)]] <- path.1
      replicate.models[[paste0(species_2_name, ".rep.", i)]] <- path.2
      
    } else {
      #replicate.models[[paste0(species.1$species.name, ".rep.", i)]] <- rep.species.1.model
      #replicate.models[[paste0(species.2$species.name, ".rep.", i)]] <- rep.species.2.model
      replicate.models[[paste0(species_1_name, ".rep.", i)]] <- rep_rf_dws_1
      replicate.models[[paste0(species_2_name, ".rep.", i)]] <- rep_rf_dws_2
    }
  }
  
  
  rownames(reps.overlap) <- c("empirical", paste("rep", 1:nreps))
  
  p.values <- apply(reps.overlap, 2, function(x) rank(x)[1]/length(x))
  
  reps.overlap <- as.data.frame(reps.overlap)
  
  d.plot <- ggplot(reps.overlap[2:nrow(reps.overlap),], aes(x = .data$D, fill = "density", alpha = 0.5)) +
    geom_histogram(binwidth = 0.05) +
    geom_vline(xintercept = reps.overlap[1,"D"], linetype = "longdash") +
    xlim(-.05,1.05) + guides(fill = "none", alpha = "none") + xlab("D") +
    theme(plot.title = element_text(hjust = 0.5))
  
  i.plot <- ggplot(reps.overlap[2:nrow(reps.overlap),], aes(x = .data$I, fill = "density", alpha = 0.5)) +
    geom_histogram(binwidth = 0.05) +
    geom_vline(xintercept = reps.overlap[1,"I"], linetype = "longdash") +
    xlim(-.05,1.05) + guides(fill = "none", alpha = "none") + xlab("I") +
    theme(plot.title = element_text(hjust = 0.5))
  
  cor.plot <- ggplot(reps.overlap[2:nrow(reps.overlap),], aes(x = .data$rank.cor, fill = "density", alpha = 0.5)) +
    geom_histogram(binwidth = 0.05) +
    geom_vline(xintercept = reps.overlap[1,"rank.cor"], linetype = "longdash") +
    xlim(-1.05,1.05) + guides(fill = "none", alpha = "none") + xlab("Rank Correlation") +
    theme(plot.title = element_text(hjust = 0.5))
  
  env.d.plot <- ggplot(reps.overlap[2:nrow(reps.overlap),], aes(x = .data$env.D, fill = "density", alpha = 0.5)) +
    geom_histogram(binwidth = 0.05) +
    geom_vline(xintercept = reps.overlap[1,"env.D"], linetype = "longdash") +
    xlim(-.05,1.05) + guides(fill = "none", alpha = "none") + xlab("D, Environmental Space") +
    theme(plot.title = element_text(hjust = 0.5))
  
  env.i.plot <- ggplot(reps.overlap[2:nrow(reps.overlap),], aes(x = .data$env.I, fill = "density", alpha = 0.5)) +
    geom_histogram(binwidth = 0.05) +
    geom_vline(xintercept = reps.overlap[1,"env.I"], linetype = "longdash") +
    xlim(-.05,1.05) + guides(fill = "none", alpha = "none") + xlab("I, Environmental Space") +
    theme(plot.title = element_text(hjust = 0.5))
  
  env.cor.plot <- ggplot(reps.overlap[2:nrow(reps.overlap),], aes(x = .data$env.cor, fill = "density", alpha = 0.5)) +
    geom_histogram(binwidth = 0.05) +
    geom_vline(xintercept = reps.overlap[1,"env.cor"], linetype = "longdash") +
    xlim(-1.05,1.05) + guides(fill = "none", alpha = "none") + xlab("Rank Correlation, Environmental Space") +
    theme(plot.title = element_text(hjust = 0.5))
  
  # mean(id.dm[,1] > id.dm[1,1])
  
  output <- list(description = paste("Identity test:", species_1_name, "vs.", species_2_name),
                 reps.overlap = reps.overlap,
                 p.values = p.values,
                 empirical.species.1.model = empirical.species.1.model,
                 empirical.species.2.model = empirical.species.2.model,
                 replicate.models = replicate.models,
                 d.plot = d.plot,
                 i.plot = i.plot,
                 cor.plot = cor.plot,
                 env.d.plot = env.d.plot,
                 env.i.plot = env.i.plot,
                 env.cor.plot = env.cor.plot)
  
  #class(output) <- "enmtools.identity.test"
  
  return(output)
  
}