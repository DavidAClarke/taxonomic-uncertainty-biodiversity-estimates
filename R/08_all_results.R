################################################################################
## Script name: 08_all_results.R
################################################################################

## Predictor variable combinations
pred_combs <- read.csv(here("data", "environmental", "pred_vars.csv"))

# Union of coast for EOO calculations
coast <- st_read(here(shared_data, "coastline", "Coastline.shp")) %>%
  st_transform("ESRI:102019")  %>% 
  st_union()

## 3.1 | Species distribution modelling----
mod_files <- list.files(res_path)

# Evaluation
eval_files <- mod_files[str_detect(mod_files, "evaluation")]

eval_list <- list()

for(i in 1:length(eval_files)){
  
  mod_eval <- read.csv(here(res_path, eval_files[i]))
  
  if(str_detect(eval_files[i], "old")){
    
    mod_eval <- mod_eval %>% mutate(Species = "Promachocrinus")
  }  
  
  if(str_detect(eval_files[i], "comp")){
    
    mod_eval <- mod_eval %>% mutate(Species = "P. kerguelensis (s.l.)")
  } 
  
  if(str_detect(eval_files[i], "new")){
    
    mod_eval <- mod_eval %>% mutate(Species = paste("P.", str_split(eval_files[i], "_")[[1]][3]))
  }
  
  eval_list[[i]] <- mod_eval
}

mod_evals <- bind_rows(eval_list) %>%
  
  mutate(Species = factor(Species, levels = c("Promachocrinus", 
                                              "P. kerguelensis (s.l.)", 
                                              "P. kerguelensis",
                                              "P. fragarius", "P. unruhi",
                                              "P. uskglass", "P. joubini",
                                              "P. mawsoni"))) %>%
  arrange(Species) %>%
  filter(Species != "Promachocrinus")

auc_plot <- ggplot(mod_evals, aes(x=Species, y=AUC, fill=Species, group = Species)) + 
  geom_boxplot() +
  theme_bw() +
  theme(axis.text.x = element_blank(),
        axis.title.x = element_blank(),
        axis.ticks.x = element_blank(),
        legend.position = "top",
        legend.text = element_text(size = 12),
        legend.title = element_text(size = 14),
        axis.text = element_text(size = 14),
        axis.title = element_text(size = 16)) +
  scale_fill_manual(values = unlist(cartocolors[cartocolors$Name == "Safe",8]),
                    breaks = unique(mod_evals$Species),
                    labels = c(expression(italic("P. kerguelensis (s.l.)")),
                               expression(italic("P. kerguelensis")),
                               expression(italic("P. fragarius")),
                               expression(italic("P. unruhi")),
                               expression(italic("P. uskglassi")),
                               expression(italic("P. joubini")),
                               expression(italic("P. mawsoni"))))


kappa_plot <- ggplot(mod_evals, aes(x=Species, y=KAPPA, fill=Species, group = Species)) + 
  geom_boxplot() +
  theme_bw() +
  theme(axis.text.x = element_blank(),
        axis.title.x = element_blank(),
        axis.ticks.x = element_blank(),
        legend.position = "top",
        legend.text = element_text(size = 12),
        legend.title = element_text(size = 14),
        axis.text = element_text(size = 14),
        axis.title = element_text(size = 16)) +
  scale_fill_manual(values = unlist(cartocolors[cartocolors$Name == "Safe",8]),
                    breaks = unique(mod_evals$Species),
                    labels = c(expression(italic("P. kerguelensis (s.l.)")),
                               expression(italic("P. kerguelensis")),
                               expression(italic("P. fragarius")),
                               expression(italic("P. unruhi")),
                               expression(italic("P. uskglassi")),
                               expression(italic("P. joubini")),
                               expression(italic("P. mawsoni"))))

# Sensitivity and specificity
files <- list.files(model_path)[!str_detect(list.files(model_path),"fut")]

sens <- c()
specif <- c()

for(f in files){
  
  mod <- get(load(here(model_path, f)))
  se <- 1-mod$fit$confusion[2,3]
  sens <- c(sens, se)
  sp <- 1-mod$fit$confusion[1,3]
  specif <- c(specif, sp)
  
}

sens_spec <- data.frame(fl = files,
                 sens = sens,
                 specif = specif)

write.csv(sens_spec, here("data", "biodiversity", "output", "sens_spec_df.csv"))
sens_spec <- read.csv(here("data", "biodiversity", "output", "sens_spec_df.csv"))

sens_spec_split <- str_split(sens_spec[,2], "_")

species <- c()
rep <- c()
model <- c()
status <- c()

for(i in 1:length(sens_spec_split)){
  
  sp <- paste(sens_spec_split[[i]][2], sens_spec_split[[i]][3])
  species <- c(species, sp)
  r <- sens_spec_split[[i]][6]
  rep <- c(rep, r)
  m <- parse_number(sens_spec_split[[i]][8])
  model <- c(model, m)
  s <- sens_spec_split[[i]][1]
  status <- c(status, s)
  
}

sens_spec_df <- sens_spec %>%
  mutate(status = status) %>%
  mutate(species = species) %>%
  mutate(model = model) %>%
  mutate(rep = rep) %>%
  mutate(species = case_when(status == "old" ~ "Promachocrinus",
                             status == "comp" ~ "P. kerguelensis (s.l.)", 
                             .default = species)) %>%
  mutate(species = str_replace(species, "romachocrinus", ".")) %>%
  mutate(species = factor(species, levels = c("P.",
                                              "P. kerguelensis (s.l.)", 
                                              "P. kerguelensis",
                                              "P. fragarius", "P. unruhi",
                                              "P. uskglass", "P. joubini",
                                              "P. mawsoni"))) %>%
  arrange(species) %>%
  filter(status != "old")


sens_plot <- ggplot(sens_spec_df, aes(x=species, y=sens, fill=species, group = species)) + 
  geom_boxplot() +
  theme_bw() +
  theme(axis.text.x = element_blank(),
        axis.title.x = element_blank(),
        axis.ticks.x = element_blank(),
        legend.position = "top",
        legend.text = element_text(size = 12),
        legend.title = element_text(size = 14),
        axis.text = element_text(size = 14),
        axis.title = element_text(size = 16)) +
  scale_fill_manual(values = unlist(cartocolors[cartocolors$Name == "Safe",8]),
                    breaks = unique(mod_evals$Species),
                    labels = c(expression(italic("P. kerguelensis (s.l.)")),
                               expression(italic("P. kerguelensis")),
                               expression(italic("P. fragarius")),
                               expression(italic("P. unruhi")),
                               expression(italic("P. uskglassi")),
                               expression(italic("P. joubini")),
                               expression(italic("P. mawsoni")))) +
  ylab("Sensitivity")

spec_plot <- ggplot(sens_spec_df, aes(x=species, y=specif, fill=species, group = species)) + 
  geom_boxplot() +
  theme_bw() +
  theme(axis.text.x = element_blank(),
        axis.title.x = element_blank(),
        axis.ticks.x = element_blank(),
        legend.position = "top",
        legend.text = element_text(size = 12),
        legend.title = element_text(size = 14),
        axis.text = element_text(size = 14),
        axis.title = element_text(size = 16)) +
  scale_fill_manual(values = unlist(cartocolors[cartocolors$Name == "Safe",8]),
                    breaks = unique(mod_evals$Species),
                    labels = c(expression(italic("P. kerguelensis (s.l.)")),
                               expression(italic("P. kerguelensis")),
                               expression(italic("P. fragarius")),
                               expression(italic("P. unruhi")),
                               expression(italic("P. uskglassi")),
                               expression(italic("P. joubini")),
                               expression(italic("P. mawsoni")))) +
  ylab("Specificity")

eval_plot <- ggpubr::ggarrange(auc_plot, kappa_plot, sens_plot, spec_plot,
                  labels = c("A", "B", "C", "D"),
                  ncol = 2, nrow = 2, common.legend = T)

# Variable importance
varimp_files <- mod_files[str_detect(mod_files, "importance")]

varimp_list <- list()

for(i in 1:length(varimp_files)){
  
  mod_varimp <- read.csv(here(res_path, varimp_files[i]))
  
  if(str_detect(varimp_files[i], "old")){
    
    mod_varimp <- mod_varimp %>% mutate(Species = "Promachocrinus")
  }  
  
  if(str_detect(varimp_files[i], "comp")){
    
    mod_varimp <- mod_varimp %>% mutate(Species = "P. kerguelensis (s.l.)")
  } 
  
  if(str_detect(varimp_files[i], "new")){
    
    mod_varimp <- mod_varimp %>% mutate(Species = paste("P.", str_split(varimp_files[i], "_")[[1]][3]))
  }
  
  varimp_list[[i]] <- mod_varimp
}

mod_varimps <- bind_rows(varimp_list) %>%
  
  mutate(Species = factor(Species, levels = c("Promachocrinus", 
                                              "P. kerguelensis (s.l.)", 
                                              "P. kerguelensis",
                                              "P. fragarius", "P. unruhi",
                                              "P. uskglass", "P. joubini",
                                              "P. mawsoni"))) %>%
  
  mutate(Cor_group = case_when(Var_name %in% pred_combs[,1] ~ "A",
                               Var_name %in% pred_combs[,2] ~ "B",
                               Var_name %in% pred_combs[,3] ~ "C",
                               Var_name %in% pred_combs[,4] ~ "D",
                               Var_name %in% pred_combs[,5] ~ "E")) %>%
  
  mutate(Var_name = ifelse(str_detect(Var_name,"Depth"), 
                           str_split_i(Var_name,"Depth.", 2),"Depth")) %>%
  
  mutate(Var_name = gsub(".Lt.min", " (Lt. min)", Var_name)) %>%
  
  mutate(Var_name = gsub(".Lt.max", " (Lt. max)", Var_name)) %>%
  
  mutate(Var_name = gsub(".Range", " (Range)", Var_name)) %>%
  
  mutate(Var_name = gsub(".Mean", " (mean)", Var_name)) %>%
  
  mutate(Var_name = gsub(".Min", " (Min)", Var_name)) %>%
  
  mutate(Var_name = gsub("Dissolved.oxygen", "Dissolved oxygen", Var_name)) %>%
  
  mutate(Var_name = gsub("Current.Velocity", "Current velocity", Var_name))

varimp_boxplot <- ggplot(mod_varimps, aes(x=Var_name, y=Acc, fill = Cor_group)) + 
                  geom_boxplot(alpha = 0.6) +
                  facet_wrap(~Species, scale = "free", nrow = 2) +
                  theme_bw() +
                  coord_flip() +
                  ylab("Mean decrease in accuracy") +
                  xlab("Predictor variable") +
                  scale_fill_discrete(name = "Correlation\ngroup") +
                  theme(strip.text = element_text(face = "italic", size = 7))

accuracy_p <- accuracy_prep(mod_varimps, "Promachocrinus") %>%
  mutate(Var_name = str_replace_all(
    Var_name, 
    setNames(c("A", "B", "C", "D", "E"), 
             c("Lt. max", "Min", "Lt. min", "mean", "Range"))))
accuracy_pk_comp <- accuracy_prep(mod_varimps, "P. kerguelensis (s.l.)") %>%
  mutate(Var_name = str_replace_all(
    Var_name, 
    setNames(c("A", "B", "C", "D", "E"), 
             c("Lt. max", "Min", "Lt. min", "mean", "Range"))))
accuracy_pk <- accuracy_prep(mod_varimps, "P. kerguelensis") %>%
  mutate(Var_name = str_replace_all(
    Var_name, 
    setNames(c("A", "B", "C", "D", "E"), 
             c("Lt. max", "Min", "Lt. min", "mean", "Range"))))
accuracy_pf <- accuracy_prep(mod_varimps, "P. fragarius") %>%
  mutate(Var_name = str_replace_all(
    Var_name, 
    setNames(c("A", "B", "C", "D", "E"), 
             c("Lt. max", "Min", "Lt. min", "mean", "Range"))))
accuracy_pu <- accuracy_prep(mod_varimps, "P. unruhi") %>%
  mutate(Var_name = str_replace_all(
    Var_name, 
    setNames(c("A", "B", "C", "D", "E"), 
             c("Lt. max", "Min", "Lt. min", "mean", "Range"))))
accuracy_pus <- accuracy_prep(mod_varimps, "P. uskglass") %>%
  mutate(Var_name = str_replace_all(
    Var_name, 
    setNames(c("A", "B", "C", "D", "E"), 
             c("Lt. max", "Min", "Lt. min", "mean", "Range"))))
accuracy_pj <- accuracy_prep(mod_varimps, "P. joubini") %>%
  mutate(Var_name = str_replace_all(
    Var_name, 
    setNames(c("A", "B", "C", "D", "E"), 
             c("Lt. max", "Min", "Lt. min", "mean", "Range"))))
accuracy_pm <- accuracy_prep(mod_varimps, "P. mawsoni") %>%
  mutate(Var_name = str_replace_all(
    Var_name, 
    setNames(c("A", "B", "C", "D", "E"), 
             c("Lt. max", "Min", "Lt. min", "mean", "Range"))))

cor_df <- data.frame(accuracy_pk_comp$var_rank,
                     accuracy_pk$var_rank,
                     accuracy_pf$var_rank,
                     accuracy_pu$var_rank,
                     accuracy_pus$var_rank,
                     accuracy_pj$var_rank,
                     accuracy_pm$var_rank)
names(cor_df) <- levels(mod_varimps$Species)[-1]
cor_mat <- Hmisc::rcorr(as.matrix(cor_df))

var_imp_cor <- corrplot::corrplot(cor_mat[[1]], 
                   type = "lower", 
                   #addCoef.col = "white",
                   tl.srt = 45,
                   tl.col = "black", 
                   col = corrplot::COL1("Blues"),
                   tl.pos = "n",
                   tl.cex = 2,
                   cl.cex = 2,
                   number.cex = 2,
                   is.corr = F,
                   col.lim = c(0,1))

pk_comp_nl <- accuracy_pk_comp %>% 
  arrange(-var_rank) %>% 
  pull(Var_name)

var_df <- bind_rows(accuracy_pk_comp, accuracy_pk, accuracy_pf, accuracy_pu,
                    accuracy_pus, accuracy_pj, accuracy_pm) %>%
  mutate(Species = factor(Species, levels = c("P. kerguelensis (s.l.)",
                                              "P. kerguelensis",
                                              "P. fragarius",
                                              "P. unruhi",
                                              "P. uskglass",
                                              "P. joubini",
                                              "P. mawsoni"))) %>%
  mutate(Var_name = factor(Var_name, levels = c(pk_comp_nl))) 

ggplot(var_df, aes(x = Var_name, y = var_rank, fill = Cor_group))+
  geom_bar(stat = "identity") +
  scale_x_discrete(limits = rev) +
  facet_wrap(~Species, ncol = 2) +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 0),
        legend.position = "none",
        legend.text = element_text(size = 12),
        legend.title = element_text(size = 14),
        axis.text = element_text(size = 12),
        axis.title = element_text(size = 16),
        strip.text = element_text(face = "italic", size = 14)) +
  coord_flip() +
  scale_fill_viridis_d(option = "rocket", direction = 1)+
  xlab("Variable") +
  ylab("Variable rank")

## 3.2 | Range size----
# Current
#Use model 50 for niche coparisons, different models for range sizes
lyrs <- list.files(here("data", "biodiversity", "output", "reps_sdm"), 
                   pattern = "model_50", full.names = T)
pk_comp <- list.files(here("data", "biodiversity", "output", "reps_sdm"), 
                   pattern = glob2rx("*comp*model_16*"), full.names = T)

sdm_list <- list()

pk_comp <- lyrs[str_detect(lyrs,"comp")]
pk_comp <- pk_comp[str_detect(pk_comp, "FALSE")]
sdm_list[["comp"]] <- rast(pk_comp)

new_species <- lyrs[str_detect(lyrs,"new")]

species_names <- c("kerguelensis", "fragarius", "unruhi", "uskglass", #uskglassi
                   "joubini", "mawsoni")

species_models <- c(72,40,50,54,54,5) #model with highest mean AUC for each species

for(spec in species_names){
  
  sp <- new_species[str_detect(new_species,spec)]
  sdm_list[[spec]] <- rast(sp)
  
}

## Only for range sizes, not niche comparisons
for(i in seq_along(species_names)){
  
  #sp <- new_species[str_detect(new_species,glob2rx(paste0("*",species_names[i],"*",paste0("model_",species_models[i],"*"))))]
  sp_lyrs <- list.files(here(dirname(here()),"data", "biodiversity", "output", "reps_sdm"), 
                           pattern = glob2rx(paste0("*new*",species_names[i],"*",paste0("model_",species_models[i],"."),"*")), full.names = T)
  sdm_list[[species_names[i]]] <- rast(sp_lyrs)
  
}

## Calculate range size metrics for each species. Using convertToPA function within
# range_fun(). Default beta value of 0.5.

betas <- c(0.5, 0.75)

for(n in betas){
  
  rs_eoo <- c()
  rs_aoo <- c()
  
  for(i in seq_along(sdm_list)){
    
    rs_list <- range_fun(sdm_list[[i]], beta = n)
    rs_eoo <- c(rs_eoo, rs_list[[1]])
    rs_aoo <- c(rs_aoo, rs_list[[2]])
    
  }
  
  save(rs_eoo, file = here(dirname(here()),"data", "biodiversity", "output", "range_size",
                           paste0("rs_eoo_b_",n,"_diff_models.RData")))
  save(rs_aoo, file = here(dirname(here()),"data", "biodiversity", "output", "range_size",
                           paste0("rs_aoo_b_",n,"_diff_models.RData")))
  
}

rs_eoo_b0.5 <- loadRData(file = here(dirname(here()),"data", "biodiversity", "output", "range_size",
                 paste0("rs_eoo_b_",0.5,".RData")))
rs_eoo_b0.75 <- loadRData(file = here(dirname(here()),"data", "biodiversity", "output", "range_size",
                                     paste0("rs_eoo_b_",0.75,".RData")))
rs_aoo_b0.5 <- loadRData(file = here(dirname(here()),"data", "biodiversity", "output", "range_size",
                                     paste0("rs_aoo_b_",0.5,".RData")))
rs_aoo_b0.75 <- loadRData(file = here(dirname(here()),"data", "biodiversity", "output", "range_size",
                                      paste0("rs_aoo_b_",0.75,".RData")))


# ## Create range rasters
# pk_comp_bin <- range_rast(bio_data, "Promachocrinus kerguelensis", 
#                           sdm_list[[1]], status = "comp", bin_cut_num = 1)
# pk_bin <- range_rast(bio_data, "Promachocrinus kerguelensis", sdm_list[[2]], 
#                      status = "new", bin_cut_num = 1)
# pf_bin <- range_rast(bio_data, "Promachocrinus fragarius", sdm_list[[3]], 
#                      status = "new", bin_cut_num = 1)
# pu_bin <- range_rast(bio_data, "Promachocrinus unruhi", sdm_list[[4]], 
#                      status = "new", bin_cut_num = 1)
# pus_bin <- range_rast(bio_data, "Promachocrinus uskglassi", sdm_list[[5]], 
#                       status = "new", bin_cut_num = 1)
# pj_bin <- range_rast(bio_data, "Promachocrinus joubini", sdm_list[[6]], 
#                      status = "new", bin_cut_num = 1)
# pm_bin <- range_rast(bio_data, "Promachocrinus mawsoni", sdm_list[[7]], 
#                      status = "new", bin_cut_num = 1)
# 
# bin_stack <- c(pk_comp_bin, pk_bin, pf_bin, pu_bin, pus_bin, pj_bin, pm_bin)
# 
# overlaps <- c()
# for(i in 1:nlyr(bin_stack)){
#   for(j in 1:nlyr(bin_stack)){
#     
#     ov <- range.ov(bin_stack[[i]], bin_stack[[j]])
#     overlaps <- c(overlaps, ov)
#     
#   }
# }
# 
# ov_mat <- matrix(overlaps, nrow = 7, ncol = 7)
# colnames(ov_mat) <- c("Promachocrinus kerguelensis (s.l.)", "Promachocrinus kerguelensis",
#                       "Promachocrinus fragarius", "Promachocrinus unruhi",
#                       "Promachocrinus uskglassi", "Promachocrinus joubini",
#                       "Promachocrinus mawsoni")
# rownames(ov_mat) <- c("Promachocrinus kerguelensis (s.l.)", "Promachocrinus kerguelensis",
#                       "Promachocrinus fragarius", "Promachocrinus unruhi",
#                       "Promachocrinus uskglassi", "Promachocrinus joubini",
#                       "Promachocrinus mawsoni")

# ## Range size metrics using cut offs
# pk_comp_range_mtp <- range_fun(bio_data, "Promachocrinus kerguelensis", 
#                                sdm_list[[1]], status = "comp", bin_cut_num = 1)
# pk_comp_range_p10 <- range_fun(bio_data, "Promachocrinus kerguelensis", 
#                                sdm_list[[1]], status = "comp", bin_cut_num = 3)
# pk_range_mtp <- range_fun(bio_data, "Promachocrinus kerguelensis", 
#                           sdm_list[[2]], status = "new", bin_cut_num = 1)
# pk_range_p10 <- range_fun(bio_data, "Promachocrinus kerguelensis", 
#                           sdm_list[[2]], status = "new", bin_cut_num = 3)
# pf_range_mtp <- range_fun(bio_data, "Promachocrinus fragarius", 
#                           sdm_list[[3]], status = "new", bin_cut_num = 1)
# pf_range_p10 <- range_fun(bio_data, "Promachocrinus fragarius", 
#                           sdm_list[[3]], status = "new", bin_cut_num = 3)
# pu_range_mtp <- range_fun(bio_data, "Promachocrinus unruhi", 
#                           sdm_list[[4]], status = "new", bin_cut_num = 1)
# pu_range_p10 <- range_fun(bio_data, "Promachocrinus unruhi", 
#                           sdm_list[[4]], status = "new", bin_cut_num = 3)
# pus_range_mtp <- range_fun(bio_data, "Promachocrinus uskglassi", 
#                            sdm_list[[5]], status = "new", bin_cut_num = 1)
# pus_range_p10 <- range_fun(bio_data, "Promachocrinus uskglassi", 
#                            sdm_list[[5]], status = "new", bin_cut_num = 3)
# pj_range_mtp <- range_fun(bio_data, "Promachocrinus joubini", 
#                           sdm_list[[6]], status = "new", bin_cut_num = 1)
# pj_range_p10 <- range_fun(bio_data, "Promachocrinus joubini", 
#                           sdm_list[[6]], status = "new", bin_cut_num = 3)
# pm_range_mtp <- range_fun(bio_data, "Promachocrinus mawsoni", 
#                           sdm_list[[7]], status = "new", bin_cut_num = 1)
# pm_range_p10 <- range_fun(bio_data, "Promachocrinus mawsoni", 
#                           sdm_list[[7]], status = "new", bin_cut_num = 3)
# 
# ## Range size metrics using thresholds
# pk_comp_range_0.5 <- range_fun(bio_data, "Promachocrinus kerguelensis", 
#                            sdm_list[[1]], status = "comp", thresh = 0.5)
# pk_comp_range_0.75 <- range_fun(bio_data, "Promachocrinus kerguelensis", 
#                             sdm_list[[1]], status = "comp", thresh = 0.75)
# pk_range_0.5 <- range_fun(bio_data, "Promachocrinus kerguelensis", 
#                           sdm_list[[2]], status = "new", thresh = 0.5)
# pk_range_0.75 <- range_fun(bio_data, "Promachocrinus kerguelensis", 
#                            sdm_list[[2]], status = "new", thresh = 0.75)
# pf_range_0.5 <- range_fun(bio_data, "Promachocrinus fragarius", 
#                           sdm_list[[3]], status = "new", thresh = 0.5)
# pf_range_0.75 <- range_fun(bio_data, "Promachocrinus fragarius", 
#                            sdm_list[[3]], status = "new", thresh = 0.75)
# pu_range_0.5 <- range_fun(bio_data, "Promachocrinus unruhi", 
#                           sdm_list[[4]], status = "new", thresh = 0.5)
# pu_range_0.75 <- range_fun(bio_data, "Promachocrinus unruhi", 
#                            sdm_list[[4]], status = "new", thresh = 0.75)
# pus_range_0.5 <- range_fun(bio_data, "Promachocrinus uskglassi", 
#                            sdm_list[[5]], status = "new", thresh = 0.5)
# pus_range_0.75 <- range_fun(bio_data, "Promachocrinus uskglassi", 
#                             sdm_list[[5]], status = "new", thresh = 0.75)
# pj_range_0.5 <- range_fun(bio_data, "Promachocrinus joubini", 
#                           sdm_list[[6]], status = "new", thresh = 0.5)
# pj_range_0.75 <- range_fun(bio_data, "Promachocrinus joubini", 
#                            sdm_list[[6]], status = "new", thresh = 0.75)
# pm_range_0.5 <- range_fun(bio_data, "Promachocrinus mawsoni", 
#                           sdm_list[[7]], status = "new", thresh = 0.5)
# pm_range_0.75 <- range_fun(bio_data, "Promachocrinus mawsoni", 
#                            sdm_list[[7]], status = "new", thresh = 0.75)
# 
# ## Collect EOO values and write to disk
# eoo_vals_mtp <- c(pk_comp_range_mtp[[1]], pk_range_mtp[[1]], pf_range_mtp[[1]], 
#                   pu_range_mtp[[1]],  pus_range_mtp[[1]], pj_range_mtp[[1]], 
#                   pm_range_mtp[[1]])
# eoo_vals_p10 <- c(pk_comp_range_p10[[1]], pk_range_p10[[1]], pf_range_p10[[1]], 
#                   pu_range_p10[[1]],  pus_range_p10[[1]], pj_range_p10[[1]], 
#                   pm_range_p10[[1]])
# eoo_vals_0.5 <- c(pk_comp_range_0.5[[1]], pk_range_0.5[[1]], pf_range_0.5[[1]], 
#                   pu_range_0.5[[1]], pus_range_0.5[[1]], pj_range_0.5[[1]], 
#                   pm_range_0.5[[1]])
# eoo_vals_0.75 <- c(pk_comp_range_0.75[[1]], pk_range_0.75[[1]], pf_range_0.75[[1]], 
#                    pu_range_0.75[[1]], pus_range_0.75[[1]], pj_range_0.75[[1]], 
#                    pm_range_0.75[[1]])
# 
# save(eoo_vals_mtp, file = here("data", "biodiversity", "output", "range_size",
#                                "eoo_vals_mtp.RData"))
# save(eoo_vals_p10, file = here("data", "biodiversity", "output", "range_size",
#                                "eoo_vals_p10.RData"))
# save(eoo_vals_0.5, file = here("data", "biodiversity", "output", "range_size",
#                                "eoo_vals_0.5.RData"))
# save(eoo_vals_0.75, file = here("data", "biodiversity", "output", "range_size",
#                                 "eoo_vals_0.75.RData"))
# 
# eoo_vals_mtp <- get(load(here("data", "biodiversity", "output", "range_size",
#                               "eoo_vals_mtp.RData")))
# eoo_vals_0.5 <- get(load(here("data", "biodiversity", "output","range_size",
#                               "eoo_vals_0.5.RData")))
# eoo_vals_0.75 <- get(load(here("data", "biodiversity", "output","range_size",
#                               "eoo_vals_0.75.RData")))
# 
# ## Collect AOO values and write to disk
# aoo_vals_mtp <- c(pk_comp_range_mtp[[2]], pk_range_mtp[[2]], pf_range_mtp[[2]], 
#                   pu_range_mtp[[2]], pus_range_mtp[[2]], pj_range_mtp[[2]], 
#                   pm_range_mtp[[2]])
# aoo_vals_p10 <- c(pk_comp_range_p10[[2]], pk_range_p10[[2]], pf_range_p10[[2]], 
#                   pu_range_p10[[2]], pus_range_p10[[2]], pj_range_p10[[2]], 
#                   pm_range_p10[[2]])
# aoo_vals_0.5 <- c(pk_comp_range_0.5[[2]], pk_range_0.5[[2]], pf_range_0.5[[2]], 
#                   pu_range_0.5[[2]], pus_range_0.5[[2]], pj_range_0.5[[2]], 
#                   pm_range_0.5[[2]])
# aoo_vals_0.75 <- c(pk_comp_range_0.75[[2]], pk_range_0.75[[2]], pf_range_0.75[[2]], 
#                    pu_range_0.75[[2]], pus_range_0.75[[2]], pj_range_0.75[[2]], 
#                    pm_range_0.75[[2]])
# 
# save(aoo_vals_mtp, file = here("data", "biodiversity", "output", "range_size",
#                                "aoo_vals_mtp.RData"))
# save(aoo_vals_p10, file = here("data", "biodiversity", "output", "range_size",
#                                "aoo_vals_p10.RData"))
# save(aoo_vals_0.5, file = here("data", "biodiversity", "output", "range_size",
#                                "aoo_vals_0.5.RData"))
# save(aoo_vals_0.75, file = here("data", "biodiversity", "output", "range_size",
#                                 "aoo_vals_0.75.RData"))
# 
# aoo_vals_mtp <- get(load(here("data", "biodiversity", "output","range_size",
#                               "aoo_vals_mtp.RData")))
# aoo_vals_0.5 <- get(load(here("data", "biodiversity", "output","range_size",
#                               "aoo_vals_0.5.RData")))
# aoo_vals_0.75 <- get(load(here("data", "biodiversity", "output","range_size",
#                               "aoo_vals_0.75.RData")))

species <- c(rep("P. kerguelensis (s.l.)",20), 
             rep("P. kerguelensis",20),
             rep("P. fragarius",20),
             rep("P. unruhi",20),
             rep("P. uskglassi",20),
             rep("P. joubini",20),
             rep("P. mawsoni",20))

# rl_df <- data.frame(species, eoo_vals_mtp, eoo_vals_0.5, eoo_vals_0.75,
#                     aoo_vals_mtp, aoo_vals_0.5, aoo_vals_0.75)
rl_df <- data.frame(species, 
                    rs_eoo_b0.5, rs_aoo_b0.5, 
                    rs_eoo_b0.75, rs_aoo_b0.75) %>%
  mutate(species = factor(species, levels = c("P. kerguelensis (s.l.)", "P. kerguelensis",
                                              "P. fragarius", "P. unruhi",
                                              "P. uskglassi", "P. joubini",
                                              "P. mawsoni")))

rl_df_b05 <- rl_df %>%
  group_by(species) %>%
  dplyr::select(species, rs_eoo_b0.5, rs_aoo_b0.5) %>%
  mutate(med_eoo = median(rs_eoo_b0.5)) %>%
  mutate(mean_eoo = mean(rs_eoo_b0.5)) %>%
  mutate(sd_eoo = sqrt(var(rs_eoo_b0.5))) %>%
  mutate(med_aoo = median(rs_aoo_b0.5)) %>%
  mutate(mean_aoo = mean(rs_aoo_b0.5)) %>%
  mutate(sd_aoo = sqrt(var(rs_aoo_b0.5))) 

eoo_b05_es <- rcompanion::multiVDA(rs_eoo_b0.5 ~ species, data = rl_df_b05)$pairs
write.csv(eoo_b05_es, file = here(dirname(here()), "data", "biodiversity", "output",
                                  "eoo_b05_es.csv"))
aoo_b05_es <- rcompanion::multiVDA(rs_aoo_b0.5 ~ species, data = rl_df_b05)$pairs
write.csv(aoo_b05_es, file = here(dirname(here()), "data", "biodiversity", "output",
                                  "aoo_b05_es.csv"))

rl_df_b075 <- rl_df %>%
  group_by(species) %>%
  dplyr::select(species, rs_eoo_b0.75, rs_aoo_b0.75) %>%
  mutate(med_eoo = median(rs_eoo_b0.5)) %>%
  mutate(mean_eoo = mean(rs_eoo_b0.5)) %>%
  mutate(sd_eoo = sqrt(var(rs_eoo_b0.5))) %>%
  mutate(med_aoo = median(rs_aoo_b0.5)) %>%
  mutate(mean_aoo = mean(rs_aoo_b0.5)) %>%
  mutate(sd_aoo = sqrt(var(rs_aoo_b0.5))) 

eoo_b075_es <- rcompanion::multiVDA(rs_eoo_b0.75 ~ species, data = rl_df_b075)$pairs
write.csv(eoo_b075_es, file = here(dirname(here()), "data", "biodiversity", "output",
                                  "eoo_b075_es.csv"))
aoo_b075_es <- rcompanion::multiVDA(rs_aoo_b0.75 ~ species, data = rl_df_b075)$pairs
write.csv(aoo_b075_es, file = here(dirname(here()), "data", "biodiversity", "output",
                                   "aoo_b075_es.csv"))



# rl_eoo <- rl_df_mtp %>% filter(name == "eoo_vals_mtp")
# rl_eoo_0.5 <- rl_df_0.5 %>% filter(name == "eoo_vals_0.5")
# rl_eoo_0.75 <- rl_df_0.75 %>% filter(name == "eoo_vals_0.75")
# 
# rl_aoo <- rl_df_mtp %>% filter(name == "aoo_vals_mtp") %>% 
#   mutate(logv = log(value))
# rl_aoo_0.5 <- rl_df_0.5 %>% filter(name == "aoo_vals_0.5") %>% 
#   mutate(logv = log(value))
# rl_aoo_0.75 <- rl_df_0.75 %>% filter(name == "aoo_vals_0.75") %>% 
#   mutate(logv = log(value))

# rl_med_eoo <-  rl_df %>% filter(name == "med_eoo")
# rl_med_aoo <-  rl_df %>% filter(name == "med_aoo")


# Could remove x axis text for EOO plots and just use AOO text when combining figures
eoo_b05_kw <- ggstatsplot::ggbetweenstats(data = rl_df_b05, 
                                      y = rs_eoo_b0.5, 
                                      x = species, 
                                      type = "nonparametric",
                                      pairwise.display = "none", # "none"
                                      results.subtitle = F,
                                      p.adjust.method = "holm",
                                      nboot = 200,
                                      xlab = "Species",
                                      ylab = bquote("Extent of occurrence " (km^2)),
                                      ggtheme = ggplot2::theme_bw(),
                                      point.args = list(alpha = 1),
                                      ggsignif.args = list(textsize = 4, 
                                                           vjust = 0.5),
                                      centrality.label.args = list(size = 4),
                                      centrality.plotting = F) +
  scale_colour_manual(values = unlist(cartocolors[cartocolors$Name == "Safe",8]),
                      breaks = unique(rl_df_b05$species)) +
  scale_y_continuous(labels = expSup) +
  theme(axis.title = element_text(size = 18),
        axis.text = element_text(size = 14),
        axis.text.x = element_text(vjust = 0.5, face = "italic")) +
  guides(x = guide_axis(n.dodge = 2))

# Note: need to display statistics in figure in order to extract them.
eoo_b05_kw_stats <- ggstatsplot::extract_stats(eoo_b05_kw)$subtitle_data
eoo_b05_ph_stats <- ggstatsplot::extract_stats(eoo_b05_kw)$pairwise_comparisons_data %>%
  mutate(metric = "EOO") %>% mutate(beta = 0.5)

eoo_b075_kw <- ggstatsplot::ggbetweenstats(data = rl_df_b075, 
                                          y = rs_eoo_b0.75, 
                                          x = species, 
                                          type = "nonparametric",
                                          pairwise.display = "none", # "none"
                                          results.subtitle = F,
                                          p.adjust.method = "holm",
                                          nboot = 200,
                                          xlab = "Species",
                                          ylab = bquote("Extent of occurrence " (km^2)),
                                          ggtheme = ggplot2::theme_bw(),
                                          point.args = list(alpha = 1),
                                          ggsignif.args = list(textsize = 4, vjust = 0.5),
                                          centrality.label.args = list(size = 4),
                                          centrality.plotting = F) +
  scale_colour_manual(values = unlist(cartocolors[cartocolors$Name == "Safe",8]),
                      breaks = unique(rl_df_b075$species)) +
  scale_y_continuous(labels = expSup) +
  theme(axis.title = element_text(size = 18),
        axis.text = element_text(size = 14),
        axis.text.x = element_text(vjust = 0.5, face = "italic")) +
  guides(x = guide_axis(n.dodge = 2))

eoo_b075_kw_stats <- ggstatsplot::extract_stats(eoo_b075_kw)$subtitle_data
eoo_b075_ph_stats <- ggstatsplot::extract_stats(eoo_b075_kw)$pairwise_comparisons_data %>%
  mutate(metric = "EOO") %>% mutate(beta = 0.75)

aoo_b05_kw <- ggstatsplot::ggbetweenstats(data = rl_df_b05, 
                                      y = rs_aoo_b0.5, 
                                      x = species, 
                                      type = "nonparametric",
                                      pairwise.display = "none", # "none"
                                      results.subtitle = F,
                                      p.adjust.method = "holm",
                                      nboot = 200,
                                      xlab = "Species",
                                      ylab = bquote("Area of occupancy " (km^2)),
                                      ggtheme = ggplot2::theme_bw(),
                                      point.args = list(alpha = 1),
                                      ggsignif.args = list(textsize = 4, vjust = 0.5),
                                      centrality.label.args = list(size = 4),
                                      centrality.plotting = F) +
  scale_colour_manual(values = unlist(cartocolors[cartocolors$Name == "Safe",8]),
                      breaks = unique(rl_df_b05$species)) +
  scale_y_continuous(labels = expSup) +
  theme(axis.title = element_text(size = 18),
        axis.text = element_text(size = 14),
        axis.text.x = element_text(vjust = 0.5, face = "italic")) +
  guides(x = guide_axis(n.dodge = 2))

aoo_b05_kw_stats <- ggstatsplot::extract_stats(aoo_b05_kw)$subtitle_data
aoo_b05_ph_stats <- ggstatsplot::extract_stats(aoo_b05_kw)$pairwise_comparisons_data %>%
  mutate(metric = "AOO") %>% mutate(beta = 0.5)

aoo_b075_kw <- ggstatsplot::ggbetweenstats(data = rl_df_b075, 
                                           y = rs_aoo_b0.75, 
                                           x = species, 
                                           type = "nonparametric",
                                          pairwise.display = "none", # "none"
                                          results.subtitle = F,
                                           nboot = 200,
                                           xlab = "Species",
                                           ylab = bquote("Area of occupancy " (km^2)),
                                           ggtheme = ggplot2::theme_bw(),
                                           point.args = list(alpha = 1),
                                           ggsignif.args = list(textsize = 4, vjust = 0.5),
                                           centrality.label.args = list(size = 4),
                                          centrality.plotting = F) +
  scale_colour_manual(values = unlist(cartocolors[cartocolors$Name == "Safe",8]),
                      breaks = unique(rl_df_b075$species)) +
  scale_y_continuous(labels = expSup) +
  theme(axis.title = element_text(size = 18),
        axis.text = element_text(size = 14),
        axis.text.x = element_text(vjust = 0.5, face = "italic")) +
  guides(x = guide_axis(n.dodge = 2))

aoo_b075_kw_stats <- ggstatsplot::extract_stats(aoo_b075_kw)$subtitle_data
aoo_b075_ph_stats <- ggstatsplot::extract_stats(aoo_b075_kw)$pairwise_comparisons_data %>%
  mutate(metric = "AOO") %>% mutate(beta = 0.75)

kw_stats <- as.data.frame(bind_rows(eoo_b05_kw_stats, eoo_b075_kw_stats, 
                      aoo_b05_kw_stats, aoo_b075_kw_stats)) %>%
  dplyr::select(-ncol(.))
write.csv(kw_stats, file = here(dirname(here()),"data", "biodiversity", "output", "range_size", "kw_stats.csv"))

ph_stats <- as.data.frame(bind_rows(eoo_b05_ph_stats, eoo_b075_ph_stats,
                                    aoo_b05_ph_stats, aoo_b075_ph_stats)) %>%
  dplyr::select(-9)
write.csv(ph_stats, file = here(dirname(here()),"data", "biodiversity", "output", "range_size", "ph_stats.csv"))

## Need figures made without stats
ggpubr::ggarrange(eoo_b05_kw, aoo_b05_kw, ncol = 1, nrow = 2, align = "v")
ggpubr::ggarrange(eoo_b075_kw, aoo_b075_kw, ncol = 1, nrow = 2, align = "v")

# Future
fut_lyrs <- list.files(here("data", "biodiversity", "output", "reps_sdm"), 
                       pattern = "fut_model", full.names = T)

pk_comp_fut_rasters <- prep_fut_lyrs(fut_lyrs, "comp")
pk_fut_rasters <- prep_fut_lyrs(fut_lyrs, "new")

# 
# fut_pk_comp <- fut_lyrs[str_detect(fut_lyrs,"comp")]
# 
# fut_pk_comp_cur <- fut_pk_comp[str_detect(fut_pk_comp,"for_fut")]
# fut_pk_comp_205026 <- fut_pk_comp[str_detect(fut_pk_comp,"2050RCP26")]
# fut_pk_comp_205045 <- fut_pk_comp[str_detect(fut_pk_comp,"2050RCP45")]
# fut_pk_comp_205060 <- fut_pk_comp[str_detect(fut_pk_comp,"2050RCP60")]
# fut_pk_comp_205085 <- fut_pk_comp[str_detect(fut_pk_comp,"2050RCP85")]
# 
# fut_pk_comp_sdm <- rast(fut_pk_comp_cur)
# fut_pk_comp_205026_sdm <- rast(fut_pk_comp_205026)
# fut_pk_comp_205045_sdm <- rast(fut_pk_comp_205045)
# fut_pk_comp_205060_sdm <- rast(fut_pk_comp_205060)
# fut_pk_comp_205085_sdm <- rast(fut_pk_comp_205085)
# 
# fut_pk_comp_210026 <- fut_pk_comp[str_detect(fut_pk_comp,"2100RCP26")]
# fut_pk_comp_210045 <- fut_pk_comp[str_detect(fut_pk_comp,"2100RCP45")]
# fut_pk_comp_210060 <- fut_pk_comp[str_detect(fut_pk_comp,"2100RCP60")]
# fut_pk_comp_210085 <- fut_pk_comp[str_detect(fut_pk_comp,"2100RCP85")]
# 
# fut_pk_comp_210026_sdm <- rast(fut_pk_comp_210026)
# fut_pk_comp_210045_sdm <- rast(fut_pk_comp_210045)
# fut_pk_comp_210060_sdm <- rast(fut_pk_comp_210060)
# fut_pk_comp_210085_sdm <- rast(fut_pk_comp_210085)
# 
# 
# pk_comp_fut_rasters <- list(fut_pk_comp_sdm, fut_pk_comp_205026_sdm,
#                          fut_pk_comp_205045_sdm,fut_pk_comp_205060_sdm,
#                          fut_pk_comp_205085_sdm, fut_pk_comp_210026_sdm,
#                          fut_pk_comp_210045_sdm,fut_pk_comp_210060_sdm,
#                          fut_pk_comp_210085_sdm)
# 
# rm(fut_pk_comp_sdm, fut_pk_comp_205026_sdm,fut_pk_comp_205045_sdm,
#    fut_pk_comp_205060_sdm,fut_pk_comp_205085_sdm, fut_pk_comp_210026_sdm,
#    fut_pk_comp_210045_sdm,fut_pk_comp_210060_sdm, fut_pk_comp_210085_sdm)

betas <- c(0.5, 0.75)

for(n in betas){
  
  rs_eoo <- c()
  rs_aoo <- c()
  
  for(i in seq_along(pk_comp_fut_rasters)){
    
    rs_list <- range_fun(pk_comp_fut_rasters[[i]], beta = n)
    rs_eoo <- c(rs_eoo, rs_list[[1]])
    rs_aoo <- c(rs_aoo, rs_list[[2]])
    
  }
  
  save(rs_eoo, file = here("data", "biodiversity", "output", "range_size",
                           paste0("fut_rs_eoo_pkcomp_b_",n,".RData")))
  save(rs_aoo, file = here("data", "biodiversity", "output", "range_size",
                           paste0("fut_rs_aoo_pkcomp_b_",n,".RData")))
  
}

for(n in betas){
  
  rs_eoo <- c()
  rs_aoo <- c()
  
  for(i in seq_along(pk_fut_rasters)){
    
    rs_list <- range_fun(pk_fut_rasters[[i]], beta = n)
    rs_eoo <- c(rs_eoo, rs_list[[1]])
    rs_aoo <- c(rs_aoo, rs_list[[2]])
    
  }
  
  save(rs_eoo, file = here("data", "biodiversity", "output", "range_size",
                           paste0("fut_rs_eoo_pk_b_",n,".RData")))
  save(rs_aoo, file = here("data", "biodiversity", "output", "range_size",
                           paste0("fut_rs_aoo_pk_b_",n,".RData")))
  
}

# 
# fut_pk <- fut_lyrs[str_detect(fut_lyrs,"new")]
# 
# fut_pk_cur <- fut_pk[str_detect(fut_pk,"for_fut")]
# fut_pk_205026 <- fut_pk[str_detect(fut_pk,"2050RCP26")]
# fut_pk_205045 <- fut_pk[str_detect(fut_pk,"2050RCP45")]
# fut_pk_205060 <- fut_pk[str_detect(fut_pk,"2050RCP60")]
# fut_pk_205085 <- fut_pk[str_detect(fut_pk,"2050RCP85")]
# 
# fut_pk_sdm <- rast(fut_pk_cur)
# fut_pk_205026_sdm <-rast(fut_pk_205026)
# fut_pk_205045_sdm <- rast(fut_pk_205045)
# fut_pk_205060_sdm <- rast(fut_pk_205060)
# fut_pk_205085_sdm <- rast(fut_pk_205085)
# 
# fut_pk_210026 <- fut_pk[str_detect(fut_pk,"2100RCP26")]
# fut_pk_210045 <- fut_pk[str_detect(fut_pk,"2100RCP45")]
# fut_pk_210060 <- fut_pk[str_detect(fut_pk,"2100RCP60")]
# fut_pk_210085 <- fut_pk[str_detect(fut_pk,"2100RCP85")]
# 
# fut_pk_210026_sdm <- rast(fut_pk_210026)
# fut_pk_210045_sdm <- rast(fut_pk_210045)
# fut_pk_210060_sdm <- rast(fut_pk_210060)
# fut_pk_210085_sdm <- rast(fut_pk_210085)


# pk_fut_rasters <- c(fut_pk_sdm, fut_pk_205026_sdm,
#                          fut_pk_205045_sdm,fut_pk_205060_sdm,
#                          fut_pk_205085_sdm, fut_pk_210026_sdm,
#                          fut_pk_210045_sdm,fut_pk_210060_sdm,
#                          fut_pk_210085_sdm)
# 
# rm(fut_pk_sdm, fut_pk_205026_sdm, fut_pk_205045_sdm,fut_pk_205060_sdm,
#    fut_pk_205085_sdm, fut_pk_210026_sdm,fut_pk_210045_sdm,fut_pk_210060_sdm,
#    fut_pk_210085_sdm)



# # P. kerguelensis (sensu lato)
# fut_pk_comp_range <- range_fun(bio_data, "Promachocrinus kerguelensis", fut_pk_comp_sdm, status = "comp")
# fut_pk_comp_range_0.5 <- range_fun(bio_data, "Promachocrinus kerguelensis", fut_pk_comp_sdm, status = "comp", thresh = 0.5)
# fut_pk_comp_range_0.75 <- range_fun(bio_data, "Promachocrinus kerguelensis", fut_pk_comp_sdm, status = "comp", thresh = 0.75)
# fut_pk_comp_205026_range <- range_fun(bio_data, "Promachocrinus kerguelensis", fut_pk_comp_205026_sdm, status = "comp")
# fut_pk_comp_205026_range_0.5 <- range_fun(bio_data, "Promachocrinus kerguelensis", fut_pk_comp_205026_sdm, status = "comp", thresh = 0.5)
# fut_pk_comp_205026_range_0.75 <- range_fun(bio_data, "Promachocrinus kerguelensis", fut_pk_comp_205026_sdm, status = "comp", thresh = 0.75)
# fut_pk_comp_205045_range <- range_fun(bio_data, "Promachocrinus kerguelensis", fut_pk_comp_205045_sdm, status = "comp")
# fut_pk_comp_205045_range_0.5 <- range_fun(bio_data, "Promachocrinus kerguelensis", fut_pk_comp_205045_sdm, status = "comp", thresh = 0.5)
# fut_pk_comp_205045_range_0.75 <- range_fun(bio_data, "Promachocrinus kerguelensis", fut_pk_comp_205045_sdm, status = "comp", thresh = 0.75)
# fut_pk_comp_205060_range <- range_fun(bio_data, "Promachocrinus kerguelensis", fut_pk_comp_205060_sdm, status = "comp")
# fut_pk_comp_205060_range_0.5 <- range_fun(bio_data, "Promachocrinus kerguelensis", fut_pk_comp_205060_sdm, status = "comp", thresh = 0.5)
# fut_pk_comp_205060_range_0.75 <- range_fun(bio_data, "Promachocrinus kerguelensis", fut_pk_comp_205060_sdm, status = "comp", thresh = 0.75)
# fut_pk_comp_205085_range <- range_fun(bio_data, "Promachocrinus kerguelensis", fut_pk_comp_205085_sdm, status = "comp")
# fut_pk_comp_205085_range_0.5 <- range_fun(bio_data, "Promachocrinus kerguelensis", fut_pk_comp_205085_sdm, status = "comp", thresh = 0.5)
# fut_pk_comp_205085_range_0.75 <- range_fun(bio_data, "Promachocrinus kerguelensis", fut_pk_comp_205085_sdm, status = "comp", thresh = 0.75)
# fut_pk_comp_210026_range <- range_fun(bio_data, "Promachocrinus kerguelensis", fut_pk_comp_210026_sdm, status = "comp")
# fut_pk_comp_210026_range_0.5 <- range_fun(bio_data, "Promachocrinus kerguelensis", fut_pk_comp_210026_sdm, status = "comp", thresh = 0.5)
# fut_pk_comp_210026_range_0.75 <- range_fun(bio_data, "Promachocrinus kerguelensis", fut_pk_comp_210026_sdm, status = "comp", thresh = 0.75)
# fut_pk_comp_210045_range <- range_fun(bio_data, "Promachocrinus kerguelensis", fut_pk_comp_210045_sdm, status = "comp")
# fut_pk_comp_210045_range_0.5 <- range_fun(bio_data, "Promachocrinus kerguelensis", fut_pk_comp_210045_sdm, status = "comp", thresh = 0.5)
# fut_pk_comp_210045_range_0.75 <- range_fun(bio_data, "Promachocrinus kerguelensis", fut_pk_comp_210045_sdm, status = "comp", thresh = 0.75)
# fut_pk_comp_210060_range <- range_fun(bio_data, "Promachocrinus kerguelensis", fut_pk_comp_210060_sdm, status = "comp")
# fut_pk_comp_210060_range_0.5 <- range_fun(bio_data, "Promachocrinus kerguelensis", fut_pk_comp_210060_sdm, status = "comp", thresh = 0.5)
# fut_pk_comp_210060_range_0.75 <- range_fun(bio_data, "Promachocrinus kerguelensis", fut_pk_comp_210060_sdm, status = "comp", thresh = 0.75)
# fut_pk_comp_210085_range <- range_fun(bio_data, "Promachocrinus kerguelensis", fut_pk_comp_210085_sdm, status = "comp")
# fut_pk_comp_210085_range_0.5 <- range_fun(bio_data, "Promachocrinus kerguelensis", fut_pk_comp_210085_sdm, status = "comp", thresh = 0.5)
# fut_pk_comp_210085_range_0.75 <- range_fun(bio_data, "Promachocrinus kerguelensis", fut_pk_comp_210085_sdm, status = "comp", thresh = 0.75)
# 
# 
# 
# # P. kerguelensis
# fut_pk_range <- range_fun(bio_data, "Promachocrinus kerguelensis", fut_pk_sdm, status = "new")
# fut_pk_range_0.5 <- range_fun(bio_data, "Promachocrinus kerguelensis", fut_pk_sdm, status = "new", thresh = 0.5)
# fut_pk_range_0.75 <- range_fun(bio_data, "Promachocrinus kerguelensis", fut_pk_sdm, status = "new", thresh = 0.75)
# fut_pk_205026_range <- range_fun(bio_data, "Promachocrinus kerguelensis", fut_pk_205026_sdm, status = "new")
# fut_pk_205026_range_0.5 <- range_fun(bio_data, "Promachocrinus kerguelensis", fut_pk_205026_sdm, status = "new", thresh = 0.5)
# fut_pk_205026_range_0.75 <- range_fun(bio_data, "Promachocrinus kerguelensis", fut_pk_205026_sdm, status = "new", thresh = 0.75)
# fut_pk_205045_range <- range_fun(bio_data, "Promachocrinus kerguelensis", fut_pk_205045_sdm, status = "new")
# fut_pk_205045_range_0.5 <- range_fun(bio_data, "Promachocrinus kerguelensis", fut_pk_205045_sdm, status = "new", thresh = 0.5)
# fut_pk_205045_range_0.75 <- range_fun(bio_data, "Promachocrinus kerguelensis", fut_pk_205045_sdm, status = "new", thresh = 0.75)
# fut_pk_205060_range <- range_fun(bio_data, "Promachocrinus kerguelensis", fut_pk_205060_sdm, status = "new")
# fut_pk_205060_range_0.5 <- range_fun(bio_data, "Promachocrinus kerguelensis", fut_pk_205060_sdm, status = "new", thresh = 0.5)
# fut_pk_205060_range_0.75 <- range_fun(bio_data, "Promachocrinus kerguelensis", fut_pk_205060_sdm, status = "new", thresh = 0.75)
# fut_pk_205085_range <- range_fun(bio_data, "Promachocrinus kerguelensis", fut_pk_205085_sdm, status = "new")
# fut_pk_205085_range_0.5 <- range_fun(bio_data, "Promachocrinus kerguelensis", fut_pk_205085_sdm, status = "new", thresh = 0.5)
# fut_pk_205085_range_0.75 <- range_fun(bio_data, "Promachocrinus kerguelensis", fut_pk_205085_sdm, status = "new", thresh = 0.75)
# fut_pk_210026_range <- range_fun(bio_data, "Promachocrinus kerguelensis", fut_pk_210026_sdm, status = "new")
# fut_pk_210026_range_0.5 <- range_fun(bio_data, "Promachocrinus kerguelensis", fut_pk_210026_sdm, status = "new", thresh = 0.5)
# fut_pk_210026_range_0.75 <- range_fun(bio_data, "Promachocrinus kerguelensis", fut_pk_210026_sdm, status = "new", thresh = 0.75)
# fut_pk_210045_range <- range_fun(bio_data, "Promachocrinus kerguelensis", fut_pk_210045_sdm, status = "new")
# fut_pk_210045_range_0.5 <- range_fun(bio_data, "Promachocrinus kerguelensis", fut_pk_210045_sdm, status = "new", thresh = 0.5)
# fut_pk_210045_range_0.75 <- range_fun(bio_data, "Promachocrinus kerguelensis", fut_pk_210045_sdm, status = "new", thresh = 0.75)
# fut_pk_210060_range <- range_fun(bio_data, "Promachocrinus kerguelensis", fut_pk_210060_sdm, status = "new")
# fut_pk_210060_range_0.5 <- range_fun(bio_data, "Promachocrinus kerguelensis", fut_pk_210060_sdm, status = "new", thresh = 0.5)
# fut_pk_210060_range_0.75 <- range_fun(bio_data, "Promachocrinus kerguelensis", fut_pk_210060_sdm, status = "new", thresh = 0.75)
# fut_pk_210085_range <- range_fun(bio_data, "Promachocrinus kerguelensis", fut_pk_210085_sdm, status = "new")
# fut_pk_210085_range_0.5 <- range_fun(bio_data, "Promachocrinus kerguelensis", fut_pk_210085_sdm, status = "new", thresh = 0.5)
# fut_pk_210085_range_0.75 <- range_fun(bio_data, "Promachocrinus kerguelensis", fut_pk_210085_sdm, status = "new", thresh = 0.75)
# 
# fut_eoo_vals <- c(fut_pk_comp_range[[1]], fut_pk_comp_205026_range[[1]],
#                   fut_pk_comp_205045_range[[1]], fut_pk_comp_205060_range[[1]],
#                   fut_pk_comp_205085_range[[1]], fut_pk_comp_210026_range[[1]],
#                   fut_pk_comp_210045_range[[1]], fut_pk_comp_210060_range[[1]],
#                   fut_pk_comp_210085_range[[1]], fut_pk_range[[1]],
#                   fut_pk_205026_range[[1]], fut_pk_205045_range[[1]],
#                   fut_pk_205060_range[[1]], fut_pk_205085_range[[1]],
#                   fut_pk_210026_range[[1]], fut_pk_210045_range[[1]],
#                   fut_pk_210060_range[[1]], fut_pk_210085_range[[1]])
# save(fut_eoo_vals, file = here("data", "biodiversity", "output", "range_size", "fut_eoo_vals.RData"))
pkcomp_rs_eoo_b0.5 <- loadRData(file = here("data", "biodiversity", "output", "range_size", "fut_rs_eoo_pkcomp_b_0.5.RData"))
pk_rs_eoo_b0.5 <- loadRData(file = here("data", "biodiversity", "output", "range_size", "fut_rs_eoo_pk_b_0.5.RData"))

pkcomp_rs_aoo_b0.5 <- loadRData(file = here("data", "biodiversity", "output", "range_size", "fut_rs_aoo_pkcomp_b_0.5.RData"))
pk_rs_aoo_b0.5 <- loadRData(file = here("data", "biodiversity", "output", "range_size", "fut_rs_aoo_pk_b_0.5.RData"))

# fut_eoo_vals_0.5 <- c(fut_pk_comp_range_0.5[[1]], fut_pk_comp_205026_range_0.5[[1]],
#                   fut_pk_comp_205045_range_0.5[[1]], fut_pk_comp_205060_range_0.5[[1]],
#                   fut_pk_comp_205085_range_0.5[[1]], fut_pk_comp_210026_range_0.5[[1]],
#                   fut_pk_comp_210045_range_0.5[[1]], fut_pk_comp_210060_range_0.5[[1]],
#                   fut_pk_comp_210085_range_0.5[[1]], fut_pk_range_0.5[[1]],
#                   fut_pk_205026_range_0.5[[1]], fut_pk_205045_range_0.5[[1]],
#                   fut_pk_205060_range_0.5[[1]], fut_pk_205085_range_0.5[[1]],
#                   fut_pk_210026_range_0.5[[1]], fut_pk_210045_range_0.5[[1]],
#                   fut_pk_210060_range_0.5[[1]], fut_pk_210085_range_0.5[[1]])
# save(fut_eoo_vals_0.5, file = here("data", "biodiversity", "output", "range_size", "fut_eoo_vals_0.5.RData"))
pkcomp_rs_eoo_b0.75 <- loadRData(file = here("data", "biodiversity", "output", "range_size", "fut_rs_eoo_pkcomp_b_0.75.RData"))
pk_rs_eoo_b0.75 <- loadRData(file = here("data", "biodiversity", "output", "range_size", "fut_rs_eoo_pk_b_0.75.RData"))

pkcomp_rs_aoo_b0.75 <- loadRData(file = here("data", "biodiversity", "output", "range_size", "fut_rs_aoo_pkcomp_b_0.75.RData"))
pk_rs_aoo_b0.75 <- loadRData(file = here("data", "biodiversity", "output", "range_size", "fut_rs_aoo_pk_b_0.75.RData"))

# fut_eoo_vals_0.75 <- c(fut_pk_comp_range_0.75[[1]], fut_pk_comp_205026_range_0.75[[1]],
#                       fut_pk_comp_205045_range_0.75[[1]], fut_pk_comp_205060_range_0.75[[1]],
#                       fut_pk_comp_205085_range_0.75[[1]], fut_pk_comp_210026_range_0.75[[1]],
#                       fut_pk_comp_210045_range_0.75[[1]], fut_pk_comp_210060_range_0.75[[1]],
#                       fut_pk_comp_210085_range_0.75[[1]], fut_pk_range_0.75[[1]],
#                       fut_pk_205026_range_0.75[[1]], fut_pk_205045_range_0.75[[1]],
#                       fut_pk_205060_range_0.75[[1]], fut_pk_205085_range_0.75[[1]],
#                       fut_pk_210026_range_0.75[[1]], fut_pk_210045_range_0.75[[1]],
#                       fut_pk_210060_range_0.75[[1]], fut_pk_210085_range_0.75[[1]])
# save(fut_eoo_vals_0.75, file = here("data", "biodiversity", "output", "range_size", "fut_eoo_vals_0.75.RData"))
#load(file = here("data", "biodiversity", "output", "range_size", "fut_eoo_vals_0.75.RData"))

# fut_aoo_vals <- c(fut_pk_comp_range[[2]], fut_pk_comp_205026_range[[2]],
#                   fut_pk_comp_205045_range[[2]], fut_pk_comp_205060_range[[2]],
#                   fut_pk_comp_205085_range[[2]], fut_pk_comp_210026_range[[2]],
#                   fut_pk_comp_210045_range[[2]], fut_pk_comp_210060_range[[2]],
#                   fut_pk_comp_210085_range[[2]], fut_pk_range[[2]],
#                   fut_pk_205026_range[[2]], fut_pk_205045_range[[2]],
#                   fut_pk_205060_range[[2]], fut_pk_205085_range[[2]],
#                   fut_pk_210026_range[[2]], fut_pk_210045_range[[2]],
#                   fut_pk_210060_range[[2]], fut_pk_210085_range[[2]])
# save(fut_aoo_vals, file = here("data", "biodiversity", "output", "range_size", "fut_aoo_vals.RData"))
# #load(file = here("data", "biodiversity", "output", "range_size", "fut_aoo_vals.RData"))
# 
# fut_aoo_vals_0.5 <- c(fut_pk_comp_range_0.5[[2]], fut_pk_comp_205026_range_0.5[[2]],
#                       fut_pk_comp_205045_range_0.5[[2]], fut_pk_comp_205060_range_0.5[[2]],
#                       fut_pk_comp_205085_range_0.5[[2]], fut_pk_comp_210026_range_0.5[[2]],
#                       fut_pk_comp_210045_range_0.5[[2]], fut_pk_comp_210060_range_0.5[[2]],
#                       fut_pk_comp_210085_range_0.5[[2]], fut_pk_range_0.5[[2]],
#                       fut_pk_205026_range_0.5[[2]], fut_pk_205045_range_0.5[[2]],
#                       fut_pk_205060_range_0.5[[2]], fut_pk_205085_range_0.5[[2]],
#                       fut_pk_210026_range_0.5[[2]], fut_pk_210045_range_0.5[[2]],
#                       fut_pk_210060_range_0.5[[2]], fut_pk_210085_range_0.5[[2]])
# save(fut_aoo_vals_0.5, file = here("data", "biodiversity", "output", "range_size", "fut_aoo_vals_0.5.RData"))
# #load(file = here("data", "biodiversity", "output", "range_size", "fut_aoo_vals_0.5.RData"))
# 
# fut_aoo_vals_0.75 <- c(fut_pk_comp_range_0.75[[2]], fut_pk_comp_205026_range_0.75[[2]],
#                        fut_pk_comp_205045_range_0.75[[2]], fut_pk_comp_205060_range_0.75[[2]],
#                        fut_pk_comp_205085_range_0.75[[2]], fut_pk_comp_210026_range_0.75[[2]],
#                        fut_pk_comp_210045_range_0.75[[2]], fut_pk_comp_210060_range_0.75[[2]],
#                        fut_pk_comp_210085_range_0.75[[2]], fut_pk_range_0.75[[2]],
#                        fut_pk_205026_range_0.75[[2]], fut_pk_205045_range_0.75[[2]],
#                        fut_pk_205060_range_0.75[[2]], fut_pk_205085_range_0.75[[2]],
#                        fut_pk_210026_range_0.75[[2]], fut_pk_210045_range_0.75[[2]],
#                        fut_pk_210060_range_0.75[[2]], fut_pk_210085_range_0.75[[2]])
# save(fut_aoo_vals_0.75, file = here("data", "biodiversity", "output", "range_size", "fut_aoo_vals_0.75.RData"))
# #load(file = here("data", "biodiversity", "output", "range_size", "fut_aoo_vals_0.75.RData"))

sp <- c(rep("P. kerguelensis (s.l.)", 180), rep("P. kerguelensis", 180))
year <- rep(c(rep("2023",20), rep("2050", 80), rep("2100", 80)),2)
scenario <- rep(c(rep("Current",20),
                  rep(c(rep("RCP 2.6",20), 
                        rep("RCP 4.5",20), 
                        rep("RCP 6.0",20), 
                        rep("RCP 8.5",20)),2)),2)

fut_eoo_vals_0.5 <- c(pkcomp_rs_eoo_b0.5, pk_rs_eoo_b0.5)
fut_eoo_vals_0.75 <- c(pkcomp_rs_eoo_b0.75, pk_rs_eoo_b0.75)
fut_aoo_vals_0.5 <- c(pkcomp_rs_aoo_b0.5, pk_rs_aoo_b0.5)
fut_aoo_vals_0.75 <- c(pkcomp_rs_aoo_b0.75, pk_rs_aoo_b0.75)

# range_df <- data.frame(species = sp,
#                  year = year,
#                  scenario = scenario,
#                  EOO_mtp = fut_eoo_vals,
#                  AOO_mtp = fut_aoo_vals,
#                  EOO_0.5 = fut_eoo_vals_0.5,
#                  AOO_0.5 = fut_aoo_vals_0.5,
#                  EOO_0.75 = fut_eoo_vals_0.75,
#                  AOO_0.75 = fut_aoo_vals_0.75)

range_df <- data.frame(species = sp,
                       year = year,
                       scenario = scenario,
                       eoo_0.5 = fut_eoo_vals_0.5,
                       eoo_0.75 = fut_eoo_vals_0.75,
                       aoo_0.5 = fut_aoo_vals_0.5,
                       aoo_0.75 = fut_aoo_vals_0.75)

## Boxplots for supplementary information
# EOO
# eoo_mtp_box <- ggplot(range_df, aes(y = EOO_mtp, x = scenario, fill = species)) +
#   geom_boxplot() +
#   scale_fill_carto_d(name = "Species", labels = c("P. kerguelensis (s.s.)", "P. kerguelensis (s.l.)")) +
#   facet_wrap(~year) +
#   theme_bw() +
#   theme(axis.text.x = element_text(angle = 45, vjust = 0.5),
#         legend.text = element_text(face = "italic", size = 16),
#         legend.title = element_text(size = 18),
#         axis.text = element_text(size = 16),
#         axis.title = element_text(size = 18),
#         strip.text = element_text(size = 16)) +
#   xlab("Scenario") +
#   ylab(bquote("EOO " (km^2))) +
#   scale_y_continuous(labels = expSup)

eoo_0.5_box <- ggplot(range_df, aes(y = EOO_0.5, x = scenario, fill = species)) +
  geom_boxplot() +
  scale_fill_carto_d(name = "Species", labels = c("P. kerguelensis (s.s.)", "P. kerguelensis (s.l.)")) +
  facet_wrap(~year) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, vjust = 0.5),
        legend.text = element_text(face = "italic", size = 16),
        legend.title = element_text(size = 18),
        axis.text = element_text(size = 16),
        axis.title = element_text(size = 18),
        strip.text = element_text(size = 16)) +
  xlab("Scenario") +
  ylab(bquote("EOO " (km^2))) +
  scale_y_continuous(labels = expSup)

eoo_0.75_box <- ggplot(range_df, aes(y = EOO_0.75, x = scenario, fill = species)) +
  geom_boxplot() +
  scale_fill_carto_d(name = "Species", labels = c("P. kerguelensis (s.s.)", "P. kerguelensis (s.l.)")) +
  facet_wrap(~year) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, vjust = 0.5),
        legend.text = element_text(face = "italic", size = 16),
        legend.title = element_text(size = 18),
        axis.text = element_text(size = 16),
        axis.title = element_text(size = 18),
        strip.text = element_text(size = 16)) +
  xlab("Scenario") +
  ylab(bquote("EOO " (km^2))) +
  scale_y_continuous(labels = expSup)

ggpubr::ggarrange(eoo_mtp_box, eoo_0.5_box, eoo_0.75_box, 
                  labels = c("A", "B", "C"),
                  ncol = 1, nrow = 3, common.legend = T)

# AOO
# aoo_mtp_box <- ggplot(range_df, aes(y = AOO_mtp, x = scenario, fill = species)) +
#   geom_boxplot() +
#   scale_fill_carto_d(name = "Species", labels = c("P. kerguelensis (s.s.)", "P. kerguelensis (s.l.)")) +
#   facet_wrap(~year) +
#   theme_bw() +
#   theme(axis.text.x = element_text(angle = 45, vjust = 0.5),
#         legend.text = element_text(face = "italic", size = 16),
#         legend.title = element_text(size = 18),
#         axis.text = element_text(size = 16),
#         axis.title = element_text(size = 18),
#         strip.text = element_text(size = 16)) +
#   xlab("Scenario") +
#   ylab(bquote("AOO " (km^2)))

aoo_0.5_box <- ggplot(range_df, aes(y = AOO_0.5, x = scenario, fill = species)) +
  geom_boxplot() +
  scale_fill_carto_d(name = "Species", labels = c("P. kerguelensis (s.s.)", "P. kerguelensis (s.l.)")) +
  facet_wrap(~year) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, vjust = 0.5),
        legend.text = element_text(face = "italic", size = 16),
        legend.title = element_text(size = 18),
        axis.text = element_text(size = 16),
        axis.title = element_text(size = 18),
        strip.text = element_text(size = 16)) +
  xlab("Scenario") +
  ylab(bquote("AOO " (km^2)))

aoo_0.75_box <- ggplot(range_df, aes(y = AOO_0.75, x = scenario, fill = species)) +
  geom_boxplot() +
  scale_fill_carto_d(name = "Species", labels = c("P. kerguelensis (s.s.)", "P. kerguelensis (s.l.)")) +
  facet_wrap(~year) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, vjust = 0.5),
        legend.text = element_text(face = "italic", size = 16),
        legend.title = element_text(size = 18),
        axis.text = element_text(size = 16),
        axis.title = element_text(size = 18),
        strip.text = element_text(size = 16)) +
  xlab("Scenario") +
  ylab(bquote("AOO " (km^2)))

ggpubr::ggarrange(aoo_mtp_box, aoo_0.5_box, aoo_0.75_box, 
                  labels = c("A", "B", "C"),
                  ncol = 1, nrow = 3, common.legend = T, align = "v")

# Line plots using median/mean
eoo_0.5_df <- range_df %>% 
  group_by(species , year, scenario) %>%
  mutate(eoo_mean = mean(eoo_0.5)) %>%
  mutate(eoo_med = median(eoo_0.5)) %>%
  mutate(eoo_sd = sd(eoo_0.5)) %>%
  dplyr::select(-eoo_0.75) %>%
  distinct(eoo_mean, .keep_all = T) %>%
  ungroup()

eoo_0.75_df <- range_df %>% 
  group_by(species , year, scenario) %>%
  mutate(eoo_mean = mean(eoo_0.75)) %>%
  mutate(eoo_med = median(eoo_0.75)) %>%
  mutate(eoo_sd = sd(eoo_0.75)) %>%
  dplyr::select(-eoo_0.5) %>%
  distinct(eoo_mean, .keep_all = T) %>%
  ungroup()

# #s.l. to 2050
# mean(c(abs(38521081 - 53077517),abs(38521081 - 53664054),abs(38521081 - 48997523),
#      abs(38521081 - 52326581)))
# 
# sqrt(var(c(abs(38521081 - 53077517),abs(38521081 - 53664054),abs(38521081 - 48997523),
#        abs(38521081 - 52326581))))
# 
# #s.s. to 2050
# mean(c(abs(30536741 - 51943832),abs(30536741 - 52206623),abs(30536741 - 44388853),
#        abs(30536741 - 46536201)))
# 
# sqrt(var(c(abs(30536741 - 51943832),abs(30536741 - 52206623),abs(30536741 - 44388853),
#        abs(30536741 - 46536201))))
# 
# #s.s. 2050 to 2100 rcp 2.6
# abs(51943832-51364727) #579 105
# 
# #s.s. 2050 to 2100 rcp 2.6
# abs(53077517-43449857) #8 493 975

nr1 <- eoo_0.5_df %>% slice(rep(1, each = 3))
eoo_0.5_df <- eoo_0.5_df %>% add_row(nr1, .before = 2)
nr2 <- eoo_0.5_df %>% slice(rep(13, each = 3))
eoo_0.5_df <- eoo_0.5_df %>% add_row(nr2, .before = 14)
eoo_0.5_df <- eoo_0.5_df %>%
  mutate(scenario = rep(c("RCP 2.6", "RCP 4.5", "RCP 6.0", "RCP 8.5"),6))

eoo_plot_0.5 <- ggplot(eoo_0.5_df, aes(x = as.numeric(year), y = eoo_mean))+
  geom_point(aes(col = scenario, pch = species), size = 2) +
  geom_line(aes(col = scenario, linetype = species), linewidth = 1) +
  theme_bw() +
  scale_color_manual(values = rev(PNWColors::pnw_palette("Moth", 4, type = "discrete"))) +
  scale_y_continuous(labels = expSup) +
  theme(legend.text = element_text(size = 12),
        legend.title = element_text(size = 14),
        axis.text = element_text(size = 14),
        axis.title = element_text(size = 16),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black")) +
  xlab("Year") +
  ylab(bquote("Mean EOO " (km^2)))

## 0.5
eoo_df <- range_df %>% 
  group_by(species , year, scenario) %>%
  mutate(eoo_mean = mean(EOO_0.5)) %>%
  mutate(eoo_med = median(EOO_0.5)) %>%
  mutate(eoo_sd = sd(EOO_0.5)) %>%
  dplyr::select(-c(EOO_mtp, EOO_0.5, EOO_0.75,AOO_mtp, AOO_0.5, AOO_0.75)) %>%
  distinct(eoo_mean, .keep_all = T) %>%
  ungroup()

nr1 <- eoo_0.75_df %>% slice(rep(1, each = 3))
eoo_0.75_df <- eoo_0.75_df %>% add_row(nr1, .before = 2)
nr2 <- eoo_0.75_df %>% slice(rep(13, each = 3))
eoo_0.75_df <- eoo_0.75_df %>% add_row(nr2, .before = 14)
eoo_0.75_df <- eoo_0.75_df %>%
  mutate(scenario = rep(c("RCP 2.6", "RCP 4.5", "RCP 6.0", "RCP 8.5"),6))

eoo_plot_0.75 <- ggplot(eoo_0.75_df, aes(x = as.numeric(year), y = eoo_mean))+
  geom_point(aes(col = scenario, pch = species), size = 2) +
  geom_line(aes(col = scenario, linetype = species), linewidth = 1) +
  theme_bw() +
  scale_color_manual(values = rev(PNWColors::pnw_palette("Moth", 4, type = "discrete"))) +
  scale_y_continuous(labels = expSup) +
  theme(legend.text = element_text(size = 12),
        legend.title = element_text(size = 14),
        axis.text = element_text(size = 14),
        axis.title = element_text(size = 16),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black")) +
  xlab("Year") +
  ylab(bquote("Mean EOO " (km^2)))

## 0.75
eoo_df <- range_df %>% 
  group_by(species , year, scenario) %>%
  mutate(eoo_mean = mean(EOO_0.75)) %>%
  mutate(eoo_med = median(EOO_0.75)) %>%
  mutate(eoo_sd = sd(EOO_0.75)) %>%
  dplyr::select(-c(EOO_mtp, EOO_0.5, EOO_0.75,AOO_mtp, AOO_0.5, AOO_0.75)) %>%
  distinct(eoo_mean, .keep_all = T) %>%
  ungroup()

nr1 <- eoo_df %>% slice(rep(1, each = 3))
eoo_df <- eoo_df %>% add_row(nr1, .before = 2)
nr2 <- eoo_df %>% slice(rep(13, each = 3))
eoo_df <- eoo_df %>% add_row(nr2, .before = 14)
eoo_df <- eoo_df %>%
  mutate(scenario = rep(c("RCP 2.6", "RCP 4.5", "RCP 6.0", "RCP 8.5"),6))

eoo_plot_0.75 <- ggplot(eoo_df, aes(x = as.numeric(year), y = eoo_mean))+
  geom_point(aes(col = scenario, pch = species), size = 2) +
  geom_line(aes(col = scenario, linetype = species), linewidth = 1) +
  theme_bw() +
  scale_color_manual(values = rev(PNWColors::pnw_palette("Moth", 4, type = "discrete"))) +
  scale_y_continuous(labels = expSup) +
  theme(legend.text = element_text(size = 12),
        legend.title = element_text(size = 14),
        axis.text = element_text(size = 14),
        axis.title = element_text(size = 16),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black")) +
  xlab("Year") +
  ylab(bquote("Mean EOO " (km^2)))

# ggpubr::ggarrange(eoo_plot_MTP, eoo_plot_0.5, eoo_plot_0.75,
#                   labels = c("A", "B", "C"),
#                   ncol = 3, nrow = 1, common.legend = T)


aoo_df <- range_df %>% 
  group_by(species , year, scenario) %>%
  mutate(aoo_mean = mean(AOO_mtp)) %>%
  mutate(aoo_med = median(AOO_mtp)) %>%
  mutate(aoo_sd = sd(AOO_mtp)) %>%
  dplyr::select(-c(EOO_mtp, EOO_0.5, EOO_0.75,AOO_mtp, AOO_0.5, AOO_0.75)) %>%
  distinct(aoo_mean, .keep_all = T) %>%
  ungroup()

nr1 <- aoo_df %>% slice(rep(1, each = 3))
aoo_df <- aoo_df %>% add_row(nr1, .before = 2)
nr2 <- aoo_df %>% slice(rep(13, each = 3))
aoo_df <- aoo_df %>% add_row(nr2, .before = 14)
aoo_df <- aoo_df %>%
  mutate(scenario = rep(c("RCP 2.6", "RCP 4.5", "RCP 6.0", "RCP 8.5"),6))

aoo_plot_MTP <- ggplot(aoo_df, aes(x = as.numeric(year), y = aoo_mean))+
  geom_point(aes(col = scenario, pch = species), size = 2) +
  geom_line(aes(col = scenario, linetype = species), linewidth = 1) +
  scale_color_manual(values = rev(PNWColors::pnw_palette("Moth", 4, type = "discrete"))) +
  scale_y_continuous(labels = expSup) +
  theme_bw() +
  theme(legend.text = element_text(size = 12),
        legend.title = element_text(size = 14),
        axis.text = element_text(size = 14),
        axis.title = element_text(size = 16),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black")) +
  xlab("Year") +
  ylab(bquote("Mean AOO " (km^2)))

aoo_df <- range_df %>% 
  group_by(species , year, scenario) %>%
  mutate(aoo_mean = mean(AOO_0.5)) %>%
  mutate(aoo_med = median(AOO_0.5)) %>%
  mutate(aoo_sd = sd(AOO_0.5)) %>%
  dplyr::select(-c(EOO_mtp, EOO_0.5, EOO_0.75,AOO_mtp, AOO_0.5, AOO_0.75)) %>%
  distinct(aoo_mean, .keep_all = T) %>%
  ungroup()

nr1 <- aoo_df %>% slice(rep(1, each = 3))
aoo_df <- aoo_df %>% add_row(nr1, .before = 2)
nr2 <- aoo_df %>% slice(rep(13, each = 3))
aoo_df <- aoo_df %>% add_row(nr2, .before = 14)
aoo_df <- aoo_df %>%
  mutate(scenario = rep(c("RCP 2.6", "RCP 4.5", "RCP 6.0", "RCP 8.5"),6))

aoo_plot_0.5 <- ggplot(aoo_df, aes(x = as.numeric(year), y = aoo_mean))+
  geom_point(aes(col = scenario, pch = species), size = 2) +
  geom_line(aes(col = scenario, linetype = species), linewidth = 1) +
  scale_color_manual(values = rev(PNWColors::pnw_palette("Moth", 4, type = "discrete"))) +
  scale_y_continuous(labels = expSup) +
  theme_bw() +
  theme(legend.text = element_text(size = 12),
        legend.title = element_text(size = 14),
        axis.text = element_text(size = 14),
        axis.title = element_text(size = 16),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black")) +
  xlab("Year") +
  ylab(bquote("Mean AOO " (km^2)))

aoo_df <- range_df %>% 
  group_by(species , year, scenario) %>%
  mutate(aoo_mean = mean(AOO_0.75)) %>%
  mutate(aoo_med = median(AOO_0.75)) %>%
  mutate(aoo_sd = sd(AOO_0.75)) %>%
  dplyr::select(-c(EOO_mtp, EOO_0.5, EOO_0.75,AOO_mtp, AOO_0.5, AOO_0.75)) %>%
  distinct(aoo_mean, .keep_all = T) %>%
  ungroup()

nr1 <- aoo_df %>% slice(rep(1, each = 3))
aoo_df <- aoo_df %>% add_row(nr1, .before = 2)
nr2 <- aoo_df %>% slice(rep(13, each = 3))
aoo_df <- aoo_df %>% add_row(nr2, .before = 14)
aoo_df <- aoo_df %>%
  mutate(scenario = rep(c("RCP 2.6", "RCP 4.5", "RCP 6.0", "RCP 8.5"),6))

aoo_plot_0.75 <- ggplot(aoo_df, aes(x = as.numeric(year), y = aoo_mean))+
  geom_point(aes(col = scenario, pch = species), size = 2) +
  geom_line(aes(col = scenario, linetype = species), linewidth = 1) +
  scale_color_manual(values = rev(PNWColors::pnw_palette("Moth", 4, type = "discrete"))) +
  scale_y_continuous(labels = expSup) +
  theme_bw() +
  theme(legend.text = element_text(size = 12),
        legend.title = element_text(size = 14),
        axis.text = element_text(size = 14),
        axis.title = element_text(size = 16),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black")) +
  xlab("Year") +
  ylab(bquote("Mean AOO " (km^2)))

# ggpubr::ggarrange(eoo_plot_MTP, eoo_plot_0.5, eoo_plot_0.75,
#                   aoo_plot_MTP, aoo_plot_0.5, aoo_plot_0.75,
#                   #labels = c("A", "B", "C", "D", "E", "F"),
#                   ncol = 3, nrow = 2, common.legend = T)

ggpubr::ggarrange(eoo_plot_MTP, aoo_plot_MTP,
                  eoo_plot_0.5, aoo_plot_0.5,
                  eoo_plot_0.75,aoo_plot_0.75,
                  labels = c("A", "D", "B", "E", "C", "F"),
                  ncol = 2, nrow = 3, common.legend = T,
                  legend = "top")


## 3.3 | Environmental niche----
# Identity tests
id_tests <- id_test_info(pth = here(dirname(here()), "data", "biodiversity", 
                                    "output", "identity_tests"))

## Hypervolumes
hv_pk_comp <- readRDS(here(hypervolume_res_path, "hv_Pk_comp_m50.rds"))
hv_pk_comp@Name <- "P. kerguelensis (s.l.)"
hv_pkn <- readRDS(here(hypervolume_res_path, "hv_Pkn_m50.rds"))
hv_pkn@Name <- "P. kerguelensis"
hv_pf <- readRDS(here(hypervolume_res_path, "hv_Pf_m50.rds"))
hv_pu <- readRDS(here(hypervolume_res_path, "hv_Pu_m50.rds"))
hv_pus <- readRDS(here(hypervolume_res_path, "hv_Pus_m50.rds"))
hv_pus@Name <- "P. uskglassi"
hv_pj <- readRDS(here(hypervolume_res_path, "hv_Pj_m50.rds"))
hv_pm <- readRDS(here(hypervolume_res_path, "hv_Pm_m50.rds"))
hv_list <- list(hv_pk_comp, hv_pkn,hv_pf,hv_pu,hv_pus,hv_pj,hv_pm)
rm(hv_pk_comp,hv_pkn,hv_pf,hv_pu,hv_pus,hv_pj,hv_pm)

templist <- new("HypervolumeList")
templist@HVList <- hv_list
mycols <- c("#88CCEE", "#CC6677", "#DDCC77", "#117733", "#332288", "#AA4499", "#888888")

plot.HypervolumeList(templist, 
                     colors = mycols,
                     cex.names = 1.5,
                     cex.legend = 1,
                     show.legend = F,
                     names = c("Phosphate (Lt. max)", "Current Velocity (Lt. min)",
                               "Dissolved Oxygen (Min)", "Dissolved Oxygen (Range)",
                               "Silicate (Range)"))

## Hypervolume overlaps
# Volumes
set.seed(1234)

hv_vols <- list()
for(i in 1:length(hv_list)){
  hv_vols[[i]] <- get_volume(hv_list[[i]])
}
hv_vols <- unlist(hv_vols)

# Distances
hv_combs <- combn(7,2) #each combination of hv's

# centroids
cen_hv_dists <- list()
for(i in 1:ncol(hv_combs)){
  cen_dl <- hypervolume_distance(hv_list[[hv_combs[,i][1]]], 
                                 hv_list[[hv_combs[,i][2]]],
                                 type = "centroid")
  cen_hv_dists[[i]] <- cen_dl
}
cen_hv_dists <- unlist(cen_hv_dists)

m <- matrix(NA,7,6)
rownames(m) <- c(hv_list[[1]]@Name,hv_list[[2]]@Name,hv_list[[3]]@Name,
                 hv_list[[4]]@Name,hv_list[[5]]@Name,hv_list[[6]]@Name, 
                 hv_list[[7]]@Name)
colnames(m) <- c(hv_list[[1]]@Name,hv_list[[2]]@Name,hv_list[[3]]@Name,
                 hv_list[[4]]@Name,hv_list[[5]]@Name,hv_list[[6]]@Name)
m[lower.tri(m,diag=F)] <- cen_hv_dists

# minimum
min_hv_dists <- list()
for(i in 1:ncol(hv_combs)){
  min_dl <- hypervolume_distance(hv_list[[hv_combs[,i][1]]], 
                                 hv_list[[hv_combs[,i][2]]],
                                 type = "minimum",
                                 check.memory = F,
                                 num.points.max = 1000)
  min_hv_dists[[i]] <- min_dl
}
min_hv_dists <- unlist(min_hv_dists)

m2 <- matrix(NA,7,6)
rownames(m2) <- c(hv_list[[1]]@Name,hv_list[[2]]@Name,hv_list[[3]]@Name,
                  hv_list[[4]]@Name,hv_list[[5]]@Name,hv_list[[6]]@Name, 
                  hv_list[[7]]@Name)
colnames(m2) <- c(hv_list[[1]]@Name,hv_list[[2]]@Name,hv_list[[3]]@Name,
                  hv_list[[4]]@Name,hv_list[[5]]@Name,hv_list[[6]]@Name)
m2[lower.tri(m2,diag=F)] <- min_hv_dists

## Overlaps
hv_overlaps <- list()
hv_set_ops <- list()
for(i in 1:ncol(hv_combs)){
  hv_set <- hypervolume_set(hv_list[[hv_combs[,i][1]]],
                            hv_list[[hv_combs[,i][2]]], 
                            check.memory=FALSE)
  hv_set_ops[[i]] <- hv_set
  hv_overlaps[[i]] <- hypervolume_overlap_statistics(hv_set)
}

save(hv_overlaps, file = here("data", "biodiversity", "output", "hypervolumes", "hv_overlaps.RData"))
save(hv_set_ops, file = here("data", "biodiversity", "output", "hypervolumes", "hv_set_ops.RData"))

ov_df <- data.frame(jaccard = numeric(),
                    sorensen = numeric(),
                    frac_unique_1 = numeric(),
                    frac_unique_2 = numeric())

for(i in 1:length(hv_overlaps)){
  
  df <- as.data.frame(t(as.data.frame(unlist(hv_overlaps[[i]]))))
  ov_df <- bind_rows(ov_df, df)
  row.names(ov_df) <- NULL
}

species_1 <- c(rep(hv_list[[1]]@Name,6), rep(hv_list[[2]]@Name,5), 
               rep(hv_list[[3]]@Name,4), rep(hv_list[[4]]@Name,3), 
               rep(hv_list[[5]]@Name,2), hv_list[[6]]@Name)
species_2 <- c(hv_list[[2]]@Name,hv_list[[3]]@Name,hv_list[[4]]@Name,
               hv_list[[5]]@Name,hv_list[[6]]@Name, hv_list[[7]]@Name,
               hv_list[[3]]@Name, hv_list[[4]]@Name, hv_list[[5]]@Name,
               hv_list[[6]]@Name, hv_list[[7]]@Name, hv_list[[4]]@Name, 
               hv_list[[5]]@Name, hv_list[[6]]@Name, hv_list[[7]]@Name,
               hv_list[[5]]@Name,hv_list[[6]]@Name,hv_list[[7]]@Name,
               hv_list[[6]]@Name,hv_list[[7]]@Name,
               hv_list[[7]]@Name)
fin_ov_df <- data.frame(species_1, species_2, round(ov_df,2))

# minmaxdf <- data.frame("Species 1" = c("min", "max"),
#                        "Species 2" = c("min", "max"),
#                        "Jaccard" = c(0,1),
#                        "Sorensen" = c(0,1),
#                        "Frac unique sp1" = c(0,1),
#                        "Frac unique sp2" = c(0,1)) %>%
#             rename("Species 1" = "Species.1") %>%
#             rename("Species 2" = "Species.2") %>%
#             rename("Frac unique sp1" = "Frac.unique.sp1") %>%
#             rename("Frac unique sp2" = "Frac.unique.sp2") %>%
#             mutate(id1 = c(10,11)) %>%
#             relocate(id1) %>%
#             mutate(id2 = c(10,11)) %>%
#             relocate(id2, .before = "Species 2")

fin_ov_df_clean <- fin_ov_df %>%
  rename("Species 1" = "species_1") %>%
  rename("Species 2" = "species_2") %>%
  rename("Jaccard" = "jaccard") %>%
  rename("Sorensen" = "sorensen") %>%
  rename("Frac unique sp1" = "frac_unique_1") %>%
  rename("Frac unique sp2" = "frac_unique_2") %>%
  mutate(id1 = c(rep(1,6),rep(2,5),rep(3,4),rep(4,3),rep(5,2),6)) %>%
  relocate(id1) %>%
  mutate(id2 = c(2,3,4,5,6,7,3,4,5,6,7,4,5,6,7,5,6,7,6,7,7)) %>%
  relocate(id2, .before = "Species 2") #%>%
  #bind_rows(minmaxdf)

# Overlap tests
hv_ovs <- list.files(here("data", "biodiversity", "output", "hypervolumes"))
hv_ovt <- hv_ovs[str_detect(hv_ovs, "ot")][str_detect(hv_ovs[str_detect(hv_ovs, "ot")], "Pko", negate = T)]
hv_ovt <- factor(hv_ovt, levels = c("Pk_comp_Pkn_ot.rds", "Pk_comp_Pf_ot.rds", 
                            "Pk_comp_Pu_ot.rds", "Pk_comp_Pus_ot.rds",
                            "Pk_comp_Pj_ot.rds", "Pk_comp_Pm_ot.rds", 
                            "Pkn_Pf_ot.rds", "Pkn_Pu_ot.rds","Pkn_Pus_ot.rds", 
                            "Pkn_Pj_ot.rds", "Pkn_Pm_ot.rds", "Pf_Pu_ot.rds", 
                            "Pf_Pus_ot.rds","Pf_Pj_ot.rds", "Pf_Pm_ot.rds", 
                            "Pu_Pus_ot.rds", "Pu_Pj_ot.rds", "Pu_Pm_ot.rds", 
                            "Pus_Pj_ot.rds", "Pus_Pm_ot.rds", "Pj_Pm_ot.rds")) |>
  sort() |>
  as.character()

hv_overlaps <- loadRData(here("data", "biodiversity", "output", "hypervolumes", "hv_overlaps.RData"))

hv_tests <- hv_ot_info(fl_names = hv_ovt, 
                       hv_path = here(dirname(here()),"data", "biodiversity", 
                                      "output", "hypervolumes"),
                       hv_over_stats = hv_overlaps)

# Agreement between SDM and hypervolumes
library(irr)
sdm_d <- c("sig", "sig","non-sig","sig","non-sig","sig","sig","sig","sig","sig","sig",
           "sig","sig","sig","sig","sig","sig","sig","sig","sig","sig")
sdm_i <- c("sig", "sig","sig","sig","sig","sig","sig","sig","sig","sig","sig",
           "sig","sig","sig","sig","sig","sig","sig","sig","sig","sig")

hyp_jac <- c("sig", "sig","non-sig","non-sig","sig","non-sig","sig","sig","sig","sig","sig",
             "sig","non-sig","sig","sig","sig","sig","sig","sig","sig","sig")
hyp_sor <- c("sig", "sig","non-sig","non-sig","sig","non-sig","sig","sig","sig","sig","sig",
             "sig","non-sig","sig","sig","sig","sig","sig","sig","sig","sig")


sdm <- bind_cols(sdm_d, sdm_i)
hyp <- bind_cols(hyp_jac, hyp_sor)
sdm_hyp <- bind_cols(sdm_d, sdm_i, hyp_jac, hyp_sor)
agree(sdm)
agree(hyp)
agree(sdm_hyp)
