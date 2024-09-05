################################################################################
## Script name: 06_hypervolumes.R
################################################################################

## z-transform climate layers to make axes comparable
pred_list_sc <- pred_list

for(j in 1:length(pred_list)){
  
  pred_list_sc[[j]] <- scale(pred_list[[j]])
  
}

## Calculate hypervolumes
# Model 50 had the collection of most frequently important variables in each correlation group
# genus
calc_hypervolume(bio_data, status = "genus", model = 50)

# P. kergeulensis (sensu lato)
calc_hypervolume(bio_data, status = "comp", model = 50)

# Each species
sp <- bio_data %>% distinct(scientificName) %>% pull()

for(s in sp){
  
calc_hypervolume(bio_data, status = "new", model = 50, species = s)
  
}
