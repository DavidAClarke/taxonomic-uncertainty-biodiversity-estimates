################################################################################
## Script name: 04_run_rfe_reps.R
################################################################################

## Models for the genus and P. kerguelensis complex
status <- c("genus", "comp")

lapply(status, function(x){
  
  st <- Sys.time()
  res_old <- run_rf(data = bio_data, 
                    status = x, 
                    bias.cor = T,
                    n = 20,
                    preds = pred_list, 
                    group = c("G1", "G2"))
  en <- Sys.time()
  en-st 
  
})

## Models for each species
species <- c("Promachocrinus kerguelensis", "Promachocrinus fragarius",
             "Promachocrinus unruhi", "Promachocrinus unruhi", 
             "Promachocrinus uskglass", "Promachocrinus joubini",
             "Promachocrinus mawsoni")

lapply(species, function(x){
  
  st <- Sys.time()
  res_old <- run_rf(data = bio_data, 
                    status = "new", 
                    species = x,
                    bias.cor = T,
                    n = 20,
                    preds = pred_list, 
                    group = c("G1", "G2"))
  en <- Sys.time()
  en-st 
  
})
