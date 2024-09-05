################################################################################
## Random Forest SDMs
################################################################################

st <- Sys.time()
res_old <- run_rf(data = bio_data, 
                  status = "genus", 
                  bias.cor = F,
                  n = 20,
                  preds = pred_list, 
                  group = c("G1", "G2"))
en <- Sys.time()
en-st 

st <- Sys.time()
res_old <- run_rf(data = bio_data, 
                  status = "comp", 
                  bias.cor = F,
                  n = 20,
                  preds = pred_list, 
                  group = c("G1", "G2"))
en <- Sys.time()
en-st 

st <- Sys.time()
res_new <- run_rf(data = bio_data, 
                  status = "new", 
                  bias.cor = F,
                  species = "Promachocrinus kerguelensis",
                  preds = pred_list[28:72], 
                  group = c("G1", "G2"))
en <- Sys.time()
en-st

st <- Sys.time()
res_new <- run_rf(data = bio_data, 
                  status = "new", 
                  bias.cor = F,
                  species = "Promachocrinus fragarius",
                  preds = pred_list, 
                  group = c("G1", "G2"))
en <- Sys.time()
en-st

st <- Sys.time()
res_new <- run_rf(data = bio_data, 
                  status = "new", 
                  bias.cor = F,
                  species = "Promachocrinus unruhi",
                  preds = pred_list, 
                  group = c("G1", "G2"))
en <- Sys.time()
en-st

st <- Sys.time()
res_new <- run_rf(data = bio_data, 
                  status = "new", 
                  bias.cor = F,
                  species = "Promachocrinus uskglass",
                  preds = pred_list, 
                  group = c("G1", "G2"))
en <- Sys.time()
en-st

st <- Sys.time()
res_new <- run_rf(data = bio_data, 
                  status = "new", 
                  bias.cor = F,
                  species = "Promachocrinus joubini",
                  preds = pred_list, 
                  group = c("G1", "G2"))
en <- Sys.time()
en-st

st <- Sys.time()
res_new <- run_rf(data = bio_data, 
                  status = "new", 
                  bias.cor = F,
                  species = "Promachocrinus mawsoni",
                  preds = pred_list, 
                  group = c("G1", "G2"))
en <- Sys.time()
en-st
