################################################################################
## Script name: 05_identity_tests.R
################################################################################

################################# Promachocrinus ###############################

st <- Sys.time()
id_test <- identity.test(occurrence, pkn_occurrence,  
                         bg1, bg2,
                         "P. kerguelensis (putative)", "P. kerguelensis",
                         env = terra::stack(scale(pred_list[[50]])), 
                         type = "rf",
                         nreps = 99)
en <- Sys.time()
en - st
save(id_test, file = here("data", "biodiversity", "output", "pko_pkn_id_test.RData"))

pkn_bg <- bg2
pf_occurrence <- bio_data %>%
  dplyr::filter(scientificName == "Promachocrinus fragarius") %>%
  dplyr::mutate(x = sf::st_coordinates(.)[,1]) %>%
  dplyr::mutate(y = sf::st_coordinates(.)[,2]) %>%
  dplyr::select(x,y) %>%
  sf::st_drop_geometry()

pf_bg <- dismo::randomPoints(raster::stack(pred_list[[50]]), n = 50000) %>% 
  as.data.frame()

pko_occurrence <- occurrence
pko_bg <- bg1

st <- Sys.time()
pko_pf_id_test <- identity.test(pko_occurrence, pf_occurrence,  
                                pko_bg, pf_bg,
                                "P. kerguelensis", "P. fragarius",
                                env = terra::stack(scale(pred_list[[50]])), 
                                type = "rf",
                                nreps = 99)
en <- Sys.time()
en - st
save(pko_pf_id_test, file = here("data", "biodiversity", "output", "pko_pf_id_test.RData"))

pu_occurrence <- bio_data %>%
  dplyr::filter(scientificName == "Promachocrinus unruhi") %>%
  dplyr::mutate(x = sf::st_coordinates(.)[,1]) %>%
  dplyr::mutate(y = sf::st_coordinates(.)[,2]) %>%
  dplyr::select(x,y) %>%
  sf::st_drop_geometry()

pu_bg <- dismo::randomPoints(raster::stack(pred_list[[50]]), n = 50000) %>% 
  as.data.frame()

st <- Sys.time()
pko_pu_id_test <- identity.test(pko_occurrence, pu_occurrence,  
                                pko_bg, pu_bg,
                                "P. kerguelensis", "P. unruhi",
                                env = terra::stack(scale(pred_list[[50]])), 
                                type = "rf",
                                nreps = 99)
en <- Sys.time()
en - st
save(pko_pu_id_test, file = here("data", "biodiversity", "output", "pko_pu_id_test.RData"))

pus_occurrence <- bio_data %>%
  dplyr::filter(scientificName == "Promachocrinus uskglass") %>%
  dplyr::mutate(x = sf::st_coordinates(.)[,1]) %>%
  dplyr::mutate(y = sf::st_coordinates(.)[,2]) %>%
  dplyr::select(x,y) %>%
  sf::st_drop_geometry()

pus_bg <- dismo::randomPoints(raster::stack(pred_list[[50]]), n = 50000) %>% 
  as.data.frame()

st <- Sys.time()
pko_pus_id_test <- identity.test(pko_occurrence, pus_occurrence,  
                                pko_bg, pus_bg,
                                "P. kerguelensis", "P. uskglass",
                                env = terra::stack(scale(pred_list[[50]])), 
                                type = "rf",
                                nreps = 99)
en <- Sys.time()
en - st
save(pko_pus_id_test, file = here("data", "biodiversity", "output", "pko_pus_id_test.RData"))

pj_occurrence <- bio_data %>%
  dplyr::filter(scientificName == "Promachocrinus joubini") %>%
  dplyr::mutate(x = sf::st_coordinates(.)[,1]) %>%
  dplyr::mutate(y = sf::st_coordinates(.)[,2]) %>%
  dplyr::select(x,y) %>%
  sf::st_drop_geometry()

pj_bg <- dismo::randomPoints(raster::stack(pred_list[[50]]), n = 50000) %>% 
  as.data.frame()

st <- Sys.time()
pko_pj_id_test <- identity.test(pko_occurrence, pj_occurrence,  
                                 pko_bg, pj_bg,
                                 "P. kerguelensis", "P. joubini",
                                 env = terra::stack(scale(pred_list[[50]])), 
                                 type = "rf",
                                 nreps = 99)
en <- Sys.time()
en - st
save(pko_pj_id_test, file = here("data", "biodiversity", "output", "pko_pj_id_test.RData"))

pm_occurrence <- bio_data %>%
  dplyr::filter(scientificName == "Promachocrinus mawsoni") %>%
  dplyr::mutate(x = sf::st_coordinates(.)[,1]) %>%
  dplyr::mutate(y = sf::st_coordinates(.)[,2]) %>%
  dplyr::select(x,y) %>%
  sf::st_drop_geometry()

pm_bg <- dismo::randomPoints(raster::stack(pred_list[[50]]), n = 50000) %>% 
  as.data.frame()

st <- Sys.time()
pko_pm_id_test <- identity.test(pko_occurrence, pm_occurrence,  
                                pko_bg, pm_bg,
                                "P. kerguelensis (putative)", "P. mawsoni",
                                env = terra::stack(scale(pred_list[[50]])), 
                                type = "rf",
                                nreps = 99)
en <- Sys.time()
en - st
save(pko_pm_id_test, file = here("data", "biodiversity", "output", "pko_pm_id_test.RData"))

######################### P. kerguelensis (complex) ############################
p_occurrence <- bio_data %>%
  dplyr::mutate(x = sf::st_coordinates(.)[,1]) %>%
  dplyr::mutate(y = sf::st_coordinates(.)[,2]) %>%
  dplyr::select(x,y) %>%
  sf::st_drop_geometry()

pk_comp_occurrence <- bio_data %>%
  dplyr::filter(scientificName == "Promachocrinus kerguelensis" |
                  scientificName == "Promachocrinus joubini" | 
                  scientificName == "Promachocrinus fragarius" |
                  scientificName == "Promachocrinus unruhi" |
                  scientificName == "Promachocrinus uskglass") %>%
  dplyr::mutate(x = sf::st_coordinates(.)[,1]) %>%
  dplyr::mutate(y = sf::st_coordinates(.)[,2]) %>%
  dplyr::select(x,y) %>%
  sf::st_drop_geometry()

pk_comp_bg <- dismo::randomPoints(raster::stack(pred_list[[50]]), n = 50000) %>% 
  as.data.frame()

p_bg <- dismo::randomPoints(raster::stack(pred_list[[50]]), n = 50000) %>% 
  as.data.frame()

st <- Sys.time()
p_pk_comp_id_test <- identity.test(p_occurrence, pk_comp_occurrence,  
                                   p_bg, pk_comp_bg,
                                   "Promachocrinus", "P. kergeulensis (s.l.)",
                                   env = terra::scale(pred_list[[50]]), 
                                   type = "rf",
                                   nreps = 99)
en <- Sys.time()
en - st
save(p_pk_comp_id_test, file = here("data", "biodiversity", "output", "p_pk_comp_id_test.RData"))

pk_occurrence <- bio_data %>%
  dplyr::filter(scientificName == "Promachocrinus kerguelensis") %>%
  dplyr::mutate(x = sf::st_coordinates(.)[,1]) %>%
  dplyr::mutate(y = sf::st_coordinates(.)[,2]) %>%
  dplyr::select(x,y) %>%
  sf::st_drop_geometry()

pk_bg <- dismo::randomPoints(raster::stack(pred_list[[50]]), n = 50000) %>% 
  as.data.frame()

st <- Sys.time()
pk_comp_pk_id_test <- identity.test(pk_comp_occurrence, pk_occurrence,  
                                    pk_comp_bg, pk_bg,
                                    "P. kergeulensis (s.l.)", "P. kergeulensis (s.s.)",
                                    env = terra::scale(pred_list[[50]]), 
                                    type = "rf",
                                    nreps = 99)
en <- Sys.time()
en - st
save(pk_comp_pk_id_test, file = here("data", "biodiversity", "output", "pk_comp_pk_id_test.RData"))

pf_occurrence <- bio_data %>%
  dplyr::filter(scientificName == "Promachocrinus fragarius") %>%
  dplyr::mutate(x = sf::st_coordinates(.)[,1]) %>%
  dplyr::mutate(y = sf::st_coordinates(.)[,2]) %>%
  dplyr::select(x,y) %>%
  sf::st_drop_geometry()

pf_bg <- dismo::randomPoints(raster::stack(pred_list[[50]]), n = 50000) %>% 
  as.data.frame()

st <- Sys.time()
pk_comp_pf_id_test <- identity.test(pk_comp_occurrence, pf_occurrence,  
                                    pk_comp_bg, pf_bg,
                                    "P. kergeulensis (s.l.)", "P. fragarius",
                                    env = terra::scale(pred_list[[50]]), 
                                    type = "rf",
                                    nreps = 99)
en <- Sys.time()
en - st
save(pk_comp_pf_id_test, file = here("data", "biodiversity", "output", "pk_comp_pf_id_test.RData"))

pu_occurrence <- bio_data %>%
  dplyr::filter(scientificName == "Promachocrinus unruhi") %>%
  dplyr::mutate(x = sf::st_coordinates(.)[,1]) %>%
  dplyr::mutate(y = sf::st_coordinates(.)[,2]) %>%
  dplyr::select(x,y) %>%
  sf::st_drop_geometry()

pu_bg <- dismo::randomPoints(raster::stack(pred_list[[50]]), n = 50000) %>% 
  as.data.frame()

st <- Sys.time()
pk_comp_pu_id_test <- identity.test(pk_comp_occurrence, pu_occurrence,  
                                    pk_comp_bg, pu_bg,
                                    "P. kergeulensis (s.l.)", "P. unruhi",
                                    env = terra::scale(pred_list[[50]]), 
                                    type = "rf",
                                    nreps = 99)
en <- Sys.time()
en - st
save(pk_comp_pu_id_test, file = here("data", "biodiversity", "output", "pk_comp_pu_id_test.RData"))

pus_occurrence <- bio_data %>%
  dplyr::filter(scientificName == "Promachocrinus uskglass") %>%
  dplyr::mutate(x = sf::st_coordinates(.)[,1]) %>%
  dplyr::mutate(y = sf::st_coordinates(.)[,2]) %>%
  dplyr::select(x,y) %>%
  sf::st_drop_geometry()

pus_bg <- dismo::randomPoints(raster::stack(pred_list[[50]]), n = 50000) %>% 
  as.data.frame()

st <- Sys.time()
pk_comp_pus_id_test <- identity.test(pk_comp_occurrence, pus_occurrence,  
                                     pk_comp_bg, pus_bg,
                                     "P. kergeulensis (s.l.)", "P. uskglass",
                                     env = terra::scale(pred_list[[50]]), 
                                     type = "rf",
                                     nreps = 99)
en <- Sys.time()
en - st
save(pk_comp_pus_id_test, file = here("data", "biodiversity", "output", "pk_comp_pus_id_test.RData"))

pj_occurrence <- bio_data %>%
  dplyr::filter(scientificName == "Promachocrinus joubini") %>%
  dplyr::mutate(x = sf::st_coordinates(.)[,1]) %>%
  dplyr::mutate(y = sf::st_coordinates(.)[,2]) %>%
  dplyr::select(x,y) %>%
  sf::st_drop_geometry()

pj_bg <- dismo::randomPoints(raster::stack(pred_list[[50]]), n = 50000) %>% 
  as.data.frame()

st <- Sys.time()
pk_comp_pj_id_test <- identity.test(pk_comp_occurrence, pj_occurrence,  
                                    pk_comp_bg, pj_bg,
                                    "P. kergeulensis (s.l.)", "P. joubini",
                                    env = terra::scale(pred_list[[50]]), 
                                    type = "rf",
                                    nreps = 99)
en <- Sys.time()
en - st
save(pk_comp_pj_id_test, file = here("data", "biodiversity", "output", "pk_comp_pj_id_test.RData"))

pm_occurrence <- bio_data %>%
  dplyr::filter(scientificName == "Promachocrinus mawsoni") %>%
  dplyr::mutate(x = sf::st_coordinates(.)[,1]) %>%
  dplyr::mutate(y = sf::st_coordinates(.)[,2]) %>%
  dplyr::select(x,y) %>%
  sf::st_drop_geometry()

pm_bg <- dismo::randomPoints(raster::stack(pred_list[[50]]), n = 50000) %>% 
  as.data.frame()

st <- Sys.time()
pk_comp_pm_id_test <- identity.test(pk_comp_occurrence, pm_occurrence,  
                                    pk_comp_bg, pm_bg,
                                    "P. kergeulensis (s.l.)", "P. mawsoni",
                                    env = terra::scale(pred_list[[50]]), 
                                    type = "rf",
                                    nreps = 99)
en <- Sys.time()
en - st
save(pk_comp_pm_id_test, file = here("data", "biodiversity", "output", "pk_comp_pm_id_test.RData"))

################################ P. kerguelensis ###############################
Sys.time()
st <- Sys.time()
pkn_pf_id_test <- identity.test(pkn_occurrence, pf_occurrence,  
                                pkn_bg, pf_bg,
                                "P. kerguelensis", "P. fragarius",
                                env = terra::scale(pred_list[[50]]), 
                                type = "rf",
                                nreps = 99)
en <- Sys.time()
en - st
save(pkn_pf_id_test, file = here("data", "biodiversity", "output", "pkn_pf_id_test.RData"))

Sys.time()
st <- Sys.time()
pkn_pu_id_test <- identity.test(pkn_occurrence, pu_occurrence,  
                                pkn_bg, pu_bg,
                                "P. kerguelensis", "P. unruhi",
                                env = terra::stack(scale(pred_list[[50]])), 
                                type = "rf",
                                nreps = 99)
en <- Sys.time()
en - st
save(pkn_pu_id_test, file = here("data", "biodiversity", "output", "pkn_pu_id_test.RData"))

Sys.time()
st <- Sys.time()
pkn_pus_id_test <- identity.test(pkn_occurrence, pus_occurrence,  
                                pkn_bg, pus_bg,
                                "P. kerguelensis", "P. uskglass",
                                env = terra::stack(scale(pred_list[[50]])), 
                                type = "rf",
                                nreps = 99)
en <- Sys.time()
en - st
save(pkn_pus_id_test, file = here("data", "biodiversity", "output", "pkn_pus_id_test.RData"))

Sys.time()
st <- Sys.time()
pkn_pj_id_test <- identity.test(pkn_occurrence, pj_occurrence,  
                                pkn_bg, pj_bg,
                                "P. kerguelensis", "P. joubini",
                                env = terra::stack(scale(pred_list[[50]])), 
                                type = "rf",
                                nreps = 99)
en <- Sys.time()
en - st
save(pkn_pj_id_test, file = here("data", "biodiversity", "output", "pkn_pj_id_test.RData"))

Sys.time()
st <- Sys.time()
pkn_pm_id_test <- identity.test(pkn_occurrence, pm_occurrence,  
                                pkn_bg, pm_bg,
                                "P. kerguelensis", "P. mawsoni",
                                env = terra::stack(scale(pred_list[[50]])), 
                                type = "rf",
                                nreps = 99)
en <- Sys.time()
en - st
save(pkn_pm_id_test, file = here("data", "biodiversity", "output", "pkn_pm_id_test.RData"))

################################# P. fragarius #################################
Sys.time()
st <- Sys.time()
pf_pu_id_test <- identity.test(pf_occurrence, pu_occurrence,  
                                pf_bg, pu_bg,
                                "P. fragarius", "P. unruhi",
                                env = terra::stack(scale(pred_list[[50]])), 
                                type = "rf",
                                nreps = 99)
en <- Sys.time()
en - st
save(pf_pu_id_test, file = here("data", "biodiversity", "output", "pf_pu_id_test.RData"))
rm(pf_pu_id_test)

Sys.time()
st <- Sys.time()
pf_pus_id_test <- identity.test(pf_occurrence, pus_occurrence,  
                                 pf_bg, pus_bg,
                                 "P. fragarius", "P. uskglass",
                                 env = terra::stack(scale(pred_list[[50]])), 
                                 type = "rf",
                                 nreps = 99)
en <- Sys.time()
en - st
save(pf_pus_id_test, file = here("data", "biodiversity", "output", "pf_pus_id_test.RData"))
rm(pf_pus_id_test)

Sys.time()
st <- Sys.time()
pf_pj_id_test <- identity.test(pf_occurrence, pj_occurrence,  
                                pf_bg, pj_bg,
                                "P. fragarius", "P. joubini",
                                env = terra::stack(scale(pred_list[[50]])), 
                                type = "rf",
                                nreps = 99)
en <- Sys.time()
en - st
save(pf_pj_id_test, file = here("data", "biodiversity", "output", "pf_pj_id_test.RData"))
rm(pf_pj_id_test)

Sys.time()
st <- Sys.time()
pf_pm_id_test <- identity.test(pf_occurrence, pm_occurrence,  
                                pf_bg, pm_bg,
                                "P. fragarius", "P. mawsoni",
                                env = terra::stack(scale(pred_list[[50]])), 
                                type = "rf",
                                nreps = 99)
en <- Sys.time()
en - st
save(pf_pm_id_test, file = here("data", "biodiversity", "output", "pf_pm_id_test.RData"))
rm(pf_pm_id_test)

################################### P. unruhi ##################################
Sys.time()
st <- Sys.time()
pu_pus_id_test <- identity.test(pu_occurrence, pus_occurrence,  
                                pu_bg, pus_bg,
                                "P. unruhi", "P. uskglass",
                                env = terra::stack(scale(pred_list[[50]])), 
                                type = "rf",
                                nreps = 99)
en <- Sys.time()
en - st
save(pu_pus_id_test, file = here("data", "biodiversity", "output", "pu_pus_id_test.RData"))
rm(pu_pus_id_test)

Sys.time()
st <- Sys.time()
pu_pj_id_test <- identity.test(pu_occurrence, pj_occurrence,  
                               pu_bg, pj_bg,
                               "P. unruhi", "P. joubini",
                               env = terra::stack(scale(pred_list[[50]])), 
                               type = "rf",
                               nreps = 99)
en <- Sys.time()
en - st
save(pu_pj_id_test, file = here("data", "biodiversity", "output", "pu_pj_id_test.RData"))
rm(pu_pj_id_test)

Sys.time()
st <- Sys.time()
pu_pm_id_test <- identity.test(pu_occurrence, pm_occurrence,  
                               pu_bg, pm_bg,
                               "P. unruhi", "P. mawsoni",
                               env = terra::stack(scale(pred_list[[50]])), 
                               type = "rf",
                               nreps = 99)
en <- Sys.time()
en - st
save(pu_pm_id_test, file = here("data", "biodiversity", "output", "pu_pm_id_test.RData"))
rm(pu_pm_id_test)

################################# P. uskglass ##################################
Sys.time()
st <- Sys.time()
pus_pj_id_test <- identity.test(pus_occurrence, pj_occurrence,  
                               pus_bg, pj_bg,
                               "P. uskglass", "P. joubini",
                               env = terra::stack(scale(pred_list[[50]])), 
                               type = "rf",
                               nreps = 99)
en <- Sys.time()
en - st
save(pus_pj_id_test, file = here("data", "biodiversity", "output", "pus_pj_id_test.RData"))
rm(pus_pj_id_test)

Sys.time()
st <- Sys.time()
pus_pm_id_test <- identity.test(pus_occurrence, pm_occurrence,  
                               pus_bg, pm_bg,
                               "P. uskglass", "P. mawsoni",
                               env = terra::stack(scale(pred_list[[50]])), 
                               type = "rf",
                               nreps = 99)
en <- Sys.time()
en - st
save(pus_pm_id_test, file = here("data", "biodiversity", "output", "pus_pm_id_test.RData"))
rm(pus_pm_id_test)

################################# P. joubini ###################################
Sys.time()
st <- Sys.time()
pj_pm_id_test <- identity.test(pj_occurrence, pm_occurrence,  
                                pj_bg, pm_bg,
                                "P. joubini", "P. mawsoni",
                                env = terra::stack(scale(pred_list[[50]])), 
                                type = "rf",
                                nreps = 99)
en <- Sys.time()
en - st
save(pj_pm_id_test, file = here("data", "biodiversity", "output", "pj_pm_id_test.RData"))
rm(pj_pm_id_test)

