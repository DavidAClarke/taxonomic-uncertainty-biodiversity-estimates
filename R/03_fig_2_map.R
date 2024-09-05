################################################################################
## Script name: 00_fig_2_map.R
################################################################################

## Prepare data for map
# Occurrence data
bio_data_map <- st_transform(bio_data, "ESRI:102019")

# GADM
load(here(shared_data, "coastline", "gadm_lvl_0.RData"))
gadm <- st_as_sf(lvl_0) %>%
  st_transform("ESRI:102019") %>%
  st_crop(st_buffer(bio_data_map, 1500000))

# Antarctic coastline
coast <- st_read(here(shared_data, "coastline", "coastline.shp")) %>%
  st_transform("ESRI:102019") 

# Fronts
fronts <- st_read(here(shared_data, "southern_ocean_fronts", 
                       "shapefile", "antarctic_circumpolar_current_fronts.shp")) %>%
  st_transform("ESRI:102019") %>%
  st_crop(gadm) %>% mutate(CODE = case_when(
    DISPLAY == "Subantarctic Front" ~ "SAF",
    DISPLAY == "Polar Front" ~ "PF",
    DISPLAY == "Southern boundary" ~ "sACCF",
    DISPLAY == "Southern Antarctic Circumpolar Current Front" ~ "ACCF",
    DISPLAY == "Subtropical Front" ~ "STF"))

## Make map
ggplot() +
  geom_sf(
    data = gadm,
    fill="tan",
    show.legend = F
  ) +
  geom_sf(
    data = coast,
    aes(fill = surface),
    show.legend = F
  ) +
  scale_fill_manual(values = c("white", "#A7C7E7", "#A7C7E7", "#A7C7E7"),
                    breaks = unique(coast$surface),
                    guide = "none") +
  
  ggnewscale::new_scale_colour() +
  
  geom_sf(
    data = fronts,
    aes(linetype = CODE),
    show.legend = T
  ) +
  
  scale_linetype_manual(guide = guide_legend(theme = theme(
    legend.position = "bottom"),
    nrow = 2, ncol = 3),
    name = "Front",
    values = c(1,3,5,2,4)) +
  
  geom_sf(
    data = bio_data_map,
    aes(col = scientificName),
    show.legend = T
  ) +
  scale_color_manual("Species",
                     values = unlist(cartocolors[cartocolors$Name == "Safe",8])[-1],
                     breaks = unique(bio_data_map$scientificName),
                     labels = c(expression(italic("P. kerguelensis")),
                                expression(italic("P. fragarius")),
                                expression(italic("P. unruhi")),
                                expression(italic("P. uskglassi")),
                                expression(italic("P. joubini")),
                                expression(italic("P. mawsoni")))) +
  theme_bw() +
  theme(panel.background = element_rect(fill = "lightblue"),
        legend.key = element_rect(fill = "transparent", colour = "transparent"),
        panel.grid.major = element_line(colour = "transparent"),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        legend.position = "bottom",
        legend.direction = "horizontal",
        legend.text = element_text(size = 14),
        legend.title = element_text(size = 16)) +
  scale_y_continuous(expand = c(0,0)) +
  scale_x_continuous(expand = c(0,0))

rm(bio_data_map, fronts, gadm, lvl_0)