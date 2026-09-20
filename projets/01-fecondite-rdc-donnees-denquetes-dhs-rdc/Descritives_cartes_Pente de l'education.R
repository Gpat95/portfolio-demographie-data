library(haven)
library(ggplot2)
library(tidyverse)
library(scales)
library(giscoR) # Get the world polygon and extract UK
library(geodata) #les provinces
library(terra)
library(sf) #convertir de spatvector (necessaire pour carto)
# Ouvrir R dans le dossier de ce projet (voir README.md).
DRC <- gisco_get_countries(country = "COD", resolution = "1") #Shapefile RDC
plot(DRC["NAME_ENGL"])
prov_RDC <- geodata::gadm(country = "COD", level = 1, path = tempdir())
prov_RDC_sf <- st_as_sf(prov_RDC)
plot(prov_RDC_sf["VARNAME_1"])

clusters <- st_read("CD_2023-24_DHS_GEODATA/CDGE81FL/CDGE81FL.shp")
names(clusters)
plot(clusters["DHSCLUST"])



slope_cluster <- read_dta("slope_cluster_IRR.dta")

slope_cluster_geo <- clusters %>%
  left_join(slope_cluster, by = c("DHSCLUST" = "v021"))

#-----------------------------
slope_cluster_geo_outliers <- slope_cluster_geo %>%
  filter(irr_educ >= 1)
save(slope_cluster_geo_outliers, file = "slope_cluster_geo_outliers.RData")



slope_cluster_geo_outliers_villages_osm <- read.csv("Depot_18Mars/slope_cluster_geo_outliers_villages_osm.csv")

# 1. Keep only cluster + residence from EDS women file
CDIR81FL <- read_dta("CDIR81FL.dta")

residence_cluster <- CDIR81FL %>%
  mutate(v025 = as_factor(v025)) %>%
  select(v021, v025) %>%
  distinct(v021, .keep_all = TRUE)

# 2. Add residence to your database, only for clusters already in your base
slope_cluster_geo_outliers_villages_osm <- slope_cluster_geo_outliers_villages_osm %>%
  left_join(residence_cluster, by = c("DHSCLUST" = "v021"))


nb_clusters_province <- CDIR81FL %>%
  mutate(province = as_factor(v024)) %>%
  select(province, v021) %>%
  distinct() %>%
  count(province, name = "nb_clusters")


nb_clusters_province <- nb_clusters_province %>%
  mutate(
    province = stri_trans_general(province, "Latin-ASCII"))


slope_cluster_geo_outliers_villages_osm <- slope_cluster_geo_outliers_villages_osm %>%
  left_join(nb_clusters_province,
            by = c("province"))


top20 <- slope_cluster_geo_outliers_villages_osm %>%
  arrange(desc(irr_educ)) %>%   # ordre décroissant
  slice_head(n = 20)          # garder les 20 plus grandes valeurs



write_sav(slope_cluster_geo_outliers_villages_osm, "slope_cluster_geo_outliers_villages_osm.sav")
#---------------------------------

min(slope_cluster_geo$slope_educ)
max(slope_cluster_geo$slope_educ)
min(slope_cluster_geo$irr_educ)
max(slope_cluster_geo$irr_educ)

#La carte

ggplot() +
  geom_sf(
    data = prov_RDC_sf,
    fill = "grey98",
    color = "grey72",
    linewidth = 0.25
  ) +
  
  geom_sf(
    data = slope_cluster_geo,
    aes(color = irr_educ),
    size = 2.4,
    alpha = 0.9
  ) +
  
  scale_color_gradient2(
    low = "#2166AC",
    mid = "#D9D9D9",
    high = "#B2182B",
    midpoint = 1,
    limits = c(0.6, 1.6),
    oob = scales::squish,
    breaks = c(0.6, 0.8, 1.0, 1.2, 1.4, 1.6),
    name = "IRR prédit",
    labels = label_number(accuracy = 0.1)
  ) +
  
  coord_sf(expand = FALSE) +
  
  labs(
    title = "Incidence Rate Ratio prédit par cluster DHS",
    subtitle = "République démocratique du Congo",
    caption = "Source : EDS RDC ; calculs de l’auteur."
  ) +
  
  guides(
    color = guide_colorbar(
      title.position = "top",
      barheight = unit(45, "mm"),
      barwidth = unit(5, "mm")
    )
  ) +
  
  theme_void(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    plot.subtitle = element_text(size = 10.5, color = "grey35"),
    plot.caption = element_text(size = 8.5, color = "grey40", hjust = 1),
    
    legend.position = "right",
    legend.title = element_text(face = "bold", size = 9.5),
    legend.text = element_text(size = 8.5),
    
    plot.margin = margin(6, 8, 6, 8))
    
  ggsave(
    filename = "carte_pente_ed_clster_RDC4.jpg",
    width = 18,
    height = 14,
    units = "cm",
    dpi = 600)
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  #--------Test
  
  #avec les etiquettes
  
  ggsave(
    filename = "carte_pente_ed_clster_RDC3.png",
    width = 18,
    height = 14,
    units = "cm",
    dpi = 600)
  
  ggplot() +
    geom_sf(data = prov_RDC_sf, fill = "grey100", color = "grey55", linewidth = 0.2) +
    
    # 👉 Noms des provinces
    geom_sf_text(
      data = prov_RDC_sf,
      aes(label = NAME_1),
      size = 3,
      color = "black"
    ) +
    
    geom_sf(
      data = slope_cluster_geo,
      aes(color = slope_educ),
      size = 1.8,
      alpha = 0.85
    ) +
    scale_color_gradient2(
      low = "#2166AC",
      mid = "#F0F0F0",
      high = "#B2182B",
      midpoint = 0,
      limits = c(-0.62, 0.62),
      name = "Coefficients aléatoires\nprédits",
      labels = label_number(accuracy = 0.01)
    ) +
    coord_sf() +
    labs(
      title = "",
      subtitle = "Pente estimée par cluster DHS en RDC",
      caption = "Source : EDS RDC, calculs de l’auteur."
    ) +
    theme_minimal(base_size = 12) +
    theme(
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      axis.text = element_blank(),
      axis.title = element_blank(),
      axis.ticks = element_blank(),
      legend.position = "right",
      legend.title = element_text(size = 10),
      legend.text = element_text(size = 9),
      plot.title = element_text(face = "bold", size = 14),
      plot.subtitle = element_text(size = 11),
      plot.caption = element_text(size = 9, color = "grey35")
    )
  
  
  ggsave(
    filename = "carte_pente_ed_clster_RDC4.png",
    width = 18,
    height = 14,
    units = "cm",
    dpi = 600)
  #$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$