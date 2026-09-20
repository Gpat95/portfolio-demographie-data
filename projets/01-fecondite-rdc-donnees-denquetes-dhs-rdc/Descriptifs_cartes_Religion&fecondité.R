library(haven)
library(ggplot2)
library(tidyverse)
library(scales)
library(giscoR) # Get the world polygon and extract UK
library(geodata) #les provinces
library(terra)
library(sf) #convertir de spatvector (necessaire pour carto)
# Ouvrir R dans le dossier de ce projet (voir README.md).
#------------------------------------------------------------------------------
DRC <- gisco_get_countries(country = "COD", resolution = "1") #Shapefile RDC
plot(DRC["NAME_ENGL"])
prov_RDC <- geodata::gadm(country = "COD", level = 1, path = tempdir())
prov_RDC_sf <- st_as_sf(prov_RDC)
plot(prov_RDC_sf["VARNAME_1"])

clusters <- st_read("CD_2023-24_DHS_GEODATA/CDGE81FL/CDGE81FL.shp")
names(clusters)
plot(clusters["DHSCLUST"])
#----------------------------------------------------------------------
#Je prepare une seule table avec un seul TFR par cluster

 cluster_tfr <- Niveau2_cluster_Final %>%
  group_by(v021) %>%
  summarise(
    TFR = max(TFR, na.rm = TRUE),
    n_women = max(n_women, na.rm = TRUE),
    n_birth_5y = max(n_birth_5y, na.rm = TRUE),
    .groups = "drop") %>%
  mutate(
    TFR = ifelse(is.infinite(TFR), NA, TFR))
 
 #Je joint les TFRS aux points de clusters
 
 clusters_tfr_geo <- clusters %>%
   left_join(cluster_tfr, by = c("DHSCLUST" = "v021"))
 
 #Vérifier les projections
 st_crs(prov_RDC_sf) #WGS84
 st_crs(clusters_tfr) #WGS84
 ------------------------------------------------------
 #La carte
 
 clusters_tfr_geoCL <- clusters_tfr_geo %>%
   mutate(
     TFR_classe = cut(
       TFR,
       breaks = c(0, 4, 5, 6, 7, 8, 20),
       labels = c("<4", "4-5", "5-6", "6-7", "7-8", "8+"),
       include.lowest = TRUE))
 
 ggplot() +
   geom_sf(data = prov_RDC_sf, fill = "white", color = "black", linewidth = 0.25) +
   geom_sf(data = clusters_tfr_geoCL, aes(color = TFR_classe), size = 2, alpha = 0.95) +
   scale_color_brewer(
     name = "ISF",
     palette = "RdYlBu",
     direction = -1
   ) +
   coord_sf() +
   theme_bw() +
   theme(
     panel.grid.major = element_blank(),
     axis.text = element_blank(),
     axis.ticks = element_blank()
   ) +
   labs(
     title = "Répartition spatiale de l’indice synthétique de fécondité",
     subtitle = "Clusters DHS, RDC",
     caption = "Source : EDS RDC 2023–2024 (DHS Program). Calculs et cartographie : auteur.")
 
 ggsave(
   filename = "carte_ISF_clusters_RDC.png",
   width = 18,
   height = 14,
   units = "cm",
   dpi = 600)
 
 ----------------------------------------------------------------------------------------------------
#RELIGION
 
cl_rel <- Niveau2_cluster_Final
names(cl_rel)

cl_rel <- cl_rel %>%
  select(-TFR, -n_women, -n_birth_5y, -X_merge, -n_cluster_allsex)
   
   
 # Je joint les religion avec les points (clusters

 clusters_rel_geo <- clusters %>%
   left_join(cl_rel, by = c("DHSCLUST" = "v021")) %>%
   st_transform(st_crs(prov_RDC_sf))
 
 names(clusters_rel_geo)
 unique(clusters_rel_geo$religion)
 
 
 clusters_rel_geo <- clusters_rel_geo %>%
   mutate(
     religion = recode(
       religion,
       "_Catholiques" = "Catholiques",
       "_Eglises_de_reveil" = "Églises de réveil",
       "_Eglises_traditionnelles" = "Églises traditionnelles",
       "_Musulmans" = "Musulmans",
       "_Protestants" = "Protestants"
     )
   )
 
#Je cree la variable de classes de proportions
 
 clusters_rel_geo <- clusters_rel_geo %>%
   mutate(
     prop_classe = cut(
       prop_religion,
       breaks = c(0, 0.1, 0.25, 0.5, 0.75, 1),
       labels = c("0–10%", "10–25%", "25–50%", "50–75%", "75–100%"),
       include.lowest = TRUE))
 
 #La carte
 
 ggplot() +
   geom_sf(data = prov_RDC_sf, fill = "white", color = "grey40", linewidth = 0.2) +
   geom_sf(
     data = clusters_rel_geo,
     aes(color = prop_classe),
     size = 1.8,
     alpha = 0.95
   ) +
   scale_color_brewer(
     name = "Proportion",
     palette = "YlOrRd"
   ) +
   coord_sf() +
   facet_wrap(~ religion, ncol = 3) +
   theme_bw() +
   theme(
     panel.grid = element_blank(),
     axis.text = element_blank(),
     axis.ticks = element_blank(),
     strip.text = element_text(face = "bold"),
     plot.caption = element_text(size = 9, hjust = 0)
   ) +
   labs(
     title = "Répartition spatiale des religions par cluster",
     subtitle = "Proportions",
     caption = "Source : EDS RDC 2023–2024 (DHS Program). Calculs et cartographie : auteur."
   )
 
 ggsave(
   filename = "carte_REL_clusters_RDC.png",
   width = 18,
   height = 14,
   units = "cm",
   dpi = 600)
 
 
#------------------------------------
 
 #Representation de la variation de la pente
 
 
 slope_cluster_IRR <- read_dta("slope_cluster_IRR.dta")

 min(slope_cluster_geo$irr_educ)
 max(slope_cluster_geo$irr_educ)
   
 slope_cluster_geo <- clusters %>%
   left_join(slope_cluster_IRR, by = c("DHSCLUST" = "v021"))

 ggplot() +
   geom_sf(data = prov_RDC_sf, fill = "grey100", color = "grey55", linewidth = 0.2) +
   geom_sf(
     data = slope_cluster_geo,
     aes(color = irr_educ),
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
     aes(color = IRR_educ),
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
 
