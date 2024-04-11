library(sf)
library(ggplot2)
library(ggspatial)


#load tricot data
ug_tricot_df <- read.csv("data/tricot_cleaned/ug_tricot_merged.csv")

ken <- st_read(dsn = "data/geo/gadm41_KEN.gpkg", layer = "ADM_ADM_0")

tza <- st_read(dsn = "data/geo/gadm41_TZA.gpkg", layer = "ADM_ADM_0")

cod <- st_read(dsn = "data/geo/gadm41_COD.gpkg", layer = "ADM_ADM_0")

bdi <- st_read(dsn = "data/geo/gadm41_BDI.gpkg", layer = "ADM_ADM_0")

rwa <- st_read(dsn = "data/geo/gadm41_RWA.gpkg", layer = "ADM_ADM_0")

ssd <- st_read(dsn = "data/geo/gadm41_SSD.gpkg", layer = "ADM_ADM_0")

tza_wat <- st_read(dsn = "data/geo/TZA_wat/TZA_water_areas_dcw.shp")

uga_wat <- st_read(dsn = "data/geo/UGA_wat/UGA_water_areas_dcw.shp")

uga_dist <- st_read(dsn = "data/geo/Uganda_Ochola/Uganda_Districts/uganda_districts_2019_i.shp")


uga_dist$DName2019 <- tolower(uga_dist$DName2019)

uga_dist$DName2019 <- tools::toTitleCase(uga_dist$DName2019)


all_districts <- unique(ug_tricot_df$district)


ug_tricot_sf <- sf::st_as_sf(ug_tricot_df, coords = c("longitude",
                                                      "latitude"),
                             crs = 4326)

ug_tricot_sf$cycle <- as.factor(ug_tricot_sf$cycle)

nextgen_uga_dist <- uga_dist[uga_dist$DName2019 %in% all_districts, ]

#colors

water_color <- "#91C8E4"

nei_border_col <- "gray70"

#Figure 1

ggplot() +
  geom_sf(data = ken, fill = "#FDFAF6", color = nei_border_col, linewidth = 0.45) +
  geom_sf(data = ssd, fill = "#FDFAF6", color = nei_border_col, linewidth = 0.45) +
  geom_sf(data = rwa, fill = "#FDFAF6", color = nei_border_col, linewidth = 0.45) +
  geom_sf(data = tza, fill = "#FDFAF6", color = nei_border_col, linewidth = 0.45) +
  geom_sf(data = cod, fill = "#FDFAF6", color = nei_border_col, linewidth = 0.45) +
  geom_sf(data = uga_dist, fill = "gray90", color = "gray80", size = 0.1) +
  geom_sf(data = uga_wat, fill= water_color, color = NA) +
  geom_sf(data = tza_wat, fill= water_color, color = NA) +
  geom_sf(data = nextgen_uga_dist, fill = "darkgray", color = "white", linewidth = .5) +
  geom_sf(data = ug_tricot_sf, aes(color = cycle), size = 2.5) +
  geom_sf_text(data = nextgen_uga_dist,
               aes(label = DName2019),
               color = "black",
               size = 5,
               fontface = "bold",
               nudge_x = c(0, 0, 0, 0, -.45, -.4, .2, -.23, 0, 0),
               nudge_y = c(0, -.07, .15, .15, -.05, -.05, 0, 0, .1, -.18)) +
  coord_sf(xlim = c(29.5, 35), ylim = c(-1.3, 4), expand = T) +
  theme(panel.background = element_rect(fill = "lightgray"),
        axis.title.x = element_blank(),
        axis.title.y = element_blank()) +
  scale_color_manual(name = 'Cycle', values = c("#1a9850", "#104e8b")) +
  theme(legend.key.size = unit(x = 1, "cm"),
        legend.text = element_text(size = 14),
        legend.title = element_text(size = 16)) 

ggsave(filename = "output/figures/uganda_tricot_locations_002.png", dpi = 600, width = 10, height = 10)

# aez_palette <- c("#ffffbf",
#                  "#ff7f00",
#                  "#b2df8a",
#                  "#e41a1c",
#                  "#053061",
#                  "#4daf4a",
#                  "#984ea3",
#                  "#01665e",
#                  "#bf812d",
#                  "#ffff33",
#                  "#a65628",
#                  "#f781b9",
#                  "#999999",
#                  "#c51b7d")

#Agro-ecological Zones - Suplementary data - S1
# ggplot() + 
#   geom_sf(data = ken, fill = "gray95") +
#   geom_sf(data = ssd, fill = "gray95") +
#   geom_sf(data = rwa, fill = "gray95") +
#   geom_sf(data = tza, fill = "gray95") +
#   geom_sf(data = tza_wat, fill= "lightblue" ) +
#   geom_sf(data = cod, fill = "gray95") +
#   geom_sf(data = uga_dist, color = NA) +
#   #geom_sf(data = ug_tricot_adm_2, fill = "darkgray") +
#   #geom_sf(data = uga_aez, aes(fill = zone), color = NA, alpha = 1) +
#   scale_fill_manual(values = aez_palette) +
#   geom_sf(data = uga_wat, fill= "lightblue") +
#   geom_sf(data = uga_dist, fill = NA, linewidth = .1) + 
#   geom_sf(data = nextgen_uga_dist, color = "gray30", fill = NA, linewidth = .85) +
#   geom_sf(data = ug_tricot_sf, aes(shape = as.factor(cycle), color = as.factor(cycle)), size = 2) + 
#   scale_color_manual(values = c("gray20", "black")) +
#   geom_sf_text(data = nextgen_uga_dist,
#                aes(label = DName2019),
#                color = "black",
#                size = 5,
#                fontface = "bold",
#                nudge_x = c(0, 0, 0, 0, -.45, -.4, .2, -.23, 0, 0),
#                nudge_y = c(0, -.07, .15, .15, -.05, -.05, 0, 0, .1, -.18)) + 
#   # geom_sf_text(data = ug_tricot_adm_2, aes(label = NAME_2)) +
#   coord_sf(xlim = c(29.5, 35), ylim = c(-1.3, 4), expand = T) +
#   theme(panel.background = element_rect(fill = "lightgray"),
#         axis.title.x = element_blank(),
#         axis.title.y = element_blank()) 
# 
# 
# ggsave(filename = "output/uganda_tricot_locations_aez_001.png", dpi = 600, width = 10, height = 10)

nextgen_uga_dist$DName2016


# 10 Agro-ecological zones of Uganda
# Source: https://microdata.ubos.org:7070/index.php/catalog/62/study-description
# https://www.ubos.org/wp-content/uploads/publications/04_2022AAS2019_Report.pdf

Abi <- c("Arua", "Nebbi", "Moyo", "Adjumani", "Koboko", "Yumbe", "Maracha", "Terego", "Zombo")

Buginyanya <- c("Sironko", "Mbale", "Iganga", "Jinja", "Tororo", "Mayuge", "Namutumba", "Namayingo",
                "Luuka", "Kamuli", "Kaliro", "Buyende", "Bugiri", "Pallisa", "Kibuku", "Butaleja",
                "Busia", "Budaka", "Manafwa", "Kween", "Kapchorwa", "Bulambuli", "Bukwo", "Bududa")

Bulindi <- c("Hoima", "Masindi", "Kiryandongo", "Kibaale", "Buliisa", "Kakumiro", "Kagadi")

Kachwekano <- c("Kabale", "Rukungiri", "Kanungu", "Kisoro", "Rubanda")

Mukono <- c("Mukono", "Mpigi", "Kayunga", "Kalangala", "Kampala", "Luwero", "Masaka", "Nakasongola", "Mubende",
            "Wakiso", "Nakaseke", "Buikwe", "Buvuma", "Mityana", "Kiboga", "Kyankwanzi", "Gomba", "Kalungu", 
            "Bukomansimbi", "Butambala", "Lwengo")

Ngetta <- c("Lira", "Apac", "Dokolo", "Lamwo", "Nwoya", "Agago", "Alebtong", "Amolatar", "Kole", "Otuke", "Oyam",
            "Pader", "Kitgum", "Amuru", "Gulu", "Omoro")

Nabuin <- c("Moroto", "Nakapiripirit", "Kotido", "Napak", "Amudat", "Kaabong", "Abim")

Serere <- c("Serere", "Kumi", "Bukedea", "Amuria", "Ngora", "Katakwi", "Soroti", "Kaberamaido")

Mbarara <- c("Mbarara", "Ntungamo", "Bushenyi", "Kiruhura", "Lyantonde", "Sheema", "Rubirizi", "Mitooma",
             "Isingiro", "Ibanda", "Buhweju", "Ssembabule", "Rakai")

Rwebitaba <- c("Bundibugyo", "Kabarole", "Kamwenge", "Kasese", "Kyegegwa", "Kyenjojo", "Ntoroko")


zardi_names <- c("Abi", "Buginyanya", "Bulindi", "Kachwekano", "Mukono", 
                 "Ngetta", "Nabuin", "Serere", "Mbarara", "Rwebitaba")



Abi_sf <- sf::st_union(uga_dist[uga_dist$DName2016 %in% Abi, ])
Buginyanya_sf <- sf::st_union(uga_dist[uga_dist$DName2016 %in% Buginyanya, ])
Bulindi_sf <- sf::st_union(uga_dist[uga_dist$DName2016 %in% Bulindi, ])
Kachwekano_sf <- sf::st_union(uga_dist[uga_dist$DName2016 %in% Kachwekano, ])
Mukono_sf <- sf::st_union(uga_dist[uga_dist$DName2016 %in% Mukono, ])
Ngetta_sf <- sf::st_union(uga_dist[uga_dist$DName2016 %in% Ngetta, ])
Nabuin_sf <- sf::st_union(uga_dist[uga_dist$DName2016 %in% Nabuin, ])
Serere_sf <- sf::st_union(uga_dist[uga_dist$DName2016 %in% Serere, ])
Mbarara_sf <- sf::st_union(uga_dist[uga_dist$DName2016 %in% Mbarara, ])
Rwebitaba_sf <- sf::st_union(uga_dist[uga_dist$DName2016 %in% Rwebitaba, ])

ug_zardis <- c(Abi_sf,
               Buginyanya_sf,
               Bulindi_sf,
               Kachwekano_sf,
               Mukono_sf,
               Ngetta_sf,
               Nabuin_sf,
               Serere_sf,
               Mbarara_sf,
               Rwebitaba_sf)

# st_write(obj = ug_zardis,
#          dsn = "data/geo/zardis/zardis.gpkg"
#   )

ug_zardis_sf <- st_read(dsn = "data/geo/zardis/zardis.gpkg")

ug_zardis_sf$name <- zardi_names

zardi_colors <- c("#b2df8a",
                  "#D2DE32",
                  "#fb9a99",
                  "#D04848",
                  "#436850",
                  "#FE7A36",
                  "#fdbf6f",
                  "#6962AD",
                  "#B0926A",
                  "#b15928")

ggplot() +
  geom_sf(data = ken, fill = "#FDFAF6", color = nei_border_col, linewidth = 0.45) +
  geom_sf(data = ssd, fill = "#FDFAF6", color = nei_border_col, linewidth = 0.45) +
  geom_sf(data = rwa, fill = "#FDFAF6", color = nei_border_col, linewidth = 0.45) +
  geom_sf(data = tza, fill = "#FDFAF6", color = nei_border_col, linewidth = 0.45) +
  geom_sf(data = cod, fill = "#FDFAF6", color = nei_border_col, linewidth = 0.45) +
  #geom_sf(data = uga_dist, fill = "gray90", color = "gray80", size = 0.1) +
  #geom_sf(data = uga_dist, color = NA) +
  #geom_sf(data = nextgen_uga_dist, fill = "darkgray") +
  geom_sf(data = ug_zardis_sf, aes(fill = name),  color = "white", linewidth = .5) +
  geom_sf(data = uga_wat, fill= water_color, color = NA, alpha = .85) +
  geom_sf(data = tza_wat, fill= water_color, color = NA, alpha = .85) +
  # geom_sf_text(data = ug_zardis_sf,
  #              aes(label = name) ) +
  #geom_sf(data = uga_wat, fill= "lightblue", alpha = .65 ) +
  scale_fill_manual(values = zardi_colors, name = "ZARDI") +
  #geom_sf(data = uga_dist, fill = NA) +
  geom_sf(data = ug_tricot_sf, aes(shape = as.factor(cycle)), 
          size = 2, color = "gray15") +
  labs(shape = "Cycle") +
  coord_sf(xlim = c(29.5, 35), ylim = c(-1.3, 4), expand = T) 
  

ggsave(filename = "output/figures/uganda_tricot_locations_ZARDIs_002.png", dpi = 600, width = 10, height = 10)






