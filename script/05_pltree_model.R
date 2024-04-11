# PlackettLuce tree models 

library(gosset)
library(PlackettLuce)
library(ggplot2)
library(stablelearner)

source("script/utils/ggpltree.R")

# Load tricot data for overall performance at harvest
# Two cycles merged
ug_tricot_df <- read.csv("data/tricot_cleaned/ug_tricot_merged.csv")

# Load climatic covariates
agera5 <- read.csv(file = "data/env/agera5_clim_covars.csv")

rain_indices <- read.csv(file = "data/env/rain_indices.csv")

rain_indices <- rain_indices[, !colnames(rain_indices) %in% "Rtotal"]

temp_indices <- read.csv(file = "data/env/temp_indices.csv")

temp_indices <- temp_indices[, !grepl("max|min", 
                                    colnames(temp_indices))]


clim_covars <- cbind(agera5, rain_indices, temp_indices)

# Remove variables with near zero variability
vars_to_remove <- caret::nearZeroVar(clim_covars, names = T)

vars_to_remove

clim_covars <- clim_covars[, !colnames(clim_covars) %in% vars_to_remove]

clim_covars <- clim_covars[, !colnames(clim_covars) %in% vars_to_remove]

# Get the clones evaluated in each cycle

items_c1 <- unique(ug_tricot_df[ug_tricot_df$cycle == 1, "package_item_A"])

items_c2 <- unique(ug_tricot_df[ug_tricot_df$cycle == 2, "package_item_A"])

# Order the clones by name and by cycle
ordered_varieties <- c(sort(items_c1[!items_c1 %in% "NAROCASS 1"]),
                       "NAROCASS 1",
                       sort(items_c2[!items_c2 %in% "NAROCASS 1"]))


# Make a grouped PlackettLuce ranking
ug_tricot_rank <- rank_tricot(data = ug_tricot_df,
                              items = c("package_item_A", "package_item_B", "package_item_C"),
                              input = c("overall_best", "overall_worst"),
                              group = TRUE,
                              validate.rankings = T)

# Fit Plackett-Luce tree with sociodemographic and geographic covariates

ug_tricot_df$gender <- as.factor(ug_tricot_df$gender)

ug_tricot_df$district <- as.factor(ug_tricot_df$district)

ug_tricot_data <- cbind("R" = ug_tricot_rank, ug_tricot_df)

# Fit Plackett-Luce tree model
plt_1 <- pltree(R ~ gender + age + district ,
                data = ug_tricot_data,
                npseudo = .5,
                alpha = 0.05,
                verbose = TRUE,
                minsize = floor(nrow(ug_tricot_data) * .1),
                ref = "NAROCASS 1")


# Plot the tree
# plot(plt_1) + theme_bw()

ggplt_1 <- ggpltree(plt_1,
                    log = T,
                    qve = T,
                    ordered_items = ordered_varieties, 
                    ref = "NAROCASS 1",
                    group_1 = items_c1)

ggsave(filename = "output/figures/Figure_3_plt_sociodem_geo.png", 
       plot = ggplt_1, 
       width = 4600,
       height = 2300, 
       units = "px", 
       dpi = 300,
       bg = "white")



# Get summaries of each node
plt_1_n2_sum <- PlackettLuce:::summary.PlackettLuce(plt_1[[2]]$node$info$object, ref = "NAROCASS 1")

sink(file = "output/tables/plt_1_sum_node_2.txt")

print(plt_1_n2_sum)

sink()

sort(plt_1_n2_sum$coefficients[, 1], decreasing = T)

options(scipen = 999)

plt_1_n2_sum_ar <- dplyr::arrange(as.data.frame(plt_1_n2_sum$coefficients), desc(Estimate))

write.csv(x = round(plt_1_n2_sum_ar, 3), file = "output/tables/plt_1_sum_node_2.csv")

plt_1_n3_sum <- PlackettLuce:::summary.PlackettLuce(plt_1[[3]]$node$info$object, ref = "NAROCASS 1")

sink(file = "output/tables/plt_1_sum_node_3.txt")

print(plt_1_n3_sum)

sink()

sort(plt_1_n3_sum$coefficients[, 1], decreasing = T)

plt_1_n3_sum_ar <- dplyr::arrange(as.data.frame(plt_1_n3_sum$coefficients), desc(Estimate))

write.csv(x = round(plt_1_n3_sum_ar, 3), file = "output/tables/plt_1_sum_node_3.csv")

# #Stability assessment # Uncomment for run
# stb_01 <- NULL
# 
# set.seed(123)
# stb_01 <- stablelearner::stabletree(plt_1,
#                                     sampler = stablelearner::subsampling,
#                                     savetrees = TRUE,
#                                     B = 1000,
#                                     v = 0.8)
# 
# stb_01
# 
# saveRDS(object = stb_01, file = "output/stability_01.rds")

stb_01 <- readRDS("output/stability_01.rds")

summary(stb_01)
sum(colSums(stb_01$vs)) / 1000 * 100


# This one considers all the runs, even when they don't make a split
#rel_freqs_01 <- colSums(stb_01$vs) / 1000 * 100

#rel_freqs_01 <- c(rel_freqs_01, 100 - sum(rel_freqs_01))

# This one only considers the ones that produce a split
rel_freqs_01 <- colSums(stb_01$vs) / sum(colSums(stb_01$vs)) * 100

names(rel_freqs_01)[1] <- "sex"

rel_freqs_01_df <- data.frame("variable" = names(rel_freqs_01), "rel_freq" = rel_freqs_01)

rel_freqs_01_df$initial <- ifelse(rel_freqs_01_df$variable == "district", "yes", "no")

rel_freqs_01_df <- rel_freqs_01_df[rel_freqs_01_df$rel_freq > 0, ]

ggplot() +
  geom_col(data = rel_freqs_01_df, aes(x = reorder(variable, -rel_freq),
                                    y = rel_freq,
                                    fill = initial),
           width = 0.3) +
  theme(legend.position = "none",
        axis.text.x = element_text(angle = 35, hjust = 1)) +
  labs(x = "Variables",
       y = "Frequency (%)") +
  scale_fill_manual(values = c("yes" = "#b10026", "no" = "gray35")) + 
  scale_y_continuous(limits = c(0, 100), breaks = seq(from = 0, to = 100, by = 20))



ggsave("output/figures/Figure_4_stability_01.png", dpi = 300, width = 8, height = 4)



#---- End of stability assessment of plt_1 ----#

# Compute reliability for plt_1

rel_plt_1 <- reliability(plt_1, ref = "NAROCASS 1")

# Get reliability only for node 2
rel_plt_1_n_2 <- as.data.frame(rel_plt_1[rel_plt_1$node == 2, ])

rel_plt_1_n_2 <- dplyr::arrange(rel_plt_1_n_2, desc(reliability))

rel_plt_1_n_2

# Get reliability only for node 3
rel_plt_1_n_3 <- as.data.frame(rel_plt_1[rel_plt_1$node == 3, ])

rel_plt_1_n_3 <- dplyr::arrange(rel_plt_1_n_3, desc(reliability))

rel_plt_1_n_3


# Classify districts by main use of cassava

boiled_dist <- c("Buikwe",
                 "Luwero",
                 "Kibaale",
                 "Mityana",
                 "Dokolo")

flour_dist <- c("Serere",
                "Arua",
                "Busia",
                "Tororo",
                "Kaberamaido")


# Add a variable for main use by district
ug_tricot_df$main_use <- ifelse(ug_tricot_df$district %in% boiled_dist,
                                "Boiled meal",
                                "Flour meal")

ug_tricot_df$main_use <- as.factor(ug_tricot_df$main_use)

ug_tricot_data <- cbind("R" = ug_tricot_rank, ug_tricot_df)

# Fit a Plackett-Luce tree model variables from plt_1 plus main_use
plt_2 <- pltree(R ~  gender + age + district + main_use,
                data = ug_tricot_data,
                npseudo = .5,
                alpha = .05,
                verbose = TRUE,
                minsize = floor(nrow(ug_tricot_data) * .1),
                )

plot(plt_2) + theme_bw()

plt_2_rel <- as.data.frame(reliability(plt_2, ref = "NAROCASS 1"))

plt_2_rel_n2 <- plt_2_rel[plt_2_rel$node == 2, ]

plt_2_rel_n2 <- dplyr::arrange(plt_2_rel_n2, desc(reliability))


plt_2_rel_n2_df <- cbind("variety" = plt_2_rel_n2[, "item"],
                         round(plt_2_rel_n2[, 3:7], 3))

plt_2_rel_n2_df


plt_2_rel_n3 <- plt_2_rel[plt_2_rel$node == 3, ]

plt_2_rel_n3 <- dplyr::arrange(plt_2_rel_n3, desc(worth))

plt_2_rel_n3_df <- cbind("variety" = plt_2_rel_n3[, "item"],
                         round(plt_2_rel_n3[, 3:7], 3))

plt_2_rel_n3_df

gosset::top_items(plt_2)

strucchange::sctest(plt_2)

#---- Plackett-Luce tree model with climatic covariates ----#

ug_tricot_data <- cbind("R" = ug_tricot_rank, clim_covars)

#save table for manuscript
# write.csv(x = data.frame("varaible" = colnames(clim_covars),
#            "description" = vector(mode = "character", 
#                                   length = length(colnames(clim_covars))),
#            "unit" = vector(mode = "character", 
#                           length = length(colnames(clim_covars)))
#            ),
#           file = "output/clim_vars_table.csv"
#             )

covars <-  paste0(colnames(clim_covars),collapse = " + ")

form_1 <- as.formula(paste("R ~", covars))

nrow(ug_tricot_data)

plt_3 <- PlackettLuce::pltree(formula = R ~ .,
                                      data = ug_tricot_data,
                                      alpha = .05,
                                      minsize = floor(nrow(ug_tricot_data) * .1),
                                      #prune = "AIC",
                                      verbose = T,
                                      npseudo = .5,
                                      ref = "NAROCASS 1"
                                      )

#plot(plt_3, qve = F) + theme_bw()

ggplt_3 <- ggpltree(plt_3, 
                    qve = T, 
                    ref = "NAROCASS 1", 
                    ordered_items = ordered_varieties,
                    group_1 = items_c1)

ggsave(filename = "output/figures/plt_3_climate.png", plot = ggplt_3,
       width = 4600,
       height = 2300, 
       units = "px", 
       dpi = 300,
       bg = "white")


PlackettLuce:::summary.PlackettLuce(plt_3[[2]]$node$info$object, ref = "NAROCASS 1")

plt_3_n2_sum <- PlackettLuce:::summary.PlackettLuce(plt_3[[2]]$node$info$object, ref = "NAROCASS 1")

sink("output/tables/plt_3_sum_node_2.txt")

print(plt_3_n2_sum)

sink()

sort(plt_3_n2_sum$coefficients[, 1], decreasing = T)

plt_3_n2_sum_ar <- dplyr::arrange(as.data.frame(plt_3_n2_sum$coefficients), desc(Estimate))

write.csv(x = round(plt_3_n2_sum_ar, 3), file = "output/tables/plt_3_n2_sum_ar.csv")

plt_1_n2_sum_ar[1:5, ]
plt_3_n2_sum_ar[1:5, ]

PlackettLuce:::summary.PlackettLuce(plt_3[[3]]$node$info$object, ref = "NAROCASS 1")

plt_3_n3_sum <- PlackettLuce:::summary.PlackettLuce(plt_3[[3]]$node$info$object, ref = "NAROCASS 1")

sink("output/tables/plt_3_sum_node_3.txt")

print(plt_3_n3_sum)

sink()

sort(plt_3_n3_sum$coefficients[, 1], decreasing = T)

plt_3_n3_sum_ar <- dplyr::arrange(as.data.frame(plt_3_n3_sum$coefficients), desc(Estimate))

write.csv(x = round(plt_3_n3_sum_ar, 3), file = "output/tables/plt_3_n3_sum_ar.csv")

gosset::top_items(plt_3)

summary(plt_3)

sum_plt <- summary(plt_3, ref = "NAROCASS 1")

dplyr::arrange(as.data.frame(sum_plt[[1]]$coefficients), desc(Estimate))

ug_tricot_pld[ug_tricot_pld$R20mm > 2, "rainfall"]

plt_3_nodes <- predict(plt_3, type = "node")

ug_tricot_node_2 <- ug_tricot_data[plt_3_nodes == 2, ]

ug_tricot_node_2$node <- rep(2, nrow(ug_tricot_node_2))

unique(ug_tricot_node_2$district)

ug_tricot_node_3 <- ug_tricot_data[plt_3_nodes == 3, ]

unique(ug_tricot_node_3$district)

ug_tricot_node_3$node <- rep(3, nrow(ug_tricot_node_3))

ug_tricot_nodes <- rbind(ug_tricot_node_2, ug_tricot_node_3)

# #check PL tree stability
# library(stablelearner)
# 
# stb_02 <- NULL
# 
# set.seed(123)
# stb_02 <- stablelearner::stabletree(plt_3,
#                                     sampler = stablelearner::subsampling,
#                                     savetrees = TRUE,
#                                     B = 1000,
#                                     v = 0.8)
# 
# stb_02
# 
# saveRDS(stb_02, file = "output/stability_02.rds")

stb_02 <- readRDS("output/stability_02.rds")

summary(stb_02)

png(filename = "output/figures/plt_stability_002.png",
    width = 1200,
    height = 600,
    units = "px")

#barplot(stb_02)
sum(colSums(stb_02$vs))
sum(colSums(stb_02$vs))/1000 * 100

#rel_freqs_02 <- colSums(stb_02$vs) / 1000 * 100

#rel_freqs_02 <- c(rel_freqs_02, "no_split" = 100 - sum(rel_freqs_02))

rel_freqs_02 <- colSums(stb_02$vs) / sum(colSums(stb_02$vs)) * 100

rel_freqs_02_df <- data.frame("variable" = names(rel_freqs_02), "rel_freq" = rel_freqs_02)

rel_freqs_02_df$initial <- ifelse(rel_freqs_02_df$variable == "R20mm", "yes", "no")

rel_freqs_02_df <- rel_freqs_02_df[rel_freqs_02_df$rel_freq > 0, ]

ggplot() +
  geom_col(data = rel_freqs_02_df, aes(x = reorder(variable, -rel_freq),
                                    y = rel_freq,
                                    fill = initial)) +
  theme(legend.position = "none",
        axis.text.x = element_text(angle = 35, hjust = 1)) +
  labs(x = "Variables",
       y = "Frequency (%)") +
  scale_fill_manual(values = c("yes" = "#b10026", "no" = "gray35")) + 
  scale_y_continuous(limits = c(0, 55), breaks = seq(from = 0, to = 100, by = 10))


ggsave("output/figures/Figure_6_plt_stability_002.png", dpi = 300, width = 8, height = 4)


# Reliability assessment based on overall performance 12MAP

rel <- as.data.frame(reliability(plt_3, ref = "NAROCASS 1"))

rel_node_2 <- dplyr::arrange(rel[rel$node == 2, ], desc(reliability))

rel_node_2 <- cbind("variety" = rel_node_2[, "item"],
                    round(rel_node_2[, 3:7], 3))

rel_node_2

write.csv(x = rel_node_2, file = "output/tables/reliability_plt_3_n2.csv")

rel_node_3 <- dplyr::arrange(rel[rel$node == 3, ], desc(reliability))

rel_node_3 <- cbind("variety" = rel_node_3[, "item"],
                    round(rel_node_3[, 3:7], 5))

rel_node_3

write.csv(x = rel_node_3, file = "output/tables/reliability_plt_3_n3.csv")


#------------------------------------------------------------------#
uga_wat <- st_read(dsn = "data/geo/UGA_wat/UGA_water_areas_dcw.shp")

uga_dist <- st_read(dsn = "data/geo/Uganda_Ochola/Uganda Districts/uganda_districts_2019_i.shp")

ug_tricot_nodes_sf <- sf::st_as_sf(ug_tricot_nodes,
                                   coords = c("longitude",
                                              "latitude"), 
                                   crs = 4326)


ug_tricot_nodes_sf$node <- as.factor(ug_tricot_nodes_sf$node)

ggplot() +
  geom_sf(data = uga_dist) +
  geom_sf(data = nextgen_uga_dist, fill = "gray") +
  geom_sf(data = ug_tricot_nodes_sf, aes(color = node)) + 
  scale_color_manual(name = "node", values = c("#ca0020", "#104e8b")) +
  geom_sf_text(data = nextgen_uga_dist, 
               aes(label = DName2019), 
               color = "black", 
               fontface = "bold",
               nudge_x = c(0, 0, 0, 0, 0, 0, .1, -.15, 0, 0),
               nudge_y = c(0, -.05, .1, .1, 0, .09, 0, 0, .1, -.15))


plot(density(ug_tricot_data[ug_tricot_df$district == "Arua", "rainfall"]))

hist(ug_tricot_data[ug_tricot_df$district == "Arua", "rainfall"])

plot(density(ug_tricot_data[ug_tricot_df$district == "Mityana", "rainfall_total"]))

hist(ug_tricot_data[ug_tricot_df$district == "Mityana", "rainfall_total"])

plot(density(ug_tricot_data[ug_tricot_df$district == "Luwero", "rainfall_total"]))

plot(density(ug_tricot_data[ug_tricot_df$district == "Kibaale", "rainfall_total"]))


ug_tricot_data <- cbind(ug_tricot_data, "district" = ug_tricot_df$district)

ggplot() + 
  geom_freqpoly(data = ug_tricot_data, 
                 aes(x = rainfall_total),
                 binwidth = 100) + 
  facet_wrap(~ district)



