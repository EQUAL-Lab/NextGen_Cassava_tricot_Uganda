#Network connectivity
library(gosset)
library(network)
library(igraph)
library(GGally)
library(ggplot2)
library(PlackettLuce)

#Load tricot data
ug_tricot_df <- read.csv("data/tricot_cleaned/ug_tricot_merged.csv")

ug_tricot_rank <- rank_tricot(data = ug_tricot_df,
                                items = c("package_item_A", "package_item_B", "package_item_C"),
                                input = c("overall_best", "overall_worst"),
                                group = FALSE)

items_c1 <- unique(ug_tricot_df[ug_tricot_df$cycle == 1, ]$package_item_A)


variety_names <- unique(ug_tricot_df$package_item_A)

variety_names <- variety_names[!is.na(variety_names)]

varieties <- vector(mode = "list", length = length(variety_names))

varieties <- tapply(variety_names, 
                    INDEX = variety_names,
                    function(X){
                      y <- ug_tricot_df[ug_tricot_df$package_item_A == X, ]
                      y <- y[!is.na(y$package_item_A), ]
                    }
)

ug_tricot_adj <- adjacency(ug_tricot_rank)

net <- network::network(ug_tricot_adj, names.eval = "weights")

igraph_net <- graph.adjacency(ug_tricot_adj, weighted = T)

varieties <- network::network.vertex.names(net)

network::get.vertex.attribute(net, "vertex.names")

network::set.vertex.attribute(net, "Cycle", 
                              ifelse(varieties == "NAROCASS 1", 
                                     "Both (Check)", 
                                     ifelse(varieties %in% items_c1, 
                                     "Cycle 1", 
                                     "Cycle 2")))

cycle_palette <- c("Cycle 1" = "#1a9850",
                    "Both (Check)" = "#ca0020",
                    "Cycle 2" = "#104e8b")

GGally::ggnet2(net, 
               label = TRUE,
               arrow.size = 8, 
               edge.color = "gray40",
               mode = "fruchtermanreingold",
               arrow.gap = 0.03, 
               color = "Cycle",
               palette = cycle_palette,
               legend.size = 12,
               fontface = "plain") 

ggsave("output/connectivity_net_001.png", width = 14, height = 6, dpi = 600, units = "in")
