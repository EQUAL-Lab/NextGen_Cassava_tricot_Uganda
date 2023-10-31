library(ClimMobTools)
library(gosset)
library(PlackettLuce)
library(ggplot2)

library(RColorBrewer)
library(ComplexHeatmap)
library(circlize)


# Load functions for trait labels formatting
source(file = "script/utils/get_trait_labels.R")
source(file = "script/utils/set_trait_labels.R")


# Load data
ug_tricot_c1 <- read.csv(file = "data/tricot_cleaned/ug_tricot_c1.csv")

ug_tricot_c2 <- read.csv(file = "data/tricot_cleaned/ug_tricot_c2.csv")

# Get list of traits
traits_c1 <- getTraitList(ug_tricot_c1, c("_pos", "_neg"))

traits_c2 <- getTraitList(ug_tricot_c2, c("_pos", "_neg"))

# Load trait labels revised by the Uganda scientists
trait_labels_rev <- as.data.frame(readxl::read_xlsx(path = "data/trait_labels/trait_description_002.xlsx"))

trait_labels_rev <- janitor::clean_names(trait_labels_rev)

# Format traits labels to be readable
# Cycle 1
for(i in seq_along(traits_c1)){
  
  if(traits_c1[[i]]$trait %in% trait_labels_rev$trait_code){
    traits_c1[[i]]$trait_label <- trait_labels_rev[trait_labels_rev$trait_code == traits_c1[[i]]$trait, ]$trait_label
  }
  
}

# Cycle 2
for(i in seq_along(traits_c2)){
  
  if(traits_c2[[i]]$trait %in% trait_labels_rev$trait_code){
    traits_c2[[i]]$trait_label <- trait_labels_rev[trait_labels_rev$trait_code == traits_c2[[i]]$trait, ]$trait_label
  }
  
}

# Print traits lists for checking
traits_c1

traits_c2

# Put the labels in separate lists 
trait_labels_c1 <- get_trait_labels(traits_c1)

trait_labels_c2 <- get_trait_labels(traits_c2)

# Set the trait lists names
names(traits_c1) <- trait_labels_c1

traits_c1

names(traits_c2) <- trait_labels_c2

traits_c2

# Set the reference or check variety
reference <- "NAROCASS 1"

# Get names of package labels
pack_index <- c("package_item_A", "package_item_B", "package_item_C")


# Get clones names for each cycle
items_c1 <- unique(unlist(ug_tricot_c1[pack_index]))

items_c1

items_c2 = unique(unlist(ug_tricot_c2[pack_index]))

items_c2

# Check common clones across cycles
items_c1[items_c1 %in% items_c2]

# Check the number of clones on each cycle
length(items_c1)

length(items_c2)

# Check which traits are common among cycles
sum(trait_labels_c1 %in% trait_labels_c2)

# Get common traits across cycles
common_traits <- trait_labels_c1[trait_labels_c1 %in% trait_labels_c2]

# Reorder common traits list by time point
traits_12MAP_reord <- grep(pattern = "12 MAP", x = common_traits, value = T)

traits_12MAP_reord

traits_12MAP_reord <- c("Stem quality - 12 MAP",
                        "Fresh root shape - 12 MAP",
                        "Fresh root size - 12 MAP",
                        "Fresh root yield - 12 MAP",
                        "Overall preferred fresh roots - 12 MAP",
                        "Overall performance harvest - 12 MAP",
                        "Boiled root mealiness - 12 MAP",
                        "Boiled root softness - 12 MAP",
                        "Less fibrous boiled root - 12 MAP",
                        "Preferred boiled food - 12 MAP",
                        "Overall preferred from planting to boiled food - 12 MAP",
                        "Ease of drying - 12 MAP",
                        "Ease of mixing flour paste - 12 MAP",
                        "Flour based meal color - 12 MAP",
                        "Taste of flour meal - 12 MAP",
                        "Texture of flour meal - 12 MAP",
                        "Preferred flour based meal - 12 MAP",
                        "Overall performance from planting to flour meal - 12 MAP")

common_traits_ord <- c(sort(grep(pattern = "3 MAP", x = common_traits, value = T), decreasing = T),
                       sort(grep(pattern = "6 MAP", x = common_traits, value = T), decreasing = T),
                       grep(pattern = "9 MAP", x = common_traits, value = T),
                       traits_12MAP_reord)


# Select only common traits from c1, in correct order
common_traits_c1 <- traits_c1[common_traits_ord]

#create rankings
comm_traits_c1_r <- lapply(common_traits_c1, function(x){
  rank_tricot(data = ug_tricot_c1[x$keep, ],
              items = pack_index,
              input = x$string)
})

# Select only common traits from c2, in correct order
common_traits_c2 <- traits_c2[common_traits_ord]

# Get rankings for each trait
comm_traits_c2_r <- lapply(common_traits_c2, function(x){
  rank_tricot(data = ug_tricot_c2[x$keep, ],
              items = pack_index,
              input = x$string)
})

# Combine two cycle rankings of common traits
combined_rankings <- mapply(function(X, Y){
  rbind(X, Y)},
  comm_traits_c1_r,
  comm_traits_c2_r)

# Check bind results
length(comm_traits_c1_r)
length(comm_traits_c2_r)
length(combined_rankings)
 
# Fit a Plackett-Luce model for each traits for the combined two cycles data
pl_mods <- lapply(combined_rankings, 
                  function(X){
                    PlackettLuce(X, ref = "NAROCASS 1")
                    })

#Check if trait labels order match among cycles datasets
data.frame("Cycle 1" = sapply(common_traits_c1, 
                              FUN = function(X){
                                X$trait_label
                                }),
           "Cycle 2" = sapply(common_traits_c2, 
                              FUN = function(X){
                                X$trait_label
                                }))

# Get a matrix with worth estimates from all the Plackett-Luce models
# Old way
#winprobs_old = gosset:::.combine_coeffs(pl_mods, log = TRUE, vcov = FALSE, ref = "NAROCASS 1")
#

# New way, take the summaries and then extract the estimates and p-values 
# Get summaries for each PL model
pl_sums <- lapply(pl_mods, function(X){
  summary(X, ref = "NAROCASS 1")
})


pl_rels <-  lapply(pl_mods, function(X){
  reliability(X, ref = "NAROCASS 1")
})


# Get the estimates in a matrix
# winprobs <- sapply(pl_sums, function(X){
#   X$coefficients[, "Estimate"]
# })

# Now using reliability
winprobs <- sapply(pl_rels, function(X){
  X$reliability
})

rownames(winprobs) <- pl_rels$`Susceptibility to diseases - 3 MAP`$item

# Get p-values
# pl_pvs <- sapply(pl_sums, function(X){
#   X$coefficients[, "Pr(>|z|)"]
# })

# Now using p-values from the reliability table

pl_pvs <- sapply(pl_rels, function(X){
  X$`Pr(>|z|)`
})

rownames(pl_pvs) <- pl_rels$`Susceptibility to diseases - 3 MAP`$item


#Check the order of columns and rows between winprobs and p-values
identical(colnames(winprobs), colnames(pl_pvs))
identical(rownames(winprobs), rownames(pl_pvs))

# Get a list with all the evaluated clones, ordered by name and cycle
ordered_varieties <- c(sort(items_c1[!items_c1 %in% "NAROCASS 1"]),
                       "NAROCASS 1",
                       sort(items_c2[!items_c2 %in% "NAROCASS 1"]))


remv_sufx <- c(" - 3 MAP", " - 6 MAP", "- 9 MAP"," - 12 MAP")

traits_short_labels <- common_traits_ord

for(i in remv_sufx){
  traits_short_labels <- sub(pattern = i, 
                          replacement = "",
                          traits_short_labels)
}

#Check the order
data.frame("order_1" = common_traits_ord,
           "order_2" = traits_short_labels)


# write.csv(x = data.frame("Traits - Time" = common_traits_ord,
#                          "Traits" = traits_short_labels),
#          file = "output/common_evaluated_traits_2_cycles.csv")

# Reorder estimates and p-values 
ordered_winprobs <- winprobs[ordered_varieties, ]

ordered_pvalues <- pl_pvs[ordered_varieties, ]

# Check results
# write.csv(x = winprobs, file = "output/winprobs.csv")
# write.csv(x = ordered_winprobs, file = "output/winprobs_ord.csv")
#write.csv(x = ordered_pvalues, file = "output/ordered_pvalues.csv")

# Use the short labels for the worthmap
colnames(ordered_winprobs) <- traits_short_labels

max(ordered_winprobs)
min(ordered_winprobs)
hist(as.matrix(ordered_winprobs))

quantile(as.matrix(ordered_winprobs))

# color_pal <- colorRamp2(colors = c("#D73019", "#FC8D59", "#FEE090", "#FFFFBF","white", "#E0F3F8", "#91BFDB", "#4575B4", "darkblue"), 
#                         breaks = c(-4, -3, -2, -1, -0.5, 0, 0.5, 1, 1.5),
#                         )

# Create an color palette
# color_pal <- colorRamp2(colors = c("#D73019", "#FC8D59", "#FEE090", "white",  "#E0F3F8", "#91BFDB", "#4575B4"), 
#                         breaks = c(-3.5, -2, -0.5, 0, 0.5, 1, 1.5))

color_pal <- colorRamp2(colors = c("#D73019", "#FC8D59", "#FEE090", "white",  "#E0F3F8", "#91BFDB", "#4575B4"), 
                        breaks = c(.1, .2, .3, 0.5, .6, .7, .8))

# Group rows by cycle
row_split_rule <- factor(c(rep("Cycle 1 { ", length(items_c1)-1),
                           "Check - ",
                           rep("Cycle 2 { ", length(items_c2)-1)), 
                         levels = c("Cycle 1 { ", "Check - ", "Cycle 2 { "))

# Group columns by time point
col_split_rule <- factor(c(rep("3 MAP", sum(grepl(pattern = "3 MAP", x =  colnames(winprobs)))), 
                           rep("6 MAP", sum(grepl(pattern = "6 MAP", x =  colnames(winprobs)))),
                           rep("9 MAP", sum(grepl(pattern = "9 MAP", x =  colnames(winprobs)))),
                           rep("12 MAP", sum(grepl(pattern = "12 MAP", x =  colnames(winprobs))))),
                         levels = c("3 MAP", "6 MAP", "9 MAP", "12 MAP"))


cycle_annotation <- HeatmapAnnotation(foo = anno_empty(border = TRUE))

# Plot the worthmap
hm <- Heatmap(matrix = ordered_winprobs,
              cluster_rows = F,
              cluster_columns =  F,
              col = color_pal,
              row_names_side = "left",
              row_names_gp = gpar(fontface = c(rep("plain", 11),
                                               "bold",
                                               rep("plain", 9))),
              column_names_rot = 35,
              cluster_row_slices = FALSE,
              row_split = row_split_rule,
              row_title = " ",
              column_title = " ",
              column_split = col_split_rule,
              row_gap = unit(1.5, "mm"),
              border = T,
              cell_fun = function(j, i, x, y, width, height, fill) {
                if(is.na(ordered_pvalues[i, j])){
                  grid.text("0.5", x, y, gp = gpar(fontsize = 10))
                }else{
                if(ordered_pvalues[i, j] < 0.05){
                  grid.text(sprintf("%.2f", ordered_winprobs[i, j]), x, y, gp = gpar(fontsize = 12, fontface = "bold"))
                }
                if(ordered_pvalues[i, j] > 0.05){
                  grid.text(sprintf("%.2f", ordered_winprobs[i, j]), x, y, gp = gpar(fontsize = 10))
                }
                
                }},
              rect_gp = gpar(col = "gray", lwd = 1),
              heatmap_legend_param = list(title = "Reliability"),
              left_annotation = rowAnnotation(foo = anno_block(gp = gpar(fill = c("#1a9850", "#ca0020", "#104e8b")),
                                                                labels = c("Cycle 1","","Cycle 2"),
                                                                labels_gp = gpar(col = "white"))),
              top_annotation = columnAnnotation(foo = anno_block(gp = gpar(fill = c("gray", "gray", "gray", "gray")),
                                                                 labels = c("3 MAP", "6 MAP", "9 MAP", "12 MAP"),
                                                                 labels_gp = gpar(col = "Black")))
              )

hm


png(filename = "output/figures/Figure_7_worthmap_all.png",
    width = 1300,
    height = 900,
    units = "px"
) 

draw(hm)

dev.off()
# End of plotting worth map

# Get the top-3 clones for each evaluated trait

sel <- matrix(nrow = nrow(ordered_pvalues),
              ncol = ncol(ordered_pvalues))

colnames(sel) <- colnames(ordered_pvalues)

rownames(sel) <- rownames(ordered_pvalues)


for(i in seq_along(colnames(ordered_pvalues))){
  
  for(j in seq_len(nrow(ordered_pvalues))){
    
    
    sel[j, i] <- ifelse(ordered_pvalues[j, i] < 0.05 && ordered_winprobs[j, i] > 0,
                        ordered_winprobs[j, i], NA)
    
    
  }
    
}

top_3 <- data.frame("trait" = colnames(ordered_winprobs),
           "Top_1" = NA,
           "Top_2" = NA,
           "Top_3" = NA)


for(i in seq_len(ncol(sel))){
  
  top_3[i, 2:4] <- names(sort(sel[, i], decreasing = T))[1:3]
  
}

write.csv(x = top_3,
          file = "output/tables/top_3.csv", 
          row.names = FALSE)



# Check correlation among traits

Heatmap(matrix = cor(ordered_winprobs),
        cluster_rows = F,
        cluster_columns =  T,
        #col = color_pal,
        row_names_side = "left",
        column_names_rot = 35,
        cluster_row_slices = FALSE,
        #row_split = row_split_rule,
        #row_title_rot = 0,
        row_title = " ",
        column_title = " ",
        #column_split = col_split_rule,
        row_gap = unit(1.5, "mm"),
        border = T,
        cell_fun = function(j, i, x, y, width, height, fill) {
          
            grid.text(sprintf("%.1f", cor(winprobs)[i, j]), x, y, gp = gpar(fontsize = 10))
          
            
          },
        rect_gp = gpar(col = "gray", lwd = 1),
        
)


############## END Worth analysis ######################

