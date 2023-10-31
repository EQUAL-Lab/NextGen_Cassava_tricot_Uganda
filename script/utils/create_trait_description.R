#Creates a csv file with the traits from both cycles to be reviewed by Uganda scientists
library(ClimMobTools)

# additional functions from ClimMob-analysis
source("https://raw.githubusercontent.com/AgrDataSci/ClimMob-analysis/master/modules/01_functions.R")

ug_tricot_c1 <- read.csv(file = "data/tricot_cleaned/ug_tricot_c1.csv")

ug_tricot_c2 <- read.csv(file = "data/tricot_cleaned/ug_tricot_c2.csv")

# get an organized list of traits
traits_c1 <- getTraitList(ug_tricot_c1, c("_pos", "_neg"))

traits_c2 <- getTraitList(ug_tricot_c2, c("_pos", "_neg"))

#raw trait codes
raw_traits_c1 <- unlist(lapply(traits_c1, function(X){
  X$trait_label
}))

raw_traits_c2 <- unlist(lapply(traits_c2, function(X){
  X$trait_label
}))


# Create a data.frame with required variables for trait validation
traits_c1_df <- data.frame("trait_code" = sort(raw_traits_c1),
                           "trait_label" = rep("", length(raw_traits_c1)),
                           "description" = rep("", length(raw_traits_c1)),
                           "cycle" = rep("cycle_1", length(raw_traits_c1)),
                           "time" = rep("", length(raw_traits_c1)))

traits_c2_df <- data.frame("trait_code" = sort(raw_traits_c2),
                           "trait_label" = rep("", length(raw_traits_c1)),
                           "description" = rep("", length(raw_traits_c2)),
                           "cycle" = rep("cycle_2", length(raw_traits_c2)),
                           "time" = rep("", length(raw_traits_c2)))


#merge traits of the two cycles in one data.frame
traits_df <- rbind(traits_c1_df,
                   traits_c2_df)


traits_df

traits_df$time <- ifelse(grepl(pattern = "1map", x = traits_df$trait_code),
                         "1_MAP", ifelse(grepl(pattern = "3map", x = traits_df$trait_code),
                                         "3_MAP", ifelse(grepl(pattern = "6map", x = traits_df$trait_code),
                                                         "6_MAP", ifelse(grepl(pattern = "9map", x = traits_df$trait_code),
                                                                         "9_MAP", ifelse(grepl(pattern = "9map", x = traits_df$trait_code),
                                                                                         "9_MAP", ifelse(grepl(pattern = "12map", x = traits_df$trait_code),
                                                                                                         "12_MAP", traits_df$time))))))
traits_df
# Save the data.frame as a csv file
#write.csv(x = traits_df, file = "data/trait_labels/trait_description.csv")

###############################################






