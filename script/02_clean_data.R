############################
# Clean and formatting data

library("ClimMobTools")

# Read raw data 

# tricot cycle 1
ug_tricot_c1 <- read.csv("data/tricot_raw_climmob/nextgen_tricot_Uganda_c1.csv")

# tricot cycle 2
ug_tricot_c2 <- read.csv("data/tricot_raw_climmob/nextgen_tricot_Uganda_c2.csv")


# Correct district names for cycle 1
unique(ug_tricot_c1$registration_district)

ug_tricot_c1$registration_district <- gsub(pattern = "Mitiyana",
                                           x = ug_tricot_c1$registration_district, 
                                           replacement = "Mityana")

ug_tricot_c1$registration_district <- gsub(pattern = "Luweero",
                                           x = ug_tricot_c1$registration_district,
                                           replacement = "Luwero")

ug_tricot_c1$registration_district <- gsub(pattern = "Sereref",
                                           x = ug_tricot_c1$registration_district,
                                           replacement = "Serere")

unique(ug_tricot_c1$registration_district)

# Correct district name Kibaale for cycle 2
unique(ug_tricot_c2$registration_tricotdistrict)

ug_tricot_c2$registration_tricotdistrict <- gsub(pattern = "Kibale",
                                                 x = ug_tricot_c2$registration_tricotdistrict,
                                                 replacement = "Kibaale")

# Create variable cycle to identify each cycle data when merging
ug_tricot_c1$cycle <- rep(1, nrow(ug_tricot_c1))

ug_tricot_c2$cycle <- rep(2, nrow(ug_tricot_c2))

# Remove registration from columne names
names(ug_tricot_c1) <- gsub("registration_", "", names(ug_tricot_c1))

names(ug_tricot_c2) <- gsub("registration_", "", names(ug_tricot_c2))


# Rename variables so column names match among cycles

# Cycle 1
#overall performance harvest 12 MAP
ug_tricot_c1$overall_best <- ug_tricot_c1$X12map_harvest_field_overalperfharvst_pos

ug_tricot_c1$overall_worst <- ug_tricot_c1$X12map_harvest_field_overalperfharvst_neg

# Cycle 2
#overall performance harvest 12 MAP
ug_tricot_c2$overall_best <- ug_tricot_c2$tricot2021_2022fieldharvestdataat12map_qst_performharvest_pos

ug_tricot_c2$overall_worst <- ug_tricot_c2$tricot2021_2022fieldharvestdataat12map_qst_performharvest_neg

# Check variety names

# Cycle 1
unique(ug_tricot_c1$package_item_A)

unique(ug_tricot_c1$package_item_B)

unique(ug_tricot_c1$package_item_C)

# Cycle 2
unique(ug_tricot_c2$package_item_A)

unique(ug_tricot_c2$package_item_B)

unique(ug_tricot_c2$package_item_C)

# Make standard column names for geographical coordinates and planting dates 

# Cycle 1
ug_tricot_c1$longitude <- ug_tricot_c1$farm_geo_longitude
ug_tricot_c1$latitude <- ug_tricot_c1$farm_geo_latitude
ug_tricot_c1$planting_date <- ug_tricot_c1$d1map_plantingdate

# Cycle 2
ug_tricot_c2$longitude <- ug_tricot_c2$farm_geo_longitude
ug_tricot_c2$latitude <- ug_tricot_c2$farm_geo_latitude
ug_tricot_c2$planting_date <- ug_tricot_c2$plantingdate

# Check and format planting date

# Cycle 1
ug_tricot_c1$planting_date <- as.Date(ug_tricot_c1$planting_date)

mean_pdate_c1 <- as.Date(mean(ug_tricot_c1$planting_date, na.rm = T))

ug_tricot_c1[is.na(ug_tricot_c1$planting_date), "planting_date"] <- mean_pdate_c1

# Cycle 2
ug_tricot_c2[is.na(ug_tricot_c2$planting_date), "planting_date"]

ug_tricot_c2$planting_date <- as.Date(ug_tricot_c2$planting_date)

# Compute experiment end date
ug_tricot_c1$end_date <- ug_tricot_c1$planting_date + 365

ug_tricot_c2$end_date <- ug_tricot_c2$planting_date + 365

# Make new column names for district, age and gender in cycle 2 to match cycle 1

ug_tricot_c2$district <- ug_tricot_c2$tricotdistrict
ug_tricot_c2$age <- ug_tricot_c2$bioage
ug_tricot_c2$gender <- ug_tricot_c2$biogender


write.csv(x = ug_tricot_c1, file = "data/tricot_cleaned/ug_tricot_c1.csv")

write.csv(x = ug_tricot_c2, file = "data/tricot_cleaned/ug_tricot_c2.csv")

# Merge the two cycles in one data.frame only with selected columns

sel_columns <- c("package_item_A",
                 "package_item_B",
                 "package_item_C",
                 "overall_best",
                 "overall_worst",
                 "age",
                 "district",
                 "gender",
                 "longitude",
                 "latitude",
                 "planting_date",
                 "end_date",
                 "cycle")

ug_tricot_c1_df <- ug_tricot_c1[, sel_columns]

ug_tricot_c2_df <- ug_tricot_c2[,  sel_columns]

# Merge the two cycles in one data.frame
ug_tricot_df <- rbind(ug_tricot_c1_df, ug_tricot_c2_df)


# Remove rows with NAs in overall performance
ug_tricot_df <- ug_tricot_df[!is.na(ug_tricot_df$overall_best), ]

ug_tricot_df <- ug_tricot_df[!is.na(ug_tricot_df$overall_worst), ]

#remove ties and missing ranking elements

ug_tricot_df <- ug_tricot_df[ug_tricot_df$overall_best %in% c("A", "B", "C"), ]

ug_tricot_df <- ug_tricot_df[ug_tricot_df$overall_worst %in% c("A", "B", "C"), ]

#remove NAs from district, gender and age
ug_tricot_df <- ug_tricot_df[!is.na(ug_tricot_df$gender), ]

ug_tricot_df<- ug_tricot_df[!is.na(ug_tricot_df$district), ]

ug_tricot_df <- ug_tricot_df[!is.na(ug_tricot_df$age), ]

write.csv(x = ug_tricot_df, file = "data/tricot_cleaned/ug_tricot_merged.csv", row.names = FALSE)


