# Extract AgERA5 data using the package ag5Tools

library(ag5Tools)

#load tricot data for the two cycles

ug_tricot_df <- read.csv("data/tricot_cleaned/ug_tricot_merged.csv")

# Rainfall
rainfall <- vector(mode = "list", length = nrow(ug_tricot_df))

rainfall <- ag5_extract(coords = ug_tricot_df,
                        start_date = "planting_date",
                        end_date = "end_date",
                        lon = "longitude",
                        lat = "latitude",
                        variable = "Precipitation-Flux",
                        path = "data/env/",
                        cores = 4)

save(rainfall, file = "data/env/rainfall.rda")

# Max daytime temperature
max_dt <- vector(mode = "list", length = nrow(ug_tricot_df))

max_dt <- ag5_extract(coords = ug_tricot_df,
                      start_date = "planting_date",
                      end_date = "end_date",
                      lon = "longitude",
                      lat = "latitude",
                      variable = "Temperature-Air-2m",
                      statistic = "Max-Day-Time",
                      path = "data/env/",
                      celsius = TRUE,
                      cores = 6)

save(max_dt, file = "data/env/max_dt.rda")

# Min night temperature
min_nt <- vector(mode = "list", length = nrow(ug_tricot_df))

min_nt <- ag5_extract(coords = ug_tricot_df,
                      start_date = "planting_date",
                      end_date = "end_date",
                      lon = "longitude",
                      lat = "latitude",
                      variable = "Temperature-Air-2m",
                      statistic = "Min-Night-Time",
                      path = "data/env/",
                      celsius = TRUE,
                      cores = 6)

save(min_nt, file = "data/env/min_nt.rda")

# Mean-Day-Time temperature
mean_dt <- vector(mode = "list", length = nrow(ug_tricot_df))

mean_dt <- ag5_extract(coords = ug_tricot_df,
                       start_date = "planting_date",
                       end_date = "end_date",
                       lon = "longitude",
                       lat = "latitude",
                       variable = "Temperature-Air-2m",
                       statistic = "Mean-Day-Time",
                       path = "data/env/",
                       celsius = TRUE)

save(mean_dt, file = "data/env/mean_dt.rda")

# Mean-Night-Time temperature
mean_nt <- vector(mode = "list", length = nrow(ug_tricot_df))

mean_nt <- ag5_extract(coords = ug_tricot_df,
                       start_date = "planting_date",
                       end_date = "end_date",
                       lon = "longitude",
                       lat = "latitude",
                       variable = "Temperature-Air-2m",
                       statistic = "Mean-Night-Time",
                       path = "data/env/",
                       celsius = TRUE,
                       cores = 4)

save(mean_nt, file = "data/env/mean_nt.rda")

# Max-24h temperature
max_24t <- vector(mode = "list", length = nrow(ug_tricot_df))

max_24t <- ag5_extract(coords = ug_tricot_df,
                       start_date = "planting_date",
                       end_date = "end_date",
                       lon = "longitude",
                       lat = "latitude",
                       variable = "Temperature-Air-2m",
                       statistic = "Max-24h",
                       path = "data/env/",
                       celsius = TRUE,
                       cores = 5)

save(max_24t, file = "data/env/max_24t.rda")

# Mean-24h temperature
mean_24t <- vector(mode = "list", length = nrow(ug_tricot_df))

mean_24t <- ag5_extract(coords = ug_tricot_df,
                        start_date = "planting_date",
                        end_date = "end_date",
                        lon = "longitude",
                        lat = "latitude",
                        variable = "Temperature-Air-2m",
                        statistic = "Mean-24h",
                        path = "data/env/",
                        celsius = TRUE)

save(mean_24t, file = "data/env/mean_24t.rda")

# minimum temperature 24h
min_24t <- vector(mode = "list", length = nrow(ug_tricot_df))

min_24t <- ag5_extract(coords = ug_tricot_df,
                       start_date = "planting_date",
                       end_date = "end_date",
                       lon = "longitude",
                       lat = "latitude",
                       variable = "Temperature-Air-2m",
                       statistic = "Min-24h",
                       path = "data/env/",
                       celsius = TRUE)

save(min_24t, file = "data/env/min_24t.rda")

# Relative-Humidity-2m 06h
rhum_06 <- vector(mode = "list", length = nrow(ug_tricot_df))

rhum_06 <- ag5_extract(coords = ug_tricot_df,
                       start_date = "planting_date",
                       end_date = "end_date",
                       lon = "longitude",
                       lat = "latitude",
                       variable = "Relative-Humidity-2m",
                       time = "06h",
                       path = "data/env/")

save(rhum_06, file = "data/env/rhum_06.rda")

#-----------------------------#

# Relative-Humidity-2m 09h
rhum_09 <- vector(mode = "list", length = nrow(ug_tricot_df))

rhum_09 <- ag5_extract(coords = ug_tricot_df,
                       start_date = "planting_date",
                       end_date = "end_date",
                       lon = "longitude",
                       lat = "latitude",
                       variable = "Relative-Humidity-2m",
                       time = "09h",
                       path = "data/env/")

save(rhum_09, file = "data/env/rhum_09.rda")

#-----------------------------#

# Relative-Humidity-2m 12h
rhum_12 <- vector(mode = "list", length = nrow(ug_tricot_df))

rhum_12 <- ag5_extract(coords = ug_tricot_df,
                       start_date = "planting_date",
                       end_date = "end_date",
                       lon = "longitude",
                       lat = "latitude",
                       variable = "Relative-Humidity-2m",
                       time = "12h",
                       path = "data/env/",
                       cores = 5)

save(rhum_12, file = "data/env/rhum_12.rda")

#-----------------------------#

# Relative-Humidity-2m 15h
rhum_15 <- vector(mode = "list", length = nrow(ug_tricot_df))

rhum_15 <- ag5_extract(coords = ug_tricot_df,
                       start_date = "planting_date",
                       end_date = "end_date",
                       lon = "longitude",
                       lat = "latitude",
                       variable = "Relative-Humidity-2m",
                       time = "15h",
                       path = "data/env/",
                       cores = 5)

save(rhum_15, file = "data/env/rhum_15.rda")

#-----------------------------#

# Relative-Humidity-2m 18h
rhum_18 <- vector(mode = "list", length = nrow(ug_tricot_df))

rhum_18 <- ag5_extract(coords = ug_tricot_df,
                       start_date = "planting_date",
                       end_date = "end_date",
                       lon = "longitude",
                       lat = "latitude",
                       variable = "Relative-Humidity-2m",
                       time = "18h",
                       path = "data/env/",
                       cores = 5)

save(rhum_18, file = "data/env/rhum_18.rda")


#-----------------------------#

# Solar radiation flux
srf <- vector(mode = "list", length = nrow(ug_tricot_df))

srf <- ag5_extract(coords = ug_tricot_df,
                   start_date = "planting_date",
                   end_date = "end_date",
                   lon = "longitude",
                   lat = "latitude",
                   variable = "Solar-Radiation-Flux",
                   path = "data/env/",
                   cores = 5)

save(srf, file = "data/env/srf.rda")

