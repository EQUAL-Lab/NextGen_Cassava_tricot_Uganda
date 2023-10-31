library(climatrends)

climate_files <- list.files("data/env", pattern = ".rda",full.names = T)
  
lapply(climate_files, function(X) load(file = X, envir = .GlobalEnv))

clim_covar_names <- gsub(pattern = "(data/env/)|(.rda)", "", climate_files)

#clim_covar <- gsub("(data/env/)", climate_files)

clim_covars <- vector(mode = "list",
                      length = length(clim_covar_names))

names(clim_covars) <- clim_covar_names

clim_covars[clim_covar_names[3]]

for(i in seq_along(clim_covars)){
  
  clim_covars[[i]] <- unlist(lapply(get(clim_covar_names[i], .GlobalEnv), 
                                       function(X) mean(unlist(X))))
  print(i)
}

clim_covars_df <- as.data.frame(do.call(cbind, clim_covars))

clim_covars_df$rainfall_total <- unlist(lapply(rainfall, 
                                               function(X) sum(unlist(X))))
clim_covars_df$rainfall_total

write.csv(clim_covars_df, file = "data/env/agera5_clim_covars.csv", row.names = FALSE)


#### Get climate indices with climatrends #################################

# Rainfall
rain_indices <- vector(mode = "list", length = length(rainfall))

for(i in 1:length(rain_indices)){
  rain_indices[[i]] <- rainfall(object  = as.numeric(rainfall[[i]]),
                                day.one = colnames(rainfall[[i]])[1],
                                span    = length(rainfall[[i]]))
}

rain_indices <- do.call(rbind, rain_indices)

rain_indices <- as.data.frame(rain_indices)

rain_indices

write.csv(rain_indices, file = "data/env/rain_indices.csv", row.names = FALSE)


# Temperature

temp_indices <- vector(mode = "list", length = length(max_dt))

for(i in 1:length(max_dt)){
  temp_indices[[i]] <- temperature(object  = as.numeric(max_dt[[i]]),
                              tmin = as.numeric(min_nt[[i]]),
                              day.one = colnames(max_dt[[i]])[1],
                              span    = length(max_dt[[i]]))
  
}

temp_indices <- do.call(rbind, temp_indices)

temp_indices <- as.data.frame(temp_indices)

write.csv(temp_indices, file = "data/env/temp_indices.csv", row.names = FALSE)

#################################  


