library("ClimMobTools")
library("readr")

# fetch data from ClimMob

user_key <- readLines("token/api-key.txt")

getProjectsCM(user_key)

cycle_1 <- getDataCM(key = user_key, 
                    project = "NaCRRI02", 
                    userowner = "nextgennacrritricot",
                    as.data.frame = TRUE,
                    pivot.wider = TRUE, 
                    tidynames = T)


cycle_2 <- getDataCM(key = user_key, 
                    project = "TRICOT2021", 
                    userowner = "nextgennacrritricot",
                    as.data.frame = TRUE,
                    pivot.wider = TRUE, 
                    tidynames = T)

# Anonymize data removing farmers' names and telephone number

cycle_1[,grep(pattern = "name", x = colnames(cycle_1), value = T)]

cycle_1[,grep(pattern = "phone", x = colnames(cycle_1), value = T)]

remove_cycle_1 <- c("package_participant_name",
                    "registration_participant_name",
                    "registration_familiyofthefarmer",
                    "registration_telephone")

cycle_1 <- cycle_1[, !colnames(cycle_1) %in% remove_cycle_1]

colnames(cycle_2)

cycle_2[,grep(pattern = "name", x = colnames(cycle_2), value = T)]

cycle_2[,grep(pattern = "phone", x = colnames(cycle_2), value = T)]

remove_cycle_2 <- c("package_participant_name",
                    "registration_participant_name",
                    "registration_instancename",
                    "tricot2021mid.altitudedatacollection.d3map_instancename",
                    "tricot2021mid.altitudedata.6map_instancename",
                    "tricot2021_2022flour.basedmealdataat12map_instancename",
                    "tricot2021_2022fieldharvestdataat12map_instancename",
                    "tricot2021_2022boiledmealdataat12map_instancename",
                    "d9map22_instancename")



cycle_2 <- cycle_2[, !colnames(cycle_2) %in% remove_cycle_2]

write.csv(cycle_1, "data/tricot_raw_climmob/nextgen_tricot_Uganda_c1.csv", row.names = FALSE)

write.csv(cycle_2, "data/tricot_raw_climmob/nextgen_tricot_Uganda_c2.csv", row.names = FALSE)


