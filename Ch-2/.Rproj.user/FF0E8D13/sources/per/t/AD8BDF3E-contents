#alternate analyses
library(tidyverse)

# analysis without forest-cropland mosaic or barren 

#### rPIm ####

#### breeding ####

load("data_outputs/rpis_breeding_2019.RData")

#now need to decide which landcover classes are "natural"
#and which are "impacted"
colnames(rpis_id_breeding)

natural <- c("Evergreen Needleleaf Forests PLAND","Evergreen Broadleaf Forests PLAND",                
             "Deciduous Needleleaf Forests PLAND","Deciduous Broadleaf Forests PLAND",                
             "Mixed Broadleaf/Needleleaf Forests PLAND",
             "Mixed Broadleaf Evergreen/Deciduous Forests PLAND",
             "Open Forests PLAND", "Woody Wetlands PLAND") 


impacted <- c(#"Forest/Cropland Mosaics PLAND",                    
              "Natural Herbaceous/Croplands Mosaics PLAND",       
              "Herbaceous Croplands PLAND")#, 
              #"Barren PLAND" )


rpis_cover <- rpis_id_breeding %>%
  mutate(natural = rowSums(select(., all_of(natural))),
         impacted = rowSums(select(., all_of(impacted))),
         ratio = (impacted+1)/(natural+1))

#new - rescale for only modified vs natural, instead of using ratio
#change for ease of comprehension, same basic relationship
rpis_cover <- rpis_cover %>%
  mutate(modified = impacted/(impacted+natural),
         forest = 1-modified)

breeding <- rpis_cover %>%
  group_by(species_code)%>%
  dplyr::summarize(modified_rPI = mean(modified, na.rm = TRUE),
                   modrPI_var = var(modified, na.rm = TRUE),
                   natural_rPI = mean(forest, na.rm = TRUE),
                   natrPI_var = var(forest, na.rm = TRUE))#,
#impacted1 = mean(impacted, na.rm = TRUE),
#imp_var = var(impacted, na.rm = TRUE))

load("data/species_basic.RData")

breeding_rpi_data <- inner_join(breeding, species_basic, by = "species_code")
save(breeding_rpi_data, file = "data_outputs/breeding_rpi_data_alt.RData")

#### postbreeding ####

load("data_outputs/rpis_postbreeding_2019.RData")

#now need to decide which landcover classes are "natural"
#and which are "impacted"


natural <- c("Evergreen Needleleaf Forests PLAND","Evergreen Broadleaf Forests PLAND",                
             "Deciduous Needleleaf Forests PLAND","Deciduous Broadleaf Forests PLAND",                
             "Mixed Broadleaf/Needleleaf Forests PLAND",
             "Mixed Broadleaf Evergreen/Deciduous Forests PLAND",
             "Open Forests PLAND", "Woody Wetlands PLAND") 


impacted <- c(#"Forest/Cropland Mosaics PLAND",                    
              "Natural Herbaceous/Croplands Mosaics PLAND",       
              "Herbaceous Croplands PLAND")#, 
              #"Barren PLAND" )

rpis_cover <- rpis_id_postbreeding %>%
  mutate(natural = rowSums(select(., all_of(natural))),
         impacted = rowSums(select(., all_of(impacted))),
         ratio = (impacted+1)/(natural+1))

#new - rescale for only modified vs natural, instead of using ratio
#change for ease of comprehension, same basic relationship
rpis_cover <- rpis_cover %>%
  mutate(modified = impacted/(impacted+natural),
         forest = 1-modified)

postbreeding <- rpis_cover %>%
  group_by(species_code)%>%
  dplyr::summarize(modified_rPI = mean(modified, na.rm = TRUE),
                   modrPI_var = var(modified, na.rm = TRUE),
                   natural_rPI = mean(forest, na.rm = TRUE),
                   natrPI_var = var(forest, na.rm = TRUE))#,
#impacted1 = mean(impacted, na.rm = TRUE),
#imp_var = var(impacted, na.rm = TRUE))

load("data/species_basic.RData")

postbreeding_rpi_data <- inner_join(postbreeding, species_basic, by = "species_code")
save(postbreeding_rpi_data, file = "data_outputs/postbreeding_rpi_data_alt.RData")

#### nonbreeding ####

load("data_outputs/rpis_nonbreeding_2019.RData")

#now need to decide which landcover classes are "natural"
#and which are "impacted"
#colnames(rpis_id_nonbreeding)

natural <- c("Evergreen Needleleaf Forests PLAND","Evergreen Broadleaf Forests PLAND",                
             "Deciduous Needleleaf Forests PLAND","Deciduous Broadleaf Forests PLAND",                
             "Mixed Broadleaf/Needleleaf Forests PLAND",
             "Mixed Broadleaf Evergreen/Deciduous Forests PLAND",
             "Open Forests PLAND", "Woody Wetlands PLAND") 


impacted <- c(#"Forest/Cropland Mosaics PLAND",                    
              "Natural Herbaceous/Croplands Mosaics PLAND",       
              "Herbaceous Croplands PLAND")#, 
              #"Barren PLAND" )


rpis_cover <- rpis_id_nonbreeding %>%
  mutate(natural = rowSums(select(., all_of(natural))),
         impacted = rowSums(select(., all_of(impacted))),
         ratio = (impacted+1)/(natural+1))

#new - rescale for only modified vs natural, instead of using ratio
#change for ease of comprehension, same basic relationship
rpis_cover <- rpis_cover %>%
  mutate(modified = impacted/(impacted+natural),
         forest = 1-modified)

nonbreeding <- rpis_cover %>%
  group_by(species_code)%>%
  dplyr::summarize(modified_rPI = mean(modified, na.rm = TRUE),
                   modrPI_var = var(modified, na.rm = TRUE),
                   natural_rPI = mean(forest, na.rm = TRUE),
                   natrPI_var = var(forest, na.rm = TRUE))#,
#impacted1 = mean(impacted, na.rm = TRUE),
#imp_var = var(impacted, na.rm = TRUE))


load("data/species_basic.RData")

nonbreeding_rpi_data <- inner_join(nonbreeding, species_basic, by = "species_code")
#save(nonbreeding_rpi_data, file = "data_outputs/IHM_nonbreeding_rpi_data_2019.RData")
save(nonbreeding_rpi_data, file = "data_outputs/nonbreeding_rpi_data_alt.RData")

#### prebreeding ####

load("data_outputs/rpis_prebreeding_2019.RData")

#now need to decide which landcover classes are "natural"
#and which are "impacted"
#colnames(rpis_id_prebreeding)

natural <- c("Evergreen Needleleaf Forests PLAND","Evergreen Broadleaf Forests PLAND",                
             "Deciduous Needleleaf Forests PLAND","Deciduous Broadleaf Forests PLAND",                
             "Mixed Broadleaf/Needleleaf Forests PLAND",
             "Mixed Broadleaf Evergreen/Deciduous Forests PLAND",
             "Open Forests PLAND", "Woody Wetlands PLAND") 


impacted <- c(#"Forest/Cropland Mosaics PLAND",                    
              "Natural Herbaceous/Croplands Mosaics PLAND",       
              "Herbaceous Croplands PLAND")#, 
              #"Barren PLAND" )


rpis_cover <- rpis_id_prebreeding %>%
  mutate(natural = rowSums(select(., all_of(natural))),
         impacted = rowSums(select(., all_of(impacted))),
         ratio = (impacted+1)/(natural+1))

#new - rescale for only modified vs natural, instead of using ratio
#change for ease of comprehension, same basic relationship
rpis_cover <- rpis_cover %>%
  mutate(modified = impacted/(impacted+natural),
         forest = 1-modified)

prebreeding <- rpis_cover %>%
  group_by(species_code)%>%
  dplyr::summarize(modified_rPI = mean(modified, na.rm = TRUE),
                   modrPI_var = var(modified, na.rm = TRUE),
                   natural_rPI = mean(forest, na.rm = TRUE),
                   natrPI_var = var(forest, na.rm = TRUE))#,
#impacted1 = mean(impacted, na.rm = TRUE),
#imp_var = var(impacted, na.rm = TRUE))

load("data/species_basic.RData")

prebreeding_rpi_data <- inner_join(prebreeding, species_basic, by = "species_code")
#save(prebreeding_rpi_data, file = "data_outputs/IHM_prebreeding_rpi_data_2019.RData")
save(prebreeding_rpi_data, file = "data_outputs/prebreeding_rpi_data_alt.RData")


#create migrants data
load("data_outputs/breeding_rpi_data_alt.RData")
breeding_rpi_data$season <- rep("breeding", length(breeding_rpi_data$species_code))
load("data_outputs/postbreeding_rpi_data_alt.RData")
postbreeding_rpi_data$season <- rep("postbreeding", length(postbreeding_rpi_data$species_code))
load("data_outputs/nonbreeding_rpi_data_alt.RData")
nonbreeding_rpi_data$season <- rep("nonbreeding", length(nonbreeding_rpi_data$species_code))
load("data_outputs/prebreeding_rpi_data_alt.RData")
prebreeding_rpi_data$season <- rep("prebreeding", length(prebreeding_rpi_data$species_code))

migrants_2019 <- rbind(breeding_rpi_data,postbreeding_rpi_data, nonbreeding_rpi_data,prebreeding_rpi_data)
migrants_2019$season <- factor(migrants_2019$season, levels=c("breeding", "postbreeding","nonbreeding", "prebreeding"))
save(migrants_2019, file = "data_outputs/rPI_migrants_alt.RData")


#### SHM ####

#will have to redo mod-mods, and all code after (but not nat-mods)



