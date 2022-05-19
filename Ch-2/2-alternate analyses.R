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

load("data/species_basic.RData")

postbreeding_rpi_data <- inner_join(postbreeding, species_basic, by = "species_code")
save(postbreeding_rpi_data, file = "data_outputs/postbreeding_rpi_data_alt.RData")

#### nonbreeding ####

load("data_outputs/rpis_nonbreeding_2019.RData")

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

load("data/species_basic.RData")

nonbreeding_rpi_data <- inner_join(nonbreeding, species_basic, by = "species_code")
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

load("data/species_basic.RData")

prebreeding_rpi_data <- inner_join(prebreeding, species_basic, by = "species_code")
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

#note that alternate no FCM analysis for MHA is in the 2-landcover script

#### SHM ####

library(ebirdst)
library(dplyr)
library(raster)
library(prioritizr)
library(gurobi)
library(tidyverse)
library(here)
library(DBI)
library(ggsci)

#need updated run names
ebirdst_species <- ebirdst_runs %>%
  dplyr::select(c("species_code","run_name"))

my_species <- read.csv("data/bird_data_v4.csv")
my_species <- left_join(my_species, ebirdst_species, by = "species_code")

setwd("E:/eBird/data/raw/STEM")

natural <- c("mcd12q1_lccs1_fs_c11_1500_pland",
             "mcd12q1_lccs1_fs_c12_1500_pland",
             "mcd12q1_lccs1_fs_c13_1500_pland",
             "mcd12q1_lccs1_fs_c14_1500_pland",
             "mcd12q1_lccs1_fs_c15_1500_pland",
             "mcd12q1_lccs1_fs_c16_1500_pland",
             "mcd12q1_lccs1_fs_c21_1500_pland",
             "mcd12q1_lccs3_fs_c27_1500_pland")

modified <- c(#"mcd12q1_lccs2_fs_c25_1500_pland",
              "mcd12q1_lccs2_fs_c35_1500_pland",
              "mcd12q1_lccs2_fs_c36_1500_pland")#,
#"mcd12q1_lccs1_fs_c1_1500_pland")

natural1 <- c("Evergreen Needleleaf Forests","Evergreen Broadleaf Forests",                
              "Deciduous Needleleaf Forests","Deciduous Broadleaf Forests",                
              "Mixed Broadleaf/Needleleaf Forests",
              "Mixed Broadleaf Evergreen/Deciduous Forests",
              "Open Forests", "Woody Wetlands") 

modified1 <- c(#"Forest/Cropland Mosaics",                    
               "Natural Herbaceous/Croplands Mosaics",       
               "Herbaceous Croplands" )

###############################################################

#### BREEDING ####

###############################################################


breeding_pds <- list()
breeding_nat_mods <- vector(mode = "list", length = length(my_species$species_code))#list()
breeding_mod_mods <- vector(mode = "list", length = length(my_species$species_code))#list()


#this loops through 238 species
for(i in 1:length(my_species$species_code)){
  
  code <- my_species$species_code[i]
  
  start <- my_species %>%
    filter(species_code==code)%>%
    {.[["breeding_start_dt"]]}
  
  end <- my_species %>%
    filter(species_code==code)%>%
    {.[["breeding_end_dt"]]}
  
  bre <- c(start, end)  
  
  ext <-  c(xmin = -180, xmax = 180, 
            ymin = -90, ymax = 90)
  
  bre_extent <- ebirdst_extent(ext, t = bre)
  
  run_name <- my_species$run_name.y[i]
  
  skip_to_next <- FALSE
  
  tryCatch(
    pds <- load_pds(run_name), error = function(e){skip_to_next <<- TRUE})
  
  if(skip_to_next) { next }
  
  #this loops through all natural pd relationships for each species
  pds_nat <- list()
  nat_mods <- list()
  imp_mods <- list()
  
  for(j in 1:length(natural)){
    
    data <- ebirdst_subset(pds, bre_extent) %>%
      filter(predictor == natural[j])
    
    #some species don't have predictor pd data for certain land cover variables
    if(nrow(data)>1){
      lm <- lm(response ~ predictor_value, data = data)
      coef <- coef(lm)
      slope <- coef[2]
      int <- coef[1]
      
    }else{
      coef <- NA
      slope <- NA
      int <- NA
      
    }
    
    pds1 <- data.frame(slope,int,natural1[j],"natural")
    colnames(pds1)<- c("slope","intercept","lc","state")
    
    pds_nat[[j]] <- pds1
    nat_mods[[j]] <- vcov(lm) #note that if there wasn't enough data, the cov matrix was still generated
    #using the previous lm... shouldn't be a problem so long as you don't use the null pds
    
    
  }
  
  pds_nat1 <- bind_rows(pds_nat)
  
  #this loops through all impacted pd relationships for each species
  pds_imp <- list()
  
  for(k in 1:length(modified)){

    data <- ebirdst_subset(pds, bre_extent) %>%
      filter(predictor == modified[k])
    
    #some species don't have predictor pd data for certain land cover variables
    if(nrow(data)>1){
      lm <- lm(response ~ predictor_value, data = data)
      coef <- coef(lm)
      slope <- coef[2]
      int <- coef[1]
      
    }else{
      coef <- NA
      slope <- NA
      int <- NA
      
    }
    
    pds1 <- data.frame(slope,int,modified1[k],"modified")
    colnames(pds1)<- c("slope","intercept","lc","state")
    
    pds_imp[[k]] <- pds1
    imp_mods[[k]] <- vcov(lm)
    
  }
  
  pds_imp1 <- bind_rows(pds_imp)
  
  pds_all <- rbind(pds_nat1, pds_imp1)
  
  breeding_pds[[i]] <- pds_all
  breeding_nat_mods[[i]] <- nat_mods
  breeding_mod_mods[[i]] <- imp_mods
  
}

names(breeding_pds) <- my_species$species_code
names(breeding_nat_mods) <- my_species$species_code
names(breeding_mod_mods) <- my_species$species_code

breeding_pds1 <- bind_rows(breeding_pds, .id = "species_code")

setwd("D:/Allison/Github_Projects/Ch-2/Ch-2")

save(breeding_pds1, 
     file = "data_outputs/breeding_pds1_noFCM.RData")
save(breeding_nat_mods, file = "data_outputs/breeding_nat_mods_noFCM.RData")
save(breeding_mod_mods, file = "data_outputs/breeding_mod_mods_noFCM.RData")


#########################################################################################

#### POSTBREEDING ####

#########################################################################################

postbreeding_pds <- list()
postbreeding_nat_mods <- vector(mode = "list", length = length(my_species$species_code))#list()
postbreeding_mod_mods <- vector(mode = "list", length = length(my_species$species_code))#list()


#this loops through 238 species
for(i in 1:length(my_species$species_code)){
  
  code <- my_species$species_code[i]
  
  start <- my_species %>%
    filter(species_code==code)%>%
    {.[["postbreeding_migration_start_dt"]]}
  
  end <- my_species %>%
    filter(species_code==code)%>%
    {.[["postbreeding_migration_end_dt"]]}
  
  postbre <- c(start, end)  
  
  ext <-  c(xmin = -180, xmax = 180, 
            ymin = -90, ymax = 90)
  
  postbre_extent <- ebirdst_extent(ext, t = postbre)
  
  run_name <- my_species$run_name.y[i]
  
  skip_to_next <- FALSE
  
  tryCatch(
    pds <- load_pds(run_name), error = function(e){skip_to_next <<- TRUE})
  
  if(skip_to_next) { next }
  
  #this loops through all natural pd relationships for each species
  pds_nat <- list()
  nat_mods <- list()
  imp_mods <- list()
  
  for(j in 1:length(natural)){

    data <- ebirdst_subset(pds, postbre_extent) %>%
      filter(predictor == natural[j])
    
    #some species don't have predictor pd data for certain land cover variables
    if(nrow(data)>1){
      lm <- lm(response ~ predictor_value, data = data)
      coef <- coef(lm)
      slope <- coef[2]
      int <- coef[1]
      
    }else{
      coef <- NA
      slope <- NA
      int <- NA
      
    }
    
    pds1 <- data.frame(slope,int,natural1[j],"natural")
    colnames(pds1)<- c("slope","intercept","lc","state")
    
    pds_nat[[j]] <- pds1
    nat_mods[[j]] <- vcov(lm)
    
  }
  
  pds_nat1 <- bind_rows(pds_nat)
  
  #this loops through all impacted pd relationships for each species
  pds_imp <- list()
  
  for(k in 1:length(modified)){
    
    data <- ebirdst_subset(pds, postbre_extent) %>%
      filter(predictor == modified[k])
    
    #some species don't have predictor pd data for certain land cover variables
    if(nrow(data)>1){
      lm <- lm(response ~ predictor_value, data = data)
      coef <- coef(lm)
      slope <- coef[2]
      int <- coef[1]
      
    }else{
      coef <- NA
      slope <- NA
      int <- NA
      
    }
    
    pds1 <- data.frame(slope,int,modified1[k],"modified")
    colnames(pds1)<- c("slope","intercept","lc","state")
    
    pds_imp[[k]] <- pds1
    imp_mods[[k]] <- vcov(lm)
    
  }
  
  pds_imp1 <- bind_rows(pds_imp)
  
  pds_all <- rbind(pds_nat1, pds_imp1)
  
  postbreeding_pds[[i]] <- pds_all
  
  postbreeding_pds[[i]] <- pds_all
  postbreeding_nat_mods[[i]] <- nat_mods
  postbreeding_mod_mods[[i]] <- imp_mods
  
  
}

names(postbreeding_pds) <- my_species$species_code
names(postbreeding_nat_mods) <- my_species$species_code
names(postbreeding_mod_mods) <- my_species$species_code

postbreeding_pds1 <- bind_rows(postbreeding_pds, .id = "species_code")

setwd("D:/Allison/Github_Projects/Ch-2/Ch-2")

save(postbreeding_pds1, 
     file = "data_outputs/postbreeding_pds1_noFCM.RData")
save(postbreeding_nat_mods, file = "data_outputs/postbreeding_nat_mods_noFCM.RData")
save(postbreeding_mod_mods, file = "data_outputs/postbreeding_mod_mods_noFCM .RData")


#########################################################################################

#### NONBREEDING ####

#########################################################################################

nonbreeding_pds <- list()
nonbreeding_nat_mods <- vector(mode = "list", length = length(my_species$species_code))#list()
nonbreeding_mod_mods <- vector(mode = "list", length = length(my_species$species_code))#list()


#this loops through 238 species
for(i in 1:length(my_species$species_code)){
  
  code <- my_species$species_code[i]
  
  start <- my_species %>%
    filter(species_code==code)%>%
    {.[["nonbreeding_start_dt"]]}
  
  end <- my_species %>%
    filter(species_code==code)%>%
    {.[["nonbreeding_end_dt"]]}
  
  nonbre <- c(start, end)  
  
  ext <-  c(xmin = -180, xmax = 180, 
            ymin = -90, ymax = 90)
  
  nonbre_extent <- ebirdst_extent(ext, t = nonbre)
  
  run_name <- my_species$run_name.y[i]
  
  skip_to_next <- FALSE
  
  tryCatch(
    pds <- load_pds(run_name), error = function(e){skip_to_next <<- TRUE})
  
  if(skip_to_next) { next }
  
  #this loops through all natural pd relationships for each species
  pds_nat <- list()
  nat_mods <- list()
  imp_mods <- list()
  
  for(j in 1:length(natural)){
    
    data <- ebirdst_subset(pds, nonbre_extent) %>%
      filter(predictor == natural[j])
    
    #some species don't have predictor pd data for certain land cover variables
    if(nrow(data)>1){
      lm <- lm(response ~ predictor_value, data = data)
      coef <- coef(lm)
      slope <- coef[2]
      int <- coef[1]
      
    }else{
      coef <- NA
      slope <- NA
      int <- NA
      
    }
    
    pds1 <- data.frame(slope,int,natural1[j],"natural")
    colnames(pds1)<- c("slope","intercept","lc","state")
    
    pds_nat[[j]] <- pds1
    nat_mods[[j]] <- vcov(lm)
    
  }
  
  pds_nat1 <- bind_rows(pds_nat)
  
  #this loops through all impacted pd relationships for each species
  pds_imp <- list()
  
  for(k in 1:length(modified)){
    
    #pd_smooth <- plot_pds(pds, impacted[k], ext = bre_extent, show_stixel_pds = F, plot = F)
    data <- ebirdst_subset(pds, nonbre_extent) %>%
      filter(predictor == modified[k])
    
    #some species don't have predictor pd data for certain land cover variables
    if(nrow(data)>1){
      lm <- lm(response ~ predictor_value, data = data)
      coef <- coef(lm)
      slope <- coef[2]
      int <- coef[1]
      
    }else{
      coef <- NA
      slope <- NA
      int <- NA
      
    }
    
    pds1 <- data.frame(slope,int,modified1[k],"modified")
    colnames(pds1)<- c("slope","intercept","lc","state")
    
    pds_imp[[k]] <- pds1
    imp_mods[[k]] <- vcov(lm)
    
  }
  
  pds_imp1 <- bind_rows(pds_imp)
  
  pds_all <- rbind(pds_nat1, pds_imp1)
  
  nonbreeding_pds[[i]] <- pds_all
  
  nonbreeding_pds[[i]] <- pds_all
  nonbreeding_nat_mods[[i]] <- nat_mods
  nonbreeding_mod_mods[[i]] <- imp_mods
  
  
}

names(nonbreeding_pds) <- my_species$species_code
names(nonbreeding_nat_mods) <- my_species$species_code
names(nonbreeding_mod_mods) <- my_species$species_code

nonbreeding_pds1 <- bind_rows(nonbreeding_pds, .id = "species_code")

setwd("D:/Allison/Github_Projects/Ch-2/Ch-2")

save(nonbreeding_pds1, 
     file = "data_outputs/nonbreeding_pds1_noFCM.RData")
save(nonbreeding_nat_mods, file = "data_outputs/nonbreeding_nat_mods_noFCM.RData")
save(nonbreeding_mod_mods, file = "data_outputs/nonbreeding_mod_mods_noFCM.RData")



#########################################################################################

#### PREBREEDING ####

#########################################################################################

prebreeding_pds <- list()
prebreeding_nat_mods <- vector(mode = "list", length = length(my_species$species_code))#list()
prebreeding_mod_mods <- vector(mode = "list", length = length(my_species$species_code))#list()


#this loops through 238 species
for(i in 1:length(my_species$species_code)){
  
  code <- my_species$species_code[i]
  
  start <- my_species %>%
    filter(species_code==code)%>%
    {.[["prebreeding_migration_start_dt"]]}
  
  end <- my_species %>%
    filter(species_code==code)%>%
    {.[["prebreeding_migration_end_dt"]]}
  
  prebre <- c(start, end)  
  
  ext <-  c(xmin = -180, xmax = 180, 
            ymin = -90, ymax = 90)
  
  prebre_extent <- ebirdst_extent(ext, t = prebre)
  
  run_name <- my_species$run_name.y[i]
  
  skip_to_next <- FALSE
  
  tryCatch(
    pds <- load_pds(run_name), error = function(e){skip_to_next <<- TRUE})
  
  if(skip_to_next) { next }
  
  #this loops through all natural pd relationships for each species
  pds_nat <- list()
  nat_mods <- list()
  imp_mods <- list()
  
  for(j in 1:length(natural)){
    
    data <- ebirdst_subset(pds, prebre_extent) %>%
      filter(predictor == natural[j])
    
    #some species don't have predictor pd data for certain land cover variables
    if(nrow(data)>1){
      lm <- lm(response ~ predictor_value, data = data)
      coef <- coef(lm)
      slope <- coef[2]
      int <- coef[1]
      
    }else{
      coef <- NA
      slope <- NA
      int <- NA
      
    }
    
    pds1 <- data.frame(slope,int,natural1[j],"natural")
    colnames(pds1)<- c("slope","intercept","lc","state")
    
    pds_nat[[j]] <- pds1
    nat_mods[[j]] <- vcov(lm)
    
  }
  
  pds_nat1 <- bind_rows(pds_nat)
  
  #this loops through all impacted pd relationships for each species
  pds_imp <- list()
  
  for(k in 1:length(modified)){
    
    data <- ebirdst_subset(pds, prebre_extent) %>%
      filter(predictor == modified[k])
    
    #some species don't have predictor pd data for certain land cover variables
    if(nrow(data)>1){
      lm <- lm(response ~ predictor_value, data = data)
      coef <- coef(lm)
      slope <- coef[2]
      int <- coef[1]
      
    }else{
      coef <- NA
      slope <- NA
      int <- NA
      
    }
    
    pds1 <- data.frame(slope,int,modified1[k],"modified")
    colnames(pds1)<- c("slope","intercept","lc","state")
    
    pds_imp[[k]] <- pds1
    imp_mods[[k]] <- vcov(lm)
    
  }
  
  pds_imp1 <- bind_rows(pds_imp)
  
  pds_all <- rbind(pds_nat1, pds_imp1)
  
  prebreeding_pds[[i]] <- pds_all
  
  prebreeding_pds[[i]] <- pds_all
  prebreeding_nat_mods[[i]] <- nat_mods
  prebreeding_mod_mods[[i]] <- imp_mods
  
  
}

names(prebreeding_pds) <- my_species$species_code
names(prebreeding_nat_mods) <- my_species$species_code
names(prebreeding_mod_mods) <- my_species$species_code

prebreeding_pds1 <- bind_rows(prebreeding_pds, .id = "species_code")

setwd("D:/Allison/Github_Projects/Ch-2/Ch-2")

save(prebreeding_pds1, 
     file = "data_outputs/prebreeding_pds1_noFCM.RData")
save(prebreeding_nat_mods, file = "data_outputs/prebreeding_nat_mods_noFCM.RData")
save(prebreeding_mod_mods, file = "data_outputs/prebreeding_mod_mods_noFCM.RData")



####################################################################################

#### Bootstrapping ####

####################################################################################
library(MASS)
library(matrixStats)
library(tidyverse)

setwd("D:/Allison/Github_Projects/Ch-2/Ch-2")

my_species <- read.csv("data/bird_data_v4.csv")


natural1 <- c("Evergreen Needleleaf Forests","Evergreen Broadleaf Forests",                
              "Deciduous Needleleaf Forests","Deciduous Broadleaf Forests",                
              "Mixed Broadleaf/Needleleaf Forests",
              "Mixed Broadleaf Evergreen/Deciduous Forests",
              "Open Forests", "Woody Wetlands") 

modified1 <- c(#"Forest/Cropland Mosaics",                    
               "Natural Herbaceous/Croplands Mosaics",       
               "Herbaceous Croplands")#, 
#"Barren" )

#######################################

#### BREEDING bootstrapping ####

#######################################

load("data_outputs/breeding_pds1_noFCM.RData")
breeding_pds1$season <- rep("breeding", length(my_species$species_code))
load("data_outputs/breeding_nat_mods_noFCM.RData")


#for each model (12 for each species in each season)
#1000 bootstrap replicates

species_breeding_nat <- list()

for(b in 1:length(my_species$species_code)){
  
  spec <- my_species$species_code[b]
  
  boot_reps <- list()
  
  #1000 reps of linear model slopes/intercepts accounting for the var/covar
  for(a in 1:length(natural1)){
    
    spec_cov <- breeding_nat_mods[[spec]]
    
    sp_df <- breeding_pds1 %>%
      filter(state == "natural" & species_code == spec)
    
    tryCatch({
      lc_bs <- MASS::mvrnorm(1000, c(sp_df$intercept[a], sp_df$slope[a]), spec_cov[[a]])
      
      lc_bs1 <- as.data.frame(t(lc_bs[,2]))},
      
      error = function(e){ lc_bs1 <- as.data.frame(rep(NA, 1000))
      })
    
    boot_reps[[a]] <- lc_bs1
    
  }
  
  names(boot_reps) <- natural1
  boot_reps_df <- bind_rows(boot_reps, .id = "lc")
  boot_reps_df$state <- rep("natural", length(natural1))
  
  #average slope for natural landcover types for each species
  #for each of the 1000 bootstrap estimates
  #ie gives 1000 averages
  
  df <- boot_reps_df %>% 
    summarise_if(is.numeric, mean, na.rm = T)
  
  species_code <- spec
  state <- "natural"
  season <- "breeding"
  
  df1 <- cbind(species_code, state, season, df)
  
  species_breeding_nat[[b]] <- df1
  
  
}

species_breeding_nat1 <- bind_rows(species_breeding_nat)


#breeding modified

load("data_outputs/breeding_mod_mods_noFCM.RData")

#for each model (12 for each species in each season)
#1000 bootstrap replicates

species_breeding_mod <- list()

for(b in 1:length(my_species$species_code)){
  
  spec <- my_species$species_code[b]
  
  boot_reps <- list()
  
  #1000 reps of linear model slopes/intercepts accounting for the var/covar
  for(a in 1:length(modified1)){
    
    spec_cov <- breeding_mod_mods[[spec]]
    
    sp_df <- breeding_pds1 %>%
      filter(state == "modified" & species_code == spec)
    
    tryCatch({
      lc_bs <- MASS::mvrnorm(1000, c(sp_df$intercept[a], sp_df$slope[a]), spec_cov[[a]])
      
      lc_bs1 <- as.data.frame(t(lc_bs[,2]))},
      
      error = function(e){ lc_bs1 <- as.data.frame(rep(NA, 1000))
      })
    
    boot_reps[[a]] <- lc_bs1
    
  }
  
  names(boot_reps) <- modified1
  boot_reps_df <- bind_rows(boot_reps, .id = "lc")
  boot_reps_df$state <- rep("modified", length(modified1))
  
  #average slope for natural landcover types for each species
  #for each of the 1000 bootstrap estimates
  #ie gives 1000 averages
  
  df <- boot_reps_df %>% 
    summarise_if(is.numeric, mean, na.rm = T)
  
  species_code <- spec
  state <- "modified"
  season <- "breeding"
  
  df1 <- cbind(species_code, state, season, df)
  
  species_breeding_mod[[b]] <- df1
  
  
}

species_breeding_mod1 <- bind_rows(species_breeding_mod)

species_breeding_bs <- bind_rows(species_breeding_nat1, species_breeding_mod1)

save(species_breeding_bs, file = "data_outputs/species_breeding_bootstrap_noFCM.RData")


#######################################

#### POSTBREEDING bootstrapping ####

#######################################

load("data_outputs/postbreeding_pds1_noFCM.RData")
postbreeding_pds1$season <- rep("postbreeding", length(my_species$species_code))
load("data_outputs/postbreeding_nat_mods_noFCM.RData")


#for each model (12 for each species in each season)
#1000 bootstrap replicates

species_postbreeding_nat <- list()

for(b in 1:length(my_species$species_code)){
  
  spec <- my_species$species_code[b]
  
  boot_reps <- list()
  
  #1000 reps of linear model slopes/intercepts accounting for the var/covar
  for(a in 1:length(natural1)){
    
    spec_cov <- postbreeding_nat_mods[[spec]]
    
    sp_df <- postbreeding_pds1 %>%
      filter(state == "natural" & species_code == spec)
    
    tryCatch({
      lc_bs <- MASS::mvrnorm(1000, c(sp_df$intercept[a], sp_df$slope[a]), spec_cov[[a]])
      
      lc_bs1 <- as.data.frame(t(lc_bs[,2]))},
      
      error = function(e){ lc_bs1 <- as.data.frame(rep(NA, 1000))
      })
    
    boot_reps[[a]] <- lc_bs1
    
  }
  
  names(boot_reps) <- natural1
  boot_reps_df <- bind_rows(boot_reps, .id = "lc")
  boot_reps_df$state <- rep("natural", length(natural1))
  
  #average slope for natural landcover types for each species
  #for each of the 1000 bootstrap estimates
  #ie gives 1000 averages
  
  df <- boot_reps_df %>% 
    summarise_if(is.numeric, mean, na.rm = T)
  
  species_code <- spec
  state <- "natural"
  season <- "postbreeding"
  
  df1 <- cbind(species_code, state, season, df)
  
  species_postbreeding_nat[[b]] <- df1
  
  
}

species_postbreeding_nat1 <- bind_rows(species_postbreeding_nat)


#postbreeding modified

load("data_outputs/postbreeding_mod_mods_noFCM .RData")

#for each model (12 for each species in each season)
#1000 bootstrap replicates

species_postbreeding_mod <- list()

for(b in 1:length(my_species$species_code)){
  
  spec <- my_species$species_code[b]
  
  boot_reps <- list()
  
  #1000 reps of linear model slopes/intercepts accounting for the var/covar
  for(a in 1:length(modified1)){
    
    spec_cov <- postbreeding_mod_mods[[spec]]
    
    sp_df <- postbreeding_pds1 %>%
      filter(state == "modified" & species_code == spec)
    
    tryCatch({
      lc_bs <- MASS::mvrnorm(1000, c(sp_df$intercept[a], sp_df$slope[a]), spec_cov[[a]])
      
      lc_bs1 <- as.data.frame(t(lc_bs[,2]))},
      
      error = function(e){ lc_bs1 <- as.data.frame(rep(NA, 1000))
      })
    
    boot_reps[[a]] <- lc_bs1
    
  }
  
  names(boot_reps) <- modified1
  boot_reps_df <- bind_rows(boot_reps, .id = "lc")
  boot_reps_df$state <- rep("modified", length(modified1))
  
  #average slope for natural landcover types for each species
  #for each of the 1000 bootstrap estimates
  #ie gives 1000 averages
  
  df <- boot_reps_df %>% 
    summarise_if(is.numeric, mean, na.rm = T)
  
  species_code <- spec
  state <- "modified"
  season <- "postbreeding"
  
  df1 <- cbind(species_code, state, season, df)
  
  species_postbreeding_mod[[b]] <- df1
  
  
}

species_postbreeding_mod1 <- bind_rows(species_postbreeding_mod)

species_postbreeding_bs <- bind_rows(species_postbreeding_nat1, species_postbreeding_mod1)

save(species_postbreeding_bs, file = "data_outputs/species_postbreeding_bootstrap_noFCM.RData")


#######################################

#### NONBREEDING bootstrapping ####

#######################################

load("data_outputs/nonbreeding_pds1_noFCM.RData")
nonbreeding_pds1$season <- rep("nonbreeding", length(my_species$species_code))
load("data_outputs/nonbreeding_nat_mods_noFCM.RData")


#for each model (12 for each species in each season)
#1000 bootstrap replicates

species_nonbreeding_nat <- list()

for(b in 1:length(my_species$species_code)){
  
  spec <- my_species$species_code[b]
  
  boot_reps <- list()
  
  #1000 reps of linear model slopes/intercepts accounting for the var/covar
  for(a in 1:length(natural1)){
    
    spec_cov <- nonbreeding_nat_mods[[spec]]
    
    sp_df <- nonbreeding_pds1 %>%
      filter(state == "natural" & species_code == spec)
    
    tryCatch({
      lc_bs <- MASS::mvrnorm(1000, c(sp_df$intercept[a], sp_df$slope[a]), spec_cov[[a]])
      
      lc_bs1 <- as.data.frame(t(lc_bs[,2]))},
      
      error = function(e){ lc_bs1 <- as.data.frame(rep(NA, 1000))
      })
    
    boot_reps[[a]] <- lc_bs1
    
  }
  
  names(boot_reps) <- natural1
  boot_reps_df <- bind_rows(boot_reps, .id = "lc")
  boot_reps_df$state <- rep("natural", length(natural1))
  
  #average slope for natural landcover types for each species
  #for each of the 1000 bootstrap estimates
  #ie gives 1000 averages
  
  df <- boot_reps_df %>% 
    summarise_if(is.numeric, mean, na.rm = T)
  
  species_code <- spec
  state <- "natural"
  season <- "nonbreeding"
  
  df1 <- cbind(species_code, state, season, df)
  
  species_nonbreeding_nat[[b]] <- df1
  
  
}

species_nonbreeding_nat1 <- bind_rows(species_nonbreeding_nat)


#nonbreeding modified

load("data_outputs/nonbreeding_mod_mods_noFCM.RData")

#for each model (12 for each species in each season)
#1000 bootstrap replicates

species_nonbreeding_mod <- list()

for(b in 1:length(my_species$species_code)){
  
  spec <- my_species$species_code[b]
  
  boot_reps <- list()
  
  #1000 reps of linear model slopes/intercepts accounting for the var/covar
  for(a in 1:length(modified1)){
    
    spec_cov <- nonbreeding_mod_mods[[spec]]
    
    sp_df <- nonbreeding_pds1 %>%
      filter(state == "modified" & species_code == spec)
    
    tryCatch({
      lc_bs <- MASS::mvrnorm(1000, c(sp_df$intercept[a], sp_df$slope[a]), spec_cov[[a]])
      
      lc_bs1 <- as.data.frame(t(lc_bs[,2]))},
      
      error = function(e){ lc_bs1 <- as.data.frame(rep(NA, 1000))
      })
    
    boot_reps[[a]] <- lc_bs1
    
  }
  
  names(boot_reps) <- modified1
  boot_reps_df <- bind_rows(boot_reps, .id = "lc")
  boot_reps_df$state <- rep("modified", length(modified1))
  
  #average slope for natural landcover types for each species
  #for each of the 1000 bootstrap estimates
  #ie gives 1000 averages
  
  df <- boot_reps_df %>% 
    summarise_if(is.numeric, mean, na.rm = T)
  
  species_code <- spec
  state <- "modified"
  season <- "nonbreeding"
  
  df1 <- cbind(species_code, state, season, df)
  
  species_nonbreeding_mod[[b]] <- df1
  
  
}

species_nonbreeding_mod1 <- bind_rows(species_nonbreeding_mod)

species_nonbreeding_bs <- bind_rows(species_nonbreeding_nat1, species_nonbreeding_mod1)

save(species_nonbreeding_bs, file = "data_outputs/species_nonbreeding_bootstrap_noFCM.RData")




#######################################

#### PREBREEDING bootstrapping ####

#######################################


load("data_outputs/prebreeding_pds1_noFCM.RData")
prebreeding_pds1$season <- rep("prebreeding", length(my_species$species_code))
load("data_outputs/prebreeding_nat_mods_noFCM.RData")

#for each model (12 for each species in each season)
#1000 bootstrap replicates

species_prebreeding_nat <- list()

for(b in 1:length(my_species$species_code)){
  
  spec <- my_species$species_code[b]
  
  boot_reps <- list()
  
  #1000 reps of linear model slopes/intercepts accounting for the var/covar
  for(a in 1:length(natural1)){
    
    spec_cov <- prebreeding_nat_mods[[spec]]
    
    sp_df <- prebreeding_pds1 %>%
      filter(state == "natural" & species_code == spec)
    
    tryCatch({
      lc_bs <- MASS::mvrnorm(1000, c(sp_df$intercept[a], sp_df$slope[a]), spec_cov[[a]])
      
      lc_bs1 <- as.data.frame(t(lc_bs[,2]))},
      
      error = function(e){ lc_bs1 <- as.data.frame(rep(NA, 1000))
      })
    
    boot_reps[[a]] <- lc_bs1
    
  }
  
  names(boot_reps) <- natural1
  boot_reps_df <- bind_rows(boot_reps, .id = "lc")
  boot_reps_df$state <- rep("natural", length(natural1))
  
  #average slope for natural landcover types for each species
  #for each of the 1000 bootstrap estimates
  #ie gives 1000 averages
  
  df <- boot_reps_df %>% 
    summarise_if(is.numeric, mean, na.rm = T)
  
  species_code <- spec
  state <- "natural"
  season <- "prebreeding"
  
  df1 <- cbind(species_code, state, season, df)
  
  species_prebreeding_nat[[b]] <- df1
  
  
}

species_prebreeding_nat1 <- bind_rows(species_prebreeding_nat)


#prebreeding modified

load("data_outputs/prebreeding_mod_mods_noFCM.RData")

#for each model (12 for each species in each season)
#1000 bootstrap replicates

species_prebreeding_mod <- list()

for(b in 1:length(my_species$species_code)){
  
  spec <- my_species$species_code[b]
  
  boot_reps <- list()
  
  #1000 reps of linear model slopes/intercepts accounting for the var/covar
  for(a in 1:length(modified1)){
    
    spec_cov <- prebreeding_mod_mods[[spec]]
    
    sp_df <- prebreeding_pds1 %>%
      filter(state == "modified" & species_code == spec)
    
    tryCatch({
      lc_bs <- MASS::mvrnorm(1000, c(sp_df$intercept[a], sp_df$slope[a]), spec_cov[[a]])
      
      lc_bs1 <- as.data.frame(t(lc_bs[,2]))},
      
      error = function(e){ lc_bs1 <- as.data.frame(rep(NA, 1000))
      })
    
    boot_reps[[a]] <- lc_bs1
    
  }
  
  names(boot_reps) <- modified1
  boot_reps_df <- bind_rows(boot_reps, .id = "lc")
  boot_reps_df$state <- rep("modified", length(modified1))
  
  #average slope for natural landcover types for each species
  #for each of the 1000 bootstrap estimates
  #ie gives 1000 averages
  
  df <- boot_reps_df %>% 
    summarise_if(is.numeric, mean, na.rm = T)
  
  species_code <- spec
  state <- "modified"
  season <- "prebreeding"
  
  df1 <- cbind(species_code, state, season, df)
  
  species_prebreeding_mod[[b]] <- df1
  
  
}

species_prebreeding_mod1 <- bind_rows(species_prebreeding_mod)

species_prebreeding_bs <- bind_rows(species_prebreeding_nat1, species_prebreeding_mod1)

save(species_prebreeding_bs, file = "data_outputs/species_prebreeding_bootstrap_noFCM.RData")



#### combine bootstrapping ####

#################################################################

load("data_outputs/species_breeding_bootstrap_noFCM.RData")
load("data_outputs/species_postbreeding_bootstrap_noFCM.RData")
load("data_outputs/species_nonbreeding_bootstrap_noFCM.RData")
load("data_outputs/species_prebreeding_bootstrap_noFCM.RData")

bootstrap_all <- bind_rows(species_breeding_bs,
                           species_postbreeding_bs,
                           species_nonbreeding_bs,
                           species_prebreeding_bs)

save(bootstrap_all, file = "data_outputs/bootstrap_all_noFCM.RData")

