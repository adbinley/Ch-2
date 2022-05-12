#partial dependence

library(ebirdst)
library(dplyr)
library(raster)
library(prioritizr)
library(gurobi)
library(tidyverse)
library(here)
library(DBI)
library(ggsci)

#get updated run names if necessary (current run names are for 2019 data, but they change for each release)
ebirdst_species <- ebirdst_runs %>%
  dplyr::select(c("species_code","run_name"))

my_species <- read.csv("data/bird_data_v4.csv")
my_species <- left_join(my_species, ebirdst_species, by = "species_code")

setwd("E:/eBird/data/raw/STEM") #where my eBird data is stored

#MODIS land cover codes for all natural land cover classes in the analysis
natural <- c("mcd12q1_lccs1_fs_c11_1500_pland",
             "mcd12q1_lccs1_fs_c12_1500_pland",
             "mcd12q1_lccs1_fs_c13_1500_pland",
             "mcd12q1_lccs1_fs_c14_1500_pland",
             "mcd12q1_lccs1_fs_c15_1500_pland",
             "mcd12q1_lccs1_fs_c16_1500_pland",
             "mcd12q1_lccs1_fs_c21_1500_pland",
             "mcd12q1_lccs3_fs_c27_1500_pland")

#MODIS land cover codes for all modified (agricultural) land cover classes in the analysis
modified <- c("mcd12q1_lccs2_fs_c25_1500_pland",
              "mcd12q1_lccs2_fs_c35_1500_pland",
              "mcd12q1_lccs2_fs_c36_1500_pland")

#names to go with the codes
natural1 <- c("Evergreen Needleleaf Forests","Evergreen Broadleaf Forests",                
             "Deciduous Needleleaf Forests","Deciduous Broadleaf Forests",                
             "Mixed Broadleaf/Needleleaf Forests",
             "Mixed Broadleaf Evergreen/Deciduous Forests",
             "Open Forests", "Woody Wetlands") 

modified1 <- c("Forest/Cropland Mosaics",                    
              "Natural Herbaceous/Croplands Mosaics",       
              "Herbaceous Croplands" )

###############################################################

                          #### BREEDING ####

###############################################################


breeding_pds <- list()
breeding_nat_mods <- vector(mode = "list", length = length(my_species$species_code))
breeding_mod_mods <- vector(mode = "list", length = length(my_species$species_code))


#this loops through all 238 species, for breeding period only
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
  #shouldn't be a problem so long as you don't use the null pds

}

pds_nat1 <- bind_rows(pds_nat)

  #this loops through all modified pd relationships for each species
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

#reset working directory to R project - need to switch between harddrive because the dataset is huge

#saving the linear model coefficients and variance-covariance matrices, so that we can use these to bootstrap
save(breeding_pds1, 
     file = "data_outputs/breeding_pds1_nobarren.RData")
save(breeding_nat_mods, file = "data_outputs/breeding_nat_mods_nobarren.RData")
save(breeding_mod_mods, file = "data_outputs/breeding_mod_mods_nobarren.RData")


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
  
  #this loops through all modified pd relationships for each species
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

#reset working directory to R project - need to switch between harddrive because the dataset is huge

save(postbreeding_pds1, 
     file = "data_outputs/postbreeding_pds1_nobarren.RData")
save(postbreeding_nat_mods, file = "data_outputs/postbreeding_nat_mods_nobarren.RData")
save(postbreeding_mod_mods, file = "data_outputs/postbreeding_mod_mods_nobarren.RData")


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
  
  #this loops through all modified pd relationships for each species
  pds_imp <- list()
  
  for(k in 1:length(modified)){
    
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

#reset working directory to R project - need to switch between harddrive because the dataset is huge

save(nonbreeding_pds1, 
     file = "data_outputs/nonbreeding_pds1_nobarren.RData")
save(nonbreeding_nat_mods, file = "data_outputs/nonbreeding_nat_mods_nobarren.RData")
save(nonbreeding_mod_mods, file = "data_outputs/nonbreeding_mod_mods_nobarren.RData")



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
  
  #this loops through all modified pd relationships for each species
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

#reset working directory to R project - need to switch between harddrive because the dataset is huge

save(prebreeding_pds1, 
     file = "data_outputs/prebreeding_pds1_nobarren.RData")
save(prebreeding_nat_mods, file = "data_outputs/prebreeding_nat_mods_nobarren.RData")
save(prebreeding_mod_mods, file = "data_outputs/prebreeding_mod_mods_nobarren.RData")



####################################################################################

           #### Bootstrapping ####

####################################################################################
library(MASS)
library(matrixStats)
library(tidyverse)

#reset working directory to R project - need to switch between harddrive because the dataset is huge

my_species <- read.csv("data/bird_data_v4.csv")

natural1 <- c("Evergreen Needleleaf Forests","Evergreen Broadleaf Forests",                
              "Deciduous Needleleaf Forests","Deciduous Broadleaf Forests",                
              "Mixed Broadleaf/Needleleaf Forests",
              "Mixed Broadleaf Evergreen/Deciduous Forests",
              "Open Forests", "Woody Wetlands") 

modified1 <- c("Forest/Cropland Mosaics",                    
               "Natural Herbaceous/Croplands Mosaics",       
               "Herbaceous Croplands")#, 
               #"Barren" )

#######################################

#### BREEDING bootstrapping ####

#######################################

load("data_outputs/breeding_pds1_nobarren.RData")
breeding_pds1$season <- rep("breeding", length(my_species$species_code))
load("data_outputs/breeding_nat_mods_nobarren.RData")

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

load("data_outputs/breeding_mod_mods_nobarren.RData")

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

save(species_breeding_bs, file = "data_outputs/species_breeding_bootstrap_nobarren.RData")


#######################################

#### POSTBREEDING bootstrapping ####

#######################################

load("data_outputs/postbreeding_pds1_nobarren.RData")
postbreeding_pds1$season <- rep("postbreeding", length(my_species$species_code))
load("data_outputs/postbreeding_nat_mods_nobarren.RData")

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

load("data_outputs/postbreeding_mod_mods_nobarren.RData")

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

save(species_postbreeding_bs, file = "data_outputs/species_postbreeding_bootstrap_nobarren.RData")


#######################################

#### NONBREEDING bootstrapping ####

#######################################

load("data_outputs/nonbreeding_pds1_nobarren.RData")
nonbreeding_pds1$season <- rep("nonbreeding", length(my_species$species_code))
load("data_outputs/nonbreeding_nat_mods_nobarren.RData")

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

load("data_outputs/nonbreeding_mod_mods_nobarren.RData")

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

save(species_nonbreeding_bs, file = "data_outputs/species_nonbreeding_bootstrap_nobarren.RData")




#######################################

#### PREBREEDING bootstrapping ####

#######################################

load("data_outputs/prebreeding_pds1_nobarren.RData")
prebreeding_pds1$season <- rep("prebreeding", length(my_species$species_code))
load("data_outputs/prebreeding_nat_mods_nobarren.RData")


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

load("data_outputs/prebreeding_mod_mods_nobarren.RData")

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

save(species_prebreeding_bs, file = "data_outputs/species_prebreeding_bootstrap_nobarren.RData")


#################################################################

#### combine bootstrapping ####

#################################################################

load("data_outputs/species_breeding_bootstrap_nobarren.RData")
load("data_outputs/species_postbreeding_bootstrap_nobarren.RData")
load("data_outputs/species_nonbreeding_bootstrap_nobarren.RData")
load("data_outputs/species_prebreeding_bootstrap_nobarren.RData")

bootstrap_all <- bind_rows(species_breeding_bs,
                           species_postbreeding_bs,
                           species_nonbreeding_bs,
                           species_prebreeding_bs)

save(bootstrap_all, file = "data_outputs/bootstrap_all_nobarren.RData")

