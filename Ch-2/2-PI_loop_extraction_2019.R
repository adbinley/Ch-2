#PI loop extraction 2019 data

library(tidyverse)
library(ebirdst)


setwd("E:/eBird/data/raw/STEM")

#need updated run names for 2019 data
ebirdst_species <- ebirdst_runs %>%
  select(c("species_code","run_name"))

my_species <- read.csv("data/bird_data_v4.csv")
my_species <- left_join(my_species, ebirdst_species, by = "species_code")

species_codes <- my_species$species_code

########################################################################################

###BREEDING###

########################################################################################

breeding_pis <- list()

for(i in 1:length(my_species1$species_code)){
  
  code <- species_codes[i]
  
  run_name <- my_species1$run_name.y[i]
  
  skip_to_next <- FALSE
  
  tryCatch(
    pis <- load_pis(run_name), error = function(e){skip_to_next <<- TRUE})
  
  if(skip_to_next) { next }
  
  start <- my_species1 %>%
    filter(species_code==code)%>%
    {.[["breeding_start_dt"]]}
  
  end <- my_species1 %>%
    filter(species_code==code)%>%
    {.[["breeding_end_dt"]]}
  
  bre <- c(start, end)  
  
  ext <-  c(xmin = -180, xmax = 180, 
            ymin = -90, ymax = 90)
  
  bre_extent <- ebirdst_extent(ext, t = bre)
  
  #breeding
  bre_pis <- ebirdst_subset(pis, ext = bre_extent)
  
  breeding_pis[[i]] <- bre_pis
  
}

names(breeding_pis) <- species_codes

breeding_pis1 <- bind_rows(breeding_pis, .id = "species_code")

these_predictors <- ebirdst::ebirdst_predictors

tidy <- c("species_code", "stixel_id", "lat","lon","date", these_predictors$predictor_label)

breeding_pis2 <- breeding_pis1[, tidy]
colnames(breeding_pis1) <- tidy

breeding_pis3 <- breeding_pis2#q

#filter to land cover only
drops <- c("Is Stationary", "Score", "Year", "Day", "Solar Noon Diff", "Effort Hours", "Effort Distance (km)", "Number Observers", "Eastness Median", "Eastness SD", "Elevation Median", "Elevation SD", "Island", "Northness Median", "Northness SD", "Nighttime Lights")
pis<-breeding_pis3 %>%
  select(!drops)


#calculate rPI from PI - after removing other predictors
library(plyr)

pis2 <- pis[,6:51] 
pis3 <- colwise(as.numeric)(pis2)

pis3 <- pis3 %>%
  select(contains(" PLAND"))

rpis <- apply(pis3, 1, function(pis3) pis3 / sum(pis3, na.rm = TRUE)) %>%
  t() %>%
  as.data.frame(stringsAsFactors = FALSE)

#reattach non-lc columns

rpis_id <- cbind(pis[,1:5], rpis)

rpis_id_breeding <- rpis_id

#save(rpis_id_breeding, file = "data_outputs/rpis_breeding_2019.RData")
load("data_outputs/rpis_breeding_2019.RData")

#now need to specify which landcover classes are "natural"
#and which are "modified"
colnames(rpis_id_breeding)

natural <- c("Evergreen Needleleaf Forests PLAND","Evergreen Broadleaf Forests PLAND",                
             "Deciduous Needleleaf Forests PLAND","Deciduous Broadleaf Forests PLAND",                
             "Mixed Broadleaf/Needleleaf Forests PLAND",
             "Mixed Broadleaf Evergreen/Deciduous Forests PLAND",
             "Open Forests PLAND", "Woody Wetlands PLAND") 


modified <- c("Forest/Cropland Mosaics PLAND",                    
              "Natural Herbaceous/Croplands Mosaics PLAND",       
              "Herbaceous Croplands PLAND")


rpis_cover <- rpis_id_breeding %>%
  mutate(natural = rowSums(select(., all_of(natural))),
         modified = rowSums(select(., all_of(modified))))

#rescale for only modified vs natural
#this is not the most seamless coding but parts of the analysis changed over time
rpis_cover <- rpis_cover %>%
  mutate(modified = modified/(modified+natural),
         forest = 1-modified)

breeding <- rpis_cover %>%
  group_by(species_code)%>%
  dplyr::summarize(modified_rPI = mean(modified, na.rm = TRUE),
                   modrPI_var = var(modified, na.rm = TRUE),
                   natural_rPI = mean(forest, na.rm = TRUE),
                   natrPI_var = var(forest, na.rm = TRUE))

load("data/species_basic.RData")

breeding_rpi_data <- inner_join(breeding, species_basic, by = "species_code")
#save(breeding_rpi_data, file = "data_outputs/IHM_breeding_rpi_data_2019.RData")
save(breeding_rpi_data, file = "data_outputs/breeding_rpi_data_2019.RData") #now without barren

########################################################################################

###POST BREEDING###

########################################################################################

postbreeding_pis <- list()

for(i in 1:length(my_species$species_code)){
  
  code <- species_codes[i]
  
  run_name <- my_species$run_name.y[i]
  
  skip_to_next <- FALSE
  
  tryCatch(
    pis <- load_pis(run_name), error = function(e){skip_to_next <<- TRUE})
  
  if(skip_to_next) { next }
  
  start <- my_species %>%
    filter(species_code==code)%>%
    {.[["postbreeding_migration_start_dt"]]} #note that it isn't actually migration if resident species, but the column is called this from the ebird data
  
  end <- my_species %>%
    filter(species_code==code)%>%
    {.[["postbreeding_migration_end_dt"]]}
  
  posbre <- c(start, end)  
  
  ext <-  c(xmin = -180, xmax = 180, 
            ymin = -90, ymax = 90)
  
  posbre_extent <- ebirdst_extent(ext, t = posbre)
  
  #breeding
  posbre_pis <- ebirdst_subset(pis, ext = posbre_extent)
  
  postbreeding_pis[[i]] <- posbre_pis
  
}

names(postbreeding_pis) <- species_codes

postbreeding_pis1 <- bind_rows(postbreeding_pis, .id = "species_code")

these_predictors <- ebirdst::ebirdst_predictors

tidy <- c("species_code", "stixel_id", "lat","lon","date", these_predictors$predictor_label)
colnames(postbreeding_pis1) <- tidy
postbreeding_pis2 <- postbreeding_pis1[, tidy]


postbreeding_pis3 <- postbreeding_pis2

#filter to land cover only
drops <- c("Is Stationary", "Score", "Year", "Day", "Solar Noon Diff", "Effort Hours", "Effort Distance (km)", "Number Observers", "Eastness Median", "Eastness SD", "Elevation Median", "Elevation SD", "Island", "Northness Median", "Northness SD", "Nighttime Lights")
pis<-postbreeding_pis3 %>%
  select(!drops)


#calculate rPI from PI - after removing other predictors
library(plyr)

pis2 <- pis[,6:51] 
pis3 <- colwise(as.numeric)(pis2)

pis3 <- pis3 %>%
  select(contains(" PLAND"))

rpis <- apply(pis3, 1, function(pis3) pis3 / sum(pis3, na.rm = TRUE)) %>%
  t() %>%
  as.data.frame(stringsAsFactors = FALSE)

#reattach non-lc columns

rpis_id <- cbind(pis[,1:5], rpis)

rpis_id_postbreeding <- rpis_id

#save(rpis_id_postbreeding, file = "data_outputs/rpis_postbreeding_2019.RData")
load("data_outputs/rpis_postbreeding_2019.RData")

#now need to decide which landcover classes are "natural"
#and which are "modified"
colnames(rpis_id_postbreeding)

natural <- c("Evergreen Needleleaf Forests PLAND","Evergreen Broadleaf Forests PLAND",                
             "Deciduous Needleleaf Forests PLAND","Deciduous Broadleaf Forests PLAND",                
             "Mixed Broadleaf/Needleleaf Forests PLAND",
             "Mixed Broadleaf Evergreen/Deciduous Forests PLAND",
             "Open Forests PLAND", "Woody Wetlands PLAND") 


modified <- c("Forest/Cropland Mosaics PLAND",                    
              "Natural Herbaceous/Croplands Mosaics PLAND",       
              "Herbaceous Croplands PLAND" )

rpis_cover <- rpis_id_postbreeding %>%
  mutate(natural = rowSums(select(., all_of(natural))),
         modified = rowSums(select(., all_of(modified))))

#rescale for only modified vs natural
rpis_cover <- rpis_cover %>%
  mutate(modified = modified/(modified+natural),
         forest = 1-modified)

postbreeding <- rpis_cover %>%
  group_by(species_code)%>%
  dplyr::summarize(modified_rPI = mean(modified, na.rm = TRUE),
                   modrPI_var = var(modified, na.rm = TRUE),
                   natural_rPI = mean(forest, na.rm = TRUE),
                   natrPI_var = var(forest, na.rm = TRUE))

load("data/species_basic.RData")

postbreeding_rpi_data <- inner_join(postbreeding, species_basic, by = "species_code")
#save(postbreeding_rpi_data, file = "data_outputs/IHM_postbreeding_rpi_data_2019.RData")
save(postbreeding_rpi_data, file = "data_outputs/postbreeding_rpi_data_2019.RData")#now without barren

########################################################################################

###NONBREEDING###

########################################################################################

nonbreeding_pis <- list()

for(i in 1:length(my_species$species_code)){
  
  code <- species_codes[i]
  
  run_name <- my_species$run_name.y[i]
  
  skip_to_next <- FALSE
  
  tryCatch(
    pis <- load_pis(run_name), error = function(e){skip_to_next <<- TRUE})
  
  if(skip_to_next) { next }
  
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
  
  #breeding
  nonbre_pis <- ebirdst_subset(pis, ext = nonbre_extent)
  
  nonbreeding_pis[[i]] <- nonbre_pis
  
}

names(nonbreeding_pis) <- species_codes

nonbreeding_pis1 <- bind_rows(nonbreeding_pis, .id = "species_code")

these_predictors <- ebirdst::ebirdst_predictors

tidy <- c("species_code", "stixel_id", "lat","lon","date", these_predictors$predictor_label)
colnames(nonbreeding_pis1) <- tidy
nonbreeding_pis2 <- nonbreeding_pis1[, tidy]


nonbreeding_pis3 <- nonbreeding_pis2

#filter to land cover only
drops <- c("Is Stationary", "Score", "Year", "Day", "Solar Noon Diff", "Effort Hours", "Effort Distance (km)", "Number Observers", "Eastness Median", "Eastness SD", "Elevation Median", "Elevation SD", "Island", "Northness Median", "Northness SD", "Nighttime Lights")
pis<-nonbreeding_pis3 %>%
  select(!drops)


#calculate rPI from PI - after removing other predictors
library(plyr)

pis2 <- pis[,6:51] 
pis3 <- colwise(as.numeric)(pis2)

pis3 <- pis3 %>%
  select(contains(" PLAND"))

rpis <- apply(pis3, 1, function(pis3) pis3 / sum(pis3, na.rm = TRUE)) %>%
  t() %>%
  as.data.frame(stringsAsFactors = FALSE)

#reattach non-lc columns

rpis_id <- cbind(pis[,1:5], rpis)

rpis_id_nonbreeding <- rpis_id

#save(rpis_id_nonbreeding, file = "data_outputs/rpis_nonbreeding_2019.RData")
load("data_outputs/rpis_nonbreeding_2019.RData")

#now need to specifify which landcover classes are "natural"
#and which are "modified"

natural <- c("Evergreen Needleleaf Forests PLAND","Evergreen Broadleaf Forests PLAND",                
             "Deciduous Needleleaf Forests PLAND","Deciduous Broadleaf Forests PLAND",                
             "Mixed Broadleaf/Needleleaf Forests PLAND",
             "Mixed Broadleaf Evergreen/Deciduous Forests PLAND",
             "Open Forests PLAND", "Woody Wetlands PLAND") 


modified <- c("Forest/Cropland Mosaics PLAND",                    
              "Natural Herbaceous/Croplands Mosaics PLAND",       
              "Herbaceous Croplands PLAND")


rpis_cover <- rpis_id_nonbreeding %>%
  mutate(natural = rowSums(select(., all_of(natural))),
         modified = rowSums(select(., all_of(modified))))

#rescale for only modified vs natural
rpis_cover <- rpis_cover %>%
  mutate(modified = modified/(modified+natural),
         forest = 1-modified)

nonbreeding <- rpis_cover %>%
  group_by(species_code)%>%
  dplyr::summarize(modified_rPI = mean(modified, na.rm = TRUE),
                   modrPI_var = var(modified, na.rm = TRUE),
                   natural_rPI = mean(forest, na.rm = TRUE),
                   natrPI_var = var(forest, na.rm = TRUE))


load("data/species_basic.RData")

nonbreeding_rpi_data <- inner_join(nonbreeding, species_basic, by = "species_code")
#save(nonbreeding_rpi_data, file = "data_outputs/IHM_nonbreeding_rpi_data_2019.RData")
save(nonbreeding_rpi_data, file = "data_outputs/nonbreeding_rpi_data_2019.RData")#now without barren

########################################################################################

###### PRE BREEDING

########################################################################################

prebreeding_pis <- list()

for(i in 1:length(my_species$species_code)){
  
  code <- species_codes[i]
  
  run_name <- my_species$run_name.y[i]
  
  skip_to_next <- FALSE
  
  tryCatch(
    pis <- load_pis(run_name), error = function(e){skip_to_next <<- TRUE})
  
  if(skip_to_next) { next }
  
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
  
  #breeding
  prebre_pis <- ebirdst_subset(pis, ext = prebre_extent)
  
  prebreeding_pis[[i]] <- prebre_pis
  
}

names(prebreeding_pis) <- species_codes

prebreeding_pis1 <- bind_rows(prebreeding_pis, .id = "species_code")

these_predictors <- ebirdst::ebirdst_predictors

tidy <- c("species_code", "stixel_id", "lat","lon","date", these_predictors$predictor_label)
colnames(prebreeding_pis1) <- tidy
prebreeding_pis2 <- prebreeding_pis1[, tidy]


prebreeding_pis3 <- prebreeding_pis2

#filter to land cover only
drops <- c("Is Stationary", "Score", "Year", "Day", "Solar Noon Diff", "Effort Hours", "Effort Distance (km)", "Number Observers", "Eastness Median", "Eastness SD", "Elevation Median", "Elevation SD", "Island", "Northness Median", "Northness SD", "Nighttime Lights")
pis<-prebreeding_pis3 %>%
  select(!drops)


#calculate rPI from PI - after removing other predictors
library(plyr)

pis2 <- pis[,6:51] #pis[,4:29]
pis3 <- colwise(as.numeric)(pis2)

pis3 <- pis3 %>%
  select(contains(" PLAND"))

rpis <- apply(pis3, 1, function(pis3) pis3 / sum(pis3, na.rm = TRUE)) %>%
  t() %>%
  as.data.frame(stringsAsFactors = FALSE)

#reattach non-lc columns

rpis_id <- cbind(pis[,1:5], rpis)

rpis_id_prebreeding <- rpis_id

#save(rpis_id_prebreeding, file = "data_outputs/rpis_prebreeding_2019.RData")
load("data_outputs/rpis_prebreeding_2019.RData")

#now need to specify which landcover classes are "natural"
#and which are "modified"

natural <- c("Evergreen Needleleaf Forests PLAND","Evergreen Broadleaf Forests PLAND",                
             "Deciduous Needleleaf Forests PLAND","Deciduous Broadleaf Forests PLAND",                
             "Mixed Broadleaf/Needleleaf Forests PLAND",
             "Mixed Broadleaf Evergreen/Deciduous Forests PLAND",
             "Open Forests PLAND", "Woody Wetlands PLAND") 


modified <- c("Forest/Cropland Mosaics PLAND",                    
              "Natural Herbaceous/Croplands Mosaics PLAND",       
              "Herbaceous Croplands PLAND" )


rpis_cover <- rpis_id_prebreeding %>%
  mutate(natural = rowSums(select(., all_of(natural))),
         modified = rowSums(select(., all_of(modified))))

#rescale for only modified vs natural
rpis_cover <- rpis_cover %>%
  mutate(modified = modified/(modified+natural),
         forest = 1-modified)

prebreeding <- rpis_cover %>%
  group_by(species_code)%>%
  dplyr::summarize(modified_rPI = mean(modified, na.rm = TRUE),
                   modrPI_var = var(modified, na.rm = TRUE),
                   natural_rPI = mean(forest, na.rm = TRUE),
                   natrPI_var = var(forest, na.rm = TRUE))

load("data/species_basic.RData")

prebreeding_rpi_data <- inner_join(prebreeding, species_basic, by = "species_code")
#save(prebreeding_rpi_data, file = "data_outputs/IHM_prebreeding_rpi_data_2019.RData")
save(prebreeding_rpi_data, file = "data_outputs/prebreeding_rpi_data_2019.RData")

#create migrants data - note this is actually migrants and residents (analysis of these groups was originally separated)
load("data_outputs/breeding_rpi_data_2019.RData")
breeding_rpi_data$season <- rep("breeding", length(breeding_rpi_data$species_code))
load("data_outputs/postbreeding_rpi_data_2019.RData")
postbreeding_rpi_data$season <- rep("postbreeding", length(postbreeding_rpi_data$species_code))
load("data_outputs/nonbreeding_rpi_data_2019.RData")
nonbreeding_rpi_data$season <- rep("nonbreeding", length(nonbreeding_rpi_data$species_code))
load("data_outputs/prebreeding_rpi_data_2019.RData")
prebreeding_rpi_data$season <- rep("prebreeding", length(prebreeding_rpi_data$species_code))

migrants_2019 <- rbind(breeding_rpi_data,postbreeding_rpi_data, nonbreeding_rpi_data,prebreeding_rpi_data)
migrants_2019$season <- factor(migrants_2019$season, levels=c("breeding", "postbreeding","nonbreeding", "prebreeding"))
save(migrants_2019, file = "data_outputs/rPI_migrants_2019.RData")#now without barren

