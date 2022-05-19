library(ebirdst)
library(tidyverse)
library(raster)

#### Modified Habitat Availability ####

# note that MHA was called exposure in earlier iterations, which is still reflected in the code

#calculate PLAND within 3km square for LCCS2 (modified covers)
r <- raster("D:/Allison/Big_data/Ch-2 landcover/MCD12Q1.006_LC_Prop2.tif") #MODIS modified land cover data 
filter_r <- r %in% c(25,35,36) #modified land cover classes only, no barren
filter_r <- r %in% c(35,36) #modified land cover classes only, no FCM
filter_r1 <- clamp(filter_r,lower = 0.5, useValues = F)
r2 <- mask(r,filter_r1)
unique(r2)#checking this worked

writeRaster(r2, filename = "D:/Allison/Big_data/Ch-2 landcover/modified_cover_only_nobarren.tif")
writeRaster(r2, filename = "D:/Allison/Big_data/Ch-2 landcover/modified_cover_only_noFCM.tif")

mod_pland <- aggregate(r2, fact=6, fun=function(vals, na.rm) {
  sum(vals>0, na.rm=na.rm)/length(vals)
})

#match to ebd abundance raster
#here using the abd map for house wren
#need updated run names for 2019 data
ebirdst_species <- ebirdst_runs %>%
  dplyr::select(c("species_code","run_name"))

#new dataset with resident species, traits - 29.3.2021
my_species <- read.csv("C:/Users/AllisonBinley/OneDrive - Carleton University/thesis/CH2_2021/data/bird_data_v4.csv")
my_species <- left_join(my_species, ebirdst_species, by = "species_code")

run_name <- my_species$run_name.y[62] #house wren
abd <- load_raster(product = "abundance", path = run_name)
mod_pland_ebd <- resample(mod_pland,abd, method="bilinear")
writeRaster(mod_pland_ebd, filename = "D:/Allison/Big_data/Ch-2 landcover/modified_cover_ebd_nobarren.tif")
writeRaster(mod_pland_ebd, filename = "D:/Allison/Big_data/Ch-2 landcover/modified_cover_ebd_noFCM.tif")
#two different rasters, one for main text analysis and one for the supplemental analysis without FCM

#### loops ####
#relative abundance for seasonal windows, multiplied by PLAND
mod_pland_ebd <- raster("D:/Allison/Big_data/Ch-2 landcover/modified_cover_ebd_nobarren.tif")
mod_pland_ebd <- raster("D:/Allison/Big_data/Ch-2 landcover/modified_cover_ebd_noFCM.tif")

#need updated run names
ebirdst_species <- ebirdst_runs %>%
  dplyr::select(c("species_code","run_name"))

my_species <- read.csv("C:/Users/AllisonBinley/OneDrive - Carleton University/thesis/CH2_2021/data/bird_data_v4.csv")
my_species <- left_join(my_species, ebirdst_species, by = "species_code")

setwd("E:/eBird/data/raw/STEM")


#### breeding ####

breeding_exp <- list()

for(i in 1:238){
  
  code <- my_species$species_code[i]
  
  run_name <- my_species$run_name.y[i]
  
  skip_to_next <- FALSE
  
  tryCatch(

    abd <- load_raster(product = "abundance", path = run_name), error = function(e){skip_to_next <<- TRUE})
  
  if(skip_to_next) { next }
  
  start <- my_species %>%
    filter(species_code==code)%>%
    {.[["breeding_start_dt"]]}
  
  end <- my_species %>%
    filter(species_code==code)%>%
    {.[["breeding_end_dt"]]}
  
  bre <- c(start, end)  
  
  ext_vec <-  c(xmin = -12240259, xmax = -1344534, 
            ymin = -6945058, ymax = 9202317)
  
  ext <- sf::st_bbox(ext_vec, crs = "+proj=sinu +lon_0=0 +x_0=0 +y_0=0 +R=6371007.181 +units=m +no_defs")
  
  bre_extent <- ebirdst_extent(ext, 
                               t = bre)
  
  #breeding
  bre_abd <- ebirdst_subset(abd, ext = bre_extent)
  
  bre_abd_mean <- calc(bre_abd, mean)
  
  exposure <- bre_abd_mean*mod_pland_ebd
  exposure_vals <- getValues(exposure)%>%
    na.omit()
  exposure_vals <- exposure_vals[exposure_vals!=0]
  exposure_sum <- sum(exposure_vals)
  
  bre_vals <- getValues(bre_abd_mean)%>%
    na.omit()
  bre_vals <- bre_vals[bre_vals!=0]
  weight_sum <- sum(bre_vals)
  
  exp <- exposure_sum/weight_sum

  breeding_exp[[i]]<- exp
  
}

library(beepr)
beep("mario")

save(breeding_exp, file = "D:/Allison/Big_data/eBird_outputs/breeding_exp_nobarren.RData")
save(breeding_exp, file = "D:/Allison/Big_data/eBird_outputs/breeding_exp_noFCM.RData")


#### postbreeding ####

mod_pland_ebd <- raster("D:/Allison/Big_data/Ch-2 landcover/modified_cover_ebd_nobarren.tif")
mod_pland_ebd <- raster("D:/Allison/Big_data/Ch-2 landcover/modified_cover_ebd_noFCM.tif")

#need updated run names
ebirdst_species <- ebirdst_runs %>%
  dplyr::select(c("species_code","run_name"))

my_species <- read.csv("C:/Users/AllisonBinley/OneDrive - Carleton University/thesis/CH2_2021/data/bird_data_v4.csv")
my_species <- left_join(my_species, ebirdst_species, by = "species_code")

setwd("E:/eBird/data/raw/STEM")

postbreeding_exp <- list()

for(i in 1:238){
  
  code <- my_species$species_code[i]
  
  run_name <- my_species$run_name.y[i]
  
  skip_to_next <- FALSE
  
  tryCatch(
    abd <- load_raster(product = "abundance", path = run_name), error = function(e){skip_to_next <<- TRUE})
  
  if(skip_to_next) { next }
  
  start <- my_species %>%
    filter(species_code==code)%>%
    {.[["postbreeding_migration_start_dt"]]}
  
  end <- my_species %>%
    filter(species_code==code)%>%
    {.[["postbreeding_migration_end_dt"]]}
  
  postbre <- c(start, end)  
  
  ext_vec <-  c(xmin = -12240259, xmax = -1344534, 
                ymin = -6945058, ymax = 9202317)
  
  ext <- sf::st_bbox(ext_vec, crs = "+proj=sinu +lon_0=0 +x_0=0 +y_0=0 +R=6371007.181 +units=m +no_defs")
  
  postbre_extent <- ebirdst_extent(ext, 
                               t = postbre)
  
  #postbreeding
  postbre_abd <- ebirdst_subset(abd, ext = postbre_extent)
  rm(abd)
  
  postbre_abd_mean <- calc(postbre_abd, mean)
  rm(postbre_abd)
  
  exposure <- postbre_abd_mean*mod_pland_ebd
  exposure_vals <- getValues(exposure)%>%
    na.omit()
  exposure_vals <- exposure_vals[exposure_vals!=0]
  exposure_sum <- sum(exposure_vals)
  
  postbre_vals <- getValues(postbre_abd_mean)%>%
    na.omit()
  postbre_vals <- postbre_vals[postbre_vals!=0]
  weight_sum <- sum(postbre_vals)
  
  exp <- exposure_sum/weight_sum
  
  postbreeding_exp[[i]]<- exp
  
}

library(beepr)
beep("mario")

save(postbreeding_exp, file = "D:/Allison/Big_data/eBird_outputs/postbreeding_exp_nobarren.RData")
save(postbreeding_exp, file = "D:/Allison/Big_data/eBird_outputs/postbreeding_exp_noFCM.RData")



#### nonbreeding ####

mod_pland_ebd <- raster("D:/Allison/Big_data/Ch-2 landcover/modified_cover_ebd_nobarren.tif")
mod_pland_ebd <- raster("D:/Allison/Big_data/Ch-2 landcover/modified_cover_ebd_noFCM.tif")

#need updated run names
ebirdst_species <- ebirdst_runs %>%
  dplyr::select(c("species_code","run_name"))

my_species <- read.csv("C:/Users/AllisonBinley/OneDrive - Carleton University/thesis/CH2_2021/data/bird_data_v4.csv")
my_species <- left_join(my_species, ebirdst_species, by = "species_code")

setwd("E:/eBird/data/raw/STEM")


nonbreeding_exp <- list()

for(i in 1:238){

  
  code <- my_species$species_code[i]
  
  run_name <- my_species$run_name.y[i]
  
  skip_to_next <- FALSE
  
  tryCatch(
    abd <- load_raster(product = "abundance", path = run_name), error = function(e){skip_to_next <<- TRUE})
  
  if(skip_to_next) { next }
  
  
  start <- my_species %>%
    filter(species_code==code)%>%
    {.[["nonbreeding_start_dt"]]}
  
  end <- my_species %>%
    filter(species_code==code)%>%
    {.[["nonbreeding_end_dt"]]}
  
  nonbre <- c(start, end)  
  
  ext_vec <-  c(xmin = -12240259, xmax = -1344534, 
                ymin = -6945058, ymax = 9202317)
  
  ext <- sf::st_bbox(ext_vec, crs = "+proj=sinu +lon_0=0 +x_0=0 +y_0=0 +R=6371007.181 +units=m +no_defs")
  
  nonbre_extent <- ebirdst_extent(ext, 
                                   t = nonbre)
  
  #nonbreeding
  nonbre_abd <- ebirdst_subset(abd, ext = nonbre_extent)
  rm(abd)
  
  nonbre_abd_mean <- calc(nonbre_abd, mean)
  rm(nonbre_abd)
  
  exposure <- nonbre_abd_mean*mod_pland_ebd
  exposure_vals <- getValues(exposure)%>%
    na.omit()
  exposure_vals <- exposure_vals[exposure_vals!=0]
  exposure_sum <- sum(exposure_vals)
  
  nonbre_vals <- getValues(nonbre_abd_mean)%>%
    na.omit()
  nonbre_vals <- nonbre_vals[nonbre_vals!=0]
  weight_sum <- sum(nonbre_vals)
  
  exp <- exposure_sum/weight_sum
  
  
  nonbreeding_exp[[i]]<- exp
  
  
}

library(beepr)
beep("mario")

save(nonbreeding_exp, file = "D:/Allison/Big_data/eBird_outputs/nonbreeding_exp_nobarren.RData")
save(nonbreeding_exp, file = "D:/Allison/Big_data/eBird_outputs/nonbreeding_exp_noFCM.RData")



#### prebreeding ####

mod_pland_ebd <- raster("D:/Allison/Big_data/Ch-2 landcover/modified_cover_ebd_nobarren.tif")
mod_pland_ebd <- raster("D:/Allison/Big_data/Ch-2 landcover/modified_cover_ebd_noFCM.tif")

#need updated run names
ebirdst_species <- ebirdst_runs %>%
  dplyr::select(c("species_code","run_name"))

my_species <- read.csv("C:/Users/AllisonBinley/OneDrive - Carleton University/thesis/CH2_2021/data/bird_data_v4.csv")
my_species <- left_join(my_species, ebirdst_species, by = "species_code")

setwd("E:/eBird/data/raw/STEM")


prebreeding_exp <- list()

for(i in 1:238){

  
  code <- my_species$species_code[i]
  
  run_name <- my_species$run_name.y[i]
  
  skip_to_next <- FALSE
  
  tryCatch(

    abd <- load_raster(product = "abundance", path = run_name), error = function(e){skip_to_next <<- TRUE})
  
  if(skip_to_next) { next }
  

  start <- my_species %>%
    filter(species_code==code)%>%
    {.[["prebreeding_migration_start_dt"]]}
  
  end <- my_species %>%
    filter(species_code==code)%>%
    {.[["prebreeding_migration_end_dt"]]}
  
  prebre <- c(start, end)  
  
  ext_vec <-  c(xmin = -12240259, xmax = -1344534, 
                ymin = -6945058, ymax = 9202317)
  
  ext <- sf::st_bbox(ext_vec, crs = "+proj=sinu +lon_0=0 +x_0=0 +y_0=0 +R=6371007.181 +units=m +no_defs")
  
  prebre_extent <- ebirdst_extent(ext, 
                                  t = prebre)
  
  #prebreeding
  prebre_abd <- ebirdst_subset(abd, ext = prebre_extent)
  rm(abd)
  
  prebre_abd_mean <- calc(prebre_abd, mean)
  rm(prebre_abd)
  
  exposure <- prebre_abd_mean*mod_pland_ebd
  exposure_vals <- getValues(exposure)%>%
    na.omit()
  exposure_vals <- exposure_vals[exposure_vals!=0]
  exposure_sum <- sum(exposure_vals)
  
  prebre_vals <- getValues(prebre_abd_mean)%>%
    na.omit()
  prebre_vals <- prebre_vals[prebre_vals!=0]
  weight_sum <- sum(prebre_vals)
  
  exp <- exposure_sum/weight_sum
  
  
  prebreeding_exp[[i]]<- exp
}

library(beepr)
beep("mario")

save(prebreeding_exp, file = "D:/Allison/Big_data/eBird_outputs/prebreeding_exp_nobarren.RData")
save(prebreeding_exp, file = "D:/Allison/Big_data/eBird_outputs/prebreeding_exp_noFCM.RData")


#### all seasons ####

my_species <- read.csv("C:/Users/AllisonBinley/OneDrive - Carleton University/thesis/CH2_2021/data/bird_data_v4.csv")
names <- my_species$species_code


load("D:/Allison/Big_data/eBird_outputs/breeding_exp_nobarren.RData")
load("D:/Allison/Big_data/eBird_outputs/breeding_exp_noFCM.RData")
load("D:/Allison/Big_data/eBird_outputs/postbreeding_exp_nobarren.RData")
load("D:/Allison/Big_data/eBird_outputs/postbreeding_exp_noFCM.RData")
load("D:/Allison/Big_data/eBird_outputs/nonbreeding_exp_nobarren.RData")
load("D:/Allison/Big_data/eBird_outputs/nonbreeding_exp_noFCM.RData")
load("D:/Allison/Big_data/eBird_outputs/prebreeding_exp_nobarren.RData")
load("D:/Allison/Big_data/eBird_outputs/prebreeding_exp_noFCM.RData")

names(breeding_exp) <- names
breeding <- bind_rows(breeding_exp, .id = "species_code")
breeding <- pivot_longer(cols = 1:238, breeding, names_to = "species_code", values_to = "weighted_abd")
breeding$season <- rep("breeding",length(breeding$species_code))

names(postbreeding_exp) <- names
postbreeding <- bind_rows(postbreeding_exp, .id = "species_code")
postbreeding <- pivot_longer(cols = 1:238, postbreeding, names_to = "species_code", values_to = "weighted_abd")
postbreeding$season <- rep("postbreeding",length(postbreeding$species_code))

names(nonbreeding_exp) <- names
nonbreeding <- bind_rows(nonbreeding_exp, .id = "species_code")
nonbreeding <- pivot_longer(cols = 1:238, nonbreeding, names_to = "species_code", values_to = "weighted_abd")
nonbreeding$season <- rep("nonbreeding",length(nonbreeding$species_code))

names(prebreeding_exp) <- names
prebreeding <- bind_rows(prebreeding_exp, .id = "species_code")
prebreeding <- pivot_longer(cols = 1:238, prebreeding, names_to = "species_code", values_to = "weighted_abd")
prebreeding$season <- rep("prebreeding",length(prebreeding$species_code))

availability <- rbind(breeding,postbreeding,nonbreeding,prebreeding)
load("data/species_basic.RData")

availability1 <- left_join(availability,species_data)
save(availability1, file = "data_outputs/availability_data_nobarren_updated.RData")
save(availability1, file = "data_outputs/availability_data_noFCM_updated.RData")




