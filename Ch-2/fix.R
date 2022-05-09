#fix stupid mistake :(
library(ebirdst)
library(tidyverse)
library(raster)

#### breeding ####

ebirdst_species <- ebirdst_runs %>%
  dplyr::select(c("species_code","run_name"))

my_species <- read.csv("C:/Users/AllisonBinley/OneDrive - Carleton University/thesis/CH2_2021/data/bird_data_v4.csv")
my_species <- left_join(my_species, ebirdst_species, by = "species_code")

setwd("E:/eBird/data/raw/STEM")

save(breeding_weights, file = "D:/Allison/Big_data/eBird_outputs/breeding_weights.RData")
load("D:/Allison/Big_data/eBird_outputs/breeding_weights.RData")

#breeding_weights <- list()

for(i in 195:238){
  
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
  
  bre_vals <- getValues(bre_abd_mean)%>%
    na.omit()
  weight_sum <- sum(bre_vals)
  
  breeding_weights[[i]]<- weight_sum
  print(code)
  
}

library(beepr)
beep("mario")


library(ebirdst)
library(tidyverse)
library(raster)

#### postbreeding ####

ebirdst_species <- ebirdst_runs %>%
  dplyr::select(c("species_code","run_name"))

my_species <- read.csv("C:/Users/AllisonBinley/OneDrive - Carleton University/thesis/CH2_2021/data/bird_data_v4.csv")
my_species <- left_join(my_species, ebirdst_species, by = "species_code")

setwd("E:/eBird/data/raw/STEM")

save(postbreeding_weights, file = "D:/Allison/Big_data/eBird_outputs/postbreeding_weights.RData")
load("D:/Allison/Big_data/eBird_outputs/postbreeding_weights.RData")

#postbreeding_weights <- list()

for(i in 171:238){
  
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
  
  #breeding
  postbre_abd <- ebirdst_subset(abd, ext = postbre_extent)
  
  postbre_abd_mean <- calc(postbre_abd, mean)
  
  postbre_vals <- getValues(postbre_abd_mean)%>%
    na.omit()
  weight_sum <- sum(postbre_vals)
  
  postbreeding_weights[[i]]<- weight_sum
  #print(code)
  print(i)
  
}

library(beepr)
beep("mario")


library(ebirdst)
library(tidyverse)
library(raster)

#### nonbreeding ####

ebirdst_species <- ebirdst_runs %>%
  dplyr::select(c("species_code","run_name"))

my_species <- read.csv("C:/Users/AllisonBinley/OneDrive - Carleton University/thesis/CH2_2021/data/bird_data_v4.csv")
my_species <- left_join(my_species, ebirdst_species, by = "species_code")

setwd("E:/eBird/data/raw/STEM")

save(nonbreeding_weights, file = "D:/Allison/Big_data/eBird_outputs/nonbreeding_weights.RData")
load("D:/Allison/Big_data/eBird_outputs/nonbreeding_weights.RData")

#nonbreeding_weights <- list()

for(i in 232:238){
  
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
  
  #breeding
  nonbre_abd <- ebirdst_subset(abd, ext = nonbre_extent)
  
  nonbre_abd_mean <- calc(nonbre_abd, mean)
  
  nonbre_vals <- getValues(nonbre_abd_mean)%>%
    na.omit()
  weight_sum <- sum(nonbre_vals)
  
  nonbreeding_weights[[i]]<- weight_sum
  #print(code)
  print(i)
  
}

library(beepr)
beep("mario")

library(ebirdst)
library(tidyverse)
library(raster)

#### prebreeding ####

ebirdst_species <- ebirdst_runs %>%
  dplyr::select(c("species_code","run_name"))

my_species <- read.csv("C:/Users/AllisonBinley/OneDrive - Carleton University/thesis/CH2_2021/data/bird_data_v4.csv")
my_species <- left_join(my_species, ebirdst_species, by = "species_code")

setwd("E:/eBird/data/raw/STEM")

save(prebreeding_weights, file = "D:/Allison/Big_data/eBird_outputs/prebreeding_weights.RData")
load("D:/Allison/Big_data/eBird_outputs/prebreeding_weights.RData")

#prebreeding_weights <- list()

for(i in 161:238){
  
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
  
  #breeding
  prebre_abd <- ebirdst_subset(abd, ext = prebre_extent)
  
  prebre_abd_mean <- calc(prebre_abd, mean)
  
  prebre_vals <- getValues(prebre_abd_mean)%>%
    na.omit()
  weight_sum <- sum(prebre_vals)
  
  prebreeding_weights[[i]]<- weight_sum
  #print(code)
  print(i)
  
}

library(beepr)
beep("mario")
