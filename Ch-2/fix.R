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

#reevir is still missing

library(ebirdst)
x <- ebirdst_runs
ebirdst_download("example_data")

species<-"Red-eyed Vireo"
path = rappdirs::user_data_dir("ebirdst")
species <- get_species(species)
which_run <- which(ebirdst::ebirdst_runs$species_code == species)
run <- ebirdst::ebirdst_runs$run_name[which_run]
key<-Sys.getenv("EBIRDST_KEY")
api_url <- "https://st-download.ebird.org/v1/"
list_obj_url <- stringr::str_glue("{api_url}list-obj/{species}?key={key}")
files <-jsonlite::read_json(list_obj_url, simplifyVector = TRUE)
files <- data.frame(file = files)
files <- files[!stringr::str_detect(files$file, "\\.db$"), , drop = FALSE]
files$src_path <- stringr::str_glue("{api_url}fetch?objKey={files$file}",
                                    "&key={key}")
files$dest_path <- file.path(path, files$file)
files$exists <- file.exists(files$dest_path)
dirs <- unique(dirname(files$dest_path))
for (d in dirs) {
  dir.create(d, showWarnings = FALSE, recursive = TRUE)
}

old_timeout <- getOption("timeout")
options(timeout = max(3000, old_timeout))

for (i in seq_len(nrow(files))) {
  dl_response <- utils::download.file(files$src_path[i],
                                      files$dest_path[i],
                                      mode = "wb")
  if (dl_response != 0) {
    stop("Error downloading file: ", files$file[i])
  }
}



revi_bre <- data.frame(species_code="reevir1",
                       sum=exposure_sum,
                       season="breeding",
                       weight_sum=weight_sum,
                       weighted_abd=exp,
                       group="migrant",
                       SW_mig="N",
                       diet2="I",
                       sw_foraging="F")

revi_postbre <- data.frame(species_code="reevir1",
                       sum=exposure_sum,
                       season="postbreeding",
                       weight_sum=weight_sum,
                       weighted_abd=exp,
                       group="migrant",
                       SW_mig="N",
                       diet2="I",
                       sw_foraging="F")

revi_nonbre <- data.frame(species_code="reevir1",
                           sum=exposure_sum,
                           season="nonbreeding",
                           weight_sum=weight_sum,
                           weighted_abd=exp,
                           group="migrant",
                           SW_mig="N",
                           diet2="I",
                           sw_foraging="F")

revi_prebre <- data.frame(species_code="reevir1",
                          sum=exposure_sum,
                          season="prebreeding",
                          weight_sum=weight_sum,
                          weighted_abd=exp,
                          group="migrant",
                          SW_mig="N",
                          diet2="I",
                          sw_foraging="F")


availability_revi_noFCM <- rbind(revi_bre, revi_postbre,revi_nonbre, revi_prebre)

setwd("D:/Allison/Github_Projects/Ch-2/Ch-2")
save(availability, file = "data_outputs/availability_data_nobarren_updated.RData")
save(availability_revi_noFCM, file = "data_outputs/availability_data_noFCM_revi.RData")
load("data_outputs/availability_data_noFCM_revi.RData")
availability <- rbind(availability1,availability_revi_noFCM)
save(availability, file = "data_outputs/availability_data_noFCM_updated.RData")
