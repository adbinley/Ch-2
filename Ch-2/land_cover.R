#land cover - best practices
#everything broken
library(ebirdst)
library(tidyverse)
library(raster)

mod_pland_ebd <- raster("D:/Allison/Big_data/Ch-2 landcover/modified_cover_ebd_nobarren.tif")

png("fig_outputs/mod_pland.png", height = 9, width = 8, units = "in",res=300)
plot(mod_pland_ebd)
dev.off()

x <- raster("E:/eBird/data/raw/STEM/bkcchi-ERD2019-STATUS-20200929-9269169c/abundance_seasonal/bkcchi-ERD2019-STATUS-20200929-9269169c_lr_2019_abundance-seasonal_resident.tif")

abd <- as(x, "SpatialPixelsDataFrame")
abd_df <- as.data.frame(abd)
colnames(abd_df) <- c("value", "x", "y")

library(viridis)

png("fig_outputs/rel_abd.png", height = 9, width = 8, units = "in",res=300)
ggplot() +  
  geom_tile(data=abd_df, aes(x=x, y=y, fill=value), alpha=0.8) + 
  scale_fill_gradient2() +
  coord_equal() +
  theme_classic()
dev.off()

exposure1 <- as(mod_pland_ebd, "SpatialPixelsDataFrame")
exposure2 <- as.data.frame(exposure1)
colnames(exposure2) <- c("value", "x", "y")

png("fig_outputs/exposure.png", height = 9, width = 8, units = "in",res=300)
ggplot() +  
  geom_tile(data=exposure2, aes(x=x, y=y, fill=value), alpha=0.8) + 
  scale_fill_viridis() +
  coord_equal() +
  theme_classic()
dev.off()


preds <- ebirdst::ebirdst_predictors

#downloaded manually

#need land cover class names
library(raster)
library(sf)

x <- raster("E:/eBird/data/raw/STEM/bkpwar-ERD2019-STATUS-20201002-1c7c7cf3/srd_raster_template.tif")
xb <- st_make_grid(x,n=1)
plot(xb)
plot(x, add=T)

coords = matrix(c( -170,-60, #xmin, ymin
                   -170, 80, #xmin, ymax
                   -30, 80, #xmax, ymax
                   -30, -60, #xmax, ymin
                   -170, -60), #xmin, ymin
                ncol = 2, byrow = TRUE)

coords = matrix(c( -12015109,-6673060, #xmin, ymin
                   -12015109, 10007555, #xmin, ymax
                   0, 10007555, #xmax, ymax
                   0, -6673060, #xmax, ymin
                   -12015109, -6673060), #xmin, ymin
                ncol = 2, byrow = TRUE)

#xmin = -20015109
#xmax = 20015371
#ymin = -6673060
#ymax = 10007555

P1 = Polygon(coords)
Ps1 = SpatialPolygons(list(Polygons(list(P1), ID = "a")), proj4string=CRS("+proj=sinu +lon_0=0 +x_0=0 +y_0=0 +R=6371007.181 +units=m +no_defs"))
plot(Ps1, axes = TRUE)
plot(x, add=T)
plot(q, add=T)

#save(Ps1, file = "WestHem.shp")
#head(P1@data)
#install.packages("geojsonio")
library(geojsonio)
Ps1_json <- geojson_json(Ps1)
geojson_write(Ps1_json, file = "WestHem1.geojson")


### working with tifs ####

q <- raster("D:/Allison/Big_data/Ch-2 landcover/MCD12Q1.006_LC_Prop1_lambert.tif")
r <- raster("D:/Allison/Big_data/Ch-2 landcover/MCD12Q1.006_LC_Prop2_lambert.tif")
s <- raster("D:/Allison/Big_data/Ch-2 landcover/MCD12Q1.006_LC_Prop3_lambert.tif")

#plot(q, legend = FALSE, col = rev(terrain.colors(12)))
#legend("topright", legend = c("category 1", "category 2", "category 3", "category 4"), fill = rev(terrain.colors(4)))

#natural <- c("Evergreen Needleleaf Forests","Evergreen Broadleaf Forests","Deciduous Needleleaf Forests","Deciduous Broadleaf Forests",
#             "Mixed Broadleaf/Needleleaf Forests","Mixed Broadleaf Evergreen/Deciduous Forests","Open Forests")

nat <- c(11:16,20,21,27)
mod <- c(25,35,36,1)
lc <- c(nat,mod)

x <- raster("E:/eBird/data/raw/STEM/bkpwar-ERD2019-STATUS-20201002-1c7c7cf3/srd_raster_template.tif")
x1 <- projectRaster(x,q2)
writeRaster(x1, filename = "D:/Allison/Big_data/Ch-2 landcover/base_map_lambert.tif")

#LCCS1
filter_q <- q %in% lc
filter_q1 <- clamp(filter_q,lower = 0.5, useValues = F)
q2 <- mask(q,filter_q1)
writeRaster(q2, filename = "D:/Allison/Big_data/Ch-2 landcover/LCCS1_filtered.tif")
q2 <- raster("D:/Allison/Big_data/Ch-2 landcover/LCCS1_filtered.tif")
#rat <- levels(q2)[[1]]
#rat[["landcover"]] <- c("Barren","Evergreen Needleleaf Forests", "Evergreen Broadleaf Forests",
#                        "Deciduous Needleleaf Forests","Deciduous Broadleaf Forests","Mixed Broadleaf/Needleleaf Forests",
 #                       "Mixed Broadleaf Evergreen/Deciduous Forests","Open Forests")
#levels(q2) <- rat

plot(x1, legend=F, col = "black")
my_col = rev(terrain.colors(n = 8))
my_col1 <- c("#969595",my_col[2:8])
plot(q2, legend = FALSE, col = my_col1, add=T)
legend(x='bottomleft', legend = c("Barren","Evergreen Needleleaf Forests", "Evergreen Broadleaf Forests",
                                  "Deciduous Needleleaf Forests","Deciduous Broadleaf Forests","Mixed Broadleaf/Needleleaf Forests",
                                  "Mixed Broadleaf Evergreen/Deciduous Forests","Open Forests"), 
       fill = my_col1)

png("fig_outputs/LCCS1.png", height = 9, width = 11.5, units = "in",res=300)
plot(x1, legend=F, col = "black")
plot(q2, legend = FALSE, col = my_col1, add=T)
dev.off()
#q3 <- raster("D:/Allison/Big_data/Ch-2 landcover/LCCS1_filtered.tif")

#LCCS2
r <- raster("D:/Allison/Big_data/Ch-2 landcover/MCD12Q1.006_LC_Prop2_lambert.tif")
filter_r <- r %in% lc
filter_r1 <- clamp(filter_r,lower = 0.5, useValues = F)
r2 <- mask(r,filter_r1)
unique(r2)
writeRaster(r2, filename = "D:/Allison/Big_data/Ch-2 landcover/LCCS2_filtered.tif")

x1 <- raster("D:/Allison/Big_data/Ch-2 landcover/base_map_lambert.tif")

plot(x1, legend=F, col = "black")
my_col = rev(terrain.colors(n = 5))
my_col1 <- c("#969595",my_col[2:5])
plot(r2, legend = FALSE, col = my_col1, add=T)
legend(x='bottomleft', legend = c("Barren", "Open Forests", "Forest/Cropland Mosaics",
                                  "Natural Herbaceous/Croplands Mosaics","Herbaceous Croplands"), 
       fill = my_col1)

png("fig_outputs/LCCS2.png", height = 9, width = 11.5, units = "in",res=300)
plot(x1, legend=F, col = "black")
plot(r2, legend = FALSE, col = my_col1, add=T)
dev.off()

#LCCS3
s <- raster("D:/Allison/Big_data/Ch-2 landcover/MCD12Q1.006_LC_Prop3_lambert.tif")
filter_s <- s %in% lc
filter_s1 <- clamp(filter_s,lower = 0.5, useValues = F)
s2 <- mask(s,filter_s1)
unique(s2)
writeRaster(s2, filename = "D:/Allison/Big_data/Ch-2 landcover/LCCS3_filtered.tif")

x1 <- raster("D:/Allison/Big_data/Ch-2 landcover/base_map_lambert.tif")

plot(x1, legend=F, col = "black")
my_col = rev(terrain.colors(n = 3))
my_col1 <- c("#969595",my_col[2:3])
plot(s2, legend = FALSE, col = my_col1, add=T)
legend(x='bottomleft', legend = c("Barren", "Open Forests", "Woody Wetlands"), 
       fill = my_col1)

png("fig_outputs/LCCS3.png", height = 9, width = 11.5, units = "in",res=300)
plot(x1, legend=F, col = "black")
plot(s2, legend = FALSE, col = my_col1, add=T)
dev.off()


#### exposure ####

my_species <- read.csv("C:/Users/AllisonBinley/OneDrive - Carleton University/thesis/CH2_2021/data/bird_data_v4.csv")

x <- raster("E:/eBird/data/raw/STEM/bkpwar-ERD2019-STATUS-20201002-1c7c7cf3/srd_raster_template.tif")

#calculate PLAND within 3km square for LCCS2 (modified covers)
r <- raster("D:/Allison/Big_data/Ch-2 landcover/MCD12Q1.006_LC_Prop2.tif")
#filter_r <- r %in% c(1,25,35,36) #modified land cover classes only
filter_r <- r %in% c(25,35,36) #modified land cover classes only, no barren
filter_r <- r %in% c(35,36) #modified land cover classes only, no FCM
filter_r1 <- clamp(filter_r,lower = 0.5, useValues = F)
r2 <- mask(r,filter_r1)
unique(r2)
#writeRaster(r2, filename = "D:/Allison/Big_data/Ch-2 landcover/modified_cover_only.tif")
#writeRaster(r2, filename = "D:/Allison/Big_data/Ch-2 landcover/modified_cover_only_nobarren.tif")
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

run_name <- my_species$run_name.y[62]
abd <- load_raster(product = "abundance", path = run_name)
mod_pland_ebd <- resample(mod_pland,abd, method="bilinear")
writeRaster(mod_pland_ebd, filename = "D:/Allison/Big_data/Ch-2 landcover/modified_cover_ebd_nobarren.tif")
writeRaster(mod_pland_ebd, filename = "D:/Allison/Big_data/Ch-2 landcover/modified_cover_ebd_noFCM.tif")


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

#breeding_rabd <- list()
#save(breeding_exp, file = "D:/Allison/Big_data/eBird_outputs/breeding_exp.RData")
#save(breeding_exp, file = "D:/Allison/Big_data/eBird_outputs/breeding_exp_nobarren.RData")
save(breeding_exp, file = "D:/Allison/Big_data/eBird_outputs/breeding_exp_noFCM.RData")
#load("D:/Allison/Big_data/eBird_outputs/breeding_exp.RData")
#load("D:/Allison/Big_data/eBird_outputs/breeding_exp_nobarren.RData")
load("D:/Allison/Big_data/eBird_outputs/breeding_exp_noFCM.RData")



breeding_exp <- list()

for(i in 198:238){
#for(i in 169:length(my_species$species_code)){
#for(i in 1:3){
  
  #library(ebirdst)
  
  code <- my_species$species_code[i]
  
  run_name <- my_species$run_name.y[i]
  
  skip_to_next <- FALSE
  
  tryCatch(
  #  pis <- load_pis(run_name), error = function(e){skip_to_next <<- TRUE})
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
  rm(exposure)
  #detach(package:ebirdst)
  
}

library(beepr)
beep("mario")

#average for each species across breeding range
names(breeding_exp) <- my_species$species_code

func <- function(x) {
  c(mean = mean(x), var = var(x))
}

test <- lapply(breeding_exp, func)
test1 <- bind_rows(test, .id = "species")


#### postbreeding ####

#relative abundance for seasonal windows, multiplied by PLAND
#mod_pland_ebd <- raster("D:/Allison/Big_data/Ch-2 landcover/modified_cover_ebd_nobarren.tif")
mod_pland_ebd <- raster("D:/Allison/Big_data/Ch-2 landcover/modified_cover_ebd_noFCM.tif")

#need updated run names
ebirdst_species <- ebirdst_runs %>%
  dplyr::select(c("species_code","run_name"))

my_species <- read.csv("C:/Users/AllisonBinley/OneDrive - Carleton University/thesis/CH2_2021/data/bird_data_v4.csv")
my_species <- left_join(my_species, ebirdst_species, by = "species_code")

setwd("E:/eBird/data/raw/STEM")


#breeding_rabd <- list()
#save(postbreeding_exp, file = "D:/Allison/Big_data/eBird_outputs/postbreeding_exp.RData")
#save(postbreeding_exp, file = "D:/Allison/Big_data/eBird_outputs/postbreeding_exp_nobarren.RData")
#load("D:/Allison/Big_data/eBird_outputs/postbreeding_exp_nobarren.RData")
save(postbreeding_exp, file = "D:/Allison/Big_data/eBird_outputs/postbreeding_exp_noFCM.RData")
load("D:/Allison/Big_data/eBird_outputs/postbreeding_exp_noFCM.RData")

postbreeding_exp <- list()

for(i in 146:238){
  #for(i in 169:length(my_species$species_code)){
  #for(i in 1:3){
  
  #library(ebirdst)
  
  code <- my_species$species_code[i]
  
  run_name <- my_species$run_name.y[i]
  
  skip_to_next <- FALSE
  
  tryCatch(
    #  pis <- load_pis(run_name), error = function(e){skip_to_next <<- TRUE})
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
  rm(abd)
  
  postbre_abd_mean <- calc(postbre_abd, mean)
  rm(postbre_abd)
  
  #breeding_rabd[[i]] <- bre_abd_mean
  
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

#average for each species across breeding range
names(postbreeding_exp) <- my_species$species_code

func <- function(x) {
  c(mean = mean(x), var = var(x))
}

test <- lapply(breeding_exp, func)
test1 <- bind_rows(test, .id = "species")


#### nonbreeding ####

#relative abundance for seasonal windows, multiplied by PLAND
#mod_pland_ebd <- raster("D:/Allison/Big_data/Ch-2 landcover/modified_cover_ebd_nobarren.tif")
mod_pland_ebd <- raster("D:/Allison/Big_data/Ch-2 landcover/modified_cover_ebd_noFCM.tif")

#need updated run names
ebirdst_species <- ebirdst_runs %>%
  dplyr::select(c("species_code","run_name"))

my_species <- read.csv("C:/Users/AllisonBinley/OneDrive - Carleton University/thesis/CH2_2021/data/bird_data_v4.csv")
my_species <- left_join(my_species, ebirdst_species, by = "species_code")

setwd("E:/eBird/data/raw/STEM")


#breeding_rabd <- list()
#save(nonbreeding_exp, file = "D:/Allison/Big_data/eBird_outputs/nonbreeding_exp_nobarren.RData")
#load("D:/Allison/Big_data/eBird_outputs/nonbreeding_exp_nobarren.RData")
save(nonbreeding_exp, file = "D:/Allison/Big_data/eBird_outputs/nonbreeding_exp_noFCM.RData")
load("D:/Allison/Big_data/eBird_outputs/nonbreeding_exp_noFCM.RData")

nonbreeding_exp <- list()

for(i in 227:238){
  #for(i in 169:length(my_species$species_code)){
  #for(i in 1:3){
  
  #library(ebirdst)
  
  code <- my_species$species_code[i]
  
  run_name <- my_species$run_name.y[i]
  
  skip_to_next <- FALSE
  
  tryCatch(
    #  pis <- load_pis(run_name), error = function(e){skip_to_next <<- TRUE})
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
  
  #breeding_rabd[[i]] <- bre_abd_mean
  
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

#average for each species across breeding range
names(nonbreeding_exp) <- my_species$species_code

func <- function(x) {
  c(mean = mean(x), var = var(x))
}

test <- lapply(breeding_exp, func)
test1 <- bind_rows(test, .id = "species")





#### prebreeding ####

#relative abundance for seasonal windows, multiplied by PLAND
#mod_pland_ebd <- raster("D:/Allison/Big_data/Ch-2 landcover/modified_cover_ebd_nobarren.tif")
mod_pland_ebd <- raster("D:/Allison/Big_data/Ch-2 landcover/modified_cover_ebd_noFCM.tif")

#need updated run names
ebirdst_species <- ebirdst_runs %>%
  dplyr::select(c("species_code","run_name"))

my_species <- read.csv("C:/Users/AllisonBinley/OneDrive - Carleton University/thesis/CH2_2021/data/bird_data_v4.csv")
my_species <- left_join(my_species, ebirdst_species, by = "species_code")

setwd("E:/eBird/data/raw/STEM")


#breeding_rabd <- list()
# save(prebreeding_exp, file = "D:/Allison/Big_data/eBird_outputs/prebreeding_exp_nobarren.RData")
# load("D:/Allison/Big_data/eBird_outputs/prebreeding_exp_nobarren.RData")
save(prebreeding_exp, file = "D:/Allison/Big_data/eBird_outputs/prebreeding_exp_noFCM.RData")
load("D:/Allison/Big_data/eBird_outputs/prebreeding_exp_noFCM.RData")

prebreeding_exp <- list()

for(i in 154:238){
  #for(i in 169:length(my_species$species_code)){
  #for(i in 1:3){
  
  #library(ebirdst)
  
  code <- my_species$species_code[i]
  
  run_name <- my_species$run_name.y[i]
  
  skip_to_next <- FALSE
  
  tryCatch(
    #  pis <- load_pis(run_name), error = function(e){skip_to_next <<- TRUE})
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
  
  #breeding_rabd[[i]] <- bre_abd_mean
  
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

#### all seasons ####

my_species <- read.csv("C:/Users/AllisonBinley/OneDrive - Carleton University/thesis/CH2_2021/data/bird_data_v4.csv")
names <- my_species$species_code
names <- names[-114]

#first rescue reevir1 - not working because stupid API key wont work
library(ebirdst)
#setwd("E:/eBird/data/raw/STEM")

# ebirdst_download(
#   species = "reevir1", #spec,
#   path = getwd(),
#   tifs_only = FALSE,
#   force = TRUE,
#   show_progress = TRUE
# )

load("D:/Allison/Big_data/eBird_outputs/breeding_exp_nobarren.RData")
load("D:/Allison/Big_data/eBird_outputs/breeding_exp_noFCM.RData")
load("D:/Allison/Big_data/eBird_outputs/postbreeding_exp_nobarren.RData")
load("D:/Allison/Big_data/eBird_outputs/postbreeding_exp_noFCM.RData")
load("D:/Allison/Big_data/eBird_outputs/nonbreeding_exp_nobarren.RData")
load("D:/Allison/Big_data/eBird_outputs/nonbreeding_exp_noFCM.RData")
load("D:/Allison/Big_data/eBird_outputs/prebreeding_exp_nobarren.RData")
load("D:/Allison/Big_data/eBird_outputs/prebreeding_exp_noFCM.RData")

func <- function(x) {
  c(sum = sum(x))#, se = sd(x)/sqrt(length(x))) 
}

breeding <- lapply(breeding_exp1, func)
names(breeding) <- names
breeding <- bind_rows(breeding, .id = "species_code")
breeding$season <- rep("breeding",length(breeding$species_code))
breeding$sum[108] <- 0 #gycthr - breeds in the far north, no trees
#breeding$se[108] <- 0
load("D:/Allison/Big_data/eBird_outputs/breeding_weights.RData")
names(breeding_weights1) <- names
breeding_weights2 <- bind_rows(breeding_weights1, .id = "species_code")
breeding_w <- pivot_longer(breeding_weights2, cols = 1:238, names_to= "species_code", values_to = "weight_sum")
breeding_weighted_abd <- inner_join(breeding, breeding_w)
breeding_weighted_abd <- breeding_weighted_abd %>%
  mutate(weighted_abd = sum/weight_sum)

postbreeding <- lapply(postbreeding_exp1, func)
names(postbreeding) <- names
postbreeding <- bind_rows(postbreeding, .id = "species_code")
postbreeding$season <- rep("postbreeding",length(postbreeding$species_code))
load("D:/Allison/Big_data/eBird_outputs/postbreeding_weights.RData")
names(postbreeding_weights1) <- names
postbreeding_weights2 <- bind_rows(postbreeding_weights1, .id = "species_code")
postbreeding_w <- pivot_longer(postbreeding_weights2, cols = 1:238, names_to= "species_code", values_to = "weight_sum")
postbreeding_weighted_abd <- inner_join(postbreeding, postbreeding_w)
postbreeding_weighted_abd <- postbreeding_weighted_abd %>%
  mutate(weighted_abd = sum/weight_sum)

nonbreeding <- lapply(nonbreeding_exp1, func)
names(nonbreeding) <- names
nonbreeding <- bind_rows(nonbreeding, .id = "species_code")
nonbreeding$season <- rep("nonbreeding",length(nonbreeding$species_code))
#nonbreeding$se[90] <- 0 #only one cell
load("D:/Allison/Big_data/eBird_outputs/nonbreeding_weights.RData")
names(nonbreeding_weights1) <- names
nonbreeding_weights2 <- bind_rows(nonbreeding_weights1, .id = "species_code")
nonbreeding_w <- pivot_longer(nonbreeding_weights2, cols = 1:238, names_to= "species_code", values_to = "weight_sum")
nonbreeding_weighted_abd <- inner_join(nonbreeding, nonbreeding_w)
nonbreeding_weighted_abd <- nonbreeding_weighted_abd %>%
  mutate(weighted_abd = sum/weight_sum)

prebreeding <- lapply(prebreeding_exp1, func)
names(prebreeding) <- names
prebreeding <- bind_rows(prebreeding, .id = "species_code")
prebreeding$season <- rep("prebreeding",length(prebreeding$species_code))
load("D:/Allison/Big_data/eBird_outputs/prebreeding_weights.RData")
names(prebreeding_weights1) <- names
prebreeding_weights2 <- bind_rows(prebreeding_weights1, .id = "species_code")
prebreeding_w <- pivot_longer(prebreeding_weights2, cols = 1:238, names_to= "species_code", values_to = "weight_sum")
prebreeding_weighted_abd <- inner_join(prebreeding, prebreeding_w)
prebreeding_weighted_abd <- prebreeding_weighted_abd %>%
  mutate(weighted_abd = sum/weight_sum)

availability <- rbind(breeding_weighted_abd,postbreeding_weighted_abd,nonbreeding_weighted_abd,prebreeding_weighted_abd)
load("data/species_basic.RData")

availability1 <- left_join(availability,species_data)
save(availability1, file = "data_outputs/availability_data_nobarren_updated.RData")
save(availability1, file = "data_outputs/availability_data_noFCM_updated.RData")




