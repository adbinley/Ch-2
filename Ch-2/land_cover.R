#land cover - best practices
#everything broken
library(ebirdst)
library(tidyverse)

preds <- ebirdst::ebirdst_predictors

library(sf)
library(raster)
library(mapdata)
library(devtools)
install_version("MODIS")

library(MODIS)
library(exactextractr)
library(viridis)
library(tidyverse)
# resolve namespace conflicts
select <- dplyr::select
map <- purrr::map
projection <- raster::projection

# bcr 27 boundary
bcr <- read_sf("C:/Users/AllisonBinley/OneDrive - Carleton University/eBird r code/2. Subset eBird/BestPracdata/data/gis-data.gpkg", "bcr") %>% 
  filter(bcr_code == 27) %>% 
  # project to the native modis projection
  st_transform(crs = paste("+proj=sinu +lon_0=0 +x_0=0 +y_0=0",
                           "+a=6371007.181 +b=6371007.181 +units=m +no_defs"))%>%
  st_geometry()
# load ebird data
#ebird <- read_csv("data/ebd_woothr_june_bcr27_zf.csv")
# get list of tiles required to cover this bcr
tiles <- getTile(bcr)
tiles@tile