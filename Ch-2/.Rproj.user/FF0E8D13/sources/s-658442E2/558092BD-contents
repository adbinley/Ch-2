#land cover - best practices
#everything broken
library(ebirdst)
library(tidyverse)

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

plot(q, legend = FALSE, col = rev(terrain.colors(12)))
legend("topright", legend = c("category 1", "category 2", "category 3", "category 4"), fill = rev(terrain.colors(4)))

natural <- c("Evergreen Needleleaf Forests","Evergreen Broadleaf Forests","Deciduous Needleleaf Forests","Deciduous Broadleaf Forests",
             "Mixed Broadleaf/Needleleaf Forests","Mixed Broadleaf Evergreen/Deciduous Forests","Open Forests")

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
rat <- levels(q2)[[1]]
rat[["landcover"]] <- c("Barren","Evergreen Needleleaf Forests", "Evergreen Broadleaf Forests",
                        "Deciduous Needleleaf Forests","Deciduous Broadleaf Forests","Mixed Broadleaf/Needleleaf Forests",
                        "Mixed Broadleaf Evergreen/Deciduous Forests","Open Forests")
levels(q2) <- rat

plot(x1, legend=F, col = "black")
my_col = rev(terrain.colors(n = 8))
plot(q2, legend = FALSE, col = my_col, add=T)
legend(x='bottomleft', text.width = 5e6, legend = c("Barren","Evergreen Needleleaf Forests", "Evergreen Broadleaf Forests",
                                  "Deciduous Needleleaf Forests","Deciduous Broadleaf Forests","Mixed Broadleaf/Needleleaf Forests",
                                  "Mixed Broadleaf Evergreen/Deciduous Forests","Open Forests"), 
       fill = my_col)

png("fig_outputs/LCCS1.png", height = 9, width = 11.5, units = "in",res=300)
LCCS1
dev.off()


rs <- stack(q,r,s)

N<-Which(q %in% nat,cells=T)
#N1[N]<-NEW_VALUE
N1 <- q[N]
N2 <-Which(r %in% nat,cells=T)
N3 <-r[N2]
N4 <-Which(s %in% nat,cells=T)
n5 <-s[N4]

nr <- do.call(merge, list(N1,N3,N5))

plot(r, legend = FALSE, col = rev(terrain.colors(4)))
legend("topright", legend = c("category 1", "category 2", "category 3", "category 4"), fill = rev(terrain.colors(4)))

