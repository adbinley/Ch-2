library(ebirdst)
library(sf)
library(tidyverse)

setwd("E:/eBird/data/raw/STEM")
path <- "magwar-ERD2019-STATUS-20200930-3ed92d66"

load("data_outputs/rpis_breeding_2019.RData")

stixels <- load_stixels(path)

stixels1 <- stixels %>%
  filter(stixel_id %in% magwar$stixel_id)

stixels_random <- sample_n(stixels1, 10, replace=F)

stixels2 <- stixelize(stixels_random)
plot(st_geometry(stixels2))

# load gis data for making maps
map_proj <- st_crs(4326)
ne_land <- read_sf("C:/Users/AllisonBinley/OneDrive - Carleton University/eBird r code/2. Subset eBird/BestPracdata/data/gis-data.gpkg", "ne_land") %>% 
  st_transform(crs = map_proj) #%>% 
  st_geometry()
bcr <- read_sf("C:/Users/AllisonBinley/OneDrive - Carleton University/eBird r code/2. Subset eBird/BestPracdata/data/gis-data.gpkg", "bcr") %>% 
  st_transform(crs = map_proj) %>% 
  st_geometry()
ne_country_lines <- read_sf("C:/Users/AllisonBinley/OneDrive - Carleton University/eBird r code/2. Subset eBird/BestPracdata/data/gis-data.gpkg", "ne_country_lines") %>% 
  st_transform(crs = map_proj) #%>% 
  st_geometry()
ne_state_lines <- read_sf("C:/Users/AllisonBinley/OneDrive - Carleton University/eBird r code/2. Subset eBird/BestPracdata/data/gis-data.gpkg", "ne_state_lines") %>% 
  st_transform(crs = map_proj) #%>% 
  st_geometry()

plot(st_geometry(stixels2), col = "#1F968BFF")
plot(ne_land, add=T)
plot(ne_country_lines, add=T)
#plot(ne_state_lines, add = T)

setwd("D:/Allison/Github_Projects/Ch-2/Ch-2")

save(list=c("stixels2","ne_land","ne_country_lines"), file = "appendix_plots/appC1.RData")

colnames(rpis_id_breeding)

stixels3 <- stixels2[4,]
plot(st_geometry(stixels3))

plot(ne_country_lines)
plot(ne_land, add=T)
plot(st_geometry(stixels3), col = "yellow", border= 6, add=T)


library(png)
library(cowplot)
library(magick)
map1 <- ggplot()+
  geom_sf(data= ne_land)+
  geom_sf(data= ne_country_lines)+
  geom_sf(data=stixels2, fill="#1F968BFF", alpha = 0.5)+
  geom_sf(data = stixels3, col = "#1F968BFF", fill = "yellow")+
  coord_sf(xlim=c(-150,-25))+
  theme_classic()

map1

map2 <- ggdraw(map1) + 
  draw_image("appendix_plots/mawa3.png", x = 0.95, y = 0.1, hjust = 1, vjust = 0, width = 0.25, height = 0.25)

  
geom_sf(data= ne_country_lines)+
  geom_sf(data=ne_land)+
  geom_sf(st_geometry(stixels3), col = "yellow", border= 6)

stacked <- rpis_id_breeding%>%
  filter(stixel_id == "63-199.6-NSSWNEEN",#"48-177-NSSNSW",
         species_code=="magwar")%>% #this is the specific stixel pictured on the map
  select(c(6:28))

stacked_long <- pivot_longer(stacked, 1:23, names_to="lc_class", values_to = "PI")
stacked_long$x <- rep("x", length(stacked_long$lc_class))

stacked_long$lc_class <- factor(stacked_long$lc_class, levels=c("Evergreen Needleleaf Forests PLAND","Evergreen Broadleaf Forests PLAND",                
                                                                "Deciduous Needleleaf Forests PLAND","Deciduous Broadleaf Forests PLAND",                
                                                                "Mixed Broadleaf/Needleleaf Forests PLAND",
                                                                "Mixed Broadleaf Evergreen/Deciduous Forests PLAND",
                                                                "Open Forests PLAND", "Woody Wetlands PLAND", #8 natural
                                                                "Forest/Cropland Mosaics PLAND",                    
                                                                "Natural Herbaceous/Croplands Mosaics PLAND",       
                                                                "Herbaceous Croplands PLAND", 
                                                                "Barren PLAND", # 4 modified
                                                                "Tidal Mudflats PLAND","Permanent Snow and Ice PLAND","Sparse Forests PLAND",
                                                                "Unclassified PLAND","Dense Herbaceous PLAND","Sparse Herbaceous PLAND",
                                                                "Dense Shrublands PLAND","Shrubland/Grassland Mosaics PLAND","Sparse Shrublands PLAND",
                                                                "Herbaceous Wetlands PLAND","Tundra PLAND" # 11 other
                                                                ))


stacked_long1 <- stacked_long %>%
  filter(PI >0) #6 natural, 3 modified, 2 other

clrs <- c("#062891","#0541C2","#1097F7","#051F72","#04528A","#6c8EF8","#FEE227","#FDC12A","#FDA50F","#616161","#C1C1C1")
stacked_long1$lc_class <- factor(stacked_long1$lc_class, levels=c("Evergreen Needleleaf Forests PLAND",               
                                 "Deciduous Needleleaf Forests PLAND","Deciduous Broadleaf Forests PLAND",                
                                 "Mixed Broadleaf/Needleleaf Forests PLAND",
                                 "Open Forests PLAND", "Woody Wetlands PLAND", #8 natural
                                 "Forest/Cropland Mosaics PLAND",                    
                                 "Herbaceous Croplands PLAND", 
                                 "Barren PLAND", # 4 modified
                                 "Sparse Forests PLAND",
                                 "Dense Herbaceous PLAND"))

plot_main <- ggplot(stacked_long1, aes(x=x, y=PI, fill = lc_class))+
  geom_bar(position="fill", stat="identity")+
  scale_fill_manual(values= clrs)+
  theme_classic(base_family = "serif", base_size = 18)+
  theme(axis.ticks.x = element_blank(),
        axis.text.x = element_blank())+
  xlab("")+
  ylab("Relative Predictor Importance")+
  labs(fill="Land-Cover Class")

ggplot(stacked_long1, aes(x=x, y=PI, fill = lc_class))+
  geom_bar(position="fill", stat="identity")+
  scale_fill_manual(name = "Land Cover Class",values= clrs,
                    labels = c("Evergreen Needleleaf Forests",               
                               "Deciduous Needleleaf Forests","Deciduous Broadleaf Forests",                
                               "Mixed Broadleaf/Needleleaf Forests",
                               "Open Forests", "Woody Wetlands PLAND", #8 natural
                               "Forest/Cropland Mosaics",                    
                               "Herbaceous Croplands", 
                               "Barren", # 4 modified
                               "Sparse Forests",
                               "Dense Herbaceous"))+
  theme_classic(base_family = "serif", base_size = 18)+
  theme(axis.ticks.x = element_blank(),
        axis.text.x = element_blank())+
  xlab("")+
  ylab("Relative Predictor Importance")

plot_list <- list(map2,rpi_plot)

ggarrange(plotlist = plot_list,
          ncol=2,
          widths = c(1,2))

#pds

#get and plot pds for magwar only

ebirdst_species <- ebirdst_runs %>%
  filter(species_code == "magwar")

start <- ebirdst_species %>%
  {.[["breeding_start"]]}

end <- ebirdst_species %>%
  {.[["breeding_end"]]}

bre <- c(start, end)  

ext <-  c(xmin = -180, xmax = 180, 
          ymin = -90, ymax = 90)

bre_extent <- ebirdst_extent(ext, t = bre)

run_name <- ebirdst_species$run_name

path <- "E:/eBird/data/raw/STEM/magwar-ERD2019-STATUS-20200930-3ed92d66"

pds <- load_pds(path = path)

natural <- c("mcd12q1_lccs1_fs_c11_1500_pland",
                           "mcd12q1_lccs1_fs_c12_1500_pland",
                           "mcd12q1_lccs1_fs_c13_1500_pland",
                           "mcd12q1_lccs1_fs_c14_1500_pland",
                           "mcd12q1_lccs1_fs_c15_1500_pland",
                           "mcd12q1_lccs1_fs_c16_1500_pland",
                           "mcd12q1_lccs1_fs_c21_1500_pland",
                           "mcd12q1_lccs3_fs_c27_1500_pland")

modified <- c("mcd12q1_lccs2_fs_c25_1500_pland",
              "mcd12q1_lccs2_fs_c35_1500_pland",
              "mcd12q1_lccs2_fs_c36_1500_pland",
              "mcd12q1_lccs1_fs_c1_1500_pland")


natural1 <- c("Evergreen Needleleaf Forests","Evergreen Broadleaf Forests",                
              "Deciduous Needleleaf Forests","Deciduous Broadleaf Forests",                
              "Mixed Broadleaf/Needleleaf Forests",
              "Mixed Broadleaf Evergreen/Deciduous Forests",
              "Open Forests", "Woody Wetlands") 

modified1 <- c("Forest/Cropland Mosaics",                    
               "Natural Herbaceous/Croplands Mosaics",       
               "Herbaceous Croplands", 
               "Barren" )
  
lc_classes <- c(natural,modified)
lc_classes1 <- c(natural1,modified1)

pds1 <- pds %>%
  filter(predictor %in% lc_classes) %>% #filtered to only our natural and modified land cover class predictors
  filter(stixel_id == "63-199.6-NSSWNEEN") #filtered to highlighted stixel

pds2 <- pds %>%
  filter(predictor %in% lc_classes)

#pd_smooth <- plot_pds(pds2, lc_classes[12], ext = bre_extent)
data <- ebirdst_subset(pds, bre_extent) %>%
  filter(predictor == natural[j])

#### pd plots #### 
#yes I should have looped it but i didnt so

library(ggtext)

pd_plot1 <- ebirdst_subset(pds2, bre_extent) %>%
  filter(predictor == lc_classes[1])%>%
  ggplot(aes(predictor_value,response))+
  #geom_point(alpha=0.2)+
  geom_smooth(method = "lm", size = 2, se=F)+
  geom_smooth(method = "gam", se=F, size=1, col="black", linetype="dashed")+
  theme_classic(base_size = 12, base_family = "serif")+
  xlab("")+ #Predictor Level
  ylab("")+#Probability Occurrence
  ggtitle("Evergreen Needleleaf Forest")+
  xlim(0,100)+
  ylim(0,0.4)+
  theme(plot.title = element_textbox_simple())

pd_plot2 <- ebirdst_subset(pds2, bre_extent) %>%
  filter(predictor == lc_classes[2])%>%
  ggplot(aes(predictor_value,response))+
  #geom_point(alpha=0.2)+
  geom_smooth(method = "lm", size = 2, se=F)+
  geom_smooth(method = "gam", se=F, size=1, col="black", linetype="dashed")+
  theme_classic(base_size = 12, base_family = "serif")+
  xlab("")+ #Predictor Level
  ylab("")+#Probability Occurrence
  ggtitle("Evergreen Broadleaf Forests")+
  xlim(0,100)+
  ylim(0,0.4)+
  theme(plot.title = element_textbox_simple())
#pd_plot2

pd_plot3 <- ebirdst_subset(pds2, bre_extent) %>%
  filter(predictor == lc_classes[3])%>%
  ggplot(aes(predictor_value,response))+
  #geom_point(alpha=0.2)+
  geom_smooth(method = "lm", size = 2, se=F)+
  geom_smooth(method = "gam", se=F, size=1, col="black", linetype="dashed")+
  theme_classic(base_size = 12, base_family = "serif")+
  xlab("")+ #Predictor Level
  ylab("")+#Probability Occurrence
  ggtitle("Deciduous Needleleaf Forests")+
  xlim(0,100)+
  ylim(0,0.4)+
  theme(plot.title = element_textbox_simple())
#pd_plot3

pd_plot4 <- ebirdst_subset(pds2, bre_extent) %>%
  filter(predictor == lc_classes[4])%>%
  ggplot(aes(predictor_value,response))+
  #geom_point(alpha=0.2)+
  geom_smooth(method = "lm", size = 2, se=F)+
  geom_smooth(method = "gam", se=F, size=1, col="black", linetype="dashed")+
  theme_classic(base_size = 12, base_family = "serif")+
  xlab("")+ #Predictor Level
  ylab("")+#Probability Occurrence
  ggtitle("Deciduous Broadleaf Forests")+
  xlim(0,100)+
  ylim(0,0.4)+
  theme(plot.title = element_textbox_simple())
#pd_plot4

pd_plot5 <- ebirdst_subset(pds2, bre_extent) %>%
  filter(predictor == lc_classes[5])%>%
  ggplot(aes(predictor_value,response))+
  #geom_point(alpha=0.2)+
  geom_smooth(method = "lm", size = 2, se=F)+
  geom_smooth(method = "gam", se=F, size=1, col="black", linetype="dashed")+
  theme_classic(base_size = 12, base_family = "serif")+
  xlab("")+ #Predictor Level
  ylab("")+#Probability Occurrence
  ggtitle("Mixed Broadleaf/Needleleaf Forests")+
  xlim(0,100)+
  ylim(0,0.4)+
  theme(plot.title = element_textbox_simple())
#pd_plot5

pd_plot6 <- ebirdst_subset(pds2, bre_extent) %>%
  filter(predictor == lc_classes[6])%>%
  ggplot(aes(predictor_value,response))+
  #geom_point(alpha=0.2)+
  geom_smooth(method = "lm", size = 2, se=F)+
  geom_smooth(method = "gam", se=F, size=1, col="black", linetype="dashed")+
  theme_classic(base_size = 12, base_family = "serif")+
  xlab("")+ #Predictor Level
  ylab("")+#Probability Occurrence
  ggtitle("Mixed Broadleaf Evergreen/Deciduous Forests")+
  xlim(0,100)+
  ylim(0,0.4)+
  theme(plot.title = element_textbox_simple())
#pd_plot6

pd_plot7 <- ebirdst_subset(pds2, bre_extent) %>%
  filter(predictor == lc_classes[7])%>%
  ggplot(aes(predictor_value,response))+
  #geom_point(alpha=0.2)+
  geom_smooth(method = "lm", size = 2, se=F)+
  geom_smooth(method = "gam", se=F, size=1, col="black", linetype="dashed")+
  theme_classic(base_size = 12, base_family = "serif")+
  xlab("")+ #Predictor Level
  ylab("")+#Probability Occurrence
  ggtitle("Open Forests")+
  xlim(0,100)+
  ylim(0,0.4)+
  theme(plot.title = element_textbox_simple())
#pd_plot7

pd_plot8 <- ebirdst_subset(pds2, bre_extent) %>%
  filter(predictor == lc_classes[8])%>%
  ggplot(aes(predictor_value,response))+
  #geom_point(alpha=0.2)+
  geom_smooth(method = "lm", size = 2, se=F)+
  geom_smooth(method = "gam", se=F, size=1, col="black", linetype="dashed")+
  theme_classic(base_size = 12, base_family = "serif")+
  xlab("")+ #Predictor Level
  ylab("")+#Probability Occurrence
  ggtitle("Woody Wetlands")+
  xlim(0,100)+
  ylim(0,0.4)+
  theme(plot.title = element_textbox_simple())
#pd_plot8

pd_plot9 <- ebirdst_subset(pds2, bre_extent) %>%
  filter(predictor == lc_classes[9])%>%
  ggplot(aes(predictor_value,response))+
  #geom_point(alpha=0.2)+
  geom_smooth(method = "lm", size = 2, col = "#FDC12A", se=F)+
  geom_smooth(method = "gam", se=F, size=1, col="black", linetype="dashed")+
  theme_classic(base_size = 12, base_family = "serif")+
  xlab("")+ #Predictor Level
  ylab("")+#Probability Occurrence
  ggtitle("Forest/Cropland Mosaics")+
  xlim(0,100)+
  ylim(0,0.4)+
  theme(plot.title = element_textbox_simple())
#pd_plot9

pd_plot10 <- ebirdst_subset(pds2, bre_extent) %>%
  filter(predictor == lc_classes[10])%>%
  ggplot(aes(predictor_value,response))+
  #geom_point(alpha=0.2)+
  geom_smooth(method = "lm", size = 2, col = "#FDC12A", se=F)+
  geom_smooth(method = "gam", se=F, size=1, col="black", linetype="dashed")+
  theme_classic(base_size = 12, base_family = "serif")+
  xlab("")+ #Predictor Level
  ylab("")+#Probability Occurrence
  ggtitle("Natural Herbaceous/Croplands Mosaics")+
  xlim(0,100)+
  ylim(0,0.4)+
  theme(plot.title = element_textbox_simple())
#pd_plot10

pd_plot11 <- ebirdst_subset(pds2, bre_extent) %>%
  filter(predictor == lc_classes[11])%>%
  ggplot(aes(predictor_value,response))+
  #geom_point(alpha=0.2)+
  geom_smooth(method = "lm", size = 2, col = "#FDC12A", se=F)+
  geom_smooth(method = "gam", se=F, size=1, col="black", linetype="dashed")+
  theme_classic(base_size = 12, base_family = "serif")+
  xlab("")+ #Predictor Level
  ylab("")+#Probability Occurrence
  ggtitle("Herbaceous Croplands")+
  xlim(0,100)+
  ylim(0,0.4)+
  theme(plot.title = element_textbox_simple())
#pd_plot11

pd_plot12 <- ebirdst_subset(pds2, bre_extent) %>%
  filter(predictor == lc_classes[12])%>%
  ggplot(aes(predictor_value,response))+
  #geom_point(alpha=0.2)+
  geom_smooth(method = "lm", size = 2, col = "#FDC12A", se=F)+
  geom_smooth(method = "gam", se=F, size=1, col="black", linetype="dashed")+
  theme_classic(base_size = 12, base_family = "serif")+
  xlab("")+ #Predictor Level
  ylab("")+#Probability Occurrence
  ggtitle("Barren")+
  xlim(0,100)+
  ylim(0,0.4)+
  theme(plot.title = element_textbox_simple())
#pd_plot12

pd_plot_list <- list(pd_plot1,pd_plot2,pd_plot3,pd_plot4,
                     pd_plot5,pd_plot6,pd_plot7,pd_plot8,
                     pd_plot9,pd_plot10,pd_plot11,pd_plot12)


pd_plots <- ggarrange(plotlist = pd_plot_list,
          ncol=4,
          nrow = 3)

theme(text = element_text())$text[ c("serif") ]

pd_plots <- annotate_figure(pd_plots,
                left = "Prob. Occurrence",
                bottom = "Proportion Cover")

png("fig_outputs/pd_plots.png", height = 9, width = 11.5, units = "in",res=300)
pd_plots
dev.off()
