#supplementary materials

#### Appendix S1 ####

load("data/species_basic.RData")

# Table S1- contingency table of migratory strategy and diet
table(species_basic$diet2,species_basic$SW_mig)

# Table S2- contingency table of migratory strategy and foraging strategy
table(species_basic$sw_foraging,species_basic$SW_mig)

# Table S3- contingency table of diet and foraging strategy
table(species_basic$sw_foraging,species_basic$diet2)

#### Appendix S2 ####

data <- read.csv("data/bird_data_v4.csv")
#cleaner table linked in supp, but same information contained here
#all relevant scripts in repository

#### Appendix S3 ####

library(ebirdst)
library(sf)
library(tidyverse)
library(ggpubr)
library(png)
library(cowplot)
library(magick)
library(ggtext)

setwd("E:/eBird/data/raw/STEM")
path <- "magwar-ERD2019-STATUS-20200930-3ed92d66"

load("D:/Allison/Github_Projects/Ch-2/Ch-2/data_outputs/rpis_breeding_2019.RData")
magwar <- rpis_id_breeding %>%
  filter(species_code=="magwar")

stixels <- load_stixels(path)

stixels1 <- stixels %>%
  filter(stixel_id %in% magwar$stixel_id)

set.seed(45)
stixels_random <- sample_n(stixels1, 10, replace=F)

stixels2 <- stixelize(stixels_random)
plot(st_geometry(stixels2))

# load gis data for making maps
map_proj <- st_crs(4326)
ne_land <- read_sf("data/gis-data.gpkg", "ne_land") %>% 
  st_transform(crs = map_proj) #%>% 
st_geometry()
bcr <- read_sf("data/gis-data.gpkg", "bcr") %>% 
  st_transform(crs = map_proj) %>% 
  st_geometry()
ne_country_lines <- read_sf("data/gis-data.gpkg", "ne_country_lines") %>% 
  st_transform(crs = map_proj) #%>% 
st_geometry()
ne_state_lines <- read_sf("C:/Users/AllisonBinley/OneDrive - Carleton University/eBird r code/2. Subset eBird/BestPracdata/data/gis-data.gpkg", "ne_state_lines") %>% 
  st_transform(crs = map_proj) #%>% 
st_geometry()

plot(st_geometry(stixels2), col = "#1F968BFF")
plot(ne_land, add=T)
plot(ne_country_lines, add=T)

setwd("D:/Allison/Github_Projects/Ch-2/Ch-2")

stixels3 <- stixels2[4,]

plot(ne_country_lines)
plot(ne_land, add=T)
plot(st_geometry(stixels3), col = "yellow", border= 6, add=T)


map1 <- ggplot()+
  geom_sf(data= ne_land)+
  geom_sf(data= ne_country_lines)+
  geom_sf(data=stixels2, fill="#1F968BFF", alpha = 0.5)+
  geom_sf(data = stixels3, col = "orange", fill = "yellow")+
  coord_sf(xlim=c(-150,-25))+
  theme_classic()

map1

map2 <- ggdraw(map1) + 
  draw_image("appendix_plots/mawa3.png", x = 0.95, y = 0.1, hjust = 1, vjust = 0, width = 0.25, height = 0.25)

png("fig_outputs/map.png", height = 9, width = 11.5, units = "in",res=300)
map2
dev.off()


stacked <- rpis_id_breeding%>%
  filter(stixel_id == "70-168-NSSWEWWN",#"63-199.6-NSSWNEEN",#"48-177-NSSNSW",
         species_code=="magwar")%>% #this is the specific stixel pictured on the map
  select(c(6:28))

stacked_long <- pivot_longer(stacked, 1:23, names_to="lc_class", values_to = "PI")
stacked_long$x <- rep("x", length(stacked_long$lc_class))
stacked_long1 <- filter(stacked_long)%>%
  filter(PI > 0)

stacked_long$lc_class <- factor(stacked_long$lc_class, levels=c("Evergreen Needleleaf Forests PLAND",
                                                                "Deciduous Needleleaf Forests PLAND",
                                                                "Deciduous Broadleaf Forests PLAND",
                                                                "Mixed Broadleaf/Needleleaf Forests PLAND",
                                                                "Open Forests PLAND",
                                                                "Woody Wetlands PLAND", #natural forest
                                                                "Forest/Cropland Mosaics PLAND",
                                                                "Herbaceous Croplands PLAND", #modified
                                                                "Sparse Forests PLAND",
                                                                "Dense Herbaceous PLAND",
                                                                "Herbaceous Wetlands PLAND"#other
))



clrs <- c("#062891","#0541C2","#1097F7","#051F72","#04528A","#6c8EF8","#FEE227","#FDA50F","#616161","#C1C1C1","9A9A9A") #"#FDC12A",

stacked_long1$lc_class <- factor(stacked_long1$lc_class, levels=c("Evergreen Needleleaf Forests PLAND",
                                                                  "Deciduous Needleleaf Forests PLAND",
                                                                  "Deciduous Broadleaf Forests PLAND",
                                                                  "Mixed Broadleaf/Needleleaf Forests PLAND",
                                                                  "Open Forests PLAND",
                                                                  "Woody Wetlands PLAND", #natural forest
                                                                  "Forest/Cropland Mosaics PLAND",
                                                                  "Herbaceous Croplands PLAND", #modified
                                                                  "Sparse Forests PLAND",
                                                                  "Dense Herbaceous PLAND",
                                                                  "Herbaceous Wetlands PLAND"#other
))


PI <- ggplot(stacked_long1, aes(x=x, y=PI, fill = lc_class))+
  geom_bar(position="fill", stat="identity")+
  scale_fill_manual(name = "Land Cover Class",values= clrs,
                    labels = c("Evergreen Needleleaf Forests",
                               "Deciduous Needleleaf Forests",
                               "Deciduous Broadleaf Forests",
                               "Mixed Broadleaf/Needleleaf Forests",
                               "Open Forests",
                               "Woody Wetlands", #natural forest
                               "Forest/Cropland Mosaics",
                               "Herbaceous Croplands", #modified
                               "Sparse Forests",
                               "Dense Herbaceous",
                               "Herbaceous Wetlands"#other
                    ))+
  theme_classic(base_family = "serif", base_size = 18)+
  theme(axis.ticks.x = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text.y = element_blank(),
        legend.position = "left")+
  xlab("")+
  ylab("Predictor Importance")

png("fig_outputs/PI.png", height = 9, width = 11.5, units = "in",res=300)
PI
dev.off()

natural <- c("Evergreen Needleleaf Forests PLAND","Evergreen Broadleaf Forests PLAND",                
             "Deciduous Needleleaf Forests PLAND","Deciduous Broadleaf Forests PLAND",                
             "Mixed Broadleaf/Needleleaf Forests PLAND",
             "Mixed Broadleaf Evergreen/Deciduous Forests PLAND",
             "Open Forests PLAND", "Woody Wetlands PLAND") 


impacted <- c("Forest/Cropland Mosaics PLAND",                    
              "Natural Herbaceous/Croplands Mosaics PLAND",       
              "Herbaceous Croplands PLAND", 
              "Barren PLAND" )


stacked_long2 <- stacked_long1 %>%
  filter(lc_class %in% natural | lc_class %in% impacted)%>%
  mutate(PI2 = PI/sum(PI),
         state=ifelse(lc_class %in% natural, "natural","modified"))

stacked_long3 <- stacked_long2 %>%
  group_by(state)%>%
  summarise(rPI = sum(PI2))

stacked_long3$x <- rep("x",length(stacked_long3$state))

clrs2 <- c("#062891","#FEE227")
stacked_long3$state <- factor(stacked_long3$state, levels=c("natural","modified"))

rPI <- ggplot(stacked_long3, aes(x=x, y=rPI, fill = state))+
  geom_bar(position="fill", stat="identity")+
  scale_fill_manual(name = "Land Cover Class",values= clrs2,
                    labels = c("Natural", "Modified"
                    ))+
  theme_classic(base_family = "serif", base_size = 18)+
  theme(axis.ticks.x = element_blank(),
        axis.text.x = element_blank())+
  xlab("")+
  ylab("Relative Predictor Importance")+
  scale_y_continuous(position = "right")

png("fig_outputs/rPI.png", height = 9, width = 11.5, units = "in",res=300)
rPI
dev.off()

plot_list <- list(PI,rPI)

pi_plots <- ggarrange(plotlist = plot_list,
                      ncol=2,
                      widths = c(1,1))

png("fig_outputs/pi_plots.png", height = 9, width = 11.5, units = "in",res=300)
pi_plots
dev.off()

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
               "Herbaceous Croplands" )

lc_classes <- c(natural,modified)
lc_classes1 <- c(natural1,modified1)

pds1 <- pds %>%
  filter(predictor %in% lc_classes) %>% #filtered to only our natural and modified land cover class predictors
  filter(stixel_id == "70-168-NSSWEWWN") #filtered to highlighted stixel

pds2 <- pds %>%
  filter(predictor %in% lc_classes)


# pd plots 
#yes I should have looped it or made a function 

library(ggtext)

pd_plot1 <- ebirdst_subset(pds2, bre_extent) %>%
  filter(predictor == lc_classes[1])%>%
  ggplot(aes(predictor_value,response))+
  #geom_point(alpha=0.2)+
  geom_smooth(method = "lm", size = 2, se=F)+
  #geom_smooth(method = "gam", se=F, size=1, col="black", linetype="dashed")+
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
  #geom_smooth(method = "gam", se=F, size=1, col="black", linetype="dashed")+
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
  #geom_smooth(method = "gam", se=F, size=1, col="black", linetype="dashed")+
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
  #geom_smooth(method = "gam", se=F, size=1, col="black", linetype="dashed")+
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
  #geom_smooth(method = "gam", se=F, size=1, col="black", linetype="dashed")+
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
  #geom_smooth(method = "gam", se=F, size=1, col="black", linetype="dashed")+
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
  #geom_smooth(method = "gam", se=F, size=1, col="black", linetype="dashed")+
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
  #geom_smooth(method = "gam", se=F, size=1, col="black", linetype="dashed")+
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
  #geom_smooth(method = "gam", se=F, size=1, col="black", linetype="dashed")+
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
  #geom_smooth(method = "gam", se=F, size=1, col="black", linetype="dashed")+
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
  #geom_smooth(method = "gam", se=F, size=1, col="black", linetype="dashed")+
  theme_classic(base_size = 12, base_family = "serif")+
  xlab("")+ #Predictor Level
  ylab("")+#Probability Occurrence
  ggtitle("Herbaceous Croplands")+
  xlim(0,100)+
  ylim(0,0.4)+
  theme(plot.title = element_textbox_simple())
#pd_plot11

pd_plot_list <- list(pd_plot1,pd_plot2,pd_plot3,pd_plot4,
                     pd_plot5,pd_plot6,pd_plot7,pd_plot8,
                     pd_plot9,pd_plot10,pd_plot11)


pd_plots <- ggarrange(plotlist = pd_plot_list,
                      ncol=4,
                      nrow = 3)

theme(text = element_text())$text[ c("serif") ]

pd_plots <- annotate_figure(pd_plots,
                            left = "Logit Probability Occurrence",
                            bottom = "Proportion Cover")


png("fig_outputs/pd_plots2.png", height = 12, width = 12, units = "in",res=300)
pd_plots
dev.off()

#combined

pd_plot_nat <- ebirdst_subset(pds2, bre_extent) %>%
  filter(predictor %in% lc_classes[1:8])%>%
  ggplot(aes(predictor_value,response))+
  geom_smooth(aes(group = predictor),method = "lm", size = 2, se=F)+
  geom_smooth(method = "lm", size = 2, col= "black", linetype="dashed", se=F)+
  theme_classic(base_size = 12, base_family = "serif")+
  xlab("")+ #Predictor Level
  ylab("")+#Probability Occurrence
  ggtitle("Natural")+
  xlim(0,100)+
  ylim(0,0.4)#+
#theme(plot.title = element_textbox_simple())
pd_plot_nat

#

pd_plot_mod <- ebirdst_subset(pds2, bre_extent) %>%
  filter(predictor %in% lc_classes[9:11])%>%
  ggplot(aes(predictor_value,response))+
  geom_smooth(aes(group = predictor),method = "lm", size = 2, col = "#FDC12A", se=F)+
  geom_smooth(method = "lm", size = 2, col= "black", linetype="dashed", se=F)+
  theme_classic(base_size = 12, base_family = "serif")+
  xlab("")+ 
  ylab("")+
  ggtitle("Modified")+
  xlim(0,100)+
  ylim(0,0.4)
pd_plot_mod

plot_list_2 <- list(pd_plot_nat,pd_plot_mod)

pd_plots2 <- ggarrange(plotlist = plot_list_2,
                       ncol=2,
                       nrow = 1)

png("fig_outputs/pd_plots3.png", height = 6, width = 11, units = "in",res=300)
pd_plots2
dev.off()

#### Appendix S4 ####

#pairwise comparisons

load("data_outputs/IHM_migrants_2019.RData")

# mod 1a

mig.stat.lmer <- lmer(IHM ~ season*SW_mig + (1 | species_code), data = migrants_2019)
mig.tukey.table <- emmeans(mig.stat.lmer, list(pairwise ~ SW_mig*season), adjust = "holm")
summary(mig.tukey.table)
write.table(mig.tukey.table$`pairwise differences of SW_mig, season`, file = "fig_outputs/mig-holm.txt", sep = ",", quote = FALSE, row.names = F)

#mod 1b

load("data_outputs/all_data4_mig.RData")
pd_aov <- aov(SHM~season*SW_mig, data = all_data4_mig)
mig.tukey.pd.table <- emmeans(pd_aov, list(pairwise ~ SW_mig*season), adjust = "holm")
summary(mig.tukey.pd.table)
write.table(mig.tukey.pd.table$`pairwise differences of SW_mig, season`, file = "fig_outputs/mig-holm-pd.txt", sep = ",", quote = FALSE, row.names = F)

#mod 2a

mig.diet.lmer <- lmer(IHM ~ season*diet2 + (1 | species_code), data = migrants_2019)
mig.diet.tukey.table <- emmeans(mig.diet.lmer, list(pairwise ~ diet2*season), adjust = "holm")
summary(mig.diet.tukey.table)
write.table(mig.diet.tukey.table$`pairwise differences of diet2, season`, file = "fig_outputs/diet-tuk-rpi.txt", sep = ",", quote = FALSE, row.names = F)

#mod 2b

load("data_outputs/all_data4_diet.RData")
pd_aov <- aov(SHM~season*diet2, data = all_data4_diet)
diet.tukey.pd.table <- emmeans(pd_aov, list(pairwise ~ diet2*season), adjust = "holm")
summary(diet.tukey.pd.table)
write.table(diet.tukey.pd.table$`pairwise differences of diet2, season`, file = "fig_outputs/diet-holm-pd.txt", sep = ",", quote = FALSE, row.names = F)

#mod 3a

mig.for.lmer <- lmer(IHM ~ season*sw_foraging + (1 | species_code), data = migrants)
for.holm.rpi <- emmeans(mig.for.lmer, list(pairwise ~ sw_foraging*season), adjust = "holm")
summary(for.holm.rpi)
write.table(for.holm.rpi$`pairwise differences of sw_foraging, season`, file = "fig_outputs/rPI-for-holm.txt", sep = ",", quote = FALSE, row.names = F)

#mod 3b

load("data_outputs/all_data4_for.RData")
pd_aov <- aov(SHM~season*sw_foraging, data = all_data4_for)
for.holm.pd.table <- emmeans(pd_aov, list(pairwise ~ sw_foraging*season), adjust = "holm")
summary(for.holm.pd.table)
write.table(for.holm.pd.table$`pairwise differences of sw_foraging, season`, file = "fig_outputs/for-holm-pd.txt", sep = ",", quote = FALSE, row.names = F)

#for model diagnostics, see 3-Results script

#### Appendix S5 ####

#see 2-alternate analyses script (rPIm and SHM) or 2-land cover script (MHA)


