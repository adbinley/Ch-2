#supplementary materials

#### Appendix A ####

load("data/species_basic.RData")

# Table S1- contingency table of migratory strategy and diet
table(species_basic$diet2,species_basic$SW_mig)

# Table S2- contingency table of migratory strategy and foraging strategy
table(species_basic$sw_foraging,species_basic$SW_mig)

# Table S3- contingency table of diet and foraging strategy
table(species_basic$sw_foraging,species_basic$diet2)

#### Appendix B ####

#load data

#all relevant scripts in repository

#### Appendix C ####


#### Appendix D ####

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


#### Appendix E ####

#AIC and R squared


#### Appendix F ####

# IHM and SHM ratios separated into natural and impacted #

load("data_outputs/IHM_migrants_2019.RData")

#Figure 1a separated

IHM_sep <- migrants_2019 %>%
  pivot_longer(cols = c("natural1","impacted1"), names_to = "state", values_to = "rPI")

names <- c(
  'N'="Neotropical Migrant",
  'NR'="Neotropical Resident",
  'R'="North American Resident",
  'S'="Short-distance Migrant"
)

figS10 <- ggplot(aes(y = rPI, x = season, fill = state), data = IHM_sep)+
  geom_boxplot()+
  theme_classic(base_size = 22, base_family = "serif") +
  xlab("Season") +
  ylab("Relative Predictor Importance") +
  labs(fill = "State") +
  theme(axis.text.x = element_text(angle = 45, hjust=1))+
  scale_fill_npg(labels = c("Modified","Natural"))+
  facet_wrap(.~SW_mig, labeller = as_labeller(names))

png("appendix_plots/figureS10.png", height = 8, width = 11.5, units = "in",res=300)
figS10
dev.off()


#Figure 1b separated

load("data/species_basic.RData")
load("data_outputs/bootstrap_all.RData")

all_data <- left_join(bootstrap_all,species_basic,by="species_code")

all_data1 <- all_data %>%
  group_by(season, SW_mig, state) %>%
  summarise(across(where(is.numeric), ~sum(.>0)/length(.)))

all_data3 <- all_data1 %>%
  pivot_longer(!c("season","SW_mig","state"), names_to = "No.", values_to = "PP" )

all_data3$season <- factor(all_data3$season, levels=c("breeding", "postbreeding","nonbreeding", "prebreeding"))

figS11 <- ggplot(aes(y = PP, x = season, fill = state), data = all_data3)+
  geom_boxplot()+
  theme_classic(base_size = 22, base_family = "serif") +
  xlab("Season") +
  ylab("Proportion Positive") +
  labs(fill = "State") +
  theme(axis.text.x = element_text(angle = 45, hjust=1))+
  scale_fill_npg(labels = c("Modified","Natural"))+
  facet_wrap(.~SW_mig, labeller = as_labeller(names))

png("appendix_plots/figureS11.png", height = 8, width = 11.5, units = "in",res=300)
figS11
dev.off()

#Figure 2a separated

IHM_sep <- migrants_2019 %>%
  pivot_longer(cols = c("natural1","impacted1"), names_to = "state", values_to = "rPI")

names <- c(
  'C'="Carnivore",
  'F'="Frugivore",
  'G'="Granivore",
  'I'="Insectivore",
  'O'="Omnivore"
)

figS12 <- ggplot(aes(y = rPI, x = season, fill = state), data = IHM_sep)+
  geom_boxplot()+
  theme_classic(base_size = 22, base_family = "serif") +
  xlab("Season") +
  ylab("Relative Predictor Importance") +
  labs(fill = "State") +
  theme(axis.text.x = element_text(angle = 45, hjust=1))+
  scale_fill_npg(labels = c("Modified","Natural"))+
  facet_wrap(.~diet2, labeller = as_labeller(names))

png("appendix_plots/figureS12.png", height = 8, width = 11.5, units = "in",res=300)
figS12
dev.off()

#Figure 2b separated

load("data/species_basic.RData")
load("data_outputs/bootstrap_all.RData")

all_data <- left_join(bootstrap_all,species_basic,by="species_code")

all_data1 <- all_data %>%
  group_by(season, diet2, state) %>%
  summarise(across(where(is.numeric), ~sum(.>0)/length(.)))

all_data3 <- all_data1 %>%
  pivot_longer(!c("season","diet2","state"), names_to = "No.", values_to = "PP" )

all_data3$season <- factor(all_data3$season, levels=c("breeding", "postbreeding","nonbreeding", "prebreeding"))

figS13 <- ggplot(aes(y = PP, x = season, fill = state), data = all_data3)+
  geom_boxplot()+
  theme_classic(base_size = 22, base_family = "serif") +
  xlab("Season") +
  ylab("Proportion Positive") +
  labs(fill = "State") +
  theme(axis.text.x = element_text(angle = 45, hjust=1))+
  scale_fill_npg(labels = c("Modified","Natural"))+
  facet_wrap(.~diet2, labeller = as_labeller(names))

png("appendix_plots/figureS13.png", height = 8, width = 11.5, units = "in",res=300)
figS13
dev.off()

#Figure 3a separated

IHM_sep <- migrants_2019 %>%
  pivot_longer(cols = c("natural1","impacted1"), names_to = "state", values_to = "rPI")

names <- c(
  'F'="Foliage Gleaner",
  'B'="Bark Forager",
  'G'="Ground Forager",
  'AF'="Aerial Forager",
  'GH'="Ground Hawker",
  'AH'="Aerial Hawker"
)

figS14 <- ggplot(aes(y = rPI, x = season, fill = state), data = IHM_sep)+
  geom_boxplot()+
  theme_classic(base_size = 22, base_family = "serif") +
  xlab("Season") +
  ylab("Relative Predictor Importance") +
  labs(fill = "State") +
  theme(axis.text.x = element_text(angle = 45, hjust=1))+
  scale_fill_npg(labels = c("Modified","Natural"))+
  facet_wrap(.~sw_foraging, labeller = as_labeller(names))

png("appendix_plots/figureS14.png", height = 8, width = 11.5, units = "in",res=300)
figS14
dev.off()

#Figure 3b separated

load("data/species_basic.RData")
load("data_outputs/bootstrap_all.RData")

all_data <- left_join(bootstrap_all,species_basic,by="species_code")

all_data1 <- all_data %>%
  group_by(season, sw_foraging, state) %>%
  summarise(across(where(is.numeric), ~sum(.>0)/length(.)))

all_data3 <- all_data1 %>%
  pivot_longer(!c("season","sw_foraging","state"), names_to = "No.", values_to = "PP" )

all_data3$season <- factor(all_data3$season, levels=c("breeding", "postbreeding","nonbreeding", "prebreeding"))

figS15 <- ggplot(aes(y = PP, x = season, fill = state), data = all_data3)+
  geom_boxplot()+
  theme_classic(base_size = 22, base_family = "serif") +
  xlab("Season") +
  ylab("Proportion Positive") +
  labs(fill = "State") +
  theme(axis.text.x = element_text(angle = 45, hjust=1))+
  scale_fill_npg(labels = c("Modified","Natural"))+
  facet_wrap(.~sw_foraging, labeller = as_labeller(names))

png("appendix_plots/figureS15.png", height = 8, width = 11.5, units = "in",res=300)
figS15
dev.off()






#### maybe garbage below, delete b4 pub ####



#### Figure 1 ####
load("data_outputs/migrants_2019.RData")

x <- migrants_2019 %>%
  filter(season == "breeding") 
table(x$SW_mig, x$diet2)
table(x$SW_mig, x$sw_foraging)
table(x$diet2, x$sw_foraging)

#alternative visualization for figure 1a:

int.plota <- migrants_2019 %>%
  group_by(SW_mig, season) %>%
  dplyr::summarize(mean = mean(SHM),
                   sd = sd(SHM),
                   se = sd(SHM)/sqrt(length(SHM)))

int.plot1 <- int.plota %>% 
  mutate(group1 = ifelse(SW_mig == "N" | SW_mig == "S", "migratory", "resident"))

ggplot(int.plota, aes(x = season, y = mean, group = SW_mig, col = SW_mig)) +
  geom_line(position=position_dodge(0.25)) +
  geom_point(position=position_dodge(0.25)) +
  geom_errorbar(aes(ymin=mean-(2*se), ymax=mean+(2*se)), width=.2,
                position=position_dodge(0.25)) +
  theme_classic() +
  xlab("Season") +
  ylab("Mean SHM") +
  labs(col = "Migratory Strategy") +
  theme(axis.text.x = element_text(angle = 45, hjust=1))+
  scale_fill_npg()

#1b alternate
int.plotb <-all_data4 %>%
  group_by(SW_mig, season) %>%
  dplyr::summarize(mean = mean(ratio),
                   sd = sd(ratio),
                   se = sd(ratio)/sqrt(length(ratio)))

#int.plot2 <- int.plotb %>% 
#  mutate(group1 = ifelse(SW_mig == "N" | SW_mig == "S", "migratory", "resident"))

ggplot(int.plotb, aes(x = season, y = mean, group = SW_mig, col = SW_mig)) +
  geom_line(position=position_dodge(0.25)) +
  geom_point(position=position_dodge(0.25)) +
  geom_errorbar(aes(ymin=mean-(2*se), ymax=mean+(2*se)), width=.2,
                position=position_dodge(0.25)) +
  theme_classic() +
  xlab("Season") +
  ylab("Mean SHM") +
  geom_hline(yintercept=1, linetype="dashed", 
             color = "orange", size=1)+
  labs(col = "Migratory Strategy") +
  theme(axis.text.x = element_text(angle = 45, hjust=1))+
  scale_fill_npg()

# Figure 1 ratios separated into natural and impacted #
#discussion?

#rPIs

SHM_sep <- migrants_2019 %>%
  pivot_longer(cols = c("natural1","impacted1"), names_to = "state", values_to = "rPI")

ggplot(aes(y = rPI, x = season, fill = state), data = SHM_sep)+
  geom_boxplot()+
  theme_classic() +
  xlab("Season") +
  ylab("rPI") +
  labs(fill = "State") +
  theme(axis.text.x = element_text(angle = 45, hjust=1))+
  scale_fill_npg()+
  facet_wrap(~SW_mig)


#PDs

load("data/species_basic.RData")
load("data_outputs/bootstrap_all.RData")

all_data <- left_join(bootstrap_all,species_basic,by="species_code")

all_data1 <- all_data %>%
  group_by(season, SW_mig, state) %>%
  summarise(across(where(is.numeric), ~sum(.>0)/length(.)))

SD <- transform(all_data1[,-c(1:3)], stdev =apply(all_data1[,-c(1:3)], 1, sd, na.rm = TRUE))
stdev <- SD$stdev
M <- transform(all_data1[,-c(1:3)], mean =apply(all_data1[,-c(1:3)], 1, mean, na.rm = TRUE))
mean <- M$mean

#boxplot
all_data3 <- all_data1 %>%
  pivot_longer(!c("season","SW_mig","state"), names_to = "No.", values_to = "PD" )

all_data3$season <- factor(all_data3$season, levels=c("breeding", "postbreeding","nonbreeding", "prebreeding"))

ggplot(aes(y = PD, x = season, fill = state), data = all_data3)+
  geom_boxplot()+
  theme_classic() +
  xlab("Season") +
  ylab("Proportion Positive") +
  labs(fill = "State") +
  theme(axis.text.x = element_text(angle = 45, hjust=1))+
  scale_fill_npg()+
  facet_wrap(~SW_mig)

#### Figure 2 ####

#alternate

mig.diet.plot <- migrants_2019 %>%
  group_by(diet2, season) %>%
  dplyr::summarize(mean = mean(SHM),
                   sd = sd(SHM),
                   se = sd(SHM)/sqrt(length(SHM)))

ggplot(mig.diet.plot, aes(x = season, y = mean, group = diet2, col = diet2)) +
  geom_line(position=position_dodge(0.25)) +
  geom_point(position=position_dodge(0.25)) +
  geom_errorbar(aes(ymin=mean-(2*se), ymax=mean+(2*se)), width=.2,
                position=position_dodge(0.25)) +
  theme_classic() +
  xlab("Season") +
  ylab("Mean SHM") +
  labs(col = "Diet") +
  theme(axis.text.x = element_text(angle = 45, hjust=1)) +
facet_wrap(~diet2)

#2b alternate
int.plotb <-all_data4 %>%
  group_by(diet2, season) %>%
  dplyr::summarize(mean = mean(ratio),
                   sd = sd(ratio),
                   se = sd(ratio)/sqrt(length(ratio)))

#int.plot2 <- int.plotb %>% 
#  mutate(group1 = ifelse(SW_mig == "N" | SW_mig == "S", "migratory", "resident"))

ggplot(int.plotb, aes(x = season, y = mean, group = diet2, col = diet2)) +
  geom_line(position=position_dodge(0.25)) +
  geom_point(position=position_dodge(0.25)) +
  geom_errorbar(aes(ymin=mean-(2*se), ymax=mean+(2*se)), width=.2,
                position=position_dodge(0.25)) +
  theme_classic() +
  xlab("Season") +
  ylab("Mean IHM") +
  geom_hline(yintercept=1, linetype="dashed", 
             color = "orange", size=1)+
  labs(col = "Diet") +
  theme(axis.text.x = element_text(angle = 45, hjust=1))+
  scale_fill_npg()



#  separated

SHM_sep <- migrants_2019 %>%
  pivot_longer(cols = c("natural1","impacted1"), names_to = "state", values_to = "rPI")

ggplot(aes(y = rPI, x = season, fill = state), data = SHM_sep)+
  geom_boxplot()+
  theme_classic() +
  xlab("Season") +
  ylab("rPI") +
  labs(fill = "State") +
  theme(axis.text.x = element_text(angle = 45, hjust=1))+
  scale_fill_npg()+
  facet_wrap(~diet2)


#### Figure 3 ####


#alternative visualization for figure 3a:
mig.for.plot <- migrants_2019 %>%
  group_by(sw_foraging, season) %>%
  dplyr::summarize(mean = mean(SHM),
                   sd = sd(SHM),
                   se = sd(SHM)/sqrt(length(SHM)))

ggplot(mig.for.plot, aes(x = season, y = mean, group = sw_foraging, col = sw_foraging)) +
  geom_line(position=position_dodge(0.25)) +
  geom_point(position=position_dodge(0.25)) +
  geom_errorbar(aes(ymin=mean-(se), ymax=mean+(se)), width=.2,
                position=position_dodge(0.25)) +
  theme_classic() +
  xlab("Season") +
  ylab("Mean IHM") +
  labs(col = "Foraging Strategy") +
  theme(axis.text.x = element_text(angle = 45, hjust=1)) +
  facet_wrap(~sw_foraging)


#3b alternate
int.plotb <-all_data4 %>%
  group_by(sw_foraging, season) %>%
  dplyr::summarize(mean = mean(ratio),
                   sd = sd(ratio),
                   se = sd(ratio)/sqrt(length(ratio)))

#int.plot2 <- int.plotb %>% 
#  mutate(group1 = ifelse(SW_mig == "N" | SW_mig == "S", "migratory", "resident"))

ggplot(int.plotb, aes(x = season, y = mean, group = sw_foraging, col = sw_foraging)) +
  geom_line(position=position_dodge(0.25)) +
  geom_point(position=position_dodge(0.25)) +
  geom_errorbar(aes(ymin=mean-(2*se), ymax=mean+(2*se)), width=.2,
                position=position_dodge(0.25)) +
  theme_classic() +
  xlab("Season") +
  ylab("Mean SHM") +
  geom_hline(yintercept=1, linetype="dashed", 
             color = "orange", size=1)+
  labs(col = "Foraging Strategy") +
  theme(axis.text.x = element_text(angle = 45, hjust=1))+
  scale_fill_npg()#+
  #facet_wrap(~sw_foraging)

# Figure 3 ratios separated into natural and impacted #
#discussion?

#rPIs

SHM_sep <- migrants_2019 %>%
  pivot_longer(cols = c("natural1","impacted1"), names_to = "state", values_to = "rPI")

ggplot(aes(y = rPI, x = season, fill = state), data = SHM_sep)+
  geom_boxplot()+
  theme_classic() +
  xlab("Season") +
  ylab("rPI") +
  labs(fill = "State") +
  theme(axis.text.x = element_text(angle = 45, hjust=1))+
  scale_fill_npg()+
  facet_wrap(~sw_foraging)


#PDs

load("data/species_basic.RData")
load("data_outputs/bootstrap_all.RData")

all_data <- left_join(bootstrap_all,species_basic,by="species_code")

all_data1 <- all_data %>%
  group_by(season, sw_foraging, state) %>%
  summarise(across(where(is.numeric), ~sum(.>0)/length(.)))

SD <- transform(all_data1[,-c(1:3)], stdev =apply(all_data1[,-c(1:3)], 1, sd, na.rm = TRUE))
stdev <- SD$stdev
M <- transform(all_data1[,-c(1:3)], mean =apply(all_data1[,-c(1:3)], 1, mean, na.rm = TRUE))
mean <- M$mean

#boxplot
all_data3 <- all_data1 %>%
  pivot_longer(!c("season","sw_foraging","state"), names_to = "No.", values_to = "PD" )

all_data3$season <- factor(all_data3$season, levels=c("breeding", "postbreeding","nonbreeding", "prebreeding"))

ggplot(aes(y = PD, x = season, fill = state), data = all_data3)+
  geom_boxplot()+
  theme_classic() +
  xlab("Season") +
  ylab("SHM") +
  labs(fill = "State") +
  theme(axis.text.x = element_text(angle = 45, hjust=1))+
  scale_fill_npg()+
  facet_wrap(~sw_foraging)

#### LandCover Map ####

#land cover data

library(raster)
library(ncdf4)
library(rgdal)


landcover <- raster("data/LandCover.tif")
unique(landcover)
temp <- raster("data/srd_raster_template.tif")
#plot(landcover)
#landcover_stack <- stack("data files/LandCover.tif")

#landcover
#landcover$values

#Nov 5 2020

lc_3k <- read.csv("ebird3k_df_out.csv")
lc_8k <- read.csv("ebird8k_df_out.csv")

#some numbers are still weird - try it out anyways
#need land cover class names

####plotting maps for appendix####
writeRaster(bre_abd_mean, filename = "D:/Allison/Big_data/Ch-2 landcover/magwar_bre_abd.tif")
bre_abd_mean <- raster("D:/Allison/Big_data/Ch-2 landcover/magwar_bre_abd.tif")

x1 <- raster("D:/Allison/Big_data/Ch-2 landcover/base_map_lambert.tif")
magwar_abd <- projectRaster(bre_abd_mean,x1)
writeRaster(magwar_abd, filename = "D:/Allison/Big_data/Ch-2 landcover/magwar_bre_abd_lambert.tif")

mod_pland_ebd <- raster("D:/Allison/Big_data/Ch-2 landcover/modified_cover_ebd_nobarren.tif")
mod_lc <- projectRaster(mod_pland_ebd,x1)
plot(mod_lc)

lc_spdf <- as(mod_pland_ebd, "SpatialPixelsDataFrame")
lc_df <- as.data.frame(lc_spdf)
colnames(lc_df) <- c("value", "x", "y")

#making nice raster plots in ggplot:https://stackoverflow.com/questions/33227182/how-to-set-use-ggplot2-to-map-a-raster
ggplot() +  
  geom_tile(data=lc_df, aes(x=x, y=y, fill=value), alpha=0.8) + 
  scale_fill_viridis() +
  coord_equal() +
  theme_classic()


#relative abundance
plot(x1, legend=F, col = "black")
plot(magwar_abd, legend = FALSE, add=T)

#lc
plot(x1, legend=F, col = "black")
plot(mod_lc, legend = FALSE, add=T)







