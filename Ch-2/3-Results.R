#Results GLMM

library(lme4)
library(nlme)
library(car)
library(tidyverse)
library(lmerTest)
library(MuMIn)
library(effectsize)
library(emmeans)
library(lattice)
library(ggpubr)
library(rstatix)
library(ggsci)
library(ggsignif)
library(piecewiseSEM)


#### Figure 2 ####

load("data_outputs/rPI_migrants_2019.RData") #now without barren
load("data_outputs/rPI_migrants_alt.RData") #noFCM

##migratory strategy - rPIs

#lme4
mig.stat.lmer <- lmer(modified_rPI ~ season*SW_mig + (1 | species_code), data = migrants_2019)
summary(mig.stat.lmer)
AIC(mig.stat.lmer)
anova(mig.stat.lmer)
rsquared(mig.stat.lmer)

#assumptions
#transformation did little to help any assumptions
bxp <- ggboxplot(
  migrants_2019, x = "season", y = "modified_rPI",
  color = "SW_mig", palette = "jco"
)
bxp

outliers.mig <- migrants_2019 %>%
  group_by(season, SW_mig) %>%
  identify_outliers(modified_rPI)


#normality
#using qq since sample size relatively large
norm <- ggqqplot(migrants_2019, "modified_rPI", ggtheme = theme_bw()) +
  facet_grid(season ~ SW_mig)

#homogeneity of variances
migrants_2019 %>%
  group_by(season) %>%
  levene_test(modified_rPI ~ SW_mig)
levene_test(migrants_2019, modified_rPI~SW_mig*season)

var <- ggplot(migrants_2019, aes(y=modified_rPI, x=SW_mig))+
  geom_point() +
  facet_wrap(~season)

#homogeneity of covariances
box_m(migrants_2019[, "modified_rPI", drop = FALSE], migrants_2019$SW_mig)

plotlist <- list(bxp,norm,var)

mig_diagnostics <- ggarrange(plotlist=plotlist,
                     common.legend = T,
                     ncol=1,
                     nrow=3,
                     legend="none",
                     align="hv")

png("appendix_plots/mig_rPIm_diagnostics_nobarren.png", height = 12, width = 8, units = "in",res=300)
mig_diagnostics
dev.off()
#can also load noFCM results to examine those assumptions, but pretty similar


##post-hoc comparisons

mig.tukey.table <- emmeans(mig.stat.lmer, list(pairwise ~ SW_mig*season), adjust = "holm")
summary(mig.tukey.table)
write.table(mig.tukey.table$`pairwise differences of SW_mig, season`, file = "fig_outputs/rPI-mig-holm.txt", sep = ",", quote = FALSE, row.names = F)
write.table(mig.tukey.table$`pairwise differences of SW_mig, season`, file = "fig_outputs/rPI-mig-holm_noFCM.txt", sep = ",", quote = FALSE, row.names = F)

#marginal and conditional Rsq, AIC
library(piecewiseSEM)
rsquared(mig.stat.lmer)
AIC(mig.stat.lmer)


#### fig2a ####

migrants_2019$season <- factor(migrants_2019$season, levels=c("breeding", "postbreeding","nonbreeding", "prebreeding"))
migrants_2019$SW_mig <- factor(migrants_2019$SW_mig, levels=c("N","S","NR","R"))


mypal <- pal_npg("nrc", alpha = 1)(4)
int.plota <- migrants_2019 %>%
  group_by(SW_mig, season) %>%
  dplyr::summarize(mean = mean(modified_rPI),
                   sd = sd(modified_rPI),
                   se = sd(modified_rPI)/sqrt(length(modified_rPI)))


fig2a <- ggplot(int.plota, aes(x = season, y = mean, group = SW_mig, col = SW_mig)) +
geom_line(aes(group=SW_mig),position=position_dodge(0.6)) +
  geom_point(position=position_dodge(0.6), size=3, pch=18) +
  geom_errorbar(aes(ymin=mean-(se), ymax=mean+(se)), width=1,
                position=position_dodge(0.6)) +
theme_classic(base_size = 22, base_family = "serif") +
  xlab("Season") +
  ylab("Mean rPI") +
  theme(axis.text.x = element_text(angle = 45, hjust=1))+
  scale_color_manual(name = "Migratory Strategy",
                    labels = c("Neotropical Migrants","Short-Distance Migrants","Neotropical Residents","North American Residents"),
                    values=mypal)+
  geom_vline(xintercept=c(1.5,2.5,3.5),color="grey",alpha=0.5)
  
fig2a

#### MHA ####

load("data_outputs/availability_data_nobarren_updated.RData")
load("data_outputs/availability_data_noFCM_updated.RData")

availability1 <- availability %>%
  mutate(sqrt_ava = sqrt(weighted_abd),
         log_ava = log(weighted_abd+1))

#lme4
mig.exp.lmer <- lmer(sqrt_ava ~ season*SW_mig + (1 | species_code), data = availability1)
summary(mig.exp.lmer)
anova(mig.exp.lmer)
rsquared(mig.exp.lmer)
AIC(mig.exp.lmer)


#assumptions
bxp <- ggboxplot(
  availability1, x = "season", y = "sqrt_ava",
  color = "SW_mig", palette = "jco"
)
bxp

outliers.mig <- availability1 %>%
  group_by(season, SW_mig) %>%
  identify_outliers(sqrt_ava)

#normality
#using qq since sample size relatively large
norm <- ggqqplot(availability1, "sqrt_ava", ggtheme = theme_bw()) +
  facet_grid(season ~ SW_mig)

norm

#homogeneity of variances
availability1 %>%
  group_by(season) %>%
  levene_test(log_ava ~ SW_mig)
levene_test(availability1, log_ava~SW_mig*season)
var <- ggplot(availability1, aes(y=sqrt_ava, x=SW_mig))+
  geom_point() +
  facet_wrap(~season)
var

#homogeneity of covariances
box_m(availability1[, "sqrt_ava", drop = FALSE], availability1$SW_mig)

plotlist <- list(bxp,norm,var)

mig_diagnostics <- ggarrange(plotlist=plotlist,
                             common.legend = T,
                             ncol=1,
                             nrow=3,
                             legend="none",
                             align="hv")

png("appendix_plots/mig_MHA_diagnostics_nobarren.png", height = 12, width = 8, units = "in",res=300)
mig_diagnostics
dev.off()

##post-hoc comparisons

mig.exp.tukey.table <- emmeans(mig.exp.lmer, list(pairwise ~ SW_mig*season), adjust = "holm")
summary(mig.exp.tukey.table)
write.table(mig.exp.tukey.table$`pairwise differences of SW_mig, season`, file = "fig_outputs/exp-mig-holm.txt", sep = ",", quote = FALSE, row.names = F)
write.table(mig.exp.tukey.table$`pairwise differences of SW_mig, season`, file = "fig_outputs/exp-mig-holm_noFCM.txt", sep = ",", quote = FALSE, row.names = F)



#### fig 2b ####

availability$season <- factor(availability$season, levels=c("breeding", "postbreeding","nonbreeding", "prebreeding"))
availability$SW_mig <- factor(availability$SW_mig, levels=c("N","S","NR","R"))

mypal <- pal_npg("nrc", alpha = 1)(4)
int.plot.b <- availability %>%
  group_by(SW_mig, season) %>%
  dplyr::summarize(mean = mean(weighted_abd),
                   sd = sd(weighted_abd),
                   se = sd(weighted_abd)/sqrt(length(weighted_abd)))

fig2b <- ggplot(int.plot.b, aes(x = season, y = mean, group = SW_mig, col = SW_mig)) +
  geom_line(aes(group=SW_mig),position=position_dodge(0.6)) +
  geom_point(position=position_dodge(0.6), size=3, pch=18) +
  geom_errorbar(aes(ymin=mean-(se), ymax=mean+(se)), width=1,
                position=position_dodge(0.6)) +
  theme_classic(base_size = 22, base_family = "serif") +
  xlab("Season") +
  ylab("Mean MHA") +
  theme(axis.text.x = element_text(angle = 45, hjust=1))+
  scale_color_manual(name = "Migratory Strategy",
                     labels = c("Neotropical Migrants","Short-Distance Migrants","Neotropical Residents","North American Residents"),
                     values=mypal)+
  geom_vline(xintercept=c(1.5,2.5,3.5),color="grey",alpha=0.5)

fig2b

#### SHM ####

load("data/species_basic.RData")
load("data_outputs/bootstrap_all_nobarren.RData")
load("data_outputs/bootstrap_all_noFCM.RData")

all_data <- left_join(bootstrap_all,species_basic,by="species_code")

#summing natural and modified land cover PDs for each mig strat in each season
#and calculating the proportion positive for each
#yields 1000 proportions for each grouping
all_data1 <- all_data %>%
  group_by(season, SW_mig, state) %>%
  summarise(across(where(is.numeric), ~sum(.>0)/length(.)))

#long format (bootstrap reps)
all_data3_mig <- all_data1 %>%
  pivot_longer(!c("season","SW_mig","state"), names_to = "No.", values_to = "Proportion_Positive" )

#wide format to separate natural and modified values to calculate ratio
all_data4_mig <- all_data3_mig %>%
  pivot_wider(names_from = "state", values_from = "Proportion_Positive")

#calculate SHM - proportion positive natural relative to proportion positive modified
all_data4_mig <- all_data4_mig %>%
  mutate(SHM = (natural+1)/(modified+1))

all_data4_mig$season <- factor(all_data4_mig$season, levels=c("breeding", "postbreeding","nonbreeding", "prebreeding"))
all_data4_mig$SW_mig <- factor(all_data4_mig$SW_mig, levels=c("N","S","NR","R"))
save(all_data4_mig, file = "data_outputs/all_data4_mig_nobarren.RData")
save(all_data4_mig, file = "data_outputs/all_data4_mig_noFCM.RData")
load("data_outputs/all_data4_mig_nobarren.RData")
load("data_outputs/all_data4_mig_noFCM.RData")

pd_aov <- aov(SHM~season*SW_mig, data = all_data4_mig)
pd_lm <- lm(SHM~season*SW_mig, data = all_data4_mig)
summary(pd_aov)
summary(pd_lm)
library(piecewiseSEM)
AIC(pd_lm)
anova(pd_lm)

#assumptions
par(mfrow=c(2,2))
plot(pd_lm)

#pairwise
mig.tukey.pd.table <- emmeans(pd_aov, list(pairwise ~ SW_mig*season), adjust = "holm")
summary(mig.tukey.pd.table)
write.table(mig.tukey.pd.table$`pairwise differences of SW_mig, season`, file = "fig_outputs/mig-holm-pd.txt", sep = ",", quote = FALSE, row.names = F)
write.table(mig.tukey.pd.table$`pairwise differences of SW_mig, season`, file = "fig_outputs/mig-holm-pd_noFCM.txt", sep = ",", quote = FALSE, row.names = F)


#### fig2c ####

mypal <- pal_npg("nrc", alpha = 1)(4)
int.plotc <- all_data4_mig %>%
  group_by(SW_mig, season) %>%
  dplyr::summarize(mean = mean(SHM),
                   sd = sd(SHM),
                   se = sd(SHM)/sqrt(length(SHM)))

fig2c <- ggplot(int.plotc, aes(x = season, y = mean, group = SW_mig, col = SW_mig)) +
  geom_line(aes(group=SW_mig),position=position_dodge(0.6)) +
  geom_point(position=position_dodge(0.6), size=3, pch=18) +
  geom_errorbar(aes(ymin=mean-(se), ymax=mean+(se)), width=1,
                position=position_dodge(0.6)) +
  theme_classic(base_size = 22, base_family = "serif") +
  xlab("Season") +
  ylab("Mean SHM") +
  theme(axis.text.x = element_text(angle = 45, hjust=1))+
  scale_color_manual(name = "Migratory Strategy",
                     labels = c("Neotropical Migrants","Short-Distance Migrants","Neotropical Residents","North American Residents"),
                     values=mypal)+
  geom_vline(xintercept=c(1.5,2.5,3.5),color="grey",alpha=0.5)




fig2a
fig2b
fig2c
plotlist1 <- list(fig2a,fig2b,fig2c)

figure2 <- ggarrange(plotlist=plotlist1,
                     common.legend = T,
                     ncol=2,
                     nrow=2,
                     legend="none",
                     align="hv")

png("fig_outputs/figure2_noFCM_updated.png", height = 11.5, width = 8, units = "in",res=300)
png("fig_outputs/figure2_nobarren_updated.png", height = 9, width = 8, units = "in",res=300)
figure2
dev.off()


#### diet ####
load("data_outputs/rPI_migrants_2019.RData")
load("data_outputs/rPI_migrants_alt.RData")


#lme4
mig.diet.lmer <- lmer(modified_rPI ~ season*diet2 + (1 | species_code), data = migrants_2019)
summary(mig.diet.lmer)
anova(mig.diet.lmer)
rsquared(mig.diet.lmer)
AIC(mig.diet.lmer)

#assumptions

#boxplot 
bxp <- ggboxplot(
  migrants_2019, x = "season", y = "modified_rPI",
  color = "diet2", palette = "jco"
)
bxp

outliers.diet <- migrants_2019 %>%
  group_by(season, diet2) %>%
  identify_outliers(modified_rPI) 

#normality
#using qq since sample size relatively large
norm <- ggqqplot(migrants_2019, "modified_rPI", ggtheme = theme_bw()) +
  facet_grid(season ~ diet2)

#homogeneity of variances
migrants_2019 %>%
  group_by(season) %>%
  levene_test(modified_rPI ~ diet2)

var <- ggplot(migrants_2019, aes(y=modified_rPI, x=diet2))+
  geom_point() +
  facet_wrap(~season)
var

#homogeneity of covariances
box_m(migrants_2019[, "modified_rPI", drop = FALSE], migrants_2019$diet2)

#### post-hoc comparisons ####
mig.diet.tukey.table <- emmeans(mig.diet.lmer, list(pairwise ~ diet2*season), adjust = "holm")
summary(mig.diet.tukey.table)
write.table(mig.diet.tukey.table$`pairwise differences of diet2, season`, file = "fig_outputs/rPI_diet_holm.txt", sep = ",", quote = FALSE, row.names = F)
write.table(mig.diet.tukey.table$`pairwise differences of diet2, season`, file = "fig_outputs/rPI_diet_holm_noFCM.txt", sep = ",", quote = FALSE, row.names = F)

plotlist <- list(bxp,norm,var)

diet_diagnostics <- ggarrange(plotlist=plotlist,
                             common.legend = T,
                             ncol=1,
                             nrow=3,
                             legend="none")

png("appendix_plots/diet_rPIm_diagnostics_nobarren.png", height = 12, width = 8, units = "in",res=300)
diet_diagnostics
dev.off()

#### fig3a ####

mypal <- pal_npg("nrc", alpha = 1)(5)
mig.diet.plot <- migrants_2019 %>%
  group_by(diet2, season) %>%
  dplyr::summarize(mean = mean(modified_rPI),
                   sd = sd(modified_rPI),
                   se = sd(modified_rPI)/sqrt(length(modified_rPI)))

fig3a <- ggplot(mig.diet.plot, aes(x = season, y = mean, group = diet2, col = diet2)) +
  geom_line(aes(group=diet2),position=position_dodge(0.6)) +
  geom_point(position=position_dodge(0.6), size=3, pch=18) +
  geom_errorbar(aes(ymin=mean-(se), ymax=mean+(se)), width=1,
                position=position_dodge(0.6)) +
  theme_classic(base_size = 22, base_family = "serif") +
  xlab("Season") +
  ylab("Mean rPI") +
  theme(axis.text.x = element_text(angle = 45, hjust=1))+
  scale_color_manual(name = "Diet",
                     labels = c("Carnivore","Frugivore","Granivore","Insectivore","Omnivore"),
                     values=mypal)+
  geom_vline(xintercept=c(1.5,2.5,3.5),color="grey",alpha=0.5)


#MHA

load("data_outputs/availability_data_nobarren_updated.RData")
load("data_outputs/availability_data_noFCM_updated.RData")

availability1 <- availability %>%
  mutate(sqrt_ava = sqrt(weighted_abd),
         log_ava = log(weighted_abd))

#lme4
diet.exp.lmer <- lmer(sqrt_ava ~ season*diet2 + (1 | species_code), data = availability1)
summary(diet.exp.lmer)
anova(diet.exp.lmer)
AIC(diet.exp.lmer)
rsquared(diet.exp.lmer)

#assumptions

bxp <- ggboxplot(
  availability1, x = "season", y = "sqrt_exp",
  color = "diet2", palette = "jco"
)
bxp

outliers.diet <- availability1 %>%
  group_by(season, diet2) %>%
  identify_outliers(sqrt_ava)

#normality
#using qq since sample size relatively large
nrom <- ggqqplot(availability1, "sqrt_ava", ggtheme = theme_bw()) +
  facet_grid(season ~ diet2)

#homogeneity of variances
availability1 %>%
  group_by(season) %>%
  levene_test(sqrt_ava ~ diet2)
levene_test(availability1, sqrt_ava~diet2*season)
 
var <- ggplot(availability1, aes(y=sqrt_exp, x=diet2))+
  geom_point() +
  facet_wrap(~season)

#homogeneity of covariances
box_m(availability1[, "sqrt_ava", drop = FALSE], availability1$diet2)

plotlist <- list(bxp,norm,var)

diet_diagnostics <- ggarrange(plotlist=plotlist,
                              common.legend = T,
                              ncol=1,
                              nrow=3,
                              legend="none",
                              align="hv")

png("appendix_plots/diet_MHA_diagnostics_nobarren.png", height = 12, width = 8, units = "in",res=300)
diet_diagnostics
dev.off()


##post-hoc comparisons

diet.exp.tukey.table <- emmeans(diet.exp.lmer, list(pairwise ~ diet2*season), adjust = "holm")
summary(diet.exp.tukey.table)
write.table(diet.exp.tukey.table$`pairwise differences of diet2, season`, file = "fig_outputs/exp-diet-holm.txt", sep = ",", quote = FALSE, row.names = F)
write.table(diet.exp.tukey.table$`pairwise differences of diet2, season`, file = "fig_outputs/exp-diet-holm_noFCM.txt", sep = ",", quote = FALSE, row.names = F)


#### fig 3b ####

mypal <- pal_npg("nrc", alpha = 1)(5)
int.plot.ava <- availability %>%
  group_by(diet2, season) %>%
  dplyr::summarize(mean = mean(weighted_abd),
                   sd = sd(weighted_abd),
                   se = sd(weighted_abd)/sqrt(length(weighted_abd)))

fig3b <- ggplot(int.plot.ava, aes(x = season, y = mean, group = diet2, col = diet2)) +
  geom_line(aes(group=diet2),position=position_dodge(0.6)) +
  geom_point(position=position_dodge(0.6), size=3, pch=18) +
  geom_errorbar(aes(ymin=mean-(se), ymax=mean+(se)), width=1,
                position=position_dodge(0.6)) +
  theme_classic(base_size = 22, base_family = "serif") +
  xlab("Season") +
  ylab("Mean MHA") +
  theme(axis.text.x = element_text(angle = 45, hjust=1))+
  scale_color_manual(name = "Diet",
                     labels = c("Carnivore","Frugivore","Granivore","Insectivore","Omnivore"),
                     values=mypal)+
  geom_vline(xintercept=c(1.5,2.5,3.5),color="grey",alpha=0.5)

fig3b


#SHM

load("data/species_basic.RData")
load("data_outputs/bootstrap_all_nobarren.RData")
load("data_outputs/bootstrap_all_noFCM.RData")

all_data <- left_join(bootstrap_all,species_basic,by="species_code")

all_data1 <- all_data %>%
  group_by(season, diet2, state) %>%
  summarise(across(where(is.numeric), ~sum(.>0)/length(.)))

all_data3_diet <- all_data1 %>%
  pivot_longer(!c("season","diet2","state"), names_to = "No.", values_to = "PD" )

all_data4_diet <- all_data3_diet %>%
  pivot_wider(names_from = "state", values_from = "PD")

all_data4_diet <- all_data4_diet %>%
  mutate(SHM = (natural+1)/(modified+1))

all_data4_diet$season <- factor(all_data4_diet$season, levels=c("breeding", "postbreeding","nonbreeding", "prebreeding"))
save(all_data4_diet, file = "data_outputs/all_data4_diet_noFCM.RData")
load("data_outputs/all_data4_diet_nobarren.RData")
load("data_outputs/all_data4_diet_noFCM.RData")

pd_aov <- aov(SHM~season*diet2, data = all_data4_diet)
pd_lm <- lm(SHM~season*diet2, data = all_data4_diet)
summary(pd_aov)
summary(pd_lm)
anova(pd_lm)
AIC(pd_lm)

#assumptions
par(mfrow=c(2,2))
plot(pd_lm)

diet.tukey.pd.table <- emmeans(pd_aov, list(pairwise ~ diet2*season), adjust = "holm")
summary(diet.tukey.pd.table)
write.table(diet.tukey.pd.table$`pairwise differences of diet2, season`, file = "fig_outputs/diet-holm-pd.txt", sep = ",", quote = FALSE, row.names = F)
write.table(diet.tukey.pd.table$`pairwise differences of diet2, season`, file = "fig_outputs/diet-holm-pd_noFCM.txt", sep = ",", quote = FALSE, row.names = F)


#### fig3c ####

mypal <- pal_npg("nrc", alpha = 1)(5)
int.plot.diet.b <- all_data4_diet %>%
  group_by(diet2, season) %>%
  dplyr::summarize(mean = mean(SHM),
                   sd = sd(SHM),
                   se = sd(SHM)/sqrt(length(SHM)))

fig3c <- ggplot(int.plot.diet.b, aes(x = season, y = mean, group = diet2, col = diet2)) +
  geom_line(aes(group=diet2),position=position_dodge(0.6)) +
  geom_point(position=position_dodge(0.6), size=3, pch=18) +
  geom_errorbar(aes(ymin=mean-(se), ymax=mean+(se)), width=1,
                position=position_dodge(0.6)) +
  theme_classic(base_size = 22, base_family = "serif") +
  xlab("Season") +
  ylab("Mean SHM") +
  theme(axis.text.x = element_text(angle = 45, hjust=1))+
  scale_color_manual(name = "Diet",
                     labels = c("Carnivore","Frugivore","Granivore","Insectivore","Omnivore"),
                     values=mypal)+
  geom_vline(xintercept=c(1.5,2.5,3.5),color="grey",alpha=0.5)


fig3a
fig3b
fig3c
plotlist2 <- list(fig3a,fig3b,fig3c)

figure3 <- ggarrange(plotlist=plotlist2,
                     common.legend = T,
                     ncol=2,
                     nrow=2,
                     legend="none",
                     align="hv")

png("fig_outputs/figure3_noFCM_updated.png", height = 9, width = 8, units = "in",res=300)
png("fig_outputs/figure3_nobarren_updated.png", height = 9, width = 8, units = "in",res=300)
figure3
dev.off()




#### foraging ####
load("data_outputs/rPI_migrants_2019.RData")
load("data_outputs/rPI_migrants_alt.RData")

#lme4
mig.for.lmer <- lmer(modified_rPI ~ season*sw_foraging + (1 | species_code), data = migrants_2019)
summary(mig.for.lmer)
anova(mig.for.lmer)
AIC(mig.for.lmer)
rsquared(mig.for.lmer)

#assumptions

bxp <- ggboxplot(
  migrants_2019, x = "season", y = "modified_rPI",
  color = "sw_foraging", palette = "jco"
)
bxp

outliers.mig <- migrants_2019 %>%
  group_by(season, sw_foraging) %>%
  identify_outliers(modified_rPI)#31 outliers

#normality
#using qq since sample size relatively large
norm <- ggqqplot(migrants_2019, "modified_rPI", ggtheme = theme_bw()) +
  facet_grid(season ~ sw_foraging)

#homogeneity of variances
migrants_2019 %>%
  group_by(season) %>%
  levene_test(modified_rPI ~ sw_foraging)

var <- ggplot(migrants_2019, aes(y=modified_rPI, x=sw_foraging))+
  geom_point() +
  facet_wrap(~season)

#homogeneity of covariances
box_m(migrants_2019[, "modified_rPI", drop = FALSE], migrants_2019$sw_foraging)

plotlist <- list(bxp,norm,var)

for_diagnostics <- ggarrange(plotlist=plotlist,
                              common.legend = T,
                              ncol=1,
                              nrow=3,
                              legend="none")

png("appendix_plots/for_rPIm_diagnostics_nobarren.png", height = 12, width = 8, units = "in",res=300)
for_diagnostics
dev.off()


##post-hoc comparisons
for.holm.rpi <- emmeans(mig.for.lmer, list(pairwise ~ sw_foraging*season), adjust = "holm")
summary(for.holm.rpi)
write.table(for.holm.rpi$`pairwise differences of sw_foraging, season`, file = "fig_outputs/rPI-for-holm.txt", sep = ",", quote = FALSE, row.names = F)
write.table(for.holm.rpi$`pairwise differences of sw_foraging, season`, file = "fig_outputs/rPI-for-holm_noFCM.txt", sep = ",", quote = FALSE, row.names = F)


#vis

#### fig4a ####

mypal <- pal_npg("nrc", alpha = 1)(6)
mig.for.plot <- migrants_2019 %>%
  group_by(sw_foraging, season) %>%
  dplyr::summarize(mean = mean(modified_rPI),
                   sd = sd(modified_rPI),
                   se = sd(modified_rPI)/sqrt(length(modified_rPI)))

fig4a <- ggplot(mig.for.plot, aes(x = season, y = mean, group = sw_foraging, col = sw_foraging)) +
  geom_line(aes(group=sw_foraging),position=position_dodge(0.6)) +
  geom_point(position=position_dodge(0.6), size=3, pch=18) +
  geom_errorbar(aes(ymin=mean-(se), ymax=mean+(se)), width=1,
                position=position_dodge(0.6)) +
  theme_classic(base_size = 22, base_family = "serif") +
  xlab("Season") +
  ylab("Mean rPI") +
  theme(axis.text.x = element_text(angle = 45, hjust=1))+
  scale_color_manual(name = "Foraging Strategy",
                     labels = c("Aerial Forager","Aerial Hawker","Bark Forager",
                                "Foliage Gleaner","Ground Forager","Ground Hawker"),
                     values=mypal)+
  geom_vline(xintercept=c(1.5,2.5,3.5),color="grey",alpha=0.5)


#MHA

load("data_outputs/availability_data_nobarren_updated.RData")
load("data_outputs/availability_data_noFCM_updated.RData")

availability1 <- availability %>%
  mutate(sqrt_ava = sqrt(weighted_abd),
         log_ava = log(weighted_abd))

#lme4
for.exp.lmer <- lmer(sqrt_ava ~ season*sw_foraging + (1 | species_code), data = availability1)
summary(for.exp.lmer)
anova(for.exp.lmer)
AIC(for.exp.lmer)
rsquared(for.exp.lmer)


#assumptions

bxp <- ggboxplot(
  availability1, x = "season", y = "sqrt_exp",
  color = "sw_foraging", palette = "jco"
)
bxp

outliers.for <- availability1 %>%
  group_by(season, sw_foraging) %>%
  identify_outliers(sqrt_exp)

#normality
#using qq since sample size relatively large
norm <- ggqqplot(availability1, "sqrt_exp", ggtheme = theme_bw()) +
  facet_grid(season ~ sw_foraging)

#homogeneity of variances
availability1 %>%
  group_by(season) %>%
  levene_test(sqrt_exp ~ sw_foraging)
levene_test(availability1, sqrt_exp~sw_foraging*season)

var <-ggplot(availability1, aes(y=sqrt_exp, x=sw_foraging))+
  geom_point() +
  facet_wrap(~season)

#homogeneity of covariances
box_m(availability1[, "sqrt_exp", drop = FALSE], availability1$sw_foraging)

plotlist <- list(bxp,norm,var)

for_diagnostics <- ggarrange(plotlist=plotlist,
                             common.legend = T,
                             ncol=1,
                             nrow=3,
                             legend="none",
                             align="hv")

png("appendix_plots/for_MHA_diagnostics_nobarren.png", height = 12, width = 8, units = "in",res=300)
for_diagnostics
dev.off()


##post-hoc comparisons

for.exp.tukey.table <- emmeans(for.exp.lmer, list(pairwise ~ season), adjust = "holm")
for.exp.tukey.table <- emmeans(for.exp.lmer, list(pairwise ~ sw_foraging), adjust = "holm")
summary(for.exp.tukey.table)
write.table(for.exp.tukey.table$`pairwise differences of season`, file = "fig_outputs/exp-for-holm.txt", sep = ",", quote = FALSE, row.names = F)
write.table(for.exp.tukey.table$`pairwise differences of season`, file = "fig_outputs/exp-for-holm_noFCM_season.txt", sep = ",", quote = FALSE, row.names = F)
write.table(for.exp.tukey.table$`pairwise differences of sw_foraging`, file = "fig_outputs/exp-for-holm_noFCM_foraging.txt", sep = ",", quote = FALSE, row.names = F)

#### fig 4b ####

mypal <- pal_npg("nrc", alpha = 1)(6)
int.plot.ava <- availability %>%
  group_by(sw_foraging, season) %>%
  dplyr::summarize(mean = mean(weighted_abd),
                   sd = sd(weighted_abd),
                   se = sd(weighted_abd)/sqrt(length(weighted_abd)))

fig4b <- ggplot(int.plot.ava, aes(x = season, y = mean, group = sw_foraging, col = sw_foraging)) +
  geom_line(aes(group=sw_foraging),position=position_dodge(0.6)) +
  geom_point(position=position_dodge(0.6), size=3, pch=18) +
  geom_errorbar(aes(ymin=mean-(se), ymax=mean+(se)), width=1,
                position=position_dodge(0.6)) +
  theme_classic(base_size = 22, base_family = "serif") +
  xlab("Season") +
  ylab("Mean MHA") +
  theme(axis.text.x = element_text(angle = 45, hjust=1))+
  scale_color_manual(name = "Foraging Strategy",
                     labels = c("Aerial Forager","Aerial Hawker","Bark Forager","Foliage Gleaner","Ground Forager","Ground Hawker"),
                     values=mypal)+
  geom_vline(xintercept=c(1.5,2.5,3.5),color="grey",alpha=0.5)

fig4b


#PDs

load("data/species_basic.RData")
load("data_outputs/bootstrap_all_nobarren.RData")
load("data_outputs/bootstrap_all_noFCM.RData")

all_data <- left_join(bootstrap_all,species_basic,by="species_code")

all_data1 <- all_data %>%
  group_by(season, sw_foraging, state) %>%
  summarise(across(where(is.numeric), ~sum(.>0)/length(.)))

all_data3_for <- all_data1 %>%
  pivot_longer(!c("season","sw_foraging","state"), names_to = "No.", values_to = "PD" )

all_data4_for <- all_data3_for %>%
  pivot_wider(names_from = "state", values_from = "PD")

all_data4_for <- all_data4_for %>%
  mutate(SHM = (natural+1)/(modified+1))

all_data4_for$season <- factor(all_data4_for$season, levels=c("breeding", "postbreeding","nonbreeding", "prebreeding"))
save(all_data4_for, file = "data_outputs/all_data4_for_nobarren.RData")
save(all_data4_for, file = "data_outputs/all_data4_for_noFCM.RData")
load("data_outputs/all_data4_for_nobarren.RData")
load("data_outputs/all_data4_for_noFCM.RData")

pd_aov <- aov(SHM~season*sw_foraging, data = all_data4_for)
pd_lm <- lm(SHM~season*sw_foraging, data = all_data4_for)
summary(pd_aov)
summary(pd_lm)
AIC(pd_lm)
anova(pd_lm)

#assumptions
par(mfrow=c(2,2))
plot(pd_lm)

for.holm.pd.table <- emmeans(pd_aov, list(pairwise ~ sw_foraging*season), adjust = "holm")
summary(for.holm.pd.table)
write.table(for.holm.pd.table$`pairwise differences of sw_foraging, season`, file = "fig_outputs/for-holm-pd.txt", sep = ",", quote = FALSE, row.names = F)
write.table(for.holm.pd.table$`pairwise differences of sw_foraging, season`, file = "fig_outputs/for-holm-pd_noFCM.txt", sep = ",", quote = FALSE, row.names = F)


#### fig4c ####

mypal <- pal_npg("nrc", alpha = 1)(6)
int.plot.for.b <- all_data4_for %>%
  group_by(sw_foraging, season) %>%
  dplyr::summarize(mean = mean(SHM),
                   sd = sd(SHM),
                   se = sd(SHM)/sqrt(length(SHM)))

fig4c <- ggplot(int.plot.for.b, aes(x = season, y = mean, group = sw_foraging, col = sw_foraging)) +
  geom_line(aes(group=sw_foraging),position=position_dodge(0.6)) +
  geom_point(position=position_dodge(0.6), size=3, pch=18) +
  geom_errorbar(aes(ymin=mean-(se), ymax=mean+(se)), width=1,
                position=position_dodge(0.6)) +
  theme_classic(base_size = 22, base_family = "serif") +
  xlab("Season") +
  ylab("Mean SHM") +
  theme(axis.text.x = element_text(angle = 45, hjust=1))+
  scale_color_manual(name = "Foraging Strategy",
                     labels = c("Aerial Forager","Aerial Hawker","Bark Forager",
                                "Foliage Gleaner","Ground Forager","Ground Hawker"),
                     values=mypal)+
  geom_vline(xintercept=c(1.5,2.5,3.5),color="grey",alpha=0.5)


fig4a
fig4b
fig4c
plotlist3 <- list(fig4a,fig4b,fig4c)

figure4 <- ggarrange(plotlist=plotlist3,
                     common.legend = T,
                     ncol=2,
                     nrow=2,
                     legend="none",
                     align="hv")

png("fig_outputs/figure4_noFCM_updated.png", height = 9, width = 8, units = "in",res=300)
png("fig_outputs/figure4_nobarren_updated.png", height = 9, width = 8, units = "in",res=300)
figure4
dev.off()





