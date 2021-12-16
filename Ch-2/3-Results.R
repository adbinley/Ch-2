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


#### Figure 1 ####

load("data_outputs/IHM_migrants_2019.RData")

##migratory strategy - rPIs

#lme4
mig.stat.lmer <- lmer(IHM ~ season*SW_mig + (1 | species_code), data = migrants_2019)
summary(mig.stat.lmer)
AIC(mig.stat.lmer)
anova(mig.stat.lmer)

#assumptions
#boxplot suggests outliers
#log transformation did little to help any assumptions
bxp <- ggboxplot(
  migrants_2019, x = "season", y = "IHM",
  color = "SW_mig", palette = "jco"
)
bxp

outliers.mig <- migrants_2019 %>%
  group_by(season, SW_mig) %>%
  identify_outliers(IHM)
#9 outliers

#normality
#using qq since sample size relatively large
ggqqplot(migrants_2019, "IHM", ggtheme = theme_bw()) +
  facet_grid(season ~ SW_mig)
#looks good

#homogeneity of variances
migrants_2019 %>%
  group_by(season) %>%
  levene_test(IHM ~ SW_mig)
levene_test(migrants_2019, IHM~SW_mig*season)
#nonbreeding is significant
ggplot(migrants_2019, aes(y=IHM, x=SW_mig))+
  geom_point() +
  facet_wrap(~season)
#definitely greater variance in neotropical migrants, but not too bad

#homogeneity of covariances
box_m(migrants_2019[, "IHM", drop = FALSE], migrants_2019$SW_mig)
#assumption violated, but close - difficult to rememdy, should be relatively robust

##post-hoc comparisons

mig.tukey.table <- emmeans(mig.stat.lmer, list(pairwise ~ SW_mig*season), adjust = "holm")
summary(mig.tukey.table)
write.table(mig.tukey.table$`pairwise differences of SW_mig, season`, file = "fig_outputs/mig-holm.txt", sep = ",", quote = FALSE, row.names = F)

#marginal and conditional Rsq, AIC
#install.packages("piecewiseSEM")
library(piecewiseSEM)
rsquared(mig.stat.lmer)
AIC(mig.stat.lmer)

#boxplot

#df_mean <- migrants_2019 %>% 
#  group_by(SW_mig, season) %>% 
#  dplyr::summarize(average = mean(SHM)) %>%
#  ungroup()

#mypal <- pal_npg("nrc", alpha = 1)(4)

#### fig1a ####

#migrants_2019$season <- factor(migrants_2019$season, levels=c("breeding", "postbreeding","nonbreeding", "prebreeding"))
migrants_2019$SW_mig <- factor(migrants_2019$SW_mig, levels=c("N","S","NR","R"))


fig1a <- ggplot(aes(y = IHM, x = season), data = migrants_2019) +
  geom_boxplot(aes(fill = SW_mig),position=position_dodge(.9))+
  #geom_violin(aes(fill = SW_mig),position=position_dodge(.9))+
  #geom_jitter(aes(color=SW_mig, fill = SW_mig), size=0.4, alpha=0.5) +
  #stat_summary(fun=mean, geom="point", aes(group=SW_mig), position=position_dodge(.9), 
  #              size=2, pch = 17)+
  #geom_line(data = df_mean, 
  #          mapping = aes(x = season, y = average, group = SW_mig),
  #          position=position_dodge(.9))+
  geom_point(position=position_dodge(width=0.9),aes(group=SW_mig), alpha = 0.1)+
  theme_classic(base_size = 22, base_family = "serif") +
  xlab("Season") +
  ylab("IHM") +
  geom_hline(yintercept=1, linetype="dashed", 
             color = "orange", size=1)+
  labs(fill = "Migratory Strategy") +
  theme(axis.text.x = element_blank(),
        #axis.ticks.x = element_blank(),
        axis.title.x = element_blank())+
  scale_fill_npg(labels = c("Neotropical Migrants","Short-Distance Migrants","Neotropical Residents","North American Residents"))

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

all_data3_mig <- all_data1 %>%
  pivot_longer(!c("season","SW_mig","state"), names_to = "No.", values_to = "PD" )

all_data4_mig <- all_data3_mig %>%
  pivot_wider(names_from = "state", values_from = "PD")

all_data4_mig <- all_data4_mig %>%
  mutate(ratio = (natural+1)/(modified+1))

all_data4_mig$season <- factor(all_data4_mig$season, levels=c("breeding", "postbreeding","nonbreeding", "prebreeding"))
all_data4_mig$SW_mig <- factor(all_data4_mig$SW_mig, levels=c("N","S","NR","R"))


#all_data4$log_ratio <- log(all_data4$ratio)

pd_aov <- aov(ratio~season*SW_mig, data = all_data4_mig)
pd_lm <- lm(ratio~season*SW_mig, data = all_data4_mig)
summary(pd_aov)
summary(pd_lm)
library(piecewiseSEM)

AIC(mig.stat.lmer)

#assumptions
par(mfrow=c(2,2))
plot(pd_lm)
hist(all_data4$log_ratio)
#dont use transformed, but looks weirdly bimodal? probably on account of the groups
x <- all_data4 %>%
  filter(season == "breeding" &
           SW_mig == "")
hist(x$ratio)

mig.tukey.pd.table <- emmeans(pd_aov, list(pairwise ~ SW_mig*season), adjust = "holm")
summary(mig.tukey.pd.table)
write.table(mig.tukey.pd.table$`pairwise differences of SW_mig, season`, file = "fig_outputs/mig-holm-pd.txt", sep = ",", quote = FALSE, row.names = F)


#### fig1b ####
fig1b <- ggplot(aes(y = ratio, x = season, fill = SW_mig), data = all_data4_mig)+
  geom_boxplot(position=position_dodge(.9))+
  #geom_violin(position=position_dodge(.9))+
  theme_classic(base_size = 22, base_family = "serif") +
  xlab("Season") +
  ylab("SHM") +
  labs(fill = "Migratory Strategy") +
  geom_hline(yintercept=1, linetype="dashed", 
             color = "orange", size=1)+
  theme(axis.text.x = element_text(angle = 45, hjust=1))+
  #geom_point(position=position_dodge(width=0.9),aes(group=SW_mig), alpha = 0.1)+
  scale_fill_npg(labels = c("Neotropical Migrants","Short-Distance Migrants","Neotropical Residents","North American Residents"))

fig1a
fig1b
plotlist1 <- list(fig1a,fig1b)

figure1 <- ggarrange(plotlist=plotlist1,
                     common.legend = T,
                     ncol=1,
                     nrow=2,
                     legend="right",
                     align="hv")#%>%
  #annotate_figure(bottom = text_grob("Season", gp = gpar(cex = 1.3)))

png("fig_outputs/figure1.png", height = 11.5, width = 8, units = "in",res=300)
figure1
dev.off()




#comparison of FAC SHM between mig groups
#load("data_outputs/FAC_trend_data.RData")

#table(FAC_trend_data$SW_mig)

#aov.fac.mig <- lm(SHM~SW_mig, data = FAC_trend_data)
#summary(aov.fac.mig)
#summary(aov(SHM~SW_mig, data = FAC_trend_data))
#fac.tukey.table <- emmeans(aov.fac.mig, list(pairwise ~ SW_mig), adjust = "tukey")

#vis


#ggplot(migrants,aes(x=season,y=SHM))+
#  geom_point()+
#  geom_line(aes(group=species_code))+  ## connect subjects with a line
#  facet_grid(.~SW_mig)+
#  theme(axis.text.x = element_text(angle = 45, hjust=1))
  
#FAC variation
#load("data_outputs/FAC_trend_data.RData")

#FAC_trend_data$SW_mig2 <- FAC_trend_data$SW_mig
#FAC_trend_data$SW_mig2 <- gsub("NR", "R", FAC_trend_data$SW_mig2)


#var.plot <- FAC_trend_data %>%
#  group_by(SW_mig) %>%
#  dplyr::summarize(mean = mean(SHM),
#                   sd = sd(SHM),
#                   se = sd(SHM)/sqrt(length(SHM)))

#ggplot(var.plot, aes(x = SW_mig, y = mean)) +
#  geom_line(position=position_dodge(0.25)) +
#  geom_point(position=position_dodge(0.25)) +
#  geom_errorbar(aes(ymin=mean-(se), ymax=mean+(se)), width=.2,
#                position=position_dodge(0.25)) +
#  theme_classic() +
#  xlab("Migratory Status") +
#  ylab("Mean SHM") +
#  labs(col = "Migratory Stratus") +
#  theme(axis.text.x = element_text(angle = 45, hjust=1))

##migratory strategy - PDs

#load("data_outputs/pds_migrants.RData")

#pds_migrants_mig <- pds_migrants %>%
#  group_by(SW_mig, season, state) %>%
#  dplyr::summarise(prop_p = sum(trajectory == "P")/ sum(trajectory == "P" | trajectory == "N"))

#nlme
#mig.stat.lme.pd <- lme(prop_p ~ season*SW_mig, data = pds_migrants_mig)
#summary(mig.stat.lme)
#anova(mig.stat.lme)
#intervals(mig.stat.lme)
#lme4
#mig.stat.lmer <- lmer(SHM ~ season*SW_mig + (1 | species_code), data = migrants)
#summary(mig.stat.lmer)
#AIC(mig.stat.lmer)
#same results
#anova(mig.stat.lmer)
# F values different for season, but similar, and same outcome





#### diet ####
load("data_outputs/IHM_migrants_2019.RData")

#migrants$diet2 <- factor(migrants$diet2, levels=c("G","F","O","C","I"))

#mig.diet.lme <- nlme::lme(SHM ~ season*diet2, data = migrants, random = ~ 1 | species_code)
#summary(mig.diet.lme)
#anova(mig.diet.lme)
#summary(aov(SHM ~ season*diet2 + Error(species_code), data = migrants))
#lme4
mig.diet.lmer <- lmer(SHM ~ season*diet2 + (1 | species_code), data = migrants_2019)
summary(mig.diet.lmer)
anova(mig.diet.lmer)
library(piecewiseSEM)
rsquared(mig.diet.lmer)
AIC(mig.stat.lmer)
#sig difference for 
#x <- lmer(SHM~ season*diet2 + (1 | species_code), data = migrants)
#summary(x)
#anova(x)
#eta_squared(x, partial=T)

#assumptions

#boxplot suggests outliers, not too bad
#particularly in insectivores postbreeding
bxp <- ggboxplot(
  migrants_2019, x = "season", y = "SHM",
  color = "diet2", palette = "jco"
)
bxp

outliers.mig <- migrants_2019 %>%
  group_by(season, diet2) %>%
  identify_outliers(IHM) # 20 outliers

#normality
#using qq since sample size relatively large
ggqqplot(migrants_2019, "IHM", ggtheme = theme_bw()) +
  facet_grid(season ~ diet2)
#pretty decent


#homogeneity of variances
migrants_2019 %>%
  group_by(season) %>%
  levene_test(IHM ~ diet2)
#good
ggplot(migrants_2019, aes(y=IHM, x=diet2))+
  geom_point() +
  facet_wrap(~season)
#greater in insectivores

#homogeneity of covariances
box_m(migrants_2019[, "IHM", drop = FALSE], migrants_2019$diet2)
#assumption violated, but not too bad - difficult to remedy, relatively robust

#run without carnivores and omnivores
#migrants_diet2 <- migrants %>%
#  filter(diet2 != "C" & diet2 != "O")

#mig.diet.lme2 <- lme(SHM ~ season*diet2, data = migrants_diet2, random = ~ 1 | species_code)
#summary(mig.diet.lme2)
#anova(mig.diet.lme2)
#summary(aov(SHM ~ season*diet2 + Error(species_code), data = migrants))
#lme4
#mig.diet.lmer <- lmer(SHM ~ season*diet2 + (1 | species_code), data = migrants)
#summary(mig.diet.lmer)

#### post-hoc comparisons ####
mig.diet.tukey.table <- emmeans(mig.diet.lmer, list(pairwise ~ diet2*season), adjust = "holm")
summary(mig.diet.tukey.table)
write.table(mig.diet.tukey.table$`pairwise differences of diet2, season`, file = "fig_outputs/diet-tuk-rpi.txt", sep = ",", quote = FALSE, row.names = F)
#mig.diet.bon.table <- emmeans(mig.diet.lmer, list(pairwise ~ diet2*season), adjust = "bonferroni")
#write.table(mig.bon.table$`pairwise differences of SW_mig, season`, file = "fig_outputs/mig-bon.txt", sep = ",", quote = FALSE, row.names = F)
#insectivores higher in the breeding season that other seasons

#marginal and conditional Rsq, AIC
#install.packages("piecewiseSEM")
library(piecewiseSEM)
rsquared(mig.stat.lmer)
#rsquared.glmm(mig.stat.lmer)
AIC(mig.stat.lmer)

#vis

#### fig2a ####
fig2a <- ggplot(aes(y = IHM, x = season), data = migrants_2019) +
  geom_boxplot(aes(fill = diet2),position=position_dodge(.9))+
  #geom_violin(aes(fill = SW_mig),position=position_dodge(.9))+
  #geom_jitter(aes(color=SW_mig, fill = SW_mig), size=0.4, alpha=0.5) +
  #stat_summary(fun=mean, geom="point", aes(group=SW_mig), position=position_dodge(.9), 
  #              size=2, pch = 17)+
  #geom_line(data = df_mean, 
  #          mapping = aes(x = season, y = average, group = SW_mig),
  #          position=position_dodge(.9))+
  geom_point(position=position_dodge(width=0.9),aes(group=diet2), alpha = 0.1)+
  theme_classic(base_size = 22, base_family = "serif") +
  xlab("Season") +
  ylab("IHM") +
  labs(fill = "Diet") +
  geom_hline(yintercept=1, linetype="dashed", 
             color = "orange", size=1)+
  theme(axis.text.x = element_blank(),
        #axis.ticks.x = element_blank(),
        axis.title.x = element_blank())+#,
        #axis.title.y = element_blank())+
  scale_fill_npg(labels=c("Carnivore","Frugivore","Granivore","Insectivore","Omnivore"))#+
  #facet_wrap(~diet2)
fig2a

#PDs

load("data/species_basic.RData")
load("data_outputs/bootstrap_all.RData")

all_data <- left_join(bootstrap_all,species_basic,by="species_code")

all_data1 <- all_data %>%
  group_by(season, diet2, state) %>%
  summarise(across(where(is.numeric), ~sum(.>0)/length(.)))

SD <- transform(all_data1[,-c(1:3)], stdev =apply(all_data1[,-c(1:3)], 1, sd, na.rm = TRUE))
stdev <- SD$stdev
M <- transform(all_data1[,-c(1:3)], mean =apply(all_data1[,-c(1:3)], 1, mean, na.rm = TRUE))
mean <- M$mean

all_data3_diet <- all_data1 %>%
  pivot_longer(!c("season","diet2","state"), names_to = "No.", values_to = "PD" )

all_data4_diet <- all_data3_diet %>%
  pivot_wider(names_from = "state", values_from = "PD")

all_data4_diet <- all_data4_diet %>%
  mutate(ratio = (natural+1)/(modified+1))

all_data4_diet$season <- factor(all_data4_diet$season, levels=c("breeding", "postbreeding","nonbreeding", "prebreeding"))



#all_data4$log_ratio <- log(all_data4$ratio)

pd_aov <- aov(ratio~season*diet2, data = all_data4_diet)
pd_lm <- lm(ratio~season*diet2, data = all_data4_diet)
summary(pd_aov)
summary(pd_lm)
AIC(pd_lm)

#assumptions
par(mfrow=c(2,2))
plot(pd_lm)
#better untransformed

diet.tukey.pd.table <- emmeans(pd_aov, list(pairwise ~ diet2*season), adjust = "holm")
summary(diet.tukey.pd.table)
write.table(diet.tukey.pd.table$`pairwise differences of diet2, season`, file = "fig_outputs/diet-holm-pd.txt", sep = ",", quote = FALSE, row.names = F)


#### fig2b ####
fig2b <- ggplot(aes(y = ratio, x = season, fill = diet2), data = all_data4_diet)+
  geom_boxplot(position=position_dodge(.9))+
  #geom_violin(position=position_dodge(.9))+
  theme_classic(base_size = 22, base_family = "serif") +
  xlab("Season") +
  ylab("SHM") +
  labs(fill = "Diet") +
  geom_hline(yintercept=1, linetype="dashed", 
             color = "orange", size=1)+
  theme(axis.text.x = element_text(angle = 45, hjust=1))+#,
        #axis.title.y = element_blank())+
  #geom_point(position=position_dodge(width=0.9),aes(group=SW_mig), alpha = 0.1)+
  scale_fill_npg(labels=c("Carnivore","Frugivore","Granivore","Insectivore","Omnivore"))

fig2a
fig2b
plotlist2 <- list(fig2a,fig2b)

figure2 <- ggarrange(plotlist=plotlist2,
                     common.legend = T,
                     ncol=1,
                     nrow=2,
                     legend="right",
                     align="hv")

png("fig_outputs/figure2.png", height = 11.5, width = 8, units = "in",res=300)
figure2
dev.off()




#### foraging ####
load("data_outputs/IHM_migrants_2019.RData")

#migrants$diet2 <- factor(migrants$diet2, levels=c("G","F","O","C","I"))

#mig.for.lme <- lme(SHM ~ season*sw_foraging, data = migrants, random = ~ 1 | species_code)
#summary(mig.for.lme)
#anova(mig.diet.lme)
#summary(aov(SHM ~ season*diet2 + Error(species_code), data = migrants))
#lme4
mig.for.lmer <- lmer(IHM ~ season*sw_foraging + (1 | species_code), data = migrants_2019)
summary(mig.for.lmer)
AIC(mig.for.lmer)
library(piecewiseSEM)
rsquared(mig.for.lmer)
AIC(mig.for.lmer)
#assumptions

#boxplot suggests outliers
#particularly in insectivores postbreeding
bxp <- ggboxplot(
  migrants_2019, x = "season", y = "IHM",
  color = "sw_foraging", palette = "jco"
)
bxp

outliers.mig <- migrants_2019 %>%
  group_by(season, sw_foraging) %>%
  identify_outliers(IHM)

#normality
#using qq since sample size relatively large
ggqqplot(migrants_2019, "IHM", ggtheme = theme_bw()) +
  facet_grid(season ~ sw_foraging)
#mostly ok 

#homogeneity of variances
migrants_2019 %>%
  group_by(season) %>%
  levene_test(IHM ~ sw_foraging)
#assumptions met
ggplot(migrants_2019, aes(y=IHM, x=sw_foraging))+
  geom_point() +
  facet_wrap(~season)


#homogeneity of covariances
box_m(migrants_2019[, "IHM", drop = FALSE], migrants_2019$sw_foraging)
#assumption met

#run without ground hawkers and bark foragers
#migrants_diet2 <- migrants %>%
#  filter(diet2 != "C" & diet2 != "O")

#mig.diet.lme2 <- lme(SHM ~ season*diet2, data = migrants_diet2, random = ~ 1 | species_code)
#summary(mig.diet.lme2)
#anova(mig.diet.lme2)
#summary(aov(SHM ~ season*diet2 + Error(species_code), data = migrants))
#lme4
mig.for.lmer <- lmer(IHM ~ season*sw_foraging + (1 | species_code), data = migrants)
summary(mig.for.lmer)

##post-hoc comparisons
PI.for.holm.table <- emmeans(mig.for.lmer, list(pairwise ~ sw_foraging*season), adjust = "holm")
summary(PI.for.holm.table)
write.table(PI.for.holm.table$`pairwise differences of sw_foraging, season`, file = "fig_outputs/rPI-for-holm.txt", sep = ",", quote = FALSE, row.names = F)
#mig.for.bon.table <- emmeans(mig.for.lmer, list(pairwise ~ sw_foraging*season), adjust = "bonferroni")
#write.table(mig.bon.table$`pairwise differences of SW_mig, season`, file = "fig_outputs/mig-bon.txt", sep = ",", quote = FALSE, row.names = F)

#vis

#### fig3a ####
fig3a <- ggplot(aes(y = IHM, x = season), data = migrants_2019) +
  geom_boxplot(aes(fill = sw_foraging),position=position_dodge(.9))+
  geom_point(position=position_dodge(width=0.9),aes(group=sw_foraging), alpha = 0.1)+
  theme_classic(base_size = 22, base_family = "serif") +
  xlab("Season") +
  ylab("IHM") +
  labs(fill = "Foraging Strategy") +
  geom_hline(yintercept=1, linetype="dashed", 
             color = "orange", size=1)+
  theme(axis.text.x = element_blank(),
        axis.title.x = element_blank())+#,
        #axis.title.y = element_blank())+
  scale_fill_npg(labels=c("Aerial Forager","Aerial Hawker","Bark Forager",
                          "Foliage Gleaner","Ground Forager","Ground Hawker"))
fig3a

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

all_data3_for <- all_data1 %>%
  pivot_longer(!c("season","sw_foraging","state"), names_to = "No.", values_to = "PD" )

all_data4_for <- all_data3_for %>%
  pivot_wider(names_from = "state", values_from = "PD")

all_data4_for <- all_data4_for %>%
  mutate(ratio = (natural+1)/(modified+1))

all_data4_for$season <- factor(all_data4_for$season, levels=c("breeding", "postbreeding","nonbreeding", "prebreeding"))



#all_data4$log_ratio <- log(all_data4$ratio)

pd_aov <- aov(ratio~season*sw_foraging, data = all_data4)
pd_lm <- lm(ratio~season*sw_foraging, data = all_data4)
summary(pd_aov)
summary(pd_lm)
AIC(pd_lm)

#assumptions
par(mfrow=c(2,2))
plot(pd_lm)
#better untransformed

for.holm.pd.table <- emmeans(pd_aov, list(pairwise ~ sw_foraging*season), adjust = "holm")
summary(for.holm.pd.table)
write.table(for.holm.pd.table$`pairwise differences of sw_foraging, season`, file = "fig_outputs/for-holm-pd.txt", sep = ",", quote = FALSE, row.names = F)


#### fig3b ####
fig3b <- ggplot(aes(y = ratio, x = season, fill = sw_foraging), data = all_data4_for)+
  geom_boxplot(position=position_dodge(.9))+
  theme_classic(base_size = 22, base_family = "serif") +
  xlab("Season") +
  ylab("SHM") +
  labs(fill = "Foraging Strategy") +
  geom_hline(yintercept=1, linetype="dashed", 
             color = "orange", size=1)+
  theme(axis.text.x = element_text(angle = 45, hjust=1))+#,
        #axis.title.y = element_blank())+
  scale_fill_npg(labels=c("Aerial Forager","Aerial Hawker","Bark Forager",
                          "Foliage Gleaner","Ground Forager","Ground Hawker"))

fig3a
fig3b
plotlist3 <- list(fig3a,fig3b)

figure3 <- ggarrange(plotlist=plotlist3,
                     common.legend = T,
                     ncol=1,
                     nrow=2,
                     legend="right",
                     align="hv")

png("fig_outputs/figure3.png", height = 11.5, width = 8, units = "in",res=300)
figure3
dev.off()


all_figs_list <- list(figure1, figure2, figure3)

all_figs <- ggarrange(plotlist = all_figs_list,
                      common.legend = F,
                      ncol = 3,
                      nrow = 2)










#### recycling ####

#three-way interaction
season.diet.plot <- migrants %>%
  group_by(season, diet2, SW_mig) %>%
  dplyr::summarise(mean = mean(SHM),
                   sd = sd(SHM),
                   se = sd(SHM)/sqrt(length(SHM)))

ggplot(season.diet.plot, aes(x = diet2, y = mean, col = SW_mig)) +
  geom_line() +
  geom_point(position=position_dodge(0.25)) +
  geom_errorbar(aes(ymin=mean-(2*se), ymax=mean+(2*se)), width=.2,
                position=position_dodge(0.25)) +
  theme_classic() +
  xlab("Diet") +
  ylab("Mean SHM") +
  theme(axis.text.x = element_text(angle = 45, hjust=1)) +
  facet_wrap(~season)

### RESIDENTS ###

#diet

load("data_outputs/resident_trend_data.RData")

res.diet.lme <- lme(SHM ~ season*diet2, data = resident_trend_data, random = ~ 1 | species_code)
summary(res.diet.lme)
summary(aov(SHM ~ season*diet2 + Error(species_code), data = resident_trend_data))

res.diet.lmer <- lmer(SHM ~ season*diet2 + (1 | species_code), data = resident_trend_data)
summary(res.diet.lmer)
anova(res.diet.lmer)

##post-hoc comparisons
#some indication of an interaction from summary above
res.diet.tukey.table <- emmeans(res.diet.lmer, list(pairwise ~ diet2*season), adjust = "tukey")
write.table(res.diet.tukey.table$`pairwise differences of diet2, season`, file = "fig_outputs/res-diet-tuk.txt", sep = ",", quote = FALSE, row.names = F)
res.diet.bon.table <- emmeans(res.diet.lmer, list(pairwise ~ diet2*season), adjust = "bonferroni")
write.table(mig.bon.table$`pairwise differences of diet2, season`, file = "fig_outputs/res-diet-bon.txt", sep = ",", quote = FALSE, row.names = F)

res.diet.plot <- resident_trend_data %>%
  group_by(diet2, season) %>%
  dplyr::summarize(mean = mean(SHM),
                   sd = sd(SHM),
                   se = sd(SHM)/sqrt(length(SHM)))

ggplot(res.diet.plot, aes(x = season, y = mean, group = diet2, col = diet2)) +
  geom_line(position=position_dodge(0.25)) +
  geom_point(position=position_dodge(0.25)) +
  geom_errorbar(aes(ymin=mean-(se), ymax=mean+(se)), width=.2,
                position=position_dodge(0.25)) +
  theme_classic() +
  xlab("Season") +
  ylab("Mean SHM") +
  labs(col = "Diet") +
  theme(axis.text.x = element_text(angle = 45, hjust=1)) #+
  facet_wrap(~diet2)



#foraging

resident_trend_data$foraging2 <- resident_trend_data$sw_foraging
resident_trend_data$foraging2 <- gsub("AH", "A", resident_trend_data$foraging2)
resident_trend_data$foraging2 <- gsub("AF", "A", resident_trend_data$foraging2)
unique(resident_trend_data$foraging2)   
  
  
forag.lme <- lme(SHM ~ season*sw_foraging, data = resident_trend_data, random = ~ 1 | species_code)
summary(forag.lme)
anova(forag.lme)

res.for.lmer <- lmer(SHM ~ season*foraging2 + (1 | species_code), data = resident_trend_data)
summary(res.for.lmer)
anova(res.for.lmer)

#comparison between all combinations of foraging strategies
res.for.tukey.table <- emmeans(res.for.lmer, list(pairwise ~ sw_foraging), adjust = "tukey")

#vis
res.for.plot <- resident_trend_data %>%
  group_by(foraging2, season) %>%
  dplyr::summarize(mean = mean(SHM),
                   sd = sd(SHM),
                   se = sd(SHM)/sqrt(length(SHM)))

ggplot(res.for.plot, aes(x = season, y = mean, group = foraging2, col = foraging2)) +
  geom_line(position=position_dodge(0.25)) +
  geom_point(position=position_dodge(0.25)) +
  geom_errorbar(aes(ymin=mean-(se), ymax=mean+(se)), width=.2,
                position=position_dodge(0.25)) +
  theme_classic() +
  xlab("Season") +
  ylab("Mean SHM") +
  labs(col = "Foraging Strategy") +
  theme(axis.text.x = element_text(angle = 45, hjust=1)) +
facet_wrap(~foraging2)