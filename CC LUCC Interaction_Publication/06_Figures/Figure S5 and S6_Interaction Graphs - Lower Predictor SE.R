#################################################################################################
########                                                                                 ########
########                  Graphs of conditional effects for interactions                 ######## 
########                   btw predictors for each biodiversity response                 ########
#################################################################################################


# This code uses the three brms models constructed for observed richness, rarefied richness,
# abundance (1 model for each) and creates graphs of the results.

# Models - Lower standard error of predictors

# Plots created:
# Figure S5
# Figure S6




# Website used for tutorial:
# http://mjskay.github.io/tidybayes/articles/tidy-brms.html



########## clean workspace and load required packages ####################

# clean workspace to improve efficiency: #
# rm(list = ls() ) 
# gc() #releases memory

library(rstan)
library(brms)
library(tidybayes)
library(ggmcmc)
library(ggdist)

library(tidyverse) # Data organization

library(magrittr)
library(dplyr)
library(purrr)
library(forcats)
library(tidyr)
library(modelr)
library(ggplot2)
library(cowplot)
library(ggrepel)
library(RColorBrewer)
library(gganimate)
library(posterior)
library(sf)


###############################################
###                                         ###
###             Data Loading                ###
###                                         ###
###############################################

# Load the three model objects
path <- "01_Analysis/Modeling_BRMS/Model Objects/CC and LUCC interactions only/"
observed <- readRDS(paste(path, "Observed Richness_CC LUCC interactions.rds", sep = ""))
rarefied <- readRDS(paste(path, "Rarefied Richness_CC LUCC interactions.rds", sep = ""))
abundance <- readRDS(paste(path, "Abundance_CC LUCC interactions.rds", sep = ""))



###############################################
###                                         ###
###           Data Preparation              ###
###                                         ###
###############################################

# Set theme
theme_set(theme_tidybayes() + panel_border())


# Get a list of raw variable names (will be the same for each model)
variables <- tidybayes :: get_variables(observed)



############################################
###            Interaction Plots         ###
############################################

####################
## Canopy :: Temp ##
####################

# Make the dfs

obs.tempcan <- conditional_effects(observed, effects = "temp.lower:canopy.lower")
obs.tempcan <- as.data.frame(obs.tempcan$`temp.lower:canopy.lower`)
obs.tempcan$Type <- "observed"
rare.tempcan <- conditional_effects(rarefied, effects = "temp.lower:canopy.lower")
rare.tempcan <- as.data.frame(rare.tempcan$`temp.lower:canopy.lower`)
rare.tempcan$Type <- "rarefied"
ab.tempcan <- conditional_effects(abundance, effects = "temp.lower:canopy.lower")
ab.tempcan <- as.data.frame(ab.tempcan$`temp.lower:canopy.lower`)
ab.tempcan$Type <- "abundance"

# effect1__ is the range of temperature slope values
# effect2__ is the categorical values of canopy: 1, 0, -1

# FYI: effect1__ is identical to temp.lower (will be identical to whatever first varying
# parameter of interest is)

# Categorical levels are not exactly 1, 0 and negative 1 so they need to be recoded
# First change to numeric then round, then convert back to factor
obs.tempcan$effect2__ <- factor(round(as.numeric(as.character(obs.tempcan$effect2__))))
rare.tempcan$effect2__ <- factor(round(as.numeric(as.character(rare.tempcan$effect2__))))
ab.tempcan$effect2__ <- factor(round(as.numeric(as.character(ab.tempcan$effect2__))))

# Observed
tempcanplot <- ggplot() +
  # Zero lines
  geom_hline(yintercept = 0, color = "black") +
  geom_vline(xintercept = 0, color = "black") +
  geom_line(data = obs.tempcan, aes(x = temp.lower, y = estimate__, color = effect2__)) +
  geom_ribbon(data = obs.tempcan, aes(x = temp.lower, y = estimate__, ymin = lower__,
                                    ymax = upper__, fill = effect2__), alpha = 0.7) +
  theme_bw() +
  theme(axis.text.x = element_text(size=22, face="bold")) +
  theme(axis.text.y = element_text(size=22, face="bold")) +
  theme(axis.title.x = element_text(size=22, face="bold", color="gray30")) +
  theme(axis.title.y = element_text(size=22, face="bold", color="gray30")) +
  scale_x_continuous(name="Temperature Trend",
                     breaks=c(-3, -2, -1, 0, 1, 2)) +
  # scale_x_continuous(name="\u0394Temperature (\u00b0C)/Year",
  #                    breaks=c(-3, -2, -1, 0, 1, 2)) +
  theme(axis.title.y.right = element_text(vjust = 1.7)) +
  scale_y_continuous(name="Observed Richness Trend", limits = c(-0.0121, 0.005),
                     breaks = c(-0.010, -0.005, 0.000, 0.005),
                     sec.axis = dup_axis(name = "% Change",
                                         labels = c("-2.28", "-1.14", "0.00", "1.16"))) +
  scale_colour_manual(name="\u0394Canopy", values = c('1' = "forestgreen",
                                                      '0' = "gray40", 
                                                      '-1' = "brown4")) +
  scale_fill_manual(name="\u0394Canopy", values = c('1' = "forestgreen",
                                                    '0' = "gray40", 
                                                    '-1' = "brown4")) +
  # theme(legend.position = c(0.80, 0.85)) +
  theme(legend.position = "none")
  # theme(legend.text = element_text(size=18), legend.title = element_text(size = 18)) +
  # guides(colour = guide_legend(override.aes = (list(size=3))))

path2 <- "01_Analysis/Model_Plotting/Interaction Conditional Effects/Lower SE for predictors/"
ggsave(paste(path2, "Temperature and Canopy - Observed conditional effects.jpg", sep = ""),
       plot=tempcanplot,
       device = "jpg",
       width=7, height=5, dpi=600)

# Rarefied
tempcanplotR <- ggplot() +
  # Zero lines
  geom_hline(yintercept = 0, color = "black") +
  geom_vline(xintercept = 0, color = "black") +
  geom_line(data = rare.tempcan, aes(x = temp.lower, y = estimate__, color = effect2__)) +
  geom_ribbon(data = rare.tempcan, aes(x = temp.lower, y = estimate__, ymin = lower__,
                                      ymax = upper__, fill = effect2__), alpha = 0.7) +
  theme_bw() +
  theme(axis.text.x = element_text(size=22, face="bold")) +
  theme(axis.text.y = element_text(size=22, face="bold")) +
  theme(axis.title.x = element_text(size=22, face="bold", color="gray30")) +
  theme(axis.title.y = element_text(size=22, face="bold", color="gray30")) +
  scale_x_continuous(name="Temperature Trend",
                     breaks=c(-3, -2, -1, 0, 1, 2)) +
  # scale_x_continuous(name="\u0394Temperature (\u00b0C)/Year",
  #                    breaks=c(-3, -2, -1, 0, 1, 2)) +
  theme(axis.title.y.right = element_text(vjust = 1.7)) +
  scale_y_continuous(name="Rarefied Richness Trend", limits = c(-0.0121, 0.005),
                     breaks = c(-0.010, -0.005, 0.000, 0.005),
                     sec.axis = dup_axis(name = "% Change",
                                         labels = c("-2.28", "-1.14", "0.00", "1.16"))) +
  scale_colour_manual(name="\u0394Canopy", values = c('1' = "forestgreen",
                                                      '0' = "gray40", 
                                                      '-1' = "brown4")) +
  scale_fill_manual(name="\u0394Canopy", values = c('1' = "forestgreen",
                                                    '0' = "gray40", 
                                                    '-1' = "brown4")) +
  # theme(legend.position = c(0.80, 0.85)) +
  theme(legend.position = "none")
  # theme(legend.text = element_text(size=18), legend.title = element_text(size = 18)) +
  # guides(colour = guide_legend(override.aes = (list(size=3))))

ggsave(paste(path2, "Temperature and Canopy - Rarefied conditional effects.jpg", sep = ""),
       plot=tempcanplotR,
       device = "jpg",
       width=7, height=5, dpi=600)


# Abundance
tempcanplotA <- ggplot() +
  # Zero lines
  geom_hline(yintercept = 0, color = "black") +
  geom_vline(xintercept = 0, color = "black") +
  geom_line(data = ab.tempcan, aes(x = temp.lower, y = estimate__, color = effect2__)) +
  geom_ribbon(data = ab.tempcan, aes(x = temp.lower, y = estimate__, ymin = lower__,
                                       ymax = upper__, fill = effect2__), alpha = 0.7) +
  theme_bw() +
  theme(axis.text.x = element_text(size=22, face="bold")) +
  theme(axis.text.y = element_text(size=22, face="bold")) +
  theme(axis.title.x = element_text(size=22, face="bold", color="gray30")) +
  theme(axis.title.y = element_text(size=22, face="bold", color="gray30")) +
  scale_x_continuous(name="Temperature Trend",
                     breaks=c(-3, -2, -1, 0, 1, 2)) +
  # scale_x_continuous(name="\u0394Temperature (\u00b0C)/Year",
  #                    breaks=c(-3, -2, -1, 0, 1, 2)) +
  theme(axis.title.y.right = element_text(vjust = 1.7)) +
  scale_y_continuous(name="Abundance Trend", limits = c(-0.0121, 0.005),
                     breaks = c(-0.010, -0.005, 0.000, 0.005),
                     sec.axis = dup_axis(name = "% Change",
                                         labels = c("-2.28", "-1.14", "0.00", "1.16"))) +
  scale_colour_manual(name="\u0394Canopy", values = c('1' = "forestgreen",
                                                      '0' = "gray40", 
                                                      '-1' = "brown4")) +
  scale_fill_manual(name="\u0394Canopy", values = c('1' = "forestgreen",
                                                    '0' = "gray40", 
                                                    '-1' = "brown4")) +
  # theme(legend.position = c(0.80, 0.85)) +
  theme(legend.position = "none")
  # theme(legend.text = element_text(size=18), legend.title = element_text(size = 18)) +
  # guides(colour = guide_legend(override.aes = (list(size=3))))

ggsave(paste(path2, "Temperature and Canopy - Abundance conditional effects.jpg", sep = ""),
       plot=tempcanplotA,
       device = "jpg",
       width=7, height=5, dpi=600)



######################
## Canopy :: Precip ##
######################

# Make the dfs

obs.precipcan <- conditional_effects(observed, effects = "precip.lower:canopy.lower")
obs.precipcan <- as.data.frame(obs.precipcan$`precip.lower:canopy.lower`)
obs.precipcan$Type <- "observed"
rare.precipcan <- conditional_effects(rarefied, effects = "precip.lower:canopy.lower")
rare.precipcan <- as.data.frame(rare.precipcan$`precip.lower:canopy.lower`)
rare.precipcan$Type <- "rarefied"
ab.precipcan <- conditional_effects(abundance, effects = "precip.lower:canopy.lower")
ab.precipcan <- as.data.frame(ab.precipcan$`precip.lower:canopy.lower`)
ab.precipcan$Type <- "abundance"

# effect1__ is the range of precip slope values
# effect2__ is the categorical values of canopy: 1, 0, -1

# Categorical levels are not exactly 1, 0 and negative 1 so they need to be recoded
# First change to numeric then round, then convert back to factor
obs.precipcan$effect2__ <- factor(round(as.numeric(as.character(obs.precipcan$effect2__))))
rare.precipcan$effect2__ <- factor(round(as.numeric(as.character(rare.precipcan$effect2__))))
ab.precipcan$effect2__ <- factor(round(as.numeric(as.character(ab.precipcan$effect2__))))

# FYI: effect1__ is identical to whatever first varying
# parameter of interest is)

range(obs.precipcan$precip.lower)
range(obs.precipcan$estimate__)
#-0.0027  0.0035
range(rare.precipcan$estimate__)
#-0.0004 0.0033
range(ab.precipcan$estimate__)
# -0.01 -0.0042

# Observed
precipcanplot <- ggplot() +
  # Zero lines
  geom_hline(yintercept = 0, color = "black") +
  geom_vline(xintercept = 0, color = "black") +
  geom_line(data = obs.precipcan, aes(x = precip.lower, y = estimate__, color = effect2__)) +
  geom_ribbon(data = obs.precipcan, aes(x = precip.lower, y = estimate__, ymin = lower__,
                                      ymax = upper__, fill = effect2__), alpha = 0.7) +
  theme_bw() +
  theme(axis.text.x = element_text(size=22, face="bold")) +
  theme(axis.text.y = element_text(size=22, face="bold")) +
  theme(axis.title.x = element_text(size=22, face="bold", color="gray30")) +
  theme(axis.title.y = element_text(size=22, face="bold", color="gray30")) +
  scale_x_continuous(name="Precipitation Trend",
                     breaks=c(-3, -2, -1, 0, 1, 2, 3, 4)) +
  # scale_x_continuous(name="\u0394Precipitation (mm)/Year",
  #                    breaks=c(-3, -2, -1, 0, 1, 2, 3, 4)) +
  theme(axis.title.y.right = element_text(vjust = 1.7)) +
  scale_y_continuous(name="Observed Richness Trend", limits = c(-0.0121, 0.005),
                     breaks = c(-0.010, -0.005, 0.000, 0.005),
                     sec.axis = dup_axis(name = "% Change",
                                         labels = c("-2.28", "-1.14", "0.00", "1.16"))) +
  scale_colour_manual(name="\u0394Canopy", values = c('1' = "forestgreen",
                                                      '0' = "gray40", 
                                                      '-1' = "brown4")) +
  scale_fill_manual(name="\u0394Canopy", values = c('1' = "forestgreen",
                                                    '0' = "gray40", 
                                                    '-1' = "brown4")) +
  # theme(legend.position = c(0.80, 0.85)) +
  theme(legend.position = "none")
  # theme(legend.text = element_text(size=18), legend.title = element_text(size = 18)) +
  # guides(colour = guide_legend(override.aes = (list(size=3))))

ggsave(paste(path2, "Precipitation and Canopy - Observed conditional effects.jpg", sep = ""),
       plot=precipcanplot,
       device = "jpg",
       width=7, height=5, dpi=600)

# Rarefied
precipcanplotR <- ggplot() +
  # Zero lines
  geom_hline(yintercept = 0, color = "black") +
  geom_vline(xintercept = 0, color = "black") +
  geom_line(data = rare.precipcan, aes(x = precip.lower, y = estimate__, color = effect2__)) +
  geom_ribbon(data = rare.precipcan, aes(x = precip.lower, y = estimate__, ymin = lower__,
                                       ymax = upper__, fill = effect2__), alpha = 0.7) +
  theme_bw() +
  theme(axis.text.x = element_text(size=22, face="bold")) +
  theme(axis.text.y = element_text(size=22, face="bold")) +
  theme(axis.title.x = element_text(size=22, face="bold", color="gray30")) +
  theme(axis.title.y = element_text(size=22, face="bold", color="gray30")) +
  scale_x_continuous(name="Precipitation Trend",
                     breaks=c(-3, -2, -1, 0, 1, 2, 3, 4)) +
  # scale_x_continuous(name="\u0394Precipitation (mm)/Year",
  #                    breaks=c(-3, -2, -1, 0, 1, 2, 3, 4)) +
  theme(axis.title.y.right = element_text(vjust = 1.7)) +
  scale_y_continuous(name="Rarefied Richness Trend", limits = c(-0.0121, 0.005),
                     breaks = c(-0.010, -0.005, 0.000, 0.005),
                     sec.axis = dup_axis(name = "% Change",
                                         labels = c("-2.28", "-1.14", "0.00", "1.16"))) +
  scale_colour_manual(name="\u0394Canopy", values = c('1' = "forestgreen",
                                                      '0' = "gray40", 
                                                      '-1' = "brown4")) +
  scale_fill_manual(name="\u0394Canopy", values = c('1' = "forestgreen",
                                                    '0' = "gray40", 
                                                    '-1' = "brown4")) +
  # theme(legend.position = c(0.80, 0.85)) +
  theme(legend.position = "none")
  # theme(legend.text = element_text(size=18), legend.title = element_text(size = 18)) +
  # guides(colour = guide_legend(override.aes = (list(size=3))))

ggsave(paste(path2, "Precipitation and Canopy - Rarefied conditional effects.jpg", sep = ""),
       plot=precipcanplotR,
       device = "jpg",
       width=7, height=5, dpi=600)


# Abundance
precipcanplotA <- ggplot() +
  # Zero lines
  geom_hline(yintercept = 0, color = "black") +
  geom_vline(xintercept = 0, color = "black") +
  geom_line(data = ab.precipcan, aes(x = precip.lower, y = estimate__, color = effect2__)) +
  geom_ribbon(data = ab.precipcan, aes(x = precip.lower, y = estimate__, ymin = lower__,
                                     ymax = upper__, fill = effect2__), alpha = 0.7) +
  theme_bw() +
  theme(axis.text.x = element_text(size=22, face="bold")) +
  theme(axis.text.y = element_text(size=22, face="bold")) +
  theme(axis.title.x = element_text(size=22, face="bold", color="gray30")) +
  theme(axis.title.y = element_text(size=22, face="bold", color="gray30")) +
  scale_x_continuous(name="Precipitation Trend",
                     breaks=c(-3, -2, -1, 0, 1, 2, 3, 4)) +
  # scale_x_continuous(name="\u0394Precipitation (mm)/Year",
  #                    breaks=c(-3, -2, -1, 0, 1, 2, 3, 4)) +
  theme(axis.title.y.right = element_text(vjust = 1.7)) +
  scale_y_continuous(name="Abundance Trend", limits = c(-0.0121, 0.005),
                     breaks = c(-0.010, -0.005, 0.000, 0.005),
                     sec.axis = dup_axis(name = "% Change",
                                         labels = c("-2.28", "-1.14", "0.00", "1.16"))) +
  scale_colour_manual(name="\u0394Canopy", values = c('1' = "forestgreen",
                                                      '0' = "gray40", 
                                                      '-1' = "brown4")) +
  scale_fill_manual(name="\u0394Canopy", values = c('1' = "forestgreen",
                                                    '0' = "gray40", 
                                                    '-1' = "brown4")) +
  # theme(legend.position = c(0.80, 0.85)) +
  theme(legend.position = "none")
  # theme(legend.text = element_text(size=18), legend.title = element_text(size = 18)) +
  # guides(colour = guide_legend(override.aes = (list(size=3))))

ggsave(paste(path2, "Precipitation and Canopy - Abundance conditional effects.jpg", sep = ""),
       plot=precipcanplotA,
       device = "jpg",
       width=7, height=5, dpi=600)



####################
## Crop :: Temp ##
####################

# Make the dfs

obs.tempcrop <- conditional_effects(observed, effects = "temp.lower:crop.lower")
obs.tempcrop <- as.data.frame(obs.tempcrop$`temp.lower:crop.lower`)
obs.tempcrop$Type <- "observed"
rare.tempcrop <- conditional_effects(rarefied, effects = "temp.lower:crop.lower")
rare.tempcrop <- as.data.frame(rare.tempcrop$`temp.lower:crop.lower`)
rare.tempcrop$Type <- "rarefied"
ab.tempcrop <- conditional_effects(abundance, effects = "temp.lower:crop.lower")
ab.tempcrop <- as.data.frame(ab.tempcrop$`temp.lower:crop.lower`)
ab.tempcrop$Type <- "abundance"

# effect1__ is the range of temperature slope values
# effect2__ is the categorical values of canopy: 1, 0, -1

# FYI: effect1__ is identical to whatever first varying
# parameter of interest is)

# Categorical levels are not exactly 1, 0 and negative 1 so they need to be recoded
# First change to numeric then round, then convert back to factor
obs.tempcrop$effect2__ <- factor(round(as.numeric(as.character(obs.tempcrop$effect2__))))
rare.tempcrop$effect2__ <- factor(round(as.numeric(as.character(rare.tempcrop$effect2__))))
ab.tempcrop$effect2__ <- factor(round(as.numeric(as.character(ab.tempcrop$effect2__))))

range(obs.tempcrop$estimate__)
range(rare.tempcrop$estimate__)
range(ab.tempcrop$estimate__)

# Observed
tempcropplot <- ggplot() +
  # Zero lines
  geom_hline(yintercept = 0, color = "black") +
  geom_vline(xintercept = 0, color = "black") +
  geom_line(data = obs.tempcrop, aes(x = temp.lower, y = estimate__, color = effect2__)) +
  geom_ribbon(data = obs.tempcrop, aes(x = temp.lower, y = estimate__, ymin = lower__,
                                      ymax = upper__, fill = effect2__), alpha = 0.7) +
  theme_bw() +
  theme(axis.text.x = element_text(size=22, face="bold")) +
  theme(axis.text.y = element_text(size=22, face="bold")) +
  theme(axis.title.x = element_text(size=22, face="bold", color="gray30")) +
  theme(axis.title.y = element_text(size=22, face="bold", color="gray30")) +
  scale_x_continuous(name="Temperature Trend",
                     breaks=c(-3, -2, -1, 0, 1, 2)) +
  # scale_x_continuous(name="\u0394Temperature (\u00b0C)/Year",
  #                    breaks=c(-3, -2, -1, 0, 1, 2)) +
  theme(axis.title.y.right = element_text(vjust = 1.7)) +
  scale_y_continuous(name="Observed Richness Trend", limits = c(-0.0121, 0.005),
                     breaks = c(-0.010, -0.005, 0.000, 0.005),
                     sec.axis = dup_axis(name = "% Change",
                                         labels = c("-2.28", "-1.14", "0.00", "1.16"))) +
  scale_colour_manual(name="\u0394Cropland", values = c('1' = "orange1",
                                                        '0' = "gray40", 
                                                        '-1' = "turquoise4")) +
  scale_fill_manual(name="\u0394Cropland", values = c('1' = "orange1",
                                                      '0' = "gray40", 
                                                      '-1' = "turquoise4")) +
  # theme(legend.position = c(0.80, 0.85)) +
  theme(legend.position = "none")
  # theme(legend.text = element_text(size=18), legend.title = element_text(size = 18)) +
  # guides(colour = guide_legend(override.aes = (list(size=3))))

ggsave(paste(path2, "Temperature and Cropland - Observed conditional effects.jpg", sep = ""),
       plot=tempcropplot,
       device = "jpg",
       width=7, height=5, dpi=600)

# Rarefied
tempcropplotR <- ggplot() +
  # Zero lines
  geom_hline(yintercept = 0, color = "black") +
  geom_vline(xintercept = 0, color = "black") +
  geom_line(data = rare.tempcrop, aes(x = temp.lower, y = estimate__, color = effect2__)) +
  geom_ribbon(data = rare.tempcrop, aes(x = temp.lower, y = estimate__, ymin = lower__,
                                       ymax = upper__, fill = effect2__), alpha = 0.7) +
  theme_bw() +
  theme(axis.text.x = element_text(size=22, face="bold")) +
  theme(axis.text.y = element_text(size=22, face="bold")) +
  theme(axis.title.x = element_text(size=22, face="bold", color="gray30")) +
  theme(axis.title.y = element_text(size=22, face="bold", color="gray30")) +
  scale_x_continuous(name="Temperature Trend",
                     breaks=c(-3, -2, -1, 0, 1, 2)) +
  # scale_x_continuous(name="\u0394Temperature (\u00b0C)/Year",
  #                    breaks=c(-3, -2, -1, 0, 1, 2)) +
  theme(axis.title.y.right = element_text(vjust = 1.7)) +
  scale_y_continuous(name="Rarefied Richness Trend", limits = c(-0.0121, 0.005),
                     breaks = c(-0.010, -0.005, 0.000, 0.005),
                     sec.axis = dup_axis(name = "% Change",
                                         labels = c("-2.28", "-1.14", "0.00", "1.16"))) +
  scale_colour_manual(name="\u0394Cropland", values = c('1' = "orange1",
                                                        '0' = "gray40", 
                                                        '-1' = "turquoise4")) +
  scale_fill_manual(name="\u0394Cropland", values = c('1' = "orange1",
                                                      '0' = "gray40", 
                                                      '-1' = "turquoise4")) +
  # theme(legend.position = c(0.80, 0.85)) +
  theme(legend.position = "none")
  # theme(legend.text = element_text(size=18), legend.title = element_text(size = 18)) +
  # guides(colour = guide_legend(override.aes = (list(size=3))))

ggsave(paste(path2, "Temperature and Cropland - Rarefied conditional effects.jpg", sep = ""),
       plot=tempcropplotR,
       device = "jpg",
       width=7, height=5, dpi=600)


# Abundance
tempcropplotA <- ggplot() +
  # Zero lines
  geom_hline(yintercept = 0, color = "black") +
  geom_vline(xintercept = 0, color = "black") +
  geom_line(data = ab.tempcrop, aes(x = temp.lower, y = estimate__, color = effect2__)) +
  geom_ribbon(data = ab.tempcrop, aes(x = temp.lower, y = estimate__, ymin = lower__,
                                     ymax = upper__, fill = effect2__), alpha = 0.7) +
  theme_bw() +
  theme(axis.text.x = element_text(size=22, face="bold")) +
  theme(axis.text.y = element_text(size=22, face="bold")) +
  theme(axis.title.x = element_text(size=22, face="bold", color="gray30")) +
  theme(axis.title.y = element_text(size=22, face="bold", color="gray30")) +
  scale_x_continuous(name="Temperature Trend",
                     breaks=c(-3, -2, -1, 0, 1, 2)) +
  # scale_x_continuous(name="\u0394Temperature (\u00b0C)/Year",
  #                    breaks=c(-3, -2, -1, 0, 1, 2)) +
  theme(axis.title.y.right = element_text(vjust = 1.7)) +
  scale_y_continuous(name="Abundance Trend", limits = c(-0.0121, 0.005),
                     breaks = c(-0.010, -0.005, 0.000, 0.005),
                     sec.axis = dup_axis(name = "% Change",
                                         labels = c("-2.28", "-1.14", "0.00", "1.16"))) +
  scale_colour_manual(name="\u0394Cropland", values = c('1' = "orange1",
                                                        '0' = "gray40", 
                                                        '-1' = "turquoise4")) +
  scale_fill_manual(name="\u0394Cropland", values = c('1' = "orange1",
                                                      '0' = "gray40", 
                                                      '-1' = "turquoise4")) +
  # theme(legend.position = c(0.80, 0.85)) +
  theme(legend.position = "none")
  # theme(legend.text = element_text(size=18), legend.title = element_text(size = 18)) +
  # guides(colour = guide_legend(override.aes = (list(size=3))))

ggsave(paste(path2, "Temperature and Cropland - Abundance conditional effects.jpg", sep = ""),
       plot=tempcropplotA,
       device = "jpg",
       width=7, height=5, dpi=600)



######################
## Crop :: Precip ##
######################

# Make the dfs

obs.precipcrop <- conditional_effects(observed, effects = "precip.lower:crop.lower")
obs.precipcrop <- as.data.frame(obs.precipcrop$`precip.lower:crop.lower`)
obs.precipcrop$Type <- "observed"
rare.precipcrop <- conditional_effects(rarefied, effects = "precip.lower:crop.lower")
rare.precipcrop <- as.data.frame(rare.precipcrop$`precip.lower:crop.lower`)
rare.precipcrop$Type <- "rarefied"
ab.precipcrop <- conditional_effects(abundance, effects = "precip.lower:crop.lower")
ab.precipcrop <- as.data.frame(ab.precipcrop$`precip.lower:crop.lower`)
ab.precipcrop$Type <- "abundance"

# effect1__ is the range of precip slope values
# effect2__ is the categorical values of canopy: 1, 0, -1

# FYI: effect1__ is identical to whatever first varying
# parameter of interest is)

# Categorical levels are not exactly 1, 0 and negative 1 so they need to be recoded
# First change to numeric then round, then convert back to factor
obs.precipcrop$effect2__ <- factor(round(as.numeric(as.character(obs.precipcrop$effect2__))))
rare.precipcrop$effect2__ <- factor(round(as.numeric(as.character(rare.precipcrop$effect2__))))
ab.precipcrop$effect2__ <- factor(round(as.numeric(as.character(ab.precipcrop$effect2__))))

range(obs.precipcrop$precip.lower)
range(obs.precipcrop$estimate__)
#-0.0027  0.0024
range(rare.precipcrop$estimate__)
#-0.0006 0.0026
range(ab.precipcrop$estimate__)
# -0.01 -0.0046

# Observed
precipcropplot <- ggplot() +
  # Zero lines
  geom_hline(yintercept = 0, color = "black") +
  geom_vline(xintercept = 0, color = "black") +
  geom_line(data = obs.precipcrop, aes(x = precip.lower, y = estimate__, color = effect2__)) +
  geom_ribbon(data = obs.precipcrop, aes(x = precip.lower, y = estimate__, ymin = lower__,
                                        ymax = upper__, fill = effect2__), alpha = 0.7) +
  theme_bw() +
  theme(axis.text.x = element_text(size=22, face="bold")) +
  theme(axis.text.y = element_text(size=22, face="bold")) +
  theme(axis.title.x = element_text(size=22, face="bold", color="gray30")) +
  theme(axis.title.y = element_text(size=22, face="bold", color="gray30")) +
  scale_x_continuous(name="Precipitation Trend",
                     breaks=c(-3, -2, -1, 0, 1, 2, 3, 4)) +
  # scale_x_continuous(name="\u0394Precipitation (mm)/Year",
  #                    breaks=c(-3, -2, -1, 0, 1, 2, 3, 4)) +
  theme(axis.title.y.right = element_text(vjust = 1.7)) +
  scale_y_continuous(name="Observed Richness Trend", limits = c(-0.0121, 0.005),
                     breaks = c(-0.010, -0.005, 0.000, 0.005),
                     sec.axis = dup_axis(name = "% Change",
                                         labels = c("-2.28", "-1.14", "0.00", "1.16"))) +
  scale_colour_manual(name="\u0394Cropland", values = c('1' = "orange1",
                                                        '0' = "gray40", 
                                                        '-1' = "turquoise4")) +
  scale_fill_manual(name="\u0394Cropland", values = c('1' = "orange1",
                                                      '0' = "gray40", 
                                                      '-1' = "turquoise4")) +
  # theme(legend.position = c(0.80, 0.85)) +
  theme(legend.position = "none")
  # theme(legend.text = element_text(size=18), legend.title = element_text(size = 18)) +
  # guides(colour = guide_legend(override.aes = (list(size=3))))

ggsave(paste(path2, "Precipitation and Cropland - Observed conditional effects.jpg", sep = ""),
       plot=precipcropplot,
       device = "jpg",
       width=7, height=5, dpi=600)

# Rarefied
precipcropplotR <- ggplot() +
  # Zero lines
  geom_hline(yintercept = 0, color = "black") +
  geom_vline(xintercept = 0, color = "black") +
  geom_line(data = rare.precipcrop, aes(x = precip.lower, y = estimate__, color = effect2__)) +
  geom_ribbon(data = rare.precipcrop, aes(x = precip.lower, y = estimate__, ymin = lower__,
                                         ymax = upper__, fill = effect2__), alpha = 0.7) +
  theme_bw() +
  theme(axis.text.x = element_text(size=22, face="bold")) +
  theme(axis.text.y = element_text(size=22, face="bold")) +
  theme(axis.title.x = element_text(size=22, face="bold", color="gray30")) +
  theme(axis.title.y = element_text(size=22, face="bold", color="gray30")) +
  scale_x_continuous(name="Precipitation Trend",
                     breaks=c(-3, -2, -1, 0, 1, 2, 3, 4)) +
  # scale_x_continuous(name="\u0394Precipitation (mm)/Year",
  #                    breaks=c(-3, -2, -1, 0, 1, 2, 3, 4)) +
  theme(axis.title.y.right = element_text(vjust = 1.7)) +
  scale_y_continuous(name="Rarefied Richness Trend", limits = c(-0.0121, 0.005),
                     breaks = c(-0.010, -0.005, 0.000, 0.005),
                     sec.axis = dup_axis(name = "% Change",
                                         labels = c("-2.28", "-1.14", "0.00", "1.16"))) +
  scale_colour_manual(name="\u0394Cropland", values = c('1' = "orange1",
                                                        '0' = "gray40", 
                                                        '-1' = "turquoise4")) +
  scale_fill_manual(name="\u0394Cropland", values = c('1' = "orange1",
                                                      '0' = "gray40", 
                                                      '-1' = "turquoise4")) +
  # theme(legend.position = c(0.80, 0.85)) +
  theme(legend.position = "none")
  # theme(legend.text = element_text(size=18), legend.title = element_text(size = 18)) +
  # guides(colour = guide_legend(override.aes = (list(size=3))))

ggsave(paste(path2, "Precipitation and Cropland - Rarefied conditional effects.jpg",
             sep = ""), plot=precipcropplotR,
       device = "jpg",
       width=7, height=5, dpi=600)


# Abundance
precipcropplotA <- ggplot() +
  # Zero lines
  geom_hline(yintercept = 0, color = "black") +
  geom_vline(xintercept = 0, color = "black") +
  geom_line(data = ab.precipcrop, aes(x = precip.lower, y = estimate__, color = effect2__)) +
  geom_ribbon(data = ab.precipcrop, aes(x = precip.lower, y = estimate__, ymin = lower__,
                                       ymax = upper__, fill = effect2__), alpha = 0.7) +
  theme_bw() +
  theme(axis.text.x = element_text(size=22, face="bold")) +
  theme(axis.text.y = element_text(size=22, face="bold")) +
  theme(axis.title.x = element_text(size=22, face="bold", color="gray30")) +
  theme(axis.title.y = element_text(size=22, face="bold", color="gray30")) +
  scale_x_continuous(name="Precipitation Trend",
                     breaks=c(-3, -2, -1, 0, 1, 2, 3, 4)) +
  # scale_x_continuous(name="\u0394Precipitation (mm)/Year",
  #                    breaks=c(-3, -2, -1, 0, 1, 2, 3, 4)) +
  theme(axis.title.y.right = element_text(vjust = 1.7)) +
  scale_y_continuous(name="Abundance Trend", limits = c(-0.0121, 0.005),
                     breaks = c(-0.010, -0.005, 0.000, 0.005),
                     sec.axis = dup_axis(name = "% Change",
                                         labels = c("-2.28", "-1.14", "0.00", "1.16"))) +
  scale_colour_manual(name="\u0394Cropland", values = c('1' = "orange1",
                                                        '0' = "gray40", 
                                                        '-1' = "turquoise4")) +
  scale_fill_manual(name="\u0394Cropland", values = c('1' = "orange1",
                                                      '0' = "gray40", 
                                                      '-1' = "turquoise4")) +
  # theme(legend.position = c(0.80, 0.85)) +
  theme(legend.position = "none")
  # theme(legend.text = element_text(size=18), legend.title = element_text(size = 18)) +
  # guides(colour = guide_legend(override.aes = (list(size=3))))

ggsave(paste(path2, "Precipitation and Cropland - Abundance conditional effects.jpg", sep = ""),
       plot=precipcropplotA,
       device = "jpg",
       width=7, height=5, dpi=600)


####################
## Urban :: Temp ##
####################

# Make the dfs

obs.tempurban <- conditional_effects(observed, effects = "temp.lower:urban.lower")
obs.tempurban <- as.data.frame(obs.tempurban$`temp.lower:urban.lower`)
obs.tempurban$Type <- "observed"
rare.tempurban <- conditional_effects(rarefied, effects = "temp.lower:urban.lower")
rare.tempurban <- as.data.frame(rare.tempurban$`temp.lower:urban.lower`)
rare.tempurban$Type <- "rarefied"
ab.tempurban <- conditional_effects(abundance, effects = "temp.lower:urban.lower")
ab.tempurban <- as.data.frame(ab.tempurban$`temp.lower:urban.lower`)
ab.tempurban$Type <- "abundance"

# effect1__ is the range of temp slope values
# effect2__ is the categorical values of canopy: 1, 0, -1

# FYI: effect1__ is identical to whatever first varying
# parameter of interest is)

# Categorical levels are not exactly 1, 0 and negative 1 so they need to be recoded
# First change to numeric then round, then convert back to factor
obs.tempurban$effect2__ <- factor(round(as.numeric(as.character(obs.tempurban$effect2__))))
rare.tempurban$effect2__ <- factor(round(as.numeric(as.character(rare.tempurban$effect2__))))
ab.tempurban$effect2__ <- factor(round(as.numeric(as.character(ab.tempurban$effect2__))))

range(obs.tempurban$estimate__)
range(rare.tempurban$estimate__)
range(ab.tempurban$estimate__)
# -0.011 0.0024

# Observed
tempurbanplot <- ggplot() +
  # Zero lines
  geom_hline(yintercept = 0, color = "black") +
  geom_vline(xintercept = 0, color = "black") +
  geom_line(data = obs.tempurban, aes(x = temp.lower, y = estimate__, color = effect2__)) +
  geom_ribbon(data = obs.tempurban, aes(x = temp.lower, y = estimate__, ymin = lower__,
                                       ymax = upper__, fill = effect2__), alpha = 0.7) +
  theme_bw() +
  theme(axis.text.x = element_text(size=22, face="bold")) +
  theme(axis.text.y = element_text(size=22, face="bold")) +
  theme(axis.title.x = element_text(size=22, face="bold", color="gray30")) +
  theme(axis.title.y = element_text(size=22, face="bold", color="gray30")) +
  scale_x_continuous(name="Temperature Trend",
                     breaks=c(-3, -2, -1, 0, 1, 2)) +
  # scale_x_continuous(name="\u0394Temperature (\u00b0C)/Year",
  #                    breaks=c(-3, -2, -1, 0, 1, 2)) +
  theme(axis.title.y.right = element_text(vjust = 1.7)) +
  scale_y_continuous(name="Observed Richness Trend", limits = c(-0.0121, 0.005),
                     breaks = c(-0.010, -0.005, 0.000, 0.005),
                     sec.axis = dup_axis(name = "% Change",
                                         labels = c("-2.28", "-1.14", "0.00", "1.16"))) +
  scale_colour_manual(name="\u0394Urban", values = c('1' = "red3",
                                                     '0' = "gray40", 
                                                     '-1' = "blueviolet")) +
  scale_fill_manual(name="\u0394Urban", values = c('1' = "red3",
                                                   '0' = "gray40", 
                                                   '-1' = "blueviolet")) +
  # theme(legend.position = c(0.80, 0.85)) +
  theme(legend.position = "none")
  # theme(legend.text = element_text(size=18), legend.title = element_text(size = 18)) +
  # guides(colour = guide_legend(override.aes = (list(size=3))))

ggsave(paste(path2, "Temperature and Urban - Observed conditional effects.jpg",
             sep = ""),
       plot=tempurbanplot,
       device = "jpg",
       width=7, height=5, dpi=600)

# Rarefied
tempurbanplotR <- ggplot() +
  # Zero lines
  geom_hline(yintercept = 0, color = "black") +
  geom_vline(xintercept = 0, color = "black") +
  geom_line(data = rare.tempurban, aes(x = temp.lower, y = estimate__, color = effect2__)) +
  geom_ribbon(data = rare.tempurban, aes(x = temp.lower, y = estimate__, ymin = lower__,
                                        ymax = upper__, fill = effect2__), alpha = 0.7) +
  theme_bw() +
  theme(axis.text.x = element_text(size=22, face="bold")) +
  theme(axis.text.y = element_text(size=22, face="bold")) +
  theme(axis.title.x = element_text(size=22, face="bold", color="gray30")) +
  theme(axis.title.y = element_text(size=22, face="bold", color="gray30")) +
  scale_x_continuous(name="Temperature Trend",
                     breaks=c(-3, -2, -1, 0, 1, 2)) +
  # scale_x_continuous(name="\u0394Temperature (\u00b0C)/Year",
  #                    breaks=c(-3, -2, -1, 0, 1, 2)) +
  theme(axis.title.y.right = element_text(vjust = 1.7)) +
  scale_y_continuous(name="Rarefied Richness Trend", limits = c(-0.0121, 0.005),
                     breaks = c(-0.010, -0.005, 0.000, 0.005),
                     sec.axis = dup_axis(name = "% Change",
                                         labels = c("-2.28", "-1.14", "0.00", "1.16"))) +
  scale_colour_manual(name="\u0394Urban", values = c('1' = "red3",
                                                     '0' = "gray40", 
                                                     '-1' = "blueviolet")) +
  scale_fill_manual(name="\u0394Urban", values = c('1' = "red3",
                                                   '0' = "gray40", 
                                                   '-1' = "blueviolet")) +
  # theme(legend.position = c(0.80, 0.85)) +
  theme(legend.position = "none")
  # theme(legend.text = element_text(size=18), legend.title = element_text(size = 18)) +
  # guides(colour = guide_legend(override.aes = (list(size=3))))

ggsave(paste(path2, "Temperature and Urban - Rarefied conditional effects.jpg",
             sep = ""),
       plot=tempurbanplotR,
       device = "jpg",
       width=7, height=5, dpi=600)


# Abundance
tempurbanplotA <- ggplot() +
  # Zero lines
  geom_hline(yintercept = 0, color = "black") +
  geom_vline(xintercept = 0, color = "black") +
  geom_line(data = ab.tempurban, aes(x = temp.lower, y = estimate__, color = effect2__)) +
  geom_ribbon(data = ab.tempurban, aes(x = temp.lower, y = estimate__, ymin = lower__,
                                      ymax = upper__, fill = effect2__), alpha = 0.7) +
  theme_bw() +
  theme(axis.text.x = element_text(size=22, face="bold")) +
  theme(axis.text.y = element_text(size=22, face="bold")) +
  theme(axis.title.x = element_text(size=22, face="bold", color="gray30")) +
  theme(axis.title.y = element_text(size=22, face="bold", color="gray30")) +
  scale_x_continuous(name="Temperature Trend",
                     breaks=c(-3, -2, -1, 0, 1, 2)) +
  # scale_x_continuous(name="\u0394Temperature (\u00b0C)/Year",
  #                    breaks=c(-3, -2, -1, 0, 1, 2)) +
  theme(axis.title.y.right = element_text(vjust = 1.7)) +
  scale_y_continuous(name="Abundance Trend", limits = c(-0.0121, 0.005),
                     breaks = c(-0.010, -0.005, 0.000, 0.005),
                     sec.axis = dup_axis(name = "% Change",
                                         labels = c("-2.28", "-1.14", "0.00", "1.16"))) +
  scale_colour_manual(name="\u0394Urban", values = c('1' = "red3",
                                                     '0' = "gray40", 
                                                     '-1' = "blueviolet")) +
  scale_fill_manual(name="\u0394Urban", values = c('1' = "red3",
                                                   '0' = "gray40", 
                                                   '-1' = "blueviolet")) +
  # theme(legend.position = c(0.80, 0.85)) +
  theme(legend.position = "none")
  # theme(legend.text = element_text(size=18), legend.title = element_text(size = 18)) +
  # guides(colour = guide_legend(override.aes = (list(size=3))))

ggsave(paste(path2, "Temperature and Urban - Abundance conditional effects.jpg",
             sep = ""),
       plot=tempurbanplotA,
       device = "jpg",
       width=7, height=5, dpi=600)


######################
## Urban :: Precip ##
######################

# Make the dfs

obs.precipurban <- conditional_effects(observed, effects = "precip.lower:urban.lower")
obs.precipurban <- as.data.frame(obs.precipurban$`precip.lower:urban.lower`)
obs.precipurban$Type <- "observed"
rare.precipurban <- conditional_effects(rarefied, effects = "precip.lower:urban.lower")
rare.precipurban <- as.data.frame(rare.precipurban$`precip.lower:urban.lower`)
rare.precipurban$Type <- "rarefied"
ab.precipurban <- conditional_effects(abundance, effects = "precip.lower:urban.lower")
ab.precipurban <- as.data.frame(ab.precipurban$`precip.lower:urban.lower`)
ab.precipurban$Type <- "abundance"

# effect1__ is the range of precip slope values
# effect2__ is the categorical values of canopy: 1, 0, -1

# FYI: effect1__ is identical to whatever first varying
# parameter of interest is)

# Categorical levels are not exactly 1, 0 and negative 1 so they need to be recoded
# First change to numeric then round, then convert back to factor
obs.precipurban$effect2__ <- factor(round(as.numeric(as.character(obs.precipurban$effect2__))))
rare.precipurban$effect2__ <- factor(round(as.numeric(as.character(rare.precipurban$effect2__))))
ab.precipurban$effect2__ <- factor(round(as.numeric(as.character(ab.precipurban$effect2__))))

range(obs.precipurban$precip.lower)
range(obs.precipurban$estimate__)
#-0.0027  0.0024
range(rare.precipurban$estimate__)
#-0.0006 0.0026
range(ab.precipurban$estimate__)
# -0.01 -0.0046

# Observed
precipurbanplot <- ggplot() +
  # Zero lines
  geom_hline(yintercept = 0, color = "black") +
  geom_vline(xintercept = 0, color = "black") +
  geom_line(data = obs.precipurban, aes(x = precip.lower, y = estimate__, color = effect2__)) +
  geom_ribbon(data = obs.precipurban, aes(x = precip.lower, y = estimate__, ymin = lower__,
                                         ymax = upper__, fill = effect2__), alpha = 0.7) +
  theme_bw() +
  theme(axis.text.x = element_text(size=22, face="bold")) +
  theme(axis.text.y = element_text(size=22, face="bold")) +
  theme(axis.title.x = element_text(size=22, face="bold", color="gray30")) +
  theme(axis.title.y = element_text(size=22, face="bold", color="gray30")) +
  scale_x_continuous(name="Precipitation Trend",
                     breaks=c(-3, -2, -1, 0, 1, 2, 3, 4)) +
  # scale_x_continuous(name="\u0394Precipitation (mm)/Year",
  #                    breaks=c(-3, -2, -1, 0, 1, 2, 3, 4)) +
  theme(axis.title.y.right = element_text(vjust = 1.7)) +
  scale_y_continuous(name="Observed Richness Trend", limits = c(-0.0121, 0.005),
                     breaks = c(-0.010, -0.005, 0.000, 0.005),
                     sec.axis = dup_axis(name = "% Change",
                                         labels = c("-2.28", "-1.14", "0.00", "1.16"))) +
  scale_colour_manual(name="\u0394Urban", values = c('1' = "red3",
                                                     '0' = "gray40", 
                                                     '-1' = "blueviolet")) +
  scale_fill_manual(name="\u0394Urban", values = c('1' = "red3",
                                                   '0' = "gray40", 
                                                   '-1' = "blueviolet")) +
  # theme(legend.position = c(0.80, 0.85)) +
  theme(legend.position = "none")
  # theme(legend.text = element_text(size=18), legend.title = element_text(size = 18)) +
  # guides(colour = guide_legend(override.aes = (list(size=3))))

ggsave(paste(path2, "Precipitation and Urban - Observed conditional effects.jpg", sep = ""),
       plot=precipurbanplot,
       device = "jpg",
       width=7, height=5, dpi=600)

# Rarefied
precipurbanplotR <- ggplot() +
  # Zero lines
  geom_hline(yintercept = 0, color = "black") +
  geom_vline(xintercept = 0, color = "black") +
  geom_line(data = rare.precipurban, aes(x = precip.lower, y = estimate__, color = effect2__)) +
  geom_ribbon(data = rare.precipurban, aes(x = precip.lower, y = estimate__, ymin = lower__,
                                          ymax = upper__, fill = effect2__), alpha = 0.7) +
  theme_bw() +
  theme(axis.text.x = element_text(size=22, face="bold")) +
  theme(axis.text.y = element_text(size=22, face="bold")) +
  theme(axis.title.x = element_text(size=22, face="bold", color="gray30")) +
  theme(axis.title.y = element_text(size=22, face="bold", color="gray30")) +
  scale_x_continuous(name="Precipitation Trend",
                     breaks=c(-3, -2, -1, 0, 1, 2, 3, 4)) +
  # scale_x_continuous(name="\u0394Precipitation (mm)/Year",
  #                    breaks=c(-3, -2, -1, 0, 1, 2, 3, 4)) +
  theme(axis.title.y.right = element_text(vjust = 1.7)) +
  scale_y_continuous(name="Rarefied Richness Trend", limits = c(-0.0121, 0.005),
                     breaks = c(-0.010, -0.005, 0.000, 0.005),
                     sec.axis = dup_axis(name = "% Change",
                                         labels = c("-2.28", "-1.14", "0.00", "1.16"))) +
  scale_colour_manual(name="\u0394Urban", values = c('1' = "red3",
                                                     '0' = "gray40", 
                                                     '-1' = "blueviolet")) +
  scale_fill_manual(name="\u0394Urban", values = c('1' = "red3",
                                                   '0' = "gray40", 
                                                   '-1' = "blueviolet")) +
  # theme(legend.position = c(0.80, 0.85)) +
  theme(legend.position = "none")
  # theme(legend.text = element_text(size=18), legend.title = element_text(size = 18)) +
  # guides(colour = guide_legend(override.aes = (list(size=3))))

ggsave(paste(path2, "Precipitation and Urban - Rarefied conditional effects.jpg", sep = ""),
       plot=precipurbanplotR,
       device = "jpg",
       width=7, height=5, dpi=600)


# Abundance
precipurbanplotA <- ggplot() +
  # Zero lines
  geom_hline(yintercept = 0, color = "black") +
  geom_vline(xintercept = 0, color = "black") +
  geom_line(data = ab.precipurban, aes(x = precip.lower, y = estimate__, color = effect2__)) +
  geom_ribbon(data = ab.precipurban, aes(x = precip.lower, y = estimate__, ymin = lower__,
                                        ymax = upper__, fill = effect2__), alpha = 0.7) +
  theme_bw() +
  theme(axis.text.x = element_text(size=22, face="bold")) +
  theme(axis.text.y = element_text(size=22, face="bold")) +
  theme(axis.title.x = element_text(size=22, face="bold", color="gray30")) +
  theme(axis.title.y = element_text(size=22, face="bold", color="gray30")) +
  scale_x_continuous(name="Precipitation Trend",
                     breaks=c(-3, -2, -1, 0, 1, 2, 3, 4)) +
  # scale_x_continuous(name="\u0394Precipitation (mm)/Year",
  #                    breaks=c(-3, -2, -1, 0, 1, 2, 3, 4)) +
  theme(axis.title.y.right = element_text(vjust = 1.7)) +
  scale_y_continuous(name="Abundance Trend", limits = c(-0.0121, 0.005),
                     breaks = c(-0.010, -0.005, 0.000, 0.005),
                     sec.axis = dup_axis(name = "% Change",
                                         labels = c("-2.28", "-1.14", "0.00", "1.16"))) +
  scale_colour_manual(name="\u0394Urban", values = c('1' = "red3",
                                                     '0' = "gray40", 
                                                     '-1' = "blueviolet")) +
  scale_fill_manual(name="\u0394Urban", values = c('1' = "red3",
                                                   '0' = "gray40", 
                                                   '-1' = "blueviolet")) +
  # theme(legend.position = c(0.80, 0.85)) +
  theme(legend.position = "none")
  # theme(legend.text = element_text(size=18), legend.title = element_text(size = 18)) +
  # guides(colour = guide_legend(override.aes = (list(size=3))))

ggsave(paste(path2, "Precipitation and Urban - Abundance conditional effects.jpg", sep = ""),
       plot=precipurbanplotA,
       device = "jpg",
       width=7, height=5, dpi=600)



######################
## Water :: Precip  ##
######################

# Make the dfs

obs.precipwater <- conditional_effects(observed, effects = "precip.lower:water.lower")
obs.precipwater <- as.data.frame(obs.precipwater$`precip.lower:water.lower`)
obs.precipwater$Type <- "observed"
rare.precipwater <- conditional_effects(rarefied, effects = "precip.lower:water.lower")
rare.precipwater <- as.data.frame(rare.precipwater$`precip.lower:water.lower`)
rare.precipwater$Type <- "rarefied"
ab.precipwater <- conditional_effects(abundance, effects = "precip.lower:water.lower")
ab.precipwater <- as.data.frame(ab.precipwater$`precip.lower:water.lower`)
ab.precipwater$Type <- "abundance"

# effect1__ is the range of precip slope values
# effect2__ is the categorical values of water: 1, 0, -1

# FYI: effect1__ is identical to whatever first varying
# parameter of interest is)

# Categorical levels are not exactly 1, 0 and negative 1 so they need to be recoded
# First change to numeric then round, then convert back to factor
obs.precipwater$effect2__ <- factor(round(as.numeric(as.character(obs.precipwater$effect2__))))
rare.precipwater$effect2__ <- factor(round(as.numeric(as.character(rare.precipwater$effect2__))))
ab.precipwater$effect2__ <- factor(round(as.numeric(as.character(ab.precipwater$effect2__))))

range(obs.precipwater$precip.lower)
range(obs.precipwater$estimate__)
#-0.0036  0.0026
range(rare.precipwater$estimate__)
#-0.0006 0.0026
range(ab.precipwater$estimate__)
# -0.0111 -0.0046

# Observed
precipwaterplot <- ggplot() +
  # Zero lines
  geom_hline(yintercept = 0, color = "black") +
  geom_vline(xintercept = 0, color = "black") +
  geom_line(data = obs.precipwater, aes(x = precip.lower, y = estimate__, color = effect2__)) +
  geom_ribbon(data = obs.precipwater, aes(x = precip.lower, y = estimate__, ymin = lower__,
                                          ymax = upper__, fill = effect2__), alpha = 0.7) +
  theme_bw() +
  theme(axis.text.x = element_text(size=22, face="bold")) +
  theme(axis.text.y = element_text(size=22, face="bold")) +
  theme(axis.title.x = element_text(size=22, face="bold", color="gray30")) +
  theme(axis.title.y = element_text(size=22, face="bold", color="gray30")) +
  scale_x_continuous(name="Precipitation Trend",
                     breaks=c(-3, -2, -1, 0, 1, 2, 3, 4)) +
  # scale_x_continuous(name="\u0394Precipitation (mm)/Year",
  #                    breaks=c(-3, -2, -1, 0, 1, 2, 3, 4)) +
  theme(axis.title.y.right = element_text(vjust = 1.7)) +
  scale_y_continuous(name="Observed Richness Trend", limits = c(-0.0121, 0.005),
                     breaks = c(-0.010, -0.005, 0.000, 0.005),
                     sec.axis = dup_axis(name = "% Change",
                                         labels = c("-2.28", "-1.14", "0.00", "1.16"))) +
  scale_colour_manual(name="\u0394Water", values = c('1' = "blue4",
                                                     '0' = "gray40", 
                                                     '-1' = "turquoise4")) +
  scale_fill_manual(name="\u0394Water", values = c('1' = "blue4",
                                                   '0' = "gray40", 
                                                   '-1' = "turquoise4")) +
  # theme(legend.position = c(0.80, 0.85)) +
  theme(legend.position = "none")
  # theme(legend.text = element_text(size=18), legend.title = element_text(size = 18)) +
  # guides(colour = guide_legend(override.aes = (list(size=3))))

ggsave(paste(path2, "Precipitation and Water - Observed conditional effects.jpg", sep = ""),
       plot=precipwaterplot,
       device = "jpg",
       width=7, height=5, dpi=600)

# Rarefied
precipwaterplotR <- ggplot() +
  # Zero lines
  geom_hline(yintercept = 0, color = "black") +
  geom_vline(xintercept = 0, color = "black") +
  geom_line(data = rare.precipwater, aes(x = precip.lower, y = estimate__, color = effect2__)) +
  geom_ribbon(data = rare.precipwater, aes(x = precip.lower, y = estimate__, ymin = lower__,
                                          ymax = upper__, fill = effect2__), alpha = 0.7) +
  theme_bw() +
  theme(axis.text.x = element_text(size=22, face="bold")) +
  theme(axis.text.y = element_text(size=22, face="bold")) +
  theme(axis.title.x = element_text(size=22, face="bold", color="gray30")) +
  theme(axis.title.y = element_text(size=22, face="bold", color="gray30")) +
  scale_x_continuous(name="Precipitation Trend",
                     breaks=c(-3, -2, -1, 0, 1, 2, 3, 4)) +
  # scale_x_continuous(name="\u0394Precipitation (mm)/Year",
  #                    breaks=c(-3, -2, -1, 0, 1, 2, 3, 4)) +
  theme(axis.title.y.right = element_text(vjust = 1.7)) +
  scale_y_continuous(name="Rarefied Richness Trend", limits = c(-0.0121, 0.005),
                     breaks = c(-0.010, -0.005, 0.000, 0.005),
                     sec.axis = dup_axis(name = "% Change",
                                         labels = c("-2.28", "-1.14", "0.00", "1.16"))) +
  scale_colour_manual(name="\u0394Water", values = c('1' = "blue4",
                                                     '0' = "gray40", 
                                                     '-1' = "turquoise4")) +
  scale_fill_manual(name="\u0394Water", values = c('1' = "blue4",
                                                   '0' = "gray40", 
                                                   '-1' = "turquoise4")) +
  # theme(legend.position = c(0.80, 0.85)) +
  theme(legend.position = "none")
  # theme(legend.text = element_text(size=18), legend.title = element_text(size = 18)) +
  # guides(colour = guide_legend(override.aes = (list(size=3))))

ggsave(paste(path2, "Precipitation and Water - Rarefied conditional effects.jpg", sep = ""),
       plot=precipwaterplotR,
       device = "jpg",
       width=7, height=5, dpi=600)


# Abundance
precipwaterplotA <- ggplot() +
  # Zero lines
  geom_hline(yintercept = 0, color = "black") +
  geom_vline(xintercept = 0, color = "black") +
  geom_line(data = ab.precipwater, aes(x = precip.lower, y = estimate__, color = effect2__)) +
  geom_ribbon(data = ab.precipwater, aes(x = precip.lower, y = estimate__, ymin = lower__,
                                           ymax = upper__, fill = effect2__), alpha = 0.7) +
  theme_bw() +
  theme(axis.text.x = element_text(size=22, face="bold")) +
  theme(axis.text.y = element_text(size=22, face="bold")) +
  theme(axis.title.x = element_text(size=22, face="bold", color="gray30")) +
  theme(axis.title.y = element_text(size=22, face="bold", color="gray30")) +
  scale_x_continuous(name="Precipitation Trend",
                     breaks=c(-3, -2, -1, 0, 1, 2, 3, 4)) +
  # scale_x_continuous(name="\u0394Precipitation (mm)/Year",
  #                    breaks=c(-3, -2, -1, 0, 1, 2, 3, 4)) +
  theme(axis.title.y.right = element_text(vjust = 1.7)) +
  scale_y_continuous(name="Abundance Trend", limits = c(-0.0121, 0.005),
                     breaks = c(-0.010, -0.005, 0.000, 0.005),
                     sec.axis = dup_axis(name = "% Change",
                                         labels = c("-2.28", "-1.14", "0.00", "1.16"))) +
  scale_colour_manual(name="\u0394Water", values = c('1' = "blue4",
                                                     '0' = "gray40", 
                                                     '-1' = "turquoise4")) +
  scale_fill_manual(name="\u0394Water", values = c('1' = "blue4",
                                                   '0' = "gray40", 
                                                   '-1' = "turquoise4")) +
  # theme(legend.position = c(0.80, 0.85)) +
  theme(legend.position = "none")
  # theme(legend.text = element_text(size=18), legend.title = element_text(size = 18)) +
  # guides(colour = guide_legend(override.aes = (list(size=3))))

ggsave(paste(path2, "Precipitation and Water - Abundance conditional effects.jpg", sep = ""),
       plot=precipwaterplotA,
       device = "jpg",
       width=7, height=5, dpi=600)



######################
## Water :: temp    ##
######################

# Make the dfs

obs.tempwater <- conditional_effects(observed, effects = "temp.lower:water.lower")
obs.tempwater <- as.data.frame(obs.tempwater$`temp.lower:water.lower`)
obs.tempwater$Type <- "observed"
rare.tempwater <- conditional_effects(rarefied, effects = "temp.lower:water.lower")
rare.tempwater <- as.data.frame(rare.tempwater$`temp.lower:water.lower`)
rare.tempwater$Type <- "rarefied"
ab.tempwater <- conditional_effects(abundance, effects = "temp.lower:water.lower")
ab.tempwater <- as.data.frame(ab.tempwater$`temp.lower:water.lower`)
ab.tempwater$Type <- "abundance"

# effect1__ is the range of temp slope values
# effect2__ is the categorical values of water: 1, 0, -1

# FYI: effect1__ is identical to whatever first varying
# parameter of interest is)

# Categorical levels are not exactly 1, 0 and negative 1 so they need to be recoded
# First change to numeric then round, then convert back to factor
obs.tempwater$effect2__ <- factor(round(as.numeric(as.character(obs.tempwater$effect2__))))
rare.tempwater$effect2__ <- factor(round(as.numeric(as.character(rare.tempwater$effect2__))))
ab.tempwater$effect2__ <- factor(round(as.numeric(as.character(ab.tempwater$effect2__))))

range(obs.tempwater$temp.lower)
range(obs.tempwater$estimate__)
#-0.0036  0.0026
range(rare.tempwater$estimate__)
#-0.0006 0.0026
range(ab.tempwater$estimate__)
# -0.0111 -0.0046

# Observed
tempwaterplot <- ggplot() +
  # Zero lines
  geom_hline(yintercept = 0, color = "black") +
  geom_vline(xintercept = 0, color = "black") +
  geom_line(data = obs.tempwater, aes(x = temp.lower, y = estimate__, color = effect2__)) +
  geom_ribbon(data = obs.tempwater, aes(x = temp.lower, y = estimate__, ymin = lower__,
                                          ymax = upper__, fill = effect2__), alpha = 0.7) +
  theme_bw() +
  theme(axis.text.x = element_text(size=22, face="bold")) +
  theme(axis.text.y = element_text(size=22, face="bold")) +
  theme(axis.title.x = element_text(size=22, face="bold", color="gray30")) +
  theme(axis.title.y = element_text(size=22, face="bold", color="gray30")) +
  scale_x_continuous(name="Temperature Trend",
                     breaks=c(-3, -2, -1, 0, 1, 2)) +
  # scale_x_continuous(name="\u0394Temperature (\u00b0C)/Year",
  #                    breaks=c(-3, -2, -1, 0, 1, 2)) +
  theme(axis.title.y.right = element_text(vjust = 1.7)) +
  scale_y_continuous(name="Observed Richness Trend", limits = c(-0.0121, 0.005),
                     breaks = c(-0.010, -0.005, 0.000, 0.005),
                     sec.axis = dup_axis(name = "% Change",
                                         labels = c("-2.28", "-1.14", "0.00", "1.16"))) +
  scale_colour_manual(name="\u0394Water", values = c('1' = "blue4",
                                                     '0' = "gray40", 
                                                     '-1' = "turquoise4")) +
  scale_fill_manual(name="\u0394Water", values = c('1' = "blue4",
                                                   '0' = "gray40", 
                                                   '-1' = "turquoise4")) +
  # theme(legend.position = c(0.80, 0.85)) +
  theme(legend.position = "none")
  # theme(legend.text = element_text(size=18), legend.title = element_text(size = 18)) +
  # guides(colour = guide_legend(override.aes = (list(size=3))))

ggsave(paste(path2, "Temperature and Water - Observed conditional effects.jpg", 
             sep = ""),
       plot=tempwaterplot,
       device = "jpg",
       width=7, height=5, dpi=600)

# Rarefied
tempwaterplotR <- ggplot() +
  # Zero lines
  geom_hline(yintercept = 0, color = "black") +
  geom_vline(xintercept = 0, color = "black") +
  geom_line(data = rare.tempwater, aes(x = temp.lower, y = estimate__, color = effect2__)) +
  geom_ribbon(data = rare.tempwater, aes(x = temp.lower, y = estimate__, ymin = lower__,
                                           ymax = upper__, fill = effect2__), alpha = 0.7) +
  theme_bw() +
  theme(axis.text.x = element_text(size=22, face="bold")) +
  theme(axis.text.y = element_text(size=22, face="bold")) +
  theme(axis.title.x = element_text(size=22, face="bold", color="gray30")) +
  theme(axis.title.y = element_text(size=22, face="bold", color="gray30")) +
  scale_x_continuous(name="Temperature Trend",
                     breaks=c(-3, -2, -1, 0, 1, 2)) +
  # scale_x_continuous(name="\u0394Temperature (\u00b0C)/Year",
  #                    breaks=c(-3, -2, -1, 0, 1, 2)) +
  theme(axis.title.y.right = element_text(vjust = 1.7)) +
  scale_y_continuous(name="Rarefied Richness Trend", limits = c(-0.0121, 0.005),
                     breaks = c(-0.010, -0.005, 0.000, 0.005),
                     sec.axis = dup_axis(name = "% Change",
                                         labels = c("-2.28", "-1.14", "0.00", "1.16"))) +
  scale_colour_manual(name="\u0394Water", values = c('1' = "blue4",
                                                     '0' = "gray40", 
                                                     '-1' = "turquoise4")) +
  scale_fill_manual(name="\u0394Water", values = c('1' = "blue4",
                                                   '0' = "gray40", 
                                                   '-1' = "turquoise4")) +
  # theme(legend.position = c(0.80, 0.85)) +
  theme(legend.position = "none")
  # theme(legend.text = element_text(size=18), legend.title = element_text(size = 18)) +
  # guides(colour = guide_legend(override.aes = (list(size=3))))

ggsave(paste(path2, "Temperature and Water - Rarefied conditional effects.jpg",
             sep = ""),
       plot=tempwaterplotR,
       device = "jpg",
       width=7, height=5, dpi=600)


# Abundance
tempwaterplotA <- ggplot() +
  # Zero lines
  geom_hline(yintercept = 0, color = "black") +
  geom_vline(xintercept = 0, color = "black") +
  geom_line(data = ab.tempwater, aes(x = temp.lower, y = estimate__, color = effect2__)) +
  geom_ribbon(data = ab.tempwater, aes(x = temp.lower, y = estimate__, ymin = lower__,
                                         ymax = upper__, fill = effect2__), alpha = 0.7) +
  theme_bw() +
  theme(axis.text.x = element_text(size=22, face="bold")) +
  theme(axis.text.y = element_text(size=22, face="bold")) +
  theme(axis.title.x = element_text(size=22, face="bold", color="gray30")) +
  theme(axis.title.y = element_text(size=22, face="bold", color="gray30")) +
  scale_x_continuous(name="Temperature Trend",
                     breaks=c(-3, -2, -1, 0, 1, 2)) +
  # scale_x_continuous(name="\u0394Temperature (\u00b0C)/Year",
  #                    breaks=c(-3, -2, -1, 0, 1, 2)) +
  theme(axis.title.y.right = element_text(vjust = 1.7)) +
  scale_y_continuous(name="Abundance Trend", limits = c(-0.0121, 0.005),
                     breaks = c(-0.010, -0.005, 0.000, 0.005),
                     sec.axis = dup_axis(name = "% Change",
                                         labels = c("-2.28", "-1.14", "0.00", "1.16"))) +
  scale_colour_manual(name="\u0394Water", values = c('1' = "blue4",
                                                     '0' = "gray40", 
                                                     '-1' = "turquoise4")) +
  scale_fill_manual(name="\u0394Water", values = c('1' = "blue4",
                                                   '0' = "gray40", 
                                                   '-1' = "turquoise4")) +
  # theme(legend.position = c(0.80, 0.85)) +
  theme(legend.position = "none")
  # theme(legend.text = element_text(size=18), legend.title = element_text(size = 18)) +
  # guides(colour = guide_legend(override.aes = (list(size=3))))

ggsave(paste(path2, "Temperature and Water - Abundance conditional effects.jpg", sep = ""),
       plot=tempwaterplotA,
       device = "jpg",
       width=7, height=5, dpi=600)

