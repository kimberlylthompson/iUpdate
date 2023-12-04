#################################################################################################
########                                                                                 ########
########                  Graphs of conditional effects for interactions                 ######## 
########                   btw predictors for each biodiversity response                 ########
#################################################################################################

# Author: Kimberly Thompson

# This code uses the three brms models constructed for observed richness, rarefied richness,
# abundance (1 model for each) and creates graphs of the results.


# Plots created:
# Figure 3
# Figure 4


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


# % change values for axis labels
# 0.000


# -0.005
(10^(-0.005 * 1)-1) * 100 # -1.14

# -0.010
(10^(-0.010 * 1)-1) * 100 # -2.28

# 0.005
(10^(0.005 * 1)-1) * 100 # 1.16


############################################
###            Interaction Plots         ###
############################################

####################
## Canopy :: Temp ##
####################

# Make the dfs

obs.tempcan <- conditional_effects(observed, effects = "SLOPE_temp_400m:SLOPE_canopy.mean_400m")
obs.tempcan <- as.data.frame(obs.tempcan$`SLOPE_temp_400m:SLOPE_canopy.mean_400m`)
obs.tempcan$Type <- "observed"
rare.tempcan <- conditional_effects(rarefied, effects = "SLOPE_temp_400m:SLOPE_canopy.mean_400m")
rare.tempcan <- as.data.frame(rare.tempcan$`SLOPE_temp_400m:SLOPE_canopy.mean_400m`)
rare.tempcan$Type <- "rarefied"
ab.tempcan <- conditional_effects(abundance, effects = "SLOPE_temp_400m:SLOPE_canopy.mean_400m")
ab.tempcan <- as.data.frame(ab.tempcan$`SLOPE_temp_400m:SLOPE_canopy.mean_400m`)
ab.tempcan$Type <- "abundance"

# effect1__ is the range of temperature slope values
# effect2__ is the categorical values of canopy: 1, 0, -1

# FYI: effect1__ is identical to SLOPE_temp_400m (will be identical to whatever first varying
# parameter of interest is)

# -0.01. 0.0035 - 

#### CANOPY CHANGE LEGEND ####

# Observed
tempcanplot <- ggplot() +
  # Zero lines
  geom_hline(yintercept = 0, color = "black") +
  geom_vline(xintercept = 0, color = "black") +
  geom_line(data = obs.tempcan, aes(x = SLOPE_temp_400m, y = estimate__, color = effect2__)) +
  geom_ribbon(data = obs.tempcan, aes(x = SLOPE_temp_400m, y = estimate__, ymin = lower__,
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
  scale_y_continuous(name="Observed Richness Trend", limits = c(-0.0121, 0.004),
                     breaks = c(-0.010, -0.005, 0.000),
                     sec.axis = dup_axis(name = "% Change",
                                         labels = c("-2.28", "-1.14", "0.00"))) +
  # scale_y_continuous(name="\u0394log(S)/Year", limits = c(-0.0121, 0.004),
  #                    breaks = c(-0.010, -0.005, 0.000)) +
  scale_colour_manual(name="Tree-canopy\nTrend", values = c('1' = "forestgreen",
                                                            '0' = "gray40", 
                                                            '-1' = "brown4")) +
  scale_fill_manual(name="Tree-canopy\nTrend", values = c('1' = "forestgreen",
                                                          '0' = "gray40", 
                                                          '-1' = "brown4")) +
  # theme(legend.position = c(0.80, 0.85)) +
  # theme(legend.position = "none")
  theme(legend.text = element_text(size=18), legend.title = element_text(size = 18),
        legend.title.align = 0.5) +
  guides(colour = guide_legend(override.aes = (list(size=3))))

path2 <- "01_Analysis/Model_Plotting/Interaction Conditional Effects/Main Model CC and LUCC only/"
ggsave(paste(path2,
             "Legend - canopy change.jpg",
             sep = ""),
       plot=tempcanplot,
       device = "jpg",
       width=7, height=5, dpi=600)


# Observed
tempcanplot <- ggplot() +
  # Zero lines
  geom_hline(yintercept = 0, color = "black") +
  geom_vline(xintercept = 0, color = "black") +
  geom_line(data = obs.tempcan, aes(x = SLOPE_temp_400m, y = estimate__, color = effect2__)) +
  geom_ribbon(data = obs.tempcan, aes(x = SLOPE_temp_400m, y = estimate__, ymin = lower__,
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
  scale_y_continuous(name="Observed Richness Trend", limits = c(-0.0121, 0.004),
                     breaks = c(-0.010, -0.005, 0.000),
                     sec.axis = dup_axis(name = "% Change",
                                         labels = c("-2.28", "-1.14", "0.00"))) +
  # scale_y_continuous(name="\u0394log(S)/Year", limits = c(-0.0121, 0.004),
  #                    breaks = c(-0.010, -0.005, 0.000)) +
  scale_colour_manual(name="Tree-canopy\nTrend", values = c('1' = "forestgreen",
                                                   '0' = "gray40", 
                                                   '-1' = "brown4")) +
  scale_fill_manual(name="Tree-canopy\nTrend", values = c('1' = "forestgreen",
                                                 '0' = "gray40", 
                                                 '-1' = "brown4")) +
  # theme(legend.position = c(0.80, 0.85)) +
  theme(legend.position = "none")
  # theme(legend.text = element_text(size=18), legend.title = element_text(size = 18),
  #       legend.title.align = 0.5) +
  # guides(colour = guide_legend(override.aes = (list(size=3))))

ggsave(paste(path2,
             "Temperature and Canopy - Observed conditional effects.jpg",
             sep = ""),
       plot=tempcanplot,
       device = "jpg",
       width=7, height=5, dpi=600)

# Rarefied
tempcanplotR <- ggplot() +
  # Zero lines
  geom_hline(yintercept = 0, color = "black") +
  geom_vline(xintercept = 0, color = "black") +
  geom_line(data = rare.tempcan, aes(x = SLOPE_temp_400m, y = estimate__, color = effect2__)) +
  geom_ribbon(data = rare.tempcan, aes(x = SLOPE_temp_400m, y = estimate__, ymin = lower__,
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
  scale_y_continuous(name="Rarefied Richness Trend", limits = c(-0.0121, 0.004),
                     breaks = c(-0.010, -0.005, 0.000),
                     sec.axis = dup_axis(name = "% Change",
                                         labels = c("-2.28", "-1.14", "0.00"))) +
  # scale_y_continuous(name="\u0394log(S)/Year", limits = c(-0.0121, 0.004),
  #                    breaks = c(-0.010, -0.005, 0.000)) +
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

ggsave(paste(path2,
             "Temperature and Canopy - Rarefied conditional effects.jpg",
             sep = ""),
       plot=tempcanplotR,
       device = "jpg",
       width=7, height=5, dpi=600)


# Abundance
tempcanplotA <- ggplot() +
  # Zero lines
  geom_hline(yintercept = 0, color = "black") +
  geom_vline(xintercept = 0, color = "black") +
  geom_line(data = ab.tempcan, aes(x = SLOPE_temp_400m, y = estimate__, color = effect2__)) +
  geom_ribbon(data = ab.tempcan, aes(x = SLOPE_temp_400m, y = estimate__, ymin = lower__,
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
  scale_y_continuous(name="Abundance Trend", limits = c(-0.0121, 0.004),
                     breaks = c(-0.010, -0.005, 0.000),
                     sec.axis = dup_axis(name = "% Change",
                                         labels = c("-2.28", "-1.14", "0.00"))) +
  # scale_y_continuous(name="\u0394log(S)/Year", limits = c(-0.0121, 0.004),
  #                    breaks = c(-0.010, -0.005, 0.000)) +
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

ggsave(paste(path2,
             "Temperature and Canopy - Abundance conditional effects.jpg",
             sep = ""),
       plot=tempcanplotA,
       device = "jpg",
       width=7, height=5, dpi=600)



######################
## Canopy :: Precip ##
######################

# Make the dfs

obs.precipcan <- conditional_effects(observed, effects = "SLOPE_precip_400m:SLOPE_canopy.mean_400m")
obs.precipcan <- as.data.frame(obs.precipcan$`SLOPE_precip_400m:SLOPE_canopy.mean_400m`)
obs.precipcan$Type <- "observed"
rare.precipcan <- conditional_effects(rarefied, effects = "SLOPE_precip_400m:SLOPE_canopy.mean_400m")
rare.precipcan <- as.data.frame(rare.precipcan$`SLOPE_precip_400m:SLOPE_canopy.mean_400m`)
rare.precipcan$Type <- "rarefied"
ab.precipcan <- conditional_effects(abundance, effects = "SLOPE_precip_400m:SLOPE_canopy.mean_400m")
ab.precipcan <- as.data.frame(ab.precipcan$`SLOPE_precip_400m:SLOPE_canopy.mean_400m`)
ab.precipcan$Type <- "abundance"

# effect1__ is the range of precip slope values
# effect2__ is the categorical values of canopy: 1, 0, -1

# FYI: effect1__ is identical to whatever first varying
# parameter of interest is)

range(obs.precipcan$SLOPE_precip_400m)
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
  geom_line(data = obs.precipcan, aes(x = SLOPE_precip_400m, y = estimate__, color = effect2__)) +
  geom_ribbon(data = obs.precipcan, aes(x = SLOPE_precip_400m, y = estimate__, ymin = lower__,
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
  scale_y_continuous(name="Observed Richness Trend", limits = c(-0.0121, 0.004),
                     breaks = c(-0.010, -0.005, 0.000),
                     sec.axis = dup_axis(name = "% Change",
                                         labels = c("-2.28", "-1.14", "0.00"))) +
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

ggsave(paste(path2,
             "Precipitation and Canopy - Observed conditional effects.jpg",
             sep = ""),
       plot=precipcanplot,
       device = "jpg",
       width=7, height=5, dpi=600)

# Rarefied
precipcanplotR <- ggplot() +
  # Zero lines
  geom_hline(yintercept = 0, color = "black") +
  geom_vline(xintercept = 0, color = "black") +
  geom_line(data = rare.precipcan, aes(x = SLOPE_precip_400m, y = estimate__, color = effect2__)) +
  geom_ribbon(data = rare.precipcan, aes(x = SLOPE_precip_400m, y = estimate__, ymin = lower__,
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
  scale_y_continuous(name="Rarefied Richness Trend", limits = c(-0.0121, 0.004),
                     breaks = c(-0.010, -0.005, 0.000),
                     sec.axis = dup_axis(name = "% Change",
                                         labels = c("-2.28", "-1.14", "0.00"))) +
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

ggsave(paste(path2,
             "Precipitation and Canopy - Rarefied conditional effects.jpg",
             sep = ""),
       plot=precipcanplotR,
       device = "jpg",
       width=7, height=5, dpi=600)


# Abundance
precipcanplotA <- ggplot() +
  # Zero lines
  geom_hline(yintercept = 0, color = "black") +
  geom_vline(xintercept = 0, color = "black") +
  geom_line(data = ab.precipcan, aes(x = SLOPE_precip_400m, y = estimate__, color = effect2__)) +
  geom_ribbon(data = ab.precipcan, aes(x = SLOPE_precip_400m, y = estimate__, ymin = lower__,
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
  scale_y_continuous(name="Abundance Trend", limits = c(-0.0121, 0.004),
                     breaks = c(-0.010, -0.005, 0.000),
                     sec.axis = dup_axis(name = "% Change",
                                         labels = c("-2.28", "-1.14", "0.00"))) +
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

ggsave(paste(path2,
             "Precipitation and Canopy - Abundance conditional effects.jpg",
             sep = ""),
       plot=precipcanplotA,
       device = "jpg",
       width=7, height=5, dpi=600)



####################
## Crop :: Temp ##
####################

# Make the dfs

obs.tempcrop <- conditional_effects(observed, effects = "SLOPE_temp_400m:SLOPE_crop.cci_400m")
obs.tempcrop <- as.data.frame(obs.tempcrop$`SLOPE_temp_400m:SLOPE_crop.cci_400m`)
obs.tempcrop$Type <- "observed"
rare.tempcrop <- conditional_effects(rarefied, effects = "SLOPE_temp_400m:SLOPE_crop.cci_400m")
rare.tempcrop <- as.data.frame(rare.tempcrop$`SLOPE_temp_400m:SLOPE_crop.cci_400m`)
rare.tempcrop$Type <- "rarefied"
ab.tempcrop <- conditional_effects(abundance, effects = "SLOPE_temp_400m:SLOPE_crop.cci_400m")
ab.tempcrop <- as.data.frame(ab.tempcrop$`SLOPE_temp_400m:SLOPE_crop.cci_400m`)
ab.tempcrop$Type <- "abundance"

# effect1__ is the range of temperature slope values
# effect2__ is the categorical values of canopy: 1, 0, -1

# FYI: effect1__ is identical to whatever first varying
# parameter of interest is)

range(obs.tempcrop$estimate__)
range(rare.tempcrop$estimate__)
range(ab.tempcrop$estimate__)

#### Cropland LEgend

# Observed
tempcropplot <- ggplot() +
  # Zero lines
  geom_hline(yintercept = 0, color = "black") +
  geom_vline(xintercept = 0, color = "black") +
  geom_line(data = obs.tempcrop, aes(x = SLOPE_temp_400m, y = estimate__, color = effect2__)) +
  geom_ribbon(data = obs.tempcrop, aes(x = SLOPE_temp_400m, y = estimate__, ymin = lower__,
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
  scale_y_continuous(name="Observed Richness Trend", limits = c(-0.0121, 0.004),
                     breaks = c(-0.010, -0.005, 0.000),
                     sec.axis = dup_axis(name = "% Change",
                                         labels = c("-2.28", "-1.14", "0.00"))) +
  scale_colour_manual(name="Cropland\nTrend", values = c('1' = "orange1",
                                                         '0' = "gray40", 
                                                         '-1' = "turquoise4")) +
  scale_fill_manual(name="Cropland\nTrend", values = c('1' = "orange1",
                                                       '0' = "gray40", 
                                                       '-1' = "turquoise4")) +
  # theme(legend.position = c(0.80, 0.85)) +
  # theme(legend.position = "none")
  theme(legend.text = element_text(size=18), legend.title = element_text(size = 18),
        legend.title.align = 0.5) +
  guides(colour = guide_legend(override.aes = (list(size=3))))

ggsave(paste(path2, "Legend - Cropland.jpg", sep = ""), plot=tempcropplot,
       device = "jpg",
       width=7, height=5, dpi=600)




# Observed
tempcropplot <- ggplot() +
  # Zero lines
  geom_hline(yintercept = 0, color = "black") +
  geom_vline(xintercept = 0, color = "black") +
  geom_line(data = obs.tempcrop, aes(x = SLOPE_temp_400m, y = estimate__, color = effect2__)) +
  geom_ribbon(data = obs.tempcrop, aes(x = SLOPE_temp_400m, y = estimate__, ymin = lower__,
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
  scale_y_continuous(name="Observed Richness Trend", limits = c(-0.0121, 0.004),
                     breaks = c(-0.010, -0.005, 0.000),
                     sec.axis = dup_axis(name = "% Change",
                                         labels = c("-2.28", "-1.14", "0.00"))) +
  scale_colour_manual(name="Cropland\nTrend", values = c('1' = "orange1",
                                                      '0' = "gray40", 
                                                      '-1' = "turquoise4")) +
  scale_fill_manual(name="Cropland\nTrend", values = c('1' = "orange1",
                                                    '0' = "gray40", 
                                                    '-1' = "turquoise4")) +
  # theme(legend.position = c(0.80, 0.85)) +
  theme(legend.position = "none")
  # theme(legend.text = element_text(size=18), legend.title = element_text(size = 18),
  #       legend.title.align = 0.5) +
  # guides(colour = guide_legend(override.aes = (list(size=3))))

ggsave(paste(path2,
             "Temperature and Cropland - Observed conditional effects.jpg",
             sep = ""),
       plot=tempcropplot,
       device = "jpg",
       width=7, height=5, dpi=600)

# Rarefied
tempcropplotR <- ggplot() +
  # Zero lines
  geom_hline(yintercept = 0, color = "black") +
  geom_vline(xintercept = 0, color = "black") +
  geom_line(data = rare.tempcrop, aes(x = SLOPE_temp_400m, y = estimate__, color = effect2__)) +
  geom_ribbon(data = rare.tempcrop, aes(x = SLOPE_temp_400m, y = estimate__, ymin = lower__,
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
  scale_y_continuous(name="Rarefied Richness Trend", limits = c(-0.0121, 0.004),
                     breaks = c(-0.010, -0.005, 0.000),
                     sec.axis = dup_axis(name = "% Change",
                                         labels = c("-2.28", "-1.14", "0.00"))) +
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

ggsave(paste(path2,
             "Temperature and Cropland - Rarefied conditional effects.jpg", 
             sep = ""),
       plot=tempcropplotR,
       device = "jpg",
       width=7, height=5, dpi=600)


# Abundance
tempcropplotA <- ggplot() +
  # Zero lines
  geom_hline(yintercept = 0, color = "black") +
  geom_vline(xintercept = 0, color = "black") +
  geom_line(data = ab.tempcrop, aes(x = SLOPE_temp_400m, y = estimate__, color = effect2__)) +
  geom_ribbon(data = ab.tempcrop, aes(x = SLOPE_temp_400m, y = estimate__, ymin = lower__,
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
  scale_y_continuous(name="Abundance Trend", limits = c(-0.0121, 0.004),
                     breaks = c(-0.010, -0.005, 0.000),
                     sec.axis = dup_axis(name = "% Change",
                                         labels = c("-2.28", "-1.14", "0.00"))) +
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

ggsave(paste(path2,
             "Temperature and Cropland - Abundance conditional effects.jpg",
             sep = ""),
       plot=tempcropplotA,
       device = "jpg",
       width=7, height=5, dpi=600)



######################
## Crop :: Precip ##
######################

# Make the dfs

obs.precipcrop <- conditional_effects(observed, effects = "SLOPE_precip_400m:SLOPE_crop.cci_400m")
obs.precipcrop <- as.data.frame(obs.precipcrop$`SLOPE_precip_400m:SLOPE_crop.cci_400m`)
obs.precipcrop$Type <- "observed"
rare.precipcrop <- conditional_effects(rarefied, effects = "SLOPE_precip_400m:SLOPE_crop.cci_400m")
rare.precipcrop <- as.data.frame(rare.precipcrop$`SLOPE_precip_400m:SLOPE_crop.cci_400m`)
rare.precipcrop$Type <- "rarefied"
ab.precipcrop <- conditional_effects(abundance, effects = "SLOPE_precip_400m:SLOPE_crop.cci_400m")
ab.precipcrop <- as.data.frame(ab.precipcrop$`SLOPE_precip_400m:SLOPE_crop.cci_400m`)
ab.precipcrop$Type <- "abundance"

# effect1__ is the range of precip slope values
# effect2__ is the categorical values of canopy: 1, 0, -1

# FYI: effect1__ is identical to whatever first varying
# parameter of interest is)

range(obs.precipcrop$SLOPE_precip_400m)
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
  geom_line(data = obs.precipcrop, aes(x = SLOPE_precip_400m, y = estimate__, color = effect2__)) +
  geom_ribbon(data = obs.precipcrop, aes(x = SLOPE_precip_400m, y = estimate__, ymin = lower__,
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
  scale_y_continuous(name="Observed Richness Trend", limits = c(-0.0121, 0.004),
                     breaks = c(-0.010, -0.005, 0.000),
                     sec.axis = dup_axis(name = "% Change",
                                         labels = c("-2.28", "-1.14", "0.00"))) +
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

ggsave(paste(path2, "Precipitation and Cropland - Observed conditional effects.jpg",
             sep = ""),
       plot=precipcropplot,
       device = "jpg",
       width=7, height=5, dpi=600)

# Rarefied
precipcropplotR <- ggplot() +
  # Zero lines
  geom_hline(yintercept = 0, color = "black") +
  geom_vline(xintercept = 0, color = "black") +
  geom_line(data = rare.precipcrop, aes(x = SLOPE_precip_400m, y = estimate__, color = effect2__)) +
  geom_ribbon(data = rare.precipcrop, aes(x = SLOPE_precip_400m, y = estimate__, ymin = lower__,
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
  scale_y_continuous(name="Rarefied Richness Trend", limits = c(-0.0121, 0.004),
                     breaks = c(-0.010, -0.005, 0.000),
                     sec.axis = dup_axis(name = "% Change",
                                         labels = c("-2.28", "-1.14", "0.00"))) +
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

ggsave(paste(path2,
             "Precipitation and Cropland - Rarefied conditional effects.jpg",
             sep = ""),
       plot=precipcropplotR,
       device = "jpg",
       width=7, height=5, dpi=600)


# Abundance
precipcropplotA <- ggplot() +
  # Zero lines
  geom_hline(yintercept = 0, color = "black") +
  geom_vline(xintercept = 0, color = "black") +
  geom_line(data = ab.precipcrop, aes(x = SLOPE_precip_400m, y = estimate__, color = effect2__)) +
  geom_ribbon(data = ab.precipcrop, aes(x = SLOPE_precip_400m, y = estimate__, ymin = lower__,
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
  scale_y_continuous(name="Abundance Trend", limits = c(-0.0121, 0.004),
                     breaks = c(-0.010, -0.005, 0.000),
                     sec.axis = dup_axis(name = "% Change",
                                         labels = c("-2.28", "-1.14", "0.00"))) +
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

ggsave(paste(path2,
             "Precipitation and Cropland - Abundance conditional effects.jpg",
             sep = ""),
       plot=precipcropplotA,
       device = "jpg",
       width=7, height=5, dpi=600)


####################
## Urban :: Temp ##
####################

# Make the dfs

obs.tempurban <- conditional_effects(observed, effects = "SLOPE_temp_400m:SLOPE_urban.gaia_400m")
obs.tempurban <- as.data.frame(obs.tempurban$`SLOPE_temp_400m:SLOPE_urban.gaia_400m`)
obs.tempurban$Type <- "observed"
rare.tempurban <- conditional_effects(rarefied, effects = "SLOPE_temp_400m:SLOPE_urban.gaia_400m")
rare.tempurban <- as.data.frame(rare.tempurban$`SLOPE_temp_400m:SLOPE_urban.gaia_400m`)
rare.tempurban$Type <- "rarefied"
ab.tempurban <- conditional_effects(abundance, effects = "SLOPE_temp_400m:SLOPE_urban.gaia_400m")
ab.tempurban <- as.data.frame(ab.tempurban$`SLOPE_temp_400m:SLOPE_urban.gaia_400m`)
ab.tempurban$Type <- "abundance"

# effect1__ is the range of temp slope values
# effect2__ is the categorical values of canopy: 1, 0, -1

# FYI: effect1__ is identical to whatever first varying
# parameter of interest is)

range(obs.tempurban$estimate__)
range(rare.tempurban$estimate__)
range(ab.tempurban$estimate__)
# -0.011 0.0024

#### Urban legend ####

# Observed
tempurbanplot <- ggplot() +
  # Zero lines
  geom_hline(yintercept = 0, color = "black") +
  geom_vline(xintercept = 0, color = "black") +
  geom_line(data = obs.tempurban, aes(x = SLOPE_temp_400m, y = estimate__, color = effect2__)) +
  geom_ribbon(data = obs.tempurban, aes(x = SLOPE_temp_400m, y = estimate__, ymin = lower__,
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
  scale_y_continuous(name="Observed Richness Trend", limits = c(-0.0121, 0.004),
                     breaks = c(-0.010, -0.005, 0.000),
                     sec.axis = dup_axis(name = "% Change",
                                         labels = c("-2.28", "-1.14", "0.00"))) +
  scale_colour_manual(name="Urban Trend", values = c('1' = "red3",
                                                     '0' = "gray40", 
                                                     '-1' = "blueviolet")) +
  scale_fill_manual(name="Urban Trend", values = c('1' = "red3",
                                                   '0' = "gray40", 
                                                   '-1' = "blueviolet")) +
  # theme(legend.position = c(0.80, 0.85)) +
  # theme(legend.position = "none")
  theme(legend.text = element_text(size=18), legend.title = element_text(size = 18),
        legend.title.align = 0.5) +
  guides(colour = guide_legend(override.aes = (list(size=3))))

ggsave(paste(path2, "Legend - urban.jpg", sep = ""),
       plot=tempurbanplot,
       device = "jpg",
       width=7, height=5, dpi=600)


# Observed
tempurbanplot <- ggplot() +
  # Zero lines
  geom_hline(yintercept = 0, color = "black") +
  geom_vline(xintercept = 0, color = "black") +
  geom_line(data = obs.tempurban, aes(x = SLOPE_temp_400m, y = estimate__, color = effect2__)) +
  geom_ribbon(data = obs.tempurban, aes(x = SLOPE_temp_400m, y = estimate__, ymin = lower__,
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
  scale_y_continuous(name="Observed Richness Trend", limits = c(-0.0121, 0.004),
                     breaks = c(-0.010, -0.005, 0.000),
                     sec.axis = dup_axis(name = "% Change",
                                         labels = c("-2.28", "-1.14", "0.00"))) +
  scale_colour_manual(name="Urban Trend", values = c('1' = "red3",
                                                        '0' = "gray40", 
                                                        '-1' = "blueviolet")) +
  scale_fill_manual(name="Urban Trend", values = c('1' = "red3",
                                                      '0' = "gray40", 
                                                      '-1' = "blueviolet")) +
  # theme(legend.position = c(0.80, 0.85)) +
  theme(legend.position = "none")
  # theme(legend.text = element_text(size=18), legend.title = element_text(size = 18),
  #       legend.title.align = 0.5) +
  # guides(colour = guide_legend(override.aes = (list(size=3))))

ggsave(paste(path2,
             "Temperature and Urban - Observed conditional effects.jpg",
             sep = ""),
       plot=tempurbanplot,
       device = "jpg",
       width=7, height=5, dpi=600)

# Rarefied
tempurbanplotR <- ggplot() +
  # Zero lines
  geom_hline(yintercept = 0, color = "black") +
  geom_vline(xintercept = 0, color = "black") +
  geom_line(data = rare.tempurban, aes(x = SLOPE_temp_400m, y = estimate__, color = effect2__)) +
  geom_ribbon(data = rare.tempurban, aes(x = SLOPE_temp_400m, y = estimate__, ymin = lower__,
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
  scale_y_continuous(name="Rarefied Richness Trend", limits = c(-0.0121, 0.004),
                     breaks = c(-0.010, -0.005, 0.000),
                     sec.axis = dup_axis(name = "% Change",
                                         labels = c("-2.28", "-1.14", "0.00"))) +
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

ggsave(paste(path2,
             "Temperature and Urban - Rarefied conditional effects.jpg",
             sep = ""),
       plot=tempurbanplotR,
       device = "jpg",
       width=7, height=5, dpi=600)


# Abundance
tempurbanplotA <- ggplot() +
  # Zero lines
  geom_hline(yintercept = 0, color = "black") +
  geom_vline(xintercept = 0, color = "black") +
  geom_line(data = ab.tempurban, aes(x = SLOPE_temp_400m, y = estimate__, color = effect2__)) +
  geom_ribbon(data = ab.tempurban, aes(x = SLOPE_temp_400m, y = estimate__, ymin = lower__,
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
  scale_y_continuous(name="Abundance Trend", limits = c(-0.0121, 0.004),
                     breaks = c(-0.010, -0.005, 0.000),
                     sec.axis = dup_axis(name = "% Change",
                                         labels = c("-2.28", "-1.14", "0.00"))) +
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

ggsave(paste(path2,
             "Temperature and Urban - Abundance conditional effects.jpg",
             sep = ""),
       plot=tempurbanplotA,
       device = "jpg",
       width=7, height=5, dpi=600)


######################
## Urban :: Precip ##
######################

# Make the dfs

obs.precipurban <- conditional_effects(observed, effects = "SLOPE_precip_400m:SLOPE_urban.gaia_400m")
obs.precipurban <- as.data.frame(obs.precipurban$`SLOPE_precip_400m:SLOPE_urban.gaia_400m`)
obs.precipurban$Type <- "observed"
rare.precipurban <- conditional_effects(rarefied, effects = "SLOPE_precip_400m:SLOPE_urban.gaia_400m")
rare.precipurban <- as.data.frame(rare.precipurban$`SLOPE_precip_400m:SLOPE_urban.gaia_400m`)
rare.precipurban$Type <- "rarefied"
ab.precipurban <- conditional_effects(abundance, effects = "SLOPE_precip_400m:SLOPE_urban.gaia_400m")
ab.precipurban <- as.data.frame(ab.precipurban$`SLOPE_precip_400m:SLOPE_urban.gaia_400m`)
ab.precipurban$Type <- "abundance"

# effect1__ is the range of precip slope values
# effect2__ is the categorical values of canopy: 1, 0, -1

# FYI: effect1__ is identical to whatever first varying
# parameter of interest is)

range(obs.precipurban$SLOPE_precip_400m)
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
  geom_line(data = obs.precipurban, aes(x = SLOPE_precip_400m, y = estimate__, color = effect2__)) +
  geom_ribbon(data = obs.precipurban, aes(x = SLOPE_precip_400m, y = estimate__, ymin = lower__,
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
  scale_y_continuous(name="Observed Richness Trend", limits = c(-0.0121, 0.004),
                     breaks = c(-0.010, -0.005, 0.000),
                     sec.axis = dup_axis(name = "% Change",
                                         labels = c("-2.28", "-1.14", "0.00"))) +
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

ggsave(paste(path2,
             "Precipitation and Urban - Observed conditional effects.jpg",
             sep = ""),
       plot=precipurbanplot,
       device = "jpg",
       width=7, height=5, dpi=600)

# Rarefied
precipurbanplotR <- ggplot() +
  # Zero lines
  geom_hline(yintercept = 0, color = "black") +
  geom_vline(xintercept = 0, color = "black") +
  geom_line(data = rare.precipurban, aes(x = SLOPE_precip_400m, y = estimate__, color = effect2__)) +
  geom_ribbon(data = rare.precipurban, aes(x = SLOPE_precip_400m, y = estimate__, ymin = lower__,
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
  scale_y_continuous(name="Rarefied Richness Trend", limits = c(-0.0121, 0.004),
                     breaks = c(-0.010, -0.005, 0.000),
                     sec.axis = dup_axis(name = "% Change",
                                         labels = c("-2.28", "-1.14", "0.00"))) +
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

ggsave(paste(path2,
             "Precipitation and Urban - Rarefied conditional effects.jpg",
             sep = ""),
       plot=precipurbanplotR,
       device = "jpg",
       width=7, height=5, dpi=600)


# Abundance
precipurbanplotA <- ggplot() +
  # Zero lines
  geom_hline(yintercept = 0, color = "black") +
  geom_vline(xintercept = 0, color = "black") +
  geom_line(data = ab.precipurban, aes(x = SLOPE_precip_400m, y = estimate__, color = effect2__)) +
  geom_ribbon(data = ab.precipurban, aes(x = SLOPE_precip_400m, y = estimate__, ymin = lower__,
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
  scale_y_continuous(name="Abundance Trend", limits = c(-0.0121, 0.004),
                     breaks = c(-0.010, -0.005, 0.000),
                     sec.axis = dup_axis(name = "% Change",
                                         labels = c("-2.28", "-1.14", "0.00"))) +
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

ggsave(paste(path2,
             "Precipitation and Urban - Abundance conditional effects.jpg",
             sep = ""),
       plot=precipurbanplotA,
       device = "jpg",
       width=7, height=5, dpi=600)



######################
## Water :: Precip  ##
######################

# Make the dfs

obs.precipwater <- conditional_effects(observed, effects = "SLOPE_precip_400m:SLOPE_water.total_400m")
obs.precipwater <- as.data.frame(obs.precipwater$`SLOPE_precip_400m:SLOPE_water.total_400m`)
obs.precipwater$Type <- "observed"
rare.precipwater <- conditional_effects(rarefied, effects = "SLOPE_precip_400m:SLOPE_water.total_400m")
rare.precipwater <- as.data.frame(rare.precipwater$`SLOPE_precip_400m:SLOPE_water.total_400m`)
rare.precipwater$Type <- "rarefied"
ab.precipwater <- conditional_effects(abundance, effects = "SLOPE_precip_400m:SLOPE_water.total_400m")
ab.precipwater <- as.data.frame(ab.precipwater$`SLOPE_precip_400m:SLOPE_water.total_400m`)
ab.precipwater$Type <- "abundance"

# effect1__ is the range of precip slope values
# effect2__ is the categorical values of water: 1, 0, -1

# FYI: effect1__ is identical to whatever first varying
# parameter of interest is)

range(obs.precipwater$SLOPE_precip_400m)
range(obs.precipwater$estimate__)
#-0.0036  0.0026
range(rare.precipwater$estimate__)
#-0.0006 0.0026
range(ab.precipwater$estimate__)
# -0.0111 -0.0046

#### WATER LEGEND ####

# Observed
precipwaterplot <- ggplot() +
  # Zero lines
  geom_hline(yintercept = 0, color = "black") +
  geom_vline(xintercept = 0, color = "black") +
  geom_line(data = obs.precipwater, aes(x = SLOPE_precip_400m, y = estimate__, color = effect2__)) +
  geom_ribbon(data = obs.precipwater, aes(x = SLOPE_precip_400m, y = estimate__, ymin = lower__,
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
  scale_y_continuous(name="Observed Richness Trend", limits = c(-0.0121, 0.004),
                     breaks = c(-0.010, -0.005, 0.000),
                     sec.axis = dup_axis(name = "% Change",
                                         labels = c("-2.28", "-1.14", "0.00"))) +
  scale_colour_manual(name="Surface Water\nTrend", values = c('1' = "blue4",
                                                              '0' = "gray40", 
                                                              '-1' = "turquoise4")) +
  scale_fill_manual(name="Surface Water\nTrend", values = c('1' = "blue4",
                                                            '0' = "gray40", 
                                                            '-1' = "turquoise4")) +
  # theme(legend.position = c(0.80, 0.85)) +
  # theme(legend.position = "none")
  theme(legend.text = element_text(size=18), legend.title = element_text(size = 18),
        legend.title.align = 0.5) +
  guides(colour = guide_legend(override.aes = (list(size=3))))

ggsave(paste(path2, "Legend - water.jpg", sep = ""),
       plot=precipwaterplot,
       device = "jpg",
       width=7, height=5, dpi=600)


# Observed
precipwaterplot <- ggplot() +
  # Zero lines
  geom_hline(yintercept = 0, color = "black") +
  geom_vline(xintercept = 0, color = "black") +
  geom_line(data = obs.precipwater, aes(x = SLOPE_precip_400m, y = estimate__, color = effect2__)) +
  geom_ribbon(data = obs.precipwater, aes(x = SLOPE_precip_400m, y = estimate__, ymin = lower__,
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
  scale_y_continuous(name="Observed Richness Trend", limits = c(-0.0121, 0.004),
                     breaks = c(-0.010, -0.005, 0.000),
                     sec.axis = dup_axis(name = "% Change",
                                         labels = c("-2.28", "-1.14", "0.00"))) +
  scale_colour_manual(name="Surface Water\nTrend", values = c('1' = "blue4",
                                                     '0' = "gray40", 
                                                     '-1' = "turquoise4")) +
  scale_fill_manual(name="Surface Water\nTrend", values = c('1' = "blue4",
                                                   '0' = "gray40", 
                                                   '-1' = "turquoise4")) +
  # theme(legend.position = c(0.80, 0.85)) +
  theme(legend.position = "none")
  # theme(legend.text = element_text(size=18), legend.title = element_text(size = 18),
  #       legend.title.align = 0.5) +
  # guides(colour = guide_legend(override.aes = (list(size=3))))

ggsave(paste(path2, 
             "Precipitation and Water - Observed conditional effects.jpg",
             sep = ""),
       plot=precipwaterplot,
       device = "jpg",
       width=7, height=5, dpi=600)

# Rarefied
precipwaterplotR <- ggplot() +
  # Zero lines
  geom_hline(yintercept = 0, color = "black") +
  geom_vline(xintercept = 0, color = "black") +
  geom_line(data = rare.precipwater, aes(x = SLOPE_precip_400m, y = estimate__, color = effect2__)) +
  geom_ribbon(data = rare.precipwater, aes(x = SLOPE_precip_400m, y = estimate__, ymin = lower__,
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
  scale_y_continuous(name="Rarefied Richness Trend", limits = c(-0.0121, 0.004),
                     breaks = c(-0.010, -0.005, 0.000),
                     sec.axis = dup_axis(name = "% Change",
                                         labels = c("-2.28", "-1.14", "0.00"))) +
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

ggsave(paste(path2,
             "Precipitation and Water - Rarefied conditional effects.jpg",
             sep = ""),
       plot=precipwaterplotR,
       device = "jpg",
       width=7, height=5, dpi=600)


# Abundance
precipwaterplotA <- ggplot() +
  # Zero lines
  geom_hline(yintercept = 0, color = "black") +
  geom_vline(xintercept = 0, color = "black") +
  geom_line(data = ab.precipwater, aes(x = SLOPE_precip_400m, y = estimate__, color = effect2__)) +
  geom_ribbon(data = ab.precipwater, aes(x = SLOPE_precip_400m, y = estimate__, ymin = lower__,
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
  scale_y_continuous(name="Abundance Trend", limits = c(-0.0121, 0.004),
                     breaks = c(-0.010, -0.005, 0.000),
                     sec.axis = dup_axis(name = "% Change",
                                         labels = c("-2.28", "-1.14", "0.00"))) +
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

ggsave(paste(path2,
             "Precipitation and Water - Abundance conditional effects.jpg",
             sep = ""),
       plot=precipwaterplotA,
       device = "jpg",
       width=7, height=5, dpi=600)



######################
## Water :: temp    ##
######################

# Make the dfs

obs.tempwater <- conditional_effects(observed, effects = "SLOPE_temp_400m:SLOPE_water.total_400m")
obs.tempwater <- as.data.frame(obs.tempwater$`SLOPE_temp_400m:SLOPE_water.total_400m`)
obs.tempwater$Type <- "observed"
rare.tempwater <- conditional_effects(rarefied, effects = "SLOPE_temp_400m:SLOPE_water.total_400m")
rare.tempwater <- as.data.frame(rare.tempwater$`SLOPE_temp_400m:SLOPE_water.total_400m`)
rare.tempwater$Type <- "rarefied"
ab.tempwater <- conditional_effects(abundance, effects = "SLOPE_temp_400m:SLOPE_water.total_400m")
ab.tempwater <- as.data.frame(ab.tempwater$`SLOPE_temp_400m:SLOPE_water.total_400m`)
ab.tempwater$Type <- "abundance"

# effect1__ is the range of temp slope values
# effect2__ is the categorical values of water: 1, 0, -1

# FYI: effect1__ is identical to whatever first varying
# parameter of interest is)

range(obs.tempwater$SLOPE_temp_400m)
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
  geom_line(data = obs.tempwater, aes(x = SLOPE_temp_400m, y = estimate__, color = effect2__)) +
  geom_ribbon(data = obs.tempwater, aes(x = SLOPE_temp_400m, y = estimate__, ymin = lower__,
                                          ymax = upper__, fill = effect2__), alpha = 0.7) +
  theme_bw() +
  theme(axis.text.x = element_text(size=22, face="bold")) +
  theme(axis.text.y = element_text(size=22, face="bold")) +
  theme(axis.title.x = element_text(size=22, face="bold", color="gray30")) +
  theme(axis.title.y = element_text(size=22, face="bold", color="gray30")) +
  scale_x_continuous(name="Temperature Trend",
                     breaks=c(-3, -2, -1, 0, 1, 2)) +
  # scale_x_continuous(name="\u0394Temperature (\u00b0C)/Year",
  #                    breaks=c(-3, -2, -1, 0, 1, 2, 3, 4)) +
  theme(axis.title.y.right = element_text(vjust = 1.7)) +
  scale_y_continuous(name="Observed Richness Trend", limits = c(-0.0121, 0.004),
                     breaks = c(-0.010, -0.005, 0.000),
                     sec.axis = dup_axis(name = "% Change",
                                         labels = c("-2.28", "-1.14", "0.00"))) +
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

ggsave(paste(path2,
             "Temperature and Water - Observed conditional effects.jpg",
             sep = ""),
       plot=tempwaterplot,
       device = "jpg",
       width=7, height=5, dpi=600)

# Rarefied
tempwaterplotR <- ggplot() +
  # Zero lines
  geom_hline(yintercept = 0, color = "black") +
  geom_vline(xintercept = 0, color = "black") +
  geom_line(data = rare.tempwater, aes(x = SLOPE_temp_400m, y = estimate__, color = effect2__)) +
  geom_ribbon(data = rare.tempwater, aes(x = SLOPE_temp_400m, y = estimate__, ymin = lower__,
                                           ymax = upper__, fill = effect2__), alpha = 0.7) +
  theme_bw() +
  theme(axis.text.x = element_text(size=22, face="bold")) +
  theme(axis.text.y = element_text(size=22, face="bold")) +
  theme(axis.title.x = element_text(size=22, face="bold", color="gray30")) +
  theme(axis.title.y = element_text(size=22, face="bold", color="gray30")) +
  scale_x_continuous(name="Temperature Trend",
                     breaks=c(-3, -2, -1, 0, 1, 2)) +
  # scale_x_continuous(name="\u0394Temperature (\u00b0C)/Year",
  #                    breaks=c(-3, -2, -1, 0, 1, 2, 3, 4)) +
  theme(axis.title.y.right = element_text(vjust = 1.7)) +
  scale_y_continuous(name="Rarefied Richness Trend", limits = c(-0.0121, 0.004),
                     breaks = c(-0.010, -0.005, 0.000),
                     sec.axis = dup_axis(name = "% Change",
                                         labels = c("-2.28", "-1.14", "0.00"))) +
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

ggsave(paste(path2,
             "Temperature and Water - Rarefied conditional effects.jpg",
             sep = ""),
       plot=tempwaterplotR,
       device = "jpg",
       width=7, height=5, dpi=600)


# Abundance
tempwaterplotA <- ggplot() +
  # Zero lines
  geom_hline(yintercept = 0, color = "black") +
  geom_vline(xintercept = 0, color = "black") +
  geom_line(data = ab.tempwater, aes(x = SLOPE_temp_400m, y = estimate__, color = effect2__)) +
  geom_ribbon(data = ab.tempwater, aes(x = SLOPE_temp_400m, y = estimate__, ymin = lower__,
                                         ymax = upper__, fill = effect2__), alpha = 0.7) +
  theme_bw() +
  theme(axis.text.x = element_text(size=22, face="bold")) +
  theme(axis.text.y = element_text(size=22, face="bold")) +
  theme(axis.title.x = element_text(size=22, face="bold", color="gray30")) +
  theme(axis.title.y = element_text(size=22, face="bold", color="gray30")) +
  scale_x_continuous(name="Temperature Trend",
                     breaks=c(-3, -2, -1, 0, 1, 2)) +
  # scale_x_continuous(name="\u0394Temperature (\u00b0C)/Year",
  #                    breaks=c(-3, -2, -1, 0, 1, 2, 3, 4)) +
  theme(axis.title.y.right = element_text(vjust = 1.7)) +
  scale_y_continuous(name="Abundance Trend", limits = c(-0.0121, 0.004),
                     breaks = c(-0.010, -0.005, 0.000),
                     sec.axis = dup_axis(name = "% Change",
                                         labels = c("-2.28", "-1.14", "0.00"))) +
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

ggsave(paste(path2,
             "Temperature and Water - Abundance conditional effects.jpg",
             sep = ""),
       plot=tempwaterplotA,
       device = "jpg",
       width=7, height=5, dpi=600)

