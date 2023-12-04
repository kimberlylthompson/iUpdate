#################################################################################################
########                                                                                 ########
########                  Univariate conditional effects of predictors                   ######## 
########                       for each biodiversity response                            ########
#################################################################################################

# Author: Kimberly Thompson

# This code uses the three brms models constructed for observed richness, rarefied richness,
# abundance (1 model for each) and creates graphs of the results.

# Models = Upper SE of predictors


# Plots created:
# Figure S7



# Website used for tutorial:
# http://mjskay.github.io/tidybayes/articles/tidy-brms.html



########## clean workspace and load required packages ####################

# clean workspace to improve efficiency: #
# rm(list = ls() ) 
# gc() #releases memory


library(brms)
library(tidybayes)
library(cowplot)

library(tidyverse) # Data organization

library(ggplot2)

library(viridis)


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

get_variables(observed)

###############################################
###                                         ###
###           Make the graphs               ###
###                                         ###
###############################################

# % change values
# -0.018
(10^(-0.018 * 1)-1) * 100 # -4.06

# -0.012
(10^(-0.012 * 1)-1) * 100 # -2.73

# -0.006
(10^(-0.006 * 1)-1) * 100 # -1.37

#0

# 0.006
(10^(0.006 * 1)-1) * 100 # 1.39



# Would be better to put these in a loop to do all at once but for now doing them individually

# Temperature
obs.temp <- conditional_effects(observed, effects = "temp.upper")
obs.temp <- as.data.frame(obs.temp$temp.upper)
obs.temp$Type <- "observed"
rare.temp <- conditional_effects(rarefied, effects = "temp.upper")
rare.temp <- as.data.frame(rare.temp$temp.upper)
rare.temp$Type <- "rarefied"
ab.temp <- conditional_effects(abundance, effects = "temp.upper")
ab.temp <- as.data.frame(ab.temp$temp.upper)
ab.temp$Type <- "abundance"

# Version with the legend - only need one graph with legend - same legend can be used for all predictors
tempplot.legend <- ggplot() +
  # Zero lines
  geom_hline(yintercept = 0, color = "black") +
  geom_vline(xintercept = 0, color = "black") +
  # Observed
  geom_line(data = obs.temp, aes(x = temp.upper, y = estimate__, color = factor(Type))) +
  geom_ribbon(data = obs.temp, aes(x = temp.upper, y = estimate__, ymin = lower__,
                                   ymax = upper__), alpha = 0.7, fill = viridis(100)[42]) + 
  # Rarefied
  geom_line(data = rare.temp, aes(x = temp.upper, y = estimate__, color = factor(Type))) +
  geom_ribbon(data = rare.temp, aes(x = temp.upper, y = estimate__, ymin = lower__,
                                    ymax = upper__), alpha = 0.7, fill = viridis(100)[85]) +
  # Abundance
  geom_line(data = ab.temp, aes(x = temp.upper, y = estimate__, color = factor(Type))) +
  geom_ribbon(data = ab.temp, aes(x = temp.upper, y = estimate__, ymin = lower__,
                                  ymax = upper__), alpha = 0.7, fill = viridis(100)[1]) +
  theme_bw() +
  theme(axis.text.x = element_text(size=22, face="bold")) +
  theme(axis.text.y = element_text(size=22, face="bold")) +
  theme(axis.title.x = element_text(size=22, face="bold", color="gray30")) +
  theme(axis.title.y = element_text(size=22, face="bold", color="gray30")) +
  scale_x_continuous(name="\u0394Temperature (\u00b0C)/Year",
                     breaks=c(-3, -2, -1, 0, 1, 2)) +
  scale_y_continuous(name="\u0394 / Year") +
  scale_color_manual(name = "", values = c('abundance' = viridis(100)[1],
                                           'observed' = viridis(100)[42],
                                           'rarefied' = viridis(100)[85]),
                     labels = c("Abundance", "Observed Richness", "Rarefied Richness")) +
  # theme(legend.position = c(0.80, 0.85)) +
  # theme(legend.position = "none")
  theme(legend.text = element_text(size=18)) +
  theme(legend.key.size = unit(1, 'cm')) +
  guides(colour = guide_legend(override.aes = list(linewidth = 3))) 

path2 <- "01_Analysis/Model_Plotting/Univariate Conditional Effects/Upper SE for predictors/"
ggsave(paste(path2, "LEGEND - univariate conditional effects.jpg", sep = ""),
       plot=tempplot.legend,
       device = "jpg",
       width=9, height=5, dpi=600)


# Version without legend
tempplot <- ggplot() +
  # Zero lines
  geom_hline(yintercept = 0, color = "black") +
  geom_vline(xintercept = 0, color = "black") +
  # Observed
  geom_line(data = obs.temp, aes(x = temp.upper, y = estimate__), color = viridis(100)[42]) +
  geom_ribbon(data = obs.temp, aes(x = temp.upper, y = estimate__, ymin = lower__,
                                   ymax = upper__), alpha = 0.7, fill = viridis(100)[42]) + 
  # Rarefied
  geom_line(data = rare.temp, aes(x = temp.upper, y = estimate__), color = viridis(100)[85]) +
  geom_ribbon(data = rare.temp, aes(x = temp.upper, y = estimate__, ymin = lower__,
                                    ymax = upper__), alpha = 0.7, fill = viridis(100)[85]) +
  # Abundance
  geom_line(data = ab.temp, aes(x = temp.upper, y = estimate__), color = viridis(100)[1]) +
  geom_ribbon(data = ab.temp, aes(x = temp.upper, y = estimate__, ymin = lower__,
                                  ymax = upper__), alpha = 0.7, fill = viridis(100)[1]) +
  theme_bw() +
  theme(axis.text.x = element_text(size=22, face="bold")) +
  theme(axis.text.y = element_text(size=22, face="bold")) +
  theme(axis.title.x = element_text(size=22, face="bold", color="gray30")) +
  theme(axis.title.y = element_text(size=22, face="bold", color="gray30")) +
  scale_x_continuous(name="Change in Temperature (\u00b0C)",
                     breaks=c(-3, -2, -1, 0, 1, 2)) +
  theme(axis.title.y.right = element_text(vjust = 1.7)) +
  scale_y_continuous(name="Trend", limits = c(-0.023, 0.007),
                     breaks = c(-0.018, -0.012, -0.006, 0.000, 0.006),
                     sec.axis = dup_axis(name = "% Change",
                                         labels = c("-4.06", "-2.73", "-1.37",
                                                    "0.00", "1.39")))
# scale_x_continuous(name="\u0394Temperature (\u00b0C)/Year",
#                    breaks=c(-3, -2, -1, 0, 1, 2)) +
# scale_y_continuous(name="\u0394 / Year", limits = c(-0.009, 0.003),
#                    breaks = c(-0.009, -0.006, -0.003, 0.000, 0.003))

ggsave(paste(path2, "Temperature - univariate conditional effects.jpg", sep = ""),
       plot=tempplot,
       device = "jpg",
       width=7, height=5, dpi=600)



# Precipitation
obs.precip <- conditional_effects(observed, effects = "precip.upper")
obs.precip <- as.data.frame(obs.precip$precip.upper)
obs.precip$Type <- "observed"
rare.precip <- conditional_effects(rarefied, effects = "precip.upper")
rare.precip <- as.data.frame(rare.precip$precip.upper)
rare.precip$Type <- "rarefied"
ab.precip <- conditional_effects(abundance, effects = "precip.upper")
ab.precip <- as.data.frame(ab.precip$precip.upper)
ab.precip$Type <- "abundance"

# Precipitation Version without legend
precipplot <- ggplot() +
  # Zero lines
  geom_hline(yintercept = 0, color = "black") +
  geom_vline(xintercept = 0, color = "black") +
  # Observed
  geom_line(data = obs.precip, aes(x = precip.upper, y = estimate__), color = viridis(100)[42]) +
  geom_ribbon(data = obs.precip, aes(x = precip.upper, y = estimate__, ymin = lower__,
                                     ymax = upper__), alpha = 0.7, fill = viridis(100)[42]) + 
  # Rarefied
  geom_line(data = rare.precip, aes(x = precip.upper, y = estimate__), color = viridis(100)[85]) +
  geom_ribbon(data = rare.precip, aes(x = precip.upper, y = estimate__, ymin = lower__,
                                      ymax = upper__), alpha = 0.7, fill = viridis(100)[85]) +
  # Abundance
  geom_line(data = ab.precip, aes(x = precip.upper, y = estimate__), color = viridis(100)[1]) +
  geom_ribbon(data = ab.precip, aes(x = precip.upper, y = estimate__, ymin = lower__,
                                    ymax = upper__), alpha = 0.7, fill = viridis(100)[1]) +
  theme_bw() +
  theme(axis.text.x = element_text(size=22, face="bold")) +
  theme(axis.text.y = element_text(size=22, face="bold")) +
  theme(axis.title.x = element_text(size=22, face="bold", color="gray30")) +
  theme(axis.title.y = element_text(size=22, face="bold", color="gray30")) +
  scale_x_continuous(name="Change in Precipitation (mm)", limits = c(-3, 4),
                     breaks=c(-3, -2, -1, 0, 1, 2, 3, 4)) +
  theme(axis.title.y.right = element_text(vjust = 1.7)) +
  scale_y_continuous(name="Trend", limits = c(-0.023, 0.007),
                     breaks = c(-0.018, -0.012, -0.006, 0.000, 0.006),
                     sec.axis = dup_axis(name = "% Change",
                                         labels = c("-4.06", "-2.73", "-1.37",
                                                    "0.00", "1.39")))
# scale_x_continuous(name="\u0394Precipitation (mm)/Year", limits = c(-3.5, 4),
#                    breaks=c(-3, -2, -1, 0, 1, 2, 3, 4)) +
# scale_y_continuous(name="\u0394 / Year", limits = c(-0.010, 0.003),
#                    breaks = c(-0.009, -0.006, -0.003, 0.000, 0.003))

ggsave(paste(path2, "Precipitation - univariate conditional effects.jpg", sep = ""),
       plot=precipplot,
       device = "jpg",
       width=7, height=5, dpi=600)


#### Canopy ####
obs.canopy <- conditional_effects(observed, effects = "canopy.upper")
obs.canopy <- as.data.frame(obs.canopy$canopy.upper)
obs.canopy$Type <- "observed"
rare.canopy <- conditional_effects(rarefied, effects = "canopy.upper")
rare.canopy <- as.data.frame(rare.canopy$canopy.upper)
rare.canopy$Type <- "rarefied"
ab.canopy <- conditional_effects(abundance, effects = "canopy.upper")
ab.canopy <- as.data.frame(ab.canopy$canopy.upper)
ab.canopy$Type <- "abundance"

# Canopy Version without legend
canopyplot <- ggplot() +
  # Zero lines
  geom_hline(yintercept = 0, color = "black") +
  geom_vline(xintercept = 0, color = "black") +
  # Observed
  geom_line(data = obs.canopy, aes(x = canopy.upper, y = estimate__), color = viridis(100)[42]) +
  geom_ribbon(data = obs.canopy, aes(x = canopy.upper, y = estimate__, ymin = lower__,
                                     ymax = upper__), alpha = 0.7, fill = viridis(100)[42]) + 
  # Rarefied
  geom_line(data = rare.canopy, aes(x = canopy.upper, y = estimate__), color = viridis(100)[85]) +
  geom_ribbon(data = rare.canopy, aes(x = canopy.upper, y = estimate__, ymin = lower__,
                                      ymax = upper__), alpha = 0.7, fill = viridis(100)[85]) +
  # Abundance
  geom_line(data = ab.canopy, aes(x = canopy.upper, y = estimate__), color = viridis(100)[1]) +
  geom_ribbon(data = ab.canopy, aes(x = canopy.upper, y = estimate__, ymin = lower__,
                                    ymax = upper__), alpha = 0.7, fill = viridis(100)[1]) +
  theme_bw() +
  theme(axis.text.x = element_text(size=22, face="bold")) +
  theme(axis.text.y = element_text(size=22, face="bold")) +
  theme(axis.title.x = element_text(size=22, face="bold", color="gray30")) +
  theme(axis.title.y = element_text(size=22, face="bold", color="gray30")) +
  scale_x_continuous(name="Change in Canopy Cover (%)", limits = c(-6, 1),
                     breaks=c(-6, -4, -2, 0)) +
  theme(axis.title.y.right = element_text(vjust = 1.7)) +
  scale_y_continuous(name="Trend", limits = c(-0.023, 0.007),
                     breaks = c(-0.018, -0.012, -0.006, 0.000, 0.006),
                     sec.axis = dup_axis(name = "% Change",
                                         labels = c("-4.06", "-2.73", "-1.37",
                                                    "0.00", "1.39")))
# scale_x_continuous(name="\u0394Canopy Cover/Year", limits = c(-6, 1),
#                      breaks=c(-6, -4, -2, 0)) +
#   scale_y_continuous(name="\u0394 / Year", limits = c(-0.009, 0.003),
#                      breaks = c(-0.009, -0.006, -0.003, 0.000, 0.003))

ggsave(paste(path2, "Canopy - univariate conditional effects.jpg", sep = ""),
       plot=canopyplot,
       device = "jpg",
       width=7, height=5, dpi=600)


#### Cropland ####
obs.crop <- conditional_effects(observed, effects = "crop.upper")
obs.crop <- as.data.frame(obs.crop$crop.upper)
obs.crop$Type <- "observed"
rare.crop <- conditional_effects(rarefied, effects = "crop.upper")
rare.crop <- as.data.frame(rare.crop$crop.upper)
rare.crop$Type <- "rarefied"
ab.crop <- conditional_effects(abundance, effects = "crop.upper")
ab.crop <- as.data.frame(ab.crop$crop.upper)
ab.crop$Type <- "abundance"

# Cropland Version without legend
cropplot <- ggplot() +
  # Zero lines
  geom_hline(yintercept = 0, color = "black") +
  geom_vline(xintercept = 0, color = "black") +
  # Observed
  geom_line(data = obs.crop, aes(x = crop.upper, y = estimate__), color = viridis(100)[42]) +
  geom_ribbon(data = obs.crop, aes(x = crop.upper, y = estimate__, ymin = lower__,
                                   ymax = upper__), alpha = 0.7, fill = viridis(100)[42]) + 
  # Rarefied
  geom_line(data = rare.crop, aes(x = crop.upper, y = estimate__), color = viridis(100)[85]) +
  geom_ribbon(data = rare.crop, aes(x = crop.upper, y = estimate__, ymin = lower__,
                                    ymax = upper__), alpha = 0.7, fill = viridis(100)[85]) +
  # Abundance
  geom_line(data = ab.crop, aes(x = crop.upper, y = estimate__), color = viridis(100)[1]) +
  geom_ribbon(data = ab.crop, aes(x = crop.upper, y = estimate__, ymin = lower__,
                                  ymax = upper__), alpha = 0.7, fill = viridis(100)[1]) +
  theme_bw() +
  theme(axis.text.x = element_text(size=22, face="bold")) +
  theme(axis.text.y = element_text(size=22, face="bold")) +
  theme(axis.title.x = element_text(size=22, face="bold", color="gray30")) +
  theme(axis.title.y = element_text(size=22, face="bold", color="gray30")) +
  scale_x_continuous(name="Change in Cropland Cover (%)", limits = c(-13, 5),
                     breaks=c(-10, -5, 0, 5)) +
  theme(axis.title.y.right = element_text(vjust = 1.7)) +
  scale_y_continuous(name="Trend", limits = c(-0.023, 0.007),
                     breaks = c(-0.018, -0.012, -0.006, 0.000, 0.006),
                     sec.axis = dup_axis(name = "% Change",
                                         labels = c("-4.06", "-2.73", "-1.37",
                                                    "0.00", "1.39")))
# scale_x_continuous(name="\u0394Cropland Cover/Year", limits = c(-13, 5),
#                    breaks=c(-10, -5, 0, 5)) +
# scale_y_continuous(name="\u0394 / Year", limits = c(-0.018, 0.007),
#                    breaks = c(-0.018, -0.015, -0.012, -0.009, -0.006, -0.003, 0.000, 0.003, 0.006))

ggsave(paste(path2, "Cropland - univariate conditional effects.jpg", sep = ""),
       plot=cropplot,
       device = "jpg",
       width=7, height=5, dpi=600)



#### Urban ####
obs.urban <- conditional_effects(observed, effects = "urban.upper")
obs.urban <- as.data.frame(obs.urban$urban.upper)
obs.urban$Type <- "observed"
rare.urban <- conditional_effects(rarefied, effects = "urban.upper")
rare.urban <- as.data.frame(rare.urban$urban.upper)
rare.urban$Type <- "rarefied"
ab.urban <- conditional_effects(abundance, effects = "urban.upper")
ab.urban <- as.data.frame(ab.urban$urban.upper)
ab.urban$Type <- "abundance"

# urbanland Version without legend
urbanplot <- ggplot() +
  # Zero lines
  geom_hline(yintercept = 0, color = "black") +
  geom_vline(xintercept = 0, color = "black") +
  # Observed
  geom_line(data = obs.urban, aes(x = urban.upper, y = estimate__), color = viridis(100)[42]) +
  geom_ribbon(data = obs.urban, aes(x = urban.upper, y = estimate__, ymin = lower__,
                                    ymax = upper__), alpha = 0.7, fill = viridis(100)[42]) + 
  # Rarefied
  geom_line(data = rare.urban, aes(x = urban.upper, y = estimate__), color = viridis(100)[85]) +
  geom_ribbon(data = rare.urban, aes(x = urban.upper, y = estimate__, ymin = lower__,
                                     ymax = upper__), alpha = 0.7, fill = viridis(100)[85]) +
  # Abundance
  geom_line(data = ab.urban, aes(x = urban.upper, y = estimate__), color = viridis(100)[1]) +
  geom_ribbon(data = ab.urban, aes(x = urban.upper, y = estimate__, ymin = lower__,
                                   ymax = upper__), alpha = 0.7, fill = viridis(100)[1]) +
  theme_bw() +
  theme(axis.text.x = element_text(size=22, face="bold")) +
  theme(axis.text.y = element_text(size=22, face="bold")) +
  theme(axis.title.x = element_text(size=22, face="bold", color="gray30")) +
  theme(axis.title.y = element_text(size=22, face="bold", color="gray30")) +
  scale_x_continuous(name="Change in Urban Cover (%)", limits = c(-0.5, 12),
                     breaks=c(0, 4, 8, 12)) +
  theme(axis.title.y.right = element_text(vjust = 1.7)) +
  scale_y_continuous(name="Trend", limits = c(-0.023, 0.007),
                     breaks = c(-0.018, -0.012, -0.006, 0.000, 0.006),
                     sec.axis = dup_axis(name = "% Change",
                                         labels = c("-4.06", "-2.73", "-1.37",
                                                    "0.00", "1.39")))
# scale_x_continuous(name="\u0394Urban Cover/Year", limits = c(-0.5, 12),
#                    breaks=c(0, 4, 8, 12)) +
# scale_y_continuous(name="\u0394 / Year", limits = c(-0.023, 0.003),
#                    breaks = c(-0.020, -0.015, -0.010, -0.005, 0.000))

ggsave(paste(path2, "Urban - univariate conditional effects.jpg",
             sep = ""),
       plot=urbanplot,
       device = "jpg",
       width=7, height=5, dpi=600)


#### Water ####
obs.water <- conditional_effects(observed, effects = "water.upper")
obs.water <- as.data.frame(obs.water$water.upper)
obs.water$Type <- "observed"
rare.water <- conditional_effects(rarefied, effects = "water.upper")
rare.water <- as.data.frame(rare.water$water.upper)
rare.water$Type <- "rarefied"
ab.water <- conditional_effects(abundance, effects = "water.upper")
ab.water <- as.data.frame(ab.water$water.upper)
ab.water$Type <- "abundance"

# water Version without legend
waterplot <- ggplot() +
  # Zero lines
  geom_hline(yintercept = 0, color = "black") +
  geom_vline(xintercept = 0, color = "black") +
  # Observed
  geom_line(data = obs.water, aes(x = water.upper, y = estimate__), color = viridis(100)[42]) +
  geom_ribbon(data = obs.water, aes(x = water.upper, y = estimate__, ymin = lower__,
                                    ymax = upper__), alpha = 0.7, fill = viridis(100)[42]) + 
  # Rarefied
  geom_line(data = rare.water, aes(x = water.upper, y = estimate__), color = viridis(100)[85]) +
  geom_ribbon(data = rare.water, aes(x = water.upper, y = estimate__, ymin = lower__,
                                     ymax = upper__), alpha = 0.7, fill = viridis(100)[85]) +
  # Abundance
  geom_line(data = ab.water, aes(x = water.upper, y = estimate__), color = viridis(100)[1]) +
  geom_ribbon(data = ab.water, aes(x = water.upper, y = estimate__, ymin = lower__,
                                   ymax = upper__), alpha = 0.7, fill = viridis(100)[1]) +
  theme_bw() +
  theme(axis.text.x = element_text(size=22, face="bold")) +
  theme(axis.text.y = element_text(size=22, face="bold")) +
  theme(axis.title.x = element_text(size=22, face="bold", color="gray30")) +
  theme(axis.title.y = element_text(size=22, face="bold", color="gray30")) +
  scale_x_continuous(name="Change in Water Cover (%)", limits = c(-1.6, 5.6),
                     breaks=c(-1, 0, 1, 2, 3, 4, 5)) +
  theme(axis.title.y.right = element_text(vjust = 1.7)) +
  scale_y_continuous(name="Trend", limits = c(-0.023, 0.007),
                     breaks = c(-0.018, -0.012, -0.006, 0.000, 0.006),
                     sec.axis = dup_axis(name = "% Change",
                                         labels = c("-4.06", "-2.73", "-1.37",
                                                    "0.00", "1.39")))
# scale_x_continuous(name="\u0394Water/Year", limits = c(-4, 8.5),
#                    breaks=c(-4, -2, 0, 2, 4, 6, 8)) +
# scale_y_continuous(name="\u0394 / Year", limits = c(-0.009, 0.003),
#                    breaks = c(-0.009, -0.006, -0.003, 0.000, 0.003))

ggsave(paste(path2, "Water - univariate conditional effects.jpg",
             sep = ""),
       plot=waterplot,
       device = "jpg",
       width=7, height=5, dpi=600)