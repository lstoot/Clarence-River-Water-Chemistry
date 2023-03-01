#Load up all the packages we need
library(rgdal)
library(raster)
library(ggplot2)
library(cmocean)
library(cowplot)
library(tidyverse)
library(patchwork)
library(sf)
library(ggsn)
library(plotrix)
library(dplyr)

#------------------------------------- #
# Pre-processing the data for Figure 1 #
#--------------------------------------#

#Let's load in the data
setwd("~/GitHub/Water_Chemistry")
water <- read.csv("Input/Locations.csv")
head(water)

# Reorder plotting order
water$Reach <- factor(water$Reach, levels = c("Estuary - Lower Clarence", "Orara", "Mid - Clarence", "Mann - Nymboida", "Upper Clarence"))

# Load catchment shapefile:
catch <- st_read("Input/shapefile/MultiAttributeClarence.shp") # Use sf package (keep projection!)
head(catch)

catch[catch$LandCode == "f5k",]
catchrivers <- catch[catch$LandCode == "f5k",]
class(catchrivers)
catchrivers


catch[catch$LandCode == "f5b",]
lake <- catch[catch$LandCode == "f5b",]
class(lake)
lake

catch[catch$LandCode == "f5a",]
sriv <- catch[catch$LandCode == "f5a",]
class(sriv)
sriv

#-------------------------------------------------------------#
#                   Creating Figure 1                         #         
#-------------------------------------------------------------#

# Map water sampling locations in the Clarence - Figure 1A
sf_use_s2(FALSE)
plotA <- ggplot() + theme_bw() +
  geom_sf(data = catchrivers, fill = 'gray60', alpha = 0.4,  size = 0.07, colour = "black") +
  geom_sf(data = lake, fill = 'gray60', alpha = 0.4,  size = 0.07, colour = "black") +
  geom_sf(data = sriv, fill = 'gray60', alpha = 0.4,  size = 0.07, colour = "black") +
  geom_point(data = water, aes(x = Long, y = Lat, fill = Reach), size = 2.0, pch = 21, stroke = 0.2) +
  coord_sf(xlim = c(151.65, 153.5), ylim = c(-28.25, -30.5), expand = FALSE) +
  scale_fill_manual(values = c(cmocean('phase')(6)[1:5])) +
  scale_colour_manual(values = cmocean('thermal')(15)[1:5]) +
  labs(x = "Longitude", y = "Latitude", fill = "Reach",
       colour = "Reach") +
  guides(fill = guide_legend(ncol = 1)) +
  ggsn::scalebar(x.min = 153, x.max = 153.3, y.min = -30.30, y.max = -30.35, transform = TRUE, 
                 box.fill = c("black", "white"), box.color = "black", st.color = "black",
                 dist_unit = "km", dist = 10, st.dist = 1.0, st.size = 3, height = 0.30, border.size = 0.3)
plotA

library(ozmaps) # create Australian map with clarence location
library(cowplot) # plot both maps on top of each other

#map of Australia
oz_states <- ozmaps::ozmap_states
oz_states
library(dplyr)
oz_states %>%
  filter(NAME != "Other Territories") -> oz_mainland
m1 <- ggplot(oz_mainland) +
  geom_sf(aes(fill = FALSE), lwd = 0.1,
          show.legend = FALSE) +
  scale_fill_manual(values = c("White"))
m1 #this makes the map

# Average Clarence coordinate
mean(spatial$Latitude)
mean(spatial$Longitude)

#Australia map with red rectangle around the Clarence catchment
m2 <- m1 + theme_void() +
  theme(plot.background = element_rect(fill = 'white', colour = "black", size = 0.6, linetype = 'solid')) +
  annotate("rect", xmin = 152.45 , xmax = 153.4 , ymin = -29.85 , ymax = -29.3 , fill = NA, color = "red")
m2 #this is the Australia map with a rectangle around the area of interest

# to merge the site map with the Australia map
m3 <- ggdraw() +
  draw_plot(plotA) +
  draw_plot(m2, x = 0.4315, y = 0.7655, width = 0.22, height = 0.22) + plot_annotation(plot_annotation(title = 'a'))
m3

#To save a copy of the map
ggsave("Output/Map_clarence_watersamples.png", width = 20, height = 13, units = 'cm')

#--------------------------------------------------------------------------#
#                               Geology                                    #
# -------------------------------------------------------------------------#

#Let's load up the geology of the area
geo <- st_read("Input/shapefile/rock_units2.shp", stringsAsFactors = TRUE) # Use sf package (keep projection!)
head(geo)
class(geo)
str(geo)

#we're going to map 3 different layers (Dominant_L, NSW_CODE and Age_Medial)
#Remember to change the name of the layer in all below (ie: NSW_CODE), as well and the the total colour #s (ie: 239 vs 38)

#First we need to find how many "level" are in each "layer" 
class(geo$Province)
str(geo$Province)
levels(geo$Province)
unique(geo$Province)

#Now need to make it a factor & see how long it is
geo$Province<- as.factor(geo$Province)
summary(geo$Province)
length(levels(geo$Province))

#Overlapping geology and sites (Province)
sf_use_s2(FALSE)
geoplot <- ggplot() + theme_bw() +
  geom_sf(data = geo, aes(fill = as.factor(geo$Province)), alpha = 0.5, size = 0.3, show.legend = "fill") +
  #geom_sf(data = catchrivers, fill = 'gray60', alpha = 0.4,  size = 0.07, colour = "black") +
  #geom_sf(data = lake, fill = 'gray60', alpha = 0.4,  size = 0.07, colour = "black") +
  #geom_sf(data = sriv, fill = 'gray60', alpha = 0.4,  size = 0.07, colour = "black") +
  geom_point(data = water, aes(x = Long, y = Lat), size = 1.5, pch = 21, stroke = 0.2, colour = 'black', fill = 'black') +
  coord_sf(xlim = c(151.65, 153.5), ylim = c(-28.25, -30.5), expand = FALSE) +
  scale_fill_manual(values = rainbow(5)[1:5]) +
  scale_colour_manual(values = rainbow(5)[1:5]) +
  labs(x = "Longitude", y = "Latitude",
       colour = "Province", fill = "Province") +
  guides(fill = guide_legend(ncol = 1), guide_legend(title = "Province")) +
  ggsn::scalebar(x.min = 153, x.max = 153.3, y.min = -30.30, y.max = -30.35, transform = TRUE, 
                 box.fill = c("black", "white"), box.color = "black", st.color = "black",
                 dist_unit = "km", dist = 10, st.dist = 1.0, st.size = 3, height = 0.30, border.size = 0.3) + plot_annotation(title = 'b')
geoplot

ggsave("Output/Map_clarence_Province.png", width = 20, height = 13, units = 'cm')

#Merging sampling locations (1a) and geology (1b)
m3 + geoplot + plot_annotation(subtitle = 'a') + (plot_layout(guides = 'collect'))

ggsave("Output/Map_clarence_Figure1COMPLETE.png", width = 30, height = 23, units = 'cm')

#-------------------------------------------------------------------------#
##                    Let's do some actual stats                         ##
#-------------------------------------------------------------------------#

setwd("~/GitHub/Water_Chemistry")
element <- read.csv("Input/WaterChem.Results.csv")
element

#Lets clean up the data
#removes "na" columns
new_element <- element[-c(63, 68, 78), ]
new_element

#changes columns to numeric
i <- c(4:22)
new_element[ , i] <- apply(new_element[ , i], 2,            # Specify own function within apply
                    function(x) as.numeric(as.character(x)))
sapply(new_element, class)


#Now lets get the means for all our variables by reach 
------------------------------------------------------
#Basic Water Chemistry

#Temperature (Mean & SD)
aggregate(new_element$Temperature, list(new_element$Reach), FUN=mean)
aggregate(new_element$Temperature, list(new_element$Reach), FUN=sd)

#pH (Mean & SD)
aggregate(new_element$pH, list(new_element$Reach), FUN=mean)
aggregate(new_element$pH, list(new_element$Reach), FUN=sd)

#DO % (Mean & SD)
aggregate(new_element$DO.., list(new_element$Reach), FUN=mean)
aggregate(new_element$DO.., list(new_element$Reach), FUN=sd)

#DO (mg/L) (Mean & SD)
aggregate(new_element$DO.mg.L, list(new_element$Reach), FUN=mean)
aggregate(new_element$DO.mg.L, list(new_element$Reach), FUN=sd)

#Conductivity (Mean & SD)
aggregate(new_element$Conductivity..mS.cm., list(new_element$Reach), FUN=mean)
aggregate(new_element$Conductivity..mS.cm., list(new_element$Reach), FUN=sd)

#Turbidity (Mean & SD)
aggregate(new_element$Turbidity..NTU., list(new_element$Reach), FUN=mean)
aggregate(new_element$Turbidity..NTU., list(new_element$Reach), FUN=sd)

#Now for Elemental means & SDs

#Ba ppm (Mean & SD)
aggregate(new_element$Ba..ppm., list(new_element$Reach), FUN=mean)
aggregate(new_element$Ba..ppm., list(new_element$Reach), FUN=sd)

#Ca ppm (Mean & SD)
aggregate(new_element$Ca..ppm., list(new_element$Reach), FUN=mean)
aggregate(new_element$Ca..ppm., list(new_element$Reach), FUN=sd)

#Fe ppm (Mean & SD)
aggregate(new_element$Fe..ppm., list(new_element$Reach), FUN=mean)
aggregate(new_element$Fe..ppm., list(new_element$Reach), FUN=sd)

#Mn ppm (Mean & SD)
aggregate(new_element$Mn..ppm., list(new_element$Reach), FUN=mean)
aggregate(new_element$Mn..ppm., list(new_element$Reach), FUN=sd)

#Mg ppm (Mean & SD)
aggregate(new_element$Mg..ppm., list(new_element$Reach), FUN=mean)
aggregate(new_element$Mg..ppm., list(new_element$Reach), FUN=sd)

#Pb ppm (Mean & SD)
aggregate(new_element$Pb..ppm., list(new_element$Reach), FUN=mean)
aggregate(new_element$Pb..ppm., list(new_element$Reach), FUN=sd)

#Se ppm (Mean & SD)
aggregate(new_element$Se..ppm., list(new_element$Reach), FUN=mean)
aggregate(new_element$Se..ppm., list(new_element$Reach), FUN=sd)

#Sr ppm (Mean & SD)
aggregate(new_element$Sr..ppm., list(new_element$Reach), FUN=mean)
aggregate(new_element$Sr..ppm., list(new_element$Reach), FUN=sd)

#Ba/Ca ratio (Mean & SD)
aggregate(new_element$Ba.Ca, list(new_element$Reach), FUN=mean)
aggregate(new_element$Ba.Ca, list(new_element$Reach), FUN=sd)

#Sr/Ca ppm (Mean & SD)
aggregate(new_element$Sr.Ca, list(new_element$Reach), FUN=mean)
aggregate(new_element$Sr.Ca, list(new_element$Reach), FUN=sd)

#Sr 86/87 (Mean & SD)
aggregate(new_element$Sr87.86, list(new_element$Reach), FUN=mean)
aggregate(new_element$Sr87.86, list(new_element$Reach), FUN=sd)


#testshit
library(dplyr)
group_by(new_element, Reach) %>%
  summarise(
    count = n(),
    mean = mean(Temperature, na.rm = TRUE),
    sd = sd(Temperature, na.rm = TRUE),
    median = median(Temperature, na.rm = TRUE),
    IQR = IQR(Temperature, na.rm = TRUE)
  )

#Now let's see if the reaches are different!
#Kruskal-Wallis test - non-parametric one-way ANOVA test
#Pair-wise Wilcoxon Test

# no need for a p-value adjustment because there is no repeated sampling (?)
# exact = false fixes the "ties" issue and yields the same results.

#Temperature
kruskal.test(Temperature ~ Reach, data = new_element) 
pairwise.wilcox.test(new_element$Temperature, new_element$Reach,
                     p.adjust.method = "none", exact = FALSE)
#pH
kruskal.test(pH ~ Reach, data = new_element) 
pairwise.wilcox.test(new_element$pH, new_element$Reach,
                     p.adjust.method = "none", exact = FALSE)
#Do %
kruskal.test(DO.. ~ Reach, data = new_element) 
pairwise.wilcox.test(new_element$DO.., new_element$Reach,
                     p.adjust.method = "none", exact = FALSE)
#DO (mg/L)
kruskal.test(DO.mg.L ~ Reach, data = new_element) 
pairwise.wilcox.test(new_element$DO.mg.L, new_element$Reach,
                     p.adjust.method = "none", exact = FALSE)
#Conductivity
kruskal.test(Conductivity..mS.cm. ~ Reach, data = new_element) 
pairwise.wilcox.test(new_element$Conductivity..mS.cm., new_element$Reach,
                     p.adjust.method = "none", exact = FALSE)
#Turbidity
kruskal.test(Turbidity..NTU. ~ Reach, data = new_element) 
pairwise.wilcox.test(new_element$Turbidity..NTU., new_element$Reach,
                     p.adjust.method = "none", exact = FALSE)
#Ba
kruskal.test(Ba..ppm. ~ Reach, data = new_element) 
pairwise.wilcox.test(new_element$Ba..ppm, new_element$Reach,
                     p.adjust.method = "none", exact = FALSE)
#Ca
kruskal.test(Ca..ppm. ~ Reach, data = new_element)
pairwise.wilcox.test(new_element$Ca..ppm, new_element$Reach,
                     p.adjust.method = "none", exact = FALSE)
#Fe
kruskal.test(Fe..ppm. ~ Reach, data = new_element)
pairwise.wilcox.test(new_element$Fe..ppm, new_element$Reach,
                     p.adjust.method = "none", exact = FALSE)
#Mn
kruskal.test(Mn..ppm. ~ Reach, data = new_element)
pairwise.wilcox.test(new_element$Mn..ppm, new_element$Reach,
                     p.adjust.method = "none", exact = FALSE)
#Mg
kruskal.test(Mg..ppm. ~ Reach, data = new_element)
pairwise.wilcox.test(new_element$Mg..ppm, new_element$Reach,
                     p.adjust.method = "none", exact = FALSE)
#Pb
#kruskal.test(Pb..ppm. ~ Reach, data = new_element)
#pairwise.wilcox.test(new_element$Pb..ppm, new_element$Reach,
#                    p.adjust.method = "none", exact = FALSE)
#Se
#kruskal.test(Se..ppm. ~ Reach, data = new_element)
#pairwise.wilcox.test(new_element$Se..ppm, new_element$Reach,
#                     p.adjust.method = "none", exact = FALSE)
#Sr
kruskal.test(Sr..ppm. ~ Reach, data = new_element)
pairwise.wilcox.test(new_element$Sr..ppm, new_element$Reach,
                     p.adjust.method = "none", exact = FALSE)

#Sr8687
kruskal.test(Sr87.86 ~ Reach, data = new_element)
pairwise.wilcox.test(new_element$Sr87.86, new_element$Reach,
                     p.adjust.method = "none", exact = FALSE)

#2020 vs 2021
setwd("~/GitHub/Water_Chemistry")
seasons <- read.csv("Input/2020.2021.csv")
seasons

#Ba
kruskal.test(Ba ~ Year, data = seasons) 
pairwise.wilcox.test(seasons$Ba, seasons$Year, p.adjust.method = "none", exact = FALSE)

#Ca
kruskal.test(Ca ~ Year, data = seasons) 
pairwise.wilcox.test(seasons$Ca, seasons$Year, p.adjust.method = "none", exact = FALSE)

#Fe
kruskal.test(Fe ~ Year, data = seasons) 
pairwise.wilcox.test(seasons$Fe, seasons$Year, p.adjust.method = "none", exact = FALSE)

#Mg
kruskal.test(Mg ~ Year, data = seasons) 
pairwise.wilcox.test(seasons$Mg, seasons$Year, p.adjust.method = "none", exact = FALSE)

#Mn
kruskal.test(Mn ~ Year, data = seasons) 
pairwise.wilcox.test(seasons$Mn, seasons$Year, p.adjust.method = "none", exact = FALSE)

#Sr
kruskal.test(Sr ~ Year, data = seasons) 
pairwise.wilcox.test(seasons$Sr, seasons$Year, p.adjust.method = "none", exact = FALSE)

#Sr86.87
kruskal.test(Sr.8687 ~ Year, data = seasons) 
pairwise.wilcox.test(seasons$Sr, seasons$Year, p.adjust.method = "none", exact = FALSE)

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #
#                             Bar Graphs                            #
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #

#Now let's plot these bar graphs
#Load the data
setwd("~/GitHub/Water_Chemistry")
reach.means <- read.csv("Input/Means.csv")
head(reach.means)


#Water Chemistry
#Temperature
temps <- ggplot() + theme_bw() +
  geom_point(data = reach.means, aes(x = Reach, y = Temperature.Mean, fill = Reach), size = 3.0, pch = 21, stroke = 0.2  ) +
  labs(x = "Reach", y = "Mean Temperature (C)", title = "", fill = "Reach", colour = "Reach") + 
  scale_fill_manual(values = c(cmocean('phase')(6)[1:5])) +
  scale_colour_manual(values = cmocean('thermal')(15)[1:5]) +
  theme(axis.text.x = element_blank()) +
  theme(legend.text = element_text(size=9)) +
  theme(legend.title = element_text(size=9), legend.position = "bottom") +
  geom_errorbar(data = reach.means, aes(x = Reach, y = Temperature.Mean,
                                        ymin = Temperature.Mean - Temperature.SD, 
                                        ymax = Temperature.Mean + Temperature.SD), 
                size = 0.2, width = 0.2, alpha = 0.5)
temps

#pH
pH <- ggplot() + theme_bw() +
  geom_point(data = reach.means, aes(x = Reach, y = pH.Mean, fill = Reach), size = 3.0, pch = 21, stroke = 0.2  ) +
  labs(x = "Reach", y = "pH", title = "", fill = "", colour = "Reach") + 
  scale_fill_manual(values = c(cmocean('phase')(6)[1:5])) +
  scale_colour_manual(values = cmocean('thermal')(15)[1:5]) +
  theme(axis.text.x = element_blank()) +
  theme(legend.text = element_text(size=9)) +
  theme(legend.title = element_text(size=9), legend.position = "bottom") +
  geom_errorbar(data = reach.means, aes(x = Reach, y = pH.Mean,
                                        ymin = pH.Mean - pH.SD, 
                                        ymax = pH.Mean + pH.SD), 
                size = 0.2, width = 0.2, alpha = 0.5)
pH

#DO %
do.perc <- ggplot() + theme_bw() +
  geom_point(data = reach.means, aes(x = Reach, y = DO.Mean, fill = Reach), size = 3.0, pch = 21, stroke = 0.2  ) +
  labs(x = "Reach", y = "Dissolved Oxygen (%)", title = "", fill = "Reach", colour = "Reach") + 
  scale_fill_manual(values = c(cmocean('phase')(6)[1:5])) +
  scale_colour_manual(values = cmocean('thermal')(15)[1:5]) +
  theme(axis.text.x = element_blank()) +
  theme(legend.text = element_text(size=9)) +
  theme(legend.title = element_text(size=9), legend.position = "bottom") +
  geom_errorbar(data = reach.means, aes(x = Reach, y = DO.Mean,
                                        ymin = DO.Mean - DO.SD, 
                                        ymax = DO.Mean + DO.SD), 
                size = 0.2, width = 0.2, alpha = 0.5)
do.perc
  
#DO mg/L
do.mg <- ggplot() + theme_bw() +
  geom_point(data = reach.means, aes(x = Reach, y = DO.mg.Mean, fill = Reach), size = 3.0, pch = 21, stroke = 0.2  ) +
  labs(x = "Reach", y = "Dissolved Oxygen (mg/L)", title = "", fill = "Reach", colour = "Reach") + 
  scale_fill_manual(values = c(cmocean('phase')(6)[1:5])) +
  scale_colour_manual(values = cmocean('thermal')(15)[1:5]) +
  theme(axis.text.x = element_blank()) +
  theme(legend.text = element_text(size=9)) +
  theme(legend.title = element_text(size=9), legend.position = "bottom") +
  geom_errorbar(data = reach.means, aes(x = Reach, y = DO.mg.Mean,
                                        ymin = DO.mg.Mean - DO.mg.SD, 
                                        ymax = DO.mg.Mean + DO.mg.SD), 
                size = 0.2, width = 0.2, alpha = 0.5)
do.mg

#Conductivity
cond <- ggplot() + theme_bw() +
  geom_point(data = reach.means, aes(x = Reach, y = Cond.Mean, fill = Reach), size = 3.0, pch = 21, stroke = 0.2  ) +
  labs(x = "Reach", y = "Conductivity (mS/cm)", title = "", fill = "Reach", colour = "Reach") + 
  scale_fill_manual(values = c(cmocean('phase')(6)[1:5])) +
  scale_colour_manual(values = cmocean('thermal')(15)[1:5]) +
  theme(axis.text.x = element_blank()) +
  theme(legend.text = element_text(size=9)) +
  theme(legend.title = element_text(size=9), legend.position = "bottom") +
  geom_errorbar(data = reach.means, aes(x = Reach, y = Cond.Mean,
                                        ymin = Cond.Mean - Cond.SD, 
                                        ymax = Cond.Mean + Cond.SD), 
                size = 0.2, width = 0.2, alpha = 0.5)
cond

#Turbidity
turb <- ggplot() + theme_bw() +
  geom_point(data = reach.means, aes(x = Reach, y = Turb.Mean, fill = Reach), size = 3.0, pch = 21, stroke = 0.2  ) +
  labs(x = "Reach", y = "Turbidity (NTU)", title = "", fill = "Reach", colour = "Reach") +
  scale_fill_manual(values = c(cmocean('phase')(6)[1:5])) +
  scale_colour_manual(values = cmocean('thermal')(15)[1:5]) +
  theme(axis.text.x = element_blank()) + 
  theme(legend.text = element_text(size=9)) +
  theme(legend.title = element_text(size=9), legend.position = "bottom") +
  geom_errorbar(data = reach.means, aes(x = Reach, y = Turb.Mean,
                                        ymin = Turb.Mean - Turb.SD, 
                                        ymax = Turb.Mean + Turb.SD), 
                size = 0.2, width = 0.2, alpha = 0.5)
turb



#Let's merge them all together
temps + pH + do.perc + do.mg + cond + turb + plot_annotation(tag_levels = 'a') + (plot_layout(guides = 'collect')) & theme(legend.position = 'bottom')



#and let's save it for the manuscript :) 
ggsave("Output/Mean_plots_waterchem.png", width = 20, height = 13, units = "cm")  

----------------------------------------------------------------------------------------------------------------------

#Time for the Elemental Data 
#Load the data
setwd("~/GitHub/Water_Chemistry")
reach.means <- read.csv("Input/Means.csv")
head(reach.means)


#Ba mean with SD per reach
Ba.avs <- ggplot() + theme_bw() +
  geom_point(data = reach.means, aes(x = Reach, y = Ba.Mean, fill = Reach), size = 3.0, pch = 21, stroke = 0.2  ) +
  labs(x = "Reach", y = "Barium (ppm)", title = "", fill = "Reach", colour = "Reach") + 
  scale_fill_manual(values = c(cmocean('phase')(6)[1:5])) +
  scale_colour_manual(values = cmocean('thermal')(15)[1:5]) +
  theme(axis.text.x = element_blank()) +
  theme(legend.text = element_text(size=9)) +
  theme(legend.title = element_text(size=9), legend.position = "bottom") +
  geom_errorbar(data = reach.means, aes(x = Reach, y = Ba.Mean,
                                        ymin = Ba.Mean - Ba.SD, 
                                        ymax = Ba.Mean + Ba.SD), 
                size = 0.2, width = 0.2, alpha = 0.5)
Ba.avs

#Ca mean with SD per reach
Ca.avs <- ggplot() + theme_bw() +
  geom_point(data = reach.means, aes(x = Reach, y = Ca.Mean, fill = Reach), size = 3.0, pch = 21, stroke = 0.2  ) +
  labs(x = "Reach", y = "Calcium (ppm)", title = "", fill = "Reach", colour = "Reach") + 
  scale_fill_manual(values = c(cmocean('phase')(6)[1:5])) +
  scale_colour_manual(values = cmocean('thermal')(15)[1:5]) +
  theme(axis.text.x = element_blank()) +
  theme(legend.text = element_text(size=9)) +
  theme(legend.title = element_text(size=9), legend.position = "bottom") +
  geom_errorbar(data = reach.means, aes(x = Reach, y = Ca.Mean,
                                        ymin = Ca.Mean - Ca.SD, 
                                        ymax = Ca.Mean + Ca.SD), 
                size = 0.2, width = 0.2, alpha = 0.5)
Ca.avs

#Fe mean with SD per reach
Fe.avs <- ggplot() + theme_bw() +
  geom_point(data = reach.means, aes(x = Reach, y = Fe.Mean, fill = Reach), size = 3.0, pch = 21, stroke = 0.2  ) +
  labs(x = "Reach", y = "Iron (ppm)", title = "", fill = "Reach", colour = "Reach") + 
  scale_fill_manual(values = c(cmocean('phase')(6)[1:5])) +
  scale_colour_manual(values = cmocean('thermal')(15)[1:5]) +
  theme(axis.text.x = element_blank()) +
  theme(legend.text = element_text(size=9)) +
  theme(legend.title = element_text(size=9), legend.position = "bottom") +
  geom_errorbar(data = reach.means, aes(x = Reach, y = Fe.Mean,
                                        ymin = Fe.Mean - Fe.SD, 
                                        ymax = Fe.Mean + Fe.SD), 
                size = 0.2, width = 0.2, alpha = 0.5)
Fe.avs

#Mn mean with SD per reach
Mn.avs <- ggplot() + theme_bw() +
  geom_point(data = reach.means, aes(x = Reach, y = Mn.Mean, fill = Reach), size = 3.0, pch = 21, stroke = 0.2  ) +
  labs(x = "Reach", y = " Manganese (ppm)", title = "", fill = "Reach", colour = "Reach") + 
  scale_fill_manual(values = c(cmocean('phase')(6)[1:5])) +
  scale_colour_manual(values = cmocean('thermal')(15)[1:5]) +
  theme(axis.text.x = element_blank()) +
  theme(legend.text = element_text(size=9)) +
  theme(legend.title = element_text(size=9), legend.position = "bottom") +
  geom_errorbar(data = reach.means, aes(x = Reach, y = Mn.Mean,
                                        ymin = Mn.Mean - Mn.SD, 
                                        ymax = Mn.Mean + Mn.SD), 
                size = 0.2, width = 0.2, alpha = 0.5)
Mn.avs

#Mg mean with SD per reach
Mg.avs <- ggplot() + theme_bw() +
  geom_point(data = reach.means, aes(x = Reach, y = Mg.Mean, fill = Reach), size = 3.0, pch = 21, stroke = 0.2  ) +
  labs(x = "Reach", y = "Magnesium (ppm)", title = "", fill = "Reach", colour = "Reach") + 
  scale_fill_manual(values = c(cmocean('phase')(6)[1:5])) +
  scale_colour_manual(values = cmocean('thermal')(15)[1:5]) +
  theme(axis.text.x = element_blank()) +
  theme(legend.text = element_text(size=9)) +
  theme(legend.title = element_text(size=9), legend.position = "bottom") +
  geom_errorbar(data = reach.means, aes(x = Reach, y = Mg.Mean,
                                        ymin = Mg.Mean - Mg.SD, 
                                        ymax = Mg.Mean + Mg.SD), 
                size = 0.2, width = 0.2, alpha = 0.5)
Mg.avs

#Sr mean with SD per reach 
Sr.avs <- ggplot() + theme_bw() +
  geom_point(data = reach.means, aes(x = Reach, y = Sr.Mean, fill = Reach), size = 3.0, pch = 21, stroke = 0.2  ) +
  labs(x = "Reach", y = "Strontium (ppm)", title = "", fill = "Reach", colour = "Reach") + 
  scale_fill_manual(values = c(cmocean('phase')(6)[1:5])) +
  scale_colour_manual(values = cmocean('thermal')(15)[1:5]) +
  theme(axis.text.x = element_blank()) +
  theme(legend.text = element_text(size=9)) +
  theme(legend.title = element_text(size=9), legend.position = "bottom") +
  geom_errorbar(data = reach.means, aes(x = Reach, y = Sr.Mean,
                                        ymin = Sr.Mean - Sr.SD, 
                                        ymax = Sr.Mean + Sr.SD), 
                size = 0.2, width = 0.2, alpha = 0.5)
Sr.avs

#Let's merge them all together
Ba.avs + Ca.avs + Fe.avs + Mn.avs + Mg.avs + Sr.avs + plot_annotation(tag_levels = 'a') + (plot_layout(guides = 'collect')) & theme(legend.position = 'bottom')

#and let's save it for the manuscript :) 
ggsave("Output/Mean_Plots_Elements.png", width = 20, height = 13, units = "cm")  

#-------------------------------------#
##    Isotopic Relationships plots   ##
#-------------------------------------#

#Ba:Ca vs Sr:Ca
plot11 <- ggplot(new_element, aes(x=Sr.Ca, y=Ba.Ca, shape=Reach, color=Reach)) +
  geom_point() + theme_bw()
plot11

#Sr86.87 vs Sr:Ca
plot12 <- ggplot(new_element, aes(x=Sr.Ca, y=Sr87.86, shape=Reach, color=Reach)) +
  geom_point() + theme_bw()
plot12


#Let's merge them all together
plot11 + plot12 + plot_annotation(tag_levels = 'a') + (plot_layout(guides = 'collect')) & theme(legend.position = 'bottom')

#and let's save it for the manuscript :) 
ggsave("Output/IsotopeRelationships.png", width = 20, height = 13, units = "cm")

#--------------------------#
##    Isoscape Figures    ##
#--------------------------#

#Lets plot the isoscapes.. 

#Up first.. the Water Chemistry Isoscapes

#Temperature
sf_use_s2(FALSE)
Iso.Temp <- ggplot() + theme_bw() +
  geom_sf(data = catchrivers, fill = 'gray60', alpha = 0.4,  size = 0.07, colour = "black") +
  geom_sf(data = lake, fill = 'gray60', alpha = 0.4,  size = 0.07, colour = "black") +
  geom_sf(data = sriv, fill = 'gray60', alpha = 0.4,  size = 0.07, colour = "black") +
  geom_point(data = new_element, aes(x = Long, y = Lat, colour = Temperature, fill = Temperature), size = 2.0, pch = 21, stroke = 0.2) +
  coord_sf(xlim = c(151.65, 153.5), ylim = c(-28.25, -30.5), expand = FALSE) + 
  scale_colour_gradientn(colours = c("#1252ED", "#12ED3F", "#EDAD12", "#Ed1224"),
                         values = c(0, 0.10, 0.20, 0.30, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1)) +
  scale_fill_gradientn(colours = c("#1252ED", "#12ED3F", "#EDAD12", "#ED1224"),
                         values = c(0, 0.10, 0.20, 0.30, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1)) +
  labs(x = "Longitude", y = "Latitude", fill = "Temperature", colour = "Temperature") +
  ggsn::scalebar(x.min = 153, x.max = 153.3, y.min = -30.30, y.max = -30.35, transform = TRUE, 
                 box.fill = c("black", "white"), box.color = "black", st.color = "black",
                 dist_unit = "km", dist = 10, st.dist = 1.0, st.size = 3, height = 0.30, border.size = 0.3)
Iso.Temp
#and let's save it for the manuscript :) 
ggsave("Output/Isoscape.Temp.png", width = 20, height = 13, units = "cm")

#pH
sf_use_s2(FALSE)
Iso.pH <- ggplot() + theme_bw() +
  geom_sf(data = catchrivers, fill = 'gray60', alpha = 0.4,  size = 0.07, colour = "black") +
  geom_sf(data = lake, fill = 'gray60', alpha = 0.4,  size = 0.07, colour = "black") +
  geom_sf(data = sriv, fill = 'gray60', alpha = 0.4,  size = 0.07, colour = "black") +
  geom_point(data = new_element, aes(x = Long, y = Lat, colour = pH, fill = pH), size = 2.0, pch = 21, stroke = 0.2) +
  coord_sf(xlim = c(151.65, 153.5), ylim = c(-28.25, -30.5), expand = FALSE) + 
  scale_colour_gradientn(colours = c("#1252ED", "#12ED3F", "#EDAD12", "#Ed1224"),
                         values = c(0, 0.10, 0.20, 0.30, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1)) +
  scale_fill_gradientn(colours = c("#1252ED", "#12ED3F", "#EDAD12", "#ED1224"),
                       values = c(0, 0.10, 0.20, 0.30, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1)) +
  labs(x = "Longitude", y = "Latitude", fill = "pH", colour = "pH") +
  ggsn::scalebar(x.min = 153, x.max = 153.3, y.min = -30.30, y.max = -30.35, transform = TRUE, 
                 box.fill = c("black", "white"), box.color = "black", st.color = "black",
                 dist_unit = "km", dist = 10, st.dist = 1.0, st.size = 3, height = 0.30, border.size = 0.3)
Iso.pH
#and let's save it for the manuscript :) 
ggsave("Output/Isoscape.pH.png", width = 20, height = 13, units = "cm")

#DO %
#ADD MORE COLOURS
sf_use_s2(FALSE)
Iso.DO.Perc <- ggplot() + theme_bw() +
  geom_sf(data = catchrivers, fill = 'gray60', alpha = 0.4,  size = 0.07, colour = "black") +
  geom_sf(data = lake, fill = 'gray60', alpha = 0.4,  size = 0.07, colour = "black") +
  geom_sf(data = sriv, fill = 'gray60', alpha = 0.4,  size = 0.07, colour = "black") +
  geom_point(data = new_element, aes(x = Long, y = Lat, colour = DO.., fill = DO..), size = 2.0, pch = 21, stroke = 0.2) +
  coord_sf(xlim = c(151.65, 153.5), ylim = c(-28.25, -30.5), expand = FALSE) + 
  scale_colour_gradientn(colours = rainbow(10)) +
  scale_fill_gradientn(colours = rainbow(10)) +
  labs(x = "Longitude", y = "Latitude", fill = "DO..", colour = "DO..") +
  ggsn::scalebar(x.min = 153, x.max = 153.3, y.min = -30.30, y.max = -30.35, transform = TRUE, 
                 box.fill = c("black", "white"), box.color = "black", st.color = "black",
                 dist_unit = "km", dist = 10, st.dist = 1.0, st.size = 3, height = 0.30, border.size = 0.3)
Iso.DO.Perc
#and let's save it for the manuscript :) 
ggsave("Output/Isoscape.DO.png", width = 20, height = 13, units = "cm")

#DO(mg/L)
sf_use_s2(FALSE)
Iso.DO.mgl <- ggplot() + theme_bw() +
  geom_sf(data = catchrivers, fill = 'gray60', alpha = 0.4,  size = 0.07, colour = "black") +
  geom_sf(data = lake, fill = 'gray60', alpha = 0.4,  size = 0.07, colour = "black") +
  geom_sf(data = sriv, fill = 'gray60', alpha = 0.4,  size = 0.07, colour = "black") +
  geom_point(data = new_element, aes(x = Long, y = Lat, colour = DO.mg.L, fill = DO.mg.L), size = 2.0, pch = 21, stroke = 0.2) +
  coord_sf(xlim = c(151.65, 153.5), ylim = c(-28.25, -30.5), expand = FALSE) + 
  scale_colour_gradientn(colours = rainbow(10)) +
  scale_fill_gradientn(colours = rainbow(10)) +
  labs(x = "Longitude", y = "Latitude", fill = "DO.mg.L", colour = "DO.mg.L") +
  ggsn::scalebar(x.min = 153, x.max = 153.3, y.min = -30.30, y.max = -30.35, transform = TRUE, 
                 box.fill = c("black", "white"), box.color = "black", st.color = "black",
                 dist_unit = "km", dist = 10, st.dist = 1.0, st.size = 3, height = 0.30, border.size = 0.3)
Iso.DO.mgl
#and let's save it for the manuscript :) 
ggsave("Output/Isoscape.DO.mg.png", width = 20, height = 13, units = "cm")

#Conductivity
sf_use_s2(FALSE)
Iso.Cond <- ggplot() + theme_bw() +
  geom_sf(data = catchrivers, fill = 'gray60', alpha = 0.4,  size = 0.07, colour = "black") +
  geom_sf(data = lake, fill = 'gray60', alpha = 0.4,  size = 0.07, colour = "black") +
  geom_sf(data = sriv, fill = 'gray60', alpha = 0.4,  size = 0.07, colour = "black") +
  geom_point(data = new_element, aes(x = Long, y = Lat, colour = Conductivity..mS.cm., fill = Conductivity..mS.cm.), size = 2.0, pch = 21, stroke = 0.2) +
  coord_sf(xlim = c(151.65, 153.5), ylim = c(-28.25, -30.5), expand = FALSE) + 
  scale_colour_gradientn(colours = rainbow(10)) +
  scale_fill_gradientn(colours = rainbow(10)) +
  labs(x = "Longitude", y = "Latitude", fill = "Conductivity..mS.cm.", colour = "Conductivity..mS.cm.") +
  ggsn::scalebar(x.min = 153, x.max = 153.3, y.min = -30.30, y.max = -30.35, transform = TRUE, 
                 box.fill = c("black", "white"), box.color = "black", st.color = "black",
                 dist_unit = "km", dist = 10, st.dist = 1.0, st.size = 3, height = 0.30, border.size = 0.3)
Iso.Cond
#and let's save it for the manuscript :) 
ggsave("Output/Isoscape.Cond.png", width = 20, height = 13, units = "cm")

#Turbidity (NTU)
sf_use_s2(FALSE)
Iso.Turb <- ggplot() + theme_bw() +
  geom_sf(data = catchrivers, fill = 'gray60', alpha = 0.4,  size = 0.07, colour = "black") +
  geom_sf(data = lake, fill = 'gray60', alpha = 0.4,  size = 0.07, colour = "black") +
  geom_sf(data = sriv, fill = 'gray60', alpha = 0.4,  size = 0.07, colour = "black") +
  geom_point(data = new_element, aes(x = Long, y = Lat, colour = Turbidity..NTU., fill = Turbidity..NTU.), size = 2.0, pch = 21, stroke = 0.2) +
  coord_sf(xlim = c(151.65, 153.5), ylim = c(-28.25, -30.5), expand = FALSE) + 
  scale_colour_gradientn(colours = rainbow(10)) +
  scale_fill_gradientn(colours = rainbow(10)) +
  labs(x = "Longitude", y = "Latitude", fill = "Turbidity..NTU.", colour = "Turbidity..NTU.") +
  ggsn::scalebar(x.min = 153, x.max = 153.3, y.min = -30.30, y.max = -30.35, transform = TRUE, 
                 box.fill = c("black", "white"), box.color = "black", st.color = "black",
                 dist_unit = "km", dist = 10, st.dist = 1.0, st.size = 3, height = 0.30, border.size = 0.3)
Iso.Turb
#and let's save it for the manuscript :) 
ggsave("Output/Isoscape.Turb.png", width = 20, height = 13, units = "cm")

#Second up.. the Elemental Isoscapes

#Ba
Iso.Ba <- ggplot() + theme_bw() +
  geom_sf(data = catchrivers, fill = 'gray60', alpha = 0.4,  size = 0.07, colour = "black") +
  geom_sf(data = lake, fill = 'gray60', alpha = 0.4,  size = 0.07, colour = "black") +
  geom_sf(data = sriv, fill = 'gray60', alpha = 0.4,  size = 0.07, colour = "black") +
  geom_point(data = new_element, aes(x = Long, y = Lat, colour = Ba..ppm., fill = Ba..ppm.), size = 2.0, pch = 21, stroke = 0.2) +
  coord_sf(xlim = c(151.65, 153.5), ylim = c(-28.25, -30.5), expand = FALSE) + 
  scale_colour_gradientn(colours = rainbow(10)) +
  scale_fill_gradientn(colours = rainbow(10)) +
  labs(x = "Longitude", y = "Latitude", fill = "Ba..ppm.", colour = "Ba..ppm.") +
  ggsn::scalebar(x.min = 153, x.max = 153.3, y.min = -30.30, y.max = -30.35, transform = TRUE, 
                 box.fill = c("black", "white"), box.color = "black", st.color = "black",
                 dist_unit = "km", dist = 10, st.dist = 1.0, st.size = 3, height = 0.30, border.size = 0.3)
Iso.Ba
#and let's save it for the manuscript :) 
ggsave("Output/Isoscape.Ba.png", width = 20, height = 13, units = "cm")


#Ca
Iso.Ca <- ggplot() + theme_bw() +
  geom_sf(data = catchrivers, fill = 'gray60', alpha = 0.4,  size = 0.07, colour = "black") +
  geom_sf(data = lake, fill = 'gray60', alpha = 0.4,  size = 0.07, colour = "black") +
  geom_sf(data = sriv, fill = 'gray60', alpha = 0.4,  size = 0.07, colour = "black") +
  geom_point(data = new_element, aes(x = Long, y = Lat, colour = Ca..ppm., fill = Ca..ppm.), size = 2.0, pch = 21, stroke = 0.2) +
  coord_sf(xlim = c(151.65, 153.5), ylim = c(-28.25, -30.5), expand = FALSE) + 
  scale_colour_gradientn(colours = rainbow(10)) +
  scale_fill_gradientn(colours = rainbow(10)) +
  labs(x = "Longitude", y = "Latitude", fill = "Ca..ppm.", colour = "Ca..ppm.") +
  ggsn::scalebar(x.min = 153, x.max = 153.3, y.min = -30.30, y.max = -30.35, transform = TRUE, 
                 box.fill = c("black", "white"), box.color = "black", st.color = "black",
                 dist_unit = "km", dist = 10, st.dist = 1.0, st.size = 3, height = 0.30, border.size = 0.3)
Iso.Ca
#and let's save it for the manuscript :) 
ggsave("Output/Isoscape.Ca.png", width = 20, height = 13, units = "cm")


#Fe
Iso.Fe <- ggplot() + theme_bw() +
  geom_sf(data = catchrivers, fill = 'gray60', alpha = 0.4,  size = 0.07, colour = "black") +
  geom_sf(data = lake, fill = 'gray60', alpha = 0.4,  size = 0.07, colour = "black") +
  geom_sf(data = sriv, fill = 'gray60', alpha = 0.4,  size = 0.07, colour = "black") +
  geom_point(data = new_element, aes(x = Long, y = Lat, colour = Fe..ppm., fill = Fe..ppm.), size = 2.0, pch = 21, stroke = 0.2) +
  coord_sf(xlim = c(151.65, 153.5), ylim = c(-28.25, -30.5), expand = FALSE) + 
  scale_colour_gradientn(colours = rainbow(8)) +
  scale_fill_gradientn(colours = rainbow(8)) +
  labs(x = "Longitude", y = "Latitude", fill = "Fe..ppm.", colour = "Fe..ppm.") +
  ggsn::scalebar(x.min = 153, x.max = 153.3, y.min = -30.30, y.max = -30.35, transform = TRUE, 
                 box.fill = c("black", "white"), box.color = "black", st.color = "black",
                 dist_unit = "km", dist = 10, st.dist = 1.0, st.size = 3, height = 0.30, border.size = 0.3)
Iso.Fe
#and let's save it for the manuscript :) 
ggsave("Output/Isoscape.Fe.png", width = 20, height = 13, units = "cm")


#Mn
Iso.Mn <- ggplot() + theme_bw() +
  geom_sf(data = catchrivers, fill = 'gray60', alpha = 0.4,  size = 0.07, colour = "black") +
  geom_sf(data = lake, fill = 'gray60', alpha = 0.4,  size = 0.07, colour = "black") +
  geom_sf(data = sriv, fill = 'gray60', alpha = 0.4,  size = 0.07, colour = "black") +
  geom_point(data = new_element, aes(x = Long, y = Lat, colour = Mn..ppm., fill = Mn..ppm.), size = 2.0, pch = 21, stroke = 0.2) +
  coord_sf(xlim = c(151.65, 153.5), ylim = c(-28.25, -30.5), expand = FALSE) + 
  scale_colour_gradientn(colours = rainbow(8)) +
  scale_fill_gradientn(colours = rainbow(8)) +
  labs(x = "Longitude", y = "Latitude", fill = "Mn..ppm.", colour = "Mn..ppm.") +
  ggsn::scalebar(x.min = 153, x.max = 153.3, y.min = -30.30, y.max = -30.35, transform = TRUE, 
                 box.fill = c("black", "white"), box.color = "black", st.color = "black",
                 dist_unit = "km", dist = 10, st.dist = 1.0, st.size = 3, height = 0.30, border.size = 0.3)
Iso.Mn
#and let's save it for the manuscript :) 
ggsave("Output/Isoscape.Mn.png", width = 20, height = 13, units = "cm")

#Mg
Iso.Mg <- ggplot() + theme_bw() +
  geom_sf(data = catchrivers, fill = 'gray60', alpha = 0.4,  size = 0.07, colour = "black") +
  geom_sf(data = lake, fill = 'gray60', alpha = 0.4,  size = 0.07, colour = "black") +
  geom_sf(data = sriv, fill = 'gray60', alpha = 0.4,  size = 0.07, colour = "black") +
  geom_point(data = new_element, aes(x = Long, y = Lat, colour = Mg..ppm., fill = Mg..ppm.), size = 2.0, pch = 21, stroke = 0.2) +
  coord_sf(xlim = c(151.65, 153.5), ylim = c(-28.25, -30.5), expand = FALSE) + 
  scale_colour_gradientn(colours = rainbow(8)) +
  scale_fill_gradientn(colours = rainbow(8)) +
  labs(x = "Longitude", y = "Latitude", fill = "Mg..ppm.", colour = "Mg..ppm.") +
  ggsn::scalebar(x.min = 153, x.max = 153.3, y.min = -30.30, y.max = -30.35, transform = TRUE, 
                 box.fill = c("black", "white"), box.color = "black", st.color = "black",
                 dist_unit = "km", dist = 10, st.dist = 1.0, st.size = 3, height = 0.30, border.size = 0.3)
Iso.Mg
#and let's save it for the manuscript :) 
ggsave("Output/Isoscape.Mg.png", width = 20, height = 13, units = "cm")

#Sr
Iso.Sr <- ggplot() + theme_bw() +
  geom_sf(data = catchrivers, fill = 'gray60', alpha = 0.4,  size = 0.07, colour = "black") +
  geom_sf(data = lake, fill = 'gray60', alpha = 0.4,  size = 0.07, colour = "black") +
  geom_sf(data = sriv, fill = 'gray60', alpha = 0.4,  size = 0.07, colour = "black") +
  geom_point(data = new_element, aes(x = Long, y = Lat, colour = Sr..ppm., fill = Sr..ppm.), size = 2.0, pch = 21, stroke = 0.2) +
  coord_sf(xlim = c(151.65, 153.5), ylim = c(-28.25, -30.5), expand = FALSE) + 
  scale_colour_gradientn(colours = rainbow(8)) +
  scale_fill_gradientn(colours = rainbow(8)) +
  labs(x = "Longitude", y = "Latitude", fill = "Sr..ppm", colour = "Sr..ppm") +
  ggsn::scalebar(x.min = 153, x.max = 153.3, y.min = -30.30, y.max = -30.35, transform = TRUE, 
                 box.fill = c("black", "white"), box.color = "black", st.color = "black",
                 dist_unit = "km", dist = 10, st.dist = 1.0, st.size = 3, height = 0.30, border.size = 0.3)
Iso.Sr
#and let's save it for the manuscript :) 
ggsave("Output/Isoscape.Sr.png", width = 20, height = 13, units = "cm")


#Let's put these all together

Iso.Ca + Iso.Mg  + Iso.Sr + plot_annotation(tag_levels = 'a') + (plot_layout(guides = 'collect'))

#and let's save it for the manuscript :) 
ggsave("Output/Isoscacpe.TraceElementsA.png", width = 20, height = 13, units = "cm")

Iso.Ba + Iso.Fe + Iso.Mn + plot_annotation(tag_levels = 'a') + (plot_layout(guides = 'collect'))
#and let's save it for the manuscript :) 
ggsave("Output/Isoscacpe.TraceElementsB.png", width = 20, height = 13, units = "cm")

#sr 87.86
Iso.Sr.8786 <- ggplot() + theme_bw() +
  geom_sf(data = catchrivers, fill = 'gray60', alpha = 0.4,  size = 0.07, colour = "black") +
  geom_sf(data = lake, fill = 'gray60', alpha = 0.4,  size = 0.07, colour = "black") +
  geom_sf(data = sriv, fill = 'gray60', alpha = 0.4,  size = 0.07, colour = "black") +
  geom_point(data = new_element, aes(x = Long, y = Lat, colour = Sr87.86, fill = Sr87.86), size = 2.0, pch = 21, stroke = 0.2) +
  coord_sf(xlim = c(151.65, 153.5), ylim = c(-28.25, -30.5), expand = FALSE) + 
  scale_colour_gradientn(colours = rainbow(5)) +
  scale_fill_gradientn(colours = rainbow(5)) +
  labs(x = "Longitude", y = "Latitude", fill = "Sr87.86", colour = "Sr87.86.") +
  ggsn::scalebar(x.min = 153, x.max = 153.3, y.min = -30.30, y.max = -30.35, transform = TRUE, 
                 box.fill = c("black", "white"), box.color = "black", st.color = "black",
                 dist_unit = "km", dist = 10, st.dist = 1.0, st.size = 3, height = 0.30, border.size = 0.3)
Iso.Sr.8786

ggsave("Output/Isoscape.Sr86.87.png", width = 20, height = 13, units = "cm")

