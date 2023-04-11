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
library(FSA)



#------------------------------------- #
# Pre-processing the data for Figure 1 #
#--------------------------------------#

#Let's load in the data
setwd("~/GitHub/Water_Chemistry/")
water <- read.csv("Input/Locations.csv")
head(water)

# Reorder plotting order
water$Zone <- factor(water$Zone, levels = c("Estuary - Lower Clarence", "Orara", "Clarence main stem", "Mann - Nymboida - Boyd", "Northern tributaries"))

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
  geom_point(data = water, aes(x = Long, y = Lat, fill = Zone), size = 2.0, pch = 21, stroke = 0.2) +
  coord_sf(xlim = c(151.65, 153.5), ylim = c(-28.25, -30.5), expand = FALSE) +
  scale_fill_manual(values = c(cmocean('phase')(6)[1:5])) +
  scale_colour_manual(values = cmocean('thermal')(15)[1:5]) +
  labs(x = "Longitude", y = "Latitude", fill = "Zone",
       colour = "Zone") +
  guides(fill = guide_legend(ncol = 1)) +
  ggsn::scalebar(x.min = 153, x.max = 153.35, y.min = -30.30, y.max = -30.37, transform = TRUE, 
                 box.fill = c("black", "white"), box.color = "black", st.color = "black",
                 dist_unit = "km", dist = 10, st.dist = 1.0, st.size = 3, height = 0.50, border.size = 0.3)
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
  draw_plot(m2, x = 0.4545, y = 0.7675, width = 0.22, height = 0.22) + plot_annotation(plot_annotation(title = 'a'))
m3

#To save a copy of the map
ggsave("Output/Map_clarence_watersamples.png", width = 25, height = 17, units = 'cm')

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

ggsave("Output/Map_clarence_Province.png", width = 25, height = 17, units = 'cm')

#Merging sampling locations (1a) and geology (1b)
m3 + geoplot + plot_annotation(subtitle = 'a') + (plot_layout(guides = 'collect'))

ggsave("Output/Map_clarence_Figure1COMPLETE.png", width = 30, height = 23, units = 'cm')

#-------------------------------------------------------------------------#
##                    Let's do some actual stats                         ##
#-------------------------------------------------------------------------#

setwd("~/GitHub/Water_Chemistry/")
new_element <- read.csv("Input/WaterChem.Results.csv", header = T)
head(new_element)

sapply(new_element, class)

i <- c(6:24)

new_element[ , i] <- apply(new_element[ , i], 2,            # Specify own function within apply
                    function(x) as.numeric(as.character(x)))

sapply(new_element, class)  


#Now lets get the means for all our variables by zone
------------------------------------------------------
#Basic Water Chemistry

#Temperature (Mean & SD)
aggregate(new_element$Temperature, list(new_element$Zone), FUN=mean, na.rm=TRUE)
aggregate(new_element$Temperature, list(new_element$Zone), FUN=sd, na.rm=TRUE)

#pH (Mean & SD)
aggregate(new_element$pH, list(new_element$Zone), FUN=mean, na.rm=TRUE)
aggregate(new_element$pH, list(new_element$Zone), FUN=sd, na.rm=TRUE)

#DO % (Mean & SD)
aggregate(new_element$DO.., list(new_element$Zone), FUN=mean, na.rm=TRUE)
aggregate(new_element$DO.., list(new_element$Zone), FUN=sd, na.rm=TRUE)

#DO (mg/L) (Mean & SD)
aggregate(new_element$DO.mg.L, list(new_element$Zone), FUN=mean, na.rm=TRUE)
aggregate(new_element$DO.mg.L, list(new_element$Zone), FUN=sd, na.rm=TRUE)

#Conductivity (Mean & SD)
aggregate(new_element$Conductivity..mS.cm., list(new_element$Zone), FUN=mean, na.rm=TRUE)
aggregate(new_element$Conductivity..mS.cm., list(new_element$Zone), FUN=sd, na.rm=TRUE)

#Turbidity (Mean & SD)
aggregate(new_element$Turbidity..NTU., list(new_element$Zone), FUN=mean, na.rm=TRUE)
aggregate(new_element$Turbidity..NTU., list(new_element$Zone), FUN=sd, na.rm=TRUE)

#Now for Elemental means & SDs

#Ba ppm (Mean & SD)
aggregate(new_element$Ba, list(new_element$Zone), FUN=mean, na.rm=TRUE)
aggregate(new_element$Ba, list(new_element$Zone), FUN=sd, na.rm=TRUE)

#Ca ppm (Mean & SD)
aggregate(new_element$Ca, list(new_element$Zone), FUN=mean, na.rm=TRUE)
aggregate(new_element$Ca, list(new_element$Zone), FUN=sd, na.rm=TRUE)

#Fe ppm (Mean & SD)
aggregate(new_element$Fe, list(new_element$Zone), FUN=mean, na.rm=TRUE)
aggregate(new_element$Fe, list(new_element$Zone), FUN=sd, na.rm=TRUE)

#Mn ppm (Mean & SD)
aggregate(new_element$Mn, list(new_element$Zone), FUN=mean, na.rm=TRUE)
aggregate(new_element$Mn, list(new_element$Zone), FUN=sd, na.rm=TRUE)

#Mg ppm (Mean & SD)
aggregate(new_element$Mg, list(new_element$Zone), FUN=mean, na.rm=TRUE)
aggregate(new_element$Mg, list(new_element$Zone), FUN=sd, na.rm=TRUE)

#Sr ppm (Mean & SD)
aggregate(new_element$Sr, list(new_element$Zone), FUN=mean, na.rm=TRUE)
aggregate(new_element$Sr, list(new_element$Zone), FUN=sd, na.rm=TRUE)

#Ba/Ca ratio (Mean & SD)
aggregate(new_element$Ba.Ca, list(new_element$Zone), FUN=mean, na.rm=TRUE)
aggregate(new_element$Ba.Ca, list(new_element$Zone), FUN=sd, na.rm=TRUE)

#Fe/Ca ratio (Mean & SD)
aggregate(new_element$Fe.Ca, list(new_element$Zone), FUN=mean, na.rm=TRUE)
aggregate(new_element$Fe.Ca, list(new_element$Zone), FUN=sd, na.rm=TRUE)

#Mn/Ca ratio (Mean & SD)
aggregate(new_element$Mn.Ca, list(new_element$Zone), FUN=mean, na.rm=TRUE)
aggregate(new_element$Mn.Ca, list(new_element$Zone), FUN=sd, na.rm=TRUE)

#Mg/Ca ratio (Mean & SD)
aggregate(new_element$Mg.Ca, list(new_element$Zone), FUN=mean, na.rm=TRUE)
aggregate(new_element$Mg.Ca, list(new_element$Zone), FUN=sd, na.rm=TRUE)

#Sr/Ca ppm (Mean & SD)
aggregate(new_element$Sr.Ca, list(new_element$Zone), FUN=mean, na.rm=TRUE)
aggregate(new_element$Sr.Ca, list(new_element$Zone), FUN=sd, na.rm=TRUE)

#Sr 86/87 (Mean & SD)
aggregate(new_element$Sr87.86, list(new_element$Zone), FUN=mean, na.rm=TRUE)
aggregate(new_element$Sr87.86, list(new_element$Zone), FUN=sd, na.rm=TRUE)


#Now let's see if the Zonees are different!
#Kruskal-Wallis test - non-parametric one-way ANOVA test
#Post hoc - Dunns

# no need for a p-value adjustment because there is no repeated sampling (?)
# exact = false fixes the "ties" issue and yields the same results.

new_element$Zone = factor(new_element$Zone, levels = c("Estuary/Lower Clarence", "Orara", "Clarence main stem", "Mann - Nymboida - Boyd", "Northern tributaries"))

#Temperature
kruskal.test(Temperature ~ Zone, data = new_element) 
dunnTest(new_element$Temperature ~ new_element$Zone, data=new_element, method="none")

#pH
kruskal.test(pH ~ Zone, data = new_element) 
dunnTest(new_element$pH ~ new_element$Zone, data=new_element, method="none")

#Do %
kruskal.test(DO.. ~ Zone, data = new_element) 
dunnTest(new_element$DO.. ~ new_element$Zone, data=new_element, method="none")

#DO (mg/L)
kruskal.test(DO.mg.L ~ Zone, data = new_element) 
dunnTest(new_element$DO.mg.L ~ new_element$Zone, data=new_element, method="none")

#Conductivity
kruskal.test(Conductivity..mS.cm. ~ Zone, data = new_element) 
dunnTest(new_element$Conductivity..mS.cm. ~ new_element$Zone, data=new_element, method="none")

#Turbidity
kruskal.test(Turbidity..NTU. ~ Zone, data = new_element) 
dunnTest(new_element$Turbidity..NTU. ~ new_element$Zone, data=new_element, method="none")

#Ba
kruskal.test(Ba ~ Zone, data = new_element) 
dunnTest(new_element$Ba ~ new_element$Zone, data=new_element, method="none")

#Ca
kruskal.test(Ca. ~ Zone, data = new_element)
dunnTest(new_element$Ca ~ new_element$Zone, data=new_element, method="none")

#Fe
kruskal.test(Fe ~ Zone, data = new_element)
dunnTest(new_element$Fe ~ new_element$Zone, data=new_element, method="none")

#Mn
kruskal.test(Mn ~ Zone, data = new_element)
dunnTest(new_element$Mn ~ new_element$Zone, data=new_element, method="none")

#Mg
kruskal.test(Mg ~ Zone, data = new_element)
dunnTest(new_element$Mg ~ new_element$Zone, data=new_element, method="none")

#Sr
kruskal.test(Sr ~ Zone, data = new_element)
dunnTest(new_element$Sr ~ new_element$Zone, data=new_element, method="none")


#Sr8687
kruskal.test(Sr87.86 ~ Zone, data = new_element)
dunnTest(new_element$Sr87.86 ~ new_element$Zone, data=new_element, method="none")


####                      ####
###  Trace Element Ratios  ###
####                      ####


#Ba.Ca
kruskal.test(Ba.Ca ~ Zone, data = new_element)
dunnTest(new_element$Ba.Ca ~ new_element$Zone, data=new_element, method="none")

#Fe.Ca
kruskal.test(Fe.Ca ~ Zone, data = new_element)
dunnTest(new_element$Fe.Ca ~ new_element$Zone, data=new_element, method="none")

#Mg.Ca
kruskal.test(Mg.Ca ~ Zone, data = new_element)
dunnTest(new_element$Mg.Ca ~ new_element$Zone, data=new_element, method="none")

#Mn.Ca
kruskal.test(Mn.Ca ~ Zone, data = new_element)
dunnTest(new_element$Mn.Ca ~ new_element$Zone, data=new_element, method="none")

#Sr.Ca
kruskal.test(Sr.Ca ~ Zone, data = new_element)
dunnTest(new_element$Sr.Ca ~ new_element$Zone, data=new_element, method="none")

#2020 vs 2021
setwd("~/GitHub/Water_Chemistry")
seasons <- read.csv("Input/2020.2021.csv")
seasons

#Ba
wilcox.test(seasons$Ba ~ seasons$Year, data = seasons, paired = TRUE)

#Ca
wilcox.test(seasons$Ca ~ seasons$Year, data = seasons, paired = TRUE)

#Fe
wilcox.test(seasons$Fe ~ seasons$Year, data = seasons, paired = TRUE)

#Mg
wilcox.test(seasons$Mg ~ seasons$Year, data = seasons, paired = TRUE)

#Mn
wilcox.test(seasons$Mn ~ seasons$Year, data = seasons, paired = TRUE)

#Sr
wilcox.test(seasons$Sr ~ seasons$Year, data = seasons, paired = TRUE)

#Sr86.87
wilcox.test(seasons$Sr.8687 ~ seasons$Year, data = seasons, paired = TRUE)

#Ba.Ca
wilcox.test(seasons$Ba.Ca ~ seasons$Year, data = seasons, paired = TRUE)

#Fe.Ca
wilcox.test(seasons$Fe.Ca ~ seasons$Year, data = seasons, paired = TRUE)

#Mg.Ca
wilcox.test(seasons$Mg.Ca ~ seasons$Year, data = seasons, paired = TRUE)

#Mn.Ca
wilcox.test(seasons$Mn.Ca ~ seasons$Year, data = seasons, paired = TRUE)

#Sr.Ca
wilcox.test(seasons$Sr.Ca ~ seasons$Year, data = seasons, paired = TRUE)

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #
#                             Bar Graphs                            #
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #

#Now let's plot these bar graphs
#Load the data
setwd("~/GitHub/Water_Chemistry")
Zone.means <- read.csv("Input/Means.csv")
head(Zone.means)


#Water Chemistry
#Temperature
temps <- ggplot() + theme_bw() +
  geom_point(data = Zone.means, aes(x = Zone, y = Temperature.Mean, fill = Zone), size = 3.0, pch = 21, stroke = 0.2  ) +
  labs(x = "Zone", y = "Mean Temperature (C)", title = "", fill = "Zone", colour = "Zone") + 
  scale_fill_manual(values = c(cmocean('phase')(6)[1:5])) +
  scale_colour_manual(values = cmocean('thermal')(15)[1:5]) +
  theme(axis.text.x = element_blank()) +
  theme(legend.text = element_text(size=9)) +
  theme(legend.title = element_text(size=9), legend.position = "bottom") +
  geom_errorbar(data = Zone.means, aes(x = Zone, y = Temperature.Mean,
                                        ymin = Temperature.Mean - Temperature.SD, 
                                        ymax = Temperature.Mean + Temperature.SD), 
                size = 0.2, width = 0.2, alpha = 0.5)
temps

#pH
pH <- ggplot() + theme_bw() +
  geom_point(data = Zone.means, aes(x = Zone, y = pH.Mean, fill = Zone), size = 3.0, pch = 21, stroke = 0.2  ) +
  labs(x = "Zone", y = "pH", title = "", fill = "", colour = "Zone") + 
  scale_fill_manual(values = c(cmocean('phase')(6)[1:5])) +
  scale_colour_manual(values = cmocean('thermal')(15)[1:5]) +
  theme(axis.text.x = element_blank()) +
  theme(legend.text = element_text(size=9)) +
  theme(legend.title = element_text(size=9), legend.position = "bottom") +
  geom_errorbar(data = Zone.means, aes(x = Zone, y = pH.Mean,
                                        ymin = pH.Mean - pH.SD, 
                                        ymax = pH.Mean + pH.SD), 
                size = 0.2, width = 0.2, alpha = 0.5)
pH

#DO %
do.perc <- ggplot() + theme_bw() +
  geom_point(data = Zone.means, aes(x = Zone, y = DO.Mean, fill = Zone), size = 3.0, pch = 21, stroke = 0.2  ) +
  labs(x = "Zone", y = "Dissolved Oxygen (%)", title = "", fill = "Zone", colour = "Zone") + 
  scale_fill_manual(values = c(cmocean('phase')(6)[1:5])) +
  scale_colour_manual(values = cmocean('thermal')(15)[1:5]) +
  theme(axis.text.x = element_blank()) +
  theme(legend.text = element_text(size=9)) +
  theme(legend.title = element_text(size=9), legend.position = "bottom") +
  geom_errorbar(data = Zone.means, aes(x = Zone, y = DO.Mean,
                                        ymin = DO.Mean - DO.SD, 
                                        ymax = DO.Mean + DO.SD), 
                size = 0.2, width = 0.2, alpha = 0.5)
do.perc
  
#DO mg/L
do.mg <- ggplot() + theme_bw() +
  geom_point(data = Zone.means, aes(x = Zone, y = DO.mg.Mean, fill = Zone), size = 3.0, pch = 21, stroke = 0.2  ) +
  labs(x = "Zone", y = "Dissolved Oxygen (mg/L)", title = "", fill = "Zone", colour = "Zone") + 
  scale_fill_manual(values = c(cmocean('phase')(6)[1:5])) +
  scale_colour_manual(values = cmocean('thermal')(15)[1:5]) +
  theme(axis.text.x = element_blank()) +
  theme(legend.text = element_text(size=9)) +
  theme(legend.title = element_text(size=9), legend.position = "bottom") +
  geom_errorbar(data = Zone.means, aes(x = Zone, y = DO.mg.Mean,
                                        ymin = DO.mg.Mean - DO.mg.SD, 
                                        ymax = DO.mg.Mean + DO.mg.SD), 
                size = 0.2, width = 0.2, alpha = 0.5)
do.mg

#Conductivity
cond <- ggplot() + theme_bw() +
  geom_point(data = Zone.means, aes(x = Zone, y = Cond.Mean, fill = Zone), size = 3.0, pch = 21, stroke = 0.2  ) +
  labs(x = "Zone", y = "Conductivity (mS/cm)", title = "", fill = "Zone", colour = "Zone") + 
  scale_fill_manual(values = c(cmocean('phase')(6)[1:5])) +
  scale_colour_manual(values = cmocean('thermal')(15)[1:5]) +
  theme(axis.text.x = element_blank()) +
  theme(legend.text = element_text(size=9)) +
  theme(legend.title = element_text(size=9), legend.position = "bottom") +
  geom_errorbar(data = Zone.means, aes(x = Zone, y = Cond.Mean,
                                        ymin = Cond.Mean - Cond.SD, 
                                        ymax = Cond.Mean + Cond.SD), 
                size = 0.2, width = 0.2, alpha = 0.5)
cond

#Turbidity
turb <- ggplot() + theme_bw() +
  geom_point(data = Zone.means, aes(x = Zone, y = Turb.Mean, fill = Zone), size = 3.0, pch = 21, stroke = 0.2  ) +
  labs(x = "Zone", y = "Turbidity (NTU)", title = "", fill = "Zone", colour = "Zone") +
  scale_fill_manual(values = c(cmocean('phase')(6)[1:5])) +
  scale_colour_manual(values = cmocean('thermal')(15)[1:5]) +
  theme(axis.text.x = element_blank()) + 
  theme(legend.text = element_text(size=9)) +
  theme(legend.title = element_text(size=9), legend.position = "bottom") +
  geom_errorbar(data = Zone.means, aes(x = Zone, y = Turb.Mean,
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
Zone.means <- read.csv("Input/Means.csv")
head(Zone.means)


#Ba mean with SD per Zone
Ba.avs <- ggplot() + theme_bw() +
  geom_point(data = Zone.means, aes(x = Zone, y = Ba.Mean, fill = Zone), size = 3.0, pch = 21, stroke = 0.2  ) +
  labs(x = "Zone", y = "Barium (mg/L)", title = "", fill = "Zone", colour = "Zone") + 
  scale_fill_manual(values = c(cmocean('phase')(6)[1:5])) +
  scale_colour_manual(values = cmocean('thermal')(15)[1:5]) +
  theme(axis.text.x = element_blank()) +
  theme(legend.text = element_text(size=9)) +
  theme(legend.title = element_text(size=9), legend.position = "bottom") +
  geom_errorbar(data = Zone.means, aes(x = Zone, y = Ba.Mean,
                                        ymin = Ba.Mean - Ba.SD, 
                                        ymax = Ba.Mean + Ba.SD), 
                size = 0.2, width = 0.2, alpha = 0.5)
Ba.avs

#Ca mean with SD per Zone
Ca.avs <- ggplot() + theme_bw() +
  geom_point(data = Zone.means, aes(x = Zone, y = Ca.Mean, fill = Zone), size = 3.0, pch = 21, stroke = 0.2  ) +
  labs(x = "Zone", y = "Calcium (mg/L)", title = "", fill = "Zone", colour = "Zone") + 
  scale_fill_manual(values = c(cmocean('phase')(6)[1:5])) +
  scale_colour_manual(values = cmocean('thermal')(15)[1:5]) +
  theme(axis.text.x = element_blank()) +
  theme(legend.text = element_text(size=9)) +
  theme(legend.title = element_text(size=9), legend.position = "bottom") +
  geom_errorbar(data = Zone.means, aes(x = Zone, y = Ca.Mean,
                                        ymin = Ca.Mean - Ca.SD, 
                                        ymax = Ca.Mean + Ca.SD), 
                size = 0.2, width = 0.2, alpha = 0.5)
Ca.avs

#Fe mean with SD per Zone
Fe.avs <- ggplot() + theme_bw() +
  geom_point(data = Zone.means, aes(x = Zone, y = Fe.Mean, fill = Zone), size = 3.0, pch = 21, stroke = 0.2  ) +
  labs(x = "Zone", y = "Iron (mg/L)", title = "", fill = "Zone", colour = "Zone") + 
  scale_fill_manual(values = c(cmocean('phase')(6)[1:5])) +
  scale_colour_manual(values = cmocean('thermal')(15)[1:5]) +
  theme(axis.text.x = element_blank()) +
  theme(legend.text = element_text(size=9)) +
  theme(legend.title = element_text(size=9), legend.position = "bottom") +
  geom_errorbar(data = Zone.means, aes(x = Zone, y = Fe.Mean,
                                        ymin = Fe.Mean - Fe.SD, 
                                        ymax = Fe.Mean + Fe.SD), 
                size = 0.2, width = 0.2, alpha = 0.5)
Fe.avs

#Mn mean with SD per Zone
Mn.avs <- ggplot() + theme_bw() +
  geom_point(data = Zone.means, aes(x = Zone, y = Mn.Mean, fill = Zone), size = 3.0, pch = 21, stroke = 0.2  ) +
  labs(x = "Zone", y = " Manganese (mg/L)", title = "", fill = "Zone", colour = "Zone") + 
  scale_fill_manual(values = c(cmocean('phase')(6)[1:5])) +
  scale_colour_manual(values = cmocean('thermal')(15)[1:5]) +
  theme(axis.text.x = element_blank()) +
  theme(legend.text = element_text(size=9)) +
  theme(legend.title = element_text(size=9), legend.position = "bottom") +
  geom_errorbar(data = Zone.means, aes(x = Zone, y = Mn.Mean,
                                        ymin = Mn.Mean - Mn.SD, 
                                        ymax = Mn.Mean + Mn.SD), 
                size = 0.2, width = 0.2, alpha = 0.5)
Mn.avs

#Mg mean with SD per Zone
Mg.avs <- ggplot() + theme_bw() +
  geom_point(data = Zone.means, aes(x = Zone, y = Mg.Mean, fill = Zone), size = 3.0, pch = 21, stroke = 0.2  ) +
  labs(x = "Zone", y = "Magnesium (mg/L)", title = "", fill = "Zone", colour = "Zone") + 
  scale_fill_manual(values = c(cmocean('phase')(6)[1:5])) +
  scale_colour_manual(values = cmocean('thermal')(15)[1:5]) +
  theme(axis.text.x = element_blank()) +
  theme(legend.text = element_text(size=9)) +
  theme(legend.title = element_text(size=9), legend.position = "bottom") +
  geom_errorbar(data = Zone.means, aes(x = Zone, y = Mg.Mean,
                                        ymin = Mg.Mean - Mg.SD, 
                                        ymax = Mg.Mean + Mg.SD), 
                size = 0.2, width = 0.2, alpha = 0.5)
Mg.avs

#Sr mean with SD per Zone 
Sr.avs <- ggplot() + theme_bw() +
  geom_point(data = Zone.means, aes(x = Zone, y = Sr.Mean, fill = Zone), size = 3.0, pch = 21, stroke = 0.2  ) +
  labs(x = "Zone", y = "Strontium (mg/L)", title = "", fill = "Zone", colour = "Zone") + 
  scale_fill_manual(values = c(cmocean('phase')(6)[1:5])) +
  scale_colour_manual(values = cmocean('thermal')(15)[1:5]) +
  theme(axis.text.x = element_blank()) +
  theme(legend.text = element_text(size=9)) +
  theme(legend.title = element_text(size=9), legend.position = "bottom") +
  geom_errorbar(data = Zone.means, aes(x = Zone, y = Sr.Mean,
                                        ymin = Sr.Mean - Sr.SD, 
                                        ymax = Sr.Mean + Sr.SD), 
                size = 0.2, width = 0.2, alpha = 0.5)
Sr.avs

#Let's merge them all together
Ba.avs + Ca.avs + Fe.avs + Mn.avs + Mg.avs + Sr.avs + plot_annotation(tag_levels = 'a') + (plot_layout(guides = 'collect')) & theme(legend.position = 'bottom')

#and let's save it for the manuscript :) 
ggsave("Output/Mean_Plots_Elements.png", width = 25, height = 17, units = "cm")  



#Time for the Trace Element RATIOS
#Load the data
setwd("~/GitHub/Water_Chemistry")
Zone.means <- read.csv("Input/Means.csv")
head(Zone.means)


#Ba.Ca mean with SD per Zone
Ba.Ca.avs <- ggplot() + theme_bw() +
  geom_point(data = Zone.means, aes(x = Zone, y = Ba.Ca.Mean, fill = Zone), size = 3.0, pch = 21, stroke = 0.2  ) +
  labs(x = "Zone", y = "Ba:Ca", title = "", fill = "Zone", colour = "Zone") + 
  scale_fill_manual(values = c(cmocean('phase')(6)[1:5])) +
  scale_colour_manual(values = cmocean('thermal')(15)[1:5]) +
  theme(axis.text.x = element_blank()) +
  theme(legend.text = element_text(size=9)) +
  theme(legend.title = element_text(size=9), legend.position = "bottom") +
  geom_errorbar(data = Zone.means, aes(x = Zone, y = Ba.Ca.Mean,
                                        ymin = Ba.Ca.Mean - Ba.Ca.SD, 
                                        ymax = Ba.Ca.Mean + Ba.Ca.SD), 
                size = 0.2, width = 0.2, alpha = 0.5)
Ba.Ca.avs


#Fe.Ca mean with SD per Zone
Fe.Ca.avs <- ggplot() + theme_bw() +
  geom_point(data = Zone.means, aes(x = Zone, y = Fe.Ca.Mean, fill = Zone), size = 3.0, pch = 21, stroke = 0.2  ) +
  labs(x = "Zone", y = "Fe:Ca", title = "", fill = "Zone", colour = "Zone") + 
  scale_fill_manual(values = c(cmocean('phase')(6)[1:5])) +
  scale_colour_manual(values = cmocean('thermal')(15)[1:5]) +
  theme(axis.text.x = element_blank()) +
  theme(legend.text = element_text(size=9)) +
  theme(legend.title = element_text(size=9), legend.position = "bottom") +
  geom_errorbar(data = Zone.means, aes(x = Zone, y = Fe.Ca.Mean,
                                        ymin = Fe.Ca.Mean - Fe.Ca.SD, 
                                        ymax = Fe.Ca.Mean + Fe.Ca.SD), 
                size = 0.2, width = 0.2, alpha = 0.5)
Fe.Ca.avs

#Mn.Ca mean with SD per Zone
Mn.Ca.avs <- ggplot() + theme_bw() +
  geom_point(data = Zone.means, aes(x = Zone, y = Mn.Ca.Mean, fill = Zone), size = 3.0, pch = 21, stroke = 0.2  ) +
  labs(x = "Zone", y = " Mn:Ca", title = "", fill = "Zone", colour = "Zone") + 
  scale_fill_manual(values = c(cmocean('phase')(6)[1:5])) +
  scale_colour_manual(values = cmocean('thermal')(15)[1:5]) +
  theme(axis.text.x = element_blank()) +
  theme(legend.text = element_text(size=9)) +
  theme(legend.title = element_text(size=9), legend.position = "bottom") +
  geom_errorbar(data = Zone.means, aes(x = Zone, y = Mn.Ca.Mean,
                                        ymin = Mn.Ca.Mean - Mn.Ca.SD, 
                                        ymax = Mn.Ca.Mean + Mn.Ca.SD), 
                size = 0.2, width = 0.2, alpha = 0.5)
Mn.Ca.avs

#Mg.CA mean with SD per Zone
Mg.Ca.avs <- ggplot() + theme_bw() +
  geom_point(data = Zone.means, aes(x = Zone, y = Mg.Ca.Mean, fill = Zone), size = 3.0, pch = 21, stroke = 0.2  ) +
  labs(x = "Zone", y = "Mg:Ca", title = "", fill = "Zone", colour = "Zone") + 
  scale_fill_manual(values = c(cmocean('phase')(6)[1:5])) +
  scale_colour_manual(values = cmocean('thermal')(15)[1:5]) +
  theme(axis.text.x = element_blank()) +
  theme(legend.text = element_text(size=9)) +
  theme(legend.title = element_text(size=9), legend.position = "bottom") +
  geom_errorbar(data = Zone.means, aes(x = Zone, y = Mg.Ca.Mean,
                                        ymin = Mg.Ca.Mean - Mg.Ca.SD, 
                                        ymax = Mg.Ca.Mean + Mg.Ca.SD), 
                size = 0.2, width = 0.2, alpha = 0.5)
Mg.Ca.avs

#Sr.Ca mean with SD per Zone 
Sr.Ca.avs <- ggplot() + theme_bw() +
  geom_point(data = Zone.means, aes(x = Zone, y = Sr.Ca.Mean, fill = Zone), size = 3.0, pch = 21, stroke = 0.2  ) +
  labs(x = "Zone", y = "Sr:Ca", title = "", fill = "Zone", colour = "Zone") + 
  scale_fill_manual(values = c(cmocean('phase')(6)[1:5])) +
  scale_colour_manual(values = cmocean('thermal')(15)[1:5]) +
  theme(axis.text.x = element_blank()) +
  theme(legend.text = element_text(size=9)) +
  theme(legend.title = element_text(size=9), legend.position = "bottom") +
  geom_errorbar(data = Zone.means, aes(x = Zone, y = Sr.Ca.Mean,
                                        ymin = Sr.Ca.Mean - Sr.Ca.SD, 
                                        ymax = Sr.Ca.Mean + Sr.Ca.SD), 
                size = 0.2, width = 0.2, alpha = 0.5)
Sr.Ca.avs

#Sr.87.86 mean with SD per Zone
Sr.87.86.avs <- ggplot() + theme_bw() +
  geom_point(data = Zone.means, aes(x = Zone, y = Sr87.86.Mean, fill = Zone), size = 3.0, pch = 21, stroke = 0.2  ) +
  labs(x = "Zone", y = "Sr87:Sr86", title = "", fill = "Zone", colour = "Zone") + 
  scale_fill_manual(values = c(cmocean('phase')(6)[1:5])) +
  scale_colour_manual(values = cmocean('thermal')(15)[1:5]) +
  theme(axis.text.x = element_blank()) +
  theme(legend.text = element_text(size=9)) +
  theme(legend.title = element_text(size=9), legend.position = "bottom") +
  geom_errorbar(data = Zone.means, aes(x = Zone, y = Sr87.86.Mean,
                                       ymin = Sr87.86.Mean - Sr87.86.SD, 
                                       ymax = Sr87.86.Mean + Sr87.86.SD), 
                size = 0.2, width = 0.2, alpha = 0.5)
Sr.87.86.avs

#Let's merge them all together
Ba.Ca.avs + Fe.Ca.avs + Mn.Ca.avs + Mg.Ca.avs + Sr.Ca.avs + Sr.87.86.avs + plot_annotation(tag_levels = 'a') + (plot_layout(guides = 'collect')) & theme(legend.position = 'bottom')

#and let's save it for the manuscript :) 
ggsave("Output/Mean_Plots_TraceElementRatios.png", width = 25, height = 17, units = "cm")  

#-------------------------------------#
##    Isotopic Relationships plots   ##
#-------------------------------------#

#Ba:Ca vs Sr:Ca
plot11 <- ggplot(new_element, aes(x=Sr.Ca, y=Ba.Ca, shape=Zone, color=Zone)) + 
  labs(x = "Sr:Ca", y = "Ba:Ca") +
  geom_point() + theme_bw()
plot11

#Sr86.87 vs Sr:Ca
plot12 <- ggplot(new_element, aes(x=Sr.Ca, y=Sr87.86, shape=Zone, color=Zone)) + 
  labs(x = "Sr:Ca", y = "Sr87:Sr86") +
  geom_point() + theme_bw()
plot12


#Let's merge them all together
plot11 + plot12 + plot_annotation(tag_levels = 'a') + (plot_layout(guides = 'collect')) & theme(legend.position = 'bottom')

#and let's save it for the manuscript :) 
ggsave("Output/IsotopeRelationships.png", width = 25, height = 17, units = "cm")

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
ggsave("Output/Isoscape.Temp.png", width = 25, height = 17, units = "cm")

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
ggsave("Output/Isoscape.pH.png", width = 25, height = 17, units = "cm")

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
ggsave("Output/Isoscape.DO.png", width = 25, height = 17, units = "cm")

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
ggsave("Output/Isoscape.DO.mg.png", width = 25, height = 17, units = "cm")

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
ggsave("Output/Isoscape.Cond.png", width = 25, height = 17, units = "cm")

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
ggsave("Output/Isoscape.Turb.png", width = 25, height = 17, units = "cm")

#Second up.. the Elemental Isoscapes

#Ba
Iso.Ba <- ggplot() + theme_bw() +
  geom_sf(data = catchrivers, fill = 'gray60', alpha = 0.4,  size = 0.07, colour = "black") +
  geom_sf(data = lake, fill = 'gray60', alpha = 0.4,  size = 0.07, colour = "black") +
  geom_sf(data = sriv, fill = 'gray60', alpha = 0.4,  size = 0.07, colour = "black") +
  geom_point(data = new_element, aes(x = Long, y = Lat, colour = Ba, fill = Ba), size = 2.0, pch = 21, stroke = 0.2) +
  coord_sf(xlim = c(151.65, 153.5), ylim = c(-28.25, -30.5), expand = FALSE) + 
  scale_colour_gradientn(colours = rainbow(10)) +
  scale_fill_gradientn(colours = rainbow(10)) +
  labs(x = "Longitude", y = "Latitude", fill = "Ba", colour = "Ba") +
  ggsn::scalebar(x.min = 153, x.max = 153.3, y.min = -30.30, y.max = -30.35, transform = TRUE, 
                 box.fill = c("black", "white"), box.color = "black", st.color = "black",
                 dist_unit = "km", dist = 10, st.dist = 1.0, st.size = 3, height = 0.30, border.size = 0.3)
Iso.Ba
#and let's save it for the manuscript :) 
ggsave("Output/Isoscape.Ba.png", width = 25, height = 17, units = "cm")


#Ca
Iso.Ca <- ggplot() + theme_bw() +
  geom_sf(data = catchrivers, fill = 'gray60', alpha = 0.4,  size = 0.07, colour = "black") +
  geom_sf(data = lake, fill = 'gray60', alpha = 0.4,  size = 0.07, colour = "black") +
  geom_sf(data = sriv, fill = 'gray60', alpha = 0.4,  size = 0.07, colour = "black") +
  geom_point(data = new_element, aes(x = Long, y = Lat, colour = Ca, fill = Ca), size = 2.0, pch = 21, stroke = 0.2) +
  coord_sf(xlim = c(151.65, 153.5), ylim = c(-28.25, -30.5), expand = FALSE) + 
  scale_colour_gradientn(colours = rainbow(10)) +
  scale_fill_gradientn(colours = rainbow(10)) +
  labs(x = "Longitude", y = "Latitude", fill = "Ca", colour = "Ca") +
  ggsn::scalebar(x.min = 153, x.max = 153.3, y.min = -30.30, y.max = -30.35, transform = TRUE, 
                 box.fill = c("black", "white"), box.color = "black", st.color = "black",
                 dist_unit = "km", dist = 10, st.dist = 1.0, st.size = 3, height = 0.30, border.size = 0.3)
Iso.Ca
#and let's save it for the manuscript :) 
ggsave("Output/Isoscape.Ca.png", width = 25, height = 17, units = "cm")


#Fe
Iso.Fe <- ggplot() + theme_bw() +
  geom_sf(data = catchrivers, fill = 'gray60', alpha = 0.4,  size = 0.07, colour = "black") +
  geom_sf(data = lake, fill = 'gray60', alpha = 0.4,  size = 0.07, colour = "black") +
  geom_sf(data = sriv, fill = 'gray60', alpha = 0.4,  size = 0.07, colour = "black") +
  geom_point(data = new_element, aes(x = Long, y = Lat, colour = Fe, fill = Fe), size = 2.0, pch = 21, stroke = 0.2) +
  coord_sf(xlim = c(151.65, 153.5), ylim = c(-28.25, -30.5), expand = FALSE) + 
  scale_colour_gradientn(colours = rainbow(8)) +
  scale_fill_gradientn(colours = rainbow(8)) +
  labs(x = "Longitude", y = "Latitude", fill = "Fe", colour = "Fe") +
  ggsn::scalebar(x.min = 153, x.max = 153.3, y.min = -30.30, y.max = -30.35, transform = TRUE, 
                 box.fill = c("black", "white"), box.color = "black", st.color = "black",
                 dist_unit = "km", dist = 10, st.dist = 1.0, st.size = 3, height = 0.30, border.size = 0.3)
Iso.Fe
#and let's save it for the manuscript :) 
ggsave("Output/Isoscape.Fe.png", width = 25, height = 17, units = "cm")


#Mn
Iso.Mn <- ggplot() + theme_bw() +
  geom_sf(data = catchrivers, fill = 'gray60', alpha = 0.4,  size = 0.07, colour = "black") +
  geom_sf(data = lake, fill = 'gray60', alpha = 0.4,  size = 0.07, colour = "black") +
  geom_sf(data = sriv, fill = 'gray60', alpha = 0.4,  size = 0.07, colour = "black") +
  geom_point(data = new_element, aes(x = Long, y = Lat, colour = Mn, fill = Mn), size = 2.0, pch = 21, stroke = 0.2) +
  coord_sf(xlim = c(151.65, 153.5), ylim = c(-28.25, -30.5), expand = FALSE) + 
  scale_colour_gradientn(colours = rainbow(8)) +
  scale_fill_gradientn(colours = rainbow(8)) +
  labs(x = "Longitude", y = "Latitude", fill = "Mn", colour = "Mn") +
  ggsn::scalebar(x.min = 153, x.max = 153.3, y.min = -30.30, y.max = -30.35, transform = TRUE, 
                 box.fill = c("black", "white"), box.color = "black", st.color = "black",
                 dist_unit = "km", dist = 10, st.dist = 1.0, st.size = 3, height = 0.30, border.size = 0.3)
Iso.Mn
#and let's save it for the manuscript :) 
ggsave("Output/Isoscape.Mn.png", width = 25, height = 17, units = "cm")

#Mg
Iso.Mg <- ggplot() + theme_bw() +
  geom_sf(data = catchrivers, fill = 'gray60', alpha = 0.4,  size = 0.07, colour = "black") +
  geom_sf(data = lake, fill = 'gray60', alpha = 0.4,  size = 0.07, colour = "black") +
  geom_sf(data = sriv, fill = 'gray60', alpha = 0.4,  size = 0.07, colour = "black") +
  geom_point(data = new_element, aes(x = Long, y = Lat, colour = Mg, fill = Mg), size = 2.0, pch = 21, stroke = 0.2) +
  coord_sf(xlim = c(151.65, 153.5), ylim = c(-28.25, -30.5), expand = FALSE) + 
  scale_colour_gradientn(colours = rainbow(8)) +
  scale_fill_gradientn(colours = rainbow(8)) +
  labs(x = "Longitude", y = "Latitude", fill = "Mg", colour = "Mg") +
  ggsn::scalebar(x.min = 153, x.max = 153.3, y.min = -30.30, y.max = -30.35, transform = TRUE, 
                 box.fill = c("black", "white"), box.color = "black", st.color = "black",
                 dist_unit = "km", dist = 10, st.dist = 1.0, st.size = 3, height = 0.30, border.size = 0.3)
Iso.Mg
#and let's save it for the manuscript :) 
ggsave("Output/Isoscape.Mg.png", width = 25, height = 17, units = "cm")

#Sr
Iso.Sr <- ggplot() + theme_bw() +
  geom_sf(data = catchrivers, fill = 'gray60', alpha = 0.4,  size = 0.07, colour = "black") +
  geom_sf(data = lake, fill = 'gray60', alpha = 0.4,  size = 0.07, colour = "black") +
  geom_sf(data = sriv, fill = 'gray60', alpha = 0.4,  size = 0.07, colour = "black") +
  geom_point(data = new_element, aes(x = Long, y = Lat, colour = Sr, fill = Sr), size = 2.0, pch = 21, stroke = 0.2) +
  coord_sf(xlim = c(151.65, 153.5), ylim = c(-28.25, -30.5), expand = FALSE) + 
  scale_colour_gradientn(colours = rainbow(8)) +
  scale_fill_gradientn(colours = rainbow(8)) +
  labs(x = "Longitude", y = "Latitude", fill = "Sr", colour = "Sr") +
  ggsn::scalebar(x.min = 153, x.max = 153.3, y.min = -30.30, y.max = -30.35, transform = TRUE, 
                 box.fill = c("black", "white"), box.color = "black", st.color = "black",
                 dist_unit = "km", dist = 10, st.dist = 1.0, st.size = 3, height = 0.30, border.size = 0.3)
Iso.Sr
#and let's save it for the manuscript :) 
ggsave("Output/Isoscape.Sr.png", width = 25, height = 17, units = "cm")


#sr 87.86
Iso.Sr.8786 <- ggplot() + theme_bw() +
  geom_sf(data = catchrivers, fill = 'gray60', alpha = 0.4,  size = 0.07, colour = "black") +
  geom_sf(data = lake, fill = 'gray60', alpha = 0.4,  size = 0.07, colour = "black") +
  geom_sf(data = sriv, fill = 'gray60', alpha = 0.4,  size = 0.07, colour = "black") +
  geom_point(data = new_element, aes(x = Long, y = Lat, colour = Sr87.86, fill = Sr87.86), size = 2.0, pch = 21, stroke = 0.2) +
  coord_sf(xlim = c(151.65, 153.5), ylim = c(-28.25, -30.5), expand = FALSE) + 
  scale_colour_gradientn(colours = rainbow(5)) +
  scale_fill_gradientn(colours = rainbow(5)) +
  labs(x = "Longitude", y = "Latitude", fill = "Sr87:86", colour = "Sr87:86") +
  ggsn::scalebar(x.min = 153, x.max = 153.3, y.min = -30.30, y.max = -30.35, transform = TRUE, 
                 box.fill = c("black", "white"), box.color = "black", st.color = "black",
                 dist_unit = "km", dist = 10, st.dist = 1.0, st.size = 3, height = 0.30, border.size = 0.3)
Iso.Sr.8786

ggsave("Output/Isoscape.Sr86.87.png", width = 25, height = 17, units = "cm")

#TESTsr 87.86
TestIso.Sr.8786 <- ggplot() + theme_bw() +
  geom_sf(data = catchrivers, fill = 'gray60', alpha = 0.4,  size = 0.07, colour = "black") +
  geom_sf(data = lake, fill = 'gray60', alpha = 0.4,  size = 0.07, colour = "black") +
  geom_sf(data = sriv, fill = 'gray60', alpha = 0.4,  size = 0.07, colour = "black") +
  geom_point(data = new_element, aes(x = Long, y = Lat, colour = Sr.Num, fill = Sr.Num), size = 2.0, pch = 21, stroke = 0.2) +
  coord_sf(xlim = c(151.65, 153.5), ylim = c(-28.25, -30.5), expand = FALSE) + 
  scale_colour_gradientn(colours = rainbow(5)) +
  scale_fill_gradientn(colours = rainbow(5)) +
  labs(x = "Longitude", y = "Latitude", fill = "Sr.Num") +
  ggsn::scalebar(x.min = 153, x.max = 153.3, y.min = -30.30, y.max = -30.35, transform = TRUE, 
                 box.fill = c("black", "white"), box.color = "black", st.color = "black",
                 dist_unit = "km", dist = 10, st.dist = 1.0, st.size = 3, height = 0.30, border.size = 0.3)
TestIso.Sr.8786

ggsave("Output/Isoscape.SrNUM.png", width = 25, height = 17, units = "cm")


#Lets do the trace element:Ca ratios as isoscapes to see if there is anything fancy and shit

#Ba/Ca
Iso.Ba.Ca <- ggplot() + theme_bw() +
  geom_sf(data = catchrivers, fill = 'gray60', alpha = 0.4,  size = 0.07, colour = "black") +
  geom_sf(data = lake, fill = 'gray60', alpha = 0.4,  size = 0.07, colour = "black") +
  geom_sf(data = sriv, fill = 'gray60', alpha = 0.4,  size = 0.07, colour = "black") +
  geom_point(data = new_element, aes(x = Long, y = Lat, colour = Ba/Ca, fill = Ba/Ca), size = 2.0, pch = 21, stroke = 0.2) +
  coord_sf(xlim = c(151.65, 153.5), ylim = c(-28.25, -30.5), expand = FALSE) + 
  scale_colour_gradientn(colours = rainbow(10)) +
  scale_fill_gradientn(colours = rainbow(10)) +
  labs(x = "Longitude", y = "Latitude", fill = "Ba:Ca", colour = "Ba:Ca") +
  ggsn::scalebar(x.min = 153, x.max = 153.3, y.min = -30.30, y.max = -30.35, transform = TRUE, 
                 box.fill = c("black", "white"), box.color = "black", st.color = "black",
                 dist_unit = "km", dist = 10, st.dist = 1.0, st.size = 3, height = 0.30, border.size = 0.3)
Iso.Ba.Ca
#and let's save it for the manuscript :) 
ggsave("Output/Isoscape.Ba.Ca.png", width = 25, height = 17, units = "cm")

#Fe/Ca
Iso.Fe.Ca <- ggplot() + theme_bw() +
  geom_sf(data = catchrivers, fill = 'gray60', alpha = 0.4,  size = 0.07, colour = "black") +
  geom_sf(data = lake, fill = 'gray60', alpha = 0.4,  size = 0.07, colour = "black") +
  geom_sf(data = sriv, fill = 'gray60', alpha = 0.4,  size = 0.07, colour = "black") +
  geom_point(data = new_element, aes(x = Long, y = Lat, colour = Fe/Ca, fill = Fe/Ca), size = 2.0, pch = 21, stroke = 0.2) +
  coord_sf(xlim = c(151.65, 153.5), ylim = c(-28.25, -30.5), expand = FALSE) + 
  scale_colour_gradientn(colours = rainbow(10)) +
  scale_fill_gradientn(colours = rainbow(10)) +
  labs(x = "Longitude", y = "Latitude", fill = "Fe:Ca", colour = "Fe:Ca") +
  ggsn::scalebar(x.min = 153, x.max = 153.3, y.min = -30.30, y.max = -30.35, transform = TRUE, 
                 box.fill = c("black", "white"), box.color = "black", st.color = "black",
                 dist_unit = "km", dist = 10, st.dist = 1.0, st.size = 3, height = 0.30, border.size = 0.3)
Iso.Fe.Ca
#and let's save it for the manuscript :) 
ggsave("Output/Isoscape.Fe.Ca.png", width = 25, height = 17, units = "cm")

#Mg/Ca
Iso.Mg.Ca <- ggplot() + theme_bw() +
  geom_sf(data = catchrivers, fill = 'gray60', alpha = 0.4,  size = 0.07, colour = "black") +
  geom_sf(data = lake, fill = 'gray60', alpha = 0.4,  size = 0.07, colour = "black") +
  geom_sf(data = sriv, fill = 'gray60', alpha = 0.4,  size = 0.07, colour = "black") +
  geom_point(data = new_element, aes(x = Long, y = Lat, colour = Mg/Ca, fill = Mg/Ca), size = 2.0, pch = 21, stroke = 0.2) +
  coord_sf(xlim = c(151.65, 153.5), ylim = c(-28.25, -30.5), expand = FALSE) + 
  scale_colour_gradientn(colours = rainbow(10)) +
  scale_fill_gradientn(colours = rainbow(10)) +
  labs(x = "Longitude", y = "Latitude", fill = "Mg:Ca", colour = "Mg:Ca") +
  ggsn::scalebar(x.min = 153, x.max = 153.3, y.min = -30.30, y.max = -30.35, transform = TRUE, 
                 box.fill = c("black", "white"), box.color = "black", st.color = "black",
                 dist_unit = "km", dist = 10, st.dist = 1.0, st.size = 3, height = 0.30, border.size = 0.3)
Iso.Mg.Ca
#and let's save it for the manuscript :) 
ggsave("Output/Isoscape.Mg.Ca.png", width = 25, height = 17, units = "cm")

#Mn/Ca
Iso.Mn.Ca <- ggplot() + theme_bw() +
  geom_sf(data = catchrivers, fill = 'gray60', alpha = 0.4,  size = 0.07, colour = "black") +
  geom_sf(data = lake, fill = 'gray60', alpha = 0.4,  size = 0.07, colour = "black") +
  geom_sf(data = sriv, fill = 'gray60', alpha = 0.4,  size = 0.07, colour = "black") +
  geom_point(data = new_element, aes(x = Long, y = Lat, colour = Mn/Ca, fill = Mn/Ca), size = 2.0, pch = 21, stroke = 0.2) +
  coord_sf(xlim = c(151.65, 153.5), ylim = c(-28.25, -30.5), expand = FALSE) + 
  scale_colour_gradientn(colours = rainbow(10)) +
  scale_fill_gradientn(colours = rainbow(10)) +
  labs(x = "Longitude", y = "Latitude", fill = "Mn:Ca", colour = "Mn:Ca") +
  ggsn::scalebar(x.min = 153, x.max = 153.3, y.min = -30.30, y.max = -30.35, transform = TRUE, 
                 box.fill = c("black", "white"), box.color = "black", st.color = "black",
                 dist_unit = "km", dist = 10, st.dist = 1.0, st.size = 3, height = 0.30, border.size = 0.3)
Iso.Mn.Ca
#and let's save it for the manuscript :) 
ggsave("Output/Isoscape.Mn.Ca.png", width = 25, height = 17, units = "cm")

#Sr/Ca
Iso.Sr.Ca <- ggplot() + theme_bw() +
  geom_sf(data = catchrivers, fill = 'gray60', alpha = 0.4,  size = 0.07, colour = "black") +
  geom_sf(data = lake, fill = 'gray60', alpha = 0.4,  size = 0.07, colour = "black") +
  geom_sf(data = sriv, fill = 'gray60', alpha = 0.4,  size = 0.07, colour = "black") +
  geom_point(data = new_element, aes(x = Long, y = Lat, colour = Sr/Ca, fill = Sr/Ca), size = 2.0, pch = 21, stroke = 0.2) +
  coord_sf(xlim = c(151.65, 153.5), ylim = c(-28.25, -30.5), expand = FALSE) + 
  scale_colour_gradientn(colours = rainbow(10)) +
  scale_fill_gradientn(colours = rainbow(10)) +
  labs(x = "Longitude", y = "Latitude", fill = "Sr:Ca", colour = "Sr:Ca") +
  ggsn::scalebar(x.min = 153, x.max = 153.3, y.min = -30.30, y.max = -30.35, transform = TRUE, 
                 box.fill = c("black", "white"), box.color = "black", st.color = "black",
                 dist_unit = "km", dist = 10, st.dist = 1.0, st.size = 3, height = 0.30, border.size = 0.3)
Iso.Sr.Ca
#and let's save it for the manuscript :) 
ggsave("Output/Isoscape.Sr.Ca.png", width = 25, height = 17, units = "cm")
