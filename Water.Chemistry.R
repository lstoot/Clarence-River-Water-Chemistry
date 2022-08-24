library(actel)
library(RSP)
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
load("Input/Functions.RData") # Load custom function: calcAreas()

#-------------------------#
# Pre-processing the data #
#-------------------------#
setwd("~/GitHub/Water_Chemistry")
water <- read.csv("Input/Water.Chemistry.csv")
head(water)

# Load environmental stations
df.water <- read.csv("Input/Water.Chemistry.csv")
df.water <- subset(df.env, Station != "Baryulgil") # Station is outside study area!
df.water$Site <- c("CT1","CT2", "CT3", "CT4", "WL")
df.water$Site <- factor(df.env$Station, levels = c("CT1", "CT2", "CT3", "CT4", "WL"))

# Load Clarence River shapefile:
shp <- st_read("Input/shapefile/Clarence_total.shp") # Use sf package (keep projection!)

#-------------------------#
#     Creating Figure 1   #
#-------------------------#

# Map receivers in the Clarence
plot1 <- ggplot() + theme_bw() +
  geom_sf(data = shp,  
    fill = 'gray60', alpha = 0.4,
    size = 0.07, colour = "black") +
  geom_point(data = spatial, aes(x = Longitude, y = Latitude, fill = Array), pch = 21, stroke = 0.2) +
  coord_sf(xlim = c(152.45, 153.4), ylim = c(-29.85, -29.3), expand = FALSE) +
  scale_fill_manual(values = c(cmocean('phase')(6)[1:5])) +
  geom_text(data = df.env, aes(x = Long, y =  Lat, 
                          label = Station, colour = Type), 
                          check_overlap = FALSE, size = 2.8, fontface = "bold",
                          hjust = -0.2, vjust = 0,
                          show.legend = FALSE) +
  scale_colour_manual(values = cmocean('haline')(3)[1:2]) +
  labs(x = "Longitude", y = "Latitude", fill = "Acoustic array",
    colour = "Station type") +
  guides(fill = guide_legend(ncol = 1)) +
  ggsn::scalebar(x.min = 153, x.max = 153.2, y.min = -29.75, y.max = -29.7, transform = TRUE, 
    box.fill = c("black", "white"), box.color = "black", st.color = "black",
    dist_unit = "km", dist = 10, st.dist = 0.3, st.size = 3, height = 0.15, border.size = 0.3)
sf_use_s2(FALSE)
plot1

library(ozmaps) # create australian map with clarence location
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

# to merge the Receivers map with the Australia map
m3 <- ggdraw() +
  draw_plot(plot1) +
  draw_plot(m2, x = 0.0475, y = 0.1665, width = 0.26, height = 0.26)
m3

#To save a copy of the map
ggsave("Output/Map_clarence.png", width = 20, height = 13, units = 'cm')


#------------------------------#
# Use actel to filter the data #
#------------------------------#
setwd("Input")
exp.results <- explore(tz = 'Australia/Sydney', report = TRUE, GUI = 'never')
n
n
n
n
n
n
n
n
n
n
n
n
n
n
n
n
n
n
n
n
n
n
n
n
n
n
n
n
save(exp.results, file = "exp.results.RData") # Save filtered detections
setwd("..")

#--------------------------------------#
# Reconstruct in-water tracks with RSP #
#--------------------------------------#

# Load data processed previously with actel
load("Input/exp.results.RData") 

# Import shapefiles
water <- loadShape(path = "Input/shapefile/", shape = "Clarence.shp", size = 0.0005, buffer = 0.05)

# Check receivers are inside the water
plotRaster(input = exp.results, base.raster = water, 
  coord.x = "Longitude", coord.y = "Latitude") # Looks good (on-land receivers = no detections!)

# Create a transition layer with 8 directions
tl <- transitionLayer(x = water, directions = 8)

# Run RSP analysis
rsp.run <- runRSP(input = exp.results, t.layer = tl, 
  coord.x = "Longitude", coord.y = "Latitude",  
  verbose = TRUE, 
  max.time = 24) # Break tracks every 24-h

# Check RSP performance 
tracks <- do.call(rbind.data.frame, rsp.run$tracks)
# Add transmitter names
aux.names <- NULL
for (i in 1:length(rsp.run$tracks)) {
  aux <- rsp.run$tracks[[i]]
  aux.names <- c(aux.names, rep(names(rsp.run$tracks[i]), nrow(aux)))
}
tracks$Transmitter <- aux.names
# Check track data
head(tracks)
summary(as.numeric(tracks$Timespan)) # Maximum of 75-days track!

# Plot single track of one Transmitter: longer track
tracks[which(tracks$Timespan == max(tracks$Timespan))] # Which is the longest track?
plotTracks(input = rsp.run, base.raster = water, type = 'both',
  tag = "A69-1602-54094", track = 19) # Same location? Died? Doesn't look like...

tracks.ind <- tracks %>%
  group_by(Transmitter) %>%
  summarise(Tracks = n(), Total.time.days = round(sum(as.numeric(Timespan)) / 24, 0))

# Calculate distances travelled by sex (in km)
df.dist <- getDistances(input = rsp.run)
df.dist.km <- df.dist %>%
  filter(Loc.type == "RSP") %>% # Calculate distances with RSP positions!
  group_by(Group, Animal.tracked) %>%
  summarise(Distance = sum(Dist.travel) / 1000) # Convert from m to km
df.dist.km$Days <- tracks.ind$Total.time.days[match(df.dist.km$Animal.tracked, tracks.ind$Transmitter)]

# Some basic stats... 
# Average/max/min of distance travelled by all tagged fish
mean(df.dist.km$Distance)
sd(df.dist.km$Distance)
max(df.dist.km$Distance)
min(df.dist.km$Distance)

# Averages of distance travelled by group (M vs F)
summarize(df.dist.km,
          mean_distance = mean(Distance),
          sd_distance = sd(Distance))

# Convert list of RSP locations to data.frame 
rsp.locs <- do.call(rbind.data.frame, rsp.run$detections)

# Check RSP locations are inside the water
df.water <- as.data.frame(water, xy = TRUE)
df.water <- df.water[-which(is.na(df.water$layer))]
plot.rsp <- ggplot() + theme_bw() +
  geom_raster(data = subset(df.water, is.na(layer)), 
              aes(x = x, y = y), fill = "gray", alpha = 0.7) +
  geom_point(data = rsp.locs, aes(x = Longitude, y = Latitude, group = Track), 
             size = 0.3) +
  coord_cartesian(xlim = c(152.45, 153.45), ylim = c(-29.85, -29.3), expand = FALSE)
plot.rsp
# Locations look great (all inside water following river shape!)

# Save RSP output
save(rsp.run, file = "Input/rsp.results.RData") # Save filtered detections

# Load RSP output
load("Input/rsp.results.RData")

#--------------------------#
##   STATISTICAL TESTS    ##
#--------------------------#

#BASIC STATS

#count of number of detections at each section
rsp.locs %>% count(Section)
#count of number of detections at each receiver
rsp.locs %>% count(Receiver)

# Compare distances traveled by sex 
t.test(df.dist.km$Distance[df.dist.km$Group == "F"],
  df.dist.km$Distance[df.dist.km$Group == "M"]) # No statistical difference!

# Compare distances traveled and tracking time
cor.test(df.dist.km$Distance, df.dist.km$Days, method = 'pearson') # Not correlated!

# Compare distances traveled and length
df.bios.dist <- read.csv("Input/bios.dist.csv")
head(df.bios.dist)
df.bios.dist$Distance <- df.dist.km$Distance[match(df.bios.dist$Serial_nr, df.dist.km$Animal.tracked)]

#length
cor.test(df.bios.dist$Distance, df.bios.dist$Length_mm, method = 'pearson')
#weight
cor.test(df.bios.dist$Distance, df.bios.dist$Weight_kg, method = 'pearson')

#compare M vs F in length & weight
t.test(df.bios.dist$Length_mm[df.bios.dist$Group == "F"],
       df.bios.dist$Length_mm[df.bios.dist$Group == "M"]) #statistical difference!
t.test(df.bios.dist$Weight_kg[df.bios.dist$Group == "F"],
       df.bios.dist$Weight_kg[df.bios.dist$Group == "M"]) #statistical difference!

# Plot distances traveled by sex
plot1 <- ggplot() + theme_bw() +
  geom_col(data = subset(df.dist.km, Group == "F"), 
    aes(x = Distance, y = Animal.tracked), fill = 'dodgerblue') +
  labs(x = "Distance travelled (km)", y = "Transmitter", title = "Female") +
  coord_cartesian(xlim = c(0, 3100), ylim = c(0.3, 16.7), expand = FALSE) +
  annotate("text", x = df.dist.km$Distance[df.dist.km$Group == "F"] + 200,
    y = 1:16,
    label = df.dist.km$Days[df.dist.km$Group == "F"], size = 3)
plot2 <- ggplot() + theme_bw() +
  geom_col(data = subset(df.dist.km, Group == "M"), 
    aes(x = Distance, y = Animal.tracked), fill = 'dodgerblue') +
  labs(x = "Distance travelled (km)", y = "", title = "Male") +
  coord_cartesian(xlim = c(0, 3100), ylim = c(0.3, 14.7), expand = FALSE) +
  annotate("text", x = df.dist.km$Distance[df.dist.km$Group == "M"] + 200,
    y = 1:14,
    label = df.dist.km$Days[df.dist.km$Group == "M"], size = 3)
plot1 + plot2 + plot_annotation(tag_levels = 'A')
ggsave("Output/Distance_sex.png", width = 20, height = 13, units = "cm")
  

#--------------------------------------#
# Loading Environmental Data & Plotting#
#--------------------------------------#
# CONDUCTIVTY & TEMPERATURE
# Load environmental data
# get average daily value & sd for temp and conductivity 

#CL01
cond1 <- read.csv("Input/conductivity/CL01.csv")
cond1$Date <- as.Date(cond1$Date, format = "%d/%m/%Y")
cond1 <- cond1 %>%
  group_by(Date) %>%
  summarise(Temp.mean = mean(Temp, na.rm = TRUE),
            Temp.sd = sd(Temp, na.rm = TRUE),
            Cond.mean = mean(Cond, na.rm = TRUE),
            Cond.sd = sd(Cond, na.rm = TRUE))
cond1$Station <- "CT1"

#CL06
cond2 <- read.csv("Input/conductivity/CL06.csv")
cond2$Date <- as.Date(cond2$Date, format = "%d/%m/%Y")
cond2 <- cond2 %>%
  group_by(Date) %>%
  summarise(Temp.mean = mean(Temp, na.rm = TRUE),
            Temp.sd = sd(Temp, na.rm = TRUE),
            Cond.mean = mean(Cond, na.rm = TRUE),
            Cond.sd = sd(Cond, na.rm = TRUE))
cond2$Station <- "CT2"

#CL09
cond3 <- read.csv("Input/conductivity/CL09.csv")
cond3$Date <- as.Date(cond3$Date, format = "%d/%m/%Y")
cond3 <- cond3 %>%
  group_by(Date) %>%
  summarise(Temp.mean = mean(Temp, na.rm = TRUE),
            Temp.sd = sd(Temp, na.rm = TRUE),
            Cond.mean = mean(Cond, na.rm = TRUE),
            Cond.sd = sd(Cond, na.rm = TRUE))
cond3$Station <- "CT3"

#CL13
cond4 <- read.csv("Input/conductivity/CL13.csv")
cond4$Date <- as.Date(cond4$Date, format = "%d/%m/%Y")
cond4 <- cond4 %>%
  group_by(Date) %>%
  summarise(Temp.mean = mean(Temp, na.rm = TRUE),
            Temp.sd = sd(Temp, na.rm = TRUE),
            Cond.mean = mean(Cond, na.rm = TRUE),
            Cond.sd = sd(Cond, na.rm = TRUE))
cond4$Station <- "CT4"

# Combine all conductivty averages into 1 file (using tidyverse)
cond.tot <- rbind(cond1, cond2, cond3, cond4)
summary(cond.tot)

#Min/Max average temperature and conductivity in the study
max(cond.tot$Temp.mean, na.rm = TRUE)
min(cond.tot$Temp.mean, na.rm = TRUE)
max(cond.tot$Cond.mean, na.rm = TRUE)
min(cond.tot$Cond.mean, na.rm = TRUE)

# plot water temperature total for all four stations on one plot
plot1 <- ggplot() + theme_bw() +
  geom_point(data = cond.tot, aes(x = Date, y = Temp.mean, colour = Station),
    size = 0.9, alpha = 0.5) +
  geom_line(data = cond.tot, aes(x = Date, y = Temp.mean, colour = Station),
    lty = 'dashed', alpha = 0.5) +
  geom_errorbar(data = cond.tot, aes(x = Date, colour = Station,
                                       ymin = Temp.mean - Temp.sd, 
                                       ymax = Temp.mean + Temp.sd), 
  size = 0.2, width = 0, alpha = 0.5) +
  labs(y = "Temperature (°C)", x = "") +
  scale_x_date(limits = c(as.Date("2021-06-01"), as.Date("2022-06-01"))) +
  scale_y_continuous(breaks = seq(12, 32, 2)) +
  scale_colour_manual(values = cmocean('haline')(8)[c(1,3,5,7)])
plot1

# plot conductivity for all four stations on one plot
plot2 <- ggplot() + theme_bw() +
  geom_point(data = cond.tot, aes(x = Date, y = Cond.mean, colour = Station),
    size = 0.9, alpha = 0.5) +
  geom_line(data = cond.tot, aes(x = Date, y = Cond.mean, colour = Station),
    lty = 'dashed', alpha = 0.5) +
  geom_errorbar(data = cond.tot, aes(x = Date, colour = Station,
                                       ymin = Cond.mean - Cond.sd, 
                                       ymax = Cond.mean + Cond.sd), 
  size = 0.2, width = 0, alpha = 0.5) +
  labs(y = "Conductivity (mS/cm)", x = "") +
  scale_x_date(limits = c(as.Date("2021-06-01"), as.Date("2022-06-01"))) +
  scale_y_continuous(limits = c(0, 37)) +
  scale_colour_manual(values = cmocean('haline')(8)[c(1,3,5,7)])
plot2

# RIVER FLOW

# plot flow data
#NOT WORKING
flow <- read.csv("Input/Clarence.flow.csv")
flow$Date <- as.Date(flow$Day, format = "%d/%m/%Y")

plot3 <- ggplot() + theme_bw() +
  geom_point(data = flow, aes(x = Date, y = Mean.Water.level..m.),
    size = 0.9, alpha = 0.5) +
  geom_line(data = flow, aes(x = Date, y = Mean.Water.level..m.),
    lty = 'dashed', alpha = 0.5) +
  geom_errorbar(data = flow, aes(x = Date, 
                                       ymin = Min.Water.level..m., 
                                       ymax = Max.Water.level..m.), 
  size = 0.2, width = 0, alpha = 0.5) +
  labs(y = "Water level (m)", x = "Date") +
  scale_x_date(limits = c(as.Date("2021-06-01"), as.Date("2022-06-01"))) 
plot3

# Combine environmental plots same figure:
plot1 / plot2 / plot3 + plot_annotation(tag_levels = 'A') + plot_layout(guides = "collect")
ggsave("Output/Env_vars.png", width = 22, height = 17, units = "cm")


#--------------------------------------#
#                dBBMM                 #
#--------------------------------------#

# Import shapefile
water <- loadShape(path = "Input/shapefile/", shape = "Clarence.shp", size = 0.0005, buffer = 0.2)

# Load RSP output
load("Input/rsp.results.RData")

# Run daily dBBMM models (this function can take a couple of hours to run!)
dbbmm.out <- calcAreas(input = rsp.run, base.raster = water, UTM = 56)
write.csv(dbbmm.out, "Input/dBBMM_output.csv", row.names = FALSE)




