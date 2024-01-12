library(rerddapXtracto)
library(rerddap)
library(wesanderson)
library(here)

# Coefficient of variation function
cv <- function(x) {sd(na.omit(x))/mean(na.omit(x))}


# Extract polygon coordinates ---------------------------------------------


# Coastal ocean subregion 0: Transition Zone
poly0 <- read.csv(here::here("Coastal Ocean Polygon", "poly0_coord_020215.csv"))

# Coastal ocean subregion 1: Puget Sound
poly1 <- read.csv(here::here("Coastal Ocean Polygon", "poly1_coord_020215.csv"))

# Coastal ocean subregion 2: WA Coast & Columbia R
poly2 <- read.csv(here::here("Coastal Ocean Polygon", "poly2_coord_020215.csv"))

# Coastal ocean subregion 3: N and Central OR Coast
poly3 <- read.csv(here::here("Coastal Ocean Polygon", "poly3_coord_020215.csv"))

# Coastal ocean subregion 4: S OR and N CA Coast
poly4 <- read.csv(here::here("Coastal Ocean Polygon", "poly4_coord_020215.csv"))

# Coastal ocean subregion 5: Mendocino Coast and San Fran Coast
poly5 <- read.csv(here::here("Coastal Ocean Polygon", "poly5_coord_020215.csv"))

# Coastal ocean subregion 6: Monterey Bay
poly6 <- read.csv(here::here("Coastal Ocean Polygon", "poly6_coord_020215.csv"))

# Coastal ocean subregion 7: S CA Bight
poly7 <- read.csv(here::here("Coastal Ocean Polygon", "poly7_coord_020215.csv"))

# Coastal ocean subregion N1: Off Vancouver Island, for coho
polyN1 <- rbind(poly2[17:30,], poly1[1:100,])
polyN1 <- rbind(polyN1, c(-128,51.5), c(-130.75,50.75), c(-128.5, 49),  c(-126.0477, 48.1148))

# Coastal ocean subregion N2: Off Vancouver Island through Queen Charlotte Islands
polyN2 <- rbind(poly2[17:30,], poly1[1:100,])
polyN2 <- rbind(polyN2, c(-128,55.5), c(-135,54), c(-128.5, 49),  c(-126.0477, 48.1148))

# Coastal ocean subregion N3: Southeast Alaska (not including Yakutat Coast)
polyN3 <- data.frame(rbind( c(-128,55.5), c(-135,54),
                            c(-141, 58), c(-135, 59), c(-128,55.5)))
names(polyN3) <- c("x","y")



# Quick map of polygons ---------------------------------------------------------

library(ggplot2)
library(plotly)
library(tidyverse)
library(maps)

# Create a base map for the west coast of North America
west_coast_CA <- map_data("world", region =  "Canada") %>%
  filter( subregion %in% c("Vancouver Island"))

west_coast_US<- map_data("state") %>%
  filter(region %in% c("california", "oregon", "washington"))

west_coast <- rbind(west_coast_CA,west_coast_US)

# Specify the latitude and longitude limits
lat_limits <- c(32, 49)  # Adjust as needed
long_limits <- c(-130, -117)  # Adjust as needed

# Create a ggplot with the base map
ggplotly(
  ggplot() +
  geom_polygon(data = west_coast, aes(x = long, y = lat, group = group), fill = "white", color = "black") +
  geom_point(data = poly0, aes(x = x, y = y), color = "red", size = 1) +
  geom_point(data = poly1, aes(x = x, y = y), color = "orange", size = 1) +
  geom_point(data = poly2, aes(x = x, y = y), color = "yellow", size = 1) +
  geom_point(data = poly3, aes(x = x, y = y), color = "green", size = 1) +
  geom_point(data = poly4, aes(x = x, y = y), color = "blue", size = 1) +
  geom_point(data = poly5, aes(x = x, y = y), color = "pink", size = 1) +
  geom_point(data = poly6, aes(x = x, y = y), color = "violet", size = 1) +
  geom_point(data = poly7, aes(x = x, y = y), color = "red4", size = 1) +
  geom_point(data = polyN1, aes(x = x, y = y), color = "green4", size = 1) +
  geom_point(data = polyN2, aes(x = x, y = y), color = "orange4", size = 1) +
  geom_point(data = polyN3, aes(x = x, y = y), color = "purple4", size = 1) +
  ggtitle("Map of the West Coast from California to Canada") +
  theme_minimal(),
    tooltip = c( "x", "y")
)
# xlim(long_limits) +
# ylim(lat_limits)





# ERSST V5 ----------------------------------------------------------------

dataInfo <-rerddap::info("nceiErsstv5_LonPM180")
tpos<-c('1854-01-01','2023-11-15')

# Coastal ocean subregion 0
ypos<-(poly0[,2])    # lat
xpos<-(poly0[,1])  # lon
zpos<-rep(0.,length(xpos)) #depth

SST.dummy <- rerddapXtracto::rxtractogon(dataInfo, xcoord=xpos, ycoord=ypos, tcoord=tpos, zcoord = zpos, parameter='sst', zName = 'depth') # array of [Lon, Lat, depth, Time]

# Monthly & Yearly means
mo.mean.SST.ersst0 <- apply(SST.dummy$sst, 3, mean, na.rm = TRUE)
yr.mean.SST.ersst0 <- aggregate(mo.mean.SST.ersst0, by=list(0:(length(mo.mean.SST.ersst0)-1) %/% 12), mean)
mo.min.SST.ersst0 <- apply(SST.dummy$sst, 3, min, na.rm = TRUE)
yr.min.SST.ersst0 <- aggregate(mo.min.SST.ersst0, by=list(0:(length(mo.min.SST.ersst0)-1) %/% 12), min)
mo.max.SST.ersst0 <- apply(SST.dummy$sst, 3, max, na.rm = TRUE)
yr.max.SST.ersst0 <- aggregate(mo.max.SST.ersst0, by=list(0:(length(mo.max.SST.ersst0)-1) %/% 12), max)
mo.sd.SST.ersst0 <- apply(SST.dummy$sst, 3, sd, na.rm = TRUE)
yr.sd.SST.ersst0 <- aggregate(mo.max.SST.ersst0, by=list(0:(length(mo.max.SST.ersst0)-1) %/% 12), sd)
mo.cv.SST.ersst0 <- mo.sd.SST.ersst0 / mo.mean.SST.ersst0
yr.cv.SST.ersst0 <- aggregate(mo.max.SST.ersst0, by=list(0:(length(mo.max.SST.ersst0)-1) %/% 12), cv)


# Coastal ocean subregion 1
ypos<-(poly1[,2])    # lat
xpos<-(poly1[,1])   # lon
zpos<-rep(0.,length(xpos)) #depth

SST.dummy <- rerddapXtracto::rxtractogon(dataInfo, xcoord=xpos, ycoord=ypos, tcoord=tpos, zcoord = zpos, parameter='sst', zName = 'depth') # array of [Lon, Lat, depth, Time]
#str(SST.dummy) NA's for SST

# Monthly & Yearly means
mo.mean.SST.ersst1 <- apply(SST.dummy$sst, 3, mean, na.rm = TRUE)
yr.mean.SST.ersst1 <- aggregate(mo.mean.SST.ersst1, by=list(0:(length(mo.mean.SST.ersst1)-1) %/% 12), mean)
mo.min.SST.ersst1 <- apply(SST.dummy$sst, 3, min, na.rm = TRUE)
yr.min.SST.ersst1 <- aggregate(mo.min.SST.ersst1, by=list(0:(length(mo.min.SST.ersst1)-1) %/% 12), min)
mo.max.SST.ersst1 <- apply(SST.dummy$sst, 3, max, na.rm = TRUE)
yr.max.SST.ersst1 <- aggregate(mo.max.SST.ersst1, by=list(0:(length(mo.max.SST.ersst1)-1) %/% 12), max)
mo.sd.SST.ersst1 <- apply(SST.dummy$sst, 3, sd, na.rm = TRUE)
yr.sd.SST.ersst1 <- aggregate(mo.max.SST.ersst1, by=list(0:(length(mo.max.SST.ersst1)-1) %/% 12), sd)
mo.cv.SST.ersst1 <- mo.sd.SST.ersst1 / mo.mean.SST.ersst1
yr.cv.SST.ersst1 <- aggregate(mo.max.SST.ersst1, by=list(0:(length(mo.max.SST.ersst1)-1) %/% 12), cv)


# Coastal ocean subregion 2
ypos<-(poly2[,2])    # lat
xpos<-(poly2[,1])  # lon
zpos<-rep(0.,length(xpos)) #depth

SST.dummy <- rerddapXtracto::rxtractogon(dataInfo, xcoord=xpos, ycoord=ypos, tcoord=tpos, zcoord = zpos, parameter='sst', zName = 'depth') # array of [Lon, Lat, depth, Time]


# Monthly & Yearly means
mo.mean.SST.ersst2 <- apply(SST.dummy$sst, 3, mean, na.rm = TRUE)
yr.mean.SST.ersst2 <- aggregate(mo.mean.SST.ersst2, by=list(0:(length(mo.mean.SST.ersst2)-1) %/% 12), mean)
mo.min.SST.ersst2 <- apply(SST.dummy$sst, 3, min, na.rm = TRUE)
yr.min.SST.ersst2 <- aggregate(mo.min.SST.ersst2, by=list(0:(length(mo.min.SST.ersst2)-1) %/% 12), min)
mo.max.SST.ersst2 <- apply(SST.dummy$sst, 3, max, na.rm = TRUE)
yr.max.SST.ersst2 <- aggregate(mo.max.SST.ersst2, by=list(0:(length(mo.max.SST.ersst2)-1) %/% 12), max)
mo.sd.SST.ersst2 <- apply(SST.dummy$sst, 3, sd, na.rm = TRUE)
yr.sd.SST.ersst2 <- aggregate(mo.max.SST.ersst2, by=list(0:(length(mo.max.SST.ersst2)-1) %/% 12), sd)
mo.cv.SST.ersst2 <- mo.sd.SST.ersst2 / mo.mean.SST.ersst2
yr.cv.SST.ersst2 <- aggregate(mo.max.SST.ersst2, by=list(0:(length(mo.max.SST.ersst2)-1) %/% 12), cv)



# Coastal ocean subregion 3
ypos<-(poly3[,2])    # lat
xpos<-(poly3[,1])   # lon
zpos<-rep(0.,length(xpos)) #depth

SST.dummy <- rerddapXtracto::rxtractogon(dataInfo, xcoord=xpos, ycoord=ypos, tcoord=tpos, zcoord = zpos, parameter='sst', zName = 'depth') # array of [Lon, Lat, depth, Time]

# Monthly & Yearly means
mo.mean.SST.ersst3 <- apply(SST.dummy$sst, 3, mean, na.rm = TRUE)
yr.mean.SST.ersst3 <- aggregate(mo.mean.SST.ersst3, by=list(0:(length(mo.mean.SST.ersst3)-1) %/% 12), mean)
mo.min.SST.ersst3 <- apply(SST.dummy$sst, 3, min, na.rm = TRUE)
yr.min.SST.ersst3 <- aggregate(mo.min.SST.ersst3, by=list(0:(length(mo.min.SST.ersst3)-1) %/% 12), min)
mo.max.SST.ersst3 <- apply(SST.dummy$sst, 3, max, na.rm = TRUE)
yr.max.SST.ersst3 <- aggregate(mo.max.SST.ersst3, by=list(0:(length(mo.max.SST.ersst3)-1) %/% 12), max)
mo.sd.SST.ersst3 <- apply(SST.dummy$sst, 3, sd, na.rm = TRUE)
yr.sd.SST.ersst3 <- aggregate(mo.max.SST.ersst3, by=list(0:(length(mo.max.SST.ersst3)-1) %/% 12), sd)
mo.cv.SST.ersst3 <- mo.sd.SST.ersst3 / mo.mean.SST.ersst3
yr.cv.SST.ersst3 <- aggregate(mo.max.SST.ersst3, by=list(0:(length(mo.max.SST.ersst3)-1) %/% 12), cv)



# Coastal ocean subregion 4
ypos<-(poly4[,2])    # lat
xpos<-(poly4[,1])#+360  # lon
zpos<-rep(0.,length(xpos))

SST.dummy <- rerddapXtracto::rxtractogon(dataInfo, xcoord=xpos, ycoord=ypos, tcoord=tpos, zcoord = zpos, parameter='sst', zName = 'depth') # array of [Lon, Lat, depth, Time]

# Monthly & Yearly means
mo.mean.SST.ersst4 <- apply(SST.dummy$sst, 3, mean, na.rm = TRUE)
yr.mean.SST.ersst4 <- aggregate(mo.mean.SST.ersst4, by=list(0:(length(mo.mean.SST.ersst4)-1) %/% 12), mean)
mo.min.SST.ersst4 <- apply(SST.dummy$sst, 3, min, na.rm = TRUE)
yr.min.SST.ersst4 <- aggregate(mo.min.SST.ersst4, by=list(0:(length(mo.min.SST.ersst4)-1) %/% 12), min)
mo.max.SST.ersst4 <- apply(SST.dummy$sst, 3, max, na.rm = TRUE)
yr.max.SST.ersst4 <- aggregate(mo.max.SST.ersst4, by=list(0:(length(mo.max.SST.ersst4)-1) %/% 12), max)
mo.sd.SST.ersst4 <- apply(SST.dummy$sst, 3, sd, na.rm = TRUE)
yr.sd.SST.ersst4 <- aggregate(mo.max.SST.ersst4, by=list(0:(length(mo.max.SST.ersst4)-1) %/% 12), sd)
mo.cv.SST.ersst4 <- mo.sd.SST.ersst4 / mo.mean.SST.ersst4
yr.cv.SST.ersst4 <- aggregate(mo.max.SST.ersst4, by=list(0:(length(mo.max.SST.ersst4)-1) %/% 12), cv)



# Coastal ocean subregion 5
ypos<-(poly5[,2])    # lat
xpos<-(poly5[,1])#+360  # lon
zpos<-rep(0.,length(xpos))

SST.dummy <- rerddapXtracto::rxtractogon(dataInfo, xcoord=xpos, ycoord=ypos, tcoord=tpos, zcoord = zpos, parameter='sst', zName = 'depth') # array of [Lon, Lat, depth, Time]

# Monthly & Yearly means
mo.mean.SST.ersst5 <- apply(SST.dummy$sst, 3, mean, na.rm = TRUE)
yr.mean.SST.ersst5 <- aggregate(mo.mean.SST.ersst5, by=list(0:(length(mo.mean.SST.ersst5)-1) %/% 12), mean)
mo.min.SST.ersst5 <- apply(SST.dummy$sst, 3, min, na.rm = TRUE)
yr.min.SST.ersst5 <- aggregate(mo.min.SST.ersst5, by=list(0:(length(mo.min.SST.ersst5)-1) %/% 12), min)
mo.max.SST.ersst5 <- apply(SST.dummy$sst, 3, max, na.rm = TRUE)
yr.max.SST.ersst5 <- aggregate(mo.max.SST.ersst5, by=list(0:(length(mo.max.SST.ersst5)-1) %/% 12), max)
mo.sd.SST.ersst5 <- apply(SST.dummy$sst, 3, sd, na.rm = TRUE)
yr.sd.SST.ersst5 <- aggregate(mo.max.SST.ersst5, by=list(0:(length(mo.max.SST.ersst5)-1) %/% 12), sd)
mo.cv.SST.ersst5 <- mo.sd.SST.ersst5 / mo.mean.SST.ersst5
yr.cv.SST.ersst5 <- aggregate(mo.max.SST.ersst5, by=list(0:(length(mo.max.SST.ersst5)-1) %/% 12), cv)



# Coastal ocean subregion 6
ypos<-(poly6[,2])    # lat
xpos<-(poly6[,1]) # lon
zpos<-rep(0.,length(xpos)) # depth

SST.dummy <- rerddapXtracto::rxtractogon(dataInfo, xcoord=xpos, ycoord=ypos, tcoord=tpos, zcoord = zpos, parameter='sst', zName = 'depth') # array of [Lon, Lat, depth, Time]

# Monthly & Yearly means
mo.mean.SST.ersst6 <- apply(SST.dummy$sst, 3, mean, na.rm = TRUE)
yr.mean.SST.ersst6 <- aggregate(mo.mean.SST.ersst6, by=list(0:(length(mo.mean.SST.ersst6)-1) %/% 12), mean, na.rm=TRUE)
mo.min.SST.ersst6 <- apply(SST.dummy$sst, 3, min, na.rm = TRUE)
mo.min.SST.ersst6[which(mo.min.SST.ersst6==Inf)] <- NA
yr.min.SST.ersst6 <- aggregate(mo.min.SST.ersst6, by=list(0:(length(mo.min.SST.ersst6)-1) %/% 12), min, na.rm=TRUE)
mo.max.SST.ersst6 <- apply(SST.dummy$sst, 3, max, na.rm = TRUE)
mo.max.SST.ersst6[which(mo.max.SST.ersst6==-Inf)] <- NA
yr.max.SST.ersst6 <- aggregate(mo.max.SST.ersst6, by=list(0:(length(mo.max.SST.ersst6)-1) %/% 12), max, na.rm=TRUE)
mo.sd.SST.ersst6 <- apply(SST.dummy$sst, 3, sd, na.rm = TRUE)
yr.sd.SST.ersst6 <- aggregate(mo.max.SST.ersst6, by=list(0:(length(mo.max.SST.ersst6)-1) %/% 12), sd)
mo.cv.SST.ersst6 <- mo.sd.SST.ersst6 / mo.mean.SST.ersst6
yr.cv.SST.ersst6 <- aggregate(mo.max.SST.ersst6, by=list(0:(length(mo.max.SST.ersst6)-1) %/% 12), cv)



# Coastal ocean subregion 7
ypos<-(poly7[,2])    # lat
xpos<-(poly7[,1]) # lon
zpos<-rep(0.,length(xpos)) #depth

SST.dummy <- rerddapXtracto::rxtractogon(dataInfo, xcoord=xpos, ycoord=ypos, tcoord=tpos, zcoord = zpos, parameter='sst', zName = 'depth') # array of [Lon, Lat, depth, Time]

# Monthly & Yearly means
mo.mean.SST.ersst7 <- apply(SST.dummy$sst, 3, mean, na.rm = TRUE)
yr.mean.SST.ersst7 <- aggregate(mo.mean.SST.ersst7, by=list(0:(length(mo.mean.SST.ersst7)-1) %/% 12), mean)
mo.min.SST.ersst7 <- apply(SST.dummy$sst, 3, min, na.rm = TRUE)
yr.min.SST.ersst7 <- aggregate(mo.min.SST.ersst7, by=list(0:(length(mo.min.SST.ersst7)-1) %/% 12), min)
mo.max.SST.ersst7 <- apply(SST.dummy$sst, 3, max, na.rm = TRUE)
yr.max.SST.ersst7 <- aggregate(mo.max.SST.ersst7, by=list(0:(length(mo.max.SST.ersst7)-1) %/% 12), max)
mo.sd.SST.ersst7 <- apply(SST.dummy$sst, 3, sd, na.rm = TRUE)
yr.sd.SST.ersst7 <- aggregate(mo.max.SST.ersst7, by=list(0:(length(mo.max.SST.ersst7)-1) %/% 12), sd)
mo.cv.SST.ersst7 <- mo.sd.SST.ersst7 / mo.mean.SST.ersst7
yr.cv.SST.ersst7 <- aggregate(mo.max.SST.ersst7, by=list(0:(length(mo.max.SST.ersst7)-1) %/% 12), cv)


# Coastal ocean subregion N1

ypos<-(polyN1[,2])    # lat
xpos<-(polyN1[,1]) # lon
zpos<-rep(0.,length(xpos)) #depth

SST.dummy <- rerddapXtracto::rxtractogon(dataInfo, xcoord=xpos, ycoord=ypos, tcoord=tpos, zcoord = zpos, parameter='sst', zName = 'depth') # array of [Lon, Lat, depth, Time]

# Monthly & Yearly means
mo.mean.SST.ersstN1 <- apply(SST.dummy$sst, 3, mean, na.rm = TRUE)
yr.mean.SST.ersstN1 <- aggregate(mo.mean.SST.ersstN1, by=list(0:(length(mo.mean.SST.ersstN1)-1) %/% 12), mean)
mo.min.SST.ersstN1 <- apply(SST.dummy$sst, 3, min, na.rm = TRUE)
yr.min.SST.ersstN1 <- aggregate(mo.min.SST.ersstN1, by=list(0:(length(mo.min.SST.ersstN1)-1) %/% 12), min)
mo.max.SST.ersstN1 <- apply(SST.dummy$sst, 3, max, na.rm = TRUE)
yr.max.SST.ersstN1 <- aggregate(mo.max.SST.ersstN1, by=list(0:(length(mo.max.SST.ersstN1)-1) %/% 12), max)
mo.sd.SST.ersstN1 <- apply(SST.dummy$sst, 3, sd, na.rm = TRUE)
yr.sd.SST.ersstN1 <- aggregate(mo.max.SST.ersstN1, by=list(0:(length(mo.max.SST.ersstN1)-1) %/% 12), sd)
mo.cv.SST.ersstN1 <- mo.sd.SST.ersstN1 / mo.mean.SST.ersstN1
yr.cv.SST.ersstN1 <- aggregate(mo.max.SST.ersstN1, by=list(0:(length(mo.max.SST.ersstN1)-1) %/% 12), cv)


# Coastal ocean subregion N2

ypos<-(polyN2[,2])    # lat
xpos<-(polyN2[,1]).   # lon
zpos<-rep(0.,length(xpos)) #depth


SST.dummy <- rerddapXtracto::rxtractogon(dataInfo, xcoord=xpos, ycoord=ypos, tcoord=tpos, zcoord = zpos, parameter='sst', zName = 'depth') # array of [Lon, Lat, depth, Time]

# Monthly & Yearly means
mo.mean.SST.ersstN2 <- apply(SST.dummy$sst, 3, mean, na.rm = TRUE)
yr.mean.SST.ersstN2 <- aggregate(mo.mean.SST.ersstN2, by=list(0:(length(mo.mean.SST.ersstN2)-1) %/% 12), mean)
mo.min.SST.ersstN2 <- apply(SST.dummy$sst, 3, min, na.rm = TRUE)
yr.min.SST.ersstN2 <- aggregate(mo.min.SST.ersstN2, by=list(0:(length(mo.min.SST.ersstN2)-1) %/% 12), min)
mo.max.SST.ersstN2 <- apply(SST.dummy$sst, 3, max, na.rm = TRUE)
yr.max.SST.ersstN2 <- aggregate(mo.max.SST.ersstN2, by=list(0:(length(mo.max.SST.ersstN2)-1) %/% 12), max)
mo.sd.SST.ersstN2 <- apply(SST.dummy$sst, 3, sd, na.rm = TRUE)
yr.sd.SST.ersstN2 <- aggregate(mo.max.SST.ersstN2, by=list(0:(length(mo.max.SST.ersstN2)-1) %/% 12), sd)
mo.cv.SST.ersstN2 <- mo.sd.SST.ersstN2 / mo.mean.SST.ersstN2
yr.cv.SST.ersstN2 <- aggregate(mo.max.SST.ersstN2, by=list(0:(length(mo.max.SST.ersstN2)-1) %/% 12), cv)



# Coastal ocean subregion N3

ypos<-(polyN3[,2])    # lat
xpos<-(polyN3[,1])  # lon
zpos<-rep(0.,length(xpos)) #depth

SST.dummy <- rerddapXtracto::rxtractogon(dataInfo, xcoord=xpos, ycoord=ypos, tcoord=tpos, zcoord = zpos, parameter='sst', zName = 'depth') # array of [Lon, Lat, depth, Time]

# Monthly & Yearly means
mo.mean.SST.ersstN3 <- apply(SST.dummy$sst, 3, mean, na.rm = TRUE)
yr.mean.SST.ersstN3 <- aggregate(mo.mean.SST.ersstN3, by=list(0:(length(mo.mean.SST.ersstN3)-1) %/% 12), mean)
mo.min.SST.ersstN3 <- apply(SST.dummy$sst, 3, min, na.rm = TRUE)
yr.min.SST.ersstN3 <- aggregate(mo.min.SST.ersstN3, by=list(0:(length(mo.min.SST.ersstN3)-1) %/% 12), min)
mo.max.SST.ersstN3 <- apply(SST.dummy$sst, 3, max, na.rm = TRUE)
yr.max.SST.ersstN3 <- aggregate(mo.max.SST.ersstN3, by=list(0:(length(mo.max.SST.ersstN3)-1) %/% 12), max)
mo.sd.SST.ersstN3 <- apply(SST.dummy$sst, 3, sd, na.rm = TRUE)
yr.sd.SST.ersstN3 <- aggregate(mo.max.SST.ersstN3, by=list(0:(length(mo.max.SST.ersstN3)-1) %/% 12), sd)
mo.cv.SST.ersstN3 <- mo.sd.SST.ersstN3 / mo.mean.SST.ersstN3
yr.cv.SST.ersstN3 <- aggregate(mo.max.SST.ersstN3, by=list(0:(length(mo.max.SST.ersstN3)-1) %/% 12), cv)







# Monthly SST -------------------------------------------------------------


# SST Satellite data
mo.mean.SST.ersst <- vector("list", 10)



mo.mean.SST.ersst[[1]] <- cbind(mo.mean.SST.ersst1, mo.min.SST.ersst1, mo.max.SST.ersst1, mo.sd.SST.ersst1, mo.cv.SST.ersst1)
mo.mean.SST.ersst[[1]] <- cbind(rep(1854:2023, each = 12, length.out = nrow(mo.mean.SST.ersst[[1]])), rep(1:12, times = length(1854:2023), length.out = nrow(mo.mean.SST.ersst[[1]])), mo.mean.SST.ersst[[1]])
names(yr.mean.SST.ersst[[1]]) <- c("Year","month", "SST.mean", "SST.min", "SST.max", "SST.sd", "SST.cv")
mo.mean.SST.ersst[[2]] <- cbind(mo.mean.SST.ersst2, mo.min.SST.ersst2, mo.max.SST.ersst2, mo.sd.SST.ersst2, mo.cv.SST.ersst2)
mo.mean.SST.ersst[[2]] <- cbind(rep(1854:2023, each = 12, length.out = nrow(mo.mean.SST.ersst[[2]])), rep(1:12, times = length(1854:2023), length.out = nrow(mo.mean.SST.ersst[[2]])), mo.mean.SST.ersst[[2]])
names(yr.mean.SST.ersst[[2]]) <- c("Year","month", "SST.mean", "SST.min", "SST.max", "SST.sd", "SST.cv")
mo.mean.SST.ersst[[3]] <- cbind(mo.mean.SST.ersst3, mo.min.SST.ersst3, mo.max.SST.ersst3, mo.sd.SST.ersst3, mo.cv.SST.ersst3)
mo.mean.SST.ersst[[3]] <- cbind(rep(1854:2023, each = 12, length.out = nrow(mo.mean.SST.ersst[[3]])), rep(1:12, times = length(1854:2023), length.out = nrow(mo.mean.SST.ersst[[3]])), mo.mean.SST.ersst[[3]])
names(yr.mean.SST.ersst[[3]]) <- c("Year","month", "SST.mean", "SST.min", "SST.max", "SST.sd", "SST.cv")
mo.mean.SST.ersst[[4]] <- cbind(mo.mean.SST.ersst4, mo.min.SST.ersst4, mo.max.SST.ersst4, mo.sd.SST.ersst4, mo.cv.SST.ersst4)
mo.mean.SST.ersst[[4]] <- cbind(rep(1854:2023, each = 12, length.out = nrow(mo.mean.SST.ersst[[4]])), rep(1:12, times = length(1854:2023), length.out = nrow(mo.mean.SST.ersst[[4]])), mo.mean.SST.ersst[[4]])
names(yr.mean.SST.ersst[[4]]) <- c("Year","month", "SST.mean", "SST.min", "SST.max", "SST.sd", "SST.cv")
mo.mean.SST.ersst[[5]] <- cbind(mo.mean.SST.ersst5, mo.min.SST.ersst5, mo.max.SST.ersst5, mo.sd.SST.ersst5, mo.cv.SST.ersst5)
mo.mean.SST.ersst[[5]] <- cbind(rep(1854:2023, each = 12, length.out = nrow(mo.mean.SST.ersst[[5]])), rep(1:12, times = length(1854:2023), length.out = nrow(mo.mean.SST.ersst[[5]])), mo.mean.SST.ersst[[5]])
names(yr.mean.SST.ersst[[5]]) <- c("Year","month", "SST.mean", "SST.min", "SST.max", "SST.sd", "SST.cv")
mo.mean.SST.ersst[[6]] <- cbind(mo.mean.SST.ersst6, mo.min.SST.ersst6, mo.max.SST.ersst6, mo.sd.SST.ersst6, mo.cv.SST.ersst6)
mo.mean.SST.ersst[[6]] <- cbind(rep(1854:2023, each = 12, length.out = nrow(mo.mean.SST.ersst[[6]])), rep(1:12, times = length(1854:2023), length.out = nrow(mo.mean.SST.ersst[[6]])), mo.mean.SST.ersst[[6]])
names(yr.mean.SST.ersst[[6]]) <- c("Year","month", "SST.mean", "SST.min", "SST.max", "SST.sd", "SST.cv")
mo.mean.SST.ersst[[7]] <- cbind(mo.mean.SST.ersst7, mo.min.SST.ersst7, mo.max.SST.ersst7, mo.sd.SST.ersst7, mo.cv.SST.ersst7)
mo.mean.SST.ersst[[7]] <- cbind(rep(1854:2023, each = 12, length.out = nrow(mo.mean.SST.ersst[[7]])), rep(1:12, times = length(1854:2023), length.out = nrow(mo.mean.SST.ersst[[7]])), mo.mean.SST.ersst[[7]])
names(yr.mean.SST.ersst[[7]]) <- c("Year","month", "SST.mean", "SST.min", "SST.max", "SST.sd", "SST.cv")
mo.mean.SST.ersst[[8]] <- cbind(mo.mean.SST.ersstN1, mo.min.SST.ersstN1, mo.max.SST.ersstN1, mo.sd.SST.ersstN1, mo.cv.SST.ersstN1)
mo.mean.SST.ersst[[8]] <- cbind(rep(1854:2023, each = 12, length.out = nrow(mo.mean.SST.ersst[[8]])), rep(1:12, times = length(1854:2023), length.out = nrow(mo.mean.SST.ersst[[8]])), mo.mean.SST.ersst[[8]])
names(yr.mean.SST.ersst[[8]]) <- c("Year","month", "SST.mean", "SST.min", "SST.max", "SST.sd", "SST.cv")
mo.mean.SST.ersst[[9]] <- cbind(mo.mean.SST.ersstN2, mo.min.SST.ersstN2, mo.max.SST.ersstN2, mo.sd.SST.ersstN2, mo.cv.SST.ersstN2)
mo.mean.SST.ersst[[9]] <- cbind(rep(1854:2023, each = 12, length.out = nrow(mo.mean.SST.ersst[[9]])), rep(1:12, times = length(1854:2023), length.out = nrow(mo.mean.SST.ersst[[9]])), mo.mean.SST.ersst[[9]])
names(yr.mean.SST.ersst[[9]]) <- c("Year","month", "SST.mean", "SST.min", "SST.max", "SST.sd", "SST.cv")
mo.mean.SST.ersst[[10]] <- cbind(mo.mean.SST.ersstN3, mo.min.SST.ersstN3, mo.max.SST.ersstN3, mo.sd.SST.ersstN3, mo.cv.SST.ersstN3)
mo.mean.SST.ersst[[10]] <- cbind(rep(1854:2023, each = 12, length.out = nrow(mo.mean.SST.ersst[[10]])), rep(1:12, times = length(1854:2023), length.out = nrow(mo.mean.SST.ersst[[10]])), mo.mean.SST.ersst[[10]])
names(yr.mean.SST.ersst[[10]]) <- c("Year","month", "SST.mean", "SST.min", "SST.max", "SST.sd", "SST.cv")



# Yearly SST --------------------------------------------------------------


# SST Satellite data
yr.mean.SST.ersst <- vector("list", 10)

yr.mean.SST.ersst[[1]] <- cbind(yr.mean.SST.ersst1, yr.min.SST.ersst1, yr.max.SST.ersst1)
yr.mean.SST.ersst[[1]] <- cbind(1854:2023, yr.mean.SST.ersst[[1]][,c(2,4,6)])
names(yr.mean.SST.ersst[[1]]) <- c("Year", "SST.mean", "SST.min", "SST.max")

yr.mean.SST.ersst[[2]] <- cbind(yr.mean.SST.ersst2, yr.min.SST.ersst2, yr.max.SST.ersst2)
yr.mean.SST.ersst[[2]] <- cbind(1854:2023, yr.mean.SST.ersst[[2]][,c(2,4,6)])
names(yr.mean.SST.ersst[[2]]) <- c("Year", "SST.mean", "SST.min", "SST.max")

yr.mean.SST.ersst[[3]] <- cbind(yr.mean.SST.ersst3, yr.min.SST.ersst3, yr.max.SST.ersst3)
yr.mean.SST.ersst[[3]] <- cbind(1854:2023, yr.mean.SST.ersst[[3]][,c(2,4,6)])
names(yr.mean.SST.ersst[[3]]) <- c("Year", "SST.mean", "SST.min", "SST.max")

yr.mean.SST.ersst[[4]] <- cbind(yr.mean.SST.ersst4, yr.min.SST.ersst4, yr.max.SST.ersst4)
yr.mean.SST.ersst[[4]] <- cbind(1854:2023, yr.mean.SST.ersst[[4]][,c(2,4,6)])
names(yr.mean.SST.ersst[[4]]) <- c("Year", "SST.mean", "SST.min", "SST.max")

yr.mean.SST.ersst[[5]] <- cbind(yr.mean.SST.ersst5, yr.min.SST.ersst5, yr.max.SST.ersst5)
yr.mean.SST.ersst[[5]] <- cbind(1854:2023, yr.mean.SST.ersst[[5]][,c(2,4,6)])
names(yr.mean.SST.ersst[[5]]) <- c("Year", "SST.mean", "SST.min", "SST.max")

yr.mean.SST.ersst[[6]] <- cbind(yr.mean.SST.ersst6, yr.min.SST.ersst6, yr.max.SST.ersst6)
yr.mean.SST.ersst[[6]] <- cbind(1854:2023, yr.mean.SST.ersst[[6]][,c(2,4,6)])
names(yr.mean.SST.ersst[[6]]) <- c("Year", "SST.mean", "SST.min", "SST.max")

yr.mean.SST.ersst[[7]] <- cbind(yr.mean.SST.ersst7, yr.min.SST.ersst7, yr.max.SST.ersst7)
yr.mean.SST.ersst[[7]] <- cbind(1854:2023, yr.mean.SST.ersst[[7]][,c(2,4,6)])
names(yr.mean.SST.ersst[[7]]) <- c("Year", "SST.mean", "SST.min", "SST.max")

yr.mean.SST.ersst[[8]] <- cbind(yr.mean.SST.ersstN1, yr.min.SST.ersstN1, yr.max.SST.ersstN1)
yr.mean.SST.ersst[[8]] <- cbind(1854:2023, yr.mean.SST.ersst[[8]][,c(2,4,6)])
names(yr.mean.SST.ersst[[8]]) <- c("Year", "SST.mean", "SST.min", "SST.max")

yr.mean.SST.ersst[[9]] <- cbind(yr.mean.SST.ersstN2, yr.min.SST.ersstN2, yr.max.SST.ersstN2)
yr.mean.SST.ersst[[9]] <- cbind(1854:2023, yr.mean.SST.ersst[[9]][,c(2,4,6)])
names(yr.mean.SST.ersst[[9]]) <- c("Year", "SST.mean", "SST.min", "SST.max")

yr.mean.SST.ersst[[10]] <- cbind(yr.mean.SST.ersstN3, yr.min.SST.ersstN3, yr.max.SST.ersstN3)
yr.mean.SST.ersst[[10]] <- cbind(1854:2023, yr.mean.SST.ersst[[10]][,c(2,4,6)])
names(yr.mean.SST.ersst[[10]]) <- c("Year", "SST.mean", "SST.min", "SST.max")


