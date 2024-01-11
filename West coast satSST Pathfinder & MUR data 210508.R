wloc <- "jennifergosselin/Dropbox"

setwd(paste0("/Users/", wloc, "/NRC postdoc/Data/Satellite data"))

# https://github.com/rmendels/rerddapXtracto
# https://coastwatch.pfeg.noaa.gov/xtracto/
# https://cran.r-project.org/web/packages/rerddapXtracto/index.html


library(rerddapXtracto)

library(wesanderson)
names(wes_palettes)

dum.col <- (wes_palette("Zissou1", 11, type = "continuous"))

# Coefficient of variation function
cv <- function(x) {sd(na.omit(x))/mean(na.omit(x))}



# Coastal ocean subregion 0: Transition Zone
poly0.path <- paste0("/Users/", wloc, "/NRC postdoc/Data/Maps/Coastal Ocean Polygon/poly0_coord_020215.csv")
poly0 <- read.csv(paste0("/Users/", wloc, "/NRC postdoc/Data/Maps/Coastal Ocean Polygon/poly0_coord_020215.csv"))

# Coastal ocean subregion 1: Puget Sound 
poly1.path <- paste0("/Users/", wloc, "/NRC postdoc/Data/Maps/Coastal Ocean Polygon/poly1_coord_020215.csv")
poly1 <- read.csv(paste0("/Users/", wloc, "/NRC postdoc/Data/Maps/Coastal Ocean Polygon/poly1_coord_020215.csv"))

# Coastal ocean subregion 2: WA Coast & Columbia R
poly2.path <- paste0("/Users/", wloc, "/NRC postdoc/Data/Maps/Coastal Ocean Polygon/poly2_coord_020215.csv")
poly2 <- read.csv(paste0("/Users/", wloc, "/NRC postdoc/Data/Maps/Coastal Ocean Polygon/poly2_coord_020215.csv"))

# Coastal ocean subregion 3: N and Central OR Coast
poly3.path <- paste0("/Users/", wloc, "/NRC postdoc/Data/Maps/Coastal Ocean Polygon/poly3_coord_020215.csv")
poly3 <- read.csv(paste0("/Users/", wloc, "/NRC postdoc/Data/Maps/Coastal Ocean Polygon/poly3_coord_020215.csv"))

# Coastal ocean subregion 4: S OR and N CA Coast
poly4.path <- paste0("/Users/", wloc, "/NRC postdoc/Data/Maps/Coastal Ocean Polygon/poly4_coord_020215.csv")
poly4 <- read.csv(paste0("/Users/", wloc, "/NRC postdoc/Data/Maps/Coastal Ocean Polygon/poly4_coord_020215.csv"))

# Coastal ocean subregion 5: Mendocino Coast and San Fran Coast
poly5.path <- paste0("/Users/", wloc, "/NRC postdoc/Data/Maps/Coastal Ocean Polygon/poly5_coord_020215.csv")
poly5 <- read.csv(paste0("/Users/", wloc, "/NRC postdoc/Data/Maps/Coastal Ocean Polygon/poly5_coord_020215.csv"))

# Coastal ocean subregion 6: Monterey Bay
poly6.path <- paste0("/Users/", wloc, "/NRC postdoc/Data/Maps/Coastal Ocean Polygon/poly6_coord_020215.csv")
poly6 <- read.csv(paste0("/Users/", wloc, "/NRC postdoc/Data/Maps/Coastal Ocean Polygon/poly6_coord_020215.csv"))

# Coastal ocean subregion 7: S CA Bight
poly7.path <- paste0("/Users/", wloc, "/NRC postdoc/Data/Maps/Coastal Ocean Polygon/poly7_coord_020215.csv")
poly7 <- read.csv(paste0("/Users/", wloc, "/NRC postdoc/Data/Maps/Coastal Ocean Polygon/poly7_coord_020215.csv"))

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



# ------------------------------------------------------------------------------------------------------------------------- #

# MUR satellite data

# ----------- SST ----------
tpos<-c('2003-01-01','2020-12-31')  # Multi-scale Ultra-high Resolution (MUR) SST Analysis fv04.1, Global, 0.01°, 2002-present, Monthly

# Coastal ocean subregion 0
ypos<-(poly0[,2])    # lat
xpos<-(poly0[,1])#+360  # lon

dataInfo <- rerddap::info('jplMURSST41mday')
SST.dummy <- rxtractogon(dataInfo, xcoord=xpos, ycoord=ypos, tcoord=tpos, parameter='sst') # array of [Lon, Lat, Time]
# Monthly & Yearly means
mo.mean.SST.jplMUR0 <- apply(SST.dummy$sst, 3, mean, na.rm = TRUE)
yr.mean.SST.jplMUR0 <- aggregate(mo.mean.SST.jplMUR0, by=list(0:(length(mo.mean.SST.jplMUR0)-1) %/% 12), mean)
mo.min.SST.jplMUR0 <- apply(SST.dummy$sst, 3, min, na.rm = TRUE)
yr.min.SST.jplMUR0 <- aggregate(mo.min.SST.jplMUR0, by=list(0:(length(mo.min.SST.jplMUR0)-1) %/% 12), min)
mo.max.SST.jplMUR0 <- apply(SST.dummy$sst, 3, max, na.rm = TRUE)
yr.max.SST.jplMUR0 <- aggregate(mo.max.SST.jplMUR0, by=list(0:(length(mo.max.SST.jplMUR0)-1) %/% 12), max)
mo.sd.SST.jplMUR0 <- apply(SST.dummy$sst, 3, sd, na.rm = TRUE)
yr.sd.SST.jplMUR0 <- aggregate(mo.max.SST.jplMUR0, by=list(0:(length(mo.max.SST.jplMUR0)-1) %/% 12), sd)
mo.cv.SST.jplMUR0 <- mo.sd.SST.jplMUR0 / mo.mean.SST.jplMUR0 
yr.cv.SST.jplMUR0 <- aggregate(mo.max.SST.jplMUR0, by=list(0:(length(mo.max.SST.jplMUR0)-1) %/% 12), cv)



# Coastal ocean subregion 1
ypos<-(poly1[,2])    # lat
xpos<-(poly1[,1])#+360  # lon

dataInfo <- rerddap::info('jplMURSST41mday')
SST.dummy <- rxtractogon(dataInfo, xcoord=xpos, ycoord=ypos, tcoord=tpos, parameter='sst') # array of [Lon, Lat, Time]
# Monthly & Yearly means
mo.mean.SST.jplMUR1 <- apply(SST.dummy$sst, 3, mean, na.rm = TRUE)
yr.mean.SST.jplMUR1 <- aggregate(mo.mean.SST.jplMUR1, by=list(0:(length(mo.mean.SST.jplMUR1)-1) %/% 12), mean)
mo.min.SST.jplMUR1 <- apply(SST.dummy$sst, 3, min, na.rm = TRUE)
yr.min.SST.jplMUR1 <- aggregate(mo.min.SST.jplMUR1, by=list(0:(length(mo.min.SST.jplMUR1)-1) %/% 12), min)
mo.max.SST.jplMUR1 <- apply(SST.dummy$sst, 3, max, na.rm = TRUE)
yr.max.SST.jplMUR1 <- aggregate(mo.max.SST.jplMUR1, by=list(0:(length(mo.max.SST.jplMUR1)-1) %/% 12), max)
mo.sd.SST.jplMUR1 <- apply(SST.dummy$sst, 3, sd, na.rm = TRUE)
yr.sd.SST.jplMUR1 <- aggregate(mo.max.SST.jplMUR1, by=list(0:(length(mo.max.SST.jplMUR1)-1) %/% 12), sd)
mo.cv.SST.jplMUR1 <- mo.sd.SST.jplMUR1 / mo.mean.SST.jplMUR1 
yr.cv.SST.jplMUR1 <- aggregate(mo.max.SST.jplMUR1, by=list(0:(length(mo.max.SST.jplMUR1)-1) %/% 12), cv)



# Coastal ocean subregion 2
ypos<-(poly2[,2])    # lat
xpos<-(poly2[,1])#+360  # lon

dataInfo <- rerddap::info('jplMURSST41mday')
SST.dummy <- rxtractogon(dataInfo, xcoord=xpos, ycoord=ypos, tcoord=tpos, parameter='sst') # array of [Lon, Lat, Time]
# Monthly & Yearly means
mo.mean.SST.jplMUR2 <- apply(SST.dummy$sst, 3, mean, na.rm = TRUE)
yr.mean.SST.jplMUR2 <- aggregate(mo.mean.SST.jplMUR2, by=list(0:(length(mo.mean.SST.jplMUR2)-1) %/% 12), mean)
mo.min.SST.jplMUR2 <- apply(SST.dummy$sst, 3, min, na.rm = TRUE)
yr.min.SST.jplMUR2 <- aggregate(mo.min.SST.jplMUR2, by=list(0:(length(mo.min.SST.jplMUR2)-1) %/% 12), min)
mo.max.SST.jplMUR2 <- apply(SST.dummy$sst, 3, max, na.rm = TRUE)
yr.max.SST.jplMUR2 <- aggregate(mo.max.SST.jplMUR2, by=list(0:(length(mo.max.SST.jplMUR2)-1) %/% 12), max)
mo.sd.SST.jplMUR2 <- apply(SST.dummy$sst, 3, sd, na.rm = TRUE)
yr.sd.SST.jplMUR2 <- aggregate(mo.max.SST.jplMUR2, by=list(0:(length(mo.max.SST.jplMUR2)-1) %/% 12), sd)
mo.cv.SST.jplMUR2 <- mo.sd.SST.jplMUR2 / mo.mean.SST.jplMUR2 
yr.cv.SST.jplMUR2 <- aggregate(mo.max.SST.jplMUR2, by=list(0:(length(mo.max.SST.jplMUR2)-1) %/% 12), cv)



# Coastal ocean subregion 3
ypos<-(poly3[,2])    # lat
xpos<-(poly3[,1])#+360  # lon

dataInfo <- rerddap::info('jplMURSST41mday')
SST.dummy <- rxtractogon(dataInfo, xcoord=xpos, ycoord=ypos, tcoord=tpos, parameter='sst') # array of [Lon, Lat, Time]
# Monthly & Yearly means
mo.mean.SST.jplMUR3 <- apply(SST.dummy$sst, 3, mean, na.rm = TRUE)
yr.mean.SST.jplMUR3 <- aggregate(mo.mean.SST.jplMUR3, by=list(0:(length(mo.mean.SST.jplMUR3)-1) %/% 12), mean)
mo.min.SST.jplMUR3 <- apply(SST.dummy$sst, 3, min, na.rm = TRUE)
yr.min.SST.jplMUR3 <- aggregate(mo.min.SST.jplMUR3, by=list(0:(length(mo.min.SST.jplMUR3)-1) %/% 12), min)
mo.max.SST.jplMUR3 <- apply(SST.dummy$sst, 3, max, na.rm = TRUE)
yr.max.SST.jplMUR3 <- aggregate(mo.max.SST.jplMUR3, by=list(0:(length(mo.max.SST.jplMUR3)-1) %/% 12), max)
mo.sd.SST.jplMUR3 <- apply(SST.dummy$sst, 3, sd, na.rm = TRUE)
yr.sd.SST.jplMUR3 <- aggregate(mo.max.SST.jplMUR3, by=list(0:(length(mo.max.SST.jplMUR3)-1) %/% 12), sd)
mo.cv.SST.jplMUR3 <- mo.sd.SST.jplMUR3 / mo.mean.SST.jplMUR3 
yr.cv.SST.jplMUR3 <- aggregate(mo.max.SST.jplMUR3, by=list(0:(length(mo.max.SST.jplMUR3)-1) %/% 12), cv)



# Coastal ocean subregion 4
ypos<-(poly4[,2])    # lat
xpos<-(poly4[,1])#+360  # lon

dataInfo <- rerddap::info('jplMURSST41mday')
SST.dummy <- rxtractogon(dataInfo, xcoord=xpos, ycoord=ypos, tcoord=tpos, parameter='sst') # array of [Lon, Lat, Time]
# Monthly & Yearly means
mo.mean.SST.jplMUR4 <- apply(SST.dummy$sst, 3, mean, na.rm = TRUE)
yr.mean.SST.jplMUR4 <- aggregate(mo.mean.SST.jplMUR4, by=list(0:(length(mo.mean.SST.jplMUR4)-1) %/% 12), mean)
mo.min.SST.jplMUR4 <- apply(SST.dummy$sst, 3, min, na.rm = TRUE)
yr.min.SST.jplMUR4 <- aggregate(mo.min.SST.jplMUR4, by=list(0:(length(mo.min.SST.jplMUR4)-1) %/% 12), min)
mo.max.SST.jplMUR4 <- apply(SST.dummy$sst, 3, max, na.rm = TRUE)
yr.max.SST.jplMUR4 <- aggregate(mo.max.SST.jplMUR4, by=list(0:(length(mo.max.SST.jplMUR4)-1) %/% 12), max)
mo.sd.SST.jplMUR4 <- apply(SST.dummy$sst, 3, sd, na.rm = TRUE)
yr.sd.SST.jplMUR4 <- aggregate(mo.max.SST.jplMUR4, by=list(0:(length(mo.max.SST.jplMUR4)-1) %/% 12), sd)
mo.cv.SST.jplMUR4 <- mo.sd.SST.jplMUR4 / mo.mean.SST.jplMUR4
yr.cv.SST.jplMUR4 <- aggregate(mo.max.SST.jplMUR4, by=list(0:(length(mo.max.SST.jplMUR4)-1) %/% 12), cv)



# Coastal ocean subregion 5
ypos<-(poly5[,2])    # lat
xpos<-(poly5[,1])#+360  # lon

dataInfo <- rerddap::info('jplMURSST41mday')
SST.dummy <- rxtractogon(dataInfo, xcoord=xpos, ycoord=ypos, tcoord=tpos, parameter='sst') # array of [Lon, Lat, Time]
# Monthly & Yearly means
mo.mean.SST.jplMUR5 <- apply(SST.dummy$sst, 3, mean, na.rm = TRUE)
yr.mean.SST.jplMUR5 <- aggregate(mo.mean.SST.jplMUR5, by=list(0:(length(mo.mean.SST.jplMUR5)-1) %/% 12), mean)
mo.min.SST.jplMUR5 <- apply(SST.dummy$sst, 3, min, na.rm = TRUE)
yr.min.SST.jplMUR5 <- aggregate(mo.min.SST.jplMUR5, by=list(0:(length(mo.min.SST.jplMUR5)-1) %/% 12), min)
mo.max.SST.jplMUR5 <- apply(SST.dummy$sst, 3, max, na.rm = TRUE)
yr.max.SST.jplMUR5 <- aggregate(mo.max.SST.jplMUR5, by=list(0:(length(mo.max.SST.jplMUR5)-1) %/% 12), max)
mo.sd.SST.jplMUR5 <- apply(SST.dummy$sst, 3, sd, na.rm = TRUE)
yr.sd.SST.jplMUR5 <- aggregate(mo.max.SST.jplMUR5, by=list(0:(length(mo.max.SST.jplMUR5)-1) %/% 12), sd)
mo.cv.SST.jplMUR5 <- mo.sd.SST.jplMUR5 / mo.mean.SST.jplMUR5
yr.cv.SST.jplMUR5 <- aggregate(mo.max.SST.jplMUR5, by=list(0:(length(mo.max.SST.jplMUR5)-1) %/% 12), cv)



# Coastal ocean subregion 6
ypos<-(poly6[,2])    # lat
xpos<-(poly6[,1])#+360  # lon

dataInfo <- rerddap::info('jplMURSST41mday')
SST.dummy <- rxtractogon(dataInfo, xcoord=xpos, ycoord=ypos, tcoord=tpos, parameter='sst') # array of [Lon, Lat, Time]
# Monthly & Yearly means
mo.mean.SST.jplMUR6 <- apply(SST.dummy$sst, 3, mean, na.rm = TRUE)
yr.mean.SST.jplMUR6 <- aggregate(mo.mean.SST.jplMUR6, by=list(0:(length(mo.mean.SST.jplMUR6)-1) %/% 12), mean, na.rm=TRUE)
mo.min.SST.jplMUR6 <- apply(SST.dummy$sst, 3, min, na.rm = TRUE)
mo.min.SST.jplMUR6[which(mo.min.SST.jplMUR6==Inf)] <- NA
yr.min.SST.jplMUR6 <- aggregate(mo.min.SST.jplMUR6, by=list(0:(length(mo.min.SST.jplMUR6)-1) %/% 12), min, na.rm=TRUE)
mo.max.SST.jplMUR6 <- apply(SST.dummy$sst, 3, max, na.rm = TRUE)
mo.max.SST.jplMUR6[which(mo.max.SST.jplMUR6==-Inf)] <- NA
yr.max.SST.jplMUR6 <- aggregate(mo.max.SST.jplMUR6, by=list(0:(length(mo.max.SST.jplMUR6)-1) %/% 12), max, na.rm=TRUE)
mo.sd.SST.jplMUR6 <- apply(SST.dummy$sst, 3, sd, na.rm = TRUE)
yr.sd.SST.jplMUR6 <- aggregate(mo.max.SST.jplMUR6, by=list(0:(length(mo.max.SST.jplMUR6)-1) %/% 12), sd)
mo.cv.SST.jplMUR6 <- mo.sd.SST.jplMUR6 / mo.mean.SST.jplMUR6
yr.cv.SST.jplMUR6 <- aggregate(mo.max.SST.jplMUR6, by=list(0:(length(mo.max.SST.jplMUR6)-1) %/% 12), cv)



# Coastal ocean subregion 7
ypos<-(poly7[,2])    # lat
xpos<-(poly7[,1])#+360  # lon

dataInfo <- rerddap::info('jplMURSST41mday')
SST.dummy <- rxtractogon(dataInfo, xcoord=xpos, ycoord=ypos, tcoord=tpos, parameter='sst') # array of [Lon, Lat, Time]
# Monthly & Yearly means
mo.mean.SST.jplMUR7 <- apply(SST.dummy$sst, 3, mean, na.rm = TRUE)
yr.mean.SST.jplMUR7 <- aggregate(mo.mean.SST.jplMUR7, by=list(0:(length(mo.mean.SST.jplMUR7)-1) %/% 12), mean)
mo.min.SST.jplMUR7 <- apply(SST.dummy$sst, 3, min, na.rm = TRUE)
yr.min.SST.jplMUR7 <- aggregate(mo.min.SST.jplMUR7, by=list(0:(length(mo.min.SST.jplMUR7)-1) %/% 12), min)
mo.max.SST.jplMUR7 <- apply(SST.dummy$sst, 3, max, na.rm = TRUE)
yr.max.SST.jplMUR7 <- aggregate(mo.max.SST.jplMUR7, by=list(0:(length(mo.max.SST.jplMUR7)-1) %/% 12), max)
mo.sd.SST.jplMUR7 <- apply(SST.dummy$sst, 3, sd, na.rm = TRUE)
yr.sd.SST.jplMUR7 <- aggregate(mo.max.SST.jplMUR7, by=list(0:(length(mo.max.SST.jplMUR7)-1) %/% 12), sd)
mo.cv.SST.jplMUR7 <- mo.sd.SST.jplMUR7 / mo.mean.SST.jplMUR7
yr.cv.SST.jplMUR7 <- aggregate(mo.max.SST.jplMUR7, by=list(0:(length(mo.max.SST.jplMUR7)-1) %/% 12), cv)


# Coastal ocean subregion N1

ypos<-(polyN1[,2])    # lat
xpos<-(polyN1[,1])#+360  # lon

dataInfo <- rerddap::info('jplMURSST41mday')
SST.dummy <- rxtractogon(dataInfo, xcoord=xpos, ycoord=ypos, tcoord=tpos, parameter='sst') # array of [Lon, Lat, Time]
# Monthly & Yearly means
mo.mean.SST.jplMURN1 <- apply(SST.dummy$sst, 3, mean, na.rm = TRUE)
yr.mean.SST.jplMURN1 <- aggregate(mo.mean.SST.jplMURN1, by=list(0:(length(mo.mean.SST.jplMURN1)-1) %/% 12), mean)
mo.min.SST.jplMURN1 <- apply(SST.dummy$sst, 3, min, na.rm = TRUE)
yr.min.SST.jplMURN1 <- aggregate(mo.min.SST.jplMURN1, by=list(0:(length(mo.min.SST.jplMURN1)-1) %/% 12), min)
mo.max.SST.jplMURN1 <- apply(SST.dummy$sst, 3, max, na.rm = TRUE)
yr.max.SST.jplMURN1 <- aggregate(mo.max.SST.jplMURN1, by=list(0:(length(mo.max.SST.jplMURN1)-1) %/% 12), max)
mo.sd.SST.jplMURN1 <- apply(SST.dummy$sst, 3, sd, na.rm = TRUE)
yr.sd.SST.jplMURN1 <- aggregate(mo.max.SST.jplMURN1, by=list(0:(length(mo.max.SST.jplMURN1)-1) %/% 12), sd)
mo.cv.SST.jplMURN1 <- mo.sd.SST.jplMURN1 / mo.mean.SST.jplMURN1
yr.cv.SST.jplMURN1 <- aggregate(mo.max.SST.jplMURN1, by=list(0:(length(mo.max.SST.jplMURN1)-1) %/% 12), cv)


# Coastal ocean subregion N2

ypos<-(polyN2[,2])    # lat
xpos<-(polyN2[,1])#+360  # lon

dataInfo <- rerddap::info('jplMURSST41mday')
SST.dummy <- rxtractogon(dataInfo, xcoord=xpos, ycoord=ypos, tcoord=tpos, parameter='sst') # array of [Lon, Lat, Time]
# Monthly & Yearly means
mo.mean.SST.jplMURN2 <- apply(SST.dummy$sst, 3, mean, na.rm = TRUE)
yr.mean.SST.jplMURN2 <- aggregate(mo.mean.SST.jplMURN2, by=list(0:(length(mo.mean.SST.jplMURN2)-1) %/% 12), mean)
mo.min.SST.jplMURN2 <- apply(SST.dummy$sst, 3, min, na.rm = TRUE)
yr.min.SST.jplMURN2 <- aggregate(mo.min.SST.jplMURN2, by=list(0:(length(mo.min.SST.jplMURN2)-1) %/% 12), min)
mo.max.SST.jplMURN2 <- apply(SST.dummy$sst, 3, max, na.rm = TRUE)
yr.max.SST.jplMURN2 <- aggregate(mo.max.SST.jplMURN2, by=list(0:(length(mo.max.SST.jplMURN2)-1) %/% 12), max)
mo.sd.SST.jplMURN2 <- apply(SST.dummy$sst, 3, sd, na.rm = TRUE)
yr.sd.SST.jplMURN2 <- aggregate(mo.max.SST.jplMURN2, by=list(0:(length(mo.max.SST.jplMURN2)-1) %/% 12), sd)
mo.cv.SST.jplMURN2 <- mo.sd.SST.jplMURN2 / mo.mean.SST.jplMURN2
yr.cv.SST.jplMURN2 <- aggregate(mo.max.SST.jplMURN2, by=list(0:(length(mo.max.SST.jplMURN2)-1) %/% 12), cv)



# Coastal ocean subregion N3

ypos<-(polyN3[,2])    # lat
xpos<-(polyN3[,1])#+360  # lon

dataInfo <- rerddap::info('jplMURSST41mday')
SST.dummy <- rxtractogon(dataInfo, xcoord=xpos, ycoord=ypos, tcoord=tpos, parameter='sst') # array of [Lon, Lat, Time]
# Monthly & Yearly means
mo.mean.SST.jplMURN3 <- apply(SST.dummy$sst, 3, mean, na.rm = TRUE)
yr.mean.SST.jplMURN3 <- aggregate(mo.mean.SST.jplMURN3, by=list(0:(length(mo.mean.SST.jplMURN3)-1) %/% 12), mean)
mo.min.SST.jplMURN3 <- apply(SST.dummy$sst, 3, min, na.rm = TRUE)
yr.min.SST.jplMURN3 <- aggregate(mo.min.SST.jplMURN3, by=list(0:(length(mo.min.SST.jplMURN3)-1) %/% 12), min)
mo.max.SST.jplMURN3 <- apply(SST.dummy$sst, 3, max, na.rm = TRUE)
yr.max.SST.jplMURN3 <- aggregate(mo.max.SST.jplMURN3, by=list(0:(length(mo.max.SST.jplMURN3)-1) %/% 12), max)
mo.sd.SST.jplMURN3 <- apply(SST.dummy$sst, 3, sd, na.rm = TRUE)
yr.sd.SST.jplMURN3 <- aggregate(mo.max.SST.jplMURN3, by=list(0:(length(mo.max.SST.jplMURN3)-1) %/% 12), sd)
mo.cv.SST.jplMURN3 <- mo.sd.SST.jplMURN3 / mo.mean.SST.jplMURN3
yr.cv.SST.jplMURN3 <- aggregate(mo.max.SST.jplMURN3, by=list(0:(length(mo.max.SST.jplMURN3)-1) %/% 12), cv)





# SST Satellite data
mo.mean.SST.jplMUR <- vector("list", 10)
yr.mean.SST.jplMUR <- vector("list", 10)

mo.mean.SST.jplMUR[[1]] <- cbind(mo.mean.SST.jplMUR1, mo.min.SST.jplMUR1, mo.max.SST.jplMUR1, mo.sd.SST.jplMUR1, mo.cv.SST.jplMUR1)
mo.mean.SST.jplMUR[[2]] <- cbind(mo.mean.SST.jplMUR2, mo.min.SST.jplMUR2, mo.max.SST.jplMUR2, mo.sd.SST.jplMUR2, mo.cv.SST.jplMUR2)
mo.mean.SST.jplMUR[[3]] <- cbind(mo.mean.SST.jplMUR3, mo.min.SST.jplMUR3, mo.max.SST.jplMUR3, mo.sd.SST.jplMUR3, mo.cv.SST.jplMUR3)
mo.mean.SST.jplMUR[[4]] <- cbind(mo.mean.SST.jplMUR4, mo.min.SST.jplMUR4, mo.max.SST.jplMUR4, mo.sd.SST.jplMUR4, mo.cv.SST.jplMUR4)
mo.mean.SST.jplMUR[[5]] <- cbind(mo.mean.SST.jplMUR5, mo.min.SST.jplMUR5, mo.max.SST.jplMUR5, mo.sd.SST.jplMUR5, mo.cv.SST.jplMUR5)
mo.mean.SST.jplMUR[[6]] <- cbind(mo.mean.SST.jplMUR6, mo.min.SST.jplMUR6, mo.max.SST.jplMUR6, mo.sd.SST.jplMUR6, mo.cv.SST.jplMUR6)
mo.mean.SST.jplMUR[[7]] <- cbind(mo.mean.SST.jplMUR7, mo.min.SST.jplMUR7, mo.max.SST.jplMUR7, mo.sd.SST.jplMUR7, mo.cv.SST.jplMUR7)
mo.mean.SST.jplMUR[[8]] <- cbind(mo.mean.SST.jplMURN1, mo.min.SST.jplMURN1, mo.max.SST.jplMURN1, mo.sd.SST.jplMURN1, mo.cv.SST.jplMURN1)
mo.mean.SST.jplMUR[[9]] <- cbind(mo.mean.SST.jplMURN2, mo.min.SST.jplMURN2, mo.max.SST.jplMURN2, mo.sd.SST.jplMURN2, mo.cv.SST.jplMURN2)
mo.mean.SST.jplMUR[[10]] <- cbind(mo.mean.SST.jplMURN3, mo.min.SST.jplMURN3, mo.max.SST.jplMURN3, mo.sd.SST.jplMURN3, mo.cv.SST.jplMURN3)


yr.mean.SST.jplMUR[[1]] <- cbind(yr.mean.SST.jplMUR1, yr.min.SST.jplMUR1, yr.max.SST.jplMUR1)
yr.mean.SST.jplMUR[[1]] <- cbind(2003:2020, yr.mean.SST.jplMUR[[1]][,c(2,4,6)])
names(yr.mean.SST.jplMUR[[1]]) <- c("Year", "SST.mean", "SST.min", "SST.max")

yr.mean.SST.jplMUR[[2]] <- cbind(yr.mean.SST.jplMUR2, yr.min.SST.jplMUR2, yr.max.SST.jplMUR2)
yr.mean.SST.jplMUR[[2]] <- cbind(2003:2020, yr.mean.SST.jplMUR[[2]][,c(2,4,6)])
names(yr.mean.SST.jplMUR[[2]]) <- c("Year", "SST.mean", "SST.min", "SST.max")

yr.mean.SST.jplMUR[[3]] <- cbind(yr.mean.SST.jplMUR3, yr.min.SST.jplMUR3, yr.max.SST.jplMUR3)
yr.mean.SST.jplMUR[[3]] <- cbind(2003:2020, yr.mean.SST.jplMUR[[3]][,c(2,4,6)])
names(yr.mean.SST.jplMUR[[3]]) <- c("Year", "SST.mean", "SST.min", "SST.max")

yr.mean.SST.jplMUR[[4]] <- cbind(yr.mean.SST.jplMUR4, yr.min.SST.jplMUR4, yr.max.SST.jplMUR4)
yr.mean.SST.jplMUR[[4]] <- cbind(2003:2020, yr.mean.SST.jplMUR[[4]][,c(2,4,6)])
names(yr.mean.SST.jplMUR[[4]]) <- c("Year", "SST.mean", "SST.min", "SST.max")

yr.mean.SST.jplMUR[[5]] <- cbind(yr.mean.SST.jplMUR5, yr.min.SST.jplMUR5, yr.max.SST.jplMUR5)
yr.mean.SST.jplMUR[[5]] <- cbind(2003:2020, yr.mean.SST.jplMUR[[5]][,c(2,4,6)])
names(yr.mean.SST.jplMUR[[5]]) <- c("Year", "SST.mean", "SST.min", "SST.max")

yr.mean.SST.jplMUR[[6]] <- cbind(yr.mean.SST.jplMUR6, yr.min.SST.jplMUR6, yr.max.SST.jplMUR6)
yr.mean.SST.jplMUR[[6]] <- cbind(2003:2020, yr.mean.SST.jplMUR[[6]][,c(2,4,6)])
names(yr.mean.SST.jplMUR[[6]]) <- c("Year", "SST.mean", "SST.min", "SST.max")

yr.mean.SST.jplMUR[[7]] <- cbind(yr.mean.SST.jplMUR7, yr.min.SST.jplMUR7, yr.max.SST.jplMUR7)
yr.mean.SST.jplMUR[[7]] <- cbind(2003:2020, yr.mean.SST.jplMUR[[7]][,c(2,4,6)])
names(yr.mean.SST.jplMUR[[7]]) <- c("Year", "SST.mean", "SST.min", "SST.max")

yr.mean.SST.jplMUR[[8]] <- cbind(yr.mean.SST.jplMURN1, yr.min.SST.jplMURN1, yr.max.SST.jplMURN1)
yr.mean.SST.jplMUR[[8]] <- cbind(2003:2020, yr.mean.SST.jplMUR[[8]][,c(2,4,6)])
names(yr.mean.SST.jplMUR[[8]]) <- c("Year", "SST.mean", "SST.min", "SST.max")

yr.mean.SST.jplMUR[[9]] <- cbind(yr.mean.SST.jplMURN2, yr.min.SST.jplMURN2, yr.max.SST.jplMURN2)
yr.mean.SST.jplMUR[[9]] <- cbind(2003:2020, yr.mean.SST.jplMUR[[9]][,c(2,4,6)])
names(yr.mean.SST.jplMUR[[9]]) <- c("Year", "SST.mean", "SST.min", "SST.max")

yr.mean.SST.jplMUR[[10]] <- cbind(yr.mean.SST.jplMURN3, yr.min.SST.jplMURN3, yr.max.SST.jplMURN3)
yr.mean.SST.jplMUR[[10]] <- cbind(2003:2020, yr.mean.SST.jplMUR[[10]][,c(2,4,6)])
names(yr.mean.SST.jplMUR[[10]]) <- c("Year", "SST.mean", "SST.min", "SST.max")


# -------------------------------------------------------------------------------------------------------------------------- #

# Pathfinder data

searchResult <- searchData('longname:Pathfinder')
# nodcPH2sstd1day

# ----------- SST ----------
# tpos<-c('1982-01-01','2012-12-31')  # SST, Pathfinder Ver 5.2 (L3C), Day, Global, Science Quality, 1981-2012 (1 Day Composite)
# tpos<-c('1982-01-01','1982-12-31')  # SST, Pathfinder Ver 5.2 (L3C), Day, Global, Science Quality, 1981-2012 (1 Day Composite)





# Coastal ocean subregion 1
ypos<-(poly1[,2])    # lat
xpos<-(poly1[,1])#+360   # lon
dataInfo <- rerddap::info('nodcPH2sstd1day')

years <- 1982:2012
da.SST.Pathf1 <- vector("list",length(years))
mo.SST.Pathf1 <- vector("list",length(years))

for (yr in 1:length(years)) {
  tpos <- c( paste0(years[yr],"-01-01"), paste0(years[yr],"-12-31") )
  SST.dummy <- rxtractogon(dataInfo, xcoord=xpos, ycoord=ypos, tcoord=tpos, parameter="sea_surface_temperature") # array of [Lon, Lat, Time]
  da.SST.Pathf1[[yr]] <- data.frame(SST.dummy$time, apply(SST.dummy$sea_surface_temperature, 3, mean, na.rm=TRUE) )
  da.SST.Pathf1[[yr]] <- da.SST.Pathf1[[yr]][complete.cases(da.SST.Pathf1[[yr]]),]
  names(da.SST.Pathf1[[yr]]) <- c("time","sst")
  da.SST.Pathf1[[yr]]$month <- substr(da.SST.Pathf1[[yr]]$time,6,7)
  mo.SST.Pathf1[[yr]] <- aggregate(da.SST.Pathf1[[yr]]$sst, by=list(da.SST.Pathf1[[yr]]$month), FUN=mean)
}
mo.SST.Pathf1[[13]] <- rbind(mo.SST.Pathf1[[13]], c("10",NA), c("11",NA), c("12",NA) )


# Coastal ocean subregion 2
ypos<-(poly2[,2])    # lat
xpos<-(poly2[,1])#+360   # lon
dataInfo <- rerddap::info('nodcPH2sstd1day')

years <- 1982:2012
da.SST.Pathf2 <- vector("list",length(years))
mo.SST.Pathf2 <- vector("list",length(years))

for (yr in 1:length(years)) {
  tpos <- c( paste0(years[yr],"-01-01"), paste0(years[yr],"-12-31") )
  SST.dummy <- rxtractogon(dataInfo, xcoord=xpos, ycoord=ypos, tcoord=tpos, parameter="sea_surface_temperature") # array of [Lon, Lat, Time]
  da.SST.Pathf2[[yr]] <- data.frame(SST.dummy$time, apply(SST.dummy$sea_surface_temperature, 3, mean, na.rm=TRUE) )
  da.SST.Pathf2[[yr]] <- da.SST.Pathf2[[yr]][complete.cases(da.SST.Pathf2[[yr]]),]
  names(da.SST.Pathf2[[yr]]) <- c("time","sst")
  da.SST.Pathf2[[yr]]$month <- substr(da.SST.Pathf2[[yr]]$time,6,7)
  mo.SST.Pathf2[[yr]] <- aggregate(da.SST.Pathf2[[yr]]$sst, by=list(da.SST.Pathf2[[yr]]$month), FUN=mean)
}
mo.SST.Pathf2[[13]] <- rbind(mo.SST.Pathf2[[13]], c("10",NA), c("11",NA), c("12",NA) )



# Coastal ocean subregion 3
ypos<-(poly3[,2])    # lat
xpos<-(poly3[,1])#+360   # lon
dataInfo <- rerddap::info('nodcPH2sstd1day')

years <- 1982:2012
da.SST.Pathf3 <- vector("list",length(years))
mo.SST.Pathf3 <- vector("list",length(years))

for (yr in 1:length(years)) {
  tpos <- c( paste0(years[yr],"-01-01"), paste0(years[yr],"-12-31") )
  SST.dummy <- rxtractogon(dataInfo, xcoord=xpos, ycoord=ypos, tcoord=tpos, parameter="sea_surface_temperature") # array of [Lon, Lat, Time]
  da.SST.Pathf3[[yr]] <- data.frame(SST.dummy$time, apply(SST.dummy$sea_surface_temperature, 3, mean, na.rm=TRUE) )
  da.SST.Pathf3[[yr]] <- da.SST.Pathf3[[yr]][complete.cases(da.SST.Pathf3[[yr]]),]
  names(da.SST.Pathf3[[yr]]) <- c("time","sst")
  da.SST.Pathf3[[yr]]$month <- substr(da.SST.Pathf3[[yr]]$time,6,7)
  mo.SST.Pathf3[[yr]] <- aggregate(da.SST.Pathf3[[yr]]$sst, by=list(da.SST.Pathf3[[yr]]$month), FUN=mean)
}
mo.SST.Pathf3[[13]] <- rbind(mo.SST.Pathf3[[13]], c("10",NA), c("11",NA), c("12",NA) )



# Coastal ocean subregion 4
ypos<-(poly4[,2])    # lat
xpos<-(poly4[,1])#+360   # lon
dataInfo <- rerddap::info('nodcPH2sstd1day')

years <- 1982:2012
da.SST.Pathf4 <- vector("list",length(years))
mo.SST.Pathf4 <- vector("list",length(years))

for (yr in 13:length(years)) {
  tpos <- c( paste0(years[yr],"-01-01"), paste0(years[yr],"-12-31") )
  SST.dummy <- rxtractogon(dataInfo, xcoord=xpos, ycoord=ypos, tcoord=tpos, parameter="sea_surface_temperature") # array of [Lon, Lat, Time]
  da.SST.Pathf4[[yr]] <- data.frame(SST.dummy$time, apply(SST.dummy$sea_surface_temperature, 3, mean, na.rm=TRUE) )
  da.SST.Pathf4[[yr]] <- da.SST.Pathf4[[yr]][complete.cases(da.SST.Pathf4[[yr]]),]
  names(da.SST.Pathf4[[yr]]) <- c("time","sst")
  da.SST.Pathf4[[yr]]$month <- substr(da.SST.Pathf4[[yr]]$time,6,7)
  mo.SST.Pathf4[[yr]] <- aggregate(da.SST.Pathf4[[yr]]$sst, by=list(da.SST.Pathf4[[yr]]$month), FUN=mean)
}
mo.SST.Pathf4[[13]] <- rbind(mo.SST.Pathf4[[13]], c("10",NA), c("11",NA), c("12",NA) )




# Coastal ocean subregion 5
ypos<-(poly5[,2])    # lat
xpos<-(poly5[,1])#+360   # lon
dataInfo <- rerddap::info('nodcPH2sstd1day')

years <- 1982:2012
da.SST.Pathf5 <- vector("list",length(years))
mo.SST.Pathf5 <- vector("list",length(years))

for (yr in 1:length(years)) {
  tpos <- c( paste0(years[yr],"-01-01"), paste0(years[yr],"-12-31") )
  SST.dummy <- rxtractogon(dataInfo, xcoord=xpos, ycoord=ypos, tcoord=tpos, parameter="sea_surface_temperature") # array of [Lon, Lat, Time]
  da.SST.Pathf5[[yr]] <- data.frame(SST.dummy$time, apply(SST.dummy$sea_surface_temperature, 3, mean, na.rm=TRUE) )
  da.SST.Pathf5[[yr]] <- da.SST.Pathf5[[yr]][complete.cases(da.SST.Pathf5[[yr]]),]
  names(da.SST.Pathf5[[yr]]) <- c("time","sst")
  da.SST.Pathf5[[yr]]$month <- substr(da.SST.Pathf5[[yr]]$time,6,7)
  mo.SST.Pathf5[[yr]] <- aggregate(da.SST.Pathf5[[yr]]$sst, by=list(da.SST.Pathf5[[yr]]$month), FUN=mean)
}
mo.SST.Pathf5[[13]] <- rbind(mo.SST.Pathf5[[13]], c("10",NA), c("11",NA), c("12",NA) )




# Coastal ocean subregion 6
ypos<-(poly6[,2])    # lat
xpos<-(poly6[,1])#+360   # lon
dataInfo <- rerddap::info('nodcPH2sstd1day')

years <- 1982:2012
da.SST.Pathf6 <- vector("list",length(years))
mo.SST.Pathf6 <- vector("list",length(years))

for (yr in 1:length(years)) {
  tpos <- c( paste0(years[yr],"-01-01"), paste0(years[yr],"-12-31") )
  SST.dummy <- rxtractogon(dataInfo, xcoord=xpos, ycoord=ypos, tcoord=tpos, parameter="sea_surface_temperature") # array of [Lon, Lat, Time]
  da.SST.Pathf6[[yr]] <- data.frame(SST.dummy$time, apply(SST.dummy$sea_surface_temperature, 3, mean, na.rm=TRUE) )
  da.SST.Pathf6[[yr]] <- da.SST.Pathf6[[yr]][complete.cases(da.SST.Pathf6[[yr]]),]
  names(da.SST.Pathf6[[yr]]) <- c("time","sst")
  da.SST.Pathf6[[yr]]$month <- substr(da.SST.Pathf6[[yr]]$time,6,7)
  mo.SST.Pathf6[[yr]] <- aggregate(da.SST.Pathf6[[yr]]$sst, by=list(da.SST.Pathf6[[yr]]$month), FUN=mean)
}
mo.SST.Pathf6[[13]] <- rbind(mo.SST.Pathf6[[13]], c("10",NA), c("11",NA), c("12",NA) )




# Coastal ocean subregion 7
ypos<-(poly7[,2])    # lat
xpos<-(poly7[,1])#+360   # lon
dataInfo <- rerddap::info('nodcPH2sstd1day')

years <- 1982:2012
da.SST.Pathf7 <- vector("list",length(years))
mo.SST.Pathf7 <- vector("list",length(years))

for (yr in 1:length(years)) {
  tpos <- c( paste0(years[yr],"-01-01"), paste0(years[yr],"-12-31") )
  SST.dummy <- rxtractogon(dataInfo, xcoord=xpos, ycoord=ypos, tcoord=tpos, parameter="sea_surface_temperature") # array of [Lon, Lat, Time]
  da.SST.Pathf7[[yr]] <- data.frame(SST.dummy$time, apply(SST.dummy$sea_surface_temperature, 3, mean, na.rm=TRUE) )
  da.SST.Pathf7[[yr]] <- da.SST.Pathf7[[yr]][complete.cases(da.SST.Pathf7[[yr]]),]
  names(da.SST.Pathf7[[yr]]) <- c("time","sst")
  da.SST.Pathf7[[yr]]$month <- substr(da.SST.Pathf7[[yr]]$time,6,7)
  mo.SST.Pathf7[[yr]] <- aggregate(da.SST.Pathf7[[yr]]$sst, by=list(da.SST.Pathf7[[yr]]$month), FUN=mean)
}
mo.SST.Pathf7[[13]] <- rbind(mo.SST.Pathf7[[13]], c("10",NA), c("11",NA), c("12",NA) )




# Coastal ocean subregion N1 coho
ypos<-(polyN1[,2])    # lat
xpos<-(polyN1[,1])#+360   # lon
dataInfo <- rerddap::info('nodcPH2sstd1day')

years <- 1982:2012
da.SST.PathfN1 <- vector("list",length(years))
mo.SST.PathfN1 <- vector("list",length(years))

for (yr in 1:length(years)) {
  tpos <- c( paste0(years[yr],"-01-01"), paste0(years[yr],"-12-31") )
  SST.dummy <- rxtractogon(dataInfo, xcoord=xpos, ycoord=ypos, tcoord=tpos, parameter="sea_surface_temperature") # array of [Lon, Lat, Time]
  da.SST.PathfN1[[yr]] <- data.frame(SST.dummy$time, apply(SST.dummy$sea_surface_temperature, 3, mean, na.rm=TRUE) )
  da.SST.PathfN1[[yr]] <- da.SST.PathfN1[[yr]][complete.cases(da.SST.PathfN1[[yr]]),]
  names(da.SST.PathfN1[[yr]]) <- c("time","sst")
  da.SST.PathfN1[[yr]]$month <- substr(da.SST.PathfN1[[yr]]$time,6,7)
  mo.SST.PathfN1[[yr]] <- aggregate(da.SST.PathfN1[[yr]]$sst, by=list(da.SST.PathfN1[[yr]]$month), FUN=mean)
}
mo.SST.PathfN1[[13]] <- rbind(mo.SST.PathfN1[[13]], c("10",NA), c("11",NA), c("12",NA) )



# Coastal ocean subregion N2 Chinook Vancouver Island-Queen Charlotte Islands
ypos<-(polyN2[,2])    # lat
xpos<-(polyN2[,1])#+360   # lon
dataInfo <- rerddap::info('nodcPH2sstd1day')

years <- 1982:2012
da.SST.PathfN2 <- vector("list",length(years))
mo.SST.PathfN2 <- vector("list",length(years))

for (yr in 4:length(years)) {
  tpos <- c( paste0(years[yr],"-01-01"), paste0(years[yr],"-12-31") )
  SST.dummy <- rxtractogon(dataInfo, xcoord=xpos, ycoord=ypos, tcoord=tpos, parameter="sea_surface_temperature") # array of [Lon, Lat, Time]
  da.SST.PathfN2[[yr]] <- data.frame(SST.dummy$time, apply(SST.dummy$sea_surface_temperature, 3, mean, na.rm=TRUE) )
  da.SST.PathfN2[[yr]] <- da.SST.PathfN2[[yr]][complete.cases(da.SST.PathfN2[[yr]]),]
  names(da.SST.PathfN2[[yr]]) <- c("time","sst")
  da.SST.PathfN2[[yr]]$month <- substr(da.SST.PathfN2[[yr]]$time,6,7)
  mo.SST.PathfN2[[yr]] <- aggregate(da.SST.PathfN2[[yr]]$sst, by=list(da.SST.PathfN2[[yr]]$month), FUN=mean)
}
mo.SST.PathfN2[[13]] <- rbind(mo.SST.PathfN2[[13]], c("10",NA), c("11",NA), c("12",NA) )



# Coastal ocean subregion N3 Chinook SEAK
ypos<-(polyN3[,2])    # lat
xpos<-(polyN3[,1])#+360   # lon
dataInfo <- rerddap::info('nodcPH2sstd1day')

years <- 1982:2012
da.SST.PathfN3 <- vector("list",length(years))
mo.SST.PathfN3 <- vector("list",length(years))

for (yr in 1:length(years)) {
  tpos <- c( paste0(years[yr],"-01-01"), paste0(years[yr],"-12-31") )
  SST.dummy <- rxtractogon(dataInfo, xcoord=xpos, ycoord=ypos, tcoord=tpos, parameter="sea_surface_temperature") # array of [Lon, Lat, Time]
  da.SST.PathfN3[[yr]] <- data.frame(SST.dummy$time, apply(SST.dummy$sea_surface_temperature, 3, mean, na.rm=TRUE) )
  da.SST.PathfN3[[yr]] <- da.SST.PathfN3[[yr]][complete.cases(da.SST.PathfN3[[yr]]),]
  names(da.SST.PathfN3[[yr]]) <- c("time","sst")
  da.SST.PathfN3[[yr]]$month <- substr(da.SST.PathfN3[[yr]]$time,6,7)
  mo.SST.PathfN3[[yr]] <- aggregate(da.SST.PathfN3[[yr]]$sst, by=list(da.SST.PathfN3[[yr]]$month), FUN=mean)
}
mo.SST.PathfN3[[13]] <- rbind(mo.SST.PathfN3[[13]], c("10",NA), c("11",NA), c("12",NA) )




# Satellite # Polygon # year # month # SST # 
satSSTPathf <- data.frame(satellite="Pathfinder", polygon=c(rep(1:7,each=31*12), rep("N1",31*12), rep("N2", 31*12), rep("N3", 31*12)),
                     year=rep(1982:2012,each=12), month=1:12, sst=NA)
for (i in c(0:30)) {
  satSSTPathf[satSSTPathf$satellite=="Pathfinder" & satSSTPathf$polygon=="1",] [i*12+(1:12),"sst"] <- mo.SST.Pathf1[[i+1]]$x
  satSSTPathf[satSSTPathf$satellite=="Pathfinder" & satSSTPathf$polygon=="2",] [i*12+(1:12),"sst"] <- mo.SST.Pathf2[[i+1]]$x
  satSSTPathf[satSSTPathf$satellite=="Pathfinder" & satSSTPathf$polygon=="3",] [i*12+(1:12),"sst"] <- mo.SST.Pathf3[[i+1]]$x
  satSSTPathf[satSSTPathf$satellite=="Pathfinder" & satSSTPathf$polygon=="4",] [i*12+(1:12),"sst"] <- mo.SST.Pathf4[[i+1]]$x
  satSSTPathf[satSSTPathf$satellite=="Pathfinder" & satSSTPathf$polygon=="5",] [i*12+(1:12),"sst"] <- mo.SST.Pathf5[[i+1]]$x
  satSSTPathf[satSSTPathf$satellite=="Pathfinder" & satSSTPathf$polygon=="6",] [i*12+(1:12),"sst"] <- mo.SST.Pathf6[[i+1]]$x
  satSSTPathf[satSSTPathf$satellite=="Pathfinder" & satSSTPathf$polygon=="7",] [i*12+(1:12),"sst"] <- mo.SST.Pathf7[[i+1]]$x
  satSSTPathf[satSSTPathf$satellite=="Pathfinder" & satSSTPathf$polygon=="N1",] [i*12+(1:12),"sst"] <- mo.SST.PathfN1[[i+1]]$x
  satSSTPathf[satSSTPathf$satellite=="Pathfinder" & satSSTPathf$polygon=="N2",] [i*12+(1:12),"sst"] <- mo.SST.PathfN2[[i+1]]$x
  satSSTPathf[satSSTPathf$satellite=="Pathfinder" & satSSTPathf$polygon=="N3",] [i*12+(1:12),"sst"] <- mo.SST.PathfN3[[i+1]]$x
}

satSSTMUR <- data.frame(satellite="MUR", polygon=c(rep(1:7,each=18*12), rep("N1",18*12), rep("N2", 18*12), rep("N3", 18*12)),
                        year=rep(2003:2020,each=12), month=1:12, sst=NA)
for (i in c(0:18)) {
  satSSTMUR[satSSTMUR$satellite=="MUR" & satSSTMUR$polygon=="1","sst"] <- mo.mean.SST.jplMUR1
  satSSTMUR[satSSTMUR$satellite=="MUR" & satSSTMUR$polygon=="2","sst"] <- mo.mean.SST.jplMUR2
  satSSTMUR[satSSTMUR$satellite=="MUR" & satSSTMUR$polygon=="3","sst"] <- mo.mean.SST.jplMUR3
  satSSTMUR[satSSTMUR$satellite=="MUR" & satSSTMUR$polygon=="4","sst"] <- mo.mean.SST.jplMUR4
  satSSTMUR[satSSTMUR$satellite=="MUR" & satSSTMUR$polygon=="5","sst"] <- mo.mean.SST.jplMUR5
  satSSTMUR[satSSTMUR$satellite=="MUR" & satSSTMUR$polygon=="6","sst"] <- mo.mean.SST.jplMUR6
  satSSTMUR[satSSTMUR$satellite=="MUR" & satSSTMUR$polygon=="7","sst"] <- mo.mean.SST.jplMUR7
  satSSTMUR[satSSTMUR$satellite=="MUR" & satSSTMUR$polygon=="N1","sst"] <- mo.mean.SST.jplMURN1
  satSSTMUR[satSSTMUR$satellite=="MUR" & satSSTMUR$polygon=="N2","sst"] <- mo.mean.SST.jplMURN2
  satSSTMUR[satSSTMUR$satellite=="MUR" & satSSTMUR$polygon=="N3","sst"] <- mo.mean.SST.jplMURN3
}

satSST <- data.frame(rbind(satSSTPathf, satSSTMUR))
satSST$quarter <- NA
satSST$quarter[satSST$month %in% 1:3] <- 1
satSST$quarter[satSST$month %in% 4:6] <- 2
satSST$quarter[satSST$month %in% 7:9] <- 3
satSST$quarter[satSST$month %in% 10:12] <- 4


date.fn <- function(x) {as.character(as.Date(paste0(x[1],"-",x[2],"-15"), format="%Y-%m-%d"))}
satSST$date <- as.character("NA")
satSST$date <- apply(satSST[,c("year","month")], 1, FUN=date.fn)
satSST$date <- as.Date(satSST$date, format="%Y-%m-%d")

satSST$sst <- as.numeric(satSST$sst)


pdf("/Users/jennifergosselin/Dropbox/NRC postdoc/Data/Satellite data/satSST monthly Pathf - MUR 190329.pdf", width=7, height=15, onefile=TRUE)
par(mfrow=c(10,1), mar=c(1,1,1,1), oma=c(4,4,1,1))
# par(mfrow=c(1,1))
quartz()

plot(satSST[satSST$satellite=="Pathfinder" & satSST$polygon=="N3","date"], satSST[satSST$satellite=="Pathfinder" & satSST$polygon=="N3","sst"], type="l", lwd=2, lty=1, col=dum.col[1], xlim=range(satSST$date), xlab="", ylab="", xaxt='n')
lines(satSST[satSST$satellite=="MUR" & satSST$polygon=="N3","date"], satSST[satSST$satellite=="MUR" & satSST$polygon=="N3","sst"], lwd=1, lty=1, col=dum.col[1+1])
legend("bottomright", "Sub-ecoregion N3", bty="n")
legend("topleft", legend=c("Pathfinder", "MUR"), lty=c(1,3), cex=0.7, bty="n")

plot(satSST[satSST$satellite=="Pathfinder" & satSST$polygon=="N2","date"], satSST[satSST$satellite=="Pathfinder" & satSST$polygon=="N2","sst"], type="l", lwd=2, lty=1, col=dum.col[2], xlim=range(satSST$date), xlab="", ylab="", xaxt='n')
lines(satSST[satSST$satellite=="MUR" & satSST$polygon=="N2","date"], satSST[satSST$satellite=="MUR" & satSST$polygon=="N2","sst"], lwd=1, lty=1, col=dum.col[2+1])
legend("bottomright", "Sub-ecoregion N2", bty="n")

plot(satSST[satSST$satellite=="Pathfinder" & satSST$polygon=="N1","date"], satSST[satSST$satellite=="Pathfinder" & satSST$polygon=="N1","sst"], type="l", lwd=2, lty=1, col=dum.col[3], xlim=range(satSST$date), xlab="", ylab="", xaxt='n')
lines(satSST[satSST$satellite=="MUR" & satSST$polygon=="N1","date"], satSST[satSST$satellite=="MUR" & satSST$polygon=="N1","sst"], lwd=1, lty=1, col=dum.col[3+1])
legend("bottomright", "Sub-ecoregion N1", bty="n")

plot(satSST[satSST$satellite=="Pathfinder" & satSST$polygon=="1","date"], satSST[satSST$satellite=="Pathfinder" & satSST$polygon=="1","sst"], type="l", lwd=2, lty=1, col=dum.col[4],xlim=range(satSST$date), xlab="", ylab="", xaxt='n')
lines(satSST[satSST$satellite=="MUR" & satSST$polygon=="1","date"], satSST[satSST$satellite=="MUR" & satSST$polygon=="1","sst"], lwd=1, lty=1, col=dum.col[4+1])
legend("bottomright", "Sub-ecoregion 1", bty="n")

plot(satSST[satSST$satellite=="Pathfinder" & satSST$polygon=="2","date"], satSST[satSST$satellite=="Pathfinder" & satSST$polygon=="2","sst"], type="l", lwd=2, lty=1, col=dum.col[5], xlim=range(satSST$date), xlab="", ylab="", xaxt='n')
lines(satSST[satSST$satellite=="MUR" & satSST$polygon=="2","date"],satSST[satSST$satellite=="MUR" & satSST$polygon=="2","sst"], lwd=1, lty=1, col=dum.col[5+1])
legend("bottomright", "Sub-ecoregion 2", bty="n")

plot(satSST[satSST$satellite=="Pathfinder" & satSST$polygon=="3","date"], satSST[satSST$satellite=="Pathfinder" & satSST$polygon=="3","sst"], type="l", lwd=2, lty=1, col=dum.col[6], xlim=range(satSST$date), xlab="", ylab="", xaxt='n')
lines(satSST[satSST$satellite=="MUR" & satSST$polygon=="3","date"], satSST[satSST$satellite=="MUR" & satSST$polygon=="3","sst"], lwd=1, lty=1, col=dum.col[6+1])
legend("bottomright", "Sub-ecoregion 3", bty="n")

plot(satSST[satSST$satellite=="Pathfinder" & satSST$polygon=="4","date"], satSST[satSST$satellite=="Pathfinder" & satSST$polygon=="4","sst"], type="l", lwd=2, lty=1, col=dum.col[7], xlim=range(satSST$date), xlab="", ylab="", xaxt='n')
lines(satSST[satSST$satellite=="MUR" & satSST$polygon=="4","date"], satSST[satSST$satellite=="MUR" & satSST$polygon=="4","sst"], lwd=1, lty=1, col=dum.col[7+1])
legend("bottomright", "Sub-ecoregion 4", bty="n")

plot(satSST[satSST$satellite=="Pathfinder" & satSST$polygon=="5","date"], satSST[satSST$satellite=="Pathfinder" & satSST$polygon=="5","sst"], type="l", lwd=2, lty=1, col=dum.col[8], xlim=range(satSST$date), xlab="", ylab="", xaxt='n')
lines(satSST[satSST$satellite=="MUR" & satSST$polygon=="5","date"], satSST[satSST$satellite=="MUR" & satSST$polygon=="5","sst"], lwd=1, lty=1, col=dum.col[8+1])
legend("bottomright", "Sub-ecoregion 5", bty="n")

plot(satSST[satSST$satellite=="Pathfinder" & satSST$polygon=="6","date"], satSST[satSST$satellite=="Pathfinder" & satSST$polygon=="6","sst"], type="l", lwd=2, lty=1, col=dum.col[9], xlim=range(satSST$date), xlab="", ylab="", xaxt='n')
lines(satSST[satSST$satellite=="MUR" & satSST$polygon=="6","date"], satSST[satSST$satellite=="MUR" & satSST$polygon=="6","sst"], lwd=1, lty=1, col=dum.col[9+1])
legend("bottomright", "Sub-ecoregion 6", bty="n")

plot(satSST[satSST$satellite=="Pathfinder" & satSST$polygon=="7","date"], satSST[satSST$satellite=="Pathfinder" & satSST$polygon=="7","sst"], type="l", lwd=2, lty=1, col=dum.col[10], xlim=range(satSST$date), xlab="", ylab="")
lines(satSST[satSST$satellite=="MUR" & satSST$polygon=="7","date"], satSST[satSST$satellite=="MUR" & satSST$polygon=="7","sst"], lwd=1, lty=1, col=dum.col[10+1])
legend("bottomright", "Sub-ecoregion 7", bty="n")


title(xlab="Year", ylab="mean monthly satSST", outer=TRUE)

dev.off()



pdf("/Users/jennifergosselin/Dropbox/NRC postdoc/Data/Satellite data/satSST seasonal Pathf - MUR 190329.pdf", width=7, height=15, onefile=TRUE)
par(mfrow=c(10,1), mar=c(1,1,1,1), oma=c(4,4,1,1))
# par(mfrow=c(1,1))
satSST.season <- satSST.season.orig

plot(satSST.season[satSST.season$satellite=="Pathfinder" & satSST.season$polygon=="N3","date"], satSST.season[satSST.season$satellite=="Pathfinder" & satSST.season$polygon=="N3","sst"], type="l", lwd=2, lty=1, col=dum.col[1], xlim=range(satSST.season$date), xlab="", ylab="", xaxt='n')
lines(satSST.season[satSST.season$satellite=="MUR" & satSST.season$polygon=="N3","date"], satSST.season[satSST.season$satellite=="MUR" & satSST.season$polygon=="N3","sst"], lwd=1, lty=1, col=dum.col[1+1])
legend("bottomright", "Sub-ecoregion N3", bty="n")
legend("topleft", legend=c("Pathfinder", "MUR"), lty=c(1,3), cex=0.7, bty="n")

plot(satSST.season[satSST.season$satellite=="Pathfinder" & satSST.season$polygon=="N2","date"], satSST.season[satSST.season$satellite=="Pathfinder" & satSST.season$polygon=="N2","sst"], type="l", lwd=2, lty=1, col=dum.col[2], xlim=range(satSST.season$date), xlab="", ylab="", xaxt='n')
lines(satSST.season[satSST.season$satellite=="MUR" & satSST.season$polygon=="N2","date"], satSST.season[satSST.season$satellite=="MUR" & satSST.season$polygon=="N2","sst"], lwd=1, lty=1, col=dum.col[2+1])
legend("bottomright", "Sub-ecoregion N2", bty="n")

plot(satSST.season[satSST.season$satellite=="Pathfinder" & satSST.season$polygon=="N1","date"], satSST.season[satSST.season$satellite=="Pathfinder" & satSST.season$polygon=="N1","sst"], type="l", lwd=2, lty=1, col=dum.col[3], xlim=range(satSST.season$date), xlab="", ylab="", xaxt='n')
lines(satSST.season[satSST.season$satellite=="MUR" & satSST.season$polygon=="N1","date"], satSST.season[satSST.season$satellite=="MUR" & satSST.season$polygon=="N1","sst"], lwd=1, lty=1, col=dum.col[3+1])
legend("bottomright", "Sub-ecoregion N1", bty="n")

plot(satSST.season[satSST.season$satellite=="Pathfinder" & satSST.season$polygon=="1","date"], satSST.season[satSST.season$satellite=="Pathfinder" & satSST.season$polygon=="1","sst"], type="l", lwd=2, lty=1, col=dum.col[4],xlim=range(satSST.season$date), xlab="", ylab="", xaxt='n')
lines(satSST.season[satSST.season$satellite=="MUR" & satSST.season$polygon=="1","date"], satSST.season[satSST.season$satellite=="MUR" & satSST.season$polygon=="1","sst"], lwd=1, lty=1, col=dum.col[4+1])
legend("bottomright", "Sub-ecoregion 1", bty="n")

plot(satSST.season[satSST.season$satellite=="Pathfinder" & satSST.season$polygon=="2","date"], satSST.season[satSST.season$satellite=="Pathfinder" & satSST.season$polygon=="2","sst"], type="l", lwd=2, lty=1, col=dum.col[5], xlim=range(satSST.season$date), xlab="", ylab="", xaxt='n')
lines(satSST.season[satSST.season$satellite=="MUR" & satSST.season$polygon=="2","date"],satSST.season[satSST.season$satellite=="MUR" & satSST.season$polygon=="2","sst"], lwd=1, lty=1, col=dum.col[5+1])
legend("bottomright", "Sub-ecoregion 2", bty="n")

plot(satSST.season[satSST.season$satellite=="Pathfinder" & satSST.season$polygon=="3","date"], satSST.season[satSST.season$satellite=="Pathfinder" & satSST.season$polygon=="3","sst"], type="l", lwd=2, lty=1, col=dum.col[6], xlim=range(satSST.season$date), xlab="", ylab="", xaxt='n')
lines(satSST.season[satSST.season$satellite=="MUR" & satSST.season$polygon=="3","date"], satSST.season[satSST.season$satellite=="MUR" & satSST.season$polygon=="3","sst"], lwd=1, lty=1, col=dum.col[6+1])
legend("bottomright", "Sub-ecoregion 3", bty="n")

plot(satSST.season[satSST.season$satellite=="Pathfinder" & satSST.season$polygon=="4","date"], satSST.season[satSST.season$satellite=="Pathfinder" & satSST.season$polygon=="4","sst"], type="l", lwd=2, lty=1, col=dum.col[7], xlim=range(satSST.season$date), xlab="", ylab="", xaxt='n')
lines(satSST.season[satSST.season$satellite=="MUR" & satSST.season$polygon=="4","date"], satSST.season[satSST.season$satellite=="MUR" & satSST.season$polygon=="4","sst"], lwd=1, lty=1, col=dum.col[7+1])
legend("bottomright", "Sub-ecoregion 4", bty="n")

plot(satSST.season[satSST.season$satellite=="Pathfinder" & satSST.season$polygon=="5","date"], satSST.season[satSST.season$satellite=="Pathfinder" & satSST.season$polygon=="5","sst"], type="l", lwd=2, lty=1, col=dum.col[8], xlim=range(satSST.season$date), xlab="", ylab="", xaxt='n')
lines(satSST.season[satSST.season$satellite=="MUR" & satSST.season$polygon=="5","date"], satSST.season[satSST.season$satellite=="MUR" & satSST.season$polygon=="5","sst"], lwd=1, lty=1, col=dum.col[8+1])
legend("bottomright", "Sub-ecoregion 5", bty="n")

plot(satSST.season[satSST.season$satellite=="Pathfinder" & satSST.season$polygon=="6","date"], satSST.season[satSST.season$satellite=="Pathfinder" & satSST.season$polygon=="6","sst"], type="l", lwd=2, lty=1, col=dum.col[9], xlim=range(satSST.season$date), xlab="", ylab="", xaxt='n')
lines(satSST.season[satSST.season$satellite=="MUR" & satSST.season$polygon=="6","date"], satSST.season[satSST.season$satellite=="MUR" & satSST.season$polygon=="6","sst"], lwd=1, lty=1, col=dum.col[9+1])
legend("bottomright", "Sub-ecoregion 6", bty="n")

plot(satSST.season[satSST.season$satellite=="Pathfinder" & satSST.season$polygon=="7","date"], satSST.season[satSST.season$satellite=="Pathfinder" & satSST.season$polygon=="7","sst"], type="l", lwd=2, lty=1, col=dum.col[10], xlim=range(satSST.season$date), xlab="", ylab="")
lines(satSST.season[satSST.season$satellite=="MUR" & satSST.season$polygon=="7","date"], satSST.season[satSST.season$satellite=="MUR" & satSST.season$polygon=="7","sst"], lwd=1, lty=1, col=dum.col[10+1])
legend("bottomright", "Sub-ecoregion 7", bty="n")


title(xlab="Year", ylab="mean seasonal satSST", outer=TRUE)

dev.off()




satSST.season <- aggregate(sst ~ quarter + year + polygon + satellite, satSST, drop=FALSE, FUN=mean)
satSST.season$month <- c(2,5,8,11)
satSST.season$date <- as.character("NA")
satSST.season$date <- apply(satSST.season[,c("year","month")], 1, FUN=date.fn)
satSST.season$date <- as.Date(satSST.season$date, format="%Y-%m-%d")


satSST.season.comp <- merge(satSST.season[satSST.season$satellite=="Pathfinder",c("date","satellite","polygon","sst")], satSST.season[satSST.season$satellite=="MUR",c("date","satellite","polygon","sst")], by=c("date","polygon"))
plot(satSST.season.comp$sst.y, satSST.season.comp$sst.x)



# predictive linear modeling
polynames <- unique(satSST.season$polygon)
lm.df <- data.frame(matrix(NA,10,4))
for (i in 1:10) {
  lm.df[i,1:2] <- coef(lm(sst.y ~ sst.x, data=satSST.season.comp[satSST.season.comp$polygon==polynames[i],]))
  lm.df[i,3] <- summary(lm(sst.y ~ sst.x, data=satSST.season.comp[satSST.season.comp$polygon==polynames[i],]))$df[2] +1
  lm.df[i,4] <- round(summary(lm(sst.y ~ sst.x, data=satSST.season.comp[satSST.season.comp$polygon==polynames[i],]))$r.squared, 3)
}



# lm.df <- round(lm.df,3)

# satSST.season.orig <- satSST.season


# transform Pathfinder sst data
for (i in 1:10) {
  satSST.season$sst[satSST.season$satellite=="Pathfinder" & satSST.season$polygon==polynames[i]] <- lm.df[i,1] + lm.df[i,2]*satSST.season$sst[satSST.season$satellite=="Pathfinder" & satSST.season$polygon==polynames[i]]
}

satSST.season[satSST.season$year==1994,]

pdf("/Users/jennifergosselin/Dropbox/NRC postdoc/Data/Satellite data/satSST seasonal transformed Pathf - MUR 190329.pdf", width=7, height=15, onefile=TRUE)
par(mfrow=c(10,1), mar=c(1,1,1,1), oma=c(4,4,1,1))
# par(mfrow=c(1,1))

plot(satSST.season[satSST.season$satellite=="Pathfinder" & satSST.season$polygon=="N3","date"], satSST.season[satSST.season$satellite=="Pathfinder" & satSST.season$polygon=="N3","sst"], type="l", lwd=2, lty=1, col=dum.col[1], xlim=range(satSST.season$date), xlab="", ylab="", xaxt='n')
lines(satSST.season[satSST.season$satellite=="MUR" & satSST.season$polygon=="N3","date"], satSST.season[satSST.season$satellite=="MUR" & satSST.season$polygon=="N3","sst"], lwd=1, lty=1, col=dum.col[1+1])
legend("bottomright", "Sub-ecoregion N3", bty="n")
legend("topleft", legend=c("transformed Pathfinder", "MUR"), lty=c(1,3), cex=0.7, bty="n")

plot(satSST.season[satSST.season$satellite=="Pathfinder" & satSST.season$polygon=="N2","date"], satSST.season[satSST.season$satellite=="Pathfinder" & satSST.season$polygon=="N2","sst"], type="l", lwd=2, lty=1, col=dum.col[2], xlim=range(satSST.season$date), xlab="", ylab="", xaxt='n')
lines(satSST.season[satSST.season$satellite=="MUR" & satSST.season$polygon=="N2","date"], satSST.season[satSST.season$satellite=="MUR" & satSST.season$polygon=="N2","sst"], lwd=1, lty=1, col=dum.col[2+1])
legend("bottomright", "Sub-ecoregion N2", bty="n")

plot(satSST.season[satSST.season$satellite=="Pathfinder" & satSST.season$polygon=="N1","date"], satSST.season[satSST.season$satellite=="Pathfinder" & satSST.season$polygon=="N1","sst"], type="l", lwd=2, lty=1, col=dum.col[3], xlim=range(satSST.season$date), xlab="", ylab="", xaxt='n')
lines(satSST.season[satSST.season$satellite=="MUR" & satSST.season$polygon=="N1","date"], satSST.season[satSST.season$satellite=="MUR" & satSST.season$polygon=="N1","sst"], lwd=1, lty=1, col=dum.col[3+1])
legend("bottomright", "Sub-ecoregion N1", bty="n")

plot(satSST.season[satSST.season$satellite=="Pathfinder" & satSST.season$polygon=="1","date"], satSST.season[satSST.season$satellite=="Pathfinder" & satSST.season$polygon=="1","sst"], type="l", lwd=2, lty=1, col=dum.col[4],xlim=range(satSST.season$date), xlab="", ylab="", xaxt='n')
lines(satSST.season[satSST.season$satellite=="MUR" & satSST.season$polygon=="1","date"], satSST.season[satSST.season$satellite=="MUR" & satSST.season$polygon=="1","sst"], lwd=1, lty=1, col=dum.col[4+1])
legend("bottomright", "Sub-ecoregion 1", bty="n")

plot(satSST.season[satSST.season$satellite=="Pathfinder" & satSST.season$polygon=="2","date"], satSST.season[satSST.season$satellite=="Pathfinder" & satSST.season$polygon=="2","sst"], type="l", lwd=2, lty=1, col=dum.col[5], xlim=range(satSST.season$date), xlab="", ylab="", xaxt='n')
lines(satSST.season[satSST.season$satellite=="MUR" & satSST.season$polygon=="2","date"],satSST.season[satSST.season$satellite=="MUR" & satSST.season$polygon=="2","sst"], lwd=1, lty=1, col=dum.col[5+1])
legend("bottomright", "Sub-ecoregion 2", bty="n")

plot(satSST.season[satSST.season$satellite=="Pathfinder" & satSST.season$polygon=="3","date"], satSST.season[satSST.season$satellite=="Pathfinder" & satSST.season$polygon=="3","sst"], type="l", lwd=2, lty=1, col=dum.col[6], xlim=range(satSST.season$date), xlab="", ylab="", xaxt='n')
lines(satSST.season[satSST.season$satellite=="MUR" & satSST.season$polygon=="3","date"], satSST.season[satSST.season$satellite=="MUR" & satSST.season$polygon=="3","sst"], lwd=1, lty=1, col=dum.col[6+1])
legend("bottomright", "Sub-ecoregion 3", bty="n")

plot(satSST.season[satSST.season$satellite=="Pathfinder" & satSST.season$polygon=="4","date"], satSST.season[satSST.season$satellite=="Pathfinder" & satSST.season$polygon=="4","sst"], type="l", lwd=2, lty=1, col=dum.col[7], xlim=range(satSST.season$date), xlab="", ylab="", xaxt='n')
lines(satSST.season[satSST.season$satellite=="MUR" & satSST.season$polygon=="4","date"], satSST.season[satSST.season$satellite=="MUR" & satSST.season$polygon=="4","sst"], lwd=1, lty=1, col=dum.col[7+1])
legend("bottomright", "Sub-ecoregion 4", bty="n")

plot(satSST.season[satSST.season$satellite=="Pathfinder" & satSST.season$polygon=="5","date"], satSST.season[satSST.season$satellite=="Pathfinder" & satSST.season$polygon=="5","sst"], type="l", lwd=2, lty=1, col=dum.col[8], xlim=range(satSST.season$date), xlab="", ylab="", xaxt='n')
lines(satSST.season[satSST.season$satellite=="MUR" & satSST.season$polygon=="5","date"], satSST.season[satSST.season$satellite=="MUR" & satSST.season$polygon=="5","sst"], lwd=1, lty=1, col=dum.col[8+1])
legend("bottomright", "Sub-ecoregion 5", bty="n")

plot(satSST.season[satSST.season$satellite=="Pathfinder" & satSST.season$polygon=="6","date"], satSST.season[satSST.season$satellite=="Pathfinder" & satSST.season$polygon=="6","sst"], type="l", lwd=2, lty=1, col=dum.col[9], xlim=range(satSST.season$date), xlab="", ylab="", xaxt='n')
lines(satSST.season[satSST.season$satellite=="MUR" & satSST.season$polygon=="6","date"], satSST.season[satSST.season$satellite=="MUR" & satSST.season$polygon=="6","sst"], lwd=1, lty=1, col=dum.col[9+1])
legend("bottomright", "Sub-ecoregion 6", bty="n")

plot(satSST.season[satSST.season$satellite=="Pathfinder" & satSST.season$polygon=="7","date"], satSST.season[satSST.season$satellite=="Pathfinder" & satSST.season$polygon=="7","sst"], type="l", lwd=2, lty=1, col=dum.col[10], xlim=range(satSST.season$date), xlab="", ylab="")
lines(satSST.season[satSST.season$satellite=="MUR" & satSST.season$polygon=="7","date"], satSST.season[satSST.season$satellite=="MUR" & satSST.season$polygon=="7","sst"], lwd=1, lty=1, col=dum.col[10+1])
legend("bottomright", "Sub-ecoregion 7", bty="n")


title(xlab="Year", ylab="mean seasonal satSST", outer=TRUE)

dev.off()



satSST.DATA <- rbind(satSST.season[satSST.season$satellite=="Pathfinder" & satSST.season$year<2007,],
                     satSST.season[satSST.season$satellite=="MUR" & satSST.season$year>2006,] )
satSST.DATA <- satSST.DATA[,c("satellite","polygon","year","quarter","date","sst")]


plot(satSST.DATA[satSST.DATA$satellite=="Pathfinder" & satSST.DATA$polygon=="N3","date"], satSST.DATA[satSST.DATA$satellite=="Pathfinder" & satSST.DATA$polygon=="N3","sst"], type="l", lwd=2, lty=1, col=dum.col[1],xlim=range(satSST.season$date))
lines(satSST.DATA[satSST.DATA$satellite=="MUR" & satSST.DATA$polygon=="N3","date"], satSST.DATA[satSST.DATA$satellite=="MUR" & satSST.DATA$polygon=="N3","sst"], lwd=1, lty=1, col=dum.col[1+1])

plot(satSST.DATA[satSST.DATA$satellite=="Pathfinder" & satSST.DATA$polygon=="N2","date"], satSST.DATA[satSST.DATA$satellite=="Pathfinder" & satSST.DATA$polygon=="N2","sst"], type="l", lwd=2, lty=1, col=dum.col[2],xlim=range(satSST.season$date))
lines(satSST.DATA[satSST.DATA$satellite=="MUR" & satSST.DATA$polygon=="N2","date"], satSST.DATA[satSST.DATA$satellite=="MUR" & satSST.DATA$polygon=="N2","sst"], lwd=1, lty=1, col=dum.col[2+1])

plot(satSST.DATA[satSST.DATA$satellite=="Pathfinder" & satSST.DATA$polygon=="N1","date"], satSST.DATA[satSST.DATA$satellite=="Pathfinder" & satSST.DATA$polygon=="N1","sst"], type="l", lwd=2, lty=1, col=dum.col[3],xlim=range(satSST.season$date))
lines(satSST.DATA[satSST.DATA$satellite=="MUR" & satSST.DATA$polygon=="N1","date"], satSST.DATA[satSST.DATA$satellite=="MUR" & satSST.DATA$polygon=="N1","sst"], lwd=1, lty=1, col=dum.col[3+1])

plot(satSST.DATA[satSST.DATA$satellite=="Pathfinder" & satSST.DATA$polygon=="1","date"], satSST.DATA[satSST.DATA$satellite=="Pathfinder" & satSST.DATA$polygon=="1","sst"], type="l", lwd=2, lty=1, col=dum.col[4],xlim=range(satSST.season$date))
lines(satSST.DATA[satSST.DATA$satellite=="MUR" & satSST.DATA$polygon=="1","date"], satSST.DATA[satSST.DATA$satellite=="MUR" & satSST.DATA$polygon=="1","sst"], lwd=1, lty=1, col=dum.col[4+1])

plot(satSST.DATA[satSST.DATA$satellite=="Pathfinder" & satSST.DATA$polygon=="2","date"], satSST.DATA[satSST.DATA$satellite=="Pathfinder" & satSST.DATA$polygon=="2","sst"], type="l", lwd=2, lty=1, col=dum.col[5],xlim=range(satSST.season$date))
lines(satSST.DATA[satSST.DATA$satellite=="MUR" & satSST.DATA$polygon=="2","date"],satSST.DATA[satSST.DATA$satellite=="MUR" & satSST.DATA$polygon=="2","sst"], lwd=1, lty=1, col=dum.col[5+1])

plot(satSST.DATA[satSST.DATA$satellite=="Pathfinder" & satSST.DATA$polygon=="3","date"], satSST.DATA[satSST.DATA$satellite=="Pathfinder" & satSST.DATA$polygon=="3","sst"], type="l", lwd=2, lty=1, col=dum.col[6],xlim=range(satSST.season$date))
lines(satSST.DATA[satSST.DATA$satellite=="MUR" & satSST.DATA$polygon=="3","date"], satSST.DATA[satSST.DATA$satellite=="MUR" & satSST.DATA$polygon=="3","sst"], lwd=1, lty=1, col=dum.col[6+1])

plot(satSST.DATA[satSST.DATA$satellite=="Pathfinder" & satSST.DATA$polygon=="4","date"], satSST.DATA[satSST.DATA$satellite=="Pathfinder" & satSST.DATA$polygon=="4","sst"], type="l", lwd=2, lty=1, col=dum.col[7],xlim=range(satSST.season$date))
lines(satSST.DATA[satSST.DATA$satellite=="MUR" & satSST.DATA$polygon=="4","date"], satSST.DATA[satSST.DATA$satellite=="MUR" & satSST.DATA$polygon=="4","sst"], lwd=1, lty=1, col=dum.col[7+1])

plot(satSST.DATA[satSST.DATA$satellite=="Pathfinder" & satSST.DATA$polygon=="5","date"], satSST.DATA[satSST.DATA$satellite=="Pathfinder" & satSST.DATA$polygon=="5","sst"], type="l", lwd=2, lty=1, col=dum.col[8],xlim=range(satSST.season$date))
lines(satSST.DATA[satSST.DATA$satellite=="MUR" & satSST.DATA$polygon=="5","date"], satSST.DATA[satSST.DATA$satellite=="MUR" & satSST.DATA$polygon=="5","sst"], lwd=1, lty=1, col=dum.col[8+1])

plot(satSST.DATA[satSST.DATA$satellite=="Pathfinder" & satSST.DATA$polygon=="6","date"], satSST.DATA[satSST.DATA$satellite=="Pathfinder" & satSST.DATA$polygon=="6","sst"], type="l", lwd=2, lty=1, col=dum.col[9],xlim=range(satSST.season$date))
lines(satSST.DATA[satSST.DATA$satellite=="MUR" & satSST.DATA$polygon=="6","date"], satSST.DATA[satSST.DATA$satellite=="MUR" & satSST.DATA$polygon=="6","sst"], lwd=1, lty=1, col=dum.col[9+1])

plot(satSST.DATA[satSST.DATA$satellite=="Pathfinder" & satSST.DATA$polygon=="7","date"], satSST.DATA[satSST.DATA$satellite=="Pathfinder" & satSST.DATA$polygon=="7","sst"], type="l", lwd=2, lty=1, col=dum.col[10],xlim=range(satSST.season$date))
lines(satSST.DATA[satSST.DATA$satellite=="MUR" & satSST.DATA$polygon=="7","date"], satSST.DATA[satSST.DATA$satellite=="MUR" & satSST.DATA$polygon=="7","sst"], lwd=1, lty=1, col=dum.col[10+1])


satSST.DATA.poly1 <- data.frame(Year=1982:2018, 
                               satSST.Wint = satSST.DATA$sst[satSST.DATA$polygon=="1" & satSST.DATA$quarter==1],
                               satSST.Spr = satSST.DATA$sst[satSST.DATA$polygon=="1" & satSST.DATA$quarter==2],
                               satSST.Sum = satSST.DATA$sst[satSST.DATA$polygon=="1" & satSST.DATA$quarter==3],
                               satSST.Fall = satSST.DATA$sst[satSST.DATA$polygon=="1" & satSST.DATA$quarter==4] )

satSST.DATA.poly2 <- data.frame(Year=1982:2018, 
                                satSST.Wint = satSST.DATA$sst[satSST.DATA$polygon=="2" & satSST.DATA$quarter==1],
                                satSST.Spr = satSST.DATA$sst[satSST.DATA$polygon=="2" & satSST.DATA$quarter==2],
                                satSST.Sum = satSST.DATA$sst[satSST.DATA$polygon=="2" & satSST.DATA$quarter==3],
                                satSST.Fall = satSST.DATA$sst[satSST.DATA$polygon=="2" & satSST.DATA$quarter==4] )


satSST.DATA.poly3 <- data.frame(Year=1982:2018, 
                                satSST.Wint = satSST.DATA$sst[satSST.DATA$polygon=="3" & satSST.DATA$quarter==1],
                                satSST.Spr = satSST.DATA$sst[satSST.DATA$polygon=="3" & satSST.DATA$quarter==2],
                                satSST.Sum = satSST.DATA$sst[satSST.DATA$polygon=="3" & satSST.DATA$quarter==3],
                                satSST.Fall = satSST.DATA$sst[satSST.DATA$polygon=="3" & satSST.DATA$quarter==4] )


satSST.DATA.poly4 <- data.frame(Year=1982:2018, 
                                satSST.Wint = satSST.DATA$sst[satSST.DATA$polygon=="4" & satSST.DATA$quarter==1],
                                satSST.Spr = satSST.DATA$sst[satSST.DATA$polygon=="4" & satSST.DATA$quarter==2],
                                satSST.Sum = satSST.DATA$sst[satSST.DATA$polygon=="4" & satSST.DATA$quarter==3],
                                satSST.Fall = satSST.DATA$sst[satSST.DATA$polygon=="4" & satSST.DATA$quarter==4] )


satSST.DATA.poly5 <- data.frame(Year=1982:2018, 
                                satSST.Wint = satSST.DATA$sst[satSST.DATA$polygon=="5" & satSST.DATA$quarter==1],
                                satSST.Spr = satSST.DATA$sst[satSST.DATA$polygon=="5" & satSST.DATA$quarter==2],
                                satSST.Sum = satSST.DATA$sst[satSST.DATA$polygon=="5" & satSST.DATA$quarter==3],
                                satSST.Fall = satSST.DATA$sst[satSST.DATA$polygon=="5" & satSST.DATA$quarter==4] )


satSST.DATA.poly6 <- data.frame(Year=1982:2018, 
                                satSST.Wint = satSST.DATA$sst[satSST.DATA$polygon=="6" & satSST.DATA$quarter==1],
                                satSST.Spr = satSST.DATA$sst[satSST.DATA$polygon=="6" & satSST.DATA$quarter==2],
                                satSST.Sum = satSST.DATA$sst[satSST.DATA$polygon=="6" & satSST.DATA$quarter==3],
                                satSST.Fall = satSST.DATA$sst[satSST.DATA$polygon=="6" & satSST.DATA$quarter==4] )

satSST.DATA.poly7 <- data.frame(Year=1982:2018, 
                                satSST.Wint = satSST.DATA$sst[satSST.DATA$polygon=="7" & satSST.DATA$quarter==1],
                                satSST.Spr = satSST.DATA$sst[satSST.DATA$polygon=="7" & satSST.DATA$quarter==2],
                                satSST.Sum = satSST.DATA$sst[satSST.DATA$polygon=="7" & satSST.DATA$quarter==3],
                                satSST.Fall = satSST.DATA$sst[satSST.DATA$polygon=="7" & satSST.DATA$quarter==4] )

satSST.DATA.polyN1 <- data.frame(Year=1982:2018, 
                                satSST.Wint = satSST.DATA$sst[satSST.DATA$polygon=="N1" & satSST.DATA$quarter==1],
                                satSST.Spr = satSST.DATA$sst[satSST.DATA$polygon=="N1" & satSST.DATA$quarter==2],
                                satSST.Sum = satSST.DATA$sst[satSST.DATA$polygon=="N1" & satSST.DATA$quarter==3],
                                satSST.Fall = satSST.DATA$sst[satSST.DATA$polygon=="N1" & satSST.DATA$quarter==4] )

satSST.DATA.polyN2 <- data.frame(Year=1982:2018, 
                                satSST.Wint = satSST.DATA$sst[satSST.DATA$polygon=="N2" & satSST.DATA$quarter==1],
                                satSST.Spr = satSST.DATA$sst[satSST.DATA$polygon=="N2" & satSST.DATA$quarter==2],
                                satSST.Sum = satSST.DATA$sst[satSST.DATA$polygon=="N2" & satSST.DATA$quarter==3],
                                satSST.Fall = satSST.DATA$sst[satSST.DATA$polygon=="N2" & satSST.DATA$quarter==4] )

satSST.DATA.polyN3 <- data.frame(Year=1982:2018, 
                                satSST.Wint = satSST.DATA$sst[satSST.DATA$polygon=="N3" & satSST.DATA$quarter==1],
                                satSST.Spr = satSST.DATA$sst[satSST.DATA$polygon=="N3" & satSST.DATA$quarter==2],
                                satSST.Sum = satSST.DATA$sst[satSST.DATA$polygon=="N3" & satSST.DATA$quarter==3],
                                satSST.Fall = satSST.DATA$sst[satSST.DATA$polygon=="N3" & satSST.DATA$quarter==4] )


season.plots <- function(x) {
  plot(x[,2], ylim=range(na.exclude(x[,2:5])), type="l", col="blue")
  lines(x[,3], col="green")
  lines(x[,4], col="red")
  lines(x[,5], col="purple")
}
  
season.plots(satSST.DATA.poly1)
season.plots(satSST.DATA.poly2)
season.plots(satSST.DATA.poly3)
season.plots(satSST.DATA.poly4)
season.plots(satSST.DATA.poly5)
season.plots(satSST.DATA.poly6)
season.plots(satSST.DATA.poly7)
season.plots(satSST.DATA.polyN1)
season.plots(satSST.DATA.polyN2)
season.plots(satSST.DATA.polyN3)

# write.csv(satSST.DATA, "satSST.DATA.1982.2018.csv", row.names=FALSE)
# write.csv(satSST.DATA.poly1, "satSST.subecoreg1.1982.2018.csv", row.names=FALSE)
# write.csv(satSST.DATA.poly2, "satSST.subecoreg2.1982.2018.csv", row.names=FALSE)
# write.csv(satSST.DATA.poly3, "satSST.subecoreg3.1982.2018.csv", row.names=FALSE)
# write.csv(satSST.DATA.poly4, "satSST.subecoreg4.1982.2018.csv", row.names=FALSE)
# write.csv(satSST.DATA.poly5, "satSST.subecoreg5.1982.2018.csv", row.names=FALSE)
# write.csv(satSST.DATA.poly6, "satSST.subecoreg6.1982.2018.csv", row.names=FALSE)
# write.csv(satSST.DATA.poly7, "satSST.subecoreg7.1982.2018.csv", row.names=FALSE)
# write.csv(satSST.DATA.polyN1, "satSST.subecoregN1.1982.2018.csv", row.names=FALSE)
# write.csv(satSST.DATA.polyN2, "satSST.subecoregN2.1982.2018.csv", row.names=FALSE)
# write.csv(satSST.DATA.polyN3, "satSST.subecoregN3.1982.2018.csv", row.names=FALSE)
