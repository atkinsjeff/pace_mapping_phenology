require(ggplot2)
require(viridis)
require(tidyverse)
require(gridExtra)
require(randomForest)
require(bcp)
require(segmented)

# reading in data
df <- read.csv("./data/pace_drone.csv")

df$date <- as.Date(df$date)

df %>%
    filter(!date == "2018-05-07" ) %>%
    data.frame() -> df

# bringin in the camera data
cam <- read.csv("./data/pace_lai_through_0604.csv")
cam$date <- as.Date(cam$date)
# the camera date has been changed to match the drone date closest
# camera date 2018-05-10 has been changed to 2018-05-07
# # camera date 2018-06-06 has been changed to 2018-06-04
# cam$date[cam$date == "2018-05-10"] <- "2018-05-07"
# cam$date[cam$date == "2018-06-06"] <- "2018-06-04"


# the camera date has been changed to match the drone date closest
# camera date 2018-05-10 has been changed to 2018-05-07
# camera date 2018-06-06 has been changed to 2018-06-04
#cam$date[cam$date == "2018-05-10"] <- "2018-05-07"
#cam$date[cam$date == "2018-06-06"] <- "2018-06-04"

# add matching id column in df
df$plot <- as.factor(paste("pace", df$id, sep = ""))

# change column names for df
colnames(df) <- c("X", "id", "date", "buffer", "mean.green", "sd.green", "plot")

# sort down
df2 <- df[c("plot", "date", "buffer", "mean.green", "sd.green")]

# break apart
df2 %>%
    pivot_wider(names_from = buffer,
                values_from = c(mean.green, sd.green)) %>%
    data.frame() -> df3


# make wide
bill <- merge(cam, df3,  by = c("plot", "date"))

# sort to center only
bill %>%
    filter(subplot == "c") %>%
    data.frame() -> bob
######

#######3


#### segmented analysis
require(segmented)

# bob <- cam
bob$jd <- as.integer(format(bob$date, "%j"))

par(mar=c(5.1,4.1,4.1,2.1))
# greenness
x <- bob$jd
y <- bob$mean.green_20
lm.green <- lm(y ~ x)

seg.green <- segmented(lm.green, seg.Z = ~x, psi = 125)
bp <-  round(seg.green$psi[2], 2)
bp.se <- round(seg.green$psi[3], 2)

x11(width = 4, height = 4)
plot(x,y,
     ylab = expression(paste("GCC"["UAV"])),
     xlab ="Julian Day",
     bg = "#E7298A",
     pch = 21,
     cex = 1.25)
plot(seg.green, add=T)
text(x = 140, y = 0.37, label = paste(bp, bp.se, sep = "+/-"))
text(x = 140, y = 0.35, label = "Estimated Breakpoint w/ SE")

# NDVI
x <- bob$jd
y <- bob$ndvi
lm.ndvi <- lm(y ~ x)

seg.ndvi <- segmented(lm.ndvi, seg.Z = ~x, psi = 125)
bp <-  round(seg.ndvi$psi[2], 2)
bp.se <- round(seg.ndvi$psi[3], 2)

x11(width = 4, height = 4)
plot(x,y,
     ylab = "Camera NDVI",
     xlab ="Julian Day",
     bg = "#1B9E77",
     pch = 21,
     cex = 1.25)
plot(seg.ndvi, add=T)
text(x = 140, y = 0.33, label = paste(bp, bp.se, sep = "+/-"))
text(x = 140, y = 0.42, label = "Estimated Breakpoint w/ SE")

# LAI
x <- bob$jd
y <- bob$lai
lm.lai <- lm(y ~ x)

seg.lai <- segmented(lm.lai, seg.Z = ~x, psi = 125)
bp <-  round(seg.lai$psi[2], 2)
bp.se <- round(seg.lai$psi[3], 2)

x11(width = 4, height = 4)
plot(x,y,
     ylab = "Camera LAI",
     xlab ="Julian Day",
     bg = "#D95F02",
     pch = 21,
     cex = 1.25)
plot(seg.lai, add=T)
text(x = 140, y = 1.7, label = paste(bp, bp.se, sep = "+/-"))
text(x = 140, y = 2.1, label = "Estimated Breakpoint w/ SE")

# GAP FRACTION
x <- bob$jd
y <- bob$gap_fraction
lm.gap_fraction <- lm(y ~ x)

seg.gap_fraction <- segmented(lm.gap_fraction, seg.Z = ~x, psi = 125)
bp <-  round(seg.gap_fraction$psi[2], 2)
bp.se <- round(seg.gap_fraction$psi[3], 2)

x11(width = 4, height = 4)
plot(x,y,
     ylab = "Camera Gap Fraction",
     xlab ="Julian Day",
     bg = "#7570B3",
     pch = 21,
     cex = 1.25)
plot(seg.gap_fraction, add=T)
text(x = 140, y = 32, label = paste(bp, bp.se, sep = "+/-"))
text(x = 140, y = 37, label = "Estimated Breakpoint w/ SE")


##### 
modis <- read.csv("./data/MODIS_PACE.csv")

modis$date <- as.Date(modis$date, "%m/%d/%Y")

modis$jd <- as.integer(format(modis$date, "%j"))

modis %>%
    filter(modis$jd > 90) -> modis

modis$ndvi <- modis$ndvi / 10000

x <- modis$jd
y <- modis$ndvi
lm.modis <- lm(y ~ x)

seg.modis <- segmented(lm.modis, seg.Z = ~x, psi = 125)
bp <-  round(seg.modis$psi[2], 2)
bp.se <- round(seg.modis$psi[3], 2)

x11(width = 4, height = 4)
plot(x,y,
     ylab = "MODIS NDVI",
     xlab ="Julian Day",
     bg = "#2EE4EC",
     pch = 21,
     cex = 1.25)
plot(seg.modis, add=T)
text(x = 160, y = 0.7, label = "Estimated Breakpoint w/ SE")
text(x = 160, y = 0.65, label = paste(bp, bp.se, sep = "+/-"))

#####
##### 
land <- read.csv("./data/l8_mean.csv")
names(land)[1] <- "date"

land$date <- as.Date(land$date, "%Y-%m-%d")

land$jd <- as.integer(format(land$date, "%j"))

land %>%
    filter(land$jd > 90) -> land


x <- land$jd
y <- land$NDVI
lm.land <- lm(y ~ x)

seg.land <- segmented(lm.land, seg.Z = ~x, psi = 125)
bp <-  round(seg.land$psi[2], 2)
bp.se <- round(seg.land$psi[3], 2)

x11(width = 4, height = 4)
plot(x,y,
     ylab = "Landsat NDVI",
     xlab ="Julian Day",
     bg = "light grey",
     pch = 21,
     cex = 1.25)
plot(seg.land, add=T)
text(x = 163, y = 0.35, label = "Estimated Breakpoint w/ SE")
text(x = 163, y = 0.29, label = paste(bp, bp.se, sep = "+/-"))










