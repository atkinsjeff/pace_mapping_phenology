# phenocam

require(phenocamr)
require(ggplot2)
require(tidyverse)
require(segmented)


# #download_phenocam(site = "pace",
#                   frequency = 3,
#                   smooth = FALSE,
#                   out_dir = "./data")



df <- read_phenocam(file.path("./data", "pace_DB_1000_3day.csv"))

# expanding data set
df <- expand_phenocam(df)

#  detect outliers
df <- detect_outliers(df)

# smooth time series
df <- smooth_ts(df)

# transitions
start_of_season <- transition_dates(df)
phenology_dates <- phenophases(df, internal = FALSE, out_dir = "./data")



bois <- read_phenocam(file.path("./data", "pace_DB_1000_3day_transition_dates.csv"))
df$date$date <- as.Date(df$data$date)

x11()
plot(as.Date(df$data$date),
     df$data$smooth_gcc_90,
     type = "l",
     xlab = "date",
     ylab = "Gcc")


# restrict to 2018

x <- as.data.frame(df$data)
x %>%
    filter( year == 2018) -> y

x11(height = 4, width = 4)
ggplot(y, aes( x = doy, y = smooth_gcc_mean))+
    geom_point(shape = 21, fill = "dark green", size = 3, alpha = 1)+
    theme_bw()+
    ylab("DOY")+
    xlab(expression(GCC[PC]))+
    theme(legend.position = "none")+
    geom_vline(xintercept =  116)+
    geom_vline(xintercept = 157)

x11(height = 4, width = 4)
ggplot(y, aes( x = doy, y = smooth_gcc_mean))+
    geom_point(shape = 21, fill = "dark green", size = 3, alpha = 1)+
    theme_bw()+
    ylab("DOY")+
    xlab(expression(GCC[PC]))+
    theme(legend.position = "none")+
    xlim(c(117, 156))

#### segmented analysis
require(segmented)

# sorting down
y %>%
    dplyr::select(doy, smooth_gcc_mean) %>%
    filter(doy > 100 & doy < 160) -> z

par(mar=c(5.1,4.1,4.1,2.1))
# greenness
x <- z$doy
y <- z$smooth_gcc_mean
lm.gcc <- lm(y ~ x)

seg.green <- segmented(lm.gcc, seg.Z = ~x, psi = 120)
bp <-  round(seg.green$psi[2], 2)
bp.se <- round(seg.green$psi[3], 2)

x11(width = 4, height = 4)
plot(x,y,
     ylab = expression(paste("GCC"["PhenoCam"])),
     xlab ="Julian Day",
     bg = "#4287f5",
     pch = 21,
     cex = 1.25)
plot(seg.green, add=T)
text(x = 136, y = 0.33, label = paste(bp, bp.se, sep = "+/-"))
text(x = 136, y = 0.32, label = "Estimated Breakpoint w/ SE")

# rising "spring" greenup dates
geom_abline(v = phenology_dates$rising$transition_50,
            col = "green")+
    # falling "autumn" senescence dates
    abline(v = phenology_dates$falling$transition_50,
           col = "brown")



x <- phenology_dates$rising$transition_50

y <- data.frame(x)


as.POSIXlt(as.numeric(y$x),origin="2017-01-01",tz="GMT")