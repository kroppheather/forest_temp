#################################################
### analysis of soil temperature and moisture ###
### and leaf area by forest type              ###
### Scripts read in tomst25:                  ###
### soil temp Tsoil 6 cm depth(deg C)         ###
### surface temp: Tsurf 2 cm height (dec C)   ###
### surface temp: Tsurf 15 cm height (dec C)  ###
### soil moisture 0-12 cm depth (m3/m3)       ###
#################################################


##### directories & libraries ----
library(dplyr)
library(lubridate)
library(ggplot2)
dirComp <- c("G:/My Drive/research/projects",
             "/Users/hkropp/Library/CloudStorage/GoogleDrive-hkropp@hamilton.edu/My Drive/research/projects")
compID <- 2

source("/Users/hkropp/Documents/GitHub/forest_temp/tomst_soil.r")
source("/Users/hkropp/Documents/GitHub/forest_temp/weather.r")


tomstDay <- tomst25 %>%
  group_by(location, year, doy ) %>%
  summarize(TSoil6 = mean(Tsoil6),
            TSurf2 = mean(Tsurf2),
            Tsurf15=mean(Tsurf15),
            VWC=mean(SWC))
airDay <- weatherDay %>%
  select(doy, year, aveT, maxT, minT)
soilAll <- left_join(tomstDay, airDay, by=c("year","doy"))

soilAll$TempDiff <- soilAll$TSoil6 - soilAll$aveT 
soilAll$date <- as.Date(soilAll$doy-1, origin=paste0(soilAll$year, "-01-01"))




#### basic time series plots to visualize data

wd <- 20
hd <- 10


