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

plotDir <- "/Users/hkropp/Library/CloudStorage/GoogleDrive-hkropp@hamilton.edu/My Drive/research/projects/forest_soil/data_analysis"

##### aggregate all data to daily level and join with weather ----

tomstDay <- tomst25

airDay <- weatherDay %>%
  select(doy, year, aveT, maxT, minT)

soilAll <- left_join(tomstDay, airDay, by=c("year","doy"))

soilAll$TempDiff <- soilAll$Tsoil_6 - soilAll$aveT 
soilAll$date <- as.Date(soilAll$doy-1, origin=paste0(soilAll$year, "-01-01"))

soilAll$VWC_f <- ifelse(soilAll$Tsoil_6 <0,NA, soilAll$SWC_12)

weatherDay$date <- as.Date(weatherDay$doy-1, origin=paste0(weatherDay$year, "-01-01"))
weatherJoin <- weatherDay %>%
  select(!c(aveT,nAveT,maxT,minT))
soilmet <- left_join(soilAll, weatherJoin, by=c("doy","year","date"))

##### VWC and frozen soils ----

# gap fill freezing soil data from measurements before freeze
soilmet$freezeSoil <- ifelse(soilmet$Tsoil_6<0,1,0)

soilMet <- soilmet %>%
  arrange(location, year,doy)

freezeCheck<- soilMet %>%
  filter(freezeSoil == 1)
# get all freeze events
freezeEvent <- c(1)
  for(i in 2:nrow(freezeCheck)){
    if(freezeCheck$location[i-1] == freezeCheck$location[i] &
      freezeCheck$year[i-1] == freezeCheck$year[i] & 
      freezeCheck$doy[i-1] == (freezeCheck$doy[i]-1)){
      freezeEvent[i] <- freezeEvent[i-1]
      
    }else{
      freezeEvent[i] <-freezeEvent[i-1]+1}
     
  }
# label start of an event
freezeStart <- c(1)
  for(i in 2:nrow(freezeCheck)){
    if(freezeEvent[i-1]!=freezeEvent[i]){
      freezeStart[i] <- 1
    }else{
      freezeStart[i] <- 0
    }
  }

# get the day before the freeze
freezeLastDay <-  freezeCheck$doy[1]-1
for(i in 2:nrow(freezeCheck)){
  if(freezeStart[i] == 1){
    freezeLastDay[i] <- freezeCheck$doy[i]-1
  }else{
    freezeLastDay[i] <- freezeLastDay[i-1]
  }
}

freezeLastDayYear <-  freezeCheck$year[1]
for(i in 2:nrow(freezeCheck)){
  if(freezeStart[i] == 1){
    freezeLastDayYear[i] <- freezeCheck$year[i]
  }else{
    freezeLastDayYear[i] <- freezeLastDayYear[i-1]
  }
}

freezeCheck$freezeStart <- freezeStart

freezeCheck$freezeEvent <- freezeEvent

freezeCheck$freezeLastDayYear <- freezeLastDayYear
freezeCheck$freezeLastDay <- freezeLastDay


soilSub <- soilMet %>%
  filter(doy==freezeCheck$freezeLastDay[1] & year==freezeCheck$freezeLastDayYear[1] &
           location == freezeCheck$location[1])

freezeFill <- numeric()
for(i in 1:nrow(freezeCheck)){
  soilSub <- soilMet %>%
    filter(doy==freezeCheck$freezeLastDay[i]& year==freezeCheck$freezeLastDayYear[i] &
             location == freezeCheck$location[i])
  freezeFill[i] <- soilSub$SWC_12
}
freezeCheck$lastVWC <- freezeFill

freezeJoin <- freezeCheck %>%
  select(location,year,doy,freezeEvent, freezeStart, lastVWC)

soilMet <- left_join(soilMet, freezeJoin, by=c("location","year","doy"))

soilMet$VWC_gap <- ifelse(soilMet$freezeSoil == 1, soilMet$lastVWC, soilMet$SWC_12)
# use water year starting Oct 1 2023
soilDat <- soilMet %>%
  filter(DD>=2023.747)

# count up how many missing gap filled precip observations
weatherDay$DD <- weatherDay$year + ((weatherDay$doy-1)/ifelse(leap_year(weatherDay$year),366,365))
PrecipCount <- weatherDay %>%
  select(year,DD,Precip_gap) %>%
  filter(DD >=2023.747) %>%
  na.omit() %>%
  group_by(year) %>%
  summarize(ncount = n())
#missing 3 days of precip in 2024
# no missing days other years

######## organize for model ----


######## color scheme for figures ----



####### Figure 1: Met and soil data ----
singleLoc <- soilDat %>%
  filter(location == "hemlock sapflow")

wd <- 45
hd <- 10

# x range
xl <- 2023.74
xh <- 2025.76
#y range for meteorological graph
#air temp and precipitation
yl <- -20
yh <- 30
#precip in mm
prMax <- 60


precipRescale <- function(x,precipMax,ylf,yhf) {
  (((yhf-ylf)/precipMax)*x)+ylf
}
precipRescale(10,prMax,yl,yh)


png(paste0(plotDir,"/daily_data.png"), width = 60, height = 50, units = "cm", res=300)
layout(matrix(c(1,2,3,4),ncol=1), width=lcm(wd),height=rep(lcm(hd),4))
#air temp and precip
par(mai=c(0.25,0,0,0))
plot(c(0,1),c(0,1), type="n", xlim=c(xl,xh), ylim=c(yl,yh), xaxs="i",yaxs="i",
     xlab= " ", ylab=" ", axes=FALSE)


for(i in 1:nrow(singleLoc)){
  polygon(c(singleLoc$DD[i]-0.001,singleLoc$DD[i]-0.001,
            singleLoc$DD[i]+0.001,singleLoc$DD[i]+0.001),
          c(precipRescale(0,prMax,yl,yh),
            precipRescale(singleLoc$Precip_gap[i],prMax,yl,yh),
            precipRescale(singleLoc$Precip_gap[i],prMax,yl,yh),
            precipRescale(0,prMax,yl,yh)),
          col="lightskyblue2", border=NA)
}
points(singleLoc$DD, singleLoc$aveT, type="l", pch=19)
axis(1, seq(2023,2026))


# above surface temp and snow
par(mai=c(0.25,0,0,0))
plot(c(0,1),c(0,1), type="n", xlim=c(xl,xh), ylim=c(yl,yh), xaxs="i",yaxs="i",
     xlab= " ", ylab=" ", axes=FALSE)
# soil temp
par(mai=c(0.25,0,0,0))
plot(c(0,1),c(0,1), type="n", xlim=c(xl,xh), ylim=c(yl,yh), xaxs="i",yaxs="i",
     xlab= " ", ylab=" ", axes=FALSE)
#soil moisture

par(mai=c(0.25,0,0,0))
plot(c(0,1),c(0,1), type="n", xlim=c(xl,xh), ylim=c(yl,yh), xaxs="i",yaxs="i",
     xlab= " ", ylab=" ", axes=FALSE)

dev.off()