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
library(tidyr)

dirComp <- c("G:/My Drive/research/projects",
             "/Users/hkropp/Library/CloudStorage/GoogleDrive-hkropp@hamilton.edu/My Drive/research/projects")
compID <- 2

source("/Users/hkropp/Documents/GitHub/forest_temp/tomst_soil.r")
source("/Users/hkropp/Documents/GitHub/forest_temp/weather.r")

plotDir <- "/Users/hkropp/Library/CloudStorage/GoogleDrive-hkropp@hamilton.edu/My Drive/research/projects/forest_soil/data_analysis"


##### read in additional site and canopy data ----


SpeciesInfo <- read.csv("/Users/hkropp/Library/CloudStorage/GoogleDrive-hkropp@hamilton.edu/My Drive/research/projects/forest_soil/canopy/speciesID.csv")
forestInventory <- read.csv("/Users/hkropp/Library/CloudStorage/GoogleDrive-hkropp@hamilton.edu/My Drive/research/projects/forest_soil/canopy/HCEF forest inventory data 25.csv")

# model output index 
modDir <- "/Users/hkropp/Library/CloudStorage/GoogleDrive-hkropp@hamilton.edu/My Drive/research/projects/forest_soil/model"

##### aggregate all data to daily level and join with weather ----

tomstDay <- tomst25

airDay <- weatherDay %>%
  select(doy, year, aveT, maxT, minT)

ggplot(weatherDay, aes(aveT, SolRad))+
  geom_point()
cor(weatherDay$aveT,weatherDay$SolRad, use="pairwise")

ggplot(weatherDay, aes(aveT, VPDd))+
  geom_point()
cor(weatherDay$aveT,weatherDay$VPDd, use="pairwise")


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
  filter(DD>=2022.747)

soilDat$locID <- ifelse(soilDat$location == "maple-beech", 1, #Deciduous forest
                  ifelse(soilDat$location == "hemlock sapflow", 2, #Conifer-deciduous forest
                  ifelse(soilDat$location == "Spruce RG09", 3, #Conifer monoculture
                  ifelse(soilDat$location == "Buckthorn RG03", 4, #Invasive scrub
                  ifelse(soilDat$location == "Rogers reforestation", 5,NA))))) #Reforestation field

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




soilMod <- soilDat %>%
  filter(is.na(aveT) == FALSE) %>%
  filter(is.na(SNWD) == FALSE)
# create ID for time periods below freezing
soilMod$freezeModID <- ifelse(soilMod$aveT <= 0, 1,2) # 1 below or at freezing
# create ID for forest type and freezing time period
soilMod$modforestID <- ifelse(soilMod$freezeModID == 1, soilMod$locID,
                              soilMod$locID+5)

hist(soilMod$SNWD)
# ID if swc is above or below field capacity
soilMod$swID <- ifelse(soilMod$SWC_12 > 0.33,2,1)
# create snow ID
# threshold for low snow vs high snow
# Zhang, H., Yuan, N., Ma, Z., & Huang, Y. (2021). Understanding the soil temperature variability at different depths: Effects of surface air temperature, snow cover, and the soil memory. Advances in Atmospheric Sciences, 38(3), 493-503.
# found that 80 cm was a threshold for air temps affecting soil temperature in shallow depths, but different location.
# setting threshold to 5 cm since there may be variability across sites
# measurements in whole inches from NOAA coop so 72 would be measurement at 3 inches

soilMod$snowID <- ifelse(soilMod$SNWD > 72, 2,1)
soilMod$regID <- ifelse(soilMod$snowID == 1 & soilMod$freezeModID == 1, 1, # freezing temps low/no snow
                        ifelse(soilMod$snowID == 1 & soilMod$freezeModID == 2, 2, # above freezing temps low/now snow
                               ifelse(soilMod$snowID == 2, 3, NA))) # snow 

# create ID table
soilIDs <- soilMod %>%
  ungroup %>%
  select(location, locID, freezeModID, snowID, regID, swID) %>%
  distinct()


######## color scheme for figures ----
locLabel <- c("Deciduous forest",
              "Conifer-deciduous forest",
              "Conifer monoculture",
              "Invasive scrub",
              "Reforestation field")

locColor <- c(rgb(126,160,78, maxColorValue = 255), # deciduous
              rgb(42,109,58, maxColorValue = 255), # mixed forest
              rgb(148,203,236, maxColorValue = 255), # monoculture
              rgb(217,148,40, maxColorValue = 255), # scrub
              rgb(216,192,138, maxColorValue = 255)) # reforestation

locColorst <- c(rgb(126,160,78,100, maxColorValue = 255), # deciduous
                rgb(42,109,58,100, maxColorValue = 255), # mixed forest
                rgb(148,203,236,100, maxColorValue = 255), # monoculture
                rgb(217,148,40,100, maxColorValue = 255), # scrub
                rgb(216,192,138,100, maxColorValue = 255)) # reforestation

loc1 <- soilMod %>% filter(locID==1)
ggplot(soilMod%>% filter(locID==5), aes(Tsurf_15, aveT, color=as.factor(snowID)))+
  geom_point()
####### Figure 1: Met and soil data ----
singleLoc <- soilDat %>%
  filter(location == "hemlock sapflow")

rainsnow <- singleLoc %>%
  filter(rain_snow == 1)

wd <- 55
hd <- 18

# x range
xl <- 2022.74
xh <- 2026.35
#y range for meteorological graph
#air temp and precipitation
yl <- -25
yh <- 30
#precip in mm
prMax <- 65

#surface temp
yl2 <- -25
yh2 <- 30
#snow depth max (cm)
snMax <- 65

yl3 <- -5
yh3 <- 25

yl4 <- 0.05
yh4 <- 0.65

#sizing for lines of graph
lw <- 4

# axes label sequences
yxAT <- seq(-15,25, by=10)
yxSuT <- seq(-15,25, by=10)
yxSoT <- seq(-5,25, by=5)
yxSW <- seq(0,0.6, by=0.1)
yxPR <- seq(0,60, by=10)
yxSN <- seq(0,600, by=100)
# axis sizing
cx_tick <- 4
# line for y axis tick labels
lyax <- 4
# line for first label 
lly1 <-18
lly2 <- 11
lly3 <- 14
lly4 <- 19
#sizing for y axis tick labels
cll <- 3
labll <- 4

monthseq <- c(1,32,60,91,121,152,182,213,244,274,305,335)
monthseqL <- c(1,32,61,92,122,153,183,214,245,275,306,336)
monthLab <- c("J","F","M","A","M","J","J","A","S","O","N","D")

months <- c(c(274,305,335),monthseq,monthseqL,  monthseq,c(1,32,60,91,121))
monthsLab <- c(c("O","N","D"),monthLab, monthLab,monthLab,c("J","F","M","A"))
years <- c(rep(2022,3), rep(2023,12),rep(2024,12),rep(2025,12),rep(2026,4))
monthseq <- ifelse(leap_year(years), (months-1)/366,(months-1)/365)
monthDD <- years+monthseq

precipRescale <- function(x,precipMax,ylf,yhf) {
  (((yhf-ylf)/precipMax)*x)+ylf
}
precipRescale(10,prMax,yl,yh)

# legend size 
lgcx = 4

# text 
xp <- 2022.8
# panel letter size
plcx <- 5




png(paste0(plotDir,"/daily_data.png"), width = 72, height = 70, units = "cm", res=300)
layout(matrix(c(1,2,3),ncol=1), width=lcm(wd),height=rep(lcm(hd),3))
#air temp and precip
par(mai=c(0.25,0,0,0))
plot(c(0,1),c(0,1), type="n", xlim=c(xl,xh), ylim=c(yl,yh), xaxs="i",yaxs="i",
     xlab= " ", ylab=" ", axes=FALSE)



for(i in 1:nrow(singleLoc)){
  polygon(c(singleLoc$DD[i]-0.001,singleLoc$DD[i]-0.001,
            singleLoc$DD[i]+0.001,singleLoc$DD[i]+0.001),
          c(precipRescale(0,snMax,yl2,yh2),
            precipRescale(singleLoc$SNWD[i]/10,snMax,yl2,yh2),
            precipRescale(singleLoc$SNWD[i]/10,snMax,yl2,yh2),
            precipRescale(0,snMax,yl2,yh2)),
          col="grey60", border=NA)
}
for(i in 1:nrow(singleLoc)){
  polygon(c(singleLoc$DD[i]-0.001,singleLoc$DD[i]-0.001,
            singleLoc$DD[i]+0.001,singleLoc$DD[i]+0.001),
          c(precipRescale(0,prMax,yl,yh),
            precipRescale(singleLoc$Precip_gap[i],prMax,yl,yh),
            precipRescale(singleLoc$Precip_gap[i],prMax,yl,yh),
            precipRescale(0,prMax,yl,yh)),
          col="#4169E199", border=NA)
}

points(singleLoc$DD, singleLoc$aveT, type="l", pch=19, lwd=lw )

axis(2, c(-30,yxAT,40), rep("", length(yxAT)+2), cex=cx_tick)
mtext(yxAT, side=2, at=yxAT, line = lyax, cex=cll, las=2)

axis(1, monthDD, rep("", length(monthDD)), cex=cx_tick)
mtext("Air temperature", side=2, line=lly1, cex=labll, )
mtext(expression(paste("(",degree,"C)")), side=2, line=lly2, cex=labll)


legend(2024.05,33, c("temperature", "snow depth", "precipitation"), col=c("black","grey75","#4169E199"), lwd=c(lw,NA,NA), pch=c(NA,15,15),
       bty="n", horiz=TRUE, cex=lgcx)
text(xp, 27, "A", cex=plcx)
axis(4,  precipRescale(yxPR,prMax,yl,yh), rep("", length(yxPR)), cex=cx_tick)
mtext(yxPR, side=4, at=precipRescale(yxPR,prMax,yl,yh), line = lyax, cex=cll, las=2)
mtext("Precipitation (mm)", side=4, line=lly3, cex=labll)
mtext("Snow depth (cm)", side=4, line=lly4, cex=labll)

# soil temp
par(mai=c(0.25,0,0,0))
plot(c(0,1),c(0,1), type="n", xlim=c(xl,xh), ylim=c(yl3,yh3), xaxs="i",yaxs="i",
     xlab= " ", ylab=" ", axes=FALSE)
abline(h=0)


for(i in 1:5){
  points(soilDat$DD[soilDat$locID==i],soilDat$Tsoil_6[soilDat$locID==i],
         type="l", col=locColor[i], lwd=lw)
}
axis(2, c(-30,yxSoT,40), rep("", length(yxSoT)+2), cex=cx_tick)
mtext(yxSoT, side=2, at=yxSoT, line = lyax, cex=cll, las=2)
axis(1, monthDD, rep("", length(monthDD)), cex=cx_tick)
mtext("Soil temperature", side=2, line=lly1, cex=labll)
mtext(expression(paste("(", degree,"C)")), side=2, line=lly2, cex=labll)
text(xp, 23, "B", cex=plcx)
#soil moisture

par(mai=c(0.25,0,0,0))
plot(c(0,1),c(0,1), type="n", xlim=c(xl,xh), ylim=c(yl4,yh4), xaxs="i",yaxs="i",
     xlab= " ", ylab=" ", axes=FALSE)

legend(2022.9,0.66, locLabel[1:2], col=locColor[1:2], lty=1, lwd=lw, bty="n", cex=lgcx)
legend(2023.95,0.66, locLabel[3:4], col=locColor[3:4], lty=1, lwd=lw, bty="n", cex=lgcx)
legend(2024.95,0.66, locLabel[5], col=locColor[5], lty=1, lwd=lw, bty="n", cex=lgcx)
for(i in 1:5){
  points(soilDat$DD[soilDat$locID==i],soilDat$SWC_12[soilDat$locID==i],
         type="l", col=locColor[i], lwd=lw )
}
axis(1, monthDD, rep("", length(monthDD)), cex=cx_tick)
mtext(monthsLab, side=1, at=monthDD, line = lyax, cex=cll)
mtext(seq(2023,2026), side=1, at=seq(2023,2026), line = lyax+5, cex=cll+1, adj=0)

axis(2, c(-1,yxSW,1), rep("", length(yxSW)+2), cex=cx_tick)
mtext(yxSW, side=2, at=yxSW, line = lyax, cex=cll, las=2)
mtext("Soil moisture", side=2, line=lly1, cex=labll)
mtext(expression(paste("(m"^3,"m"^-3,")")), side=2, line=lly2, cex=labll)
text(xp, 0.6, "C", cex=plcx)
dev.off()




####### Figure 1: Cold season Met and soil data ----
singleLoc <- soilDat %>%
  filter(location == "hemlock sapflow")

rainsnow <- singleLoc %>%
  filter(rain_snow == 1)

# create a water year index
# 2024 leap year
soilDat$wy_doy <- ifelse(leap_year(soilDat$year)==TRUE & soilDat$doy>= 275,soilDat$doy-274,
                   ifelse(leap_year(soilDat$year)==FALSE & soilDat$doy>= 274,soilDat$doy-273,
                      ifelse(leap_year(soilDat$year)==TRUE & soilDat$doy< 275,soilDat$doy+92,
                             ifelse(leap_year(soilDat$year)==FALSE & soilDat$doy< 274,soilDat$doy+92,NA ))))
soilDat$wy_year <- ifelse(leap_year(soilDat$year)==TRUE & soilDat$doy>= 275,soilDat$year+1,
                       ifelse(leap_year(soilDat$year)==FALSE & soilDat$doy>= 274,soilDat$year+1,soilDat$year))
soilDat$wyDD <- ifelse(leap_year(soilDat$year)==TRUE,soilDat$wy_year+((soilDat$wy_doy-1)/366), soilDat$wy_year+((soilDat$wy_doy-1)/365))



wd <- 12
hd <- 18
# Oct 2022 - April 2023,
# Oct 2023- April 2024
# Oct 2024- April 2025
# Oct 2025 - April 2026

# x range
xl1 <- 2023.0
xh1 <- 2023.578

xl2 <- 2024.0
xh2 <- 2024.578

xl3 <- 2025.0
xh3 <- 2025.578

xl4 <- 2026.0
xh4 <- 2026.578


#y range for meteorological graph
#air temp and precipitation
yl <- -25
yh <- 30
#precip in mm
prMax <- 65


#snow depth max (cm)
snMax <- 65

yl3 <- -5
yh3 <- 16

yl4 <- 0.05
yh4 <- 0.55

#sizing for lines of graph
lw <- 4

# axes label sequences
yxAT <- seq(-15,25, by=10)
yxSuT <- seq(-15,25, by=10)
yxSoT <- seq(-5,15, by=5)
yxSW <- seq(0,0.5, by=0.1)
yxPR <- seq(0,60, by=10)
yxSN <- seq(0,600, by=100)
# axis sizing
cx_tick <- 4
# line for y axis tick labels
lyax <- 4
# line for first label 
lly1 <-18
lly2 <- 11
lly3 <- 14
lly4 <- 19
#sizing for y axis tick labels
cll <- 3
labll <- 4

monthseq <- c(1,32,62,93,124,152,183)
monthseqL <- c(1,32,62,93,124,153,184)
monthsLab <- c("O","N","D","J","F","M","A")

monthseqDD <- monthseq/365
monthseqLDD <- monthseq/366

precipRescale <- function(x,precipMax,ylf,yhf) {
  (((yhf-ylf)/precipMax)*x)+ylf
}
precipRescale(10,prMax,yl,yh)

# legend size 
lgcx = 3.85
#legend coordinates
xo <- 0.05
yleg1 <- 32
# legend offset for forest types
xof <- -0.01
yleg2 <- -1


# text 
xp <- 2023.05
xp2 <- 2024.05
xp3 <- 2025.05
xp4 <- 2026.05
# panel letter size
plcx <- 5




png(paste0(plotDir,"/daily_data_cold.png"), width = 72, height = 70, units = "cm", res=300)
layout(matrix(seq(1,12),ncol=4,byrow=FALSE), width=rep(lcm(wd),4),height=rep(lcm(hd),3))

### Water year 2023 (Oct 2022-Apr 2023)
#air temp and precip
monthDD <- 2023+monthseqDD
par(mai=c(0.25,0.25,0.25,0.25))
plot(c(0,1),c(0,1), type="n", xlim=c(xl1,xh1), ylim=c(yl,yh), xaxs="i",yaxs="i",
     xlab= " ", ylab=" ", axes=FALSE)



for(i in 1:nrow(singleLoc)){
  polygon(c(singleLoc$wyDD[i]-0.001,singleLoc$wyDD[i]-0.001,
            singleLoc$wyDD[i]+0.001,singleLoc$wyDD[i]+0.001),
          c(precipRescale(0,snMax,yl2,yh2),
            precipRescale(singleLoc$SNWD[i]/10,snMax,yl2,yh2),
            precipRescale(singleLoc$SNWD[i]/10,snMax,yl2,yh2),
            precipRescale(0,snMax,yl2,yh2)),
          col="grey60", border=NA)
}
for(i in 1:nrow(singleLoc)){
  polygon(c(singleLoc$wyDD[i]-0.001,singleLoc$wyDD[i]-0.001,
            singleLoc$wyDD[i]+0.001,singleLoc$wyDD[i]+0.001),
          c(precipRescale(0,prMax,yl,yh),
            precipRescale(singleLoc$Precip_gap[i],prMax,yl,yh),
            precipRescale(singleLoc$Precip_gap[i],prMax,yl,yh),
            precipRescale(0,prMax,yl,yh)),
          col="#4169E199", border=NA)
}

points(singleLoc$wyDD, singleLoc$aveT, type="l", pch=19, lwd=lw )

axis(2, c(-30,yxAT,40), rep("", length(yxAT)+2), cex=cx_tick)
mtext(yxAT, side=2, at=yxAT, line = lyax, cex=cll, las=2)

axis(1, monthDD, rep("", length(monthDD)), cex=cx_tick)
mtext("Air temperature", side=2, line=lly1, cex=labll, )
mtext(expression(paste("(",degree,"C)")), side=2, line=lly2, cex=labll)


legend(2023+xo,yleg1, c("temperature"), col=c("black"), lwd=c(lw), 
       bty="n", cex=lgcx)
text(xp, 27, "A", cex=plcx)
axis(4,  precipRescale(yxPR,prMax,yl,yh), rep("", length(yxPR)), cex=cx_tick)


# soil temp
par(mai=c(0.25,0.25,0.25,0.25))
plot(c(0,1),c(0,1), type="n", xlim=c(xl1,xh1), ylim=c(yl3,yh3), xaxs="i",yaxs="i",
     xlab= " ", ylab=" ", axes=FALSE)
abline(h=0)


for(i in 1:5){
  points(soilDat$wyDD[soilDat$locID==i],soilDat$Tsoil_6[soilDat$locID==i],
         type="l", col=locColor[i], lwd=lw)
}
axis(2, c(-30,yxSoT,40), rep("", length(yxSoT)+2), cex=cx_tick)
mtext(yxSoT, side=2, at=yxSoT, line = lyax, cex=cll, las=2)
axis(1, monthDD, rep("", length(monthDD)), cex=cx_tick)
mtext("Soil temperature", side=2, line=lly1, cex=labll)
mtext(expression(paste("(", degree,"C)")), side=2, line=lly2, cex=labll)
text(xp, 15, "B", cex=plcx)

legend(2023+xof,yleg2, c("decid. forest", "mixed forest"), col=locColor[1:2], lty=1, lwd=lw, bty="n", cex=lgcx)
#legend(2023.95,0.66, locLabel[3:4], col=locColor[3:4], lty=1, lwd=lw, bty="n", cex=lgcx)
#legend(2024.95,0.66, locLabel[5], col=locColor[5], lty=1, lwd=lw, bty="n", cex=lgcx)

#soil moisture

par(mai=c(0.25,0.25,0.25,0.25))
plot(c(0,1),c(0,1), type="n", xlim=c(xl1,xh1), ylim=c(yl4,yh4), xaxs="i",yaxs="i",
     xlab= " ", ylab=" ", axes=FALSE)

#legend(2022.9,0.66, locLabel[1:2], col=locColor[1:2], lty=1, lwd=lw, bty="n", cex=lgcx)
#legend(2023.95,0.66, locLabel[3:4], col=locColor[3:4], lty=1, lwd=lw, bty="n", cex=lgcx)
#legend(2024.95,0.66, locLabel[5], col=locColor[5], lty=1, lwd=lw, bty="n", cex=lgcx)
for(i in 1:5){
  points(soilDat$wyDD[soilDat$locID==i],soilDat$SWC_12[soilDat$locID==i],
         type="l", col=locColor[i], lwd=lw )
}
axis(1, monthDD, rep("", length(monthDD)), cex=cx_tick)
mtext(monthsLab, side=1, at=monthDD, line = lyax, cex=cll)


axis(2, c(-1,yxSW,1), rep("", length(yxSW)+2), cex=cx_tick)
mtext(yxSW, side=2, at=yxSW, line = lyax, cex=cll, las=2)
mtext("Soil moisture", side=2, line=lly1, cex=labll)
mtext(expression(paste("(m"^3,"m"^-3,")")), side=2, line=lly2, cex=labll)
text(xp, 0.5, "C", cex=plcx)


### Water year 2024 (Oct 2023-Apr 2024)
#air temp and precip
monthDD <- 2024+monthseqDD
par(mai=c(0.25,0.25,0.25,0.25))
plot(c(0,1),c(0,1), type="n", xlim=c(xl2,xh2), ylim=c(yl,yh), xaxs="i",yaxs="i",
     xlab= " ", ylab=" ", axes=FALSE)



for(i in 1:nrow(singleLoc)){
  polygon(c(singleLoc$wyDD[i]-0.001,singleLoc$wyDD[i]-0.001,
            singleLoc$wyDD[i]+0.001,singleLoc$wyDD[i]+0.001),
          c(precipRescale(0,snMax,yl2,yh2),
            precipRescale(singleLoc$SNWD[i]/10,snMax,yl2,yh2),
            precipRescale(singleLoc$SNWD[i]/10,snMax,yl2,yh2),
            precipRescale(0,snMax,yl2,yh2)),
          col="grey60", border=NA)
}
for(i in 1:nrow(singleLoc)){
  polygon(c(singleLoc$wyDD[i]-0.001,singleLoc$wyDD[i]-0.001,
            singleLoc$wyDD[i]+0.001,singleLoc$wyDD[i]+0.001),
          c(precipRescale(0,prMax,yl,yh),
            precipRescale(singleLoc$Precip_gap[i],prMax,yl,yh),
            precipRescale(singleLoc$Precip_gap[i],prMax,yl,yh),
            precipRescale(0,prMax,yl,yh)),
          col="#4169E199", border=NA)
}

points(singleLoc$wyDD, singleLoc$aveT, type="l", pch=19, lwd=lw )

axis(2, c(-30,yxAT,40), rep("", length(yxAT)+2), cex=cx_tick)


axis(1, monthDD, rep("", length(monthDD)), cex=cx_tick)

legend(2024+xo,yleg1, c( "precipitation"), col=c("#4169E199"), pch=c(15),
       bty="n", cex=lgcx)

#legend(2023.05,33, c("temperature", "snow depth", "precipitation"), col=c("black","grey75","#4169E199"), lwd=c(lw,NA,NA), pch=c(NA,15,15),
#       bty="n", horiz=TRUE, cex=lgcx)
text(xp2, 27, "D", cex=plcx)


# soil temp
par(mai=c(0.25,0.25,0.25,0.25))
plot(c(0,1),c(0,1), type="n", xlim=c(xl2,xh2), ylim=c(yl3,yh3), xaxs="i",yaxs="i",
     xlab= " ", ylab=" ", axes=FALSE)
abline(h=0)


for(i in 1:5){
  points(soilDat$wyDD[soilDat$locID==i],soilDat$Tsoil_6[soilDat$locID==i],
         type="l", col=locColor[i], lwd=lw)
}
axis(2, c(-30,yxSoT,40), rep("", length(yxSoT)+2), cex=cx_tick)

axis(1, monthDD, rep("", length(monthDD)), cex=cx_tick)

text(xp2, 15, "E", cex=plcx)

legend(2024+xof,yleg2, c("conifer","scrub"), col=locColor[3:4], lty=1, lwd=lw, bty="n", cex=lgcx)

#soil moisture

par(mai=c(0.25,0.25,0.25,0.25))
plot(c(0,1),c(0,1), type="n", xlim=c(xl2,xh2), ylim=c(yl4,yh4), xaxs="i",yaxs="i",
     xlab= " ", ylab=" ", axes=FALSE)

#legend(2022.9,0.66, locLabel[1:2], col=locColor[1:2], lty=1, lwd=lw, bty="n", cex=lgcx)
#legend(2023.95,0.66, locLabel[3:4], col=locColor[3:4], lty=1, lwd=lw, bty="n", cex=lgcx)
#legend(2024.95,0.66, locLabel[5], col=locColor[5], lty=1, lwd=lw, bty="n", cex=lgcx)
for(i in 1:5){
  points(soilDat$wyDD[soilDat$locID==i],soilDat$SWC_12[soilDat$locID==i],
         type="l", col=locColor[i], lwd=lw )
}
axis(1, monthDD, rep("", length(monthDD)), cex=cx_tick)
mtext(monthsLab, side=1, at=monthDD, line = lyax, cex=cll)


axis(2, c(-1,yxSW,1), rep("", length(yxSW)+2), cex=cx_tick)

text(xp2, 0.5, "F", cex=plcx)



### Water year 2025 (Oct 2024-Apr 2025)
#air temp and precip
monthDD <- 2025+monthseqDD
par(mai=c(0.25,0.25,0.25,0.25))
plot(c(0,1),c(0,1), type="n", xlim=c(xl3,xh3), ylim=c(yl,yh), xaxs="i",yaxs="i",
     xlab= " ", ylab=" ", axes=FALSE)



for(i in 1:nrow(singleLoc)){
  polygon(c(singleLoc$wyDD[i]-0.001,singleLoc$wyDD[i]-0.001,
            singleLoc$wyDD[i]+0.001,singleLoc$wyDD[i]+0.001),
          c(precipRescale(0,snMax,yl2,yh2),
            precipRescale(singleLoc$SNWD[i]/10,snMax,yl2,yh2),
            precipRescale(singleLoc$SNWD[i]/10,snMax,yl2,yh2),
            precipRescale(0,snMax,yl2,yh2)),
          col="grey60", border=NA)
}
for(i in 1:nrow(singleLoc)){
  polygon(c(singleLoc$wyDD[i]-0.001,singleLoc$wyDD[i]-0.001,
            singleLoc$wyDD[i]+0.001,singleLoc$wyDD[i]+0.001),
          c(precipRescale(0,prMax,yl,yh),
            precipRescale(singleLoc$Precip_gap[i],prMax,yl,yh),
            precipRescale(singleLoc$Precip_gap[i],prMax,yl,yh),
            precipRescale(0,prMax,yl,yh)),
          col="#4169E199", border=NA)
}

points(singleLoc$wyDD, singleLoc$aveT, type="l", pch=19, lwd=lw )

axis(2, c(-30,yxAT,40), rep("", length(yxAT)+2), cex=cx_tick)
legend(2025+xo,yleg1, c( "snow depth"), col=c("grey75"), pch=c(15),
       bty="n", cex=lgcx)

axis(1, monthDD, rep("", length(monthDD)), cex=cx_tick)



#legend(2023.05,33, c("temperature", "snow depth", "precipitation"), col=c("black","grey75","#4169E199"), lwd=c(lw,NA,NA), pch=c(NA,15,15),
#       bty="n", horiz=TRUE, cex=lgcx)
text(xp3, 27, "G", cex=plcx)
axis(4,  precipRescale(yxPR,prMax,yl,yh), rep("", length(yxPR)), cex=cx_tick)


# soil temp
par(mai=c(0.25,0.25,0.25,0.25))
plot(c(0,1),c(0,1), type="n", xlim=c(xl3,xh3), ylim=c(yl3,yh3), xaxs="i",yaxs="i",
     xlab= " ", ylab=" ", axes=FALSE)
abline(h=0)


for(i in 1:5){
  points(soilDat$wyDD[soilDat$locID==i],soilDat$Tsoil_6[soilDat$locID==i],
         type="l", col=locColor[i], lwd=lw)
}
axis(2, c(-30,yxSoT,40), rep("", length(yxSoT)+2), cex=cx_tick)

axis(1, monthDD, rep("", length(monthDD)), cex=cx_tick)

text(xp3, 15, "H", cex=plcx)

legend(2025+xof,yleg2, "meadow", col=locColor[5], lty=1, lwd=lw, bty="n", cex=lgcx)
#soil moisture

par(mai=c(0.25,0.25,0.25,0.25))
plot(c(0,1),c(0,1), type="n", xlim=c(xl3,xh3), ylim=c(yl4,yh4), xaxs="i",yaxs="i",
     xlab= " ", ylab=" ", axes=FALSE)

#legend(2022.9,0.66, locLabel[1:2], col=locColor[1:2], lty=1, lwd=lw, bty="n", cex=lgcx)
#legend(2023.95,0.66, locLabel[3:4], col=locColor[3:4], lty=1, lwd=lw, bty="n", cex=lgcx)
#legend(2024.95,0.66, locLabel[5], col=locColor[5], lty=1, lwd=lw, bty="n", cex=lgcx)
for(i in 1:5){
  points(soilDat$wyDD[soilDat$locID==i],soilDat$SWC_12[soilDat$locID==i],
         type="l", col=locColor[i], lwd=lw )
}
axis(1, monthDD, rep("", length(monthDD)), cex=cx_tick)
mtext(monthsLab, side=1, at=monthDD, line = lyax, cex=cll)


axis(2, c(-1,yxSW,1), rep("", length(yxSW)+2), cex=cx_tick)

text(xp3, 0.5, "I", cex=plcx)



### Water year 2026 (Oct 2025-Apr 2026)
#air temp and precip
monthDD <- 2026+monthseqDD
par(mai=c(0.25,0.25,0.25,0.25))
plot(c(0,1),c(0,1), type="n", xlim=c(xl4,xh4), ylim=c(yl,yh), xaxs="i",yaxs="i",
     xlab= " ", ylab=" ", axes=FALSE)



for(i in 1:nrow(singleLoc)){
  polygon(c(singleLoc$wyDD[i]-0.001,singleLoc$wyDD[i]-0.001,
            singleLoc$wyDD[i]+0.001,singleLoc$wyDD[i]+0.001),
          c(precipRescale(0,snMax,yl2,yh2),
            precipRescale(singleLoc$SNWD[i]/10,snMax,yl2,yh2),
            precipRescale(singleLoc$SNWD[i]/10,snMax,yl2,yh2),
            precipRescale(0,snMax,yl2,yh2)),
          col="grey60", border=NA)
}
for(i in 1:nrow(singleLoc)){
  polygon(c(singleLoc$wyDD[i]-0.001,singleLoc$wyDD[i]-0.001,
            singleLoc$wyDD[i]+0.001,singleLoc$wyDD[i]+0.001),
          c(precipRescale(0,prMax,yl,yh),
            precipRescale(singleLoc$Precip_gap[i],prMax,yl,yh),
            precipRescale(singleLoc$Precip_gap[i],prMax,yl,yh),
            precipRescale(0,prMax,yl,yh)),
          col="#4169E199", border=NA)
}

points(singleLoc$wyDD, singleLoc$aveT, type="l", pch=19, lwd=lw )

axis(2, c(-30,yxAT,40), rep("", length(yxAT)+2), cex=cx_tick)


axis(1, monthDD, rep("", length(monthDD)), cex=cx_tick)



#legend(2023.05,33, c("temperature", "snow depth", "precipitation"), col=c("black","grey75","#4169E199"), lwd=c(lw,NA,NA), pch=c(NA,15,15),
#       bty="n", horiz=TRUE, cex=lgcx)
text(xp4, 27, "J", cex=plcx)
axis(4,  precipRescale(yxPR,prMax,yl,yh), rep("", length(yxPR)), cex=cx_tick)
mtext(yxPR, side=4, at=precipRescale(yxPR,prMax,yl,yh), line = lyax, cex=cll, las=2)
mtext("Precipitation (mm)", side=4, line=lly3, cex=labll)
mtext("Snow depth (cm)", side=4, line=lly4, cex=labll)

# soil temp
par(mai=c(0.25,0.25,0.25,0.25))
plot(c(0,1),c(0,1), type="n", xlim=c(xl4,xh4), ylim=c(yl3,yh3), xaxs="i",yaxs="i",
     xlab= " ", ylab=" ", axes=FALSE)
abline(h=0)


for(i in 1:5){
  points(soilDat$wyDD[soilDat$locID==i],soilDat$Tsoil_6[soilDat$locID==i],
         type="l", col=locColor[i], lwd=lw)
}
axis(2, c(-30,yxSoT,40), rep("", length(yxSoT)+2), cex=cx_tick)

axis(1, monthDD, rep("", length(monthDD)), cex=cx_tick)

text(xp4, 15, "K", cex=plcx)
#soil moisture

par(mai=c(0.25,0.25,0.25,0.25))
plot(c(0,1),c(0,1), type="n", xlim=c(xl4,xh4), ylim=c(yl4,yh4), xaxs="i",yaxs="i",
     xlab= " ", ylab=" ", axes=FALSE)

#legend(2022.9,0.66, locLabel[1:2], col=locColor[1:2], lty=1, lwd=lw, bty="n", cex=lgcx)
#legend(2023.95,0.66, locLabel[3:4], col=locColor[3:4], lty=1, lwd=lw, bty="n", cex=lgcx)
#legend(2024.95,0.66, locLabel[5], col=locColor[5], lty=1, lwd=lw, bty="n", cex=lgcx)
for(i in 1:5){
  points(soilDat$wyDD[soilDat$locID==i],soilDat$SWC_12[soilDat$locID==i],
         type="l", col=locColor[i], lwd=lw )
}
axis(1, monthDD, rep("", length(monthDD)), cex=cx_tick)
mtext(monthsLab, side=1, at=monthDD, line = lyax, cex=cll)

axis(2, c(-1,yxSW,1), rep("", length(yxSW)+2), cex=cx_tick)

text(xp4, 0.5, "L", cex=plcx)


dev.off()




####### Figure 2: Warm season Met and soil data ----
singleLoc <- soilDat %>%
  filter(location == "hemlock sapflow")

rainsnow <- singleLoc %>%
  filter(rain_snow == 1)



wd <- 12
hd <- 18


# x range
xl1 <- 2023.3315
xh1 <- 2023.74794

xl2 <- 2024.333
xh2 <- 2024.578

xl3 <- 2025.3315
xh3 <- 2025.7486



#y range for meteorological graph
#air temp and precipitation
yl <- 0
yh <- 30
#precip in mm
prMax <- 65


yl3 <- 5
yh3 <- 25

yl4 <- 0.05
yh4 <- 0.55

#sizing for lines of graph
lw <- 4

# axes label sequences
yxAT <- seq(-15,25, by=10)
yxSuT <- seq(-15,25, by=10)
yxSoT <- seq(-5,15, by=5)
yxSW <- seq(0,0.5, by=0.1)
yxPR <- seq(0,60, by=10)
yxSN <- seq(0,600, by=100)
# axis sizing
cx_tick <- 4
# line for y axis tick labels
lyax <- 4
# line for first label 
lly1 <-18
lly2 <- 11
lly3 <- 14
lly4 <- 19
#sizing for y axis tick labels
cll <- 3
labll <- 4

monthseq <- c(121,152,182,213,244)
monthseqL <- c(122,153,183,214,245)
monthsLab <- c("M","J","J","A","S")

monthseqDD <- monthseq/365
monthDDL <- monthseq/366

precipRescale <- function(x,precipMax,ylf,yhf) {
  (((yhf-ylf)/precipMax)*x)+ylf
}
precipRescale(10,prMax,yl,yh)

# legend size 
lgcx = 3.85
#legend coordinates
xo <- 0.38
yleg1 <- 32
# legend offset for forest types
xof <- -0.01
yleg2 <- -1


# text 
xp <- 2023.05
xp2 <- 2024.05
xp3 <- 2025.05
xp4 <- 2026.05
# panel letter size
plcx <- 5




png(paste0(plotDir,"/daily_data_warm.png"), width = 72, height = 70, units = "cm", res=300)
layout(matrix(seq(1,12),ncol=4,byrow=FALSE), width=rep(lcm(wd),4),height=rep(lcm(hd),3))

### Water year 2023 (Oct 2022-Apr 2023)
#air temp and precip
monthDD <- 2023+monthseqDD
par(mai=c(0.25,0.25,0.25,0.25))
plot(c(0,1),c(0,1), type="n", xlim=c(xl1,xh1), ylim=c(yl,yh), xaxs="i",yaxs="i",
     xlab= " ", ylab=" ", axes=FALSE)


for(i in 1:nrow(singleLoc)){
  polygon(c(singleLoc$DD[i]-0.001,singleLoc$DD[i]-0.001,
            singleLoc$DD[i]+0.001,singleLoc$DD[i]+0.001),
          c(precipRescale(0,prMax,yl,yh),
            precipRescale(singleLoc$Precip_gap[i],prMax,yl,yh),
            precipRescale(singleLoc$Precip_gap[i],prMax,yl,yh),
            precipRescale(0,prMax,yl,yh)),
          col="#4169E199", border=NA)
}

points(singleLoc$DD, singleLoc$aveT, type="l", pch=19, lwd=lw )

axis(2, c(-30,yxAT,40), rep("", length(yxAT)+2), cex=cx_tick)
mtext(yxAT, side=2, at=yxAT, line = lyax, cex=cll, las=2)

axis(1, monthDD, rep("", length(monthDD)), cex=cx_tick)
mtext("Air temperature", side=2, line=lly1, cex=labll, )
mtext(expression(paste("(",degree,"C)")), side=2, line=lly2, cex=labll)


legend(2023+xo,yleg1, c("temperature"), col=c("black"), lwd=c(lw), 
       bty="n", cex=lgcx)
text(xp, 27, "A", cex=plcx)
axis(4,  precipRescale(yxPR,prMax,yl,yh), rep("", length(yxPR)), cex=cx_tick)


# soil temp
par(mai=c(0.25,0.25,0.25,0.25))
plot(c(0,1),c(0,1), type="n", xlim=c(xl1,xh1), ylim=c(yl3,yh3), xaxs="i",yaxs="i",
     xlab= " ", ylab=" ", axes=FALSE)



for(i in 1:5){
  points(soilDat$DD[soilDat$locID==i],soilDat$Tsoil_6[soilDat$locID==i],
         type="l", col=locColor[i], lwd=lw)
}
axis(2, c(-30,yxSoT,40), rep("", length(yxSoT)+2), cex=cx_tick)
mtext(yxSoT, side=2, at=yxSoT, line = lyax, cex=cll, las=2)
axis(1, monthDD, rep("", length(monthDD)), cex=cx_tick)
mtext("Soil temperature", side=2, line=lly1, cex=labll)
mtext(expression(paste("(", degree,"C)")), side=2, line=lly2, cex=labll)
text(xp, 15, "B", cex=plcx)

legend(2023+xof,yleg2, c("decid. forest", "mixed forest"), col=locColor[1:2], lty=1, lwd=lw, bty="n", cex=lgcx)


#soil moisture

par(mai=c(0.25,0.25,0.25,0.25))
plot(c(0,1),c(0,1), type="n", xlim=c(xl1,xh1), ylim=c(yl4,yh4), xaxs="i",yaxs="i",
     xlab= " ", ylab=" ", axes=FALSE)

for(i in 1:5){
  points(soilDat$DD[soilDat$locID==i],soilDat$SWC_12[soilDat$locID==i],
         type="l", col=locColor[i], lwd=lw )
}
axis(1, monthDD, rep("", length(monthDD)), cex=cx_tick)
mtext(monthsLab, side=1, at=monthDD, line = lyax, cex=cll)


axis(2, c(-1,yxSW,1), rep("", length(yxSW)+2), cex=cx_tick)
mtext(yxSW, side=2, at=yxSW, line = lyax, cex=cll, las=2)
mtext("Soil moisture", side=2, line=lly1, cex=labll)
mtext(expression(paste("(m"^3,"m"^-3,")")), side=2, line=lly2, cex=labll)
text(xp, 0.5, "C", cex=plcx)


### W year 2024 (May 2024-Sept 2024)
#air temp and precip
monthDD <- 2024+monthseqLDD
par(mai=c(0.25,0.25,0.25,0.25))
plot(c(0,1),c(0,1), type="n", xlim=c(xl2,xh2), ylim=c(yl,yh), xaxs="i",yaxs="i",
     xlab= " ", ylab=" ", axes=FALSE)


for(i in 1:nrow(singleLoc)){
  polygon(c(singleLoc$DD[i]-0.001,singleLoc$DD[i]-0.001,
            singleLoc$DD[i]+0.001,singleLoc$DD[i]+0.001),
          c(precipRescale(0,prMax,yl,yh),
            precipRescale(singleLoc$Precip_gap[i],prMax,yl,yh),
            precipRescale(singleLoc$Precip_gap[i],prMax,yl,yh),
            precipRescale(0,prMax,yl,yh)),
          col="#4169E199", border=NA)
}

points(singleLoc$DD, singleLoc$aveT, type="l", pch=19, lwd=lw )

axis(2, c(-30,yxAT,40), rep("", length(yxAT)+2), cex=cx_tick)


axis(1, monthDDL, rep("", length(monthDDL)), cex=cx_tick)

legend(2024+xo,yleg1, c( "precipitation"), col=c("#4169E199"), pch=c(15),
       bty="n", cex=lgcx)

text(xp2, 27, "D", cex=plcx)


# soil temp
par(mai=c(0.25,0.25,0.25,0.25))
plot(c(0,1),c(0,1), type="n", xlim=c(xl2,xh2), ylim=c(yl3,yh3), xaxs="i",yaxs="i",
     xlab= " ", ylab=" ", axes=FALSE)



for(i in 1:5){
  points(soilDat$DD[soilDat$locID==i],soilDat$Tsoil_6[soilDat$locID==i],
         type="l", col=locColor[i], lwd=lw)
}
axis(2, c(-30,yxSoT,40), rep("", length(yxSoT)+2), cex=cx_tick)

axis(1, monthDDL, rep("", length(monthDDL)), cex=cx_tick)

text(xp2, 15, "E", cex=plcx)

legend(2024+xof,yleg2, c("conifer","scrub"), col=locColor[3:4], lty=1, lwd=lw, bty="n", cex=lgcx)

#soil moisture

par(mai=c(0.25,0.25,0.25,0.25))
plot(c(0,1),c(0,1), type="n", xlim=c(xl2,xh2), ylim=c(yl4,yh4), xaxs="i",yaxs="i",
     xlab= " ", ylab=" ", axes=FALSE)

#legend(2022.9,0.66, locLabel[1:2], col=locColor[1:2], lty=1, lwd=lw, bty="n", cex=lgcx)
#legend(2023.95,0.66, locLabel[3:4], col=locColor[3:4], lty=1, lwd=lw, bty="n", cex=lgcx)
#legend(2024.95,0.66, locLabel[5], col=locColor[5], lty=1, lwd=lw, bty="n", cex=lgcx)
for(i in 1:5){
  points(soilDat$DD[soilDat$locID==i],soilDat$SWC_12[soilDat$locID==i],
         type="l", col=locColor[i], lwd=lw )
}
axis(1, monthDDL, rep("", length(monthDDL)), cex=cx_tick)
mtext(monthsLab, side=1, at=monthDDL, line = lyax, cex=cll)


axis(2, c(-1,yxSW,1), rep("", length(yxSW)+2), cex=cx_tick)

text(xp2, 0.5, "F", cex=plcx)


### Year 2025 (May 2025-Sept 2025)
#air temp and precip
monthDD <- 2025+monthseqDD
par(mai=c(0.25,0.25,0.25,0.25))
plot(c(0,1),c(0,1), type="n", xlim=c(xl3,xh3), ylim=c(yl,yh), xaxs="i",yaxs="i",
     xlab= " ", ylab=" ", axes=FALSE)


for(i in 1:nrow(singleLoc)){
  polygon(c(singleLoc$DD[i]-0.001,singleLoc$DD[i]-0.001,
            singleLoc$DD[i]+0.001,singleLoc$DD[i]+0.001),
          c(precipRescale(0,prMax,yl,yh),
            precipRescale(singleLoc$Precip_gap[i],prMax,yl,yh),
            precipRescale(singleLoc$Precip_gap[i],prMax,yl,yh),
            precipRescale(0,prMax,yl,yh)),
          col="#4169E199", border=NA)
}

points(singleLoc$DD, singleLoc$aveT, type="l", pch=19, lwd=lw )

axis(2, c(-30,yxAT,40), rep("", length(yxAT)+2), cex=cx_tick)

axis(1, monthDD, rep("", length(monthDD)), cex=cx_tick)

axis(4,  precipRescale(yxPR,prMax,yl,yh), rep("", length(yxPR)), cex=cx_tick)
mtext(yxPR, side=4, at=precipRescale(yxPR,prMax,yl,yh), line = lyax, cex=cll, las=2)
mtext("Precipitation (mm)", side=4, line=lly3, cex=labll)

#legend(2023.05,33, c("temperature", "snow depth", "precipitation"), col=c("black","grey75","#4169E199"), lwd=c(lw,NA,NA), pch=c(NA,15,15),
#       bty="n", horiz=TRUE, cex=lgcx)
text(xp3, 27, "G", cex=plcx)
axis(4,  precipRescale(yxPR,prMax,yl,yh), rep("", length(yxPR)), cex=cx_tick)


# soil temp
par(mai=c(0.25,0.25,0.25,0.25))
plot(c(0,1),c(0,1), type="n", xlim=c(xl3,xh3), ylim=c(yl3,yh3), xaxs="i",yaxs="i",
     xlab= " ", ylab=" ", axes=FALSE)



for(i in 1:5){
  points(soilDat$DD[soilDat$locID==i],soilDat$Tsoil_6[soilDat$locID==i],
         type="l", col=locColor[i], lwd=lw)
}
axis(2, c(-30,yxSoT,40), rep("", length(yxSoT)+2), cex=cx_tick)

axis(1, monthDD, rep("", length(monthDD)), cex=cx_tick)

text(xp3, 15, "H", cex=plcx)

legend(2025+xof,yleg2, "meadow", col=locColor[5], lty=1, lwd=lw, bty="n", cex=lgcx)
#soil moisture

par(mai=c(0.25,0.25,0.25,0.25))
plot(c(0,1),c(0,1), type="n", xlim=c(xl3,xh3), ylim=c(yl4,yh4), xaxs="i",yaxs="i",
     xlab= " ", ylab=" ", axes=FALSE)

#legend(2022.9,0.66, locLabel[1:2], col=locColor[1:2], lty=1, lwd=lw, bty="n", cex=lgcx)
#legend(2023.95,0.66, locLabel[3:4], col=locColor[3:4], lty=1, lwd=lw, bty="n", cex=lgcx)
#legend(2024.95,0.66, locLabel[5], col=locColor[5], lty=1, lwd=lw, bty="n", cex=lgcx)
for(i in 1:5){
  points(soilDat$DD[soilDat$locID==i],soilDat$SWC_12[soilDat$locID==i],
         type="l", col=locColor[i], lwd=lw )
}
axis(1, monthDD, rep("", length(monthDD)), cex=cx_tick)
mtext(monthsLab, side=1, at=monthDD, line = lyax, cex=cll)


axis(2, c(-1,yxSW,1), rep("", length(yxSW)+2), cex=cx_tick)

text(xp3, 0.5, "I", cex=plcx)

dev.off()


### examples
####### Tables: Site data & temperature trends----


# calculate stand basal area and dominant species
forestInventory$tree_area.m2 <- (((forestInventory$DBH.cm / 2)^2) * pi/10000) 
FI <- forestInventory %>%
  filter(Dead == "N", DBH.cm >3 ) %>%
  filter(Species != "CEOR") %>%
  filter(Species != "VITIS") %>% # remove vines from dbh calcs
  group_by(Plot, Species) %>%
  summarise(totArea = sum(tree_area.m2,na.rm=TRUE),
            ncount = n(),
            aveDBH = mean(DBH.cm,na.rm=TRUE))

TotBA <- FI %>%
  group_by(Plot) %>%
  summarise(totBA = sum(totArea))
#forest plot area
PA_ha <- (15^2*pi)/10000          
TotBA$BA_m2_ha <- TotBA$totBA/PA_ha 

plotStudy <- c("RG01","RG25","RG09","RG03")

PlotTable <- TotBA %>%
  filter(Plot %in% plotStudy)



FITot <-  forestInventory %>%
  filter(Dead == "N", DBH.cm >3 ) %>%
  filter(Species != "CEOR") %>%
  filter(Species != "VITIS") %>% # remove vines from dbh calcs
  group_by(Plot) %>%
  summarise(totArea = sum(tree_area.m2,na.rm=TRUE),
            ncount = n(),
            aveDBH = mean(DBH.cm,na.rm=TRUE))

FIjoin <- left_join(FI,FITot, by="Plot")
FIjoin$PercBA <- (FIjoin$totArea.x/FIjoin$totArea.y)*100  


FItop <- FIjoin %>%
  filter(PercBA > 5)

PlotSpec <- FItop %>%
  select(Species,Plot, PercBA)

plotsI <- unique(PlotSpec$Plot)

PlotComp <- left_join(PlotSpec, SpeciesInfo, by=c("Species"="Code"))

# summarize soil measurements
# total sample size
AllSoil <- soilDat %>%
  group_by(location, year) %>%
  summarize(nObs = n(),
            doyYearS = min(doy),
            doyYearE=max(doy),
            minTemp = min(Tsoil_6),
            maxTemp=max(Tsoil_6),
            aveTemp=mean(Tsoil_6)) 
 
totObs <- AllSoil %>%
  group_by(location) %>%
  summarize(totObs = sum(nObs))

# total sample size
modnSoil <- soilMod %>%
  group_by(location) %>%
  summarize(nObs = n())


# freezing events
freezeEvent <- freezeCheck %>%
  group_by(freezeEvent, location,year)%>%
  summarize(minTemp =min(Tsoil_6),
            startdoy=min(doy),
            duration = n())

freezePlot <- freezeEvent %>%
  group_by(location, year) %>%
  summarize(nEvent = n(),
            max_duration = max(duration),
            minT = min(minTemp))

# temp summary for table

soilTable <- soilDat %>%
  group_by(location, year) %>%
  summarize(minT = round(min(Tsoil_6),1),
            maxT = round(max(Tsoil_6),1),
            aveT = round(mean(Tsoil_6),1))

weatherTable <- weatherDay %>%
  group_by(year) %>%
  summarize(nobs =n(),
            maxT = max(aveT,na.rm=TRUE),
            minT=min(aveT,na.rm=TRUE),
            ave=mean(aveT,na.rm=TRUE),
            totP=sum(Precip_gap,na.rm=TRUE),
            maxSnow=max(SNWD,na.rm=TRUE))


# evaluate air freezing events

weatherDay$freezeAir <- ifelse(weatherDay$aveT<0,1,0)

weatherDay <- weatherDay %>%
  arrange(year,doy)

freezeCheckA <- weatherDay %>%
  filter(freezeAir == 1)
# get all freeze events
freezeEventA <- c(1)
for(i in 2:nrow(freezeCheckA)){
  if(
     freezeCheckA$year[i-1] == freezeCheckA$year[i] & 
     freezeCheckA$doy[i-1] == (freezeCheckA$doy[i]-1)){
    freezeEventA[i] <- freezeEventA[i-1]
    
  }else{
    freezeEventA[i] <-freezeEventA[i-1]+1}
  
}
# label start of an event
freezeStartA <- c(1)
for(i in 2:nrow(freezeCheckA)){
  if(freezeEventA[i-1]!=freezeEventA[i]){
    freezeStartA[i] <- 1
  }else{
    freezeStartA[i] <- 0
  }
}

# get the day before the freeze
freezeLastDayA <-  freezeCheckA$doy[1]-1
for(i in 2:nrow(freezeCheckA)){
  if(freezeStartA[i] == 1){
    freezeLastDayA[i] <- freezeCheckA$doy[i]-1
  }else{
    freezeLastDayA[i] <- freezeLastDayA[i-1]
  }
}

freezeLastDayYearA <-  freezeCheckA$year[1]
for(i in 2:nrow(freezeCheckA)){
  if(freezeStartA[i] == 1){
    freezeLastDayYearA[i] <- freezeCheckA$year[i]
  }else{
    freezeLastDayYearA[i] <- freezeLastDayYearA[i-1]
  }
}

freezeCheckA$freezeStart <- freezeStartA

freezeCheckA$freezeEvent <- freezeEventA

freezeCheckA$freezeLastDayYear <- freezeLastDayYearA
freezeCheckA$freezeLastDay <- freezeLastDayA

freezeEventA <- freezeCheckA %>%
  group_by(freezeEvent, year)%>%
  summarize(minTemp =min(aveT),
            startdoy=min(doy),
            duration = n())

freezePlotA <- freezeEventA %>%
  group_by(year) %>%
  summarize(nEvent = n(),
            max_duration = max(duration),
            minT = min(minTemp))
  
####### Figure : temperature model and patterns ----
#index order: forest, swID, snowID
#snowID: 1= no/low snow
#swID 1= at or below fc, 2= above fc
beta_n <- read.csv(paste0(modDir, "/beta_n_out.csv"))
beta_n$locID <- rep(seq(1,5), times=4)
beta_n$swID <- rep(c(1,2,1,2), each=5)
beta_n$snowID <- rep(c(1,2),each=10)
# regID 1=freezing temps low/no snow, 2=above freezing temps low/now snow, 3= snow

beta_air <- read.csv(paste0(modDir, "/air_slope.csv"))
# check for significance
beta_air$sigID <- ifelse(beta_air$X2.5.<0 & beta_air$X97.5.<0,1,
                    ifelse(beta_air$X2.5.>0 & beta_air$X97.5.>0,1,0))

# for plotting: predicted mu with CI

plotFreeze <- data.frame(temp_freeze = seq(-20,0, length.out=41))
                                               
plotWarm <- data.frame( temp_warm= seq(0, 30, length.out=61))
plotSnow <- data.frame( temp_snow=seq(-21,10, length.out=100))


mu_temp_freeze <- read.csv(paste0(modDir, "/mu_temp_freeze.csv"))
mu_temp_freeze$IDS <- gsub("mu_temp_freeze","",mu_temp_freeze$X)

mu_temp_freezes  <- separate_wider_delim(mu_temp_freeze, IDS, ",", names=c("repID","locID","swID"))
mu_temp_freezes$locID <- as.numeric( mu_temp_freezes$locID )
mu_temp_freezes$swID <- as.numeric(gsub("\\D","", mu_temp_freezes$swID ))


mu_temp_warm <- read.csv(paste0(modDir, "/mu_temp_warm.csv"))
mu_temp_warm$IDS <- gsub("mu_temp_warm","",mu_temp_warm$X)

mu_temp_warms  <- separate_wider_delim(mu_temp_warm, IDS, ",", names=c("repID","locID","swID"))
mu_temp_warms$locID <- as.numeric( mu_temp_warms$locID )
mu_temp_warms$swID <- as.numeric(gsub("\\D","", mu_temp_warms$swID ))

mu_temp_snow <- read.csv(paste0(modDir, "/mu_temp_snow.csv"))
mu_temp_snow$IDS <- gsub("mu_temp_snow","",mu_temp_snow$X)

mu_temp_snow  <- separate_wider_delim(mu_temp_snow, IDS, ",", names=c("repID","locID","swID"))
mu_temp_snow$locID <- as.numeric( mu_temp_snow$locID )
mu_temp_snow$swID <- as.numeric(gsub("\\D","", mu_temp_snow$swID ))


########## plot 2 graphing ----


wd <- 10
hd <- 10

xl1 <- -20
xh1 <- 30
yl1 <- -6
yh1 <- 25

xl2 <- -20
xh2 <- 10
yl2 <- -5
yh2 <- 10

# snow free/low snow x
tx1 <- seq(-20,30,by=10)
tx2 <- seq(-20,10,by=10)
ty1 <- seq(-5,25,by=5)
ty2 <- seq(-5,10, by = 5)
#size of axis ticks
tcx <- 3
#size of tick labels
cll <- 2
#line of y axis labels
yll <- 2
xll <- 2

# line for axis title
lly1 <- -5
llx1 <- -5
llx2 <- 2
# size for axis title
labll <- 3
labll2 <- 2
# size of points
pcx <- 1.5
# transparency of points
tp <- 0.8
# transparency of CI
tci <- 0.55
# line thickness regression
lwr <- 4.5
# coordinates of the text panel label
cx <- -17.5
cy1 <- 23.5
cy2 <- 9.25
# size of text label
ptc <- 3
# legend size
cxl <- 2


png(paste0(plotDir,"/mod_data.png"), width = 25, height = 55, units = "cm", res=300)
layout(matrix(seq(1,10),ncol=2, byrow=FALSE), width=rep(lcm(wd),2),height=rep(lcm(hd),5))

# loc 1: maple beech no snow
plotS1 <- soilMod %>% filter(locID == 1 & swID == 1 & snowID ==1)
plotS2 <- soilMod %>% filter(locID == 1 & swID == 2 & snowID ==1)

### snow free/low snow # 
# location 1 


mfr1 <- mu_temp_freezes %>% filter(locID == 1 & swID == 1)
mfr2 <- mu_temp_freezes %>% filter(locID == 1 & swID == 2)
mwr1 <- mu_temp_warms %>% filter(locID == 1 & swID == 1)
mwr2 <- mu_temp_warms %>% filter(locID == 1 & swID == 2)
# gray green rgb(0.58,0.63,0.53,0.25)
par(mai=c(0.75,0.75,0,0))
plot(c(0,1),c(0,1), type="n", xlim=c(xl1,xh1), ylim=c(yl1,yh1), xaxs="i",yaxs="i",
  xlab= " ", ylab=" ", axes=FALSE)
  points(plotS1$aveT, plotS1$Tsoil_6, pch=16, col=rgb(0.49,0.63,0.3,tp), cex=pcx)
  points(plotS2$aveT, plotS2$Tsoil_6, pch=16, col=rgb(0.5,0.5,0.5,tp), cex=pcx)

      # model mean line for SWC <= 0.33  
 
    
      polygon(c(plotFreeze$temp_freeze, rev(plotFreeze$temp_freeze)),
              c(mfr1$X2.5., rev(mfr1$X97.5.)), col=rgb(0.49,0.63,0.3,tci),
              border=NA)
      polygon(c(plotWarm$temp_warm, rev(plotWarm$temp_warm)),
              c(mwr1$X2.5., rev(mwr1$X97.5.)), col=rgb(0.49,0.63,0.3,tci),
              border=NA)
      points(plotFreeze$temp_freeze, mfr1$mean, type="l", col=rgb(0.49,0.63,0.3), lwd=lwr ,
             lty=2-beta_air$sigID[beta_air$forestID == 1 & beta_air$swID == 1 & beta_air$regID ==1]) 
    
        points(plotWarm$temp_warm, mwr1$mean, type="l", col=rgb(0.49,0.63,0.3), lwd=lwr ,
               lty=2-beta_air$sigID[beta_air$forestID == 1 & beta_air$swID == 1 & beta_air$regID ==2]) 


        # model mean line for SWC > 0.33  
        
        
        polygon(c(plotFreeze$temp_freeze, rev(plotFreeze$temp_freeze)),
                c(mfr2$X2.5., rev(mfr2$X97.5.)), col=rgb(0.55,0.5,0.5,tci),
                border=NA)
        polygon(c(plotWarm$temp_warm, rev(plotWarm$temp_warm)),
                c(mwr2$X2.5., rev(mwr2$X97.5.)), col=rgb(0.5,0.5,0.5,tci),
                border=NA)
        points(plotFreeze$temp_freeze, mfr2$mean, type="l", col=rgb(0.5,0.5,0.5), lwd=lwr ,
               lty=2-beta_air$sigID[beta_air$forestID == 1 & beta_air$swID == 2 & beta_air$regID ==1]) 
        
        points(plotWarm$temp_warm, mwr2$mean, type="l", col=rgb(0.5,0.5,0.5), lwd=lwr ,
               lty=2-beta_air$sigID[beta_air$forestID == 1 & beta_air$swID == 2 & beta_air$regID ==2])
      #axis
        axis(2, at= ty1,labels=FALSE, cex=tcx)
        mtext(ty1, side=2, at=ty1, line = yll, cex=cll, las=2)
        
        axis(1, at= tx1,labels=FALSE, cex=tcx)
        #mtext(tx1, side=2, at=tx1, line = xll, cex=cll, las=2)
        mtext("No/Low Snow", side=3, line=llx2, cex=labll2)
        
        text(cx,cy1,"A", cex=ptc)
        
        legend("bottomright",c("Decid.forest <= fc","Decid. forest > fc"),
               pch=16, col=c(rgb(0.49,0.63,0.3,tp),rgb(0.5,0.5,0.5,tp)), bty="n",
               cex=cxl)
        
        
        # location 2
        plotS1 <- soilMod %>% filter(locID == 2 & swID == 1 & snowID ==1)
        plotS2 <- soilMod %>% filter(locID == 2 & swID == 2 & snowID ==1)
        
        mfr1 <- mu_temp_freezes %>% filter(locID == 2 & swID == 1)
        mfr2 <- mu_temp_freezes %>% filter(locID == 2 & swID == 2)
        mwr1 <- mu_temp_warms %>% filter(locID == 2 & swID == 1)
        mwr2 <- mu_temp_warms %>% filter(locID == 2 & swID == 2)
        # gray green rgb(0.58,0.63,0.53,0.25)
        par(mai=c(0.75,0.75,0,0))
        plot(c(0,1),c(0,1), type="n", xlim=c(xl1,xh1), ylim=c(yl1,yh1), xaxs="i",yaxs="i",
             xlab= " ", ylab=" ", axes=FALSE)
        points(plotS1$aveT, plotS1$Tsoil_6, pch=16, col=rgb(0.16,0.42,0.22,tp), cex=pcx)
        points(plotS2$aveT, plotS2$Tsoil_6, pch=16, col=rgb(0.5,0.5,0.5,tp), cex=pcx)
        
        # model mean line for SWC <= 0.33  
        
        
        polygon(c(plotFreeze$temp_freeze, rev(plotFreeze$temp_freeze)),
                c(mfr1$X2.5., rev(mfr1$X97.5.)), col=rgb(0.16,0.42,0.22,tci),
                border=NA)
        polygon(c(plotWarm$temp_warm, rev(plotWarm$temp_warm)),
                c(mwr1$X2.5., rev(mwr1$X97.5.)), col=rgb(0.16,0.42,0.22,tci),
                border=NA)
        points(plotFreeze$temp_freeze, mfr1$mean, type="l", col=rgb(0.16,0.42,0.22), 
               lwd=lwr ,lty=2-beta_air$sigID[beta_air$forestID == 2 & beta_air$swID == 1 & beta_air$regID ==1]) 
        
        points(plotWarm$temp_warm, mwr1$mean, type="l", col=rgb(0.16,0.42,0.22), lwd=lwr ,
              lty= 2-beta_air$sigID[beta_air$forestID == 2 & beta_air$swID == 1 & beta_air$regID ==2]) 
        
        
        # model mean line for SWC > 0.33  
        
        
        polygon(c(plotFreeze$temp_freeze, rev(plotFreeze$temp_freeze)),
                c(mfr2$X2.5., rev(mfr2$X97.5.)), col=rgb(0.55,0.5,0.5,tci),
                border=NA)
        polygon(c(plotWarm$temp_warm, rev(plotWarm$temp_warm)),
                c(mwr2$X2.5., rev(mwr2$X97.5.)), col=rgb(0.5,0.5,0.55,tci),
                border=NA)
        points(plotFreeze$temp_freeze, mfr2$mean, type="l", col=rgb(0.5,0.5,0.5), lwd=lwr ,
               lty= 2-beta_air$sigID[beta_air$forestID == 2 & beta_air$swID == 2 & beta_air$regID ==1]) 
        
        points(plotWarm$temp_warm, mwr2$mean, type="l", col=rgb(0.5,0.5,0.5), lwd=lwr ,
               lty= 2-beta_air$sigID[beta_air$forestID == 2 & beta_air$swID == 2 & beta_air$regID ==2])
        
        #axis
        axis(2, at= ty1,labels=FALSE, cex=tcx)
        mtext(ty1, side=2, at=ty1, line = yll, cex=cll, las=2)
        
        axis(1, at= tx1,labels=FALSE, cex=tcx)
        #mtext(tx1, side=2, at=tx1, line = xll, cex=cll, las=2)
        
        legend("bottomright",c("mixed forest <= fc","mixed forest > fc"),
               pch=16, col=c(rgb(0.16,0.42,0.22,tp),rgb(0.5,0.5,0.5,tp)), bty="n",
               cex=cxl)
        
        text(cx,cy1,"B", cex=ptc)
        
        
        # location 3
        plotS1 <- soilMod %>% filter(locID == 3 & swID == 1 & snowID ==1)
        plotS2 <- soilMod %>% filter(locID == 3 & swID == 2 & snowID ==1)
        
        mfr1 <- mu_temp_freezes %>% filter(locID == 3 & swID == 1)
        mfr2 <- mu_temp_freezes %>% filter(locID == 3 & swID == 2)
        mwr1 <- mu_temp_warms %>% filter(locID == 3 & swID == 1)
        mwr2 <- mu_temp_warms %>% filter(locID == 3 & swID == 2)
        # gray green rgb(0.58,0.63,0.53,0.25)
        par(mai=c(0.75,0.75,0,0))
        plot(c(0,1),c(0,1), type="n", xlim=c(xl1,xh1), ylim=c(yl1,yh1), xaxs="i",yaxs="i",
             xlab= " ", ylab=" ", axes=FALSE)
        points(plotS1$aveT, plotS1$Tsoil_6, pch=16, col=rgb(0.58,0.79,0.92,tp), cex=pcx)
        points(plotS2$aveT, plotS2$Tsoil_6, pch=16, col=rgb(0.5,0.5,0.5,tp), cex=pcx)
        
        # model mean line for SWC <= 0.33  
        
        
        polygon(c(plotFreeze$temp_freeze, rev(plotFreeze$temp_freeze)),
                c(mfr1$X2.5., rev(mfr1$X97.5.)), col=rgb(0.58,0.79,0.92,tci),
                border=NA)
        polygon(c(plotWarm$temp_warm, rev(plotWarm$temp_warm)),
                c(mwr1$X2.5., rev(mwr1$X97.5.)), col=rgb(0.58,0.79,0.92,tci),
                border=NA)
        points(plotFreeze$temp_freeze, mfr1$mean, type="l", col=rgb(0.58,0.79,0.92), lwd=lwr,
               lty= 2-beta_air$sigID[beta_air$forestID == 3 & beta_air$swID == 1 & beta_air$regID ==1]) 
        
        points(plotWarm$temp_warm, mwr1$mean, type="l", col=rgb(0.58,0.79,0.92), lwd=lwr,
               lty= 2-beta_air$sigID[beta_air$forestID == 3 & beta_air$swID == 1 & beta_air$regID ==2]) 
        
        
        # model mean line for SWC > 0.33  
        
        
        polygon(c(plotFreeze$temp_freeze, rev(plotFreeze$temp_freeze)),
                c(mfr2$X2.5., rev(mfr2$X97.5.)), col=rgb(0.55,0.5,0.5,tci),
                border=NA)
        polygon(c(plotWarm$temp_warm, rev(plotWarm$temp_warm)),
                c(mwr2$X2.5., rev(mwr2$X97.5.)), col=rgb(0.5,0.5,0.55,tci),
                border=NA)
        points(plotFreeze$temp_freeze, mfr2$mean, type="l", col=rgb(0.5,0.5,0.5), lwd=lwr,
               lty= 2-beta_air$sigID[beta_air$forestID == 3 & beta_air$swID == 2 & beta_air$regID ==1]) 
        
        points(plotWarm$temp_warm, mwr2$mean, type="l", col=rgb(0.5,0.5,0.5), lwd=lwr,
               lty= 2-beta_air$sigID[beta_air$forestID == 3 & beta_air$swID == 2 & beta_air$regID ==2])
        
        
        
        #axis
        axis(2, at= ty1,labels=FALSE, cex=tcx)
        mtext(ty1, side=2, at=ty1, line = yll, cex=cll, las=2)
        
        axis(1, at= tx1,labels=FALSE, cex=tcx)
        #mtext(tx1, side=2, at=tx1, line = xll, cex=cll, las=2)
        legend("bottomright",c("conifer <= fc","conifer > fc"),
               pch=16, col=c(rgb(0.58,0.79,0.92,tp),rgb(0.5,0.5,0.5,tp)), bty="n",
               cex=cxl)
        
        text(cx,cy1,"C", cex=ptc)
        
        
        # location 4
        plotS1 <- soilMod %>% filter(locID == 4 & swID == 1 & snowID ==1)
        plotS2 <- soilMod %>% filter(locID == 4 & swID == 2 & snowID ==1)
        
        mfr1 <- mu_temp_freezes %>% filter(locID == 4 & swID == 1)
        mfr2 <- mu_temp_freezes %>% filter(locID == 4 & swID == 2)
        mwr1 <- mu_temp_warms %>% filter(locID == 4 & swID == 1)
        mwr2 <- mu_temp_warms %>% filter(locID == 4 & swID == 2)
        # gray green rgb(0.58,0.63,0.53,0.25)
        par(mai=c(0.75,0.75,0,0))
        plot(c(0,1),c(0,1), type="n", xlim=c(xl1,xh1), ylim=c(yl1,yh1), xaxs="i",yaxs="i",
             xlab= " ", ylab=" ", axes=FALSE)
        points(plotS1$aveT, plotS1$Tsoil_6, pch=16, col=rgb(0.85,0.58,0.39,tp), cex=pcx)
        points(plotS2$aveT, plotS2$Tsoil_6, pch=16, col=rgb(0.5,0.5,0.5,tp), cex=pcx)
        
        # model mean line for SWC <= 0.33  
        
        
        polygon(c(plotFreeze$temp_freeze, rev(plotFreeze$temp_freeze)),
                c(mfr1$X2.5., rev(mfr1$X97.5.)), col=rgb(0.85,0.58,0.39,tci),
                border=NA)
        polygon(c(plotWarm$temp_warm, rev(plotWarm$temp_warm)),
                c(mwr1$X2.5., rev(mwr1$X97.5.)), col=rgb(0.85,0.58,0.39,tci),
                border=NA)
        points(plotFreeze$temp_freeze, mfr1$mean, type="l", col=rgb(0.85,0.58,0.39,), lwd=lwr,
               lty= 2-beta_air$sigID[beta_air$forestID == 4 & beta_air$swID == 1 & beta_air$regID ==1]) 
        
        points(plotWarm$temp_warm, mwr1$mean, type="l", col=rgb(0.85,0.58,0.39), lwd=lwr,
               lty= 2-beta_air$sigID[beta_air$forestID == 4 & beta_air$swID == 1 & beta_air$regID ==2]) 
        
        
        # model mean line for SWC > 0.33  
        
        
        polygon(c(plotFreeze$temp_freeze, rev(plotFreeze$temp_freeze)),
                c(mfr2$X2.5., rev(mfr2$X97.5.)), col=rgb(0.55,0.5,0.5,tci),
                border=NA)
        polygon(c(plotWarm$temp_warm, rev(plotWarm$temp_warm)),
                c(mwr2$X2.5., rev(mwr2$X97.5.)), col=rgb(0.5,0.5,0.5,tci),
                border=NA)
        points(plotFreeze$temp_freeze, mfr2$mean, type="l", col=rgb(0.5,0.5,0.5), lwd=lwr,
               lty= 2-beta_air$sigID[beta_air$forestID == 4 & beta_air$swID == 2 & beta_air$regID ==1]) 
        
        points(plotWarm$temp_warm, mwr2$mean, type="l", col=rgb(0.5,0.5,0.5), lwd=lwr,
               lty= 2-beta_air$sigID[beta_air$forestID == 4 & beta_air$swID == 2 & beta_air$regID ==2])
        
        
        #axis
        axis(2, at= ty1,labels=FALSE, cex=tcx)
        mtext(ty1, side=2, at=ty1, line = yll, cex=cll, las=2)
        
        axis(1, at= tx1,labels=FALSE, cex=tcx)
        #mtext(tx1, side=2, at=tx1, line = xll, cex=cll, las=2)
        
        legend("bottomright",c("scrub <= fc","scrub > fc"),
               pch=16, col=c(rgb(0.85,0.58,0.39,tp),rgb(0.5,0.5,0.5,tp)), bty="n",
               cex=cxl)
        
        text(cx,cy1,"D", cex=ptc)
        
        
        # location 5
        plotS1 <- soilMod %>% filter(locID == 5 & swID == 1 & snowID ==1)
        plotS2 <- soilMod %>% filter(locID == 5 & swID == 2 & snowID ==1)
        
        mfr1 <- mu_temp_freezes %>% filter(locID == 5 & swID == 1)
        mfr2 <- mu_temp_freezes %>% filter(locID == 5 & swID == 2)
        mwr1 <- mu_temp_warms %>% filter(locID == 5 & swID == 1)
        mwr2 <- mu_temp_warms %>% filter(locID == 5 & swID == 2)
        
 
        
        par(mai=c(0.75,0.75,0,0))
        plot(c(0,1),c(0,1), type="n", xlim=c(xl1,xh1), ylim=c(yl1,yh1), xaxs="i",yaxs="i",
             xlab= " ", ylab=" ", axes=FALSE)
        points(plotS1$aveT, plotS1$Tsoil_6, pch=16, col=rgb(0.85,0.75,0.54,tp), cex=pcx)
        points(plotS2$aveT, plotS2$Tsoil_6, pch=16, col=rgb(0.5,0.5,0.5,tp), cex=pcx)
        
        # model mean line for SWC <= 0.33  
        
        
        polygon(c(plotFreeze$temp_freeze, rev(plotFreeze$temp_freeze)),
                c(mfr1$X2.5., rev(mfr1$X97.5.)), col=rgb(0.85,0.75,0.54,tci),
                border=NA)
        polygon(c(plotWarm$temp_warm, rev(plotWarm$temp_warm)),
                c(mwr1$X2.5., rev(mwr1$X97.5.)), col=rgb(0.85,0.75,0.54,tci),
                border=NA)
        points(plotFreeze$temp_freeze, mfr1$mean, type="l", col=rgb(0.85,0.75,0.54), lwd=lwr,
               lty= 2-beta_air$sigID[beta_air$forestID == 5 & beta_air$swID == 1 & beta_air$regID ==1]) 
        
        points(plotWarm$temp_warm, mwr1$mean, type="l", col=rgb(0.85,0.75,0.54), lwd=lwr,
               lty= 2-beta_air$sigID[beta_air$forestID == 5 & beta_air$swID == 1 & beta_air$regID ==2]) 
        
        
        # model mean line for SWC > 0.33  
        
        
        polygon(c(plotFreeze$temp_freeze, rev(plotFreeze$temp_freeze)),
                c(mfr2$X2.5., rev(mfr2$X97.5.)), col=rgb(0.55,0.5,0.5,tci),
                border=NA)
        polygon(c(plotWarm$temp_warm, rev(plotWarm$temp_warm)),
                c(mwr2$X2.5., rev(mwr2$X97.5.)), col=rgb(0.5,0.5,0.55,tci),
                border=NA)
        points(plotFreeze$temp_freeze, mfr2$mean, type="l", col=rgb(0.5,0.5,0.5), lwd=lwr,
               lty= 2-beta_air$sigID[beta_air$forestID == 5 & beta_air$swID == 2 & beta_air$regID ==1]) 
        
        points(plotWarm$temp_warm, mwr2$mean, type="l", col=rgb(0.5,0.5,0.5), lwd=lwr,
               lty= 2-beta_air$sigID[beta_air$forestID == 5 & beta_air$swID == 2 & beta_air$regID ==2])       

        
        #axis
        axis(2, at= ty1,labels=FALSE, cex=tcx)
        mtext(ty1, side=2, at=ty1, line = yll, cex=cll, las=2)
        
        axis(1, at= tx1,labels=FALSE, cex=tcx)
        mtext(tx1, side=1, at=tx1, line = xll, cex=cll)
        
        
        legend("bottomright",c("meadow <= fc","meadow > fc"),
               pch=16, col=c(rgb(0.85,0.75,0.54,tp),rgb(0.5,0.5,0.5,tp)), bty="n",
               cex=cxl)
        
        mtext(expression(paste("Soil temperature (",degree,"C)")), side=2, line=lly1, cex=labll, outer=TRUE)
        text(cx,cy1,"E", cex=ptc)
        
        ### snow free/low snow # 
        # location 1 
        plotS1 <- soilMod %>% filter(locID == 1 & swID == 1 & snowID ==2)
        plotS2 <- soilMod %>% filter(locID == 1 & swID == 2 & snowID ==2)
        
        msr1 <- mu_temp_snow %>% filter(locID == 1 & swID == 1)
        msr2 <- mu_temp_snow %>% filter(locID == 1 & swID == 2)

        # gray green rgb(0.58,0.63,0.53,0.25)
        par(mai=c(0.75,0.75,0,0))
        plot(c(0,1),c(0,1), type="n", xlim=c(xl2,xh2), ylim=c(yl2,yh2), xaxs="i",yaxs="i",
             xlab= " ", ylab=" ", axes=FALSE)
        points(plotS1$aveT, plotS1$Tsoil_6, pch=16, col=rgb(0.49,0.63,0.3,tp), cex=pcx)
        points(plotS2$aveT, plotS2$Tsoil_6, pch=16, col=rgb(0.5,0.5,0.5,tp), cex=pcx)
        
        # model mean line for SWC <= 0.33  
        
        
        polygon(c(plotSnow$temp_snow, rev(plotSnow$temp_snow)),
                c(msr1$X2.5., rev(msr1$X97.5.)), col=rgb(0.49,0.63,0.3,tci),
                border=NA)
  
        points(plotSnow$temp_snow, msr1$mean, type="l", col=rgb(0.49,0.63,0.3), lwd=lwr,
               lty=2-beta_air$sigID[beta_air$forestID == 1 & beta_air$swID == 1 & beta_air$regID ==3])
        
        
        polygon(c(plotSnow$temp_snow, rev(plotSnow$temp_snow)),
                c(msr2$X2.5., rev(msr2$X97.5.)), col=rgb(0.5,0.5,0.5,tci),
                border=NA)
        
        points(plotSnow$temp_snow, msr2$mean, type="l", col=rgb(0.5,0.5,0.5), lwd=lwr,
               lty=2-beta_air$sigID[beta_air$forestID == 1 & beta_air$swID == 2 & beta_air$regID ==3])
        

        #axis
        axis(2, at= ty2,labels=FALSE, cex=tcx)
        mtext(ty2, side=2, at=ty2, line = yll, cex=cll, las=2)
        axis(1, at= tx2,labels=FALSE, cex=tcx)
        #mtext(tx2, side=1, at=tx2, line = xll, cex=cll)
        mtext("Snow", side=3, line=llx2, cex=labll2)
        legend("bottomright",c("Decid.forest <= fc","Decid. forest > fc"),
               pch=16, col=c(rgb(0.49,0.63,0.3,tp),rgb(0.5,0.5,0.5,tp)), bty="n",
               cex=cxl)
        
        text(cx,cy2,"F", cex=ptc)
        
        
        ### snow free/low snow # 
        # location 2 
        plotS1 <- soilMod %>% filter(locID == 2 & swID == 1 & snowID ==2)
        plotS2 <- soilMod %>% filter(locID == 2 & swID == 2 & snowID ==2)
        
        msr1 <- mu_temp_snow %>% filter(locID == 2 & swID == 1)
        msr2 <- mu_temp_snow %>% filter(locID == 2 & swID == 2)
        
        # gray green rgb(0.58,0.63,0.53,0.25)
        par(mai=c(0.75,0.75,0,0))
        plot(c(0,1),c(0,1), type="n", xlim=c(xl2,xh2), ylim=c(yl2,yh2), xaxs="i",yaxs="i",
             xlab= " ", ylab=" ", axes=FALSE)
        points(plotS1$aveT, plotS1$Tsoil_6, pch=16, col=rgb(0.16,0.42,0.22,tp), cex=pcx)
        points(plotS2$aveT, plotS2$Tsoil_6, pch=16, col=rgb(0.5,0.5,0.5,tp), cex=pcx)
        
        
        polygon(c(plotSnow$temp_snow, rev(plotSnow$temp_snow)),
                c(msr1$X2.5., rev(msr1$X97.5.)), col=rgb(0.16,0.42,0.22,tci),
                border=NA)
        
        points(plotSnow$temp_snow, msr1$mean, type="l", col=rgb(0.16,0.42,0.22), lwd=lwr,
               lty=2-beta_air$sigID[beta_air$forestID == 2 & beta_air$swID == 1 & beta_air$regID ==3])
        
        
        polygon(c(plotSnow$temp_snow, rev(plotSnow$temp_snow)),
                c(msr2$X2.5., rev(msr2$X97.5.)), col=rgb(0.5,0.5,0.5,tci),
                border=NA)
        
        points(plotSnow$temp_snow, msr2$mean, type="l", col=rgb(0.5,0.5,0.5), lwd=lwr,
               lty=2-beta_air$sigID[beta_air$forestID == 2 & beta_air$swID == 2 & beta_air$regID ==3])
        
        
        #axis
        axis(2, at= ty2,labels=FALSE, cex=tcx)
        mtext(ty2, side=2, at=ty2, line = yll, cex=cll, las=2)
        axis(1, at= tx2,labels=FALSE, cex=tcx)
        #mtext(tx2, side=1, at=tx2, line = xll, cex=cll)
        legend("bottomright",c("mixed forest <= fc","mixed forest > fc"),
               pch=16, col=c(rgb(0.16,0.42,0.22,tp),rgb(0.5,0.5,0.5,tp)), bty="n",
               cex=cxl)
        
        
        text(cx,cy2,"G", cex=ptc)
        
        
        
        ### snow free/low snow # 
        # location 3 
        plotS1 <- soilMod %>% filter(locID == 3 & swID == 1 & snowID ==2)
        plotS2 <- soilMod %>% filter(locID == 3 & swID == 2 & snowID ==2)
        
        msr1 <- mu_temp_snow %>% filter(locID == 3 & swID == 1)
        msr2 <- mu_temp_snow %>% filter(locID == 3 & swID == 2)
        
      
        par(mai=c(0.75,0.75,0,0))
        plot(c(0,1),c(0,1), type="n", xlim=c(xl2,xh2), ylim=c(yl2,yh2), xaxs="i",yaxs="i",
             xlab= " ", ylab=" ", axes=FALSE)
        points(plotS1$aveT, plotS1$Tsoil_6, pch=16, col=rgb(0.58,0.79,0.92,tp), cex=pcx)
        points(plotS2$aveT, plotS2$Tsoil_6, pch=16, col=rgb(0.5,0.5,0.5,tp), cex=pcx)
        
        
        polygon(c(plotSnow$temp_snow, rev(plotSnow$temp_snow)),
                c(msr1$X2.5., rev(msr1$X97.5.)), col=rgb(0.58,0.79,0.92,tci),
                border=NA)
        
        points(plotSnow$temp_snow, msr1$mean, type="l", col=rgb(0.58,0.79,0.92), lwd=lwr,
               lty=2-beta_air$sigID[beta_air$forestID == 3 & beta_air$swID == 1 & beta_air$regID ==3])
        
        
        polygon(c(plotSnow$temp_snow, rev(plotSnow$temp_snow)),
                c(msr2$X2.5., rev(msr2$X97.5.)), col=rgb(0.5,0.5,0.5,tci),
                border=NA)
        
        points(plotSnow$temp_snow, msr2$mean, type="l", col=rgb(0.5,0.5,0.5), lwd=lwr,
               lty=2-beta_air$sigID[beta_air$forestID == 3 & beta_air$swID == 2 & beta_air$regID ==3])
        
        
        
        #axis
        axis(2, at= ty2,labels=FALSE, cex=tcx)
        mtext(ty2, side=2, at=ty2, line = yll, cex=cll, las=2)
        axis(1, at= tx2,labels=FALSE, cex=tcx)
        #mtext(tx2, side=1, at=tx2, line = xll, cex=cll)
        text(cx,cy2,"H", cex=ptc)
        legend("bottomright",c("conifer <= fc","conifer > fc"),
               pch=16, col=c(rgb(0.58,0.79,0.92,tp),rgb(0.5,0.5,0.5,tp)), bty="n",
               cex=cxl)
        
        ### snow free/low snow # 
        # location 4 
        plotS1 <- soilMod %>% filter(locID == 4 & swID == 1 & snowID ==2)
        plotS2 <- soilMod %>% filter(locID == 4 & swID == 2 & snowID ==2)
        
        msr1 <- mu_temp_snow %>% filter(locID == 4 & swID == 1)
        msr2 <- mu_temp_snow %>% filter(locID == 4 & swID == 2)
        
        
        par(mai=c(0.75,0.75,0,0))
        plot(c(0,1),c(0,1), type="n", xlim=c(xl2,xh2), ylim=c(yl2,yh2), xaxs="i",yaxs="i",
             xlab= " ", ylab=" ", axes=FALSE)
        points(plotS1$aveT, plotS1$Tsoil_6, pch=16, col=rgb(0.85,0.58,0.39,tp), cex=pcx)
        points(plotS2$aveT, plotS2$Tsoil_6, pch=16, col=rgb(0.5,0.5,0.5,tp), cex=pcx)
        
        
        polygon(c(plotSnow$temp_snow, rev(plotSnow$temp_snow)),
                c(msr1$X2.5., rev(msr1$X97.5.)), col=rgb(0.85,0.58,0.39,tci),
                border=NA)
        
        points(plotSnow$temp_snow, msr1$mean, type="l", col=rgb(0.85,0.58,0.39), lwd=lwr,
               lty=2-beta_air$sigID[beta_air$forestID == 4 & beta_air$swID == 1 & beta_air$regID ==3])
        
        
        polygon(c(plotSnow$temp_snow, rev(plotSnow$temp_snow)),
                c(msr2$X2.5., rev(msr2$X97.5.)), col=rgb(0.5,0.5,0.5,tci),
                border=NA)
        
        points(plotSnow$temp_snow, msr2$mean, type="l", col=rgb(0.5,0.5,0.5), lwd=lwr,
               lty=2-beta_air$sigID[beta_air$forestID == 4 & beta_air$swID == 2 & beta_air$regID ==3])
        
        #axis
        axis(2, at= ty2,labels=FALSE, cex=tcx)
        mtext(ty2, side=2, at=ty2, line = yll, cex=cll, las=2)
        axis(1, at= tx2,labels=FALSE, cex=tcx)
        text(cx,cy2,"I", cex=ptc)
        
        legend("bottomright",c("scrub <= fc","scrub > fc"),
               pch=16, col=c(rgb(0.85,0.58,0.39,tp),rgb(0.5,0.5,0.5,tp)), bty="n",
               cex=cxl)
        
        ### snow free/low snow # 
        # location 5 
        plotS1 <- soilMod %>% filter(locID == 5 & swID == 1 & snowID ==2)
        plotS2 <- soilMod %>% filter(locID == 5 & swID == 2 & snowID ==2)
        
        msr1 <- mu_temp_snow %>% filter(locID == 5 & swID == 1)
        msr2 <- mu_temp_snow %>% filter(locID == 5 & swID == 2)
        
        par(mai=c(0.75,0.75,0,0))
        plot(c(0,1),c(0,1), type="n", xlim=c(xl2,xh2), ylim=c(yl2,yh2), xaxs="i",yaxs="i",
             xlab= " ", ylab=" ", axes=FALSE)
        points(plotS1$aveT, plotS1$Tsoil_6, pch=16, col=rgb(0.85,0.75,0.54,tp), cex=pcx)
        points(plotS2$aveT, plotS2$Tsoil_6, pch=16, col=rgb(0.5,0.5,0.5,tp), cex=pcx)
        
        
        polygon(c(plotSnow$temp_snow, rev(plotSnow$temp_snow)),
                c(msr1$X2.5., rev(msr1$X97.5.)), col=rgb(0.85,0.75,0.54,tci),
                border=NA)
        
        points(plotSnow$temp_snow, msr1$mean, type="l", col=rgb(0.85,0.75,0.54), lwd=lwr,
               lty=2-beta_air$sigID[beta_air$forestID == 5 & beta_air$swID == 1 & beta_air$regID ==3])
        
        
        polygon(c(plotSnow$temp_snow, rev(plotSnow$temp_snow)),
                c(msr2$X2.5., rev(msr2$X97.5.)), col=rgb(0.5,0.5,0.5,tci),
                border=NA)
        
        points(plotSnow$temp_snow, msr2$mean, type="l", col=rgb(0.5,0.5,0.5), lwd=lwr,
               lty=2-beta_air$sigID[beta_air$forestID == 5 & beta_air$swID == 2 & beta_air$regID ==3])
        
        
        #axis
        axis(2, at= ty2,labels=FALSE, cex=tcx)
        mtext(ty2, side=2, at=ty2, line = yll, cex=cll, las=2)
        axis(1, at= tx2,labels=FALSE, cex=tcx)
        mtext(tx2, side=1, at=tx2, line = xll, cex=cll)
        text(cx,cy2,"J", cex=ptc)
        legend("bottomright",c("meadow <= fc","meadow > fc"),
               pch=16, col=c(rgb(0.85,0.75,0.54,tp),rgb(0.5,0.5,0.5,tp)), bty="n",
               cex=cxl)
        
        mtext(expression(paste("Air temperature (",degree,"C)")), side=1, line=llx1, cex=labll, outer=TRUE)
        
dev.off()


################# Supplement: -----

par(mai=c(0.25,0,0,0))
plot(c(0,1),c(0,1), type="n", xlim=c(xl,xh), ylim=c(yl2,yh2), xaxs="i",yaxs="i",
     xlab= " ", ylab=" ", axes=FALSE)

for(i in 1:nrow(singleLoc)){
  polygon(c(singleLoc$DD[i]-0.001,singleLoc$DD[i]-0.001,
            singleLoc$DD[i]+0.001,singleLoc$DD[i]+0.001),
          c(precipRescale(0,snMax,yl2,yh2),
            precipRescale(singleLoc$SNWD[i],snMax,yl2,yh2),
            precipRescale(singleLoc$SNWD[i],snMax,yl2,yh2),
            precipRescale(0,snMax,yl2,yh2)),
          col="grey75", border=NA)
}



for(i in 1:5){
  points(soilDat$DD[soilDat$locID==i],soilDat$Tsurf_15[soilDat$locID==i],
         type="l", col=locColor[i], lwd=lw )
}
abline(h=0)
#text(rainsnow$DD, rep(25,length(rainsnow$DD)), "*")
axis(2, c(-30,yxSuT,40), rep("", length(yxSuT)+2), cex=cx_tick)
mtext(yxSuT, side=2, at=yxSuT, line = lyax, cex=cll, las=2)
axis(4,  precipRescale(yxSN,snMax,yl2,yh2), rep("", length(yxSN)), cex=cx_tick)
mtext(yxSN, side=4, at=precipRescale(yxSN,snMax,yl2,yh2), line = lyax, cex=cll, las=2)
axis(1, monthDD, rep("", length(monthDD)), cex=cx_tick)
mtext("Surface temperature", side=2, line=lly1, cex=labll)
mtext(expression(paste("(",degree,"C)")), side=2, line=lly2, cex=labll)




