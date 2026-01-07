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
  filter(is.na(aveT) == FALSE)
# create ID for time periods below freezing
soilMod$freezeModID <- ifelse(soilMod$aveT <= 0, 1,2) # 1 below or at freezing
# create ID for forest type and freezing time period
soilMod$modforestID <- ifelse(soilMod$freezeModID == 1, soilMod$locID,
                              soilMod$locID+5)

# create ID table
soilIDs <- soilMod %>%
  ungroup %>%
  select(location, locID, freezeModID, modforestID) %>%
  distinct()%>%
  arrange(modforestID)

######## color scheme for figures ----
locLabel <- c("Deciduous forest",
              "Conifer-deciduous forest",
              "Conifer monoculture",
              "Invasive scrub",
              "Reforestation field")

locColor <- c(rgb(93,168,153, maxColorValue = 255),
              rgb(51,117,56, maxColorValue = 255),
              rgb(148,203,236, maxColorValue = 255),
              rgb(194,106,119, maxColorValue = 255),
              rgb(220,205,125, maxColorValue = 255))

locColorst <- c(rgb(93,168,153,100, maxColorValue = 255),
              rgb(51,117,56,100, maxColorValue = 255),
              rgb(148,203,236,100, maxColorValue = 255),
              rgb(194,106,119,100, maxColorValue = 255),
              rgb(220,205,125,100, maxColorValue = 255))


####### Figure 1: Met and soil data ----
singleLoc <- soilDat %>%
  filter(location == "hemlock sapflow")

rainsnow <- singleLoc %>%
  filter(rain_snow == 1)

wd <- 50
hd <- 15

# x range
xl <- 2022.74
xh <- 2025.76
#y range for meteorological graph
#air temp and precipitation
yl <- -20
yh <- 30
#precip in mm
prMax <- 60

#surface temp
yl2 <- -20
yh2 <- 30
#snow depth max (mm)
snMax <- 650

yl3 <- -5
yh3 <- 25

yl4 <- 0.05
yh4 <- 0.65

#sizing for lines of graph
lw <- 5

# axes label sequences
yxAT <- seq(-15,25, by=10)
yxSuT <- seq(-15,25, by=10)
yxSoT <- seq(-5,25, by=5)
yxSW <- seq(0,0.6, by=0.1)
yxPR <- seq(0,50, by=10)
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

months <- c(c(274,305,335),monthseq,monthseqL,  c(1,32,60,91,121,152,182,213,244))
monthsLab <- c(c("O","N","D"),monthLab, monthLab,c("J","F","M","A","M","J","J","A","S"))
years <- c(rep(2022,3), rep(2023,12),rep(2024,12),rep(2025,9))
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




png(paste0(plotDir,"/daily_data.png"), width = 67, height = 70, units = "cm", res=300)
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
points(singleLoc$DD, singleLoc$aveT, type="l", pch=19, lwd=lw )
axis(2, c(-30,yxAT,40), rep("", length(yxAT)+2), cex=cx_tick)
mtext(yxAT, side=2, at=yxAT, line = lyax, cex=cll, las=2)
axis(4,  precipRescale(yxPR,prMax,yl,yh), rep("", length(yxPR)), cex=cx_tick)
mtext(yxPR, side=4, at=precipRescale(yxPR,prMax,yl,yh), line = lyax, cex=cll, las=2)
axis(1, monthDD, rep("", length(monthDD)), cex=cx_tick)
mtext("Air temperature", side=2, line=lly1, cex=labll, )
mtext(expression(paste("(",degree,"C)")), side=2, line=lly2, cex=labll)

mtext("Precipitation", side=4, line=lly3, cex=labll)
mtext("(mm)", side=4, line=lly4, cex=labll)
legend(2024.25,33, c("temperature", "precipitation"), col=c("black","lightskyblue2"), lwd=c(lw,NA), pch=c(NA,15),
       bty="n", horiz=TRUE, cex=lgcx)
text(xp, 27, "A", cex=plcx)
# above surface temp and snow
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

mtext("Snow depth", side=4, line=lly3, cex=labll)
mtext("(mm)", side=4, line=lly4, cex=labll)
legend(2024.25,33, c( "snow depth"), col=c("grey75"), pch=15,
       bty="n", horiz=TRUE, cex=lgcx)
text(xp, 27, "B", cex=plcx)
# soil temp
par(mai=c(0.25,0,0,0))
plot(c(0,1),c(0,1), type="n", xlim=c(xl,xh), ylim=c(yl3,yh3), xaxs="i",yaxs="i",
     xlab= " ", ylab=" ", axes=FALSE)
abline(h=0)


for(i in 1:5){
  points(soilDat$DD[soilDat$locID==i],soilDat$Tsoil_6[soilDat$locID==i],
         type="l", col=locColor[i], lwd=lw )
}
axis(2, c(-30,yxSoT,40), rep("", length(yxSoT)+2), cex=cx_tick)
mtext(yxSoT, side=2, at=yxSoT, line = lyax, cex=cll, las=2)
axis(1, monthDD, rep("", length(monthDD)), cex=cx_tick)
mtext("Soil temperature", side=2, line=lly1, cex=labll)
mtext(expression(paste("(", degree,"C)")), side=2, line=lly2, cex=labll)
text(xp, 23, "C", cex=plcx)
#soil moisture

par(mai=c(0.25,0,0,0))
plot(c(0,1),c(0,1), type="n", xlim=c(xl,xh), ylim=c(yl4,yh4), xaxs="i",yaxs="i",
     xlab= " ", ylab=" ", axes=FALSE)

legend(2022.9,0.66, locLabel[1:2], col=locColor[1:2], lty=1, lwd=lw, bty="n", cex=lgcx)
legend(2023.9,0.66, locLabel[3:4], col=locColor[3:4], lty=1, lwd=lw, bty="n", cex=lgcx)
legend(2024.9,0.66, locLabel[5], col=locColor[5], lty=1, lwd=lw, bty="n", cex=lgcx)
for(i in 1:5){
  points(soilDat$DD[soilDat$locID==i],soilDat$VWC_gap[soilDat$locID==i],
         type="l", col=locColor[i], lwd=lw )
}
axis(1, monthDD, rep("", length(monthDD)), cex=cx_tick)
mtext(monthsLab, side=1, at=monthDD, line = lyax, cex=cll)
mtext(seq(2023,2025), side=1, at=seq(2023,2025), line = lyax+5, cex=cll+1, adj=0)

axis(2, c(-1,yxSW,1), rep("", length(yxSW)+2), cex=cx_tick)
mtext(yxSW, side=2, at=yxSW, line = lyax, cex=cll, las=2)
mtext("Soil moisture", side=2, line=lly1, cex=labll)
mtext(expression(paste("(m"^3,"m"^-3,")")), side=2, line=lly2, cex=labll)
text(xp, 0.6, "D", cex=plcx)
dev.off()
