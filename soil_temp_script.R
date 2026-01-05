##### directories & libraries ----
library(dplyr)
library(lubridate)
library(ggplot2)
library(rjags)
library(MCMCvis)

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

#remove observations with missing air temp

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
  distinct()
############### set up model ---------
# data
dataList <- list(Nobs= nrow(soilMod),
                 s_temp = soilMod$Tsoil_6,
                 modforestID = soilMod$modforestID,
                 forestID = soilMod$locID,
                 air_temp = soilMod$aveT)

