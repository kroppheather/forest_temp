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

soilAll$VWC_f <- ifelse(soilAll$TSoil6 <0,NA, soilAll$VWC)

weatherDay$date <- as.Date(weatherDay$doy-1, origin=paste0(weatherDay$year, "-01-01"))
weatherJoin <- weatherDay %>%
  select(!c(aveT,nAveT,maxT,minT))
soilmet <- left_join(soilAll, weatherJoin, by=c("doy","year"))


# gap fill freezing soil data from measurements before freeze
soilmet$freezeSoil <- ifelse(soilmet$TSoil6<0,1,0)

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
  freezeFill[i] <- soilSub$VWC
}
freezeCheck$lastVWC <- freezeFill

freezeJoin <- freezeCheck %>%
  select(location,year,doy,freezeEvent, freezeStart, lastVWC)

soilMet <- left_join(soilMet, freezeJoin, by=c("location","year","doy"))

soilMet$VWC_gap <- ifelse(soilMet$freezeSoil == 1, soilMet$lastVWC, soilMet$VWC)

# check assumptions for model. Verify linear -----
ggplot(soilAll, aes(date, TSoil6, color=location))+
  geom_line(alpha=0.5)

ggplot(soilAll, aes(date, aveT))+
  geom_line(alpha=0.5)

ggplot(soilAll%>%filter(year==2025), aes(date, Tsurf15, color=location))+
  geom_point(alpha=0.5)+
  geom_line()
ggplot(soilAll%>%filter(year==2025), aes(date, TSurf2, color=location))+
  geom_point(alpha=0.5)+
  geom_line()

ggplot(weatherDay, aes(date, SNWD/10))+
  geom_col(alpha=0.5)


ggplot(soilAll, aes(VWC_f, TSoil6, color=location))+
  geom_point(alpha=0.5)


ggplot(soilAll, aes(Tsurf15, TSoil6, color=location))+
  geom_point(alpha=0.5)
ggplot(soilAll, aes(aveT, TSoil6, color=location))+
  geom_point(alpha=0.5)

ggplot(soilAll%>%filter(location=="Buckthorn RG03"), aes(aveT, TSoil6, color=VWC_f))+
  geom_point()
ggplot(soilAll%>%filter(location=="Rogers reforestation"), aes(aveT, TSoil6, color=VWC_f))+
  geom_point()
ggplot(soilAll%>%filter(location=="Spruce RG09"), aes(aveT, TSoil6, color=VWC_f))+
  geom_point()
ggplot(soilAll%>%filter(location=="hemlock sapflow"), aes(aveT, TSoil6, color=VWC_f))+
  geom_point()
ggplot(soilAll%>%filter(location=="maple-beech"), aes(aveT, TSoil6, color=VWC_f))+
  geom_point()

ggplot(soilmet%>%filter(location=="Spruce RG09"&aveT<0), aes(aveT, TSoil6, color=SNWD))+
  geom_point()
ggplot(soilmet%>%filter(location=="Buckthorn RG03"&aveT<0), aes(aveT, TSoil6, color=VWC_f))+
  geom_point()
ggplot(soilmet%>%filter(location=="Buckthorn RG03"&aveT<0), aes(VWC_f, TSoil6))+
  geom_point()
ggplot(soilmet%>%filter(location=="Spruce RG09"&aveT<0), aes(VWC_f, TSoil6))+
  geom_point()

ggplot(soilmet%>%filter(location=="maple-beech"&aveT<0), aes(VWC_f, TSoil6))+
  geom_point()

ggplot(soilmet%>%filter(location=="maple-beech"), aes(aveT, TSoil6, color=aveT))+
  geom_point()
ggplot(soilmet%>%filter(location=="maple-beech"&aveT>=0), aes(VWC_f, TSoil6, color=aveT))+
  geom_point()
ggplot(soilmet%>%filter(location=="maple-beech"&aveT<0), aes(VWC_f, TSoil6, color=aveT))+
  geom_point()
ggplot(soilmet%>%filter(location=="Spruce RG09"&aveT>=0), aes(VWC_f, TSoil6, color=aveT))+
  geom_point()
ggplot(soilmet%>%filter(location=="Buckthorn RG03"&aveT>=0), aes(VWC_f, TSoil6, color=aveT))+
  geom_point()
ggplot(soilmet%>%filter(location=="hemlock sapflow"&aveT>=0), aes(VWC_f, TSoil6, color=aveT))+
  geom_point()
ggplot(soilmet%>%filter(location=="Rogers reforestation"), aes(VWC_f, TSoil6, color=aveT))+
  geom_point()

ggplot(soilmet%>%filter(location=="Rogers reforestation"), aes(VWC_f, TSoil6, color=as.factor(year)))+
  geom_point()




ggplot(soilmet, aes(SNWD, TSurf2, color=location))+
  geom_point(alpha=0.5)
rf <- soilmet%>%filter(location=="Rogers reforestation" & year == 2024)


