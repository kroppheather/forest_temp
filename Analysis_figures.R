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

weatherDay$date <- as.Date(weatherDay$doy-1, origin=paste0(weatherDay$year, "-01-01"))

ggplot(weatherDay, aes(date, SNWD/10))+
  geom_col(alpha=0.5)


ggplot(soilAll, aes(VWC_f, TSoil6, color=location))+
  geom_point(alpha=0.5)

soilmet <- left_join(soilAll, weatherDay, by=c("doy","year"))

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

ggplot(soilmet%>%filter(location=="Spruce RG09"&aveT.x<0), aes(aveT.x, TSoil6, color=SNWD))+
  geom_point()
ggplot(soilmet%>%filter(location=="Buckthorn RG03"&aveT.x<0), aes(aveT.x, TSoil6, color=VWC_f))+
  geom_point()
ggplot(soilmet%>%filter(location=="Buckthorn RG03"&aveT.x<0), aes(VWC_f, TSoil6))+
  geom_point()
ggplot(soilmet%>%filter(location=="Spruce RG09"&aveT.x<0), aes(VWC_f, TSoil6))+
  geom_point()

ggplot(soilmet%>%filter(location=="maple-beech"&aveT.x<0), aes(VWC_f, TSoil6))+
  geom_point()

ggplot(soilmet%>%filter(location=="maple-beech"&aveT.x<0), aes(aveT.x, TSoil6))+
  geom_point()

ggplot(soilmet, aes(SNWD, TSurf2, color=location))+
  geom_point(alpha=0.5)

#### basic time series plots to visualize data

wd <- 20
hd <- 10


