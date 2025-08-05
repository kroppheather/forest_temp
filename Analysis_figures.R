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
library(zoo)
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

ggplot(soilmet%>%filter(location=="maple-beech"&aveT<0), aes(aveT, TSoil6))+
  geom_point()
ggplot(soilmet%>%filter(location=="maple-beech"&aveT>=0), aes(VWC_f, TSoil6))+
  geom_point()
ggplot(soilmet%>%filter(location=="Spruce RG09"&aveT>=0), aes(VWC_f, TSoil6))+
  geom_point()
ggplot(soilmet%>%filter(location=="Buckthorn RG03"&aveT>=0), aes(VWC_f, TSoil6))+
  geom_point()
ggplot(soilmet%>%filter(location=="hemlock sapflow"&aveT>=0), aes(VWC_f, TSoil6))+
  geom_point()
ggplot(soilmet%>%filter(location=="Rogers reforestation"&aveT>=0), aes(VWC_f, TSoil6))+
  geom_point()

ggplot(soilmet%>%filter(location=="Rogers reforestation"&aveT>=0), aes(VWC_f, TSoil6, color=as.factor(year)))+
  geom_point()




ggplot(soilmet, aes(SNWD, TSurf2, color=location))+
  geom_point(alpha=0.5)

############### check time series for each location
dayAll <- data.frame(doy=c(seq(1,365),seq(1,365),seq(1,366),seq(1,365)),
                    year=c(rep(2022,365),rep(2023,365),rep(2024,366),rep(2025,365)))

weatherDays <- left_join(dayAll, weatherJoin, by=c("doy","year"))
soilAll$DD <- ifelse(leap_year(soilAll$year),soilAll$year + ((soilAll$doy-1)/366),
                     soilAll$year + ((soilAll$doy-1)/365))
soilLoc <- list()
Locsub <- list()
Locs <- unique(soilAll$location)
for(i in 1:length(Locs)){
  Locsub[[i]] <- soilAll %>% filter(location == Locs[i])
  soilLoc[[i]] <- left_join(weatherDays,Locsub[[i]], by=c("doy","year"))
  soilLoc[[i]]$TsoilL1 <- c(soilLoc[[i]]$TSoil6[-1],NA)
  soilLoc[[i]]$TsoilL2 <- c(soilLoc[[i]]$TSoil6[c(-1,-2)],NA,NA)
  soilLoc[[i]]$TsoilL3 <- c(soilLoc[[i]]$TSoil6[c(-1,-2,-3)],NA,NA,NA)
  soilLoc[[i]]$TsoilL4 <- c(soilLoc[[i]]$TSoil6[c(-1,-2,-3,-4)],NA,NA,NA,NA)
}

ggplot(soilLoc[[5]], aes(TsoilL4, TSoil6))+
  geom_point()

#### basic time series plots to visualize data

wd <- 20
hd <- 10


