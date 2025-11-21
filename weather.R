######### organizes weather data

##### libraries ----
library(dplyr)
library(ggplot2)
library(lubridate)

##### directories and data files ----

dirComp <- c("G:/My Drive/research/projects",
             "/Users/hkropp/Library/CloudStorage/GoogleDrive-hkropp@hamilton.edu/My Drive/research/projects")
compID <- 2


##
atmos <- read.csv(paste0(dirComp[compID],"/forest_soil/climate/weather_campus/v1.7/Atmos41_weather.csv"))


# coop snow and precip data from westmoreland
  snow <- read.csv(paste0(dirComp[compID],"/forest_soil/climate/snow_westermoreland/4049483.csv"))
# units in mm
snow$date <- ymd(snow$DATE)
snow$year <- year(snow$date)
snow$doy <- yday(snow$date)
snow$month <- month(snow$date)


atmos$Precip_F <- ifelse(is.na(atmos$PrecipFlag), atmos$Precip, NA)

atmosHourly <- atmos %>%
  group_by(doy, hour, year) %>%
  summarize(SolR = mean(SolRad, na.rm=TRUE),
            nSolR = length(na.omit(SolRad)),
            AirT = mean(AirTemp, na.rm=TRUE),
            nAirT = length(na.omit(AirTemp)),
            VPD = mean(VPD, na.rm=TRUE),
            nVPD = length(na.omit(VPD)),
            Prec = sum(Precip_F))

            
  
atmosDaily <- atmosHourly %>%
  group_by(year, doy) %>%
  summarize(SolRad = mean(SolR, na.rm=TRUE),
            nSolR = length(na.omit(SolR)),
            aveT = mean(AirT, na.rm=TRUE),
            nAveT = length(na.omit(AirT)),
            maxT = max(AirT, na.rm=TRUE),
            minT = min(AirT, na.rm=TRUE),
            VPDd = mean(VPD, na.rm=TRUE),
            nVPD = length(na.omit(VPD)),
            Precip = sum(Prec, na.rm=TRUE),
            nPr = length(na.omit(Prec)))

atmosDaily$SolRad <- ifelse(atmosDaily$nSolR >= 23,atmosDaily$SolRad,NA )
atmosDaily$aveT <- ifelse(atmosDaily$nAveT >= 23,atmosDaily$aveT,NA )
atmosDaily$Precip <- ifelse(atmosDaily$nPr == 24,atmosDaily$Precip,NA )
atmosDaily$minT <- ifelse(atmosDaily$nAveT < 23|atmosDaily$minT == -Inf,NA, atmosDaily$minT)
atmosDaily$maxT <- ifelse(atmosDaily$nAveT < 23|atmosDaily$maxT == Inf,NA,atmosDaily$maxT)
atmosDaily$VPDd <- ifelse(atmosDaily$nVPD >= 23,atmosDaily$VPDd,NA )

#NOAA coop data is recorded at 0700
precip7am <-  atmosHourly %>%
  select(year, doy, hour, Prec)

precip7am$doy7 <- ifelse(precip7am$hour <=7 & leap_year(precip7am$year)==FALSE & precip7am$doy <365, precip7am$doy, 
                  ifelse(precip7am$hour > 7 & leap_year(precip7am$year)==FALSE & precip7am$doy <365,precip7am$doy+1,
                  ifelse(precip7am$hour > 7 & leap_year(precip7am$year)==FALSE & precip7am$doy ==365,1,
                  ifelse(precip7am$hour <=7 & leap_year(precip7am$year)==FALSE & precip7am$doy ==365, precip7am$doy,
                  ifelse(precip7am$hour <=7 & leap_year(precip7am$year)==TRUE & precip7am$doy <366, precip7am$doy, 
                  ifelse(precip7am$hour > 7 & leap_year(precip7am$year)==TRUE & precip7am$doy <366,precip7am$doy+1,
                  ifelse(precip7am$hour > 7 & leap_year(precip7am$year)==TRUE & precip7am$doy ==366,1,
                  ifelse(precip7am$hour <=7 & leap_year(precip7am$year)==TRUE & precip7am$doy ==366, precip7am$doy,NA))))))))

precip7Daily <- precip7am %>%
  group_by(year, doy7) %>%
  summarise(Precip7 = sum(Prec, na.rm=TRUE),
            nPr7 = length(na.omit(Prec)))
precip7Daily$Precip7 <- ifelse(precip7Daily$nPr7 == 24,precip7Daily$Precip7,NA )
  
snowW <- snow %>%
  select(year, doy, PRCP, SNOW, SNWD)

weatherDay1 <- left_join(atmosDaily, snowW, by=c("year", "doy"))
weatherDay <- left_join(weatherDay1, precip7Daily, by=c("year"="year", "doy"="doy7"))

plot(weatherDay$PRCP, weatherDay$Precip7, pch=19)

precipGap <- lm(weatherDay$Precip7 ~ weatherDay$PRCP)
summary(precipGap)
abline(precipGap)
abline(0,1, col="red")

ggplot(weatherDay, aes(PRCP, Precip))+
  geom_point()
# gap fill missing station with westmoreland
weatherDay$Precip_gap <- ifelse(is.na(weatherDay$Precip7),weatherDay$PRCP,weatherDay$Precip7)


rm(list=setdiff(ls(),c("tomst25","weatherDay","atmosHourly","compID","dirComp")))