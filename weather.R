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
atmos <- read.csv(paste0(dirComp[compID],"/forest_soil/climate/weather_campus/v1.8/Atmos41_weather.csv"))


# coop snow and precip data from westmoreland
  snow <- read.csv(paste0(dirComp[compID],"/forest_soil/climate/snow_westermoreland/11_25_25/4175415.csv"))
# units in mm
snow$date <- ymd(snow$DATE)
snow$year <- year(snow$date)
snow$doy <- yday(snow$date)
snow$month <- month(snow$date)

romeWeather <- read.csv(paste0(dirComp[compID],"/forest_soil/climate/Rome/4175652.csv"))
rome <- romeWeather %>%
  filter(REPORT_TYPE == "SOD") %>%
  select(DATE,DailyPrecipitation,DailyWeather)
rome$date <- ymd_hms(rome$DATE)
rome$year <- year(rome$date)
rome$doy <- yday(rome$date)
rome$rome_rain_ID <- ifelse(grepl("RA", rome$DailyWeather),1,0)
rome$rome_snow_ID <- ifelse(grepl("SN", rome$DailyWeather),1,0)
rome$rome_precip <- ifelse(rome$DailyPrecipitation == "T",0,as.numeric(rome$DailyPrecipitation))
rome <- rome %>%
  select(year,doy, rome_precip, rome_rain_ID, rome_snow_ID)

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


# gap fill missing station with westmoreland
weatherDay$Precip_gap <- ifelse(is.na(weatherDay$Precip7),weatherDay$PRCP,weatherDay$Precip7)

weatherDay <- left_join(weatherDay, rome, by=c("year","doy"))

# Westmoreland to precip station
precipGap <- lm(weatherDay$Precip7 ~ weatherDay$PRCP)
plot(weatherDay$PRCP, weatherDay$Precip7, pch=19)
summary(precipGap)
abline(precipGap)
abline(0,1, col="red")
# Rome to precip station
precipGap2 <- lm(weatherDay$Precip ~ weatherDay$rome_precip)
plot(weatherDay$rome_precip, weatherDay$Precip, pch=19)
summary(precipGap2)
abline(precipGap2)
abline(0,1, col="red")

# record rain on snow
weatherDay$rain_snow <- ifelse(weatherDay$rome_rain_ID == 1 & weatherDay$SNWD > 0 &weatherDay$Precip_gap > 1,1,0  )

#westmoreland vs weather station
ggplot(weatherDay, aes(Precip7, PRCP))+
  geom_point()
ggplot(weatherDay, aes(Precip, rome_precip))+
  geom_point()

rm(list=setdiff(ls(),c("tomst25","weatherDay","atmosHourly","compID","dirComp")))