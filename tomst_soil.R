# script for organizing tomst soil data

##### libraries ----
library(dplyr)
library(ggplot2)
library(lubridate)


##### directories and data files ----
# enter current date here
currentD <- "09-20-2023"

dirT <- "G:/My Drive/research/projects/Data/campus_weather/TOMST/tomst_all"
# data downloaded in April 
currentD2 <- "04-16-2024"
dirT2 <- "G:/My Drive/research/projects/Data/campus_weather/TOMST/04_16_24"

tomstF <- list.files(paste0(dirT))
fileSN <- character()
for(i in 1:length(tomstF)){
  fileSN[i] <- as.numeric(strsplit(tomstF, "_")[[i]][2])
}
fileSNn <- as.numeric(fileSN)

sensors <- read.csv("G:/My Drive/research/projects/Data/campus_weather/TOMST/sensor inventory.csv")

##### read in data files and format for R ----

# read in files
datT <- list()

for(i in 1: length(fileSN)){
  datT[[i]] <- read.csv(paste0(dirT,"/",tomstF[i]), sep=";",
                        header=FALSE)[,1:9]
  colnames(datT[[i]])[1:9] <- c("record","date","tz","Tm6","T2","T15","SM","shake","errFlag")
  datT[[i]]$SN <- rep(fileSN[i], nrow(datT[[i]]))
  
  datT[[i]]$dateF <- ymd_hm(datT[[i]]$date) 
  datT[[i]]$estD <- with_tz(datT[[i]]$dateF, tzone="America/New_York")
}


##### organize sensor inventory info ----

sensors$start_dateF <- mdy(sensors$start_date)
sensors$end_dateC <- ifelse(sensors$end_date == "current",currentD,
                           sensors$end_date)
sensors$end_dateF <- mdy(sensors$end_dateC)

sensorInfoL <- list()

for(i in 1:length(fileSNn)){
  sensorInfoL[[i]] <- sensors %>%
    filter(sensors$sensor_sid == fileSNn[i])
  
}

sensorInfoL[[10]]
lastObs <- list()

for(i in 1:length(fileSNn)){
  datT[[i]]$location <- rep("omit",nrow(datT[[i]]))
  datT[[i]]$Plot <- rep("omit",nrow(datT[[i]]))
  for(j in 1:nrow(sensorInfoL[[i]])){
    
    datT[[i]]$location <- ifelse(datT[[i]]$estD > sensorInfoL[[i]]$start_dateF[j] & 
                                 datT[[i]]$estD < sensorInfoL[[i]]$end_dateF[j],
                                 sensorInfoL[[i]]$location[j],datT[[i]]$location)
    
    datT[[i]]$Plot <- ifelse(datT[[i]]$estD > sensorInfoL[[i]]$start_dateF[j] & 
                                   datT[[i]]$estD < sensorInfoL[[i]]$end_dateF[j],
                                 sensorInfoL[[i]]$Plot.name[j],datT[[i]]$Plot)
    lastObs[[i]] <- datT[[i]][nrow(datT[[i]]),]
  }
}  


# read in new data from D2

tomstF2 <- list.files(paste0(dirT2))
fileSN2 <- character()
sensors2ID <- character()
for(i in 1:length(tomstF2)){
  fileSN2[i] <- as.numeric(strsplit(tomstF2, "_")[[i]][2])
}
fileSNn2 <- as.numeric(fileSN2)

sensDF2 <- data.frame(sensor_sid = fileSNn2)

sensors2 <- sensors %>% filter(end_date == "current")


sensors2$start_dateF <- mdy("09-20-2023")

sensors2$end_dateF <- mdy("04-16-2024")

sensors2 <- inner_join(sensors2, sensDF2, by="sensor_sid")

# read in files
datT2 <- list()

for(i in 1: length(fileSN2)){
  datT2[[i]] <- read.csv(paste0(dirT2,"/",tomstF2[i]), sep=";",
                        header=FALSE)[,1:9]
  colnames(datT2[[i]])[1:9] <- c("record","date","tz","Tm6","T2","T15","SM","shake","errFlag")
  datT2[[i]]$SN <- rep(fileSN2[i], nrow(datT2[[i]]))
  
  datT2[[i]]$dateF <- ymd_hm(datT2[[i]]$date) 
  datT2[[i]]$estD <- with_tz(datT2[[i]]$dateF, tzone="America/New_York")
}


sensorInfoL2 <- list()

for(i in 1:length(fileSNn2)){
  sensorInfoL2[[i]] <- sensors2 %>%
    filter(sensors2$sensor_sid == fileSNn2[i])
  
}

tail(datT2[[1]])


for(i in 1:length(fileSNn2)){
  datT2[[i]]$location <- rep("omit",nrow(datT2[[i]]))
  datT2[[i]]$Plot <- rep("omit",nrow(datT2[[i]]))
  for(j in 1:nrow(sensorInfoL2[[i]])){
    
    datT2[[i]]$location <- ifelse(datT2[[i]]$estD > sensorInfoL2[[i]]$start_dateF[j] & 
                                   datT2[[i]]$estD < sensorInfoL2[[i]]$end_dateF[j],
                                 sensorInfoL2[[i]]$location[j],datT2[[i]]$location)
    
    datT2[[i]]$Plot <- ifelse(datT2[[i]]$estD > sensorInfoL2[[i]]$start_dateF[j] & 
                               datT2[[i]]$estD < sensorInfoL2[[i]]$end_dateF[j],
                             sensorInfoL2[[i]]$Plot.name[j],datT2[[i]]$Plot)
  }
}  


tomstUL <- do.call("rbind",datT)

tomstUL2 <- do.call("rbind",datT2)

tomstp1 <- tomstUL %>%
  filter(location != "omit")

tomstp2 <- tomstUL2 %>%
  filter(location != "omit")

tomst <- rbind(tomstp1,tomstp2)

tomst$Tm6 <- as.numeric(gsub("\\,","\\.", tomst$Tm6))
tomst$T2 <- as.numeric(gsub("\\,","\\.", tomst$T2))
tomst$T15 <- as.numeric(gsub("\\,","\\.", tomst$T15))
tomst$SMcor <- (-0.00000002*(tomst$SM^2)) + (0.0003*tomst$SM) -0.2062

# extra filter SM less than zero will be a sensor pulled out

tomst <- tomst %>%
  filter(SMcor >0)

# get date info
tomst$doy <- yday(tomst$estD) 
tomst$year <- year(tomst$estD)
tomst$hour <- hour(tomst$estD) 

# average for every hour
tomstHour <- tomst %>%
  group_by(location,Plot,year,doy,hour) %>%
  summarise(Tsoil6 = mean(Tm6, na.rm=TRUE),
            Tsurf2 = mean(T2, na.rm=TRUE),
            Tsurf15 = mean(T15, na.rm=TRUE),
            SWC = mean(SMcor, na.rm=TRUE))
# no leap year yet
tomstHour$DH <- tomstHour$doy + (tomstHour$hour/24)
tomstHour$DD <- ifelse(leap_year(tomstHour$year),
                           tomstHour$year + ((tomstHour$DH-1)/366),
                       tomstHour$year + ((tomstHour$DH-1)/365))


tomstLocation <- tomstHour %>%
  group_by(location,year,doy,hour,DD) %>%
  summarise(Tsoil6 = mean(Tsoil6, na.rm=TRUE),
            Tsurf2 = mean(Tsurf2, na.rm=TRUE),
            Tsurf15 = mean(Tsurf15, na.rm=TRUE),
            SWC = mean(SWC, na.rm=TRUE))


ggplot(tomstHour, aes(DD, Tsoil6, color=location))+
  geom_line()

ggplot(tomstLocation, aes(DD, Tsoil6, color=location))+
  geom_line()

ggplot(tomstLocation %>% filter(location == "weather station"),
       aes(DD, Tsoil6, color=location))+
  geom_line()

ggplot(tomstLocation %>% filter(location == "Buckthorn RG03"),
       aes(DD, Tsoil6, color=location))+
  geom_line()

ggplot(tomstLocation %>% filter(location == "hemlock sapflow"),
       aes(DD, Tsoil6, color=location))+
  geom_line()

ggplot(tomstLocation %>% filter(location == "maple-beech"),
       aes(DD, Tsoil6, color=location))+
  geom_line()

ggplot(tomstLocation %>% filter(location == "Rogers reforestation"),
       aes(DD, Tsoil6, color=location))+
  geom_line()+
  geom_point()

ggplot(tomstLocation %>% filter(location == "Rogers reforestation"& DD >2023.45 & DD < 2023.5),
       aes(DD, Tsoil6, color=location))+
  geom_line()+
  geom_point()


ggplot(tomstLocation %>% filter(location == "mowed lawn"),
       aes(DD, Tsoil6, color=location))+
  geom_line()
ggplot(tomstLocation %>% filter(location == "ag field"),
       aes(DD, Tsoil6, color=location))+
  geom_line()

ggplot(tomstLocation %>% filter(DD >= 2022.5),
       aes(DD, Tsoil6, color=location))+
  geom_line()

ggplot(tomstLocation %>% filter(DD >= 2023.8),
       aes(DD, SWC, color=location))+
  geom_line()

ggplot(tomstLocation %>% filter(DD >= 2023.8),
       aes(DD, Tsoil6, color=location))+
  geom_line()


ggplot(tomstLocation %>% filter(location == "Buckthorn RG03"|
                                  location == "Spruce RG09"|
                                  location == "hemlock sapflow"|
                                  location == "Rogers reforestation"|
                                  location == "maple-beech" ),
       aes(DD, Tsoil6, color=location))+
  geom_line()


RR <- tomstLocation %>% filter(location == "Rogers reforestation")
RRtest <- RR %>% filter(doy == 185 & year==2023)
