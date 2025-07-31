# script for organizing tomst soil data
# input dirComp and compID
##### libraries ----
library(dplyr)
library(ggplot2)
library(lubridate)

#dirComp <- c("G:/My Drive/research/projects",
 #            "/Users/hkropp/Library/CloudStorage/GoogleDrive-hkropp@hamilton.edu/My Drive/research/projects")
#compID <- 2
##### directories and data files ----

sensors <- read.csv(paste0(dirComp[compID],"/Data/campus_weather/TOMST/sensor inventory.csv"))

soil_t <- read.csv(paste0(dirComp[compID],"/forest_soil/texture/soil_text.csv"))

# enter current date here
currentD <- "09-20-2023"

dirT <- paste0(dirComp[compID],"/Data/campus_weather/TOMST/tomst_all")
# data downloaded in April 
currentD2 <- "04-16-2024"
dirT2 <- paste0(dirComp[compID],"/Data/campus_weather/TOMST/04_16_24")

# data downloaded in April 
currentD3 <- "06-26-2024"
dirT3 <- paste0(dirComp[compID],"/Data/campus_weather/TOMST/06_26_24")

# data downloaded in Oct
currentD4 <- "10-18-2024"
dirT4 <- paste0(dirComp[compID],"/Data/campus_weather/TOMST/10_18_24")

# data downloaded in March
currentD5 <- "03-20-2025"
dirT5 <- paste0(dirComp[compID],"/Data/campus_weather/TOMST/03_20_25")

# data downloaded in July
currentD6 <- "07-22-2025"
dirT6 <- paste0(dirComp[compID],"/Data/campus_weather/TOMST/07_22_25")



tomstF <- list.files(paste0(dirT))
fileSN <- character()
for(i in 1:length(tomstF)){
  fileSN[i] <- as.numeric(strsplit(tomstF, "_")[[i]][2])
}
fileSNn <- as.numeric(fileSN)

##### soil texture data ----
text_plot <- soil_t %>%
  group_by(Plot,location) %>%
  summarise(perc_org=mean(prec_organic),
    sand = mean(sand),
            silt=mean(silt))
  
text_plot$clay <- 100-text_plot$sand-text_plot$silt

#write.csv(text_plot, paste0(dirComp[compID],"/forest_soil/texture/plot_texture.csv"))
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



# read in new data from D3

tomstF3 <- list.files(paste0(dirT3))
fileSN3 <- character()
sensors3ID <- character()
for(i in 1:length(tomstF3)){
  fileSN3[i] <- as.numeric(strsplit(tomstF3, "_")[[i]][2])
}
fileSNn3 <- as.numeric(fileSN3)

sensDF3 <- data.frame(sensor_sid = fileSNn3)

sensors3 <- sensors %>% filter(end_date == "current")


sensors3$start_dateF <- mdy("04-16-2024")

sensors3$end_dateF <- mdy("06-25-2024")

sensors3 <- inner_join(sensors3, sensDF3, by="sensor_sid")

# read in files
datT3 <- list()

for(i in 1: length(fileSN3)){
  datT3[[i]] <- read.csv(paste0(dirT3,"/",tomstF3[i]), sep=";",
                         header=FALSE)[,1:9]
  colnames(datT3[[i]])[1:9] <- c("record","date","tz","Tm6","T2","T15","SM","shake","errFlag")
  datT3[[i]]$SN <- rep(fileSN3[i], nrow(datT3[[i]]))
  
  datT3[[i]]$dateF <- ymd_hm(datT3[[i]]$date) 
  datT3[[i]]$estD <- with_tz(datT3[[i]]$dateF, tzone="America/New_York")
}


sensorInfoL3 <- list()

for(i in 1:length(fileSNn3)){
  sensorInfoL3[[i]] <- sensors3 %>%
    filter(sensors3$sensor_sid == fileSNn3[i])
  
}

tail(datT3[[1]])


for(i in 1:length(fileSNn3)){
  datT3[[i]]$location <- rep("omit",nrow(datT3[[i]]))
  datT3[[i]]$Plot <- rep("omit",nrow(datT3[[i]]))
  for(j in 1:nrow(sensorInfoL3[[i]])){
    
    datT3[[i]]$location <- ifelse(datT3[[i]]$estD > sensorInfoL3[[i]]$start_dateF[j] & 
                                    datT3[[i]]$estD < sensorInfoL3[[i]]$end_dateF[j],
                                  sensorInfoL3[[i]]$location[j],datT3[[i]]$location)
    
    datT3[[i]]$Plot <- ifelse(datT3[[i]]$estD > sensorInfoL3[[i]]$start_dateF[j] & 
                                datT3[[i]]$estD < sensorInfoL3[[i]]$end_dateF[j],
                              sensorInfoL3[[i]]$Plot.name[j],datT3[[i]]$Plot)
  }
}  



# read in new data from D4

tomstF4 <- list.files(paste0(dirT4))
fileSN4 <- character()
sensors4ID <- character()
for(i in 1:length(tomstF4)){
  fileSN4[i] <- as.numeric(strsplit(tomstF4, "_")[[i]][2])
}
fileSNn4 <- as.numeric(fileSN4)

sensDF4 <- data.frame(sensor_sid = fileSNn4)

sensors4 <- sensors %>% filter(end_date == "current")


sensors4$start_dateF <- mdy("06-26-2024")

sensors4$end_dateF <- mdy("10-17-2024")

sensors4 <- inner_join(sensors4, sensDF4, by="sensor_sid")

# read in files
datT4 <- list()

for(i in 1: length(fileSN4)){
  datT4[[i]] <- read.csv(paste0(dirT4,"/",tomstF4[i]), sep=";",
                         header=FALSE)[,1:9]
  colnames(datT4[[i]])[1:9] <- c("record","date","tz","Tm6","T2","T15","SM","shake","errFlag")
  datT4[[i]]$SN <- rep(fileSN4[i], nrow(datT4[[i]]))
  
  datT4[[i]]$dateF <- ymd_hm(datT4[[i]]$date) 
  datT4[[i]]$estD <- with_tz(datT4[[i]]$dateF, tzone="America/New_York")
}


sensorInfoL4 <- list()

for(i in 1:length(fileSNn4)){
  sensorInfoL4[[i]] <- sensors4 %>%
    filter(sensors4$sensor_sid == fileSNn4[i])
  
}

tail(datT4[[1]])


for(i in 1:length(fileSNn4)){
  datT4[[i]]$location <- rep("omit",nrow(datT4[[i]]))
  datT4[[i]]$Plot <- rep("omit",nrow(datT4[[i]]))
  for(j in 1:nrow(sensorInfoL4[[i]])){
    
    datT4[[i]]$location <- ifelse(datT4[[i]]$estD > sensorInfoL4[[i]]$start_dateF[j] & 
                                    datT4[[i]]$estD < sensorInfoL4[[i]]$end_dateF[j],
                                  sensorInfoL4[[i]]$location[j],datT4[[i]]$location)
    
    datT4[[i]]$Plot <- ifelse(datT4[[i]]$estD > sensorInfoL4[[i]]$start_dateF[j] & 
                                datT4[[i]]$estD < sensorInfoL4[[i]]$end_dateF[j],
                              sensorInfoL4[[i]]$Plot.name[j],datT4[[i]]$Plot)
  }
}  

# read in new data from D5

tomstF5 <- list.files(paste0(dirT5))
fileSN5 <- character()
sensors5ID <- character()
for(i in 1:length(tomstF5)){
  fileSN5[i] <- as.numeric(strsplit(tomstF5, "_")[[i]][2])
}
fileSNn5 <- as.numeric(fileSN5)

sensDF5 <- data.frame(sensor_sid = fileSNn5)

sensors5 <- sensors %>% filter(end_date == "current")


sensors5$start_dateF <- mdy("10-18-2024")

sensors5$end_dateF <- mdy("03-19-2025")

sensors5 <- inner_join(sensors5, sensDF5, by="sensor_sid")

# read in files
datT5 <- list()

for(i in 1: length(fileSN5)){
  datT5[[i]] <- read.csv(paste0(dirT5,"/",tomstF5[i]), sep=";",
                         header=FALSE)[,1:9]
  colnames(datT5[[i]])[1:9] <- c("record","date","tz","Tm6","T2","T15","SM","shake","errFlag")
  datT5[[i]]$SN <- rep(fileSN5[i], nrow(datT5[[i]]))
  
  datT5[[i]]$dateF <- ymd_hm(datT5[[i]]$date) 
  datT5[[i]]$estD <- with_tz(datT5[[i]]$dateF, tzone="America/New_York")
}


sensorInfoL5 <- list()

for(i in 1:length(fileSNn5)){
  sensorInfoL5[[i]] <- sensors5 %>%
    filter(sensors5$sensor_sid == fileSNn5[i])
  
}

tail(datT5[[1]])


for(i in 1:length(fileSNn5)){
  datT5[[i]]$location <- rep("omit",nrow(datT5[[i]]))
  datT5[[i]]$Plot <- rep("omit",nrow(datT5[[i]]))
  for(j in 1:nrow(sensorInfoL5[[i]])){
    
    datT5[[i]]$location <- ifelse(datT5[[i]]$estD > sensorInfoL5[[i]]$start_dateF[j] & 
                                    datT5[[i]]$estD < sensorInfoL5[[i]]$end_dateF[j],
                                  sensorInfoL5[[i]]$location[j],datT5[[i]]$location)
    
    datT5[[i]]$Plot <- ifelse(datT5[[i]]$estD > sensorInfoL5[[i]]$start_dateF[j] & 
                                datT5[[i]]$estD < sensorInfoL5[[i]]$end_dateF[j],
                              sensorInfoL5[[i]]$Plot.name[j],datT5[[i]]$Plot)
  }
}  

# read in new data from D6
#missing one reforestation sensor

tomstF6 <- list.files(paste0(dirT6))
fileSN6 <- character()
sensors6ID <- character()
for(i in 1:length(tomstF6)){
  fileSN6[i] <- as.numeric(strsplit(tomstF6, "_")[[i]][2])
}
fileSNn6 <- as.numeric(fileSN6)

sensDF6 <- data.frame(sensor_sid = fileSNn6)

sensors6 <- sensors %>% filter(end_date == "current")


sensors6$start_dateF <- mdy("03-20-2025")

sensors6$end_dateF <- mdy("07-21-2025")

sensors6 <- inner_join(sensors6, sensDF6, by="sensor_sid")

# read in files
datT6 <- list()

for(i in 1: length(fileSN6)){
  datT6[[i]] <- read.csv(paste0(dirT6,"/",tomstF6[i]), sep=";",
                         header=FALSE)[,1:9]
  colnames(datT6[[i]])[1:9] <- c("record","date","tz","Tm6","T2","T15","SM","shake","errFlag")
  datT6[[i]]$SN <- rep(fileSN6[i], nrow(datT6[[i]]))
  
  datT6[[i]]$dateF <- ymd_hm(datT6[[i]]$date) 
  datT6[[i]]$estD <- with_tz(datT6[[i]]$dateF, tzone="America/New_York")
}


sensorInfoL6 <- list()

for(i in 1:length(fileSNn6)){
  sensorInfoL6[[i]] <- sensors6 %>%
    filter(sensors6$sensor_sid == fileSNn6[i])
  
}

tail(datT6[[1]])


for(i in 1:length(fileSNn6)){
  datT6[[i]]$location <- rep("omit",nrow(datT6[[i]]))
  datT6[[i]]$Plot <- rep("omit",nrow(datT6[[i]]))
  for(j in 1:nrow(sensorInfoL6[[i]])){
    
    datT6[[i]]$location <- ifelse(datT6[[i]]$estD > sensorInfoL6[[i]]$start_dateF[j] & 
                                    datT6[[i]]$estD < sensorInfoL6[[i]]$end_dateF[j],
                                  sensorInfoL6[[i]]$location[j],datT6[[i]]$location)
    
    datT6[[i]]$Plot <- ifelse(datT6[[i]]$estD > sensorInfoL6[[i]]$start_dateF[j] & 
                                datT6[[i]]$estD < sensorInfoL6[[i]]$end_dateF[j],
                              sensorInfoL6[[i]]$Plot.name[j],datT6[[i]]$Plot)
  }
}  


# combine all data

tomstUL <- do.call("rbind",datT)

tomstUL2 <- do.call("rbind",datT2)
tomstUL3 <- do.call("rbind",datT3)
tomstUL4 <- do.call("rbind",datT4)
tomstUL5 <- do.call("rbind",datT5)
tomstUL6 <- do.call("rbind",datT6)

tomstp1 <- tomstUL %>%
  filter(location != "omit")

tomstp2 <- tomstUL2 %>%
  filter(location != "omit")
tomstp3 <- tomstUL3 %>%
  filter(location != "omit")
tomstp4 <- tomstUL4 %>%
  filter(location != "omit")
tomstp5 <- tomstUL5 %>%
  filter(location != "omit")
tomstp6 <- tomstUL6 %>%
  filter(location != "omit")

tomst1 <- rbind(tomstp1,tomstp2)
tomst2 <- rbind(tomst1,tomstp3)
tomst3 <- rbind(tomst2,tomstp4)
tomst4 <- rbind(tomst3,tomstp5)
tomst <- rbind(tomst4,tomstp6)

tomst$Tm6 <- as.numeric(gsub("\\,","\\.", tomst$Tm6))
tomst$T2 <- as.numeric(gsub("\\,","\\.", tomst$T2))
tomst$T15 <- as.numeric(gsub("\\,","\\.", tomst$T15))
# silty loam moisture calibration all sites are silt loam
tomst$SMcor <- (1.70E-8*(tomst$SM^2)) + (1.18E-4*tomst$SM) -0.1011

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
# 
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

tomstDay <- tomst %>%
  group_by(location,Plot, year,doy) %>%
  summarise(Tsoil6 = mean(Tm6, na.rm=TRUE),
            Tsurf2 = mean(T2, na.rm=TRUE),
            Tsurf15 = mean(T15, na.rm=TRUE),
            SWC = mean(SMcor, na.rm=TRUE))

tomstDayLocation <- na.omit(tomstDay) %>%
  group_by(location, year,doy) %>%
  summarise(Tsoil_6 = mean(Tsoil6),
            Tsoil_6sd = sd(Tsoil6),
            Tsurf_2 = mean(Tsurf2),
            Tsurf_15 = mean(Tsurf15),
            SWC_12 = mean(SWC),
            Tsurf_2sd = sd(Tsurf2),
            Tsurf_15sd = sd(Tsurf15),
            SWC_12sd = sd(SWC),
            nobs = n())
#%>% # view reforestation two sensors for now
  #filter(nobs ==3)
tomstDayLocation$DD <- ifelse(leap_year(tomstDayLocation$year),((tomstDayLocation$doy-1)/366)+tomstDayLocation$year,
                              ((tomstDayLocation$doy-1)/365)+tomstDayLocation$year)

tomst25 <- tomstLocation %>%
  filter(location == "hemlock sapflow" |
           location == "maple-beech" |
           location == "Rogers reforestation" |
           location == "Spruce RG09" |
           location == "Buckthorn RG03" ) %>%
  filter(year >= 2022 )


rm(list=setdiff(ls(),c("tomst25","dirComp","compID")))

