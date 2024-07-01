# script for organizing tomst soil data

##### libraries ----
library(dplyr)
library(ggplot2)
library(lubridate)

##### libraries ----
# read in data

# soil data #

soil <- read.csv("K:/Environmental_Studies/hkropp/projects/canopy_LAI/soil/soil_06_24.csv")

# site label data # 
sites <- read.csv("K:/Environmental_Studies/hkropp/projects/canopy_LAI/2024/site_info.csv")

#canopy analyzer
canopyLAI <-read.csv("K:/Environmental_Studies/hkropp/projects/canopy_LAI/2024/LAI_620.csv")
canopyLAI$PAR_LAI <- canopyLAI$PAR.LAI

#species information
SpeciesInfo <- read.csv("K:/Environmental_Studies/hkropp/projects/canopy_LAI/2024/speciesID.csv")

# weather data


weather <- read.csv("K:/Environmental_Studies/hkropp/projects/canopy_LAI/2024/z6-10463(z6-10463)-1719847974/z6-10463(z6-10463)-Configuration 1-1719847974.3368664.csv",
                    skip=3, header=FALSE)

colnames(weather) <- c("Date","SolRad","Precip","LightningAct","LightningDist","WindDir","WindSpeed",
                       "GustSpeed","AirTemp","VaporPr","AtmosPr","XLevel","YLevel","MaxPrecip",
                       "SensorTemp","VPD","BatPct","BatVolt","RefPr","LogTemp")

############## Format dates ----
soil$TimestampF <- ymd_hm(soil$dateAll)
weather$DateF <- mdy_hms(weather$Date)

############## Join site info ---- 
# join canopy LAI and site info 
# left table is table you want to add site info to
canopyLAIsites <- left_join(canopyLAI, # table with info
                            sites, # table to join
                            by = c("site_id" = "Plot")) #names of comlumns that match


############## Make plots ----
https://www.maths.usyd.edu.au/u/UG/SM/STAT3022/r/current/Misc/data-visualization-2.1.pdf


# make a box plot
# by type
ggplot( canopyLAIsites, #name of data frame
        aes(x = as.factor(Type), y=PAR_LAI))+ # x and y columns to plot
  geom_boxplot() # geometry of plot

ggplot( canopyLAIsites, #name of data frame
        aes(x = as.factor(site_id), y=PAR_LAI, fill= namePerc))+ # fill color in boxes with name labels
  geom_boxplot() # geometry of plot

ggplot( canopyLAIsites, #name of data frame
        aes(x = as.factor(site_id), y=PAR_LAI, fill= Type))+
  geom_boxplot()+
  theme(text = element_text(size = 20),
        axis.text.x = element_text(angle = 90, hjust = 1))+  # geometry of plot
  labs(x= "Plot ID", y="Leaf area per area gound (LAI)")

# make a line plot for soil or weather

ggplot(soil, # data frame
       aes(x=TimestampF, y = SWC, color=location))+
  geom_line()


# make a line and point
ggplot(soil, # data frame
       aes(x=TimestampF, y = SWC, color=location))+
  geom_line()+
  geom_point()

# make a barplot
ggplot(weather, # data frame
       aes(x=DateF, y = Precip))+
  geom_col()+
  ylim(0,5)

hist(weather$Precip)

##### subset time period
# filter by criteria

soilTOPlot <- soil %>%
  filter(month == 6)

ggplot(soilTOPlot, # data frame
       aes(x=TimestampF, y = SWC, color=location))+
  geom_line()+
  geom_point()

soilTOPlot <- soil %>%
  filter(month >= "2024-03-08 11:00:00")


ggplot(soilTOPlot, # data frame
       aes(x=TimestampF, y = SWC, color=location))+
  geom_line()+
  geom_point()


# subset sites
sitestoplot <- canopyLAIsites %>%
  filter(site_id == "RG25" | site_id == "RG01")


