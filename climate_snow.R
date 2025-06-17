library(dplyr)
library(lubridate)
library(ggplot2)

# coop snow data from westermoreland
snow <- read.csv("/Users/hkropp/Library/CloudStorage/GoogleDrive-hkropp@hamilton.edu/My Drive/research/projects/forest_soil/climate/snow_westermoreland/4049483.csv")
# units in mm
snow$date <- ymd(snow$DATE)
snow$year <- year(snow$date)
snow$doy <- yday(snow$date)
snow$month <- month(snow$date)

ggplot(snow, aes(date,SNWD/1000))+
  geom_line()

snow_annual <- snow %>%
  group_by(year) %>%
  summarize(max_depth =max(SNWD, na.rm=TRUE),
            tot_acc = sum(SNOW, na.rm=TRUE),
            tot_precip = sum(PRCP,na.rm=TRUE),
            n_obs = n())

ggplot(snow_annual, aes(year, max_depth))+
  geom_point()
