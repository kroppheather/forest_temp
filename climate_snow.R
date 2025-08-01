library(dplyr)
library(lubridate)
library(ggplot2)


# coop snow and precip data from westmoreland
snow <- read.csv("/Users/hkropp/Library/CloudStorage/GoogleDrive-hkropp@hamilton.edu/My Drive/research/projects/forest_soil/climate/snow_westermoreland/4049483.csv")
# units in mm
snow$date <- ymd(snow$DATE)
snow$year <- year(snow$date)
snow$doy <- yday(snow$date)
snow$month <- month(snow$date)

attrPR <- list()
PR_m <- character()
PR_q <- character()

for(i in 1:nrow(snow)){
  attrPR[[i]] <- strsplit(snow$PRCP_ATTRIBUTES[i],",")
  PR_m[i] <- attrPR[[i]][[1]][1]
  PR_q[i] <- attrPR[[i]][[1]][2]

}

snow$PR_m <- PR_m
snow$PR_q <- PR_q

attrSN <- list()
SN_m <- list()
SN_q <- list()
for(i in 1:nrow(snow)){
  attrSN[[i]] <- strsplit(snow$SNOW_ATTRIBUTES[i],",")
  SN_m[i] <- attrSN[[i]][[1]][1]
  SN_q[i] <- attrSN[[i]][[1]][2]
  
}

attrSD <- list()
SD_m <- list()
SD_q <- list()
for(i in 1:nrow(snow)){
  attrSD[[i]] <- strsplit(snow$SNWD_ATTRIBUTES[i],",")
  SD_m[i] <- attrSD[[i]][[1]][1]
  SD_q[i] <- attrSD[[i]][[1]][2]
  
}

snow$SN_m <- SN_m
snow$SN_q <- SN_q

snow$SD_m <- SD_m
snow$SD_q <- SD_q

unique(snow$PR_q)
unique(snow$SN_q)



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

ggplot(snow_annual, aes(year, tot_precip/1000))+
  geom_point()

######## syracuse climatology -----
syr <- read.csv("/Users/hkropp/Library/CloudStorage/GoogleDrive-hkropp@hamilton.edu/My Drive/research/projects/forest_soil/climate/SYR/4049536.csv")


syr$date <- ymd(syr$DATE)
syr$year <- year(syr$date)
syr$month <- month(syr$date)
syr$doy <- yday(syr$date)

syr_snow_annual <- syr %>%
  group_by(year) %>%
  summarize(max_depth =max(SNWD, na.rm=TRUE),
            tot_acc = sum(SNOW, na.rm=TRUE),
            tot_precip = sum(PRCP,na.rm=TRUE),
            n_obs = n())%>%
  filter(year>1940)
syr$air_ave <- (syr$TMAX+syr$TMIN)/2
ggplot(syr_snow_annual, aes(year, max_depth/1000))+
  geom_point()

ggplot(syr_snow_annual, aes(year, tot_acc/1000))+
  geom_point()
FDD <- function(x){
  sum(x[x<=0],na.rm=TRUE)
}
TDD <- function(x){
  sum(x[x>0],na.rm=TRUE)
}
syr$freezeDays <- ifelse(syr$TMAX <= 0, 1,0)
# 2023 and 2024 had the 2nd and 3rd lowest snow depth since 1940
# top 5 lowest total snow, with 2023 having lowest total snow on record
syr_air_annual <- syr %>%
  group_by(year) %>%
  summarize(tave = mean(air_ave, na.rm=TRUE),
            tmax=max(TMAX, na.rm=TRUE),
            tmin=min(TMIN, na.rm=TRUE),
            nobsT=length(na.omit(air_ave)),
            freezeD = sum(freezeDays, na.rm=TRUE),
            aTDD= TDD(air_ave),
            aFDD=FDD(air_ave))%>%
  filter(nobsT > 360)
syr_air_annual$yr_cnt <- syr_air_annual$year-1940



syr_air_month <- syr %>%
  group_by(year, month) %>%
  summarize(tave = mean(air_ave, na.rm=TRUE),
            tmax=max(TMAX, na.rm=TRUE),
            tmin=min(TMIN, na.rm=TRUE),
            nobsT=length(na.omit(air_ave)),
            freezeD = sum(freezeDays, na.rm=TRUE),
            max_depth =max(SNWD, na.rm=TRUE),
            ave_depth =mean(SNWD, na.rm=TRUE))%>%
  filter(nobsT >27)
syr_air_month$yr_cnt <- syr_air_month$year - 1940


syr_month_rec <- syr_air_month %>%
  group_by(month) %>%
  summarize(avg_tave = mean(tave),
            n_rec = n(),
            avg_freezeD = mean(freezeD),
            avg_maxD = mean(max_depth),
            avg_aveD = mean(ave_depth))


syr_month_anom <- left_join(syr_air_month, syr_month_rec, by="month")
syr_month_anom$air_anom <- syr_month_anom$tave - syr_month_anom$avg_tave
syr_month_anom$d_year <- syr_month_anom$year+((syr_month_anom$month-1)/12)
syr_month_anom$date <- my(paste0(syr_month_anom$month,"-",syr_month_anom$year))
syr_month_anom$avg_snow_anom <- syr_month_anom$ave_depth-syr_month_anom$avg_aveD

ggplot(syr_month_anom%>%filter(year>=2022), aes(x=date, y=air_anom))+
  geom_point()
ggplot(syr_month_anom%>%filter(year>=2022), aes(x=date, y=avg_snow_anom))+
  geom_point()

ggplot(syr_month_anom%>% filter(month==12), aes(x=date, y=air_anom))+
  geom_point()
ggplot(syr_month_anom%>% filter(month==12), aes(x=date, y=avg_snow_anom/1000))+
  geom_point()+ylim(-0.2,0.35)

ggplot(syr_month_anom%>% filter(month==1), aes(x=date, y=avg_snow_anom/1000))+
  geom_point()
ggplot(syr_month_anom%>% filter(month==2), aes(x=date, y=avg_snow_anom/1000))+
  geom_point()
ggplot(syr_month_anom%>% filter(month==3), aes(x=date, y=avg_snow_anom/1000))+
  geom_point()

# examine trends in SYR -----

dec_syr <- syr_air_month %>%
  filter(month == 12 & nobsT>=30)
ggplot(syr_air_annual, aes(year, tave))+
  geom_point()
ann_t <- lm(syr_air_annual$tave~syr_air_annual$yr_cnt)
summary(ann_t)
ggplot(syr_air_annual, aes(year, tmax))+
  geom_point()
ggplot(syr_air_annual, aes(year, tmin))+
  geom_point()
ggplot(syr_air_annual, aes(year, freezeD))+
  geom_point()
ggplot(syr_air_annual, aes(year, aTDD))+
  geom_point()
ggplot(syr_air_annual, aes(year, aFDD))+
  geom_point()
#look at each month and allow one day missing
jan_syr <- syr_air_month %>%
  filter(month == 1 & nobsT>=30)
feb_syr <- syr_air_month %>%
  filter(month == 2 & nobsT>=27)
march_syr <- syr_air_month %>%
  filter(month == 3 & nobsT>=30)
april_syr <- syr_air_month %>%
  filter(month == 4 & nobsT>=29)
may_syr <- syr_air_month %>%
  filter(month == 5 & nobsT>=30)
june_syr <- syr_air_month %>%
  filter(month == 6 & nobsT>=29)
july_syr <- syr_air_month %>%
  filter(month == 7 & nobsT>=30)
august_syr <- syr_air_month %>%
  filter(month == 8 & nobsT>=30)
sept_syr <- syr_air_month %>%
  filter(month == 9 & nobsT>=29)
oct_syr <- syr_air_month %>%
  filter(month == 10 & nobsT>=30)
nov_syr <- syr_air_month %>%
  filter(month == 11 & nobsT>=29)

ggplot(dec_syr, aes(year, tave))+
  geom_point()
dec_lm <- lm(dec_syr$tave ~ dec_syr$yr_cnt)


ggplot(jan_syr, aes(year, tave))+
  geom_point()
jan_lm <- lm(jan_syr$tave ~ jan_syr$yr_cnt)



ggplot(feb_syr, aes(year, tave))+
  geom_point()

feb_lm <- lm(feb_syr$tave ~ feb_syr$yr_cnt)


ggplot(march_syr, aes(year, tave))+
  geom_point()
march_lm <- lm(march_syr$tave ~ march_syr$yr_cnt)


ggplot(april_syr, aes(year, tave))+
  geom_point()
april_lm <- lm(april_syr$tave ~ april_syr$yr_cnt)


ggplot(may_syr, aes(year, tave))+
  geom_point()
may_lm <- lm(may_syr$tave ~ may_syr$yr_cnt)


ggplot(june_syr, aes(year, tave))+
  geom_point()
june_lm <- lm(june_syr$tave ~ june_syr$yr_cnt)


ggplot(july_syr, aes(year, tave))+
  geom_point()
july_lm <- lm(july_syr$tave ~ july_syr$yr_cnt)


ggplot(august_syr, aes(year, tave))+
  geom_point()
august_lm <- lm(august_syr$tave ~ august_syr$yr_cnt)


ggplot(sept_syr, aes(year, tave))+
  geom_point()
sept_lm <- lm(sept_syr$tave ~ sept_syr$yr_cnt)


ggplot(oct_syr, aes(year, tave))+
  geom_point()
oct_lm <- lm(oct_syr$tave ~ oct_syr$yr_cnt)


ggplot(nov_syr, aes(year, tave))+
  geom_point()
nov_lm <- lm(nov_syr$tave ~ nov_syr$yr_cnt)


summary(dec_lm) #sig increase
summary(jan_lm)
summary(feb_lm)
summary(march_lm)
summary(april_lm)
summary(may_lm) # sig increase
summary(june_lm)
summary(july_lm) # sig increase
summary(august_lm) # sig increase
summary(sept_lm) # sig increase
summary(oct_lm)
summary(nov_lm)







