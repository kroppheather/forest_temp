library(dplyr)
library(lubridate)
library(ggplot2)


####### Geneva NY ----------
# geneva soil
gfiles <- list.files("/Users/hkropp/Library/CloudStorage/GoogleDrive-hkropp@hamilton.edu/My Drive/research/projects/forest_soil/long_term/geneva",
                     full.names = TRUE)
yearly_gen <- list()
org_df <- list()
for(i in 1:length(gfiles)){
  yearly_gen[[i]] <- read.csv(gfiles[i], skip=6, na.strings=c("NA","-99.9"))
  org_df [[i]] <- data.frame(yearly_gen[[i]][,2],
                             yearly_gen[[i]][,5],
                             yearly_gen[[i]][,6],
                             yearly_gen[[i]][,7])
  
}
gen_all <- do.call("rbind", org_df)
geneva <- data.frame(date=rep(gen_all[,1], times=3),
                     soilT=c(gen_all[,2],gen_all[,3],gen_all[,4]),
                     depth=rep(c(2,4,8), each=nrow(gen_all)))

geneva$dateF <- ymd(geneva$date)

geneva$year <- year(geneva$dateF)



ggplot(geneva, aes(dateF, soilT, color=as.factor(depth)))+
  geom_line()
# geneva met
dir_gmet <- "/Users/hkropp/Library/CloudStorage/GoogleDrive-hkropp@hamilton.edu/My Drive/research/projects/forest_soil/climate/snow_met_geneva"
gmet1 <- read.csv(paste0(dir_gmet, "/3995321.csv"))
gmet2 <- read.csv(paste0(dir_gmet, "/3995326.csv"))
gmet3 <- read.csv(paste0(dir_gmet, "/3995333.csv"))
gmet4 <- read.csv(paste0(dir_gmet, "/3995335.csv"))
gmet <- rbind(gmet1,gmet2,gmet3,gmet4)
gmet$dateF <- ymd(gmet$DATE)
gmet$TAVE <- (gmet$TMAX+gmet$TMIN)/2
gmet$year <- year(gmet$dateF)

# annual analysis
met_annual <- gmet %>%
  group_by(year) %>%
  summarise(MAT=mean(TAVE, na.rm=TRUE),
            MAMaxT=mean(TMAX, na.rm=TRUE),
            MAMinT=mean(TMIN, na.rm=TRUE),
            minT=min(TMIN, na.rm=TRUE),
            maxT=max(TMAX, na.rm=TRUE),
            nobsT=length(na.omit(TAVE)),
            maxSnow= max(SNWD, na.rm=TRUE),
            totPrecp=sum(PRCP, na.rm=TRUE),
            nobsP=length(na.omit(PRCP)))%>%
  filter(nobsT>340)

# summarize soil
soil_annual <- geneva %>%
  group_by(year, depth) %>%
  summarise(soilAve = mean(soilT, na.rm=TRUE),
            soilMax = max(soilT, na.rm=TRUE),
            soilMin =min(soilT,na.rm=TRUE),
            nobs=length(na.omit(soilT))
            )%>%
  filter(nobs>340)
soil8 <- soil_annual %>% filter(depth == 8)
ggplot(met_annual, aes(year,MAT))+
  geom_point()
ggplot(soil8, aes(year,soilAve))+
  geom_point()
ggplot(soil8, aes(year,soilMin))+
  geom_point()
ggplot(soil8, aes(year,soilMax))+
  geom_point()

ggplot(met_annual, aes(year,minT))+
  geom_point()
ggplot(met_annual, aes(year,maxT))+
  geom_point()
ggplot(met_annual, aes(year,maxSnow))+
  geom_point()
met_annual$yr_cnt <- met_annual$year-1990
aveTrend <- lm(met_annual$MAT ~ met_annual$yr_cnt)
summary(aveTrend)
maxTrend <- lm(met_annual$maxT ~ met_annual$yr_cnt)
summary(maxTrend)
minTrend <- lm(met_annual$minT ~ met_annual$yr_cnt)
summary(minTrend)
snwTrend <- lm(met_annual$maxSnow ~ met_annual$yr_cnt)
summary(snwTrend)
soil8$yr_cnt <- soil8$year -1999
s_aveTrend <- lm(soil8$soilAve ~ soil8$yr_cnt)
summary(s_aveTrend)
s_minTrend <- lm(soil8$soilMin ~ soil8$yr_cnt)
summary(s_minTrend)
s_maxTrend <- lm(soil8$soilMax ~ soil8$yr_cnt)
summary(s_maxTrend)


####### Lye Brook VT


######## Lye Brook VT------
lbfiles <- list.files("/Users/hkropp/Library/CloudStorage/GoogleDrive-hkropp@hamilton.edu/My Drive/research/projects/forest_soil/long_term/Lye_Brook_VT",
                     full.names = TRUE)

# columns change over years need to fix
yearly_lb <- list()
org_df_lb <- list()
for(i in 1:length(gfiles)){
  yearly_lb[[i]] <- read.csv(lbfiles[i], skip=6, na.strings=c("NA","-99.9"))
  org_df_lb[[i]] <- data.frame(yearly_lb[[i]][,2],
                             yearly_lb[[i]][,5],
                             yearly_lb[[i]][,7],
                             yearly_lb[[i]][,8],
                             yearly_lb[[i]][,9],
                             yearly_lb[[i]][,10],
                             yearly_lb[[i]][,16],
                             yearly_lb[[i]][,17],
                             yearly_lb[[i]][,18])
  
}
lb_all <- do.call("rbind",org_df_lb)

colnames(lb_all) <- c("date","PREC","TMAX","TMIN","TAVE","SNWD","Tsoil2","Tsoil4","Tsoil8")
lb_all$dateF <- ymd(lb_all$date)
lb_all$year <- year(lb_all$dateF)
ggplot(lb_all, aes(dateF, Tsoil4))+
  geom_point()

