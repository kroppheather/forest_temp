##### directories & libraries ----
library(dplyr)
library(lubridate)
library(ggplot2)
library(rjags)
library(MCMCvis)

dirComp <- c("G:/My Drive/research/projects",
             "/Users/hkropp/Library/CloudStorage/GoogleDrive-hkropp@hamilton.edu/My Drive/research/projects")
compID <- 2

source("/Users/hkropp/Documents/GitHub/forest_temp/tomst_soil.r")
source("/Users/hkropp/Documents/GitHub/forest_temp/weather.r")

plotDir <- "/Users/hkropp/Library/CloudStorage/GoogleDrive-hkropp@hamilton.edu/My Drive/research/projects/forest_soil/data_analysis"

##### aggregate all data to daily level and join with weather ----

tomstDay <- tomst25

airDay <- weatherDay %>%
  select(doy, year, aveT, maxT, minT)

soilAll <- left_join(tomstDay, airDay, by=c("year","doy"))

soilAll$TempDiff <- soilAll$Tsoil_6 - soilAll$aveT 
soilAll$date <- as.Date(soilAll$doy-1, origin=paste0(soilAll$year, "-01-01"))


weatherDay$date <- as.Date(weatherDay$doy-1, origin=paste0(weatherDay$year, "-01-01"))
weatherJoin <- weatherDay %>%
  select(!c(aveT,nAveT,maxT,minT))
soilmet <- left_join(soilAll, weatherJoin, by=c("doy","year","date"))

##### VWC and frozen soils ----

# gap fill freezing soil data from measurements before freeze
soilmet$freezeSoil <- ifelse(soilmet$Tsoil_6<0,1,0)

soilMet <- soilmet %>%
  arrange(location, year,doy)

freezeCheck<- soilMet %>%
  filter(freezeSoil == 1)
# get all freeze events
freezeEvent <- c(1)
for(i in 2:nrow(freezeCheck)){
  if(freezeCheck$location[i-1] == freezeCheck$location[i] &
     freezeCheck$year[i-1] == freezeCheck$year[i] & 
     freezeCheck$doy[i-1] == (freezeCheck$doy[i]-1)){
    freezeEvent[i] <- freezeEvent[i-1]
    
  }else{
    freezeEvent[i] <-freezeEvent[i-1]+1}
  
}
# label start of an event
freezeStart <- c(1)
for(i in 2:nrow(freezeCheck)){
  if(freezeEvent[i-1]!=freezeEvent[i]){
    freezeStart[i] <- 1
  }else{
    freezeStart[i] <- 0
  }
}

# get the day before the freeze
freezeLastDay <-  freezeCheck$doy[1]-1
for(i in 2:nrow(freezeCheck)){
  if(freezeStart[i] == 1){
    freezeLastDay[i] <- freezeCheck$doy[i]-1
  }else{
    freezeLastDay[i] <- freezeLastDay[i-1]
  }
}

freezeLastDayYear <-  freezeCheck$year[1]
for(i in 2:nrow(freezeCheck)){
  if(freezeStart[i] == 1){
    freezeLastDayYear[i] <- freezeCheck$year[i]
  }else{
    freezeLastDayYear[i] <- freezeLastDayYear[i-1]
  }
}

freezeCheck$freezeStart <- freezeStart

freezeCheck$freezeEvent <- freezeEvent

freezeCheck$freezeLastDayYear <- freezeLastDayYear
freezeCheck$freezeLastDay <- freezeLastDay




# use water year starting Oct 1 2023
soilDat <- soilMet %>%
  filter(DD>=2022.747)

soilDat$locID <- ifelse(soilDat$location == "maple-beech", 1, #Deciduous forest
                        ifelse(soilDat$location == "hemlock sapflow", 2, #Conifer-deciduous forest
                               ifelse(soilDat$location == "Spruce RG09", 3, #Conifer monoculture
                                      ifelse(soilDat$location == "Buckthorn RG03", 4, #Invasive scrub
                                        ifelse(soilDat$location == "Rogers reforestation", 5,NA))))) #Reforestation field

#remove observations with missing air temp

soilMod <- soilDat %>%
  filter(is.na(aveT) == FALSE)
# create ID for time periods below freezing


# ID if swc is above or below field capacity
soilMod$swID <- ifelse(soilMod$SWC_12 > 0.33,2,1)
# create snow ID
soilMod$snowID <- ifelse(soilMod$SNWD > 10, 2,1)
 
# remove days with missing snow data
soilMod <- soilMod %>%
  filter(is.na(SNWD) == FALSE)



# create ID for forest type and freezing time period
soilMod$modforestID <- ifelse(soilMod$swID == 1, soilMod$locID,
                              soilMod$locID+5)

soilSnow <- soilMod %>%
  filter(snowID == 2)
soilNoSnow <- soilMod %>%
  filter(snowID == 1)  

mod_swloc <-lm(soilNoSnow$Tsoil_6~soilNoSnow$aveT*as.factor(soilNoSnow$modforestID))
summary(mod_swloc)

qqnorm(mod_swloc$residuals)
qqline(mod_swloc$residuals)

plot(soilNoSnow$aveT,mod_swloc$residuals)


smod_swloc <-lm(soilSnow$Tsoil_6~soilSnow$aveT*as.factor(soilSnow$modforestID))
summary(smod_swloc)

qqnorm(smod_swloc$residuals)
qqline(smod_swloc$residuals)

SoilLoc1 <- soilNoSnow %>% filter(locID == 1)
modTest <- lm(SoilLoc1$Tsoil_6 ~ SoilLoc1$aveT*as.factor(SoilLoc1$swID))
summary(modTest)
qqnorm(modTest$residuals)
qqline(modTest$residuals)

plot(soilSnow$aveT,smod_swloc$residuals)

ggplot(soilSnow, aes(aveT, Tsoil_6,color=as.factor(modforestID)))+
  geom_point()

ggplot(soilNoSnow %>% filter(locID ==1), aes(aveT, Tsoil_6,color=as.factor(modforestID)))+
  geom_point()

ggplot(soilMod %>% filter(modforestID == 2), aes(aveT, Tsoil_6,color=as.factor(snowWaterID)))+
  geom_point()


ggplot(soilMod %>% filter(modforestID == 2), aes(aveT, Tsoil_6,color=as.factor(snowID)))+
  geom_point()
ggplot(soilMod %>% filter(modforestID == 2), aes(aveT, Tsoil_6,color=as.factor(swID)))+
  geom_point()

ggplot(soilMod %>% filter(locID == 1 & snowID == 1), aes(aveT, Tsoil_6,color=as.factor(swID)))+
  geom_point()

ggplot(soilMod %>% filter(locID == 1 & snowID == 2), aes(aveT, Tsoil_6,color=as.factor(swID)))+
  geom_point()


ggplot(soilMod %>% filter(locID == 2 & snowID == 1), aes(aveT, Tsoil_6,color=as.factor(swID)))+
  geom_point()

ggplot(soilMod %>% filter(locID == 2 & snowID == 2), aes(aveT, Tsoil_6,color=as.factor(swID)))+
  geom_point()


ggplot(soilMod %>% filter(locID == 3 & snowID == 1), aes(aveT, Tsoil_6,color=as.factor(swID)))+
  geom_point()

ggplot(soilMod %>% filter(locID == 3 & snowID == 2), aes(aveT, Tsoil_6,color=as.factor(swID)))+
  geom_point()

ggplot(soilMod %>% filter(locID == 4 & snowID == 1), aes(aveT, Tsoil_6,color=as.factor(swID)))+
  geom_point()

ggplot(soilMod %>% filter(locID == 4 & snowID == 2), aes(aveT, Tsoil_6,color=as.factor(swID)))+
  geom_point()

ggplot(soilMod %>% filter(locID == 5 & snowID == 1), aes(aveT, Tsoil_6,color=as.factor(swID)))+
  geom_point()

ggplot(soilMod %>% filter(locID == 5 & snowID == 2), aes(aveT, Tsoil_6,color=as.factor(swID)))+
  geom_point()


ggplot(soilMod %>% filter(locID == 5 & snowID == 1), aes(aveT, Tsoil_6,color=SWC_12))+
  geom_point()

ggplot(soilMod %>% filter(locID == 5 & snowID == 2), aes(aveT, Tsoil_6,color=as.factor(swID)))+
  geom_point()


ggplot(soilMod %>% filter(modforestID == 2 & snowWaterID >= 3), aes(aveT, Tsoil_6,color=as.factor(snowWaterID)))+
  geom_point()

obsCount <- soilMod %>%
  group_by(modforestID, snowWaterID) %>%
  summarize(nons=n())

# create ID table
soilIDs <- soilMod %>%
  ungroup %>%
  select(location, locID, freezeModID, modforestID) %>%
  distinct()

soilCheck <- soilMod %>%
  filter(locID == 5)

ggplot(soilCheck, aes(aveT, SWC_12))+
  geom_point()

ggplot(soilCheck, aes(aveT, Tsoil_6))+
  geom_point()


ggplot(soilCheck %>% filter(modforestID==5), aes(aveT, Tsoil_6,color=as.factor(swID)))+
  geom_point()

check1 <- soilCheck %>% filter(modforestID==5)

check1 <- soilMod %>% filter(modforestID==2)


############### set up model ---------
# data
dataList <- list(Nobs= nrow(soilMod),
                 s_temp = soilMod$Tsoil_6,
                 modforestID = soilMod$modforestID,
                 forestID = soilMod$locID,
                 air_temp = soilMod$aveT,
                 swID = soilMod$swID,
                 Nmodforest = nrow(soilIDs),
                NSW=2,
                 Nforest=5,
                 plot_tempFreeze = seq(-20,0, length.out=41),
                 plot_tempWarm = seq(0, 30, length.out=61),
                plotLengthFreeze=41, plotLengthWarm=61)
                 

parms <- c("sig_temp"," beta_naught","beta","rep_temp",
           "mu_temp_freeze",
           "mu_temp_warm" )

inits <- list(list(sig_temp = rep(1,nrow(soilIDs)),
                   beta_naught= matrix(rep(0,10),ncol=2),
                   beta=matrix(c(
                          rep(0,10),
                          rep(0,10)), ncol=2, byrow=TRUE)),
              list(sig_temp = rep(5,nrow(soilIDs)),
                   beta_naught = matrix(rep(1,10),ncol=2),
                   beta=matrix(c(
                                 rep(-1,10),
                                 rep(-1,10)), ncol=2, byrow=TRUE)),
              list(sig_temp = rep(3,nrow(soilIDs)),
                   beta_naught=matrix(rep(2,10),ncol=2),
                     beta=matrix(c(
                                 rep(1,10),
                                 rep(1,10)), ncol=2, byrow=TRUE)))


temp_mod <- jags.model(file="/Users/hkropp/Documents/GitHub/forest_temp/soil_temp_model.r",
                                            data=dataList, inits=inits,
                                            n.adapt=10000,
                                           n.chains=3)
temp_sample <- coda.samples(temp_mod, variable.names=parms, n.iter=90000, thin=30)

MCMCtrace(temp_sample, params=c("sig_temp","beta_naught", "beta"),
                     pdf=TRUE, 
                    wd="/Users/hkropp/Library/CloudStorage/GoogleDrive-hkropp@hamilton.edu/My Drive/research/projects/forest_soil/model",
                     filename="temp_model.pdf")
beta_n_out <- MCMCsummary(temp_sample,params=c( "beta_naught"))
beta_out <- MCMCsummary(temp_sample,params=c( "beta"))
beta_out
beta_out$p_name <- row.names(beta_out)
beta_out$modforestID <- rep(seq(1,10), times=2)
beta_out$swID <- rep(seq(1,2), each=10)
sig_out <- MCMCsummary(temp_sample,params=c( "sig_temp"))
rep_temp <- MCMCsummary(temp_sample,params=c( "rep_temp"))


model_comp <- data.frame(actual_temp = soilMod$Tsoil_6,
                        predicted_temp = rep_temp$mean )

plot(model_comp$actual_temp, model_comp$predicted_temp)
mod_eval <- lm(model_comp$predicted_temp ~ model_comp$actual_temp)
summary(mod_eval)

write.csv(beta_n_out, 
            "/Users/hkropp/Library/CloudStorage/GoogleDrive-hkropp@hamilton.edu/My Drive/research/projects/forest_soil/model/beta_n_out.csv")
write.csv(beta_out, 
          "/Users/hkropp/Library/CloudStorage/GoogleDrive-hkropp@hamilton.edu/My Drive/research/projects/forest_soil/model/air_slope.csv")

write.csv(sig_out, 
          "/Users/hkropp/Library/CloudStorage/GoogleDrive-hkropp@hamilton.edu/My Drive/research/projects/forest_soil/model/sig_out.csv")
write.csv(rep_temp, 
          "/Users/hkropp/Library/CloudStorage/GoogleDrive-hkropp@hamilton.edu/My Drive/research/projects/forest_soil/model/rep_out.csv")


mu_temp_freeze <- MCMCsummary(temp_sample,params=c( "mu_temp_freeze"))

mu_temp_warm <- MCMCsummary(temp_sample,params=c( "mu_temp_warm"))


write.csv(mu_temp_freeze, 
          "/Users/hkropp/Library/CloudStorage/GoogleDrive-hkropp@hamilton.edu/My Drive/research/projects/forest_soil/model/mu_temp_freeze.csv")

write.csv(mu_temp_warm, 
          "/Users/hkropp/Library/CloudStorage/GoogleDrive-hkropp@hamilton.edu/My Drive/research/projects/forest_soil/model/mu_temp_warm.csv")
