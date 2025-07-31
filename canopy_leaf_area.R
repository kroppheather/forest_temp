############## libraries ----
library(dplyr)
library(ggplot2)
library(lubridate)
############## read in data ----
c_dir <- "/Users/hkropp/Library/CloudStorage/GoogleDrive-hkropp@hamilton.edu/My Drive/research/projects/forest_soil/canopy"
canopyLAI <- read.csv(paste0(c_dir,"/canopy_lai_all.csv"))
SpeciesInfo <- read.csv(paste0(c_dir,"/speciesID.csv"))
forestInventory <- read.csv(paste0(c_dir,"/HCEF forest inventory data 7_25.csv"))
plot_type <- read.csv(paste0(c_dir,"/FI_history_type_2025.csv"))
############ organize leaf area data -----
canopyLAI$dateF <- mdy(canopyLAI$date_recorded)
canopyLAI$year <- year(canopyLAI$dateF)
canopyLAI$month <- month(canopyLAI$dateF)
# average LAI by plot
canopyPlot <- canopyLAI %>%
  filter(is.na(PAR_LAI)!=TRUE)%>%
  group_by(site_id, dateF) %>%
  summarize(LAI = mean(PAR_LAI, na.rm=TRUE),
            sd_LAI = sd(PAR_LAI, na.rm=TRUE),
            n_canopy = n())
canopyPlot$year <- year(canopyPlot$dateF)
canopyPlot$se <- canopyPlot$sd_LAI / sqrt(canopyPlot$n_canopy)
# soil plots
canopySoil <- canopyLAI %>%
  filter(site_id == "RG03" | site_id == "RG25" | site_id == "RG01" | site_id == "RG09")

canopySoil24 <- canopySoil %>%
  filter(year == 2024)
canopySoil_24 <- left_join(canopySoil24, plot_type[,1:2], by=c("site_id"="Plots") )

ggplot(canopySoil_24, aes(x=as.factor(dateF), PAR_LAI, fill=Current_type))+
  geom_boxplot()

maxOut <- canopyLAI %>%
  filter( month<= 8 )
ggplot(maxOut, aes(as.factor(site_id),PAR_LAI,fill=as.factor(year)))+
  geom_boxplot()

############ forest inventory -----
# caclulate total tree area
forestInventory$tree_area.cm2 <- (((forestInventory$DBH.cm / 2)^2) * pi) 

# add up tree area by species
FI <- forestInventory %>%
  filter(Dead_1 == "N" ) %>%
  group_by(Plot, Species) %>%
  summarise(Area_Spec = sum(tree_area.cm2,na.rm=TRUE),
            ncount_Spec = n(),
            aveDBH_Spec = mean(DBH.cm,na.rm=TRUE))
# add up tree area by plot
FITot <-  forestInventory %>%
  filter(Dead_1 == "N") %>%
  group_by(Plot) %>%
  summarise(totArea = sum(tree_area.cm2,na.rm=TRUE),
            ncount = n(),
            aveDBH = mean(DBH.cm,na.rm=TRUE))
# calculate percent basal area by plot
FIjoin <- left_join(FI,FITot, by="Plot")
FIjoin$PercBA <- (FIjoin$Area_Spec/FIjoin$totArea)*100 


## get a summary of major dominant species at a plot ##

# filter to look at only more dominant species with more than 20% of basal area
FItop <- FIjoin %>%
  filter(PercBA > 15)
# subset to only focus on relevant columns
PlotSpec <- FItop %>%
  select(Species,Plot, PercBA)

# get list of all plots
plotsI <- unique(PlotSpec$Plot)

# add species info into plot data
PlotComp <- left_join(PlotSpec, SpeciesInfo, by=c("Species"="Code"))

# pull out species names for proper formatting
NameSub <- character()
for(i in 1:nrow(PlotComp)){
  NameSub[i] <- paste0(substr(strsplit(PlotComp$Species.y[i], " ")[[1]][1],1,1), ". ",
                       strsplit(PlotComp$Species.y[i], " ")[[1]][2])
}

PlotComp$NameSub <- NameSub
PlotComp$Name <- ifelse(PlotComp$Genus =="Malus", "Malus sp.", PlotComp$NameSub ) 

PlotComp <- PlotComp %>%
  arrange(desc(PercBA))
PlotComp[PlotComp$Plot == "RG03",]
# combine names together for plots with multiple species
pasteSub <- character()
namecomp <- character()
namePerc <- character()

for(i in 1:length(plotsI)){
  
  pasteSub <- PlotComp$Name[PlotComp$Plot == plotsI[i]]
  percSub <-  paste0(pasteSub, "(",round(PlotComp$PercBA[PlotComp$Plot == plotsI[i]],0),")")
  namecomp[i] <- paste(pasteSub,  collapse = ", ")
  namePerc[i] <- paste(percSub,  collapse = ", ")
}
namecomp
namePerc


pasteSubC <- character()
namecompC <- character()
namePercC <- character()

for(i in 1:length(plotsI)){
  
  pasteSubC <- PlotComp$Common.Name[PlotComp$Plot == plotsI[i]]
  percSubC <-  paste0(pasteSubC, "(",round(PlotComp$PercBA[PlotComp$Plot == plotsI[i]],0),")")
  namecompC[i] <- paste(pasteSubC,  collapse = ", ")
  namePercC[i] <- paste(percSubC,  collapse = ", ")
}
namecompC
namePercC

nameDF <- data.frame(Plot = plotsI, Names = namecomp, namePerc=namePerc, cname=namecompC, cnamePerc=namePercC)

#write.csv(nameDF, "/Users/hkropp/Library/CloudStorage/GoogleDrive-hkropp@hamilton.edu/My Drive/research/projects/forest_soil/canopy/inventory_summary_7_25.csv" )

# combine LAI with dominate species

LAIinv <- inner_join(nameDF, canopyLAI, by=c("Plot" = "site_id"))

############ make plots -----



ggplot(LAIinv, aes(x=Plot, y=PAR_LAI, fill=cname))+
  geom_boxplot()+
  xlab("Forest inventory plot")+
  ylab(expression(paste("Leaf area index (m"^2,""[leaf], " m"^-2,""[ground],")")))
