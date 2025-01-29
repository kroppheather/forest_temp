############## libraries ----
library(dplyr)
library(ggplot2)
############## read in data ----

canopyLAI <- read.csv("K:/Environmental_Studies/hkropp/Private/canopy/canopy_lai.csv")
SpeciesInfo <- read.csv("K:/Environmental_Studies/hkropp/Private/canopy/speciesID.csv")
forestInventory <- read.csv("K:/Environmental_Studies/hkropp/Private/canopy/HCEF forest inventory data.csv")
############ organize data -----

# average LAI by plot
canopyPlot <- canopyLAI %>%
  na.omit() %>%
  group_by(site_id) %>%
  summarize(LAI = mean(PAR_LAI),
            sd_LAI = sd(PAR_LAI),
            n_canopy = n())

# caclulate total tree area
forestInventory$tree_area.cm2 <- (((forestInventory$DBH.cm / 2)^2) * pi) 

# add up tree area by species
FI <- forestInventory %>%
  filter(Dead == "N", DBH.cm >3 ) %>%
  group_by(Plot, Species) %>%
  summarise(totArea = sum(tree_area.cm2,na.rm=TRUE),
            ncount = n(),
            aveDBH = mean(DBH.cm,na.rm=TRUE))
# add up tree area by plot
FITot <-  forestInventory %>%
  filter(Dead == "N", DBH.cm >3 ) %>%
  group_by(Plot) %>%
  summarise(totArea = sum(tree_area.cm2,na.rm=TRUE),
            ncount = n(),
            aveDBH = mean(DBH.cm,na.rm=TRUE))
# calculate percent basal area by plot
FIjoin <- left_join(FI,FITot, by="Plot")
FIjoin$PercBA <- (FIjoin$totArea.x/FIjoin$totArea.y)*100 


## get a summary of major dominant species at a plot ##

# filter to look at only more dominant species with more than 20% of basal area
FItop <- FIjoin %>%
  filter(PercBA > 20)
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

nameDF <- data.frame(Plot = plotsI, Names = namecomp, namePerc=namePerc)

# combine LAI with dominate species

LAIinv <- inner_join(nameDF, canopyLAI, by=c("Plot" = "site_id"))

############ make plots -----

ggplot(LAIinv, aes(namePerc, PAR_LAI))+
  geom_boxplot()


ggplot(LAIinv, aes(x=Plot, y=PAR_LAI, fill=namePerc))+
  geom_boxplot()+
  xlab("Forest inventory plot")+
  ylab(expression(paste("Leaf area index (m"^2,""[leaf], " m"^-2,""[ground],")")))
