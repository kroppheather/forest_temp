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
                     depth=rep(c(2,4,5), each=nrow(gen_all)))

geneva$dateF <- ymd(geneva$date)

ggplot(geneva, aes(dateF, soilT, color=as.factor(depth)))+
  geom_line()
# geneva met
gmet1 <- read.csv()
