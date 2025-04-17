library(dplyr)

sites <- read.table("/Users/hkropp/Library/CloudStorage/GoogleDrive-hkropp@hamilton.edu/My Drive/research/projects/forest_soil/ghcnd-inventory.txt")

siteV <- unique(sites$V4)
soilMax <- siteV[grep("SX", unique(sites$V4))]
soilMin <- siteV[grep("SN", unique(sites$V4))][3:30]



sitesS <- sites %>%
  filter(V4 %in% soilMax)
sitesMax <- sitesS %>%
  filter(V2 >= 43 & V2 <= 45 & V3 >=-76 & V3 <= -70 )
soilC <- read.csv("/Users/hkropp/Library/CloudStorage/GoogleDrive-hkropp@hamilton.edu/My Drive/research/projects/forest_soil/long_term/USW00014743.csv")
length(soilC$SN02[is.na(soilC$SN02)==FALSE])
soilMaxC <- soilC %>%
  filter(is.na(SX02) ==FALSE)

# camden with soil only available 1980s to 2004
siteCheck <- sites %>%
  filter(V1 == "72519664775")
dirGHC <- "/Users/hkropp/Library/CloudStorage/GoogleDrive-hkropp@hamilton.edu/My Drive/research/projects/forest_soil/climate"
gc1 <- read.csv(paste0(dirGHC, "/3926976.csv"))
gc2 <- read.csv(paste0(dirGHC, "/3926980.csv"))
gc3 <- read.csv(paste0(dirGHC, "/3926988.csv"))
gc4 <- read.csv(paste0(dirGHC, "/3926990.csv"))
gc5 <- read.csv(paste0(dirGHC, "/3926997.csv"))
gc6 <- read.csv(paste0(dirGHC, "/3927000.csv"))
gc1[1,1]
gc2[1,1]
gc3[1,1]
gc4[1,1]
gc5[1,1]
gc6[1,1]