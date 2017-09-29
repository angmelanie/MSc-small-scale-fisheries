# Error log analysis
library(tidyverse)

setwd("~/firstchapter/R codes/MSc-small-scale-fisheries/DBEM x SAU/errorlog")
error_report <- read.csv("error_report.csv")

# Cases of error:
# 1. Inverts - have not modelled yet
# 2. My potential error in spliting G species
# 3. Check inputs! Either D or B species, or G but with comm importance and in range

# LEVEL 1 SPECIES: exclude these from the list (for now)
inverts <- c("690088", "690206", "690053", "690088", "690440", "690009", "690053", "690440")

# LEVEL 3 species, lets plot DBEM data to see where species is located

# 600751, great white shark
# run dbem_import function

# upload csv file
sppdist <- dbem_Import(600751, 1991,2060,2.6,"Catch")

lat_lon <- read.csv("C:/Users/angmel/Documents/firstchapter/Reference tables/Lat_Lon.csv", header = FALSE) %>% 
  rename("INDEX" = V1, "lon" = V2, "lat" = V3)

mapthis <- dplyr::select(sppdist, INDEX, X1991)
mapthis2 <- inner_join(mapthis, lat_lon, by = "INDEX")
mapthis2$INDEX <- NULL
mapthis2 <- mapthis2[,c(2,3,1)]

xyz <- rasterFromXYZ(mapthis2)
plot(xyz)
?rasterFromXYZ
