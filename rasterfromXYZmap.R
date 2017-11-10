??rasterfromXYZ
library(raster)

data <- dbem_Import(600247, 2010,2010, 8.5, "Catch")
?read_csv
cellID <- read_csv("C:/Users/angmel/Documents/firstchapter/Reference tables/Lat_Lon.csv", col_names = FALSE)
head(cellID)
cellID <- rename(cellID, INDEX = X1, Lon = X2, Lat = X3)

data_to_model <- left_join(data, cellID, by = "INDEX") %>% 
  dplyr::select(Lon, Lat, `2010`) 

 
plotme <- rasterFromXYZ(data_to_model)
plot(plotme)
?raster

library(rasterVis)
library(sp)
?getData
out <- getData('GADM', country='United States', level=1)
Alaska <- out[out$NAME_1 %in% 'Alaska',]

print(plotme) +
  layer(sp.polygons(Alaska))
