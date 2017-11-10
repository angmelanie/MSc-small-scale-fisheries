# DBEM data prep for integration with SAU

# import taxa for my modelled species
exploited_list <- read.csv(file = "C:/Users/angmel/Documents/firstchapter/METADATA_ACCESS/species_list_model_MA.csv")
dbem_imp_all(exploited_list, 1990, 2060, 8.5, "Catch")

# import taxa from DROBO
exploited_list_DROBO <- read.csv(file = "C:/Users/angmel/Documents/firstchapter/SAU x DBEM/exploited_species_list_DROBO.csv")
dbem_imp_all(exploited_list_DROBO, 1990, 2060, 8.5, "Catch")


dbem_Import(800043, 2000, 2060, 8.5, "Catch")

head(final_ssf)
exploited_list_2$taxonkey <- final_ssf$suggested_taxaID
exploited_list <- exploited_list[-1,]
exploited_list <- exploited_list[-2,]

library(raster)
x <- raster("C:/Temp/gridfiles/600155.asc")
plot(x)
lat_lon <- read.csv(file = "C:/Users/angmel/Documents/firstchapter/Reference tables/Lat_Lon.csv", header = FALSE)
head(lat_lon) 
?merge
library(data.table)
colnames(lat_lon) <- c("INDEX", "Long", "Lat")

?rasterFromXYZ

`A_pectoralis2` <- select(`A_pectoralis2`, -1, -2)
`A_pectoralis2`<- merge(`Albatrossia pectoralis`, lat_lon, by = "INDEX")
`A_pectoralis2` <- subset(`A_pectoralis2`, select = c(2,3,1))

library(rworldmap)
quartz()
windows()
newmap <- getMap(resolution = "low")
map <- rasterFromXYZ(`A_pectoralis2`, crs = "+init=epsg:28992")

plot(newmap, xlim = c(-180, -90), ylim = c(15,55))
plot(map, add = T, legend = T, alpha = 0.4)
