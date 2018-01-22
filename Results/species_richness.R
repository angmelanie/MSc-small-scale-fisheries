# Species Richness - Alaska 

library(tidyverse)

# GFDL 26

# specify path and directory
path <- "C:/Users/angmel/Documents/MSc-small-scale-fisheries/SAU_DBEM_RESULTS/Alaska/GFDL26/avg_new/"
filename <- dir(path, pattern = ".csv")
filepath <- paste("C:/Users/angmel/Documents/MSc-small-scale-fisheries/SAU_DBEM_RESULTS/Alaska/GFDL26/avg_new/", filename, sep = "")

# create new list for input
biolist <- list()

# for loop through files in directory
for (i in 1:length(filepath)){
  
  catch <- read.csv(filepath[i], header = TRUE)
  
  catch <- catch[!duplicated(catch$X),]

  df <- catch %>% 
    gather(year, value, -c(X))
  
  df$value[df$value > 0] <- 1
  df$value[is.na(df$value)] <- 0
  
  biolist[[i]] <- df
}

# save entire data frame - all years
al_26_lsf_avg <- dplyr::bind_rows(biolist) %>% 
  group_by(X, year) %>% 
  summarise(biodiv = sum(value)) %>% 
  spread(year, biodiv) %>% 
  mutate(change = X2050 - X2000) %>%
  write.csv("C:/Users/angmel/Documents/MSc-small-scale-fisheries/SAU_DBEM_RESULTS/Species richness_2/al_26_lsf_avg.csv")

# select only for change - species richness plot
al_26_lsf_change <- dplyr::bind_rows(biolist) %>% 
  group_by(X, year) %>% 
  summarise(biodiv = sum(value)) %>% 
  spread(year, biodiv) %>% 
  mutate(change = X2050 - X2000) %>% 
  dplyr::select(X, change) %>% 
  write.csv("C:/Users/angmel/Documents/MSc-small-scale-fisheries/SAU_DBEM_RESULTS/Species richness_2/al_26_lsf_change.csv")


###### RCP8.5 Alaska

path <- "C:/Users/angmel/Documents/MSc-small-scale-fisheries/SAU_DBEM_RESULTS/Alaska/GFDL85/avg_new/"
filename <- dir(path, pattern = ".csv")
filepath <- paste("C:/Users/angmel/Documents/MSc-small-scale-fisheries/SAU_DBEM_RESULTS/Alaska/GFDL85/avg_new/", filename, sep = "")

# create new list for input
biolist <- list()

# for loop through files in directory
for (i in 1:length(filepath)){
  
  catch <- read.csv(filepath[i], header = TRUE)
  
  catch <- catch[!duplicated(catch$X),]
  
  df <- catch %>% 
    gather(year, value, -c(X))
  
  df$value[df$value > 0] <- 1
  df$value[is.na(df$value)] <- 0
  
  biolist[[i]] <- df
}

# save entire data frame - all years
al_85_lsf_avg <- dplyr::bind_rows(biolist) %>% 
  group_by(X, year) %>% 
  summarise(biodiv = sum(value)) %>% 
  spread(year, biodiv) %>% 
  mutate(change = X2050 - X2000) %>%
  write.csv("C:/Users/angmel/Documents/MSc-small-scale-fisheries/SAU_DBEM_RESULTS/Species richness_2/al_85_lsf_avg.csv")

# select only for change - species richness plot
al_85_lsf_change <- dplyr::bind_rows(biolist) %>% 
  group_by(X, year) %>% 
  summarise(biodiv = sum(value)) %>% 
  spread(year, biodiv) %>% 
  mutate(change = X2050 - X2000) %>% 
  dplyr::select(X, change) %>% 
  write.csv("C:/Users/angmel/Documents/MSc-small-scale-fisheries/SAU_DBEM_RESULTS/Species richness_2/al_85_lsf_change.csv")




###############################################
# Plots were done in mac instead, because geom_catogram function doesn't work on PC but on mac
# Plot species richness via William's code

# # sppdist_list <- read.csv(file = "C:/Temp/gridfiles/S604874.csv")
# sppdist <- biodiversity_al85ssf
# 
# 
# # sppdist <- avg
# sppdist[,1][sppdist[,1]==0]<-NA
# unique(sppdist_list)
# sppdist
# 
# # each row is cellID
# sppdist_list<-array(NA,259200)
# 
# # set sppdist_list's rowID = sppdist$INDEX and input column 2 where it matches
# sppdist_list[sppdist$X]<-sppdist[,2]
# sppdist_list[sppdist$X]<-sppdist$X2000
# world <- map_data("world")
# 
# xllcorner=-180
# yllcorner=-90
# cellsize=0.5
# 
# 
# #Fishing impact
# #spi<-sppdist[259200:1]
# spi<-sppdist_list[259200:1]
# spiasc<-matrix(spi,ncol=720,nrow=360,byrow=T)
# spiasc<-apply(t(spiasc),2,rev) 
# 
# outputpath<-("S_test.asc")
# epi.asc(spiasc, outputpath, xllcorner, yllcorner, cellsize, na = -9999)
# x<-raster(outputpath)
# 
# rasterFromXYZ(x)
# 
# x_spdf<-as(x,"SpatialPixelsDataFrame")
# x_df<-as.data.frame(x_spdf)
# colnames(x_df) <- c("value", "x", "y")
# 
# p=ggplot()  + 
#   geom_tile(data=x_df, aes(x=x, y=y, fill=value), alpha=0.8) +
#   scale_fill_gradient2(high=rgb(69,117,180,maxColorValue = 255),mid=rgb(255,255,191,maxColorValue = 255), low=rgb(215,48,39,maxColorValue = 255),
#                        guide="colorbar",midpoint = .99,name = "Index")+
#   geom_cartogram(data=world, map=world,
#                  aes(x=long, y=lat, map_id=region),fill="white",color="black")+
#   coord_quickmap(xlim=c(-180,-100), ylim=c(0, 90))+  #for zooming
#   ylab("")+
#   xlab("")+
#   scale_y_continuous(expand = c(0, -5))+
#   scale_x_continuous(expand = c(0, 0))+
#   labs(title="",size=16)+
#   theme(axis.line = element_blank(),
#         panel.grid.major = element_blank(),
#         panel.grid.minor = element_blank(),
#         #panel.border = element_blank(),
#         panel.border = element_rect(colour = "black", fill=NA, size=1),
#         panel.background = element_blank(),
#         #axis.text.y=element_blank(),
#         axis.text=element_text(size=9,colour="black"),
#         plot.title=element_text(hjust=0,vjust=-0.4,face="bold")
#   )
# 
# p
# 
# #  median(avg$change, na.rm = TRUE)
# #  use this to change the midpoint value on plot
# 
# ggsave(p,file="test.jpg",width=8,height=7)
