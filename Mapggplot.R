rm(list=ls())
setwd("/Users/william/Google Drive/Fuzzy vulnerability")
sppdist<-read.csv(paste("Distributions/S600240.csv",sep=""),header=F)
sppdist[,1][sppdist[,1]==0]<-NA
#sppdist<-array(NA,259200)

#sppdist[taxonfile$seq]<-taxonfile[,2]

library(rgdal)
library(maptools)
library(rgeos)
library(rasterVis)
library(RColorBrewer)
library(maps)
library(dplyr)
library(ggalt) 
library(ggthemes)
library(epiR)
library(raster)
library(ggmap)
library(gridExtra)
library(ggalt)

world <- map_data("world")

xllcorner=-180
yllcorner=-90
cellsize=0.5

#Fishing impact
#spi<-sppdist[259200:1]
spi<-sppdist[259200:1,1]
spiasc<-matrix(spi,ncol=720,nrow=360,byrow=T)
spiasc<-apply(t(spiasc),2,rev) 

outputpath<-("S600240.asc")
epi.asc(spiasc, outputpath, xllcorner, yllcorner, cellsize, na = -9999)
x<-raster(outputpath)

x_spdf<-as(x,"SpatialPixelsDataFrame")
x_df<-as.data.frame(x_spdf)
colnames(x_df) <- c("value", "x", "y")

p=ggplot()  + 
  geom_tile(data=x_df, aes(x=x, y=y, fill=value), alpha=0.8) +
  scale_fill_gradient2(low=rgb(69,117,180,maxColorValue = 255),mid=rgb(255,255,191,maxColorValue = 255), high=rgb(215,48,39,maxColorValue = 255),
                       guide="colorbar",midpoint = 50,name = "Index")+
  geom_cartogram(data=world, map=world,
                 aes(x=long, y=lat, map_id=region),fill="white",color="black",)+
  coord_quickmap(xlim=c(-180,-100), ylim=c(20, 90))+  #for zooming
  ylab("")+
  xlab("")+
  scale_y_continuous(expand = c(0, -5))+
  scale_x_continuous(expand = c(0, 0))+
  labs(title="",size=16)+
  theme(axis.line = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        #panel.border = element_blank(),
        panel.border = element_rect(colour = "black", fill=NA, size=1),
        panel.background = element_blank(),
        #axis.text.y=element_blank(),
        axis.text=element_text(size=9,colour="black"),
        plot.title=element_text(hjust=0,vjust=-0.4,face="bold")
  )

ggsave(p,file="Climate_Fish vul//Figure3ADraft.jpg",width=8,height=7)
