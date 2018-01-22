# Catch potential - Canada

library(tidyverse)

#################LARGE SCALE FISHERIES#################
################# GFDL 26

# specify path and directory
path <- "C:/Users/angmel/Documents/MSc-small-scale-fisheries/SAU_DBEM_RESULTS/Canada/GFDL26/avg_new/"
filename <- dir(path, pattern = ".csv")
filepath <- paste("C:/Users/angmel/Documents/MSc-small-scale-fisheries/SAU_DBEM_RESULTS/Canada/GFDL26/avg_new/", filename, sep = "")

# create new list for input
biolist <- list()

# for loop through files in directory
for (i in 1:length(filepath)){
  
  catch <- read.csv(filepath[i], header = TRUE)
  
  catch <- catch[!duplicated(catch$X),]
  
  df <- catch %>% 
    gather(year, value, -c(X))

  biolist[[i]] <- df
}

# save entire data frame - all years
ca_26_lsf_avg <- dplyr::bind_rows(biolist) %>% 
  group_by(X, year) %>% 
  summarise(biodiv = sum(value)) %>% 
  spread(year, biodiv) %>% 
  mutate(percent_change = (((X2050 - X2000)/X2000)*100)) 
ca_26_lsf_avg%>%
  write.csv("C:/Users/angmel/Documents/MSc-small-scale-fisheries/SAU_DBEM_RESULTS/Catch potential/ca_26_lsf_avg.csv")

# select only for change - species richness plot
ca_26_lsf_change <- ca_26_lsf_avg %>% 
  dplyr::select(X, percent_change) 
ca_26_lsf_change%>% 
  write.csv("C:/Users/angmel/Documents/MSc-small-scale-fisheries/SAU_DBEM_RESULTS/Catch potential/ca_26_lsf_change.csv")


################# RCP8.5

path <- "C:/Users/angmel/Documents/MSc-small-scale-fisheries/SAU_DBEM_RESULTS/Canada/GFDL85/avg_new/"
filename <- dir(path, pattern = ".csv")
filepath <- paste("C:/Users/angmel/Documents/MSc-small-scale-fisheries/SAU_DBEM_RESULTS/Canada/GFDL85/avg_new/", filename, sep = "")

# create new list for input
biolist <- list()

# for loop through files in directory
for (i in 1:length(filepath)){
  
  catch <- read.csv(filepath[i], header = TRUE)
  
  catch <- catch[!duplicated(catch$X),]

  df <- catch %>% 
    gather(year, value, -c(X))
  
  biolist[[i]] <- df
}

# save entire data frame - all years
ca_85_lsf_avg <- dplyr::bind_rows(biolist) %>% 
  group_by(X, year) %>% 
  summarise(biodiv = sum(value)) %>% 
  spread(year, biodiv) %>% 
  mutate(percent_change = (((X2050 - X2000)/X2000)*100)) 
ca_85_lsf_avg%>%
  write.csv("C:/Users/angmel/Documents/MSc-small-scale-fisheries/SAU_DBEM_RESULTS/Catch potential/ca_85_lsf_avg.csv")

# select only for change - species richness plot
ca_85_lsf_change <- ca_85_lsf_avg %>% 
  dplyr::select(X, percent_change) 
ca_85_lsf_change%>% 
  write.csv("C:/Users/angmel/Documents/MSc-small-scale-fisheries/SAU_DBEM_RESULTS/Catch potential/ca_85_lsf_change.csv")


#################SMALL SCALE FISHERIES#################
################# GFDL 26

# specify path and directory
path <- "C:/Users/angmel/Documents/MSc-small-scale-fisheries/SAU_DBEM_RESULTS/Canada/GFDL26/SSF/avg_new/"
filename <- dir(path, pattern = ".csv")
filepath <- paste("C:/Users/angmel/Documents/MSc-small-scale-fisheries/SAU_DBEM_RESULTS/Canada/GFDL26/SSF/avg_new/", filename, sep = "")

# create new list for input
biolist <- list()

# for loop through files in directory
for (i in 1:length(filepath)){
  
  catch <- read.csv(filepath[i], header = TRUE)
  
  catch <- catch[!duplicated(catch$X),]
  
  df <- catch %>% 
    gather(year, value, -c(X))
  
  biolist[[i]] <- df
}

# save entire data frame - all years
ca_26_ssf_avg <- dplyr::bind_rows(biolist) %>% 
  group_by(X, year) %>% 
  summarise(biodiv = sum(value)) %>% 
  spread(year, biodiv) %>% 
  mutate(percent_change = (((X2050 - X2000)/X2000)*100)) 
ca_26_ssf_avg%>%
  write.csv("C:/Users/angmel/Documents/MSc-small-scale-fisheries/SAU_DBEM_RESULTS/Catch potential/ca_26_ssf_avg.csv")

# select only for change - species richness plot
ca_26_ssf_change <- ca_26_ssf_avg %>% 
  dplyr::select(X, percent_change) 
ca_26_ssf_change%>% 
  write.csv("C:/Users/angmel/Documents/MSc-small-scale-fisheries/SAU_DBEM_RESULTS/Catch potential/ca_26_ssf_change.csv")


################# RCP8.5

path <- "C:/Users/angmel/Documents/MSc-small-scale-fisheries/SAU_DBEM_RESULTS/Canada/GFDL85/SSF/avg_new/"
filename <- dir(path, pattern = ".csv")
filepath <- paste("C:/Users/angmel/Documents/MSc-small-scale-fisheries/SAU_DBEM_RESULTS/Canada/GFDL85/SSF/avg_new/", filename, sep = "")

# create new list for input
biolist <- list()

# for loop through files in directory
for (i in 1:length(filepath)){
  
  catch <- read.csv(filepath[i], header = TRUE)
  
  catch <- catch[!duplicated(catch$X),]
  
  df <- catch %>% 
    gather(year, value, -c(X))
  
  biolist[[i]] <- df
}

# save entire data frame - all years
ca_85_ssf_avg <- dplyr::bind_rows(biolist) %>% 
  group_by(X, year) %>% 
  summarise(biodiv = sum(value)) %>% 
  spread(year, biodiv) %>% 
  mutate(percent_change = (((X2050 - X2000)/X2000)*100)) 
ca_85_ssf_avg%>%
  write.csv("C:/Users/angmel/Documents/MSc-small-scale-fisheries/SAU_DBEM_RESULTS/Catch potential/ca_85_ssf_avg.csv")

# select only for change - species richness plot
ca_85_ssf_change <- ca_85_ssf_avg %>% 
  dplyr::select(X, percent_change) 
ca_85_ssf_change%>% 
  write.csv("C:/Users/angmel/Documents/MSc-small-scale-fisheries/SAU_DBEM_RESULTS/Catch potential/ca_85_ssf_change.csv")
