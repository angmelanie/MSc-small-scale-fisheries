# specify path and directory
path <- "C:/Users/angmel/Documents/MSc-small-scale-fisheries/SAU_DBEM_RESULTS/USA/GFDL26/avg_new/"
filename <- dir(path, pattern = ".csv")
filepath <- paste("C:/Users/angmel/Documents/MSc-small-scale-fisheries/SAU_DBEM_RESULTS/USA/GFDL26/avg_new/", filename, sep = "")

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
us_26_lsf_avg <- dplyr::bind_rows(biolist) %>% 
  group_by(X, year) %>% 
  summarise(biodiv = sum(value)) %>% 
  spread(year, biodiv) %>% 
  mutate(percent_change = (((X2050 - X2000)/X2000)*100)) 
us_26_lsf_avg%>%
  write.csv("C:/Users/angmel/Documents/MSc-small-scale-fisheries/SAU_DBEM_RESULTS/Catch potential/us_26_lsf_avg.csv")

# select only for change - species richness plot
us_26_lsf_change <- us_26_lsf_avg %>% 
  dplyr::select(X, percent_change) 
us_26_lsf_change%>% 
  write.csv("C:/Users/angmel/Documents/MSc-small-scale-fisheries/SAU_DBEM_RESULTS/Catch potential/us_26_lsf_change.csv")


################# RCP8.5

path <- "C:/Users/angmel/Documents/MSc-small-scale-fisheries/SAU_DBEM_RESULTS/USA/GFDL85/avg_new/"
filename <- dir(path, pattern = ".csv")
filepath <- paste("C:/Users/angmel/Documents/MSc-small-scale-fisheries/SAU_DBEM_RESULTS/USA/GFDL85/avg_new/", filename, sep = "")

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
us_85_lsf_avg <- dplyr::bind_rows(biolist) %>% 
  group_by(X, year) %>% 
  summarise(biodiv = sum(value)) %>% 
  spread(year, biodiv) %>% 
  mutate(percent_change = (((X2050 - X2000)/X2000)*100)) 
us_85_lsf_avg%>%
  write.csv("C:/Users/angmel/Documents/MSc-small-scale-fisheries/SAU_DBEM_RESULTS/Catch potential/us_85_lsf_avg.csv")

# select only for change - species richness plot
us_85_lsf_change <- us_85_lsf_avg %>% 
  dplyr::select(X, percent_change) 
us_85_lsf_change%>% 
  write.csv("C:/Users/angmel/Documents/MSc-small-scale-fisheries/SAU_DBEM_RESULTS/Catch potential/us_85_lsf_change.csv")


#################SMALL SCALE FISHERIES#################
################# GFDL 26

# specify path and directory
path <- "C:/Users/angmel/Documents/MSc-small-scale-fisheries/SAU_DBEM_RESULTS/USA/GFDL26/SSF/avg_new/"
filename <- dir(path, pattern = ".csv")
filepath <- paste("C:/Users/angmel/Documents/MSc-small-scale-fisheries/SAU_DBEM_RESULTS/USA/GFDL26/SSF/avg_new/", filename, sep = "")

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
us_26_ssf_avg <- dplyr::bind_rows(biolist) %>% 
  group_by(X, year) %>% 
  summarise(biodiv = sum(value)) %>% 
  spread(year, biodiv) %>% 
  mutate(percent_change = (((X2050 - X2000)/X2000)*100)) 
us_26_ssf_avg%>%
  write.csv("C:/Users/angmel/Documents/MSc-small-scale-fisheries/SAU_DBEM_RESULTS/Catch potential/us_26_ssf_avg.csv")

# select only for change - species richness plot
us_26_ssf_change <- us_26_ssf_avg %>% 
  dplyr::select(X, percent_change) 
us_26_ssf_change%>% 
  write.csv("C:/Users/angmel/Documents/MSc-small-scale-fisheries/SAU_DBEM_RESULTS/Catch potential/us_26_ssf_change.csv")


################# RCP8.5

path <- "C:/Users/angmel/Documents/MSc-small-scale-fisheries/SAU_DBEM_RESULTS/USA/GFDL85/SSF/avg_new/"
filename <- dir(path, pattern = ".csv")
filepath <- paste("C:/Users/angmel/Documents/MSc-small-scale-fisheries/SAU_DBEM_RESULTS/USA/GFDL85/SSF/avg_new/", filename, sep = "")

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
us_85_ssf_avg <- dplyr::bind_rows(biolist) %>% 
  group_by(X, year) %>% 
  summarise(biodiv = sum(value)) %>% 
  spread(year, biodiv) %>% 
  mutate(percent_change = (((X2050 - X2000)/X2000)*100)) 
us_85_ssf_avg%>%
  write.csv("C:/Users/angmel/Documents/MSc-small-scale-fisheries/SAU_DBEM_RESULTS/Catch potential/us_85_ssf_avg.csv")

# select only for change - species richness plot
us_85_ssf_change <- us_85_ssf_avg %>% 
  dplyr::select(X, percent_change) 
us_85_ssf_change%>% 
  write.csv("C:/Users/angmel/Documents/MSc-small-scale-fisheries/SAU_DBEM_RESULTS/Catch potential/us_85_ssf_change.csv")
