# AVERAGING CATCH ACROSS TIME PERIOD VERSION 2
# For LSF and SSF, 75% catch

# I take a 20 year average per species, per cell 
# and call that the average catch in year n.

# Here I create a for loop that goes through each folder
# You need to change: input path, input filepath and output path
# I've also removed all the error species from the folder
# You need to do this for each RCP scenario and for each EEZ

# Load package
library(zoo)

# ALASKA - 2.6 and 8.5

# LSf - 2.6 
path <- "C:/Users/angmel/Documents/MSc-small-scale-fisheries/SAU_DBEM_RESULTS/Alaska/GFDL26/"

filename <- dir(path, pattern = ".csv")
filepath <- paste("C:/Users/angmel/Documents/MSc-small-scale-fisheries/SAU_DBEM_RESULTS/Alaska/GFDL26/", filename, sep = "")

for (i in 1:length(filepath)){
  sau_dbem <- read.csv(filepath[i], header = TRUE)
  # sau_dbem <- read.csv("C:/Users/angmel/Documents/MSc-small-scale-fisheries/SAU_DBEM_RESULTS/Alaska/GFDL85/604122.csv")
  
  # remove rows with duplicated cell IDs -  error from 7final.R
  sau_dbem <- sau_dbem[!duplicated(sau_dbem$INDEX),]
  
  # replace NAs with 0
  sau_dbem[is.na(sau_dbem)] <- 0
  
  # prepare data frame for analysis
  flip_sau_dbem <- data.frame(t(sau_dbem))
  
  # average every 20 years
  smoothMean <- rollmean(x = flip_sau_dbem, # original series 
                         k = 20) # width of the rolling window 
  # tidying data
  colnames(smoothMean) <- flip_sau_dbem[1,]
  smoothMean <- smoothMean[-1,]
  
  if (sum(smoothMean, na.rm = TRUE) > 0) {
    smoothMean2 <- smoothMean[1:51,]
    rownames(smoothMean2) <- c(2000:2050)
    avg_sau_dbem <- data.frame(t(smoothMean2))
    
    # final result
    newfilepath <- paste("C:/Users/angmel/Documents/MSc-small-scale-fisheries/SAU_DBEM_RESULTS/Alaska/GFDL26/", filename[i], "_avg.csv", sep = "")
    
    write.csv(avg_sau_dbem, newfilepath)
    }

  else {
    errspp[i] <-  filepath[i]
  }
}

errspp
lsf_alaska <- data.frame(errspp) # CHANGE THIS FOR DROBO/CYGWIN
write_csv(lsf_alaska, "C:/Users/angmel/Documents/MSc-small-scale-fisheries/SAU_DBEM_RESULTS/Alaska/GFDL26/errspp_avg")

# LSF 85

path <- "C:/Users/angmel/Documents/MSc-small-scale-fisheries/SAU_DBEM_RESULTS/Alaska/GFDL85/"

filename <- dir(path, pattern = ".csv")
filepath <- paste("C:/Users/angmel/Documents/MSc-small-scale-fisheries/SAU_DBEM_RESULTS/Alaska/GFDL85/", filename, sep = "")

for (i in 1:length(filepath)){
  sau_dbem <- read.csv(filepath[i], header = TRUE)
  
  # remove rows with duplicated cell IDs -  error from 7final.R
  sau_dbem <- sau_dbem[!duplicated(sau_dbem$INDEX),]
  
  # replace NAs with 0
  sau_dbem[is.na(sau_dbem)] <- 0
  
  # prepare data frame for analysis
  flip_sau_dbem <- data.frame(t(sau_dbem))
  
  # average every 20 years
  smoothMean <- rollmean(x = flip_sau_dbem, # original series 
                         k = 20) # width of the rolling window 
  # tidying data
  colnames(smoothMean) <- flip_sau_dbem[1,]
  smoothMean <- smoothMean[-1,]
  
  if (sum(smoothMean, na.rm = TRUE) > 0) {
    smoothMean2 <- smoothMean[1:51,]
    rownames(smoothMean2) <- c(2000:2050)
    avg_sau_dbem <- data.frame(t(smoothMean2))
    
    # final result
    newfilepath <- paste("C:/Users/angmel/Documents/MSc-small-scale-fisheries/SAU_DBEM_RESULTS/Alaska/GFDL85/avg_new/", filename[i], "_avg.csv", sep = "")
    
    write.csv(avg_sau_dbem, newfilepath)
  }
  
  else {
    errspp[i] <-  filepath[i]
  }
}

errspp
lsf_alaska_85 <- data.frame(errspp) # CHANGE THIS FOR DROBO/CYGWIN
write_csv(lsf_alaska_85, "C:/Users/angmel/Documents/MSc-small-scale-fisheries/SAU_DBEM_RESULTS/Alaska/GFDL85/avg_new/errspp_avg")


# SSF - 26
path <- "C:/Users/angmel/Documents/MSc-small-scale-fisheries/SAU_DBEM_RESULTS/Alaska/GFDL26/SSF/"

filename <- dir(path, pattern = ".csv")
filepath <- paste("C:/Users/angmel/Documents/MSc-small-scale-fisheries/SAU_DBEM_RESULTS/Alaska/GFDL26/SSF/", filename, sep = "")

for (i in 1:length(filepath)){
  sau_dbem <- read.csv(filepath[i], header = TRUE)
  
  # remove rows with duplicated cell IDs -  error from 7final.R
  sau_dbem <- sau_dbem[!duplicated(sau_dbem$INDEX),]
  
  # replace NAs with 0
  sau_dbem[is.na(sau_dbem)] <- 0
  
  # prepare data frame for analysis
  flip_sau_dbem <- data.frame(t(sau_dbem))
  
  # average every 20 years
  smoothMean <- rollmean(x = flip_sau_dbem, # original series 
                         k = 20) # width of the rolling window 
  # tidying data
  colnames(smoothMean) <- flip_sau_dbem[1,]
  smoothMean <- smoothMean[-1,]
  
  if (sum(smoothMean, na.rm = TRUE) > 0) {
    smoothMean2 <- smoothMean[1:51,]
    rownames(smoothMean2) <- c(2000:2050)
    avg_sau_dbem <- data.frame(t(smoothMean2))
    
    # final result
    newfilepath <- paste("C:/Users/angmel/Documents/MSc-small-scale-fisheries/SAU_DBEM_RESULTS/Alaska/GFDL26/SSF/avg_new/", filename[i], "_avg.csv", sep = "")
    
    write.csv(avg_sau_dbem, newfilepath)
  }
  
  else {
    errspp[i] <-  filepath[i]
  }
}

errspp
x <- data.frame(errspp) # CHANGE THIS FOR DROBO/CYGWIN
write_csv(x, "C:/Users/angmel/Documents/MSc-small-scale-fisheries/SAU_DBEM_RESULTS/Alaska/GFDL26/SSF/errspp_avg")

# SSF 8.5
path <- "C:/Users/angmel/Documents/MSc-small-scale-fisheries/SAU_DBEM_RESULTS/Alaska/GFDL85/SSF/"

filename <- dir(path, pattern = ".csv")
filepath <- paste("C:/Users/angmel/Documents/MSc-small-scale-fisheries/SAU_DBEM_RESULTS/Alaska/GFDL85/SSF/", filename, sep = "")

for (i in 1:length(filepath)){
  sau_dbem <- read.csv(filepath[i], header = TRUE)
  
  # remove rows with duplicated cell IDs -  error from 7final.R
  sau_dbem <- sau_dbem[!duplicated(sau_dbem$INDEX),]
  
  # replace NAs with 0
  sau_dbem[is.na(sau_dbem)] <- 0
  
  # prepare data frame for analysis
  flip_sau_dbem <- data.frame(t(sau_dbem))
  
  # average every 20 years
  smoothMean <- rollmean(x = flip_sau_dbem, # original series 
                         k = 20) # width of the rolling window 
  # tidying data
  colnames(smoothMean) <- flip_sau_dbem[1,]
  smoothMean <- smoothMean[-1,]
  
  if (sum(smoothMean, na.rm = TRUE) > 0) {
    smoothMean2 <- smoothMean[1:51,]
    rownames(smoothMean2) <- c(2000:2050)
    avg_sau_dbem <- data.frame(t(smoothMean2))
    
    # final result
    newfilepath <- paste("C:/Users/angmel/Documents/MSc-small-scale-fisheries/SAU_DBEM_RESULTS/Alaska/GFDL85/SSF/avg_new/", filename[i], "_avg.csv", sep = "")
    
    write.csv(avg_sau_dbem, newfilepath)
  }
  
  else {
    errspp[i] <-  filepath[i]
  }
}

errspp
x <- data.frame(errspp) # CHANGE THIS FOR DROBO/CYGWIN
write_csv(x, "C:/Users/angmel/Documents/MSc-small-scale-fisheries/SAU_DBEM_RESULTS/Alaska/GFDL26/SSF/errspp_avg")

############################################################
####################################THIS IS OLD DATA, is.na() was not done
# # CANADA - 2.6 and 8.5
# 
# # LSf - 8.5 and 2.6 
# path <- "C:/Users/angmel/Documents/MSc-small-scale-fisheries/SAU_DBEM_RESULTS/Canada/GFDL85/"
# 
# filename <- dir(path, pattern = ".csv")
# filepath <- paste("C:/Users/angmel/Documents/MSc-small-scale-fisheries/SAU_DBEM_RESULTS/Canada/GFDL85/", filename, sep = "")
# 
# for (i in 1:length(filepath)){
#   sau_dbem <- read.csv(filepath[i], header = TRUE)
#   
#   # remove rows with duplicated cell IDs -  error from 7final.R
#   sau_dbem <- sau_dbem[!duplicated(sau_dbem$INDEX),]
#   
#   # prepare data frame for analysis
#   flip_sau_dbem <- data.frame(t(sau_dbem))
#   
#   # average every 20 years
#   smoothMean <- rollmean(x = flip_sau_dbem, # original series 
#                          k = 20) # width of the rolling window 
#   # tidying data
#   colnames(smoothMean) <- flip_sau_dbem[1,]
#   smoothMean <- smoothMean[-1,]
#   
#   if (sum(smoothMean, na.rm = TRUE) > 0) {
#     smoothMean2 <- smoothMean[1:51,]
#     rownames(smoothMean2) <- c(2000:2050)
#     avg_sau_dbem <- data.frame(t(smoothMean2))
#     
#     # final result
#     newfilepath <- paste("C:/Users/angmel/Documents/MSc-small-scale-fisheries/SAU_DBEM_RESULTS/Canada/GFDL85/", filename[i], "_avg.csv", sep = "")
#     
#     write.csv(avg_sau_dbem, newfilepath)
#   }
#   
#   else {
#     errspp[i] <-  filepath[i]
#   }
# }
# 
# errspp
# x <- data.frame(errspp) # CHANGE THIS FOR DROBO/CYGWIN
# write_csv(x, "C:/Users/angmel/Documents/MSc-small-scale-fisheries/SAU_DBEM_RESULTS/Canada/GFDL85/errspp_avg")
# 
# 
# # SSF
# path <- "C:/Users/angmel/Documents/MSc-small-scale-fisheries/SAU_DBEM_RESULTS/Canada/GFDL85/SSF/"
# 
# filename <- dir(path, pattern = ".csv")
# filepath <- paste("C:/Users/angmel/Documents/MSc-small-scale-fisheries/SAU_DBEM_RESULTS/Canada/GFDL85/SSF/", filename, sep = "")
# 
# for (i in 1:length(filepath)){
#   sau_dbem <- read.csv(filepath[i], header = TRUE)
#   
#   # remove rows with duplicated cell IDs -  error from 7final.R
#   sau_dbem <- sau_dbem[!duplicated(sau_dbem$INDEX),]
#   
#   # prepare data frame for analysis
#   flip_sau_dbem <- data.frame(t(sau_dbem))
#   
#   # average every 20 years
#   smoothMean <- rollmean(x = flip_sau_dbem, # original series 
#                          k = 20) # width of the rolling window 
#   # tidying data
#   colnames(smoothMean) <- flip_sau_dbem[1,]
#   smoothMean <- smoothMean[-1,]
#   
#   if (sum(smoothMean, na.rm = TRUE) > 0) {
#     smoothMean2 <- smoothMean[1:51,]
#     rownames(smoothMean2) <- c(2000:2050)
#     avg_sau_dbem <- data.frame(t(smoothMean2))
#     
#     # final result
#     newfilepath <- paste("C:/Users/angmel/Documents/MSc-small-scale-fisheries/SAU_DBEM_RESULTS/Canada/GFDL85/SSF/", filename[i], "_avg.csv", sep = "")
#     
#     write.csv(avg_sau_dbem, newfilepath)
#   }
#   
#   else {
#     errspp[i] <-  filepath[i]
#   }
# }
# 
# errspp
# x <- data.frame(errspp) # CHANGE THIS FOR DROBO/CYGWIN
# write_csv(x, "C:/Users/angmel/Documents/MSc-small-scale-fisheries/SAU_DBEM_RESULTS/Canada/GFDL85/SSF/errspp_avg")
# 
# 
# 
# # USA - 2.6 and 8.5
# 
# # LSf - 8.5 and 2.6 
# path <- "C:/Users/angmel/Documents/MSc-small-scale-fisheries/SAU_DBEM_RESULTS/USA/GFDL26/"
# 
# filename <- dir(path, pattern = ".csv")
# filepath <- paste("C:/Users/angmel/Documents/MSc-small-scale-fisheries/SAU_DBEM_RESULTS/USA/GFDL26/", filename, sep = "")
# 
# for (i in 1:length(filepath)){
#   sau_dbem <- read.csv(filepath[i], header = TRUE)
#   
#   # remove rows with duplicated cell IDs -  error from 7final.R
#   sau_dbem <- sau_dbem[!duplicated(sau_dbem$INDEX),]
#   
#   # prepare data frame for analysis
#   flip_sau_dbem <- data.frame(t(sau_dbem))
#   
#   # average every 20 years
#   smoothMean <- rollmean(x = flip_sau_dbem, # original series 
#                          k = 20) # width of the rolling window 
#   # tidying data
#   colnames(smoothMean) <- flip_sau_dbem[1,]
#   smoothMean <- smoothMean[-1,]
#   
#   if (sum(smoothMean, na.rm = TRUE) > 0) {
#     smoothMean2 <- smoothMean[1:51,]
#     rownames(smoothMean2) <- c(2000:2050)
#     avg_sau_dbem <- data.frame(t(smoothMean2))
#     
#     # final result
#     newfilepath <- paste("C:/Users/angmel/Documents/MSc-small-scale-fisheries/SAU_DBEM_RESULTS/USA/GFDL26/", filename[i], "_avg.csv", sep = "")
#     
#     write.csv(avg_sau_dbem, newfilepath)
#   }
#   
#   else {
#     errspp[i] <-  filepath[i]
#   }
# }
# 
# errspp
# x <- data.frame(errspp) # CHANGE THIS FOR DROBO/CYGWIN
# write_csv(x, "C:/Users/angmel/Documents/MSc-small-scale-fisheries/SAU_DBEM_RESULTS/USA/GFDL26/errspp_avg")
# 
# 
# # SSF
# path <- "C:/Users/angmel/Documents/MSc-small-scale-fisheries/SAU_DBEM_RESULTS/USA/GFDL26/SSF/"
# 
# filename <- dir(path, pattern = ".csv")
# filepath <- paste("C:/Users/angmel/Documents/MSc-small-scale-fisheries/SAU_DBEM_RESULTS/USA/GFDL26/SSF/", filename, sep = "")
# 
# for (i in 1:length(filepath)){
#   sau_dbem <- read.csv(filepath[i], header = TRUE)
#   
#   # remove rows with duplicated cell IDs -  error from 7final.R
#   sau_dbem <- sau_dbem[!duplicated(sau_dbem$INDEX),]
#   
#   # prepare data frame for analysis
#   flip_sau_dbem <- data.frame(t(sau_dbem))
#   
#   # average every 20 years
#   smoothMean <- rollmean(x = flip_sau_dbem, # original series 
#                          k = 20) # width of the rolling window 
#   # tidying data
#   colnames(smoothMean) <- flip_sau_dbem[1,]
#   smoothMean <- smoothMean[-1,]
#   
#   if (sum(smoothMean, na.rm = TRUE) > 0) {
#     smoothMean2 <- smoothMean[1:51,]
#     rownames(smoothMean2) <- c(2000:2050)
#     avg_sau_dbem <- data.frame(t(smoothMean2))
#     
#     # final result
#     newfilepath <- paste("C:/Users/angmel/Documents/MSc-small-scale-fisheries/SAU_DBEM_RESULTS/USA/GFDL26/SSF/", filename[i], "_avg.csv", sep = "")
#     
#     write.csv(avg_sau_dbem, newfilepath)
#   }
#   
#   else {
#     errspp[i] <-  filepath[i]
#   }
# }
# 
# errspp
# x <- data.frame(errspp) # CHANGE THIS FOR DROBO/CYGWIN
# write_csv(x, "C:/Users/angmel/Documents/MSc-small-scale-fisheries/SAU_DBEM_RESULTS/USA/GFDL85/SSF/errspp_avg")
# 
# 
# 
# # Mexico - 2.6 and 8.5
# 
# # LSf - 8.5 and 2.6 
# path <- "C:/Users/angmel/Documents/MSc-small-scale-fisheries/SAU_DBEM_RESULTS/Mexico/GFDL85/"
# 
# filename <- dir(path, pattern = ".csv")
# filepath <- paste("C:/Users/angmel/Documents/MSc-small-scale-fisheries/SAU_DBEM_RESULTS/Mexico/GFDL85/", filename, sep = "")
# 
# for (i in 1:length(filepath)){
#   sau_dbem <- read.csv(filepath[i], header = TRUE)
#   
#   # remove rows with duplicated cell IDs -  error from 7final.R
#   sau_dbem <- sau_dbem[!duplicated(sau_dbem$INDEX),]
#   
#   # prepare data frame for analysis
#   flip_sau_dbem <- data.frame(t(sau_dbem))
#   
#   # average every 20 years
#   smoothMean <- rollmean(x = flip_sau_dbem, # original series 
#                          k = 20) # width of the rolling window 
#   # tidying data
#   colnames(smoothMean) <- flip_sau_dbem[1,]
#   smoothMean <- smoothMean[-1,]
#   
#   if (sum(smoothMean, na.rm = TRUE) > 0) {
#     smoothMean2 <- smoothMean[1:51,]
#     rownames(smoothMean2) <- c(2000:2050)
#     avg_sau_dbem <- data.frame(t(smoothMean2))
#     
#     # final result
#     newfilepath <- paste("C:/Users/angmel/Documents/MSc-small-scale-fisheries/SAU_DBEM_RESULTS/Mexico/GFDL85/", filename[i], "_avg.csv", sep = "")
#     
#     write.csv(avg_sau_dbem, newfilepath)
#   }
#   
#   else {
#     errspp[i] <-  filepath[i]
#   }
# }
# 
# errspp
# x <- data.frame(errspp) # CHANGE THIS FOR DROBO/CYGWIN
# write_csv(x, "C:/Users/angmel/Documents/MSc-small-scale-fisheries/SAU_DBEM_RESULTS/Mexico/GFDL85/errspp_avg")
# 
# 
# # SSF
# path <- "C:/Users/angmel/Documents/MSc-small-scale-fisheries/SAU_DBEM_RESULTS/Mexico/GFDL85/SSF/"
# 
# filename <- dir(path, pattern = ".csv")
# filepath <- paste("C:/Users/angmel/Documents/MSc-small-scale-fisheries/SAU_DBEM_RESULTS/Mexico/GFDL85/SSF/", filename, sep = "")
# 
# for (i in 1:length(filepath)){
#   sau_dbem <- read.csv(filepath[i], header = TRUE)
#   
#   # remove rows with duplicated cell IDs -  error from 7final.R
#   sau_dbem <- sau_dbem[!duplicated(sau_dbem$INDEX),]
#   
#   # prepare data frame for analysis
#   flip_sau_dbem <- data.frame(t(sau_dbem))
#   
#   # average every 20 years
#   smoothMean <- rollmean(x = flip_sau_dbem, # original series 
#                          k = 20) # width of the rolling window 
#   # tidying data
#   colnames(smoothMean) <- flip_sau_dbem[1,]
#   smoothMean <- smoothMean[-1,]
#   
#   if (sum(smoothMean, na.rm = TRUE) > 0) {
#     smoothMean2 <- smoothMean[1:51,]
#     rownames(smoothMean2) <- c(2000:2050)
#     avg_sau_dbem <- data.frame(t(smoothMean2))
#     
#     # final result
#     newfilepath <- paste("C:/Users/angmel/Documents/MSc-small-scale-fisheries/SAU_DBEM_RESULTS/Mexico/GFDL85/SSF/", filename[i], "_avg.csv", sep = "")
#     
#     write.csv(avg_sau_dbem, newfilepath)
#   }
#   
#   else {
#     errspp[i] <-  filepath[i]
#   }
# }
# 
# errspp
# x <- data.frame(errspp) # CHANGE THIS FOR DROBO/CYGWIN
# write_csv(x, "C:/Users/angmel/Documents/MSc-small-scale-fisheries/SAU_DBEM_RESULTS/Mexico/GFDL85/SSF/errspp_avg")
# 
