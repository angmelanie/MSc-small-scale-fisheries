# USA - 2.6 and 8.5

# LSf - 2.6 
path <- "C:/Users/angmel/Documents/MSc-small-scale-fisheries/SAU_DBEM_RESULTS/USA/GFDL26/"

filename <- dir(path, pattern = ".csv")
filepath <- paste("C:/Users/angmel/Documents/MSc-small-scale-fisheries/SAU_DBEM_RESULTS/USA/GFDL26/", filename, sep = "")

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
    newfilepath <- paste("C:/Users/angmel/Documents/MSc-small-scale-fisheries/SAU_DBEM_RESULTS/USA/GFDL26/avg_new/", filename[i], "_avg.csv", sep = "")
    
    write.csv(avg_sau_dbem, newfilepath)
  }
  
  else {
    errspp[i] <-  filepath[i]
  }
}

# no error

#### LSF 85

path <- "C:/Users/angmel/Documents/MSc-small-scale-fisheries/SAU_DBEM_RESULTS/USA/GFDL85/"

filename <- dir(path, pattern = ".csv")
filepath <- paste("C:/Users/angmel/Documents/MSc-small-scale-fisheries/SAU_DBEM_RESULTS/USA/GFDL85/", filename, sep = "")

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
    newfilepath <- paste("C:/Users/angmel/Documents/MSc-small-scale-fisheries/SAU_DBEM_RESULTS/USA/GFDL85/avg_new/", filename[i], "_avg.csv", sep = "")
    
    write.csv(avg_sau_dbem, newfilepath)
  }
  
  else {
    errspp[i] <-  filepath[i]
  }
}

# no error

# SSF - 26
path <- "C:/Users/angmel/Documents/MSc-small-scale-fisheries/SAU_DBEM_RESULTS/USA/GFDL26/SSF/"

filename <- dir(path, pattern = ".csv")
filepath <- paste("C:/Users/angmel/Documents/MSc-small-scale-fisheries/SAU_DBEM_RESULTS/USA/GFDL26/SSF/", filename, sep = "")

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
    newfilepath <- paste("C:/Users/angmel/Documents/MSc-small-scale-fisheries/SAU_DBEM_RESULTS/USA/GFDL26/SSF/avg_new/", filename[i], "_avg.csv", sep = "")
    
    write.csv(avg_sau_dbem, newfilepath)
  }
  
  else {
    errspp[i] <-  filepath[i]
  }
}

# no error spp


# SSF 8.5
path <- "C:/Users/angmel/Documents/MSc-small-scale-fisheries/SAU_DBEM_RESULTS/USA/GFDL85/SSF/"

filename <- dir(path, pattern = ".csv")
filepath <- paste("C:/Users/angmel/Documents/MSc-small-scale-fisheries/SAU_DBEM_RESULTS/USA/GFDL85/SSF/", filename, sep = "")

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
    newfilepath <- paste("C:/Users/angmel/Documents/MSc-small-scale-fisheries/SAU_DBEM_RESULTS/USA/GFDL85/SSF/avg_new/", filename[i], "_avg.csv", sep = "")
    
    write.csv(avg_sau_dbem, newfilepath)
  }
  
  else {
    errspp[i] <-  filepath[i]
  }
}

# no errspp
