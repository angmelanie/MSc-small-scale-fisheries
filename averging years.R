# AVERAGING CATCH ACROSS TIME PERIOD

# I take a 20 year average per species, per cell 
# and call that the average catch in year n.

# Here I create a for loop that goes through each folder
# You need to change: input path, input filepath and output path
# I've also removed all the error species from the folder
# You need to do this for each RCP scenario and for each EEZ


path <- "C:/Users/angmel/Documents/firstchapter/SAU_DBEM_RESULTS/Alaska/GFDL26/"

filename <- dir(path, pattern = ".csv")
filepath <- paste("C:/Users/angmel/Documents/firstchapter/SAU_DBEM_RESULTS/Alaska/GFDL26/", filename, sep = "")

for (i in 1:2){
  sau_dbem <- read.csv(filepath[i], header = TRUE)
  
  # remove rows with duplicated cell IDs -  error from 7final.R
  sau_dbem <- sau_dbem[!duplicated(sau_dbem$INDEX),]
  
  # prepare data frame for analysis
  flip_sau_dbem <- data.frame(t(sau_dbem))
  
  # average every 20 years
  smoothMean <- rollmean(x = flip_sau_dbem, # original series 
                         k = 20) # width of the rolling window 
  # tidying data
  colnames(smoothMean) <- flip_sau_dbem[1,]
  smoothMean <- smoothMean[-1,]
  rownames(smoothMean) <- c(2000:2051)
  avg_sau_dbem <- data.frame(t(smoothMean))
  
  # final result
  newfilepath <- paste("C:/Users/angmel/Documents/firstchapter/SAU_DBEM_RESULTS/Mexico/GFDL26/", filename[i], "_avg.csv", sep = "")
  
  write.csv(avg_sau_dbem, newfilepath)
}