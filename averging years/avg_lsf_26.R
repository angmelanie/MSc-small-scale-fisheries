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

# LSf - 2.6 
path <- "C:/Users/angmel/Documents/MSc-small-scale-fisheries/SAU_DBEM_RESULTS/LSF/26/"

filename <- dir(path, pattern = ".csv")
filepath <- paste("C:/Users/angmel/Documents/MSc-small-scale-fisheries/SAU_DBEM_RESULTS/LSF/26/", filename, sep = "")

for (i in 1:length(filepath)){
  sau_dbem <- read.csv(filepath[i], header = TRUE)
  # sau_dbem <- read.csv("C:/Users/angmel/Documents/MSc-small-scale-fisheries/SAU_DBEM_RESULTS/LSF/26/600098.csv")
  
  # eez
  cellID <- read.csv("C:/Users/angmel/Documents/MSc-small-scale-fisheries/reference_tables/eez_cellID.csv") %>% 
    filter(EEZID %in% c(959, 925,848,945)) %>% 
    rename(INDEX = CellID.)
  
  sau_dbem <- inner_join(sau_dbem, cellID, by = "INDEX")
  sau_dbem <- data.frame(sau_dbem) %>% 
    dplyr::select(-EEZID)
  
  # remove rows with duplicated cell IDs -  error from 7final.R
  sau_dbem <- sau_dbem[!duplicated(sau_dbem$INDEX),]
  
  # replace NAs with 0
  sau_dbem[is.na(sau_dbem)] <- 0
  
if (sum(sau_dbem[2,], na.rm = TRUE) > 0) {
  
  # prepare data frame for analysis
  flip_sau_dbem <- data.frame(t(sau_dbem))
  
  # average every 20 years
  smoothMean <- rollmean(x = flip_sau_dbem, # original series 
                         k = 20) # width of the rolling window 
  # tidying data
  colnames(smoothMean) <- flip_sau_dbem[1,]
  smoothMean <- smoothMean[-1,]
  
  # if (sum(smoothMean, na.rm = TRUE) > 0) {
    smoothMean2 <- smoothMean[1:51,]
    rownames(smoothMean2) <- c(2000:2050)
    avg_sau_dbem <- data.frame(t(smoothMean2))
    
    # final result
    newfilepath <- paste("C:/Users/angmel/Documents/MSc-small-scale-fisheries/SAU_DBEM_RESULTS/LSF/26/avg/", filename[i], "_avg.csv", sep = "")
    
    write.csv(avg_sau_dbem, newfilepath)
  }
  
  else {
    errspp[i] <-  filepath[i]
  }
}

errspp
lsf_26 <- data.frame(errspp) # CHANGE THIS FOR DROBO/CYGWIN
write_csv(lsf_26, "C:/Users/angmel/Documents/MSc-small-scale-fisheries/SAU_DBEM_RESULTS/LSF/26/avg/lsf_26.csv")
