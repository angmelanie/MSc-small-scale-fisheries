# species turnover ####

# ## GFDL LSF 26 2087####

# specify path and directory
path <- "C:/Users/angmel/Documents/MSc-small-scale-fisheries/SAU_DBEM_RESULTS/LSF/GFDL/year2100/26/avg/"
filename <- dir(path, pattern = ".csv")
filepath <- paste("C:/Users/angmel/Documents/MSc-small-scale-fisheries/SAU_DBEM_RESULTS/LSF/GFDL/year2100/26/avg/", filename, sep = "")

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
  df$i <- substr(filepath[i], 95,100)
  
  biolist[[i]] <- df
}

dplyr::bind_rows(biolist) %>% 
  spread(i, value)

turnover(dplyr::bind_rows(biolist), 
         time.var = "year",
         species.var = "i",
         abundance.var = "value",
         replicate.var = "X")

lsf_26_all

