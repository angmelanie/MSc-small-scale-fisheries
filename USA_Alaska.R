# SAU X DBEM: ALASKA

# Purpose: This file takes DBEM (from DROBO or cygwin) and on an individual
# species basis, it calculates magnitude change between years pfor each cell.
# Then it applies this magnitude change to the total SAU catch per species 
# in that given EEZ.

# Result: SAU catch split according to DBEM proportions
# Error log: DBEM do not contain any records within the proposed EEZ

# SPECIFIC INFO: In Alaska, there is no cygwin files!

# NOTE TO SELF: 
# I present the code repeated for RCP85 and RCP26, no need to change RCP.

# You have to change:
# 1. dbem_import path
# 2. taxa list In 2 places
# 3. error log: assign to variable, save and refresh

# dbem_import() set to DROBO data (PATHS)
# Z:/DATA/DBEM/GFDL85F1/
# Z:/DATA/DBEM/GFDL26F1/

# REPEAT THIS WITH PATH SET TO CYGWIN
# C:/cygwin64/home/angmel/Fortran/Results/GFDL85MAF1/
# C:/cygwin64/home/angmel/Fortran/Results/GFDL26MAF1/


# Load packages

library(tidyverse) #dplyr function
# dbem_import() function

# read Alaska catch
USA_Alaska <- read.csv(file = "C:/Users/angmel/Documents/firstchapter/SAU x DBEM/USA Alaska")

# split catch by taxa and store in global env
split_Alaska <- split(USA_Alaska, USA_Alaska$suggested_taxaID)
list2env(split_Alaska, envir = .GlobalEnv)

# USA West Coast EEZ
Alaska_eez_cellID <- read.csv(file = "C:/Users/angmel/Documents/firstchapter/Reference tables/eez_cellID.csv") %>% 
  filter(EEZID %in% "959") %>%    # EEZ = USA subarctic Alaska 
  rename(eez = EEZID, INDEX = CellID.)

# upload USA taxa list
alaska_DROBO <- read.csv("C:/Users/angmel/Documents/firstchapter/SAU x DBEM/taxaID/Alaska_DROBO.csv", header = FALSE)
# alaska_cygwin <- read.csv("C:/Users/angmel/Documents/firstchapter/SAU x DBEM/taxaID/Alaska_cygwin.csv", header = FALSE)
# alaska cygwin list are species I haven't modelled yet... but may later on
# for now, there is no cygwin species


# RCP: 2.6
errspp <- numeric()

for(i in 1:length(alaska_DROBO$V1)){
  taxontemp <-as.character(alaska_DROBO$V1[i])
  dbem <- dbem_Import(taxontemp, 1990, 2060, 2.6, "Catch")
  dbem <- inner_join(dbem, Alaska_eez_cellID, by = "INDEX")
  
  if (sum(dbem[1,], na.rm = TRUE) > 0) {
    
    # extract DBEM present day average distribution
    dbem_present <- dbem[1:22]
    
    # Sum total catch potential for each year
    dbem_present_total<-colSums(dbem_present,na.rm=T)
    
    #Calculate proportion of each cell in each year in terms of relative catch potential
    dbem_present_relative<-sweep(dbem_present,MARGIN=2,dbem_present_total,'/')
    
    #Take sea around us catch
    
    taxacatch<-get(taxontemp)
    
    taxacatchbase<-taxacatch$split_catch[order(taxacatch$year)]
    
    # Average catch over 20 years (2010-1990)
    average_catch <- sum(taxacatchbase)/20
    average_catch <- as.numeric(average_catch)
    
    # prorate it based on relative abundance distribution
    dbem_present_prorated<-sweep(dbem_present_relative[,2:length(dbem_present_relative[1,])],MARGIN=2,average_catch,'*')
    
    dbem_present_prorated<-data.frame(INDEX=dbem_present$INDEX,dbem_present_prorated)
    
    # FUTURE calculations
    # extract DBEM for future years 2010-2060 relevant
    dbem_future <- dbem[c(1,22:72)]
    
    # sum total catch potential for each year
    dbem_future_total <- colSums(dbem_future, na.rm = T)
    
    # Calculate magnitude change between each year's total (i+1)/i
    dbem_future_total_mag <- data.frame(matrix(seq(1,52), ncol = 52))
    for (i in 1:52){
      dbem_future_total_mag[i] <- `dbem_future_total`[i+1]/`dbem_future_total`[i]
    }
    colnames(dbem_future_total_mag) <- c("INDEX", 2010:2060) # apply value to year n stated to get n+1
    
    # Find future sea around us catch by applying magnitude change to present catch
    sau_present <- data.frame(t(taxacatchbase))
    sau_present <- data.frame(average_catch)
    for (i in 1:20){
      sau_present[i+1] <- sau_present[i]
    }
    colnames(sau_present) <- c(1990:2010)
    for (i in 1:51){
      sau_present[i+21] <- sau_present[1,i+20]*dbem_future_total_mag[i+1]
    }
    colnames(sau_present) <- c(1990:2060)
    #******************************************
    
    #Calculate proportion of each cell in each year in terms of relative catch potential
    dbem_future_relative<-sweep(dbem_future,MARGIN=2,dbem_future_total,'/')
    dbem_future_relative[is.na(dbem_future_relative)] <- 0
    
    # Multiply future proportions with sau total to find actual cell by cell data
    sau_future <- (sau_present[20:71])
    colnames(sau_future) <- c(2009:2060) # ignore 2009, just placeholder
    sau_future_list <- as.numeric(sau_future) # convert into proper format
    dbem_future_prorated<-sweep(dbem_future_relative,MARGIN=2,sau_future_list,'*')
    
    dbem_future_prorated$INDEX <- dbem_future$INDEX
    
    # Join 1990-2060 into one dataframe
    sau_dbem <- full_join(dbem_present_prorated, dbem_future_prorated, by = "INDEX")
    sau_dbem[22] <- NULL
    
    filepath<-paste("C:/Users/angmel/Documents/firstchapter/SAU_DBEM_RESULTS/Alaska/GFDL26/",taxontemp,".csv",sep="")
    write.csv(sau_dbem,filepath,row.names=F)
  }
  else{
    errspp[i] <-  taxontemp
  }
}

Alaska26_DROBO <- errspp # CHANGE THIS FOR DROBO/CYGWIN

# USA DROBO RCP 8.5
# rerun for cygwin change taxa list

errspp <- numeric()

for(i in 1:length(alaska_DROBO$V1)){
  taxontemp <-as.character(alaska_DROBO$V1[i])
  dbem <- dbem_Import(taxontemp, 1990, 2060, 8.5, "Catch")
  dbem <- inner_join(dbem, Alaska_eez_cellID, by = "INDEX")
  
  if (sum(dbem[1,], na.rm = TRUE) > 0) {
    
    # extract DBEM present day average distribution
    dbem_present <- dbem[1:22]
    
    # Sum total catch potential for each year
    dbem_present_total<-colSums(dbem_present,na.rm=T)
    
    #Calculate proportion of each cell in each year in terms of relative catch potential
    dbem_present_relative<-sweep(dbem_present,MARGIN=2,dbem_present_total,'/')
    
    #Take sea around us catch
    
    taxacatch<-get(taxontemp)
    
    taxacatchbase<-taxacatch$split_catch[order(taxacatch$year)]
    
    # Average catch over 20 years (2010-1990)
    average_catch <- sum(taxacatchbase)/20
    average_catch <- as.numeric(average_catch)
    
    # prorate it based on relative abundance distribution
    dbem_present_prorated<-sweep(dbem_present_relative[,2:length(dbem_present_relative[1,])],MARGIN=2,average_catch,'*')
    
    dbem_present_prorated<-data.frame(INDEX=dbem_present$INDEX,dbem_present_prorated)
    
    # FUTURE calculations
    # extract DBEM for future years 2010-2060 relevant to Canada
    dbem_future <- dbem[c(1,22:72)]
    
    # sum total catch potential for each year
    dbem_future_total <- colSums(dbem_future, na.rm = T)
    
    # Calculate magnitude change between each year's total (i+1)/i
    dbem_future_total_mag <- data.frame(matrix(seq(1,52), ncol = 52))
    for (i in 1:52){
      dbem_future_total_mag[i] <- `dbem_future_total`[i+1]/`dbem_future_total`[i]
    }
    colnames(dbem_future_total_mag) <- c("INDEX", 2010:2060) # apply value to year n stated to get n+1
    
    # Find future sea around us catch by applying magnitude change to present catch
    sau_present <- data.frame(t(taxacatchbase))
    sau_present <- data.frame(average_catch)
    for (i in 1:20){
      sau_present[i+1] <- sau_present[i]
    }
    colnames(sau_present) <- c(1990:2010)
    for (i in 1:51){
      sau_present[i+21] <- sau_present[1,i+20]*dbem_future_total_mag[i+1]
    }
    colnames(sau_present) <- c(1990:2060)
    #******************************************
    
    #Calculate proportion of each cell in each year in terms of relative catch potential
    dbem_future_relative<-sweep(dbem_future,MARGIN=2,dbem_future_total,'/')
    dbem_future_relative[is.na(dbem_future_relative)] <- 0
    
    # Multiply future proportions with sau total to find actual cell by cell data
    sau_future <- (sau_present[20:71])
    colnames(sau_future) <- c(2009:2060) # ignore 2009, just placeholder
    sau_future_list <- as.numeric(sau_future) # convert into proper format
    dbem_future_prorated<-sweep(dbem_future_relative,MARGIN=2,sau_future_list,'*')
    
    dbem_future_prorated$INDEX <- dbem_future$INDEX
    
    # Join 1990-2060 into one dataframe
    sau_dbem <- full_join(dbem_present_prorated, dbem_future_prorated, by = "INDEX")
    sau_dbem[22] <- NULL
    
    filepath<-paste("C:/Users/angmel/Documents/firstchapter/SAU_DBEM_RESULTS/Alaska/GFDL85/",taxontemp,".csv",sep="")
    write.csv(sau_dbem,filepath,row.names=F)
  }
  else{
    errspp[i] <-  taxontemp
  }
}

ALASKA85_DROBO <- errspp # CHANGE THIS FOR DROBO/CYGWIN


# save error files
write.csv(Alaska26_DROBO, "ALASKA26DROBO")
write.csv(ALASKA85_DROBO, "ALASKA85DROBO")
