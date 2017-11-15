# split catch by taxa and store in global env
split_CA <- split(canada_ssf_DROBO, canada_ssf_DROBO$taxonID)
list2env(split_CA, envir = .GlobalEnv)

# Alaska EEZ
Canada_eez_cellID <- read.csv(file = "C:/Users/angmel/Documents/firstchapter/Reference tables/eez_cellID.csv") %>% 
  filter(EEZID %in% "925") %>%    # EEZ = USA subarctic Alaska 
  rename(eez = EEZID, INDEX = CellID.)

# upload USA taxa list
canada_ssf_DROBO$taxonID
# alaska_cygwin <- read.csv("C:/Users/angmel/Documents/firstchapter/SAU x DBEM/taxaID/Alaska_cygwin.csv", header = FALSE)
# alaska cygwin list are species I haven't modelled yet... but may later on
# for now, there is no cygwin species


# RCP: 2.6
errspp <- numeric()
#length(alaska_lsf_DROBO$taxonID

for(i in 1:length(canada_ssf_DROBO$taxonID)){
  taxontemp <-as.character(canada_ssf_DROBO$taxonID[i])
  dbem <- dbem_Import(taxontemp, 1990, 2060, 8.5, "Catch") #RCP
  dbem <- inner_join(dbem, Canada_eez_cellID, by = "INDEX")
  dbem <- dbem %>% 
    select(-eez)
  
  if (sum(dbem[1,], na.rm = TRUE) > 0) {
    
    # extract DBEM present day average distribution
    dbem_present <- dbem[1:22]
    
    # Sum total catch potential for each year
    dbem_present_total<-colSums(dbem_present,na.rm=T)
    
    #Calculate proportion of each cell in each year in terms of relative catch potential
    dbem_present_relative<-sweep(dbem_present,MARGIN=2,dbem_present_total,'/')
    
    #Take sea around us catch
    
    taxacatch<-get(taxontemp)
    
    # taxacatchbase<-taxacatch$year_2010[order(taxacatch$year)]
    
    # # Average catch over 20 years (2010-1990)
    # average_catch <- sum(taxacatchbase)/20
    # average_catch <- as.numeric(average_catch)
    
    # prorate it based on relative abundance distribution
    dbem_present_prorated<-sweep(dbem_present_relative[,2:length(dbem_present_relative[1,])],MARGIN=2,taxacatch$year_2010,'*')
    
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
    
    sau_present <- data.frame(matrix(seq(1,70), ncol = 70))
    sau_present[1:20] <- taxacatch$year_2010
    
    for (i in 1:51){
      sau_present[i+20] <- sau_present[1,i+19]*dbem_future_total_mag[i+1]
    }
    colnames(sau_present) <- c(1990:2059)
    
    
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
    
    filepath<-paste("C:/Users/angmel/Documents/MSc-small-scale-fisheries/SAU_DBEM_RESULTS/Canada/GFDL85/SSF/",taxontemp,".csv",sep="")
    write.csv(sau_dbem,filepath,row.names=F)
  }
  else{
    errspp[i] <-  taxontemp
  }
}

Canada85_DROBO <- errspp # CHANGE THIS FOR DROBO/CYGWIN
Canada85_DROBO <- data.frame(Canada85_DROBO) # make data frame format for saving
write_csv(Canada85_DROBO, "C:/Users/angmel/Documents/MSc-small-scale-fisheries/SAU_DBEM_RESULTS/Canada/GFDL85/SSF/errspp85ssf")

