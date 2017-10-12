# Download libraries

library(tidyverse)
# dbem_import() function


# get aggregate SAU catch by taxon (aka remove cell ID component)
?group_by
final_ssf_summary <- final_ssf %>%
  group_by(eez, year, sector, suggested_taxaID) %>%
  summarise(split_catch = sum(split_catch)) %>%
  as.data.frame()

# split the SAU catch into eezs and SAVED as csv
split_eez <- split(final_ssf_summary, final_ssf_summary$eez)
list2env(split_eez, envir= .GlobalEnv)
Canada <- read.csv(file = "C:/Users/angmel/Documents/firstchapter/SAU x DBEM/Canada (Pacific)")
write.csv(`Mexico (Pacific)`, file = "C:/Users/angmel/Documents/firstchapter/SAU x DBEM/Mexico (Pacific)")
write.csv(`USA Alaska`, file = "C:/Users/angmel/Documents/firstchapter/SAU x DBEM/USA Alaska.csv")
write.csv(`USA (West Coast)`, file = "C:/Users/angmel/Documents/firstchapter/SAU x DBEM/USA (Pacific)")

# COUNTRY SPLIT BY EEZ ------
# Canadian EEZ

Canada_EEZ_cellID <- read.csv("C:/Users/angmel/Documents/firstchapter/Reference tables/canada_eez_cellID.csv")
head(Canada_EEZ_cellID)
split_Canada <- split(Canada, Canada$suggested_taxaID)
list2env(split_Canada, envir= .GlobalEnv)

# Alaska EEZ
USA_Alaska <- read.csv(file = "C:/Users/angmel/Documents/firstchapter/SAU x DBEM/USA Alaska.csv")
Alaska_eez_cellID <- read.csv(file = "C:/Users/angmel/Documents/firstchapter/Reference tables/Alaska_eez_cellID.csv")
colnames(Alaska_eez_cellID) <- c("eez", "INDEX")
split_Alaska <- split(USA_Alaska, USA_Alaska$suggested_taxaID)
?list2env
list2env(split_Alaska, envir = .GlobalEnv)
Alaska_taxa <- data.frame(unique(USA_Alaska$suggested_taxaID))
Alaska_taxa <- Alaska_taxa[-120,]
Alaska_taxa <- data.frame(Alaska_taxa)
colnames(Alaska_taxa) <- "ID"
write.csv(`Alaska_taxa`, file = "C:/Users/angmel/Documents/firstchapter/SAU x DBEM/taxaID/Alaska_taxaID")
alaska_DROBO <- read.csv("C:/Users/angmel/Documents/firstchapter/SAU x DBEM/taxaID/Alaska_DROBO.csv", header = FALSE)
alaska_cygwin <- read.csv("C:/Users/angmel/Documents/firstchapter/SAU x DBEM/taxaID/Alaska_cygwin.csv", header = FALSE)
# the rest of the cygwin folder have not been modelled

# USA West Coast
USA_West_catch <- read.csv(file = "C:/Users/angmel/Documents/firstchapter/SAU x DBEM/USA (Pacific)")
USA_eez_cellID <- read.csv(file = "C:/Users/angmel/Documents/firstchapter/Reference tables/USA_eez_cellID.csv")
colnames(USA_eez_cellID) <- c("eez", "INDEX")
split_USA <- split(USA_West_catch, USA_West_catch$suggested_taxaID)
?list2env
list2env(split_USA, envir = .GlobalEnv)
USA_taxa <- data.frame(unique(USA_West$suggested_taxaID))
USA_taxa <- USA_taxa[-158,]
USA_taxa <- data.frame(USA_taxa)
colnames(USA_taxa) <- "ID"
write.csv(`USA_taxa`, file = "C:/Users/angmel/Documents/firstchapter/SAU x DBEM/taxaID/USA_taxaID")

# upload taxa list
USA_DROBO <- read.csv("C:/Users/angmel/Documents/firstchapter/SAU x DBEM/taxaID/USA_DROBO.csv", header = FALSE)
USA_cygwin <- read.csv("C:/Users/angmel/Documents/firstchapter/SAU x DBEM/taxaID/USA_cygwin.csv", header = FALSE)

# cell ID/EEZ table
cellID <- read.csv(file = "C:/Users/angmel/Documents/firstchapter/Reference tables/eez_cellID.csv")
colnames(cellID) <- c("eez", "INDEX")
USA_West <- cellID %>% 
  filter(eez == "848")

# add taxa into global enviro
USA_West_catch <- read.csv(file = "C:/Users/angmel/Documents/firstchapter/SAU x DBEM/USA (Pacific)")
split_USA <- split(USA_West_catch, USA_West_catch$suggested_taxaID)
list2env(split_USA, envir = .GlobalEnv)

# the rest of the cygwin folder have not been modelled


# Mexico

# read Mexico SAU catch data
Mexico <- read.csv(file = "C:/Users/angmel/Documents/firstchapter/SAU x DBEM/Mexico (Pacific)")

# read Mexico cell ID table
Mexico_eez_cellID <- read.csv(file = "C:/Users/angmel/Documents/firstchapter/Reference tables/Mexico_eez_cellID.csv")

# rename Mexico cell ID table for join
colnames(Mexico_eez_cellID) <- c("eez", "INDEX")

# split each suggested taxa into its own data frame
split_Mexico <- split(Mexico, Mexico$suggested_taxaID)

# import suggested taxa tables into global environment
list2env(split_Mexico, envir = .GlobalEnv)

# import a list of taxa
Mexico_taxa <- Mexico %>% 
  select(suggested_taxaID) %>% 
  unique() %>% 
  data.frame()

# rename column
colnames(Mexico_taxa) <- "ID"

# save file
# write.csv(`Mexico_taxa`, file = "C:/Users/angmel/Documents/firstchapter/SAU x DBEM/taxaID/Mexico_taxaID")

# This step was done in Excel (take list of species from DROBO and cygwin, inner join)
Mexico_DROBO <- read.csv("C:/Users/angmel/Documents/firstchapter/SAU x DBEM/taxaID/Mexico_DROBO.csv", header = FALSE)
Mexico_cygwin <- read.csv("C:/Users/angmel/Documents/firstchapter/SAU x DBEM/taxaID/Mexico_cygwin.csv", header = FALSE)


#************************************************************** ----

# Canada ONLY
# for(i in 1:length(Canada_taxa$taxon.taxaID)){
#   taxontemp<-as.character(Canada_taxa$taxon.taxaID[i])
#   taxontemp<-substr(taxontemp,nchar(taxontemp)-5,nchar(taxontemp))

# NOTES:
# change country eez
# change dbem_import() function's path to DROBO or cygwin!
# change RCP scenario
# change end file path

for(i in 1:length(USA_DROBO$V1)){
    taxontemp<-as.character(USA_DROBO$V1[i])
    # taxontemp<-substr(taxontemp,nchar(taxontemp)-5,nchar(taxontemp))
    # taxontemp<-as.character(800001)
  # extract DBEM data
  dbem <- dbem_Import(taxontemp, 1990, 2060, 2.6, "Catch")
  dbem <- inner_join(dbem, USA_West, by = "INDEX")
  unique(dbem$INDEX)
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
  
  filepath<-paste("C:/Users/angmel/Documents/firstchapter/SAU_DBEM_RESULTS/USA/GFDL26/",taxontemp,".csv",sep="")
  write.csv(sau_dbem,filepath,row.names=F)
}
  
  #*******************************************
  
  
  
#   sau_dbem[is.na(sau_dbem)] <- 0
#   
#   # average 10 years
#   
#   avg_sau_dbem <- data.frame(t(array(1:52)))
#   colnames(avg_sau_dbem) <- c("INDEX", 2000:2050)
#   
#   for (j in 1:length(sau_dbem[,1])){
#     
#     # a <- 52 * length(sau_dbem[,1])
#     # avg_sau_dbem <- data.frame(matrix(seq(1:a), ncol = 52))
#     # avg_sau_dbem$X1 <- sau_dbem$INDEX 
#     # colnames(avg_sau_dbem) <- c("INDEX", 2000:2050)
#     
#     for (i in 2:52){
#       x <- i
#       y <- i + 19
#       avg_sau_dbem[j,i] <- rowMeans(sau_dbem[j,x:y])
#       
#       # avg_sau_dbem[j,i] <- rowone[j,i] 
#     }}
#   
#   avg_sau_dbem$INDEX <- sau_dbem$INDEX
#   
# }
