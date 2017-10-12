# Sort by size classification

################################################
# Data prep
# Get taxa list and size class for CYGWIN and DROBO species

# CYGWIN - size available from Access table, groupings made using Excel
cygwin_sizeclass <- read.csv("C:/Users/angmel/Documents/firstchapter/ANALYSIS/sizeclass_cygwin.csv")

# DROBO - find from rfishbase

# import taxa list (cygwin + DROBO) we have
taxalist <- read.csv("C:/Users/angmel/Documents/firstchapter/ANALYSIS/taxa_cygwin_DROBO.csv")

# filter for only DROBO
drobo_taxaID <- taxalist %>% 
  filter(Source %in% "DROBO") %>% 
  dplyr::select(TaxonID)

# lets get scientific name: drobo_sci
sci_common <- read.csv("C:/Users/angmel/Documents/firstchapter/Reference tables/taxonID.csv") %>% 
  rename(TaxonID = taxonID)
drobo_sci_ID <- right_join(sci_common, drobo_taxaID, by = "TaxonID") 
drobo_sci <- drobo_sci_ID %>% 
  dplyr::select(taxon_name)

# find size via rfishbase
library(rfishbase)
drobo_length <- species(drobo_sci$taxon_name, fields = "Length") %>% 
  dplyr::select(sciname, Length) %>% 
  rename(taxon_name = sciname) %>% 
  left_join(drobo_sci_ID, by = "taxon_name")

write.csv(drobo_length, "C:/Users/angmel/Documents/firstchapter/ANALYSIS/sizeclass_drobo.csv")
# perform groupings in Excel
# Small: < 30cm, Medium: 30-90, Large: >90
drobo_sizeclass <- read.csv("C:/Users/angmel/Documents/firstchapter/ANALYSIS/sizeclass_drobo.csv")

# bind_rows into one taxa list
taxa_sizeclass <- bind_rows(cygwin_sizeclass, drobo_sizeclass)

##################################################
# LETS START WITH RCP 8.5
# The first process is the same : you're just collecting each EEZ, scenario's
# catch and taxa ID

# CANADA
path <- "C:/Users/angmel/Documents/firstchapter/SAU_DBEM_RESULTS/Canada/GFDL85/avg/"
filename <- dir(path)
filepath <- paste("C:/Users/angmel/Documents/firstchapter/SAU_DBEM_RESULTS/Canada/GFDL85/avg/", filename, sep = "")

# AGGREGATE TOTAL - FOR LOOP
# create empty list for aggregation
datalist <- list()

# loop through each .csv file in folder and sum by year
for (i in 1:length(filepath)){
  
  # upload average years
  avg_catch <- read.csv(filepath[i])
  
  # rename columns, remove X
  colnames(avg_catch) <- c("cellID", 2000:2051)
  
  # this still maintains cellID info
  p <- avg_catch %>% 
    gather(year, catch, -c(cellID), na.rm=TRUE) %>% 
    dplyr::select(.,year, catch) %>% 
    group_by(year) %>% 
    summarize(year_total = sum(catch)) %>% 
    data.frame()
  p$taxaID <- substr(filename[i],1,6) #extract from path the taxa ID & assign as column
  
  datalist[[i]] <- p # attaches each csv file as a new list
}

# bind all the list together
Canada_dp <- dplyr::bind_rows(datalist)
Canada_dp$country <- "Canada"

# ALASKA
path <- "C:/Users/angmel/Documents/firstchapter/SAU_DBEM_RESULTS/Alaska/GFDL85/avg/"
filename <- dir(path)
filepath <- paste("C:/Users/angmel/Documents/firstchapter/SAU_DBEM_RESULTS/Alaska/GFDL85/avg/", filename, sep = "")

# AGGREGATE TOTAL - FOR LOOP
# create empty list for aggregation
datalist <- list()

# loop through each .csv file in folder and sum by year
for (i in 1:length(filepath)){
  
  # upload average years
  avg_catch <- read.csv(filepath[i])
  
  # rename columns, remove X
  colnames(avg_catch) <- c("cellID", 2000:2051)
  
  # this still maintains cellID info
  p <- avg_catch %>% 
    gather(year, catch, -c(cellID), na.rm=TRUE) %>% 
    dplyr::select(.,year, catch) %>% 
    group_by(year) %>% 
    summarize(year_total = sum(catch)) %>% 
    data.frame()
  p$taxaID <- substr(filename[i],1,6) #extract from path the taxa ID & assign as column
  
  datalist[[i]] <- p # attaches each csv file as a new list
}

# bind all the list together
Alaska_dp <- dplyr::bind_rows(datalist)
Alaska_dp$country <- "Alaska"


# UNITED STATES
path <- "C:/Users/angmel/Documents/firstchapter/SAU_DBEM_RESULTS/USA/GFDL85/avg/"
filename <- dir(path)
filepath <- paste("C:/Users/angmel/Documents/firstchapter/SAU_DBEM_RESULTS/USA/GFDL85/avg/", filename, sep = "")

# AGGREGATE TOTAL - FOR LOOP
# create empty list for aggregation
datalist <- list()

# loop through each .csv file in folder and sum by year
for (i in 1:length(filepath)){
  
  # upload average years
  avg_catch <- read.csv(filepath[i])
  
  # rename columns, remove X
  colnames(avg_catch) <- c("cellID", 2000:2051)
  
  # this still maintains cellID info
  p <- avg_catch %>% 
    gather(year, catch, -c(cellID), na.rm=TRUE) %>% 
    dplyr::select(.,year, catch) %>% 
    group_by(year) %>% 
    summarize(year_total = sum(catch)) %>% 
    data.frame()
  p$taxaID <- substr(filename[i],1,6) #extract from path the taxa ID & assign as column
  
  datalist[[i]] <- p # attaches each csv file as a new list
}

# bind all the list together
USA_dp <- dplyr::bind_rows(datalist)
USA_dp$country <- "USA"

# MEXICO
path <- "C:/Users/angmel/Documents/firstchapter/SAU_DBEM_RESULTS/Mexico/GFDL85/avg/"
filename <- dir(path)
filepath <- paste("C:/Users/angmel/Documents/firstchapter/SAU_DBEM_RESULTS/Mexico/GFDL85/avg/", filename, sep = "")

# AGGREGATE TOTAL - FOR LOOP
# create empty list for aggregation
datalist <- list()

# loop through each .csv file in folder and sum by year
for (i in 1:length(filepath)){
  
  # upload average years
  avg_catch <- read.csv(filepath[i])
  
  # rename columns, remove X
  colnames(avg_catch) <- c("cellID", 2000:2051)
  
  # this still maintains cellID info
  p <- avg_catch %>% 
    gather(year, catch, -c(cellID), na.rm=TRUE) %>% 
    dplyr::select(.,year, catch) %>% 
    group_by(year) %>% 
    summarize(year_total = sum(catch)) %>% 
    data.frame()
  p$taxaID <- substr(filename[i],1,6) #extract from path the taxa ID & assign as column
  
  datalist[[i]] <- p # attaches each csv file as a new list
}

# bind all the list together
Mexico_dp <- dplyr::bind_rows(datalist)
Mexico_dp$country <- "Mexico"

##########################################################
# BIND ALL EEZS
RCP85_sizeclass <- dplyr::bind_rows(Alaska_dp, Canada_dp) %>% 
  dplyr::bind_rows(USA_dp) %>% 
  dplyr::bind_rows(Mexico_dp) %>% 
  rename(TaxonKey = taxaID) # making this compatible with size class table
RCP85_sizeclass$TaxonKey <- as.numeric(RCP85_sizeclass$TaxonKey) # likewise making compatible

# link to demersal-pelagic sheet ----
RCP85_sizeclass_catch <- left_join(RCP85_sizeclass, taxa_sizeclass, by = "TaxonKey")

RCP85d <- RCP85 %>% 
  filter(DemersPelag == "Demersal") %>% 
  group_by(year) %>% 
  summarize(year_t = sum(year_total))
RCP85d

RCP85p <- RCP85 %>% 
  filter(DemersPelag == "Pelagic") %>% 
  group_by(year) %>% 
  summarize(year_t = sum(year_total))

RCP85reef <- RCP85 %>% 
  filter(DemersPelag == "reef-associated") %>% 
  group_by(year) %>% 
  summarize(year_t = sum(year_total))

# plot it:
plot85size <- RCP85_sizeclass_catch %>% 
  group_by(size.class, year, country) %>% 
  summarize(sum(year_total)) %>% 
  ggplot(aes(x=year, y = `sum(year_total)`)) +
  geom_point(aes(colour = size.class)) +
  facet_wrap(~country) +
  theme_bw() +
  labs(title = "By size class - RCP 8.5: Projection of a sample of small-scale fisheries catches in PNA") +
  theme(axis.text.x = element_text(angle = 90)) +
  scale_x_discrete(breaks = c("2000", "2010", "2020", "2030", "2040", "2050"))

plot85size <- RCP85_sizeclass_catch %>% 
  group_by(size.class, year) %>% 
  summarize(sum(year_total)) %>% 
  ggplot(aes(x=year, y = `sum(year_total)`)) +
  geom_point(aes(colour = size.class)) +
  theme_bw() +
  labs(title = "By size class - RCP 8.5: Projection of a sample of small-scale fisheries catches in PNA") +
  theme(axis.text.x = element_text(angle = 90)) +
  scale_x_discrete(breaks = c("2000", "2010", "2020", "2030", "2040", "2050"))

ggsave("catch-by-sizeclass-facet-85.png", path = "C:/Users/angmel/Documents/firstchapter/SAU_DBEM_RESULTS/AGGREGATE/")






