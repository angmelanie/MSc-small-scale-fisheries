# let's work with RCP 26 - import list of species from each EEZ, keep eez as a column
# -----
# CANADA
path <- "C:/Users/angmel/Documents/firstchapter/SAU_DBEM_RESULTS/Canada/GFDL26/avg/"
filename <- dir(path)
filepath <- paste("C:/Users/angmel/Documents/firstchapter/SAU_DBEM_RESULTS/Canada/GFDL26/avg/", filename, sep = "")

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
path <- "C:/Users/angmel/Documents/firstchapter/SAU_DBEM_RESULTS/Alaska/GFDL26/avg/"
filename <- dir(path)
filepath <- paste("C:/Users/angmel/Documents/firstchapter/SAU_DBEM_RESULTS/Alaska/GFDL26/avg/", filename, sep = "")

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
path <- "C:/Users/angmel/Documents/firstchapter/SAU_DBEM_RESULTS/USA/GFDL26/avg/"
filename <- dir(path)
filepath <- paste("C:/Users/angmel/Documents/firstchapter/SAU_DBEM_RESULTS/USA/GFDL26/avg/", filename, sep = "")

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
path <- "C:/Users/angmel/Documents/firstchapter/SAU_DBEM_RESULTS/Mexico/GFDL26/avg/"
filename <- dir(path)
filepath <- paste("C:/Users/angmel/Documents/firstchapter/SAU_DBEM_RESULTS/Mexico/GFDL26/avg/", filename, sep = "")

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

##############################################################
# BIND ALL EEZS

# size class table: taxa_sizeclass

RCP26_sizeclass <- dplyr::bind_rows(Alaska_dp, Canada_dp) %>% 
  dplyr::bind_rows(USA_dp) %>% 
  dplyr::bind_rows(Mexico_dp) %>% 
  rename(TaxonKey = taxaID) # making this compatible with demerspelag table
RCP26_sizeclass$TaxonKey <- as.numeric(RCP26_sizeclass$TaxonKey) # likewise making compatible


# link to demersal-pelagic sheet ----
RCP26_sizeclass_catch <- left_join(RCP26_sizeclass, taxa_sizeclass, by = "TaxonKey")

RCP26d <- RCP26 %>% 
  filter(DemersPelag == "Demersal") %>% 
  group_by(year) %>% 
  summarize(year_t = sum(year_total))
RCP26d

RCP26p <- RCP26 %>% 
  filter(DemersPelag == "Pelagic") %>% 
  group_by(year) %>% 
  summarize(year_t = sum(year_total))

RCP26reef <- RCP26 %>% 
  filter(DemersPelag == "reef-associated") %>% 
  group_by(year) %>% 
  summarize(year_t = sum(year_total))

# plot it:
test26 <- RCP26_sizeclass_catch %>% 
  group_by(size.class, year) %>% 
  summarize(sum(year_total)) %>% 
  ggplot(aes(x=year, y = `sum(year_total)`)) +
  geom_point(aes(colour = size.class)) +
  theme_bw() +
  labs(title = "By size class - RCP 2.6: Projection of a sample of small-scale fisheries catches in PNA") +
  theme(axis.text.x = element_text(angle = 90)) +
  scale_x_discrete(breaks = c("2000", "2010", "2020", "2030", "2040", "2050"))

ggsave("catch-by-demerpel-26.png", path = "C:/Users/angmel/Documents/firstchapter/SAU_DBEM_RESULTS/AGGREGATE/")


######################

# Plot RCP 2.6 and 8.5 together

RCP26_sizeclass_catch$RCP <- "2.6"
RCP85_sizeclass_catch$RCP <- "8.5"
sizeclass <- bind_rows(RCP85_sizeclass_catch, RCP26_sizeclass_catch)

sizeclass %>% 
  group_by(size.class, year, RCP) %>% 
  summarize(sum(year_total)) %>% 
  ggplot(aes(x = year, y = `sum(year_total)`)) +
  geom_point(aes(colour = size.class, shape = RCP), size = 2) +
  theme_bw() + 
labs(title = "By DemersPelag: Projection of a sample of small-scale fisheries catches in PNA") +
  theme(axis.text.x = element_text(angle = 90)) +
  scale_x_discrete(breaks = c("2000", "2010", "2020", "2030", "2040", "2050"))


