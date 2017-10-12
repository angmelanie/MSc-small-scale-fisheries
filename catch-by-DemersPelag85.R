# DEMERSAL-PELAGIC
# How will SSF catches of demersal-pelagic species be impacted by CC?

# import taxa list we have
taxalist <- read.csv("C:/Users/angmel/Documents/firstchapter/ANALYSIS/taxa_cygwin_DROBO.csv")
# import cygwin known demerspelag info
cygwin_dp <- read.csv("C:/Users/angmel/Documents/firstchapter/ANALYSIS/taxa_cygwin_demerpelag.csv") %>% 
  rename(TaxonID = TaxonKey)

# join above species list with my access list (species i have demerspelagic info for)
taxalist_dp <- full_join(taxalist, cygwin_dp, by = "TaxonID") # incomplete, building

# remainder list, find sci name via join
drobo_taxaID <- taxalist_dp %>% 
  filter(Source %in% "DROBO") %>% 
  dplyr::select(TaxonID)

sci_common <- read.csv("C:/Users/angmel/Documents/firstchapter/Reference tables/taxonID.csv") %>% 
  rename(TaxonID = taxonID)

drobo_dp_sci_ID <- right_join(sci_common, drobo_taxaID, by = "TaxonID") 
drobo_dp_sci <- drobo_dp_sci_ID %>% 
  dplyr::select(taxon_name)

# run this through rfishbase demerspelag
library(rfishbase)

drobo_dp <- species(drobo_dp_sci$taxon_name, fields = "DemersPelag") %>% 
  dplyr::select(sciname, DemersPelag)

unique(drobo_dp$DemersPelag) # find variations

# rfishbase: not just Demer/Pela/Reef but other combination, simplifying to 3 options
DemersPelag <- c("pelagic-neritic", "pelagic-oceanic", "demersal", "reef-associated", "benthopelagic", "bathydemersal")
DemersPelag_2 <- c("Pelagic", "Pelagic", "Demersal", "reef-associated", "Pelagic", "Demersal")
lookup <- data.frame(DemersPelag, DemersPelag_2)

# form lookup table of speciesID, demerspelag and eez
taxa_demerspelag <- left_join(drobo_dp, lookup, by = "DemersPelag") %>%   # lookup to 1/3 options
  dplyr::select(-(DemersPelag)) %>%                   # remove the variations
  rename(taxon_name = sciname) %>% 
  full_join(drobo_dp_sci_ID) %>% 
  rename(DemersPelag = DemersPelag_2)

# let's work with RCP 85 - import list of species from each EEZ, keep eez as a column
# -----
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

# BIND ALL EEZS
RCP85_dp <- dplyr::bind_rows(Alaska_dp, Canada_dp) %>% 
  dplyr::bind_rows(USA_dp) %>% 
  dplyr::bind_rows(Mexico_dp) %>% 
  rename(TaxonID = taxaID) # making this compatible with demerspelag table
RCP85_dp$TaxonID <- as.numeric(RCP85_dp$TaxonID) # likewise making compatible

class(RCP85_dp$TaxonID)

# link to demersal-pelagic sheet ----
RCP85 <- left_join(RCP85_dp, taxa_demerspelag, by = "TaxonID")

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
test85 <- RCP85 %>% 
  group_by(DemersPelag, year) %>% 
  summarize(sum(year_total)) %>% 
  ggplot(aes(x=year, y = `sum(year_total)`)) +
  geom_point(aes(colour = DemersPelag)) +
  theme_bw() +
  labs(title = "By DemersPelag - RCP 8.5: Projection of a sample of small-scale fisheries catches in PNA") +
  theme(axis.text.x = element_text(angle = 90)) +
  scale_x_discrete(breaks = c("2000", "2010", "2020", "2030", "2040", "2050"))


ggsave("catch-by-demerpel-85.png", path = "C:/Users/angmel/Documents/firstchapter/SAU_DBEM_RESULTS/AGGREGATE/")


