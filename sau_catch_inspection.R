# STEP 1 - DATA INSPECTION, CLEANING AND PREP

# IN THIS FILE:
# 1. READ SAU CATCHES FROM GORDON, CLEAN UP DATA.
# 2. PREP DATA FOR ANALYSIS - SPLIT LSF AND SSF BY COUNTRY & TAKE 75% OF CATCHES
# please also see 1- data check for original data checks including plotting, coordinates etc.

#load packages
install.packages("tidyverse")
library(tidyverse)

# STEP 1: CLEAN DATA  ------------------------------------------------
# read SAU spatial catch for PNA (Alaska, Canada, USA, Mexico)
sau_PNA <- read_csv("sau_catch/Melanie_PNW_17-05-2017.csv")

# clean data - remove 2011-2014 from data set
unique(sau_PNA$year) # includes years 2011-2014 which you don't need
exclude <- c(2011:2014)
sau_PNA <- sau_PNA %>%
  filter(!(sau_PNA$year %in% exclude))

# seperate SSF
sau_PNA_ssf <- sau_PNA %>%
  filter(sector == "Subsistence" | sector == "Artisanal")

# filter out LSF
sau_PNA_lsf <- sau_PNA %>%
  filter(sector == "Industrial") 

# STEP 2: SPLIT DATA FOR ANALYSIS -------------------------------------
# Seperate out each EEZ AND take top 75% taxa - total

# In LSF
# From Canada EEZ
sau_lsf_ca <- sau_PNA_lsf %>% 
  filter(eez == "Canada (Pacific)") %>% 
  group_by(taxon_name) %>% 
  summarize(catch_sum = sum(catch)) %>% 
  arrange(-catch_sum)
# select the top 75%
sau_lsf_ca2 <- sau_lsf_ca[sau_lsf_ca$catch_sum > quantile(sau_lsf_ca$catch_sum, probs = 25/100),]

# ALASKA
sau_lsf_al <- sau_PNA_lsf %>% 
  filter(eez == "USA (Alaska, Subarctic)") %>% 
  group_by(taxon_name) %>% 
  summarize(catch_sum = sum(catch)) %>% 
  arrange(-catch_sum)
# select the top 75%
sau_lsf_al2 <- sau_lsf_al[sau_lsf_al$catch_sum > quantile(sau_lsf_al$catch_sum, probs = 25/100),]

# USA
sau_lsf_us <- sau_PNA_lsf %>% 
  filter(eez == "USA (West Coast)") %>% 
  group_by(taxon_name) %>% 
  summarize(catch_sum = sum(catch)) %>% 
  arrange(-catch_sum)
# select the top 75%
sau_lsf_us2 <- sau_lsf_us[sau_lsf_us$catch_sum > quantile(sau_lsf_us$catch_sum, probs = 25/100),]

# Mexico
sau_lsf_me <- sau_PNA_lsf %>% 
  filter(eez == "Mexico (Pacific)") %>% 
  group_by(taxon_name) %>% 
  summarize(catch_sum = sum(catch)) %>% 
  arrange(-catch_sum)
# select the top 75%
sau_lsf_me2 <- sau_lsf_me[sau_lsf_me$catch_sum > quantile(sau_lsf_me$catch_sum, probs = 25/100),]



# SMALL-SCALE FISHERIES
# From Canada EEZ
sau_ssf_ca <- sau_PNA_ssf %>% 
  filter(eez == "Canada (Pacific)") %>% 
  group_by(taxon_name) %>% 
  summarize(catch_sum = sum(catch)) %>% 
  arrange(-catch_sum)
# select the top 75%
sau_ssf_ca2 <- sau_ssf_ca[sau_ssf_ca$catch_sum > quantile(sau_ssf_ca$catch_sum, probs = 25/100),]

# ALASKA
sau_ssf_al <- sau_PNA_ssf %>% 
  filter(eez == "USA (Alaska, Subarctic)") %>% 
  group_by(taxon_name) %>% 
  summarize(catch_sum = sum(catch)) %>% 
  arrange(-catch_sum)
# select the top 75%
sau_ssf_al[sau_ssf_al$catch_sum > quantile(sau_ssf_al$catch_sum, probs = 25/100),]

# USA
sau_ssf_us <- sau_PNA_ssf %>% 
  filter(eez == "USA (West Coast)") %>% 
  group_by(taxon_name) %>% 
  summarize(catch_sum = sum(catch)) %>% 
  arrange(-catch_sum)
# select the top 75%
sau_ssf_us[sau_ssf_us$catch_sum > quantile(sau_ssf_us$catch_sum, probs = 25/100),]

# Mexico
sau_ssf_me_1 <- sau_PNA_ssf %>% 
  filter(eez == "Mexico (Pacific)") %>% 
  group_by(taxon_name) %>% 
  summarize(catch_sum = sum(catch)) %>% 
  arrange(-catch_sum)
dim(sau_ssf_me_1)
dim(sau_ssf_me)
# select the top 75%
sau_ssf_me[sau_ssf_me$catch_sum > quantile(sau_ssf_me$catch_sum, probs = 25/100),]


# EXTRAS ---------------------------------------------------------------
# VISUALIZATIONS
sau_PNA_lsf%>% 
  ggplot(aes(x = year, y = catch_sum)) +
  geom_line(aes(colour = eez)) +
  labs(title = "Industrial catches in Pacific North America from Sea Around Us") +
  scale_y_continuous(labels =  scales::comma)
# saved this

sau_PNA %>%
  filter(sector == "Industrial", eez == "Canada (Pacific)") %>% 
  group_by(eez, taxon_name) %>% 
  summarize(catch_sum = sum(catch)) %>% 
  arrange(-catch_sum) %>% 
  mutate(taxon_name_factor = fct_lump(taxon_name, n = 10)) 

sau_PNA %>%
  filter(sector == "Industrial", eez == "Canada (Pacific)") %>% 
  group_by(eez, taxon_name, year) %>% 
  summarize(catch_sum = sum(catch)) %>% 
  arrange(-catch_sum) %>% 
  ggplot(aes(x = year, y = catch_sum)) +
  geom_area(aes(colour = reorder(taxon_name, catch_sum)), position = "fill") +
  theme_bw()

