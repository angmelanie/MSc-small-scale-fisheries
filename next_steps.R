# Steps following DBEM modelling

# Per each eez, find which species are DROBO and cygwin
# Join compare_unique_species and alaskalist etc. by DROBO to find which ones you import by DROBO or cygwin
head(alaskalist)
head(canadalist)
head(mexicolist)
head(uslist)
head(compare_unique_species)

  # find out for each eez where you can find species info from: DROBO or cygwin

  # ALASKA
  alaska_source <- alaskalist %>%
  left_join(compare_unique_species, by = "taxonID") %>%
  select(taxon_name.x, taxonID, DROBO) %>%
  rename(taxonname = taxon_name.x) %>%
  rename(source = DROBO)
  # replace NAs with cygwin as most recently modelled
  alaska_source$source[is.na(alaska_source$source)] <- "cygwin"

  # CANADA
  canada_source <- canadalist %>%
    left_join(compare_unique_species, by = "taxonID") %>%
    select(taxon_name.x, taxonID, DROBO) %>%
    rename(taxonname = taxon_name.x) %>%
    rename(source = DROBO)
  # replace NAs with cygwin as most recently modelled
  canada_source$source[is.na(canada_source$source)] <- "cygwin"
  
  # US
  us_source <- uslist %>%
    left_join(compare_unique_species, by = "taxonID") %>%
    select(taxon_name.x, taxonID, DROBO) %>%
    rename(taxonname = taxon_name.x) %>%
    rename(source = DROBO)
  # replace NAs with cygwin as most recently modelled
  us_source$source[is.na(us_source$source)] <- "cygwin"
  
  # MEXICO
  mexico_source <- mexicolist %>%
    left_join(compare_unique_species, by = "taxonID") %>%
    select(taxon_name.x, taxonID, DROBO) %>%
    rename(taxonname = taxon_name.x) %>%
    rename(source = DROBO)
  # replace NAs with cygwin as most recently modelled
  mexico_source$source[is.na(mexico_source$source)] <- "cygwin"

# Import from folder: SPLITCATCHRESULTS * ex. sau_lsf_al_splitcatch ##########################

# alaska
alaska_lsf_split <- read_csv("C:/Users/angmel/Documents/MSc-small-scale-fisheries/SPLITCATCHRESULTS/sau_lsf_al_splitcatch.csv")
alaska_lsf_split$X1 <- NULL

# some taxa have multiple rows, group together
# Within each eez ^ average catchsum by 2010-1990 = divide by 21, so you get catch in 1 year aka 2000
# find which taxa comes from cygwin and DROBO

alaska_lsf_split_2 <- alaska_lsf_split %>% 
  group_by(suggested_taxaID) %>% 
  summarize(catch_sum = sum(catch_split)) %>% 
  mutate(year_2010 = catch_sum/21) %>% 
  rename(taxonID = suggested_taxaID) %>% 
  left_join(alaska_source) %>% 
  select(-catch_sum) %>% 
  print(n = 102)
  # this is 75% of the catch of Alaska LSF

# split into source
alaska_lsf_DROBO <- alaska_lsf_split_2 %>% 
  filter(source == "DROBO")

# Use code DBEM x SAU/USA_Alaska.R - splits your SAU by DBEM proportions take 1990-2060
  # you need to look into doing it separately by cygwin and by DROBO species

# Use averaging years.R to average 1990-2060 into 2000 to 2050

# Results: done on an EEZ and sector basis, this will give you future projections based on RCP sce

