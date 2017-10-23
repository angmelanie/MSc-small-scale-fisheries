## PURPOSE: Find list of species to model in Access

# Import unique list for each EEZ
mexicolist
uslist
canadalist
alaskalist <- alaskalist %>% 
  rename(taxon_name = Suggested) %>% 
  rename(taxonID = suggested_taxaID)

# Combine lists together to form one
master_taxalist <- bind_rows(mexicolist, uslist) %>% 
  bind_rows(canadalist) %>% 
  bind_rows(alaskalist)
str(master_taxalist) # 602, 2

# Unique this list
unique(master_taxalist) # 457, 2 - not sure why this didnt match properly

master_taxalist <- master_taxalist %>% 
  dplyr::select(taxon_name) %>% 
  unique() %>%  #440
  left_join(taxonID)
View(master_taxalist)

# Make sure all species have an assigned ID aka import your assigned taxa ID

meltaxonID <- read.csv("~/MSc-small-scale-fisheries/reference_tables/mel_taxa_assigned.csv") %>% 
  rename(taxon_name = Suggested) %>% 
  rename(taxonID = suggested_taxaID)

# Join to reassign to blank taxaID

tmp1 <- left_join(master_taxalist, meltaxonID, by  = "taxon_name")

# Assign 800000 IDs to blanks

# merge original and new suggested columns
master_taxalist$taxonID[!is.na(tmp1$taxonID.y)] <- tmp1$taxonID.y[!is.na(tmp1$taxonID.y)] 
sau_lsf_tmp3 <- sau_lsf_tmp2 %>% 
  dplyr::select(taxon_name, catch_sum.x, suggested)
master_taxalist$ID <- NULL
tmp2 <- master_taxalist %>% 
  arrange(taxonID)
write.csv(tmp2, "tmp2") # assign taxaID

unique_species <- read_csv("C:/Users/angmel/Documents/MSc-small-scale-fisheries/reference_tables/mel_assigned_taxa_updated.csv")  # THIS IS THE FINAL LIST OF UNIQUE SPECIES
View(unique_species)
# Import pre-modelled DROBO list
DROBO_list <- read_csv("C:/Users/angmel/Documents/MSc-small-scale-fisheries/reference_tables/DROBO_list.csv") %>% 
  rename(taxonID = TaxonID)
DROBO_list$DROBO <- "DROBO"

# Import cygwin list
CYGWIN_list <- read_csv("C:/Users/angmel/Documents/MSc-small-scale-fisheries/reference_tables/CYGWIN_list.csv") %>% 
  rename(taxonID = TaxonID)
CYGWIN_list$CYGWIN <- "CYGWIN"

# Compare - what is not modelled yet
compare_unique_species <- unique_species %>% 
  left_join(DROBO_list) %>% 
  left_join(CYGWIN_list)
View(compare_unique_species)

# merge DROBO and CYGWIN to find out what is missing
compare_unique_species$DROBO[!is.na(compare_unique_species$CYGWIN)] <- compare_unique_species$CYGWIN[!is.na(compare_unique_species$CYGWIN)] 

# tidy data

compare_unique_species %>% 
  rename(exist = DROBO)
compare_unique_species$CYGWIN <- NULL
compare_unique_species %>% 
  arrange(DROBO) %>% 
  write.csv("model_me")

# Export this list into Access - this is what you work with

# under exist column, the NAs are species you need to model
# all 263 species...