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
View(tmp2) # blanks need to be assigned ID and prob have to be modelled

# Import pre-modelled DROBO list

# Compare - what is not modelled yet

# Import cygwin list

# Compare - what is not modelled yet

# Export this list into Access - this is what you work with