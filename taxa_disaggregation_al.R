# TAXA DISAGGREGATION - ALASKA

# THESE ARE THE TOP 75% CATCH BY COUNTRY AND DISAGRREGATE TO TAXA LEVEL
# RESULTS - taxa_dis_al_2
# output needs to be a file of species you need to model in Access

# load packages
library(tidyverse)
library(sqldf)

# import taxon table
taxonID <- read_csv("C:/Users/angmel/Documents/firstchapter/Reference tables/taxonID.csv")

# IN ORIGINAL SPLIT - IMPORT CSV FILES BY COUNTRIES
taxa_dis <- read_csv("split_catch/taxa_dis_1.csv")

# TAXA DISAGGREGATION - ALASKA
# there are 9 error files, you have to remove these from the original split
# all except one case its fine, arctic char direct match (B species)

# extract only Alaska dis from original
taxa_dis_al <- taxa_dis %>% 
  filter(eez == "USA (Alaska, Subarctic)")

# these are the error files
al_error <- taxa_dis_al %>% 
  filter(suggested_taxaID %in% c("690651", "690115", "600898", "690206", "690088", "604252", "600751", "600309")) %>% 
  filter(Phylum == "Chordata") %>% 
  dplyr::select(suggested_taxaID)
# most of the error files are inverts which i will model
# except for 1 taxa which is a direct match
# the other taxa there are multiple species within, so I can delete them and redistribute the catch numbers

###########################################################################
taxa_dis_al %>% 
  filter(taxonID %in% "600247") 
# this is the only one thats a direct match... arctic char!
# IUCN says caught in Alaska sub arctic so should be ok but DROBO not within...WHY?!?!
##########################################################################

# CHECKS 
# SSF - everything matches, but error file
# error species redistributed
anti_join(sau_ssf_al, taxa_dis_al, by = "taxon_name") 

# LSF 
# yesss - only 3 dont match and they're species level
anti_join(sau_lsf_al, taxa_dis_al, by = "taxon_name") 

# Split ###########################################################################
# SSF------------------------------------------------------------------------------
# remove error files (EXCEPT FOR 600247) species split from dis
taxa_dis_al_2 <- taxa_dis_al %>% 
  filter(!suggested_taxaID %in% al_error$suggested_taxaID)

alaska_ssf <- left_join(sau_ssf_al, taxa_dis_al_2, by = "taxon_name")

# split catch by disaggregation
Names <- toString(names(taxa_dis_al_2["taxon_name"]))
df_count <- fn$sqldf("select $Names, count (*) count from taxa_dis_al group by $Names")
sau_ssf_al_tmp1 <- left_join(alaska_ssf, df_count, by = "taxon_name")

# divide original catch by count to get split catch ----
sau_ssf_al_tmp1$catch_split <- sau_ssf_al_tmp1$catch_sum/sau_ssf_al_tmp1$count


# LSF ------------------------------------------------------------------------------
# add the 3 LSF taxa that are species level to taxa_dis folder
# taxon_name <- c("Alaska plaice", "English sole", "Pacific sand sole")
taxonID <- as.numeric(c("604250", "604248", "604255"))
taxon_name <- c("Pleuronectes quadrituberculatus", "Parophrys vetulus", "Psettichthys melanostictus")
eez <- "USA (Alaska, Subarctic)"
Notes <- "ROUND2"
Suggested <- c("Pleuronectes quadrituberculatus", "Parophrys vetulus", "Psettichthys melanostictus")
suggested_taxaID <- as.numeric(c("604250", "604248", "604255"))
additions <- data.frame(taxonID, eez, taxon_name, Notes, Suggested, suggested_taxaID)
taxa_dis_al <- bind_rows(additions, taxa_dis_al) %>% 
  arrange(taxon_name)

alaska_lsf <- left_join(sau_lsf_al, taxa_dis_al, by = "taxon_name")

# split catch by disaggregation
Names <- toString(names(taxa_dis_al["taxon_name"]))
df_count <- fn$sqldf("select $Names, count (*) count from taxa_dis_al group by $Names")
sau_lsf_al_tmp1 <- left_join(alaska_lsf, df_count, by = "taxon_name")

# divide original catch by count to get split catch ----
sau_lsf_al_tmp1$catch_split <- sau_lsf_al_tmp1$catch_sum/sau_lsf_al_tmp1$count
View(sau_lsf_al_tmp1)

################################### SPLIT CATCH RESULT:
# LSF: sau_lsf_al_tmp1
# SSF: sau_ssf_al_tmp1
al_lsf <- unique(sau_lsf_al_tmp1$suggested_taxaID)

# To DO:
# all species should already have taxaID
# you have to get a unique list from LSF and SSF
# compare this to DROBO and cygwin
# you might have no finfishes (only 3 additions which are direct matches so hopefully)
# but INVERTS NEED TO BE MODELLED
